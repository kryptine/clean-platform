implementation module System.Process

//StdEnv
import StdArray
import StdBool
import StdClass
import StdInt
import StdList
import StdString
import StdMisc

//Data
import Data.Maybe
import Data.Either

//System
import System.FilePath
import System.OSError
import System._Pointer

import System._Windows
import qualified System._WinBase as WinBase

import Text
import Text.GenJSON

:: ReadPipe  = ReadPipe  !Int
:: WritePipe = WritePipe !Int
derive JSONEncode WritePipe, ReadPipe
derive JSONDecode WritePipe, ReadPipe

defaultPtyOptions :: ProcessPtyOptions
defaultPtyOptions =
	{ProcessPtyOptions
	|childInNewSession = True
	,childControlsTty  = True
	,useRawIO          = False
	}

runProcess :: !FilePath ![String] !(Maybe String) !*World -> (MaybeOSError ProcessHandle, *World)
runProcess path args mCurrentDirectory world
	# startupInfo = { createArray STARTUPINFO_size_int 0
	  			 	& [STARTUPINFO_cb_int_offset] 		 = STARTUPINFO_size_bytes
				 	, [STARTUPINFO_dwFlags_int_offset]	 = STARTF_USESTDHANDLES
					}
	= runProcess` path args mCurrentDirectory startupInfo world

runProcess` :: !FilePath ![String] !(Maybe String) !LPSTARTUPINFO !*World -> (MaybeOSError ProcessHandle, *World)
runProcess` path args mCurrentDirectory startupInfo world
	# commandLine = packString (foldr (\a b -> a +++ " " +++ b) "" (map escape [path:args]))
	# processInformation = createArray PROCESS_INFORMATION_size_int 0
	# (ok, world) = case mCurrentDirectory of
		Just dir	-> createProcessA_dir (packString path) commandLine 0 0 True DETACHED_PROCESS 0 (packString dir) startupInfo processInformation world
		Nothing 	-> createProcessA (packString path) commandLine 0 0 True DETACHED_PROCESS 0 0 startupInfo processInformation world
	| not ok = getLastOSError world
	# processHandle = { processHandle = processInformation.[PROCESS_INFORMATION_hProcess_int_offset]
					  , threadHandle = processInformation.[PROCESS_INFORMATION_hThread_int_offset]
					  }
	= (Ok processHandle, world)
where
	escape :: !String -> String
	escape s | indexOf " " s == -1                                    = s
	         | size s >= 2 && s.[0] == '"' && (s.[size s - 1] == '"') = s
			 | otherwise                                              = "\"" +++ s +++ "\""
	
runProcessIO :: !FilePath ![String] !(Maybe String) !*World -> (MaybeOSError (ProcessHandle, ProcessIO), *World)
runProcessIO path args mCurrentDirectory world
	// StdIn
    # (pipeStdIn, world) = openPipe world
    | isError pipeStdIn = (liftError pipeStdIn, world)
    # (pipeStdInOut, pipeStdInIn) = fromOk pipeStdIn
    # (ok, world) = setHandleInformation pipeStdInIn HANDLE_FLAG_INHERIT 0 world
    | not ok = getLastOSError world
    // StdOut
    # (pipeStdOut, world) = openPipe world
    | isError pipeStdOut = (liftError pipeStdOut, world)
    # (pipeStdOutOut, pipeStdOutIn) = fromOk pipeStdOut
    # (ok, world) = setHandleInformation pipeStdOutOut HANDLE_FLAG_INHERIT 0 world
    | not ok = getLastOSError world
    // StdErr
    # (pipeStdErr, world) = openPipe world
    | isError pipeStdErr = (liftError pipeStdErr, world)
    # (pipeStdErrOut, pipeStdErrIn) = fromOk pipeStdErr
    # (ok, world) = setHandleInformation pipeStdErrOut HANDLE_FLAG_INHERIT 0 world
    | not ok = getLastOSError world
    // runProcess
    # startupInfo = { createArray STARTUPINFO_size_int 0
	  			 	& [STARTUPINFO_cb_int_offset] 		  = STARTUPINFO_size_bytes
	  			 	, [STARTUPINFO_hStdInput_int_offset]  = pipeStdInOut
	  			 	, [STARTUPINFO_hStdOutput_int_offset] = pipeStdOutIn
	  			 	, [STARTUPINFO_hStdError_int_offset]  = pipeStdErrIn
				 	, [STARTUPINFO_dwFlags_int_offset]	  = STARTF_USESTDHANDLES
					}
	# (processHandle, world) = runProcess` path args mCurrentDirectory startupInfo world
	| isError processHandle = (liftError processHandle, world)
	= ( Ok ( fromOk processHandle
	       , { stdIn  = WritePipe pipeStdInIn
             , stdOut = ReadPipe  pipeStdOutOut
             , stdErr = ReadPipe  pipeStdErrOut
             }
           )
      , world
      )
	where
		openPipe :: !*World -> (MaybeOSError (HANDLE, HANDLE), !*World)
		openPipe world
			# (heap, world) = getProcessHeap world
			# (ptr, world) = heapAlloc heap 0 8 world
    		| ptr == 0 = abort "heapAlloc failed"
    		# (ok, world) = createPipe ptr (ptr + 4) securityAttributes 0 world
    		| not ok
        		# (_, world) = heapFree heap 0 ptr world
        		= getLastOSError world
    		# (rEnd, ptr)  = readIntP ptr 0
    		# (wEnd, ptr)  = readIntP ptr 4
    		# (_, world) = heapFree heap 0 ptr world
    		= (Ok (rEnd, wEnd), world)
    	
    	securityAttributes = { createArray SECURITY_ATTRIBUTES_SIZE_INT 0
    	                     & [SECURITY_ATTRIBUTES_nLength_INT_OFFSET]        = SECURITY_ATTRIBUTES_SIZE_BYTES
	  			 	         , [SECURITY_ATTRIBUTES_bInheritHandle_INT_OFFSET] = TRUE
					         }

runProcessEscape :: !String -> String
runProcessEscape s | indexOf " " s == -1                                    = s
	               | size s >= 2 && s.[0] == '"' && (s.[size s - 1] == '"') = s
				   | otherwise                                              = "\"" +++ s +++ "\""
           
 
checkProcess :: !ProcessHandle !*World -> (MaybeOSError (Maybe Int), *World)
checkProcess handle=:{processHandle} world
	# (ok, exitCode, world)		= getExitCodeProcess processHandle world
	| not ok					= getLastOSError world
	| exitCode == STILL_ACTIVE	= (Ok Nothing, world)
	# (mbError,world)			= closeProcessHandle handle world
	= (Ok (Just exitCode), world)

waitForProcess :: !ProcessHandle !*World -> (MaybeOSError Int, *World)
waitForProcess handle=:{processHandle} world
	# (res, world)			= waitForSingleObject processHandle INFINITE world
	# (ok, exitCode, world) = getExitCodeProcess processHandle world
	| not ok = getLastOSError world	
	# (mbError,world)		= closeProcessHandle handle world
	= (Ok exitCode, world)

closeProcessHandle :: !ProcessHandle !*World -> (MaybeOSError (), *World)
closeProcessHandle handle world
	# (ok,world) = closeHandle handle.processHandle world
	| not ok = getLastOSError world
	# (ok, world) = closeHandle handle.threadHandle world
	| not ok = getLastOSError world
	= (Ok (), world)

callProcess :: !FilePath ![String] !(Maybe String) !*World -> (MaybeOSError Int, *World)
callProcess path args mCurrentDirectory world
	# (res, world) = runProcess path args mCurrentDirectory world
	= case res of
		Error e		= (Error e,world)
		Ok handle	= waitForProcess handle world

readPipeNonBlocking :: !ReadPipe !*World -> (!MaybeOSError String, !*World)
readPipeNonBlocking (ReadPipe hPipe) world
	// get nr of bytes available to read
	# (heap, world) = getProcessHeap world
	# (nBytesPtr, world) = heapAlloc heap 0 4 world
    | nBytesPtr == 0 = abort "heapAlloc failed"
	# (ok, world) = peekNamedPipe hPipe NULL 0 NULL nBytesPtr NULL world
	| not ok
		# (_, world) = heapFree heap 0 nBytesPtr world
		= getLastOSError world
	# (nBytes, nBytesPtr) = readIntP nBytesPtr 0
	# (_, world) = heapFree heap 0 nBytesPtr world
	// read 'nBytes' bytes
	| nBytes == 0 = (Ok "", world) // read blocks also if nBytes == 0
	# (buf, world) = heapAlloc heap 0 nBytes world
	| buf == 0 = abort "heapAlloc failed"
	# (ok, world) = readFile hPipe buf nBytes NULL NULL world
	| not ok
		# (_, world) = heapFree heap 0 buf world
		= getLastOSError world
	# (str, buf) = readP (\ptr -> derefCharArray ptr nBytes) buf
	# (_, world) = heapFree heap 0 buf world
	= (Ok str, world)

writePipe :: !String !WritePipe !*World -> (!MaybeOSError (), !*World)
writePipe str (WritePipe hPipe) world
	# (ok, world) = writeFile hPipe str (size str) NULL NULL world
    | not ok = getLastOSError world
    = (Ok (), world)

terminateProcess :: !ProcessHandle !*World -> (!MaybeOSError (), !*World)
terminateProcess hProc=:{processHandle} world
	# (ok, world) = 'WinBase'.terminateProcess processHandle 0 world
	= closeProcessHandle hProc world

closeProcessIO :: !ProcessIO !*World -> (!MaybeOSError (), !*World)
closeProcessIO {stdIn = WritePipe hStdIn, stdOut = ReadPipe hStdOut, stdErr = ReadPipe hStdErr} world
	# (ok, world) = closeHandle hStdIn world
	| not ok = getLastOSError world
	# (ok, world) = closeHandle hStdOut world
	| not ok = getLastOSError world
	# (ok, world) = closeHandle hStdErr world
	| not ok = getLastOSError world
	= (Ok (), world)

exit		:: !Int !*World -> (.a,!*World)
exit _ world = (undef`, world)

undef` = undef`

instance closePipe WritePipe
where
    closePipe :: !WritePipe !*World -> (!MaybeOSError (), !*World)
    closePipe (WritePipe pipe) world = closePipe` pipe world

instance closePipe ReadPipe
where
    closePipe :: !ReadPipe !*World -> (!MaybeOSError (), !*World)
    closePipe (ReadPipe pipe) world = closePipe` pipe world

closePipe` :: !Int !*World -> (!MaybeOSError (), !*World)
closePipe` pipe world
	# (res, world) = closeHandle pipe world
	| not res      = getLastOSError world
	= (Ok (), world)
