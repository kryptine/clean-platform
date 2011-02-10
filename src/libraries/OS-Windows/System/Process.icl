implementation module Process

//StdEnv
import StdArray
import StdBool
import StdClass
import StdInt
import StdList
import StdString

//Data
import Maybe
import Void

//System
import FilePath
import OSError
import Pointer

import _Windows

runProcess :: !FilePath ![String] !(Maybe String) !*World -> (MaybeOSError ProcessHandle, *World)
runProcess path args mCurrentDirectory world
# commandLine = packString (foldr (\a b -> a +++ " " +++ b) "" ["\"" +++ path +++ "\"":args])
# startupInfo = { createArray STARTUPINFO_size_int 0
  			 	& [STARTUPINFO_cb_int_offset] 		 = STARTUPINFO_size_bytes
			 	, [STARTUPINFO_dwFlags_int_offset]	 = STARTF_USESTDHANDLES
				}
# processInformation = createArray PROCESS_INFORMATION_size_int 0
# (ok, world) = case mCurrentDirectory of
	Just dir	-> createProcessA_dir (packString path) commandLine 0 0 True DETACHED_PROCESS 0 (packString dir) startupInfo processInformation world
	Nothing 	-> createProcessA (packString path) commandLine 0 0 True DETACHED_PROCESS 0 0 startupInfo processInformation world
| not ok = getLastOSError world
# processHandle = { processHandle = processInformation.[PROCESS_INFORMATION_hProcess_int_offset]
				  , threadHandle = processInformation.[PROCESS_INFORMATION_hThread_int_offset]
				  }
= (Ok processHandle, world)

getExitCode :: !ProcessHandle *World -> (MaybeOSError (Maybe Int), *World)
getExitCode {processHandle} world
# (ok, exitCode, world) = getExitCodeProcess processHandle world
| not ok = getLastOSError world
| exitCode == STILL_ACTIVE = (Ok Nothing, world)
= (Ok (Just exitCode), world)

waitForProcess :: !ProcessHandle *World -> (MaybeOSError Int, *World)
waitForProcess {processHandle} world
# (res, world) = waitForSingleObject processHandle INFINITE world
# (ok, exitCode, world) = getExitCodeProcess processHandle world
| not ok = getLastOSError world
= (Ok exitCode, world)

waitForSingleObject :: !HANDLE !Int !*World -> (!Int,!*World);
waitForSingleObject handle timeout world
	= code {
		ccall WaitForSingleObject@8 "PpI:I:I"
	}

getExitCodeProcess :: !HANDLE !*World -> (!Bool,!Int,!*World);
getExitCodeProcess handle world
	= code {
		ccall GetExitCodeProcess@8 "PI:II:I"
	}

closeProcessHandle :: !ProcessHandle *World -> (MaybeOSError Void, *World)
closeProcessHandle handle world
# (ok,world) = closeHandle handle.processHandle world
| not ok = getLastOSError world
# (ok, world) = closeHandle handle.threadHandle world
| not ok = getLastOSError world
= (Ok Void, world)

callProcess :: !FilePath ![String] !(Maybe String) *World -> (MaybeOSError Int, *World)
callProcess path args mCurrentDirectory world
# (res, world) = runProcess path args mCurrentDirectory world
| isError res = (liftError res, world)
# handle = fromOk res
# (res, world) = waitForProcess handle world
| isError res = (res, world)
# exitCode = fromOk res
# (res, world) = closeProcessHandle handle world
| isError res = (liftError res, world)
= (Ok exitCode, world)
