implementation module System.Process

//StdEnv
import StdArray
import StdBool
import StdClass
import StdInt
import StdList
import StdString
import StdMisc
import StdFunc

//Data
import Data.Maybe
from Data.List import maximum

//System
import System.FilePath
import System.File
import System.OSError
import System._Pointer
import System._Posix

:: WritePipe = WritePipe !Int
:: ReadPipe  = ReadPipe  !Int

runProcess :: !FilePath ![String] !(Maybe String) !*World -> (MaybeOSError ProcessHandle, *World)
runProcess path args mCurrentDirectory world //TODO: Use mCurrentDirectory argument
	//Check if path exists 
	# (ok,world)	= fileExists path world
	| not ok
		= (Error (1,"File " +++ path +++ " does not exist"),world)
	//Fork
	# (pid, world) = fork world
	| pid == 0
		//Exec
		# (argv, world) = runProcessMakeArgv [path:args] world
		# (res,world)	= execvp (path +++ "\0") argv world
		= (exit 1 world)
	| pid > 0
		= (Ok {ProcessHandle| pid = pid}, world)
	| otherwise
		= getLastOSError world

runProcessIO :: !FilePath ![String] !(Maybe String) !*World -> (MaybeOSError (ProcessHandle, ProcessIO), *World)
runProcessIO path args mCurrentDirectory world //TODO: Use mCurrentDirectory argument
	//Check if path exists 
	# (ok,world)	= fileExists path world
	| not ok
		= (Error (1,"File " +++ path +++ " does not exist"),world)
    // StdIn
    # (pipeStdIn, world) = openPipe world
    | isError pipeStdIn = (liftError pipeStdIn, world)
    # (pipeStdInOut, pipeStdInIn) = fromOk pipeStdIn
    // StdOut
    # (pipeStdOut, world) = openPipe world
    | isError pipeStdOut = (liftError pipeStdOut, world)
    # (pipeStdOutOut, pipeStdOutIn) = fromOk pipeStdOut
    // StdErr
    # (pipeStdErr, world) = openPipe world
    | isError pipeStdErr = (liftError pipeStdErr, world)
    # (pipeStdErrOut, pipeStdErrIn) = fromOk pipeStdErr
	//Fork
	# (pid, world)			= fork world
	| pid == 0
        //redirect stdin/out/err to pipes
        # (res, world)          = dup2 pipeStdInOut STDIN_FILENO world
        | res == -1             = getLastOSError world
        # (res, world)          = close pipeStdInIn world
        | res == -1             = getLastOSError world

        # (res, world)          = dup2 pipeStdOutIn STDOUT_FILENO world
        | res == -1             = getLastOSError world
        # (res, world)          = close pipeStdOutOut world
        | res == -1             = getLastOSError world

        # (res, world)          = dup2 pipeStdErrIn STDERR_FILENO world
        | res == -1             = getLastOSError world
        # (res, world)          = close pipeStdErrOut world
        | res == -1             = getLastOSError world
		//Exec
		# (argv, world)         = runProcessMakeArgv [path:args] world
		# (res, world)			 = execvp (path +++ "\0") argv world
		= (exit 1 world)
	| pid > 0
        # (res, world)          = close pipeStdInOut world
        | res == -1             = getLastOSError world
        # (res, world)          = close pipeStdOutIn world
        | res == -1             = getLastOSError world
        # (res, world)          = close pipeStdErrIn world
        | res == -1             = getLastOSError world
		= ( Ok ( { ProcessHandle
                 | pid = pid
                 }
               , { stdIn  = WritePipe pipeStdInIn
                 , stdOut = ReadPipe  pipeStdOutOut
                 , stdErr = ReadPipe  pipeStdErrOut
                 }
               )
          , world
          )
	| otherwise
		= getLastOSError world

runProcessMakeArgv :: [String] *World -> (!{#Pointer}, *World)
runProcessMakeArgv argv_list world
	# args_size = argvLength argv_list 0
	  args_string = createArgsString args_size argv_list
	  args_memory = malloc args_size
	| args_memory == 0
		= abort "malloc failed"
	# args_memory = memcpy_string_to_pointer args_memory args_string args_size
	# argv = createArgv argv_list args_memory
    #!fRes         = free args_memory
    | fRes <> fRes = undef
	= (argv, world)
where
	argvLength [a:as] l
		= argvLength as (l+((size a +(IF_INT_64_OR_32 8 4)) bitand (IF_INT_64_OR_32 -8 -4)))
	argvLength [] l
		= l

	createArgsString args_size argv_list
		# s = createArray args_size '\0'
		= copyArgs argv_list 0 s
	where
		copyArgs [a:as] i s
			# s = copyChars 0 a i s
			= copyArgs as (i+((size a +(IF_INT_64_OR_32 8 4)) bitand (IF_INT_64_OR_32 -8 -4))) s
		copyArgs [] i s
			= s

		copyChars :: !Int !{#Char} !Int !*{#Char} -> *{#Char}
		copyChars ai a si s
			| ai<size a
				# s = {s & [si]=a.[ai]}
				= copyChars (ai+1) a (si+1) s
			= s

	createArgv argv_list args_memory
		# n_args = length argv_list
		# argv = createArray (n_args+1) 0;
		= fillArgv 0 argv_list argv args_memory 
	where
		fillArgv :: !Int ![{#Char}] !*{#Pointer} !Int -> *{#Pointer}
		fillArgv arg_n [a:as] argv args_memory
			# argv = {argv & [arg_n]=args_memory}
			  args_memory = args_memory + ((size a +(IF_INT_64_OR_32 8 4)) bitand (IF_INT_64_OR_32 -8 -4))
			= fillArgv (arg_n+1) as argv args_memory
		fillArgv arg_n [] argv args_memory
			= {argv & [arg_n]=0}

openPipe :: !*World -> (MaybeOSError (Int, Int), !*World)
openPipe world
    #! ptr = malloc 8
    | ptr == 0 = abort "malloc failed"
    #! (res, world) = pipe ptr world
    | res == -1
        # fRes         = free ptr
        | fRes <> fRes = undef
        = getLastOSError world
    # (rEnd, ptr)  = readP (\ptr -> readInt4S ptr 0) ptr
    # (wEnd, ptr)  = readP (\ptr -> readInt4S ptr 4) ptr
    #! fRes         = free ptr
    | fRes <> fRes = undef
    = (Ok (rEnd, wEnd), world)

checkProcess :: !ProcessHandle !*World -> (MaybeOSError (Maybe Int), *World)
checkProcess {pid} world
	# status		= createArray 1 0
	# (ret,world)	= waitpid pid status WNOHANG world //Non-blocking wait :)
	| ret == 0
		= (Ok Nothing, world)	
	| ret == pid	
		# exitCode = (status.[0] >> 8) bitand 0xFF
		= (Ok (Just exitCode), world)
	| otherwise
		= getLastOSError world

waitForProcess :: !ProcessHandle !*World -> (!MaybeOSError Int, !*World)
waitForProcess {pid} world
	# status		= createArray 1 0
	# (ret,world)	= waitpid pid status 0 world //Blocking wait
	| ret == pid
		# exitCode = (status.[0] >> 8) bitand 0xFF
		= (Ok exitCode, world)
	| otherwise
		= getLastOSError world

	
callProcess :: !FilePath ![String] !(Maybe String) !*World -> (MaybeOSError Int, *World)
callProcess path args mCurrentDirectory world
	# (res, world) = runProcess path args mCurrentDirectory world
	= case res of 
		Ok handle	= waitForProcess handle world
		Error e		= (Error e, world)

readPipeNonBlocking :: !ReadPipe !*World -> (!MaybeOSError String, !*World)
readPipeNonBlocking (ReadPipe fd) world
    # ptr           = malloc 4
    #! (res, world) = ioctl fd FIONREAD ptr world
    | res == -1
        #! fRes        = free ptr
        | fRes <> fRes = undef
        = getLastOSError world
    # (n, ptr)     = readP (\ptr -> readInt4Z ptr 0) ptr
    #! fRes         = free ptr
    | fRes <> fRes = undef
    | n == 0    = (Ok "", world)
    # buffer       = malloc n
    #! (res, world) = read fd buffer n world
    | res == -1
        #! fRes        = free ptr
        | fRes <> fRes = undef
        = getLastOSError world
    #(str, ptr)    = readP (\ptr -> derefCharArray ptr n) buffer
    #!fRes         = free ptr
    | fRes <> fRes = undef
    = (Ok str, world)

readPipeBlocking :: !ReadPipe !*World -> (!MaybeOSError String, !*World)
readPipeBlocking pipe=:(ReadPipe fd) world
    # readfds = malloc 128
    // init array
    # readfds = seq [\ptr -> writeIntElemOffset ptr i 0 \\ i <- [0..IF_INT_64_OR_32 16 32]] readfds
    // set bit for fd
    # offset  = fromInt fd / IF_INT_64_OR_32 64 32
    # val = (readIntElemOffset readfds offset) bitor (1 << (fd rem IF_INT_64_OR_32 64 32))
    # readfds = writeIntElemOffset readfds offset val
    // wait
    #! (res, world) = select_ (fd + 1) readfds 0 0 0 world
    | res == -1
        #!fRes         = free readfds
        | fRes <> fRes = undef
        = getLastOSError world
    #!fRes         = free readfds
    | fRes <> fRes = undef
    = readPipeNonBlocking pipe world

readPipeBlockingMulti :: ![ReadPipe] !*World -> (!MaybeOSError [String], !*World)
readPipeBlockingMulti pipes world
    #readfds = malloc 128
    // init array
    #readfds = seq [\ptr -> writeIntElemOffset ptr i 0 \\ i <- [0..IF_INT_64_OR_32 16 32]] readfds
    // set bits for fds
    #readfds = seq [setFdBit fd \\ ReadPipe fd <- pipes] readfds
    // wait
    #!(res, world) = select_ (maxFd + 1) readfds 0 0 0 world
    | res == -1
        #!fRes         = free readfds
        | fRes <> fRes = undef
        = getLastOSError world
    #!fRes         = free readfds
    | fRes <> fRes = undef
    = seq [ \(res, world) -> case res of
                Ok res`
                    #(r, world) = readPipeNonBlocking pipe world
                    = (seqErrors r (\r` -> Ok [r`:res`]), world)
                error =  (error, world)
            \\ pipe <- reverse pipes
          ]
          (Ok [], world)
where
    maxFd = maximum [fd \\ ReadPipe fd <- pipes]

    setFdBit fd ptr
        #offset  = fromInt fd / IF_INT_64_OR_32 64 32
        #val = (readIntElemOffset ptr offset) bitor (1 << (fd rem IF_INT_64_OR_32 64 32))
        = writeIntElemOffset ptr offset val

writePipe :: !String !WritePipe !*World -> (!MaybeOSError (), !*World)
writePipe str (WritePipe fd) world
    #(res, world) = write fd str (size str) world
    | res == -1 = getLastOSError world
    = (Ok (), world)

terminateProcess :: !ProcessHandle !*World -> (!MaybeOSError (), !*World)
terminateProcess pHandle=:{pid} world
    # (res, world) = kill pid 15 world // Termination signal
    | res == -1    = getLastOSError world
    // otherwise process will remain as zombie
    # status       = createArray 1 0
    # (res, world) = waitpid pid status 0 world
    | res == -1    = getLastOSError world
    = (Ok (), world)

closeProcessIO :: !ProcessIO !*World -> (!MaybeOSError (), !*World)
closeProcessIO {stdIn = WritePipe fdStdIn, stdOut = ReadPipe fdStdOut, stdErr = ReadPipe fdStdErr} world
    # (res, world) = close fdStdIn world
    | res == -1    = getLastOSError world
    # (res, world) = close fdStdOut world
    | res == -1    = getLastOSError world
    # (res, world) = close fdStdErr world
    | res == -1    = getLastOSError world
    = (Ok (), world)

