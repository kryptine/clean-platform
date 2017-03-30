definition module System.Process

import Data.Maybe, Data.Either
import System.OSError
import System.FilePath

/*
Not yet implemented:
- Pass startup directory
- Passsing environment, i.e. [(!String,!String)], to created process
- Ability to redirect standard input, standard output, standard error
*/

:: ProcessHandle = { processHandle :: Int
				   , threadHandle  :: Int
				   }

:: ProcessIO = { stdIn  :: WritePipe
               , stdOut :: ReadPipe
               , stdErr :: ReadPipe
               }

:: WritePipe
:: ReadPipe

/**
* runs a new process
* @param Path to the executable
* @param a list of command-line arguments
* @param (optional) startup directory
* @return Process handle to the process
*/
runProcess :: !FilePath ![String] !(Maybe String) !*World -> (MaybeOSError ProcessHandle, *World)

/**
* runs a new process
* @param Path to the executable
* @param a list of command-line arguments
* @param (optional) startup directory
* @return Process handle to the process and pipes for IO
*/
runProcessIO :: !FilePath ![String] !(Maybe String) !*World -> (MaybeOSError (ProcessHandle, ProcessIO), *World)

/**
* Check if a process is still running
* @param Process handle to the process
* @return Boolean indicating if process is still running
*/
checkProcess :: !ProcessHandle !*World -> (MaybeOSError (Maybe Int), *World)

/**
* Wait for a process to terminate, closes the handle and returns the exit code
* @param Process handle to the process
* @return Exit code of the process
*/
waitForProcess :: !ProcessHandle !*World -> (MaybeOSError Int, *World)

/**
* runs a new process and wait for it to terminate
* @param Path to the executable
* @param a list of command-line arguments
* @param (optional) startup directory
* @return Exit code of the process
*/
callProcess :: !FilePath ![String] !(Maybe String) !*World -> (MaybeOSError Int, *World)

readPipeNonBlocking   :: !ReadPipe   !*World -> (!MaybeOSError String,   !*World)
//readPipeBlocking      :: !ReadPipe   !*World -> (!MaybeOSError String,   !*World)
//readPipeBlockingMulti :: ![ReadPipe] !*World -> (!MaybeOSError [String], !*World)

writePipe :: !String !WritePipe !*World -> (!MaybeOSError (), !*World)

/**
 * Dummy function to be API-compatible with the Posix module
 */
exit		:: !Int !*World -> (.a,!*World)
