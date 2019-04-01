definition module System.Signal

from Data.Error import :: MaybeErrorString, :: MaybeError
from System.OSError import :: MaybeOSError, :: OSError, :: OSErrorMessage, :: OSErrorCode

:: SigHandler

//All portable signals numbers:
SIGABRT :== 6
SIGALRM :== 14
SIGHUP  :== 1
SIGINT  :== 2
SIGKILL :== 9 // Can't catch that one
SIGQUIT :== 3
SIGTERM :== 15
SIGTRAP :== 5

/*
 * Install a signal handler
 *
 * @param Signal type
 * @param World
 * @return Error or handler
 * @return New world
 */
signalInstall :: !Int !*env -> *(MaybeOSError *SigHandler, !*env)

/*
 * Poll a signal, this resets the state
 *
 * @param Signal handler
 * @param World
 * @return Was thrown
 * @return New handler
 * @return New world
 */
signalPoll :: !*SigHandler !*env -> *(!MaybeErrorString Bool, !*SigHandler, !*env)
