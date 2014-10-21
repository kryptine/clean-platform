implementation module System.OSError

import Data.Error, System._Pointer
import System._Posix

getLastOSError :: *World -> (MaybeOSError .a, *World)
getLastOSError world 
	# (errno,world) = errno world
	= (Error (errno, message errno),world)
where
	message :: !Int -> String
	message errno
		# ptr = strerr errno
		= derefString ptr

getLastOSErrorCode :: *World -> (MaybeOSErrorCode .a, *World)
getLastOSErrorCode world 
	# (errno,world) = errno world
	= (Error errno, world)
