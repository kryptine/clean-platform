implementation module System.OSError

import Data.Error, System._Pointer
import System._Posix

getLastOSError :: *w -> (MaybeOSError .a, *w)
getLastOSError world 
	# (errno,world) = errno world
	= (Error (errno, message errno),world)
where
	message :: !Int -> String
	message errno
		# ptr = strerr errno
		= derefString ptr

getLastOSErrorCode :: *w -> (MaybeOSErrorCode .a, *w)
getLastOSErrorCode world 
	# (errno,world) = errno world
	= (Error errno, world)
