module client

import StdEnv
import Data.Error
import Data.Maybe
import Network.IP
import System.Socket

Start :: *World -> (MaybeOSError (), *World)
Start w
	= case socket SD_AfInet ST_Stream 0 w of
		(Error e, w) = (Error e, w)
		(Ok sockfd, w)
			#! (merr, sockfd) = connect sockfd (SaAfInet {sin_port=8124,sin_addr=Just (fromString "127.0.0.1")})
			| isError merr = (merr, w)
			# (merr, w) = close sockfd w
			| isError merr = (merr, w)
			= (Ok (), w)
