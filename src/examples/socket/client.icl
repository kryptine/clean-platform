module client

import StdEnv
import Data.Error
import Data.Maybe
import Network.IP
import System.Socket
import System.Socket.Ipv4

Start :: *World -> (MaybeOSError String, *World)
Start w
	= case socket ST_Stream 0 w of
		(Error e, w) = (Error e, w)
		(Ok sockfd, w)
			#! (merr, sockfd) = connect {sin_port=8124,sin_addr=Just (fromString "127.0.0.1")} sockfd
			| isError merr = (liftError merr, w)
			#! (merr, sockfd) = recv 128 0 sockfd
			| isError merr = (merr, w)
			# (Ok msg) = merr
			# (merr, w) = close sockfd w
			| isError merr = (liftError merr, w)
			= (Ok msg, w)
