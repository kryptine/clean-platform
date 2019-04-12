module server

import StdDebug

import StdEnv
import Data.Error
import Data.Maybe
import System.Socket

Start :: *World -> (MaybeOSError (), *World)
Start w
	= case socket SD_AfInet ST_Stream 0 w of
		(Error e, w) = (Error e, w)
		(Ok sockfd, w)
			#! (merr, sockfd) = bind sockfd (SaAfInet {sin_port=8124,sin_addr=Nothing})
			| isError merr = (merr, w)
			#! (merr, sockfd) = listen sockfd 3
			| isError merr = (merr, w)
			= case accept sockfd of
				(Error e, sockfd) = (Error e, w)
				(Ok (sock, addr), sockfd)
					# (merr, w) = close sock w
					| isError merr = (merr, w)
					# (merr, w) = close sockfd w
					| isError merr = (merr, w)
					= (Ok (), w)
