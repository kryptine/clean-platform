implementation module System.Socket

import Network.IP
import StdEnv
import Data.Maybe 
import Data.Error
import System.OSError
import System._Pointer
import System._Posix
import System._Unsafe

:: *Socket :== Int
:: SocketAddress
	= SaAfInet SaInet
:: SaInet =
	{ sin_port :: !Int
	, sin_addr :: Maybe IPAddress
	}

:: SocketDomain = SD_AfInet
instance toInt SocketDomain where toInt SD_AfInet = 2
:: SocketType = ST_Stream | ST_DGram
instance toInt SocketType where
	toInt ST_Stream = 1
	toInt ST_DGram = 2

serializeSocketAddress :: !SocketAddress !*e -> *(!MaybeOSError !Pointer, !*e)
serializeSocketAddress (SaAfInet {sin_port,sin_addr}) w
	#! p = malloc 16
	| p == 0 = getLastOSError w
	#! p = writeInt2 p 0 AF_INET
	#! p = writeInt2 p 2 (htons sin_port)
	#! p = writeInt4 p 4 (maybe 0 toInt sin_addr)
	= (Ok p, w)

import Text.GenPrint
derive gPrint SocketAddress, SaInet, Maybe
gPrint{|IPAddress|} a s = gPrint{|*|} (toString a) s
instance toString SocketAddress where toString s = printToString s

socket :: !SocketDomain !SocketType !Int !*e -> *(MaybeOSError *Socket, !*e)
socket domain type protocol w
	#! (sockfd, w) = socket` (toInt domain) (toInt type) protocol w
	#! (fd, sockfd) = getFd sockfd
	= case fd of
		-1 = getLastOSError w
		_ = (Ok sockfd, w)
where
	socket` :: !Int !Int !Int !*e -> *(!*Int, !*e)
	socket` _ _ _ _ = code {
			ccall socket "III:I:A"
		}

AF_INET :== 2

bind :: !*Socket !SocketAddress -> *(MaybeOSError (), !*Socket)
bind sockfd addr
	#! (merr, sockfd) = serializeSocketAddress addr sockfd
	| isError merr = (liftError merr, sockfd)
	#! (Ok p) = merr
	#! (fd, sockfd) = getFd sockfd
	#! (r, sockfd) = bind` fd p 16 sockfd
	| r == -1 = getLastOSError sockfd
	#! r = free p
	| r == -1 = getLastOSError sockfd
	= (Ok (), sockfd)
where
	bind` :: !Int !Pointer !Int !*e -> *(!Int, !*e)
	bind` _ _ _ _ = code {
			ccall bind "IpI:I:A"
		}

listen :: !*Socket !Int -> *(!MaybeOSError (), !*Socket)
listen sockfd backlog
	#! r = listen` sockfd backlog
	| r == -1 = getLastOSError sockfd
	= (Ok (), sockfd)
where
	listen` :: !Int !Int -> Int
	listen` _ _ = code {
			ccall listen "II:I"
		}

accept :: !*Socket -> *(!MaybeOSError (!*Socket, !SocketAddress), !*Socket)
accept sockfd
	# (merr, sockfd) = serializeSocketAddress (SaAfInet {sin_port=0,sin_addr=Nothing}) sockfd
	| isError merr = (liftError merr, sockfd)
	# (Ok p) = merr
	# (fd, sockfd) = getFd sockfd
	# p1 = malloc 64
	# p2 = malloc 8
	# p2 = writeInt p2 0 64
	= case accept` fd p1 p2 sockfd of
		(-1, sockfd) = getLastOSError sockfd
		(sock, sockfd)
			#! addr = case readInt2Z p1 0 of
				_ = SaAfInet {sin_port=ntohs (readInt2Z p1 2),sin_addr=Just (fromInt (readInt4Z p1 4))}
				r = abort ("Unknown family: " +++ toString r +++ "\n")
			#! r = free p1
			| r == -1 = getLastOSError sockfd
			#! r = free p2
			| r == -1 = getLastOSError sockfd
			= (Ok (sock, addr), sockfd)
where
	accept` :: !Int !Pointer !Int !*e -> *(!*Int, !*e)
	accept` _ _ _ _ = code {
			ccall accept "IpI:I:A"
		}

connect :: !*Socket !SocketAddress -> *(MaybeOSError (), !*Socket)
connect sockfd addr
	# (merr, sockfd) = serializeSocketAddress addr sockfd
	| isError merr = (liftError merr, sockfd)
	# (Ok p) = merr
	# (fd, sockfd) = getFd sockfd
	# (r, sockfd) = connect` fd p 64 sockfd
	| r == -1 = getLastOSError sockfd
	= (Ok (), sockfd)
where
	connect` :: !Int !Pointer !Int !*e -> *(!Int, !*e)
	connect` _ _ _ _ = code {
			ccall connect "IpI:I:A"
		}

close :: !*Socket !*e -> *(!MaybeOSError (), !*e)
close sock w
	# r = close` sock
	| r == -1 = getLastOSError w
	= (Ok (), w)
where
	close` :: !Int -> Int
	close` _ = code {
			ccall close "I:I"
		}

htons :: !Int -> Int
htons x = code {
		ccall htons "I:I"
	}

ntohs :: !Int -> Int
ntohs x = code {
		ccall ntohs "I:I"
	}

getFd :: !*Socket -> *(!Int, !*Socket)
getFd s = code {
		push_b 0
	}
