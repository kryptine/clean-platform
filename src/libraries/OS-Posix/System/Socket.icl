implementation module System.Socket

import Network.IP
import StdEnv
import Data.Maybe 
import Data.Error
import System.OSError
import System._Pointer
import System._Posix
import System._Unsafe

:: *Socket a :== Int

instance toInt SocketType where
	toInt ST_Stream = 1
	toInt ST_DGram = 2

import StdDebug
derive gPrint MaybeError
instance SocketAddress SaInet where
	sa_serialize sa p w
		#! p = writeInt2 p 0 2
		#! p = writeInt2 p 2 (htons sa.sin_port)
		#! p = writeInt4 p 4 (maybe 0 toInt sa.sin_addr)
		= (p, forceEvalPointer p w)
	sa_deserialize p
		= Ok {sin_port=ntohs (readInt2Z p 2),sin_addr=Just (fromInt (readInt4Z p 4))}
	sa_length _ = 16
	sa_domain _ = 2
	sa_null = {sin_port=0, sin_addr=Nothing}
	
import Text.GenPrint
derive gPrint SaInet, Maybe
gPrint{|IPAddress|} a s = gPrint{|*|} (toString a) s
instance toString SaInet where toString s = printToString s

socket :: !SocketType !Int !*e -> *(!MaybeOSError *(Socket sa), !*e) | SocketAddress sa
socket type protocol w
	#! (sockfd, w) = socket` (sa_domain msa) (toInt type) protocol w
	#! (fd, sockfd) = getFd sockfd
	| fd == -1 = getLastOSError w
	= (Ok (coerce sockfd msa), w)
where
	msa = sa_null

	coerce :: *(Socket sa) sa -> *(Socket sa) | SocketAddress sa
	coerce x y = x

	socket` :: !Int !Int !Int !*e -> *(!*Int, !*e)
	socket` _ _ _ _ = code {
			ccall socket "III:I:A"
		}

bind :: !*(Socket sa) !sa -> *(!MaybeOSError (), !*(Socket sa)) | SocketAddress sa
bind sockfd addr
	#! p = malloc (sa_length addr)
	| p == 0 = getLastOSError sockfd
	#! (p, sockfd) = sa_serialize addr p sockfd
	#! len = sa_length addr
	#! (fd, sockfd) = getFd sockfd
	#! (r, sockfd) = bind` fd p len sockfd
	| r == -1 = getLastOSError sockfd
	#! r = free p
	| r == -1 = getLastOSError sockfd
	= (Ok (), sockfd)
where
	bind` :: !Int !Pointer !Int !*e -> *(!Int, !*e)
	bind` _ _ _ _ = code {
			ccall bind "IpI:I:A"
		}

listen :: !*(Socket sa) !Int -> *(!MaybeOSError (), !*(Socket sa)) | SocketAddress sa
listen sockfd backlog
	#! r = listen` sockfd backlog
	| r == -1 = getLastOSError sockfd
	= (Ok (), sockfd)
where
	listen` :: !Int !Int -> Int
	listen` _ _ = code {
			ccall listen "II:I"
		}

accept :: !*(Socket sa) -> *(!MaybeOSError (!*(Socket sa), !sa), !*(Socket sa)) | SocketAddress sa
accept sockfd
	# (fd, sockfd) = getFd sockfd
	# p1 = malloc 64
	# p2 = malloc 8
	# p2 = writeInt p2 0 64
	= case accept` fd p1 p2 sockfd of
		(-1, sockfd) = getLastOSError sockfd
		(sock, sockfd)
			#! (merr, p1) = readP sa_deserialize p1
			| isError merr = (Error (0, fromError merr), sockfd)
			#! r = free p1
			| r == -1 = getLastOSError sockfd
			#! r = free p2
			| r == -1 = getLastOSError sockfd
			= (Ok (sock, fromOk merr), sockfd)
where
	accept` :: !Int !Pointer !Int !*e -> *(!*Int, !*e)
	accept` _ _ _ _ = code {
			ccall accept "IpI:I:A"
		}

connect :: !*(Socket sa) !sa -> *(!MaybeOSError (), !*(Socket sa)) | SocketAddress sa
connect sockfd addr
	#! (p, sockfd) = mallocSt (sa_length addr) sockfd
	| p == 0 = getLastOSError sockfd
	#! (p, sockfd) = sa_serialize addr p sockfd
	#! (fd, sockfd) = getFd sockfd
	#! (r, sockfd) = connect` fd p (sa_length addr) sockfd
	#! sockfd = freeSt p sockfd
	| r == -1 = getLastOSError sockfd
	= (Ok (), sockfd)
where
	connect` :: !Int !Pointer !Int !*e -> *(!Int, !*e)
	connect` _ _ _ _ = code {
			ccall connect "IpI:I:A"
		}

close :: !*(Socket sa) !*e -> *(!MaybeOSError (), !*e) | SocketAddress sa
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

getFd :: !*(Socket sa) -> *(!Int, !*(Socket sa))
getFd s = code {
		push_b 0
	}
