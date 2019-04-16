implementation module System.Socket

import StdEnv
import Data.Error
import System.OSError
import System._Pointer
import System._Posix

:: *Socket a :== Int

instance toInt SocketType where
	toInt ST_Stream = 1
	toInt ST_DGram = 2

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

bind :: !sa !*(Socket sa) -> *(!MaybeOSError (), !*(Socket sa)) | SocketAddress sa
bind addr sockfd
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

listen :: !Int !*(Socket sa) -> *(!MaybeOSError (), !*(Socket sa)) | SocketAddress sa
listen backlog sockfd
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

connect :: !sa !*(Socket sa) -> *(!MaybeOSError (), !*(Socket sa)) | SocketAddress sa
connect addr sockfd
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

send :: !String !Int !*(Socket sa) -> *(!MaybeOSError Int, !*(Socket sa))
send data flags sockfd
	#! (fd, sockfd) = getFd sockfd
	#! (r, sockfd) = send` fd (packString data) (size data) flags sockfd
	| r == -1 = getLastOSError sockfd
	= (Ok r, sockfd)
where
	send` :: !Int !String !Int !Int !*e -> *(!Int, !*e)
	send` _ _ _ _ _ = code {
			ccall send "IsII:I:A"
		}

recv :: !Int !Int !*(Socket sa) -> *(!MaybeOSError String, !*(Socket sa))
recv length flags sockfd
	#! (p, sockfd) = mallocSt length sockfd
	#! (fd, sockfd) = getFd sockfd
	#! (r, sockfd) = recv` fd p length flags sockfd
	| r == -1 = getLastOSError sockfd
	#! (s, p) = readP derefString p
	#! sockfd = freeSt p sockfd
	= (Ok s, sockfd)
	
where
	recv` :: !Int !Pointer !Int !Int !*e -> *(!Int, !*e)
	recv` _ _ _ _ _ = code {
			ccall recv "IpII:I:A"
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
