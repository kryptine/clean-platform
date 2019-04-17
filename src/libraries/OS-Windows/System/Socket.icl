implementation module System.Socket

import StdEnv
import Data.Error
import System.OSError
import System._Pointer

import code from library "Ws2_32"

:: *Socket a :== Int

instance toInt SocketType where
	toInt ST_Stream = 1
	toInt ST_DGram = 2

getLastWSAError :: !*e -> *(MaybeOSError .a, !*e)
getLastWSAError w
	#! (r, w) = WSAGetLastError` w
	#! (p, w) = mallocSt 8 w
	#! msg = case r of
		6 = "Specified event object handle is invalid."
		8 = "Insufficient memory available."
		87 = "One or more parameters are invalid."
		995 = "Overlapped operation aborted."
		996 = "Overlapped I/O event object not in signaled state."
		997  = "Overlapped operations will complete later."
		10004  = "Interrupted function call."
		10009  = "File handle is not valid."
		10013  = "Permission denied."
		10014  = "Bad address."
		10022  = "Invalid argument."
		10024  = "Too many open files."
		10035  = "Resource temporarily unavailable."
		10036  = "Operation now in progress."
		10037  = "Operation already in progress."
		10038  = "Socket operation on nonsocket."
		10039  = "Destination address required."
		10040  = "Message too long."
		10041  = "Protocol wrong type for socket."
		10042  = "Bad protocol option."
		10043  = "Protocol not supported."
		10044  = "Socket type not supported."
		10045  = "Operation not supported."
		10046  = "Protocol family not supported."
		10047  = "Address family not supported by protocol family."
		10048  = "Address already in use."
		10049  = "Cannot assign requested address."
		10050  = "Network is down."
		10051  = "Network is unreachable."
		10052  = "Network dropped connection on reset."
		10053  = "Software caused connection abort."
		10054  = "Connection reset by peer."
		10055  = "No buffer space available."
		10056  = "Socket is already connected."
		10057  = "Socket is not connected."
		10058  = "Cannot send after socket shutdown."
		10059  = "Too many references."
		10060  = "Connection timed out."
		10061  = "Connection refused."
		10062  = "Cannot translate name."
		10063  = "Name too long."
		10064  = "Host is down."
		10065  = "No route to host."
		10066  = "Directory not empty."
		10067  = "Too many processes."
		10068  = "User quota exceeded."
		10069  = "Disk quota exceeded."
		10070  = "Stale file handle reference."
		10071  = "Item is remote."
		10091  = "Network subsystem is unavailable."
		10092  = "Winsock.dll version out of range."
		10093  = "Successful WSAStartup not yet performed."
		10101  = "Graceful shutdown in progress."
		10102  = "No more results."
		10103  = "Call has been canceled."
		10104  = "Procedure call table is invalid."
		10105  = "Service provider is invalid."
		10106  = "Service provider failed to initialize."
		10107  = "System call failure."
		10108  = "Service not found."
		10109  = "Class type not found."
		10110  = "No more results."
		10111  = "Call was canceled."
		10112  = "Database query was refused."
		11001  = "Host not found."
		11002  = "Nonauthoritative host not found."
		11003  = "This is a nonrecoverable error."
		11004  = "Valid name, no data record of requested type."
		11005  = "QoS receivers."
		11006  = "QoS senders."
		11007  = "No QoS senders."
		11008  = "QoS no receivers."
		11009  = "QoS request confirmed."
		11010  = "QoS admission error."
		11011  = "QoS policy failure."
		11012  = "QoS bad style."
		11013  = "QoS bad object."
		11014  = "QoS traffic control error."
		11015  = "QoS generic error."
		11016  = "QoS service type error."
		11017  = "QoS flowspec error."
		11018  = "Invalid QoS provider buffer."
		11019  = "Invalid QoS filter style."
		11020  = "Invalid QoS filter type."
		11021  = "Incorrect QoS filter count."
		11022  = "Invalid QoS object length."
		11023  = "Incorrect QoS flow count."
		11024  = "Unrecognized QoS object."
		11025  = "Invalid QoS policy object."
		11026  = "Invalid QoS flow descriptor."
		11027  = "Invalid QoS provider-specific flowspec."
		11028  = "Invalid QoS provider-specific filterspec."
		11029  = "Invalid QoS shape discard mode object."
		11030  = "Invalid QoS shaping rate object."
		11031  = "Reserved policy QoS element type."
	= (Error (r, msg), w)
where
	WSAGetLastError` :: !*env -> *(!Int, !*e)
	WSAGetLastError` _ = code {
			ccall WSAGetLastError@0 "P:I:A"
		}

socket :: !SocketType !Int !*e -> *(!MaybeOSError *(Socket sa), !*e) | SocketAddress sa
socket type protocol w
	#! (p, w) = mallocSt 2048 w
	#! (r, w) = WSAStartup (2 * 256 + 2) p w
	| r <> 0 = getLastWSAError w
	#! w = freeSt p w
	#! (sockfd, w) = socket` (sa_domain msa) (toInt type) protocol w
	#! (fd, sockfd) = getFd sockfd
	| fd == -1 = getLastWSAError w
	= (Ok (coerce sockfd msa), w)
where
	msa = sa_null

	coerce :: *(Socket sa) sa -> *(Socket sa) | SocketAddress sa
	coerce x y = x

	socket` :: !Int !Int !Int !*e -> *(!*Int, !*e)
	socket` _ _ _ _ = code {
			ccall socket@12 "PIII:I:A"
		}
		
	WSAStartup :: !Int !Pointer !*e -> *(!Int, !*e)
	WSAStartup _ _ _ = code {
			ccall WSAStartup@8 "PIp:I:A"
		}
		
import System._WinBase
mallocSt :: !Int !*env -> *(!Pointer, !*env)
mallocSt size e
	#! (heap, e) = getProcessHeap e 
	= heapAlloc heap 0 size e

freeSt :: !Pointer !*env -> *env
freeSt p e
	#! (heap, e) = getProcessHeap e
	#! (ok, e) = heapFree heap p 0 e
	= e

bind :: !sa !*(Socket sa) -> *(!MaybeOSError (), !*(Socket sa)) | SocketAddress sa
bind addr sockfd
	#! (p, sockfd) = mallocSt (sa_length addr) sockfd
	| p == 0 = getLastOSError sockfd
	#! (p, sockfd) = sa_serialize addr p sockfd
	#! len = sa_length addr
	#! (fd, sockfd) = getFd sockfd
	#! (r, sockfd) = bind` fd p len sockfd
	| r == -1 = getLastWSAError sockfd
	#! sockfd = freeSt p sockfd
	= (Ok (), sockfd)
where
	bind` :: !Int !Pointer !Int !*e -> *(!Int, !*e)
	bind` _ _ _ _ = code {
			ccall bind@12 "PIpI:I:A"
		}

listen :: !Int !*(Socket sa) -> *(!MaybeOSError (), !*(Socket sa)) | SocketAddress sa
listen backlog sockfd
	#! r = listen` sockfd backlog
	| r == -1 = getLastWSAError sockfd
	= (Ok (), sockfd)
where
	listen` :: !Int !Int -> Int
	listen` _ _ = code {
			ccall listen@8 "PII:I"
		}

accept :: !*(Socket sa) -> *(!MaybeOSError (!*(Socket sa), !sa), !*(Socket sa)) | SocketAddress sa
accept sockfd
	# (fd, sockfd) = getFd sockfd
	# (p1, sockfd) = mallocSt 64 sockfd
	# (p2, sockfd) = mallocSt 8 sockfd
	# p2 = writeInt p2 0 64
	= case accept` fd p1 p2 sockfd of
		(-1, sockfd) = getLastWSAError sockfd
		(sock, sockfd)
			#! (merr, p1) = readP sa_deserialize p1
			| isError merr = (Error (0, fromError merr), sockfd)
			#! sockfd = freeSt p1 sockfd
			#! sockfd = freeSt p2 sockfd
			= (Ok (sock, fromOk merr), sockfd)
where
	accept` :: !Int !Pointer !Int !*e -> *(!*Int, !*e)
	accept` _ _ _ _ = code {
			ccall accept@12 "PIpI:I:A"
		}

connect :: !sa !*(Socket sa) -> *(!MaybeOSError (), !*(Socket sa)) | SocketAddress sa
connect addr sockfd
	#! (p, sockfd) = mallocSt (sa_length addr) sockfd
	| p == 0 = getLastOSError sockfd
	#! (p, sockfd) = sa_serialize addr p sockfd
	#! (fd, sockfd) = getFd sockfd
	#! (r, sockfd) = connect` fd p (sa_length addr) sockfd
	| r == -1 = getLastWSAError sockfd
	#! sockfd = freeSt p sockfd
	= (Ok (), sockfd)
where
	connect` :: !Int !Pointer !Int !*e -> *(!Int, !*e)
	connect` _ _ _ _ = code {
			ccall connect@12 "PIpI:I:A"
		}

send :: !String !Int !*(Socket sa) -> *(!MaybeOSError Int, !*(Socket sa))
send data flags sockfd
	#! (fd, sockfd) = getFd sockfd
	#! (r, sockfd) = send` fd (packString data) (size data) flags sockfd
	| r == -1 = getLastWSAError sockfd
	= (Ok r, sockfd)
where
	send` :: !Int !String !Int !Int !*e -> *(!Int, !*e)
	send` _ _ _ _ _ = code {
			ccall send@16 "PIsII:I:A"
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
			ccall recv@16 "PIpII:I:A"
		}

close :: !*(Socket sa) !*e -> *(!MaybeOSError (), !*e) | SocketAddress sa
close sock w
	# (r, w) = close` sock w
	| r == -1 = getLastOSError w
	# w = WSACleanup w
	= (Ok (), w)
where
	close` :: !Int !*e -> *(!Int, !*e)
	close` _ _ = code {
			ccall closesocket@4 "PI:I:A"
		}
		
	WSACleanup :: !*e -> *e
	WSACleanup _ = code {
			ccall WSACleanup@0 "P:V:A"
		}

htons :: !Int -> Int
htons x = code {
		ccall htons@4 "PI:I"
	}

ntohs :: !Int -> Int
ntohs x = code {
		ccall ntohs@4 "PI:I"
	}

getFd :: !*(Socket sa) -> *(!Int, !*(Socket sa))
getFd s = code {
		push_b 0
	}
