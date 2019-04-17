implementation module System.Socket

import StdEnv
import System._Socket => qualified socket, bind, listen, accept, close, connect, send, recv, htons, ntohs
import System.OSError

instance toInt SocketType where
	toInt ST_Stream = SOCK_STREAM
	toInt ST_DGram = SOCK_DGRAM

socket :: !SocketType !Int !*e -> *(!MaybeOSError *(Socket sa), !*e) | SocketAddress sa
socket a b c = 'System._Socket'.socket a b c

bind :: !sa !*(Socket sa) -> *(!MaybeOSError (), !*(Socket sa)) | SocketAddress sa
bind a b = 'System._Socket'.bind a b

listen :: !Int !*(Socket sa) -> *(!MaybeOSError (), !*(Socket sa)) | SocketAddress sa
listen a b = 'System._Socket'.listen a b

accept :: !*(Socket sa) -> *(!MaybeOSError (!*(Socket sa), !sa), !*(Socket sa)) | SocketAddress sa
accept a = 'System._Socket'.accept a

close :: !*(Socket sa) !*e -> *(!MaybeOSError (), !*e) | SocketAddress sa
close a b = 'System._Socket'.close a b

connect :: !sa !*(Socket sa) -> *(!MaybeOSError (), !*(Socket sa)) | SocketAddress sa
connect a b = 'System._Socket'.connect a b

send :: !String !Int !*(Socket sa) -> *(!MaybeOSError Int, !*(Socket sa))
send a b c = 'System._Socket'.send a b c

recv :: !Int !Int !*(Socket sa) -> *(!MaybeOSError String, !*(Socket sa))
recv a b c = 'System._Socket'.recv a b c

ntohs :: !Int -> Int
ntohs a = 'System._Socket'.ntohs a

htons :: !Int -> Int
htons a = 'System._Socket'.htons a
