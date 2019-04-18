definition module System._Socket

from Data.Error import :: MaybeError
from System.OSError import :: MaybeOSError, :: OSError, :: OSErrorMessage, :: OSErrorCode
from System.Socket import :: SocketType, class SocketAddress

:: *Socket a

AF_INET :== 2
AF_UNIX :== 1
AF_INET6 :== 10
AF_IPX :== 4
AF_APPLETALK :== 5
AF_IRDA :== 23

SOCK_STREAM :== 1
SOCK_DGRAM :== 2

socket :: !SocketType !Int !*env -> *(!MaybeOSError *(Socket sa), !*env) | SocketAddress sa
bind :: !sa !*(Socket sa) -> *(!MaybeOSError (), !*Socket sa) | SocketAddress sa
listen :: !Int !*(Socket sa) -> *(!MaybeOSError (), !*Socket sa) | SocketAddress sa
accept :: !*(Socket sa) -> *(!MaybeOSError (!*Socket sa, !sa), !*Socket sa) | SocketAddress sa
close :: !*(Socket sa) !*env -> *(!MaybeOSError (), !*env) | SocketAddress sa

connect :: !sa !*(Socket sa) -> *(!MaybeOSError (), !*Socket sa) | SocketAddress sa

send :: !String !Int !*(Socket sa) -> *(!MaybeOSError Int, !*Socket sa)
recv :: !Int !Int !*(Socket sa) -> *(!MaybeOSError String, !*Socket sa)

ntohs :: !Int -> Int
htons :: !Int -> Int
