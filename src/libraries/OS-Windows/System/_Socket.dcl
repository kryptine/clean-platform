definition module System._Socket

from Data.Error import :: MaybeError
from System.OSError import :: MaybeOSError, :: OSError, :: OSErrorMessage, :: OSErrorCode
from System.Socket import :: SocketType, class SocketAddress

:: *Socket a

AF_INET :== 2
AF_INET6 :== 23
AF_IPX :== 6
AF_APPLETALK :== 16
AF_NETBIOS :== 17
AF_IRDA :== 26
AF_BTH :== 32

SOCK_STREAM :== 1
SOCK_DGRAM :== 2
SOCK_RAW :== 3
SOCK_RDM :== 4
SOCK_SEQPACKET :== 5

socket :: !SocketType !Int !*e -> *(!MaybeOSError *(Socket sa), !*e) | SocketAddress sa
bind :: !sa !*(Socket sa) -> *(!MaybeOSError (), !*(Socket sa)) | SocketAddress sa
listen :: !Int !*(Socket sa) -> *(!MaybeOSError (), !*(Socket sa)) | SocketAddress sa
accept :: !*(Socket sa) -> *(!MaybeOSError (!*(Socket sa), !sa), !*(Socket sa)) | SocketAddress sa
close :: !*(Socket sa) !*e -> *(!MaybeOSError (), !*e) | SocketAddress sa

connect :: !sa !*(Socket sa) -> *(!MaybeOSError (), !*(Socket sa)) | SocketAddress sa

send :: !String !Int !*(Socket sa) -> *(!MaybeOSError Int, !*(Socket sa))
recv :: !Int !Int !*(Socket sa) -> *(!MaybeOSError String, !*(Socket sa))

ntohs :: !Int -> Int
htons :: !Int -> Int
