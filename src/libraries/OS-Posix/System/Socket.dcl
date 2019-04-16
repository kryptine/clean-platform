definition module System.Socket

from Data.Error import :: MaybeError, :: MaybeErrorString
from System.OSError import :: MaybeOSError, :: OSError, :: OSErrorMessage, :: OSErrorCode
from System._Pointer import :: Pointer

:: *Socket a (:== Int)

:: SocketType = ST_Stream | ST_DGram

class SocketAddress sa where
	sa_length      :: !sa -> Int
	sa_serialize   :: !sa !Pointer !*e -> *(!Pointer, !*e)
	sa_deserialize :: !Pointer -> MaybeErrorString sa
	sa_domain      :: !sa -> Int
	sa_null        :: sa

socket :: !SocketType !Int !*e -> *(!MaybeOSError *(Socket sa), !*e) | SocketAddress sa
bind :: !sa !*(Socket sa) -> *(!MaybeOSError (), !*(Socket sa)) | SocketAddress sa
listen :: !Int !*(Socket sa) -> *(!MaybeOSError (), !*(Socket sa)) | SocketAddress sa
accept :: !*(Socket sa) -> *(!MaybeOSError (!*(Socket sa), !sa), !*(Socket sa)) | SocketAddress sa
close :: !*(Socket sa) !*e -> *(!MaybeOSError (), !*e) | SocketAddress sa

connect :: !sa !*(Socket sa) -> *(!MaybeOSError (), !*(Socket sa)) | SocketAddress sa

send :: !String !Int !*(Socket sa) -> *(!MaybeOSError Int, !*(Socket sa))
recv :: !Int !Int !*(Socket sa) -> *(!MaybeOSError String, !*(Socket sa))

/*
 * Get access to the raw file descriptor
 */
getFd :: !*(Socket sa) -> *(!Int, !*(Socket sa))

ntohs :: !Int -> Int
htons :: !Int -> Int
