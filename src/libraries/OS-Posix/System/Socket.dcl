definition module System.Socket

from StdOverloaded import class zero, class toString
from Data.Error import :: MaybeError, :: MaybeErrorString
from Network.IP import :: IPAddress
from StdMaybe import :: Maybe
from System.OSError import :: MaybeOSError, :: OSError, :: OSErrorMessage, :: OSErrorCode
from System._Pointer import :: Pointer

:: *Socket a (:== Int)
:: SaInet =
	{ sin_port :: !Int
	, sin_addr :: Maybe IPAddress
	}
:: SaInet6 =
	{ sin6_port     :: !Int
	, sin6_flowinfo :: !Int
	, sin6_addr     :: !String
	, sin6_scope_id :: !Int
	}

:: SocketDomain = SD_AfInet | SD_AfInet6
:: SocketType = ST_Stream | ST_DGram

class SocketAddress sa where
	sa_length      :: !sa -> Int
	sa_serialize   :: !sa !Pointer !*e -> *(Pointer, !*e)
	sa_deserialize :: !Pointer -> MaybeErrorString sa
	sa_domain      :: !sa -> Int
	sa_null        :: sa
instance SocketAddress SaInet
instance toString SaInet

socket :: !SocketType !Int !*e -> *(MaybeOSError *(Socket sa), !*e) | SocketAddress sa
bind :: !*(Socket sa) !sa -> *(MaybeOSError (), !*(Socket sa)) | SocketAddress sa
listen :: !*(Socket sa) !Int -> *(!MaybeOSError (), !*(Socket sa)) | SocketAddress sa
accept :: !*(Socket sa) -> *(!MaybeOSError (!*(Socket sa), !sa), !*(Socket sa)) | SocketAddress sa
close :: !*(Socket sa) !*e -> *(!MaybeOSError (), !*e) | SocketAddress sa

connect :: !*(Socket sa) !sa -> *(MaybeOSError (), !*(Socket sa)) | SocketAddress sa


/*
 * Get access to the raw file descriptor
 */
getFd :: !*(Socket sa) -> *(!Int, !*(Socket sa)) | SocketAddress sa
