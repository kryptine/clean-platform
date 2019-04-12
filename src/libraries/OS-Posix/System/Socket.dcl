definition module System.Socket

from Data.Error import :: MaybeError
from Network.IP import :: IPAddress
from StdMaybe import :: Maybe
from System.OSError import :: MaybeOSError, :: OSError, :: OSErrorMessage, :: OSErrorCode

:: *Socket (:== Int)
:: SocketAddress
	= SaAfInet SaInet
:: SaInet =
	{ sin_port :: !Int
	, sin_addr :: Maybe IPAddress
	}

instance toString SocketAddress

:: SocketDomain = SD_AfInet
:: SocketType = ST_Stream | ST_DGram

socket :: !SocketDomain !SocketType !Int !*e -> *(MaybeOSError *Socket, !*e)
bind :: !*Socket !SocketAddress -> *(MaybeOSError (), !*Socket)
listen :: !*Socket !Int -> *(!MaybeOSError (), !*Socket)
accept :: !*Socket -> *(!MaybeOSError (!*Socket, !SocketAddress), !*Socket)
close :: !*Socket !*e -> *(!MaybeOSError (), !*e)

connect :: !*Socket !SocketAddress -> *(MaybeOSError (), !*Socket)

/*
 * Get access to the raw file descriptor
 */
getFd :: !*Socket -> *(!Int, !*Socket)
