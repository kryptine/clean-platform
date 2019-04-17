definition module System.Socket.Ipv4

from StdOverloaded import class toString
//from Data.Error import :: MaybeError, :: MaybeErrorString
from Network.IP import :: IPAddress
from StdMaybe import :: Maybe
from System.Socket import class SocketAddress
//from System.OSError import :: MaybeOSError, :: OSError, :: OSErrorMessage, :: OSErrorCode
//from System._Pointer import :: Pointer

:: SaInet =
	{ sin_port :: !Int
	, sin_addr :: !Maybe IPAddress
	}
instance SocketAddress SaInet
instance toString SaInet
