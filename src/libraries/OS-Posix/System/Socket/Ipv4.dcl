definition module System.Socket.Ipv4

from StdOverloaded import class toString
from Network.IP import :: IPAddress
from StdMaybe import :: Maybe
from System.Socket import class SocketAddress

:: SaInet =
	{ sin_port :: !Int
	, sin_addr :: !Maybe IPAddress
	}
instance SocketAddress SaInet
instance toString SaInet
