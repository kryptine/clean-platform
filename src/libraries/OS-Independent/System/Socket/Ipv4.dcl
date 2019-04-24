definition module System.Socket.Ipv4

from StdOverloaded import class toString
from Network.IP import :: IPAddress
from StdMaybe import :: Maybe
from System.Socket import class SocketAddress

:: Ipv4SocketAddress =
	{ sin_port :: !Int
	, sin_addr :: !Maybe IPAddress
	}
instance SocketAddress Ipv4SocketAddress
instance toString Ipv4SocketAddress
