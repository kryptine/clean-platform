definition module System.Socket.Ipv6

from StdOverloaded import class toString
from Network.IP import :: IPAddress
from StdMaybe import :: Maybe
from System.Socket import class SocketAddress

:: SaInet6 =
	{ sin6_port     :: !Int
	, sin6_flowinfo :: !Int
	, sin6_addr     :: !Maybe String
	, sin6_scope_id :: !Int
	}
instance SocketAddress SaInet6
instance toString SaInet6
