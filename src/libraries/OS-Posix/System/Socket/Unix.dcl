definition module System.Socket.Unix

from System.FilePath import :: FilePath(..)
from StdOverloaded import class toString
from System.Socket import class SocketAddress

:: SaUnix =
	{ sun_path :: !FilePath
	}
instance SocketAddress SaUnix
instance toString SaUnix