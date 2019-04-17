implementation module System.Socket.Ipv4

import StdEnv
import Network.IP
import Data.Error
import System.Socket
import System._Pointer
import Text.GenPrint

instance SocketAddress SaInet where
	sa_serialize sa p w
		#! p = writeInt2 p 0 2
		#! p = writeInt2 p 2 (htons sa.sin_port)
		#! p = writeInt4 p 4 (maybe 0 toInt sa.sin_addr)
		= (p, forceEvalPointer p w)
	sa_deserialize p
		= Ok {sin_port=ntohs (readInt2Z p 2),sin_addr=Just (fromInt (readInt4Z p 4))}
	sa_length _ = 16
	sa_domain _ = 2
	sa_null = {sin_port=0, sin_addr=Nothing}

gPrint{|IPAddress|} a s = gPrint{|*|} (toString a) s
derive gPrint SaInet, Maybe
instance toString SaInet where toString s = printToString s
