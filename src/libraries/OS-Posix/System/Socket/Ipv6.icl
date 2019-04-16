implementation module System.Socket.Ipv6

import StdEnv
import Data.Error
import Data.Maybe
import System.Socket
import System._Pointer
import Text.GenPrint

instance SocketAddress SaInet6 where
	sa_serialize sa p w
		#! p = writeInt2 p 0 (sa_domain sa)
		#! p = writeInt2 p 2 (htons sa.sin6_port)
		#! p = writeInt4 p 4 (sa.sin6_flowinfo)
		#! p = writeCharArray (p+8) (pad16 (fromMaybe "::" sa.sin6_addr))
		#! p = writeInt4 p 24 (sa.sin6_scope_id)
		= (p, forceEvalPointer p w)
	where
		pad16 s = s +++ {'\0'\\_<-[0..16-1-size s]}
	sa_deserialize p = Ok
		{ sin6_port     = ntohs (readInt2Z p 2)
		, sin6_flowinfo = readInt4Z p 4
		, sin6_addr     = Just (derefCharArray (p+8) 16)
		, sin6_scope_id = readInt4Z p 24
		}
	sa_length _ = 28
	sa_domain _ = 10
	sa_null = {sin6_port=0,sin6_flowinfo=0,sin6_addr=Nothing,sin6_scope_id=0}

derive gPrint SaInet6, Maybe
instance toString SaInet6 where toString s = printToString s
