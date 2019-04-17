implementation module System.Socket.Unix

import StdEnv
import Data.Error
import System.Socket
import System._Pointer

instance SocketAddress SaUnix where
	sa_serialize sa p w
		#! p = writeInt2 p 0 (sa_domain sa)
		#! p = writeCharArray (p+2) (packString sa.sun_path)
		= (p, forceEvalPointer p w)
	sa_deserialize p
		= Ok {sun_path=derefString (p+2)}
	sa_length _ = 110
	sa_domain _ = AF_UNIX
	sa_null = {sin_path="/"}

instance toString SaInet where toString s = s.sun_path
