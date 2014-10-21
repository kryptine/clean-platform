implementation module System.OSError

import Data.Error

import StdArray
import StdClass
import StdInt
import StdMisc
import StdString

from System._Windows import 
	FORMAT_MESSAGE_ALLOCATE_BUFFER, 
	FORMAT_MESSAGE_FROM_SYSTEM, 
	FORMAT_MESSAGE_IGNORE_INSERTS,
	LANGUAGE_NEUTRAL_SUBLANG_DEFAULT, 
	NULL
from System._Windows import qualified getLastError, formatMessageA, localFree
import System._Pointer

getLastOSError :: *World -> (MaybeOSError .a, *World)
getLastOSError world
# (errorCode, world) = 'System._Windows'.getLastError world
= (Error (errorCode, formatMessage errorCode), world)

formatMessage :: !Int -> String
formatMessage errorCode
	# msgBuf = createArray 1 0
	# ok = 'System._Windows'.formatMessageA
        (FORMAT_MESSAGE_ALLOCATE_BUFFER bitor FORMAT_MESSAGE_FROM_SYSTEM bitor FORMAT_MESSAGE_IGNORE_INSERTS)
        NULL
        errorCode
        LANGUAGE_NEUTRAL_SUBLANG_DEFAULT
        msgBuf
        0
        NULL
     | ok <> ok = undef						//Force eval of ok
     # message = derefString msgBuf.[0]
     | size message <> size message = undef	//Force eval of message
     # hMem = 'System._Windows'.localFree msgBuf.[0]
     | hMem <> hMem = undef					//Force eval of hMem
     = message % (0, size message - 3)		//Strip CR+LF

getLastOSErrorCode :: *World -> (MaybeOSErrorCode .a, *World)
getLastOSErrorCode world
# (errorCode, world) = 'System._Windows'.getLastError world
= (Error errorCode, world)
