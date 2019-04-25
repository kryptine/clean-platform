implementation module System._Devices

import code from library "msvcrt.txt"

import System._Pointer
import System._WinBase
import Data.Error
import System.OSError

import StdMisc, StdDebug, StdBool, StdString
import Text

getDevices :: !*World -> *(MaybeOSError [String], !*World)
getDevices w
	# (ph, w) = getProcessHeap w
	# (ptr, w) = heapAlloc ph 0 40960 w
	| ptr == 0 = getLastOSError w
	# (ret, w) = realQDD 0 ptr 40960 w
	| ret == 0
		= getLastOSError w
	#! res = derefCharArray ptr ret
	# (ok, w) = heapFree ph 0 ptr w
	| not ok = getLastOSError w
	= (Ok (split "\0" res), w)

realQDD :: !Pointer !Pointer !Int !*env -> *(!Int, !*env)
realQDD _ _ _ _	= code {
		ccall QueryDosDeviceA@12 "PppI:I:A"
	}
