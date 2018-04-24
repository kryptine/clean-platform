implementation module Platform

import code from library "CleanSerial_library"

import System._Pointer
import System._WinBase

import StdMisc, StdDebug, StdBool, StdString
import Text

getDevices :: !*World -> *([String], !*World)
getDevices w
	# (ph, w) = getProcessHeap w
	# (ptr, w) = heapAlloc ph 0 40960 w
	# (ret, w) = realQDD 0 ptr 40960 w
	| ret == 0
		# (err, w) = getLastError w
		= abort ("error in QueryDosDevice: " +++ toString err)
	#! res = derefCharArray ptr ret
	# (ok, w) = heapFree ph 0 ptr w
	= (split "\0" res, w)

realQDD :: !Pointer !Pointer !Int !*env -> *(!Int, !*env)
realQDD _ _ _ _	= code {
		ccall QueryDosDeviceA@12 "PppI:I:A"
	}
