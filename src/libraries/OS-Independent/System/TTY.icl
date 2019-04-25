implementation module System.TTY

import StdEnv

import Data.Error
import System.OSError
import System.OS
import Text

import System._Devices

import code from "tty.o"

:: *TTY :== Int

instance zero TTYSettings where
	zero = {TTYSettings |
		devicePath = "/dev/ttyACM0",
		baudrate = B9600,
		bytesize = BytesizeEight,
		parity = ParityNone,
		stop2bits = False,
		xonxoff = False,
		sleepTime = 2}

instance toInt BaudRate where
	toInt b = case b of
		B0 = 0; B50 = 1; B75 = 2; B110 = 3; B134 = 4; B150 = 5; B200 = 6
		B300 = 7; B600 = 8; B1200 = 9; B1800 = 10; B2400 = 11; B4800 = 12
		B9600 = 13; B19200 = 14; B38400 = 15; B57600 = 16; B115200 = 17
		B230400 = 18

instance toInt ByteSize where
	toInt b = case b of
		BytesizeFive = 0; BytesizeSix = 1; BytesizeSeven = 2; BytesizeEight = 3

instance toInt Parity where
	toInt p = case p of
		ParityNone = 0; ParityOdd = 1; ParityEven = 2; ParitySpace = 3;
		ParityMark = 4

getTTYDevices :: !*World -> *(MaybeOSError [String], !*World)
getTTYDevices w
	# (ds, w) = getDevices w
	= case ds of
		(Error e) = (Error e, w)
		(Ok ds) = (Ok (
			IF_WINDOWS
				(filter isTTYWindows ds)
				(map ((+++) "/dev/") (filter isTTYPosix ds)))
			, w)
where
	isTTYPosix s = not (isEmpty (filter (flip startsWith s) ["tty", "rfcomm", "cu"]))
	isTTYWindows s = startsWith "COM" s && size s > 3 && isDigit s.[3]

makeTTYSettings :: String BaudRate ByteSize Parity Bool Bool Int -> TTYSettings
makeTTYSettings dp br bs pr sb xx st = {TTYSettings | devicePath=dp, baudrate=br,
	bytesize=bs, parity=pr, stop2bits=sb, xonxoff=xx, sleepTime=st}

TTYopen :: !TTYSettings !*env -> (!Bool, !*TTY, !*env)
TTYopen ts w = TTYopen2
	ts.devicePath
	(toInt ts.baudrate)
	(toInt ts.bytesize)
	(toInt ts.parity)
	ts.stop2bits
	ts.xonxoff
	ts.sleepTime
	w
	where
		TTYopen2 :: !String !Int !Int !Int !Bool !Bool !Int !*env -> (!Bool, !*TTY, !*env)
		TTYopen2 _ _ _ _ _ _ _ _ = code {
				ccall ttyopen "SIIIIII:VII:A"
			}

TTYclose :: !*TTY !*env -> (!Bool, !*env)
TTYclose _ _ = code {
		ccall ttyclose "I:I:A"
	}

TTYread :: !*TTY -> (!Int, !*TTY)
TTYread _ = code {
		ccall ttyread "I:VII"
	}

TTYreadline :: !*TTY -> (!String, !*TTY)
TTYreadline tty = case TTYread tty of
	(10, tty) = ("", tty)
	(c, tty)
	# (rest, tty) = TTYreadline tty
	= ({#toChar c} +++ rest, tty)

TTYwrite :: !String !*TTY -> *TTY
TTYwrite _ _ = code {
		ccall ttywrite "SI:I"
	}

TTYavailable :: !*TTY -> (!Bool, !Bool, !*TTY)
TTYavailable _ = code {
		ccall ttyavailable "I:VIII"
	}

TTYerror :: !*env -> (!String, !*env)
TTYerror _ = code {
		ccall ttyerror ":VS:A"
	}
