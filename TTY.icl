implementation module TTY

import StdFunc
import StdList
import System.OS
import Text
import StdArray
import StdClass
import StdString
import StdChar
import Platform

import code from "ctty."

:: *TTY :== Int

instance zero TTYSettings where
	zero = {TTYSettings |
		devicePath = "/dev/ttyACM0",
		baudrate = B9600,
		bytesize = BytesizeEight,
		parity = ParityNone,
		stop2bits = False,
		xonxoff = False}

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

getTTYDevices :: !*World -> *(![String], !*World)
getTTYDevices w
	# (ds, w) = getDevices w
	= (IF_WINDOWS
		(filter (startsWith "COM") ds)
		(map ((+++) "/dev/") (filter isTTY ds))
	  , w)
where
	isTTY s = not (isEmpty (filter (flip startsWith s) ["tty", "rfcomm"]))

makeTTYSettings :: String BaudRate ByteSize Parity Bool Bool -> TTYSettings
makeTTYSettings dp br bs pr sb xx = {TTYSettings | devicePath=dp, baudrate=br,
	bytesize=bs, parity=pr, stop2bits=sb, xonxoff=xx}

TTYopen :: !TTYSettings !*env -> (!Bool, !*TTY, !*env)
TTYopen ts w = TTYopen2
	ts.devicePath
	(toInt ts.baudrate)
	(toInt ts.bytesize)
	(toInt ts.parity)
	ts.stop2bits
	ts.xonxoff
	w
	where
		TTYopen2 :: !String !Int !Int !Int !Bool !Bool !*env -> (!Bool, !*TTY, !*env)
		TTYopen2 _ _ _ _ _ _ _ = code {
				ccall ttyopen "SIIIII:VII:A"
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

TTYavailable :: !*TTY -> (!Bool, !*TTY)
TTYavailable _ = code {
		ccall ttyavailable "I:VII"
	}

TTYerror :: !*env -> (!String, !*env)
TTYerror _ = code {
		ccall ttyerror ":VS:A"
	}
