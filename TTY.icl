implementation module TTY

import Data.Maybe

import StdFunc
import StdFile
import StdMisc
import StdBool
import StdString

import code from "tty."

:: *TTY :== Int

instance zero TTYSettings where
	zero = {TTYSettings |
		baudrate = B9600,
		bytesize = BytesizeEight,
		parity = ParityNone,
		stopbits = StopbitsOne,
		xonxoff = False}

instance toInt BaudRate where
	toInt B0 = 0
	toInt B50 = 1
	toInt B75 = 2
	toInt B110 = 3
	toInt B134 = 4
	toInt B150 = 5
	toInt B200 = 6
	toInt B300 = 7
	toInt B600 = 8
	toInt B1200 = 9
	toInt B1800 = 10
	toInt B2400 = 11
	toInt B4800 = 12
	toInt B9600 = 13
	toInt B19200 = 14
	toInt B38400 = 15
	toInt B57600 = 16
	toInt B115200 = 17
	toInt B230400 = 18

instance toInt ByteSize where
	toInt BytesizeFive = 0
	toInt BytesizeSix = 1
	toInt BytesizeSeven = 2
	toInt BytesizeEight = 3

instance toInt Parity where
	toInt ParityNone = 0
	toInt ParityEven = 1
	toInt ParityOdd = 2
	toInt ParityMark = 3
	toInt ParitySpace = 4

instance toInt StopBits where
	toInt StopbitsOne = 0
	toInt StopbitsOnePointFive = 1
	toInt StopbitsTwo = 2

TTYopen :: !String !TTYSettings !*env -> (!Bool, !*TTY, !*env)
TTYopen fn ts w = TTYopen2
	fn
	(toInt ts.baudrate)
	(toInt ts.bytesize)
	(toInt ts.parity)
	(toInt ts.stopbits)
	ts.xonxoff
	w
	where
		TTYopen2 :: !String !Int !Int !Int !Int !Bool !*env -> (!Bool, !*TTY, !*env)
		TTYopen2 _ _ _ _ _ _ _ = code {
				ccall ttyopen "SIIIII:VII:A"
			}

TTYclose :: !*TTY !*env -> (!Bool, !*env)
TTYclose f w = code {
		ccall ttyclose "I:I:A"
	}

TTYerror :: !*env -> (!String, !*env)
TTYerror w = code {
		ccall ttyerror ":S:A"
	}

TTYreadc :: !*TTY -> (!Char, !*TTY)
TTYreadc w = code {
		ccall ttyreadc "I:VII"
	}

TTYreadline :: !*TTY -> (!String, !*TTY)
TTYreadline t = code {
		ccall ttyreadline "I:VSI"
}

Start :: *World -> (!String, *World)
Start w
#! (ok, tty, w) = TTYopen "/dev/ttyUSB0" zero w
| not ok = TTYerror w
#! (c, tty) = TTYreadline tty
#! (ok, w) = TTYclose tty w
| not ok = TTYerror w
#! (s, w) = TTYerror w
= (c, w)
