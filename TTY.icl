implementation module TTY

import StdClass

import code from "ctty."

:: *TTY :== Int

instance zero TTYSettings where
	zero = {TTYSettings |
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

TTYopen :: !String !TTYSettings !*env -> (!Bool, !*TTY, !*env)
TTYopen fn ts w = TTYopen2
	fn
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

TTYreadline :: !*TTY -> (!String, !*TTY)
TTYreadline _ = code {
		ccall ttyreadline "I:VSI"
	}

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
