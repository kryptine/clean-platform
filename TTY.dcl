definition module TTY

from StdClass import class zero

:: *TTY

:: ByteSize = BytesizeFive | BytesizeSix | BytesizeSeven | BytesizeEight
:: Parity = ParityNone | ParityOdd | ParityEven | ParitySpace | ParityMark
:: BaudRate = B0 | B50 | B75 | B110 | B134 | B150 | B200 | B300 | B600 |
	B1200 | B1800 | B2400 | B4800 | B9600 | B19200 | B38400 | B57600 |
	B115200 | B230400

:: TTYSettings = {
		baudrate :: BaudRate,
		bytesize :: ByteSize,
		parity :: Parity,
		stop2bits :: Bool,
		xonxoff :: Bool
	}

instance zero TTYSettings

TTYclose :: !*TTY !*env -> (!Bool, !*env)
TTYerror :: !*env -> (!String, !*env)
TTYopen :: !String !TTYSettings !*env -> (!Bool,!*TTY,!*env)
TTYreadline :: !*TTY -> (!String, !*TTY)
TTYavailable :: !*TTY -> (!Bool, !*TTY)
TTYwrite :: !String !*TTY -> *TTY
