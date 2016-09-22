definition module TTY

from Data.Maybe import :: Maybe
from StdFile import class FileSystem
from StdClass import class zero

:: *TTY

:: ByteSize = BytesizeFive | BytesizeSix | BytesizeSeven | BytesizeEight
:: Parity = ParityNone | ParityEven | ParityOdd | ParityMark | ParitySpace
:: StopBits = StopbitsOne | StopbitsOnePointFive | StopbitsTwo
:: BaudRate = B0 | B50 | B75 | B110 | B134 | B150 | B200 | B300 | B600 |
	B1200 | B1800 | B2400 | B4800 | B9600 | B19200 | B38400 | B57600 |
	B115200 | B230400

:: TTYSettings = {
		baudrate :: BaudRate,
		bytesize :: ByteSize,
		parity :: Parity,
		stopbits :: StopBits,
		xonxoff :: Bool
	}

instance zero TTYSettings

TTYopen :: !String !TTYSettings !*env -> (!Bool,!*TTY,!*env)

TTYclose :: !*TTY !*env -> (!Bool, !*env)

TTYreadc :: !*TTY -> (!Char, !*TTY)

TTYreadline :: !*TTY -> (!String, !*TTY)

TTYerror :: !*env -> (!String, !*env)
