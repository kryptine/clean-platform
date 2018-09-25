definition module TTY

from StdClass import class zero

:: *TTY (:== Int)

:: ByteSize = BytesizeFive | BytesizeSix | BytesizeSeven | BytesizeEight
:: Parity = ParityNone | ParityOdd | ParityEven | ParitySpace | ParityMark
:: BaudRate = B0 | B50 | B75 | B110 | B134 | B150 | B200 | B300 | B600 |
	B1200 | B1800 | B2400 | B4800 | B9600 | B19200 | B38400 | B57600 |
	B115200 | B230400

:: TTYSettings = {
		devicePath :: String,
		baudrate :: BaudRate,
		bytesize :: ByteSize,
		parity :: Parity,
		stop2bits :: Bool,
		xonxoff :: Bool,
		//Set this to 2 if you want to connect to a borked arduino
		sleepTime :: Int
	}

instance zero TTYSettings

/**
 * Returns a list of tty devices. This list is not conclusive but just checks familiar names.
 *
 * @param The world
 * @result The list of detected devices
 * @result The updated world
 */
getTTYDevices :: !*World -> *(![String], !*World)

/**
 * Smart constructor for {{`TTYSettings`}}
 *
 * @param devicePath
 * @param baudrate
 * @param parity
 * @param stop2bits
 * @param xonxoff
 * @param sleepTime
 */
makeTTYSettings :: String BaudRate ByteSize Parity Bool Bool Int -> TTYSettings

/**
 * Closes a TTY
 *
 * @param tty handle
 * @param world
 * @result Ok flag
 * @result new world
 */
TTYclose :: !*TTY !*env -> (!Bool, !*env)

/**
 * Reads the error from the tty library
 *
 * @param world
 * @result Error
 * @result new world
 */
TTYerror :: !*env -> (!String, !*env)

/**
 * Open a tty
 *
 * @param tty settings
 * @param world
 * @result Ok flag
 * @result TTY handle
 * @result new world
 */
TTYopen :: !TTYSettings !*env -> (!Bool,!*TTY,!*env)

/**
 * Read a byte from a tty
 *
 * @param tty handle
 * @param world
 * @result byte
 * @result new tty handle
 */
TTYread :: !*TTY -> (!Int, !*TTY)

/**
 * Read a line from the tty (up until '\n' or EOF)
 *
 * @param tty handle
 * @param world
 * @result line
 * @result new tty handle
 */
TTYreadline :: !*TTY -> (!String, !*TTY)
<<<<<<< HEAD

/**
* Checks if the TTY device is available for reading
*
* @param tty handle
* @result Data available
* @result Ok flag
* @result new tty handle
*/
TTYavailable :: !*TTY -> (!Bool, !Bool, !*TTY)

/**
* Write bytes to a TTY
*
* @param The bytes to write
* @param The tty handle
* @result new tty handle
*/
TTYwrite :: !String !*TTY -> *TTY
