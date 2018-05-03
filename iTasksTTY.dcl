definition module iTasksTTY

from TTY import :: TTYSettings
import iTasks

derive class iTask TTYSettings

:: TTYException = TTYException String

/**
 * Synchronizes the channel share
 *
 * @param Device settings
 * @param Encoding function for messages to send
 * @param Streaming decoding function to decode received data
 * @param Channel SDS, first list are incoming messages, second list outgoing, third boolean is the stop flag
 * @result Task that stops when the stop flag is set
 * @throws TTYException
 */
syncSerialChannel :: TTYSettings (b -> String) (String -> (Either String [a], String)) (Shared ([a],[b],Bool)) -> Task () | iTask a & iTask b
