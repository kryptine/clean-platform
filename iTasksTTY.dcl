definition module iTasksTTY

from iTasks.Internal.Generic.Visualization import generic gText, :: TextFormat
from iTasks.SDS.Definition import :: SDS, :: Shared, :: RWShared
from iTasks.UI.Editor.Generic import generic gEditor, :: Editor
from iTasks.WF.Definition import :: Task
from iTasks.WF.Definition import class iTask

from Data.GenDefault import generic gDefault
from Data.GenEq import generic gEq
from Data.Maybe import :: Maybe
from Data.Either import :: Either
from Text.GenJSON import generic JSONEncode, generic JSONDecode, :: JSONNode
from System.Time import :: Timespec

from TTY import :: TTYSettings

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
syncSerialChannel :: Timespec TTYSettings (b -> String) (String -> (Either String [a], String)) (Shared ([a],[b],Bool)) -> Task () | iTask a & iTask b
