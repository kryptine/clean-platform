definition module iTasksTTY

from TTY import :: TTYSettings
import iTasks

derive class iTask TTYSettings

enterTTYSettings :: Task TTYSettings

syncSerialChannel :: TTYSettings (b -> String) (String -> (Either String [a], String)) (Shared ([a],[b],Bool)) -> Task () | iTask a & iTask b
