definition module iTasksTTY

from TTY import :: TTYSettings
import iTasks

derive class iTask TTYSettings

getTTYDevices :: !*env -> *(![String], !*env)

enterTTYSettings :: Task TTYSettings

syncSerialChannel :: TTYSettings (b -> String) (String -> (Either String [a], String)) (Shared ([a],[b],Bool)) -> Task () | iTask a & iTask b
