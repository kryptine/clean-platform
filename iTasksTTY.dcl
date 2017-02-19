definition module iTasksTTY

from TTY import :: TTYSettings
import iTasks

derive class iTask TTYSettings

getTTYDevices :: !*env -> *(![String], !*env)

syncSerialChannel :: String TTYSettings (b -> String) (String -> a) (Shared ([a],[b],Bool)) -> Task () | iTask a & iTask b
