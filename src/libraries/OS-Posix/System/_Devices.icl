implementation module System._Devices

import Data.Error
import System.OSError
import System.Directory

getDevices :: !*World -> *(MaybeOSError [String], !*World)
getDevices w = readDirectory "/dev" w
