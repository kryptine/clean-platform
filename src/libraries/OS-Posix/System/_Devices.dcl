definition module System._Devices

from Data.Error import :: MaybeError
from System.OSError import :: MaybeOSError, :: OSError, :: OSErrorCode, :: OSErrorMessage

getDevices :: !*World -> *(MaybeOSError [String], !*World)
