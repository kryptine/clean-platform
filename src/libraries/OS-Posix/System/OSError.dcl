definition module System.OSError

import Data.Error

:: OSErrorCode :== Int
:: OSErrorMessage :== String

:: OSError :== (OSErrorCode, OSErrorMessage)
:: MaybeOSError a :== MaybeError OSError a
:: MaybeOSErrorCode a :== MaybeError OSErrorCode a

getLastOSError :: *World -> (MaybeOSError .a, *World)

getLastOSErrorCode :: *World -> (MaybeOSErrorCode .a, *World)
