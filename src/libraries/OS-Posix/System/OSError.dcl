definition module OSError

import Error

:: OSErrorCode :== Int
:: OSErrorMessage :== String

:: OSError :== (OSErrorCode, OSErrorMessage)
:: MaybeOSError a :== MaybeError a OSError
:: MaybeOSErrorCode a :== MaybeError a OSErrorCode

getLastOSError :: *World -> (MaybeOSError a, *World)

getLastOSErrorCode :: *World -> (MaybeOSErrorCode a, *World)
