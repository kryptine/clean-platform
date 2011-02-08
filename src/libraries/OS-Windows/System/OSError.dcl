definition module OSError

import Error

:: OSErrorCode :== Int
:: OSErrorMessage :== String

:: OSError :== (OSErrorCode, OSErrorMessage)
:: MaybeOSError a :== MaybeError a OSError

getLastOSError :: *World -> (MaybeOSError a, *World)
