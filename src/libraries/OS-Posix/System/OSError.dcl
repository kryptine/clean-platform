definition module System.OSError

import Data.Error
from StdFile import class FileSystem

:: OSErrorCode :== Int
:: OSErrorMessage :== String

:: OSError :== (OSErrorCode, OSErrorMessage)
:: MaybeOSError a :== MaybeError OSError a
:: MaybeOSErrorCode a :== MaybeError OSErrorCode a

getLastOSError :: *w -> (MaybeOSError .a, *w) | FileSystem w

getLastOSErrorCode :: *w -> (MaybeOSErrorCode .a, *w) | FileSystem w
