definition module System.Select

from Data.Error import :: MaybeError
from System.OSError import :: MaybeOSError, :: OSError, :: OSErrorMessage, :: OSErrorCode
from System._Select import :: SelectSet

class Selectable s where
	toSelectSet :: !*s !*World -> (!*MaybeOSError *(SelectSet *s), !*World)

:: SelectPair a b = SelectPair a b
instance Selectable (SelectPair a b) | Selectable a & Selectable b

waitForSelectSet :: !*(SelectSet s) -> !*(SelectSet
