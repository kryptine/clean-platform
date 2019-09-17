definition module Data.Maybe.GenPrint

from Data.Maybe    import :: Maybe
from Text.GenPrint import generic gPrint, class PrintOutput, :: PrintState

derive gPrint Maybe
