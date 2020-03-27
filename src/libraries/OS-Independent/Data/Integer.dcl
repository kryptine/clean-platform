/// BigInt implementation for Clean
/// Written by John van Groningen
/// Ported from the Haskell frontend by Tim Steenvoorden
definition module Data.Integer

from Data.GenDefault import generic gDefault
from Data.GenLexOrd import generic gLexOrd, :: LexOrd
from Data.GenEq import generic gEq
import StdOverloaded

:: Integer = {integer_s ::!Int, integer_a ::!.{#Int}};

instance +				Integer
instance *  			Integer
instance zero			Integer
instance one			Integer

instance ~				Integer
instance -  			Integer

instance abs			Integer
instance sign			Integer

instance /				Integer
instance rem            Integer

instance ^				Integer

instance ==				Integer
instance <  			Integer

instance isEven Integer

class toInteger a :: !a -> Integer

instance toChar Integer
instance toInt Integer
//instance toReal Integer
instance toString Integer

instance toInteger Char
instance toInteger Int
//instance toInteger Real
instance toInteger String

/*
integer_a_to_double :: !Int !{#Int} -> Real
*/

derive gEq Integer
derive gLexOrd Integer
derive gDefault Integer
