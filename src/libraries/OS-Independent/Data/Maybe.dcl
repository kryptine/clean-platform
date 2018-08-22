definition module Data.Maybe
/**
* This module extends the basic functions on Maybe values from StdMaybe 
*/

import StdMaybe 

from StdOverloaded import class ==(..)
from Data.Functor import class Functor
from Control.Applicative import class Applicative, class *>, class <*, class Alternative
from Control.Monad import class Monad, class MonadPlus
from Data.Monoid import class Semigroup, class Monoid
from Data.Foldable import class Foldable
from Data.Traversable import class Traversable
from Data.GenEq import generic gEq

instance Functor Maybe
instance Applicative Maybe
instance *> Maybe
instance <* Maybe
instance Alternative Maybe
instance Monad Maybe
instance MonadPlus Maybe

instance Semigroup (Maybe a) | Semigroup a
instance Monoid (Maybe a)
instance Foldable Maybe
instance Traversable Maybe

derive gEq Maybe

/**
 * Apply a function to the the contents of a Just value and directly return
 * the result, or return a default value if the argument is a Nothing value.
 */
maybe :: w:b v:(.a -> w:b) !.(Maybe .a) -> w:b

/**
 * Apply a function to the the contents of a Just value and the state, and
 * directly return the result and a new state. Return the state immediately
 * if the argument is a Nothing value.
 */
maybeSt :: *st (.a *st -> *st) !(Maybe .a) -> *st

/**
 * Directly return a Just value or return a default value if the argument is a Nothing value.
 */
fromMaybe :: .a !(Maybe .a) -> .a
