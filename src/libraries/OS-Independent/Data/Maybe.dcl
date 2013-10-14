definition module Data.Maybe

from StdOverloaded import class ==(..)
from Data.Functor import class Functor
from GenEq import generic gEq

/**
 * The Maybe type represents an optional value by providing a constructor 
 * for no value (Nothing) and a constructor for just a value (Just).
 */
:: Maybe a = Nothing | Just a

derive gEq Maybe

/** 
 * Equality on Maybes:
 */
instance == (Maybe x) | == x

instance Functor Maybe

/**
 * Apply a function to the the contents of a Just value and directly return
 * the result, or return a default value if the argument is a Nothing value.
 */
maybe :: .b (.a -> .b) !(Maybe .a) -> .b
/**
 * Directly return a Just value or return a default value if the argument is a Nothing value.
 */
fromMaybe :: .a !(Maybe .a) -> .a

/**
 * Return True when the argument is a Nothing value and return False otherwise.
 */
isNothing :: !(Maybe .a) -> Bool
/**
* Variant of isNothing which returns its input parameter.
*/
isNothingU :: !u:(Maybe .a) -> (!Bool, !u:Maybe .a)

/** 
 * Return True when the argument is a Just value and return False otherwise.
 */
isJust :: !(Maybe .a) -> Bool
/**
* Variant of isJust which returns its input parameter.
*/
isJustU :: !u:(Maybe .a) -> (!Bool, !u:Maybe .a)

/**
 * Return the contents of a Just value and abort at run-time otherwise.
 */
fromJust :: !(Maybe .a) -> .a

/** 
 * Return an empty list for a Nothing value or a singleton list for a Just value.
 */
maybeToList :: !(Maybe .a) -> [.a]

/** 
 * Return a Nothing value for an empty list or a Just value with the head of the list.
 */
listToMaybe :: ![.a] -> Maybe .a

/** 
 * Collect the contents of all the Just values and discard the Nothing values.
 */
catMaybes :: ![Maybe .a] -> .[.a]
