definition module Maybe 

import StdBool
import StdFunc
import StdMisc


/**
 * The Maybe type represents an optional value by providing a constructor 
 * for no value (Nothing) and a constructor for just a value (Just).
 */
:: Maybe a = Nothing | Just a


/** 
 * Apply a function to the contents of a Just value, if such a value is present.
 */
fmap :: (.a -> .b) (Maybe .a) -> Maybe .b

/**
 * Apply a function to the the contents of a Just value and directly return
 * the result, or return a default value if the argument is a Nothing value.
 */
maybe :: .a (.a -> .a) !(Maybe .a) -> .a

/**
 * Return True when the argument is a Nothing value and return False otherwise.
 */
isNothing :: !(Maybe .a) -> Bool

/** 
 * Return True when the argument is a Just value and return False otherwise.
 */
isJust :: ((Maybe .a) -> Bool)

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
listToMaybe :: [.a] -> Maybe .a

/** 
 * Collect the contents of all the Just values and discard the Nothing values.
 */
catMaybes :: ![Maybe .a] -> .[.a]