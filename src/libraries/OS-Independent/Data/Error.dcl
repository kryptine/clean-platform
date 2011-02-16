definition module Error

:: MaybeError a b = Ok a | Error b

:: MaybeErrorString a :== MaybeError a String

/**
 * Return True when the argument is an Ok value and return False otherwise.
 */
isOk			:: !(MaybeError a b) -> Bool
/**
 * Return True when the argument is an Error value and return False otherwise.
 */
isError			:: !(MaybeError a b) -> Bool

/**
 * Return the contents of an Ok value and abort at run-time otherwise.
 */
fromOk			:: !(MaybeError a b) -> a

/**
 * Return the contents of an Error value and abort at run-time otherwise.
 */
fromError		:: !(MaybeError a b) -> b

/**
 * Maps the contents of an Ok value to another type if there is no error.
 * Otherwise give the same error.
 */
mapMaybeError	:: !(a -> c) !(MaybeError a b) -> (MaybeError c b)

/**
 * Lifts a (MaybeError a b) to another MaybeError
 * @precondition: isError x == True
 */
liftError 		:: !(MaybeError a b) -> (MaybeError c b)
