definition module Error

import Functor

:: MaybeError a b = Error a | Ok b 

:: MaybeErrorString a :== MaybeError String a

instance Functor (MaybeError a)

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
fromOk			:: !(MaybeError .a .b) -> .b

/**
 * Return the contents of an Error value and abort at run-time otherwise.
 */
fromError		:: !(MaybeError .a .b) -> .a

/**
 * Lifts a (MaybeError a b) to another MaybeError
 * @precondition: isError x == True
 */
liftError 		:: !(MaybeError .a .b) -> (MaybeError .a .c)