definition module System._PointerWithFinalizer

/**
 * This module provides pointer to which finalizers are attached,
 * which are called if no reference to the pointer is left.
 */

from System._Pointer import :: Pointer

:: PointerWithFinalizer

/**
 * Creates a pointer with a finalizer which is called if no reference to the pointer is left.
 *
 * @param A pointer
 * @param A pointer to the finalizer C function, which gets the pointer as argument
 * @result The pointer with a finalizer attached
 */
pointerWithFinalizer :: !Pointer !Pointer -> PointerWithFinalizer

/**
 * Perfoms an operation on the pointer.
 * All operations on the pointer have to be evaluated within the function
 * to make sure that no operations are performed on the pointer after the finalizer is called.
 * 
 * @param The operation to perform
 * @param The pointer with finalizer
 * @result The operation's result
 */
withPointer :: !(Pointer -> a) !PointerWithFinalizer -> (!a, !PointerWithFinalizer)
