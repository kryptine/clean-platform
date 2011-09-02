definition module SharedDataSource

import FilePath, Void

from _SharedDataSourceTypes			import :: ReadWriteShared
from _SharedDataSourceOsDependent	import :: WAITER
:: Shared a st :== ReadWriteShared a a st
:: SharedVer :== Int

createBasicDataSource ::
	!String
	!String
	!(*st -> *(!BasicSourceOps b *st, !*st))
	!(b -> r)
	!(w b -> b)
	->
	ReadWriteShared r w *st
	| TC b
	
:: BasicSourceOps b *st =
	{ read			:: !st -> *(!b, !SharedVer, !st)
	, write			:: !b st -> st
	, getVersion	:: !st -> *(!SharedVer, !st)
	, lock			:: !st -> st
	, lockExcl		:: !st -> st
	, unlock		:: !st -> st
	, close			:: !st -> st
	, addWaiter		:: !WAITER st -> st
	}

readShared	::		!(ReadWriteShared r w *st) !*st -> (!r, !SharedVer, !*st)
writeShared	:: !w	!(ReadWriteShared r w *st) !*st -> *st
getVersion	::		!(ReadWriteShared r w *st) !*st -> (!SharedVer, !*st)

// atomic update
:: UpdRes w a = NoOp !a | Update !w !a | Wait

updateShared :: !(r SharedVer -> (UpdRes w a)) !(ReadWriteShared r w *st) !*st -> (!a, !*st)

/**
* Maps the read type, the write type or both of a shared reference to another one using a functional mapping.
* The function for mapping the write type also gets the current read-value as input
* making it possible to change only parts of the datastructure.
*
* @param A functional mapping
* @param A reference to shared data
* @return A reference to shared data of another type
*/
mapSharedRead	:: !(r -> r`)				!(ReadWriteShared r w *st) -> ReadWriteShared r` w *st
mapSharedWrite	:: !(w` r -> w)				!(ReadWriteShared r w *st) -> ReadWriteShared r w` *st
mapShared		:: !(!r -> r`,!w` r -> w)	!(ReadWriteShared r w *st) -> ReadWriteShared r` w` *st

// Composition of two shared references.
// The read type is a tuple of both types.
// The write type can either be a tuple of both write types, only one of them or it is written to none of them (result is a read-only shared).
(>+<) infixl 6 :: !(ReadWriteShared rx wx *st) !(ReadWriteShared ry wy *st) -> ReadWriteShared (rx,ry) (wx,wy) *st
(>+|) infixl 6 :: !(ReadWriteShared rx wx *st) !(ReadWriteShared ry wy *st) -> ReadWriteShared (rx,ry) wx *st
(|+<) infixl 6 :: !(ReadWriteShared rx wx *st) !(ReadWriteShared ry wy *st) -> ReadWriteShared (rx,ry) wy *st
(|+|) infixl 6 :: !(ReadWriteShared rx wx *st) !(ReadWriteShared ry wy *st) -> ReadWriteShared (rx,ry) Void *st

toReadOnlyShared :: !(ReadWriteShared r w *st) -> ReadWriteShared r Void *st

/**
* Puts a symmetric lens between two symmetric shared data sources.
* Changes of one also affects the other one.
*
* @param putr: used to map changes of shared a to shared b
* @param putl: used to map changes of shared b to shared a
* @param SymmetricShared a
* @param SymmetricShared b
* @param ReadWriteShared references of the same type with symmetric lens between them
*/
symmetricLens :: !(a b -> b) !(b a -> a) !(Shared a *st) !(Shared b *st) -> (!Shared a *st, !Shared b *st)

// STM
:: *Transaction *st

:: TRes a = YieldResult !a | Retry

atomic :: !((*Transaction *st) -> (!TRes a, !*Transaction *st)) !*st -> (!a, !*st)

transactionRead		:: 		!(ReadWriteShared r w *st) !(Transaction *st) -> (!r, !(Transaction *st))
transactionWrite	:: !w	!(ReadWriteShared r w *st) !(Transaction *st) -> (Transaction *st)
