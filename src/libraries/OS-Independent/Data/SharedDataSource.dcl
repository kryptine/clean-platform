definition module SharedDataSource

import FilePath

:: SharedDataSource r w *st
:: SymmetricSharedDataSource a st :== SharedDataSource a a st
:: SharedVersion :== Int

createBasicDataSource ::
	!String
	!String
	!(*st -> *(!BasicSourceOps b *st, !*st))
	!(b -> r)
	!(w b -> b)
	->
	SharedDataSource r w *st
	| TC b
	
:: BasicSourceOps b *st =
	{ read			:: !st -> *(!b, !SharedVersion, !st)
	, write			:: !b st -> st
	, getVersion	:: !st -> *(!SharedVersion, !st)
	, lock			:: !st -> st
	, lockExcl		:: !st -> st
	, unlock		:: !st -> st
	, close			:: !st -> st
	}

readSharedData		::								!(SharedDataSource r w *st) !*st -> (!r, !SharedVersion, !*st)
writeSharedData		:: !w							!(SharedDataSource r w *st) !*st -> *st
readWriteSharedData	:: !(r SharedVersion -> (x,w))	!(SharedDataSource r w *st) !*st -> (!x, !*st)

/**
* Maps the read type, the write type or both of a shared reference to another one using a functional mapping.
* The function for mapping the write type also gets the current read-value as input
* making it possible to change only parts of the datastructure.
*
* @param A functional mapping
* @param A reference to shared data
* @return A reference to shared data of another type
*/
mapSharedRead	:: !(r -> r`)				!(SharedDataSource r w *st) -> SharedDataSource r` w *st
mapSharedWrite	:: !(w` r -> w)				!(SharedDataSource r w *st) -> SharedDataSource r w` *st
mapShared		:: !(!r -> r`,!w` r -> w)	!(SharedDataSource r w *st) -> SharedDataSource r` w` *st

// Composition of two shared references.
// The read type is a tuple of both types.
// The write type can either be a tuple of both write types, only one of them or it is written to none of them (result is a read-only shared).
(>+<) infixl 6 :: !(SharedDataSource rx wx *st) !(SharedDataSource ry wy *st) -> SharedDataSource (rx,ry) (wx,wy) *st
(>+|) infixl 6 :: !(SharedDataSource rx wx *st) !(SharedDataSource ry wy *st) -> SharedDataSource (rx,ry) wx *st
(|+<) infixl 6 :: !(SharedDataSource rx wx *st) !(SharedDataSource ry wy *st) -> SharedDataSource (rx,ry) wy *st
(|+|) infixl 6 :: !(SharedDataSource rx wx *st) !(SharedDataSource ry wy *st) -> SharedDataSource (rx,ry) wy *st

// STM
:: *Transaction *st

:: TransactionResult a = YieldResult !a | Retry

atomic :: !((*Transaction *st) -> (!TransactionResult a, !*Transaction *st)) !*st -> (!a, !*st)

transactionRead		:: 		!(SharedDataSource r w *st) !(Transaction *st) -> (!r, !(Transaction *st))
transactionWrite	:: !w	!(SharedDataSource r w *st) !(Transaction *st) -> (Transaction *st)
