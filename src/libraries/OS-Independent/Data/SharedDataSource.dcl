definition module SharedDataSource

import FilePath, Void, Maybe

from _SharedDataSourceTypes			import :: RWShared
from _SharedDataSourceOsDependent	import :: OBSERVER
:: Shared a st		:== RWShared a a st
:: ROShared a st	:== RWShared a Void st
:: WOShared a st	:== RWShared Void a st
:: Version			:== Int

createBasicDataSource ::
	!String
	!String
	!(*st -> *(!BasicSourceOps b *st, !*st))
	!(b -> r)
	!(w b -> b)
	->
	RWShared r w *st
	
:: BasicSourceOps b *st =
	{ read			:: !st -> *(!b, !Version, !st)
	, write			:: !b st -> st
	, getVersion	:: !st -> *(!Version, !st)
	, lock			:: !st -> st
	, lockExcl		:: !st -> st
	, unlock		:: !st -> st
	, close			:: !st -> st
	, addObserver	:: !OBSERVER st -> st
	}

read		::		!(RWShared r w *st) !*st -> (!r, !Version, !*st)
write		:: !w	!(RWShared r w *st) !*st -> *st
getVersion	::		!(RWShared r w *st) !*st -> (!Version, !*st)

// atomic update
:: RWRes w a = YieldResult !a | Write !w !a | Redo

readWrite	:: !(r Version -> (RWRes w a))			!(RWShared r w *st) !*st -> (!a, !*st)
unsafeRW	:: !(r Version *st -> (RWRes w a, *st))	!(RWShared r w *st)	!*st -> (!a, !*st)

/**
* Maps the read type, the write type or both of a shared reference to another one using a functional mapping.
* The function for mapping the write type also gets the current read-value as input
* making it possible to change only parts of the datastructure.
*
* @param A functional mapping
* @param A reference to shared data
* @return A reference to shared data of another type
*/
mapRead			:: !(r -> r`)					!(RWShared r w *st) -> RWShared r` w *st
mapWrite		:: !(w` r -> Maybe w)			!(RWShared r w *st) -> RWShared r w` *st
mapReadWrite	:: !(!r -> r`,!w` r -> Maybe w)	!(RWShared r w *st) -> RWShared r` w` *st

// Composition of two shared references.
// The read type is a tuple of both types.
// The write type can either be a tuple of both write types, only one of them or it is written to none of them (result is a read-only shared).
(>+<) infixl 6 :: !(RWShared rx wx *st) !(RWShared ry wy *st) -> RWShared (rx,ry) (wx,wy) *st
(>+|) infixl 6 :: !(RWShared rx wx *st) !(RWShared ry wy *st) -> RWShared (rx,ry) wx *st
(|+<) infixl 6 :: !(RWShared rx wx *st) !(RWShared ry wy *st) -> RWShared (rx,ry) wy *st
(|+|) infixl 6 :: !(RWShared rx wx *st) !(RWShared ry wy *st) -> RWShared (rx,ry) Void *st

toReadOnly :: !(RWShared r w *st) -> ROShared r *st

/**
* Puts a symmetric lens between two symmetric shared data sources.
* Changes of one also affects the other one.
*
* @param putr: used to map changes of shared a to shared b
* @param putl: used to map changes of shared b to shared a
* @param SymmetricShared a
* @param SymmetricShared b
* @param RWShared references of the same type with symmetric lens between them
*/
symmetricLens :: !(a b -> b) !(b a -> a) !(Shared a *st) !(Shared b *st) -> (!Shared a *st, !Shared b *st)

// STM
:: *Trans *st

:: TRes a = TYieldResult !a | Retry

atomic :: !((*Trans *st) -> (!TRes a, !*Trans *st)) !*st -> (!a, !*st)

transRead	:: 		!(RWShared r w *st) !(Trans *st) -> (!r, !(Trans *st))
transWrite	:: !w	!(RWShared r w *st) !(Trans *st) -> (Trans *st)

// null share
null :: WOShared a *env
