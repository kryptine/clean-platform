definition module SharedDataSource

import FilePath, Void, Maybe, Error

from _SharedDataSourceTypes			import :: RWShared
from _SharedDataSourceOsDependent	import :: OBSERVER
:: Shared a env		:== RWShared a a env
:: ROShared a env	:== RWShared a Void env
:: WOShared a env	:== RWShared Void a env
:: Version			:== Int

createBasicDataSource ::
	!String
	!String
	!(*env -> *(!BasicSourceOps b *env, !*env))
	!(b -> r)
	!(w b -> b)
	->
	RWShared r w *env
	
:: BasicSourceOps b *env =
	{ read			:: 				!env -> *(!MaybeErrorString (!b, !Version), !env)
	, write			:: !b			env -> *(!MaybeErrorString Void, !env)
	, getVersion	:: 				!env -> *(!MaybeErrorString Version, !env)
	, lock			:: 				!env -> env
	, lockExcl		:: 				!env -> env
	, unlock		:: 				!env -> env
	, close			:: 				!env -> env
	, addObserver	:: !OBSERVER	env -> env
	}
	
createProxyDataSource :: !(*env -> *(!RWShared r` w` *env, !*env)) !(r` -> r) !(w r` -> w`) -> RWShared r w *env

read		::		!(RWShared r w *env) !*env -> (!MaybeErrorString (!r, !Version), !*env)
write		:: !w	!(RWShared r w *env) !*env -> (!MaybeErrorString Void, !*env)
getVersion	::		!(RWShared r w *env) !*env -> (!MaybeErrorString Version, !*env)

// atomic update
:: RWRes w a = YieldResult !a | Write !w !a | Redo

readWrite	:: !(r Version -> (RWRes w a))				!(RWShared r w *env) !*env -> (!MaybeErrorString a, !*env)
unsafeRW	:: !(r Version *env -> (RWRes w a, *env))	!(RWShared r w *env) !*env -> (!MaybeErrorString a, !*env)

/**
* Maps the read type, the write type or both of a shared reference to another one using a functional mapping.
* The function for mapping the write type also gets the current read-value as input
* making it possible to change only parts of the datastructure.
*
* @param A functional mapping
* @param A reference to shared data
* @return A reference to shared data of another type
*/
mapRead			:: !(r -> r`)					!(RWShared r w *env) -> RWShared r` w *env
mapWrite		:: !(w` r -> Maybe w)			!(RWShared r w *env) -> RWShared r w` *env
mapReadWrite	:: !(!r -> r`,!w` r -> Maybe w)	!(RWShared r w *env) -> RWShared r` w` *env

// Composition of two shared references.
// The read type is a tuple of both types.
// The write type can either be a tuple of both write types, only one of them or it is written to none of them (result is a read-only shared).
(>+<) infixl 6 :: !(RWShared rx wx *env) !(RWShared ry wy *env) -> RWShared (rx,ry) (wx,wy) *env
(>+|) infixl 6 :: !(RWShared rx wx *env) !(RWShared ry wy *env) -> RWShared (rx,ry) wx *env
(|+<) infixl 6 :: !(RWShared rx wx *env) !(RWShared ry wy *env) -> RWShared (rx,ry) wy *env
(|+|) infixl 6 :: !(RWShared rx wx *env) !(RWShared ry wy *env) -> RWShared (rx,ry) Void *env

toReadOnly :: !(RWShared r w *env) -> ROShared r *env

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
symmetricLens :: !(a b -> b) !(b a -> a) !(Shared a *env) !(Shared b *env) -> (!Shared a *env, !Shared b *env)

// STM
:: *Trans *env

:: TRes a = TYieldResult !a | Retry

atomic :: !((*Trans *env) -> (!TRes a, !*Trans *env)) !*env -> (!MaybeErrorString a, !*env)

transRead	:: 		!(RWShared r w *env) !(Trans *env) -> (!r, !(Trans *env))
transWrite	:: !w	!(RWShared r w *env) !(Trans *env) -> (Trans *env)

// null share
null		:: WOShared a *env
// constant share, value does never change
constShare	:: !a -> ROShared a *env