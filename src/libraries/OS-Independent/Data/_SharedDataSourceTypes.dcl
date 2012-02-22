definition module _SharedDataSourceTypes

import Maybe, Error
from SharedDataSource				import :: BasicSourceOps
from _SharedDataSourceOsDependent	import :: OBSERVER

:: RWShared r w *env
	= E.b:				BasicSource		!(BasicSource b r w env)
	| E.rx wx ry wy:	ComposedSource	!(ComposedSource r w rx wx ry wy env)
	| E.r` w`:			ProxySource		!(ProxySource r` w` r w env)
	| E.b:				KeyValueSource	!(KeyValueSource b r w env)

:: BasicSource b r w *env =
	{ id		:: !ShareId
	, mkOps		:: !env -> *(!BasicSourceOps b env, !env)
	, get		:: !b -> MaybeErrorString r
	, putback	:: !w b -> MaybeErrorString (Maybe b)
	}
	
:: ComposedSource r w rx wx ry wy *env =
	{ srcX		:: !RWShared rx wx env
	, srcY		:: !RWShared ry wy env
	, get		:: !rx ry -> MaybeErrorString r
	, putback	:: !w rx ry -> MaybeErrorString (Maybe (!wx, !wy))
	}
	
:: ProxySource r` w` r w *env =
	{ getSource	:: !env -> *(!RWShared r` w` env, !env)
	, get		:: !r` -> MaybeErrorString r
	, put		:: !w r` -> MaybeErrorString (Maybe w`)
	}
	
:: KeyValueSource b r w *env =
	{ id			:: !ShareId
	, mkOps			:: !env -> *(!BasicSourceOps b env, !env)
	, get			:: !b -> MaybeErrorString r
	, putback		:: !w b -> MaybeErrorString (Maybe b)
	, keyProjection	:: !Dynamic
	}
	
:: ShareId :== String

:: SharedOps r w *env
	= E.b:				BasicSourceOps !(b -> MaybeErrorString r) !(w b -> MaybeErrorString (Maybe b)) !(BasicSourceOps b env)
	| E.rx wx ry wy:	ComposedSourceOps !(ComposedSourceOps r w rx wx ry wy env)
	
:: ComposedSourceOps r w rx wx ry wy *env =
	{ opsX			:: !SharedOps rx wx env
	, opsY			:: !SharedOps ry wy env
	, get			:: !rx ry -> MaybeErrorString r
	, putback		:: !w rx ry -> MaybeErrorString (Maybe (!wx,!wy))
	, addObserver	:: !OBSERVER env -> env
	}
	
close :: !(SharedOps r w *env) !*env -> *env