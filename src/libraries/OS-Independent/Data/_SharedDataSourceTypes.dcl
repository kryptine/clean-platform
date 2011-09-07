definition module _SharedDataSourceTypes

import Maybe
from SharedDataSource				import :: BasicSourceOps
from _SharedDataSourceOsDependent	import :: OBSERVER

:: RWShared r w *st
	= E.b:				BasicSource !(BasicSource b r w st)
	| E.rx wx ry wy:	ComposedSource !(ComposedSource r w rx wx ry wy st)

:: BasicSource b r w *st =
	{ id			:: !SharedId
	, mkOps			:: !st -> *(!BasicSourceOps b st, !st)
	, get			:: !b -> r
	, putback		:: !w b -> Maybe b
	}
	
:: ComposedSource r w rx wx ry wy *st =
	{ srcX		:: !RWShared rx wx st
	, srcY		:: !RWShared ry wy st
	, get		:: !rx ry -> r
	, putback	:: !w rx ry -> Maybe (!wx, !wy)
	}
	
:: SharedId :== String

:: SharedOps r w *st
	= E.b:				BasicSourceOps !(b -> r) !(w b -> Maybe b) !(BasicSourceOps b st)
	| E.rx wx ry wy:	ComposedSourceOps !(ComposedSourceOps r w rx wx ry wy st)
	
:: ComposedSourceOps r w rx wx ry wy *st =
	{ opsX			:: !SharedOps rx wx st
	, opsY			:: !SharedOps ry wy st
	, get			:: !rx ry -> r
	, putback		:: !w rx ry -> Maybe (!wx,!wy)
	, addObserver	:: !OBSERVER st -> st
	}
	
close :: !(SharedOps r w *st) !*st -> *st