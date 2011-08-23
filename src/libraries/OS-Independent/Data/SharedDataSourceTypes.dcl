definition module SharedDataSourceTypes

from SharedDataSource import :: BasicSourceOps

:: ReadWriteShared r w *st
	= E.b:				BasicSource !(BasicSource b r w st) & TC b
	| E.rx wx ry wy:	ComposedSource !(ComposedSource r w rx wx ry wy st)

:: BasicSource b r w *st =
	{ id			:: !SharedId
	, mkOps			:: !st -> *(!BasicSourceOps b st, !st)
	, get			:: !b -> r
	, putback		:: !w b -> b
	}
	
:: ComposedSource r w rx wx ry wy *st =
	{ srcX		:: !ReadWriteShared rx wx st
	, srcY		:: !ReadWriteShared ry wy st
	, get		:: !rx ry -> r
	, putback	:: !w rx ry -> (!wx, !wy)
	}
	
:: SharedId :== String

:: SharedOps r w *st
	= E.b:				BasicSourceOps !(b -> r) !(w b -> b) !(BasicSourceOps b st) & TC b
	| E.rx wx ry wy:	ComposedSourceOps !(ComposedSourceOps r w rx wx ry wy st)
	
:: ComposedSourceOps r w rx wx ry wy *st =
	{ opsX		:: !SharedOps rx wx st
	, opsY		:: !SharedOps ry wy st
	, get		:: !rx ry -> r
	, putback	:: !w rx ry -> (!wx,!wy)
	}