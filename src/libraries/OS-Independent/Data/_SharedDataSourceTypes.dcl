definition module Data._SharedDataSourceTypes

import Data.Maybe, Data.Error, Data.Void, System.Time

:: RWShared r w *env
	= E.b:			BasicSource		!(BasicSource b r w env)
	| E.rx wy:		ComposedRead	!(RWShared rx w env) !(rx -> MaybeErrorString (RWShared r wy env))
	| E.r` w` w``:	ComposedWrite	!(RWShared r w` env) !(w -> MaybeErrorString (RWShared r` w`` env)) !(w r` -> MaybeErrorString [WriteShare env])

:: BasicSource b r w *env =
	{ read			:: !env -> *(!MaybeErrorString (!r,!ChangeNotification env), !env)
	, write			:: !w env -> *(!MaybeErrorString Void, !env)
	, mbId			:: !Maybe BasicShareId
	}
	
:: ChangeNotification env	= OnWrite
							| Predictable	!Timestamp
							| Polling		!Timestamp !(env -> *(!CheckRes,!env))
							
:: CheckRes = Changed | CheckAgain Timestamp
						
:: BasicShareId :== String	
:: WriteShare *env = E.r w: Write !w !(RWShared r w env)
	
