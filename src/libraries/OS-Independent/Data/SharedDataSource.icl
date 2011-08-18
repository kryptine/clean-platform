implementation module SharedDataSource

import StdTuple, FilePath, Void, Maybe, StdBool, StdMisc, StdList, StdFunc, StdString, StdOrdList, Tuple, Func
import qualified Map

:: SharedDataSource r w *st
	= E.b:				BasicSource !(BasicSource b r w st) & TC b
	| E.rx wx ry wy:	ComposedSource !(ComposedSource r w rx wx ry wy st)

:: BasicSource b r w *st =
	{ id			:: !SharedId
	, mkOps			:: !st -> *(!BasicSourceOps b st, !st)
	, get			:: !b -> r
	, putback		:: !w b -> b
	}
	
:: ComposedSource r w rx wx ry wy *st =
	{ srcX		:: !SharedDataSource rx wx st
	, srcY		:: !SharedDataSource ry wy st
	, get		:: !rx ry -> r
	, putback	:: !w rx ry -> (!wx, !wy)
	}
	
:: SharedId :== String

:: SharedDataSourceOps r w *st
	= E.b:				BasicSourceOps !(b -> r) !(w b -> b) !(BasicSourceOps b st) & TC b
	| E.rx wx ry wy:	ComposedSourceOps !(ComposedSourceOps r w rx wx ry wy st)
	
:: ComposedSourceOps r w rx wx ry wy *st =
	{ opsX		:: !SharedDataSourceOps rx wx st
	, opsY		:: !SharedDataSourceOps ry wy st
	, get		:: !rx ry -> r
	, putback	:: !w rx ry -> (!wx,!wy)
	}
	
createBasicDataSource ::
	!String
	!String
	!(*st -> *(!BasicSourceOps b *st, !*st))
	!(b -> r)
	!(w b -> b)
	->
	SharedDataSource r w *st
	| TC b
createBasicDataSource type id mkOps get putback = BasicSource
	{ id = type +++ ":" +++ id
	, mkOps = mkOps
	, get = get
	, putback = putback
	}
		
readSharedData :: !(SharedDataSource r w *st) !*st ->(!r, !SharedVersion, !*st)
readSharedData shared st
	# (ops, st)		= lock False shared st
	# (r, ver, st)	= readSharedData` ops st
	# st			= close ops st
	= (r, ver, st)
	
writeSharedData :: !w !(SharedDataSource r w *st) !*st -> *st
writeSharedData w shared st
	# (ops, st)		= lock True shared st
	# st			= writeSharedData` w ops st
	# st			= close ops st
	= st

readWriteSharedData :: !(r SharedVersion -> (x,w)) !(SharedDataSource r w *st) !*st -> (!x, !*st)
readWriteSharedData f shared st
	# (ops, st)		= lock True shared st
	# (r, ver, st)	= readSharedData` ops st
	# (x, w)		= f r ver
	# st			= writeSharedData` w ops st
	# st			= close ops st
	= (x, st)

lock :: !Bool !(SharedDataSource r w *st) !*st -> (SharedDataSourceOps r w *st, !*st)
lock exclusive shared st
	# (locks, ops, st)	= lock` shared st
	// sort locks to avoid deadlocks
	#locks				= sortBy (\x y -> fst x < fst y) (removeDup` locks)
	# st				= seqSt (\(_,lock) st -> lock st) locks st
	= (ops, st)
where
	lock` :: !(SharedDataSource r w *st) !*st -> ([(SharedId, *st -> *st)], SharedDataSourceOps r w *st, !*st)
	lock` (BasicSource {BasicSource|id, mkOps, get, putback}) st
		# (ops, st) = mkOps st
		# lockF		= if exclusive ops.lockExcl ops.lock
		= ([(id, lockF)], BasicSourceOps get putback ops, st)
	lock` (ComposedSource {srcX, srcY, get, putback}) st
		# (lockX, opsX, st)	= lock` srcX st
		# (lockY, opsY, st)	= lock` srcY st
		= (lockX ++ lockY, ComposedSourceOps {opsX = opsX, opsY = opsY, get = get, putback = putback}, st)
		
	removeDup` [x:xs] = [x:removeDup` (filter (\y -> fst x <> fst y) xs)]
	removeDup` _      = []
	
readSharedData` :: !(SharedDataSourceOps r w *st) !*st ->(!r, !SharedVersion, !*st)	
readSharedData` (BasicSourceOps get _ {BasicSourceOps|read}) st
	= appFst3 get (read st)
readSharedData` (ComposedSourceOps {opsX, opsY, get}) st
	# (rx, verx, st)	= readSharedData` opsX st
	# (ry, very, st)	= readSharedData` opsY st
	= (get rx ry, verx + very, st)
	
writeSharedData` :: !w !(SharedDataSourceOps r w *st) !*st -> *st	
writeSharedData` w (BasicSourceOps _ putback {BasicSourceOps|read, write}) st
	# (b, _, st)	= read st
	# st			= write (putback w b) st
	= st
writeSharedData` w (ComposedSourceOps {opsX, opsY, get, putback}) st
	# (rx, _, st)		= readSharedData` opsX st
	# (ry, _, st)		= readSharedData` opsY st
	# (wx, wy)			= putback w rx ry
	# st				= writeSharedData` wx opsX st
	# st				= writeSharedData` wy opsY st
	= st

close :: !(SharedDataSourceOps r w *st) !*st -> *st
close (BasicSourceOps _ _ {BasicSourceOps|unlock, close}) st
	= close (unlock st)
close (ComposedSourceOps {opsX, opsY}) st
	# st	= close opsX st
	# st	= close opsY st
	= st


mapSharedRead :: !(r -> r`) !(SharedDataSource r w *st) -> SharedDataSource r` w *st
mapSharedRead get` (BasicSource shared=:{BasicSource|get}) = BasicSource
	{ BasicSource
	| shared
	& get = get` o get
	}
mapSharedRead get` (ComposedSource shared=:{ComposedSource|get}) = ComposedSource
	{ ComposedSource
	| shared
	& get = \rx ry -> get` (get rx ry)
	}

mapSharedWrite	:: !(w` r -> w) !(SharedDataSource r w *st) -> SharedDataSource r w` *st
mapSharedWrite putback` (BasicSource shared=:{BasicSource|get, putback}) = BasicSource
	{ BasicSource
	| shared
	& putback = \w b -> putback (putback` w (get b)) b
	}
mapSharedWrite putback` (ComposedSource shared=:{ComposedSource|get, putback}) = ComposedSource
	{ ComposedSource
	| shared
	& putback = \w rx ry -> putback (putback` w (get rx ry)) rx ry
	}

mapShared :: !(!r -> r`,!w` r -> w) !(SharedDataSource r w *st) -> SharedDataSource r` w` *st
mapShared (readMap,writeMap) shared = mapSharedRead readMap (mapSharedWrite writeMap shared)

toReadOnlyShared :: !(SharedDataSource r w *st) -> SharedDataSource r Void *st
toReadOnlyShared (BasicSource shared=:{BasicSource|get, putback}) = BasicSource
	{ BasicSource
	| shared
	& putback = \_ b -> b
	}
toReadOnlyShared (ComposedSource shared=:{ComposedSource|srcX, srcY}) = ComposedSource
	{ ComposedSource
	| shared
	& srcX = toReadOnlyShared srcX
	, srcY = toReadOnlyShared srcY
	, putback = \_ _ _ -> (Void, Void)
	}

(>+<) infixl 6 :: !(SharedDataSource rx wx *st) !(SharedDataSource ry wy *st) -> SharedDataSource (rx,ry) (wx,wy) *st
(>+<) srcX srcY = ComposedSource {srcX = srcX, srcY = srcY, get = tuple, putback = \w _ _ -> w}

(>+|) infixl 6 :: !(SharedDataSource rx wx *st) !(SharedDataSource ry wy *st) -> SharedDataSource (rx,ry) wx *st
(>+|) srcX srcY = ComposedSource {srcX = srcX, srcY = toReadOnlyShared srcY, get = tuple, putback = \w _ _ -> (w, Void)}

(|+<) infixl 6 :: !(SharedDataSource rx wx *st) !(SharedDataSource ry wy *st) -> SharedDataSource (rx,ry) wy *st
(|+<) srcX srcY = ComposedSource {srcX = toReadOnlyShared srcX, srcY = srcY, get = tuple, putback = \w _ _ -> (Void, w)}

(|+|) infixl 6 :: !(SharedDataSource rx wx *st) !(SharedDataSource ry wy *st) -> SharedDataSource (rx,ry) wy *st
(|+|) srcX srcY = ComposedSource {srcX = toReadOnlyShared srcX, srcY = toReadOnlyShared srcY, get = tuple, putback = \_ _ _ -> (Void, Void)}

// STM
:: *Transaction *st =	{ st	:: !st
						, log	:: !*'Map'.Map SharedId (TLogEntry st)
						}
:: TLogEntry *st = E.b w: TLogEntry !b !SharedVersion !Bool !(BasicSourceOps b st) & TC b

atomic :: !((*Transaction *st) -> (!TransactionResult a, !*Transaction *st)) !*st -> (!a, !*st)
atomic trF st
	# tr					 =	{ st	= st
								, log	= 'Map'.newMap
								}
	# (res, {st, log})		= trF tr
	// list retrieved from log is already sorted
	# entries				= map snd ('Map'.toList log)
	= case res of
		Retry
			# st			= seqSt (\(TLogEntry _ _ _ {close}) st -> close st) entries st
			= atomic trF st
		YieldResult a
			# st			= seqSt (\(TLogEntry _ _ _ {lockExcl}) st -> lockExcl st) entries st
			# (retry, st) 	= seqSt commit entries (False, st)
			| retry			= atomic trF st
			| otherwise		= (a, st)
where
	commit (TLogEntry b lVer doWrite {getVersion, write, unlock, close}) (retry, st)
		# (retry, st)		= if retry (True, st) (commit` st)
		# st				= unlock st
		# st				= close st
		= (retry, st)
	where
		commit` st
			# (cVer, st)	= getVersion st
			# retry			= cVer <> lVer
			# st			= if (not retry && doWrite) (write b st) st
			= (retry, st)

transactionRead :: !(SharedDataSource r w *st) !(Transaction *st) -> (!r, !(Transaction *st))	
transactionRead (BasicSource {BasicSource|id, get, mkOps}) tr=:{log, st}
	# (mbEntry, log)	= 'Map'.getU id log
	= case mbEntry of
		Nothing
			# (ops, st)		= mkOps st
			# st			= ops.lock st
			# (b, ver, st)	= ops.read st
			# st			= ops.unlock st
			# log			= 'Map'.put id (TLogEntry b ver False ops) log
			= (get b, {tr & log = log, st = st})
		Just (TLogEntry b _ _ _)
			= (getFromB b get, {tr & log = log})
where
	getFromB :: !bl !(b -> r) -> r | TC bl & TC b
	getFromB b get = case dynamic b of
		(b :: b^)	= get b
		_ 			= abort "read data in transactions: invalid value in transaction log"
transactionRead (ComposedSource {srcX, srcY, get}) tr
	# (rx, tr)	= transactionRead srcX tr
	# (ry, tr)	= transactionRead srcY tr
	= (get rx ry, tr)
	
transactionWrite :: !w !(SharedDataSource r w *st) !(Transaction *st) -> (Transaction *st)
transactionWrite w (BasicSource {BasicSource|id, putback, mkOps}) tr=:{log, st}
	# (mbEntry, log)	= 'Map'.getU id log
	= case mbEntry of
		Nothing
			# (ops, st)		= mkOps st
			# st			= ops.lock st
			# (b, ver, st)	= ops.read st
			# st			= ops.unlock st
			# b				= putback w b
			# log			= 'Map'.put id (TLogEntry b ver True ops) log
			= {tr & log = log, st = st}
		Just (TLogEntry b ver _ ops)
			# log = 'Map'.put id (TLogEntry (putbackToB w b putback) ver True ops) log
			= {tr & log = log}
where
	putbackToB :: !w !bl !(w b -> b) -> bl | TC bl & TC b
	putbackToB w b putback = case dynamic putback w of
		(putb :: bl^ -> bl^)	= putb b
		_						= abort "write data in transactions: invalid value in transaction log"
transactionWrite w (ComposedSource {srcX, srcY, putback}) tr
	# (rx, tr)	= transactionRead srcX tr
	# (ry, tr)	= transactionRead srcY tr
	# (wx, wy)	= putback w rx ry
	# tr		= transactionWrite wx srcX tr
	# tr		= transactionWrite wy srcY tr
	= tr