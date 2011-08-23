implementation module SharedDataSource

import SharedDataSourceTypes, StdTuple, FilePath, Void, Maybe, StdBool, StdMisc, StdList, StdFunc, StdString, StdOrdList, Tuple, Func
import qualified Map

createBasicDataSource ::
	!String
	!String
	!(*st -> *(!BasicSourceOps b *st, !*st))
	!(b -> r)
	!(w b -> b)
	->
	ReadWriteShared r w *st
	| TC b
createBasicDataSource type id mkOps get putback = BasicSource
	{ id = type +++ ":" +++ id
	, mkOps = mkOps
	, get = get
	, putback = putback
	}
		
readShared :: !(ReadWriteShared r w *st) !*st ->(!r, !SharedVer, !*st)
readShared shared st
	# (ops, st)		= lock False shared st
	# (r, ver, st)	= readSharedData` ops st
	# st			= close ops st
	= (r, ver, st)
	
writeShared :: !w !(ReadWriteShared r w *st) !*st -> *st
writeShared w shared st
	# (ops, st)		= lock True shared st
	# st			= writeSharedData` w ops st
	# st			= close ops st
	= st

getVersion :: !(ReadWriteShared r w *st) !*st -> (!SharedVer, !*st)
getVersion shared st
	# (ops, st)		= lock False shared st
	# (ver, st)		= getVersion` ops st
	# st			= close ops st
	= (ver, st)

updateShared :: !(r SharedVer -> (UpdRes w a)) !(ReadWriteShared r w *st) !*st -> (!a, !*st)
updateShared f shared st
	# (ops, st)		= lock True shared st
	# (r, ver, st)	= readSharedData` ops st
	# (a, st) = case f r ver of
		NoOp a
			= (a, st)
		Update w a
			# st	= writeSharedData` w ops st
			= (a, st)
		Wait
			= abort "not implemented"
	# st			= close ops st
	= (a, st)

lock :: !Bool !(ReadWriteShared r w *st) !*st -> (SharedOps r w *st, !*st)
lock exclusive shared st
	# (locks, ops, st)	= lock` shared st
	// sort locks to avoid deadlocks
	#locks				= sortBy (\x y -> fst x < fst y) (removeDup` locks)
	# st				= seqSt (\(_,lock) st -> lock st) locks st
	= (ops, st)
where
	lock` :: !(ReadWriteShared r w *st) !*st -> ([(SharedId, *st -> *st)], SharedOps r w *st, !*st)
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
	
readSharedData` :: !(SharedOps r w *st) !*st ->(!r, !SharedVer, !*st)	
readSharedData` (BasicSourceOps get _ {BasicSourceOps|read}) st
	= appFst3 get (read st)
readSharedData` (ComposedSourceOps {opsX, opsY, get}) st
	# (rx, verx, st)	= readSharedData` opsX st
	# (ry, very, st)	= readSharedData` opsY st
	= (get rx ry, verx + very, st)
	
writeSharedData` :: !w !(SharedOps r w *st) !*st -> *st	
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

getVersion` :: !(SharedOps r w *st) !*st ->(!SharedVer, !*st)	
getVersion` (BasicSourceOps get _ {BasicSourceOps|getVersion}) st
	= getVersion st
getVersion` (ComposedSourceOps {opsX, opsY, get}) st
	# (verx, st)	= getVersion` opsX st
	# (very, st)	= getVersion` opsY st
	= (verx + very, st)

close :: !(SharedOps r w *st) !*st -> *st
close (BasicSourceOps _ _ {BasicSourceOps|unlock, close}) st
	= close (unlock st)
close (ComposedSourceOps {opsX, opsY}) st
	# st	= close opsX st
	# st	= close opsY st
	= st


mapSharedRead :: !(r -> r`) !(ReadWriteShared r w *st) -> ReadWriteShared r` w *st
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

mapSharedWrite	:: !(w` r -> w) !(ReadWriteShared r w *st) -> ReadWriteShared r w` *st
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

mapShared :: !(!r -> r`,!w` r -> w) !(ReadWriteShared r w *st) -> ReadWriteShared r` w` *st
mapShared (readMap,writeMap) shared = mapSharedRead readMap (mapSharedWrite writeMap shared)

symmetricLens :: !(a b -> b) !(b a -> a) !(Shared a *st) !(Shared b *st) -> (!Shared a *st, !Shared b *st)
symmetricLens putr putl sharedA sharedB = (newSharedA,newSharedB)
where
	sharedAll = sharedA >+< sharedB 
	newSharedA = mapShared (fst,\a (_,b) -> (a,putr a b)) sharedAll
	newSharedB = mapShared (snd,\b (a,_) -> (putl b a,b)) sharedAll

toReadOnlyShared :: !(ReadWriteShared r w *st) -> ReadWriteShared r Void *st
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

(>+<) infixl 6 :: !(ReadWriteShared rx wx *st) !(ReadWriteShared ry wy *st) -> ReadWriteShared (rx,ry) (wx,wy) *st
(>+<) srcX srcY = ComposedSource {srcX = srcX, srcY = srcY, get = tuple, putback = \w _ _ -> w}

(>+|) infixl 6 :: !(ReadWriteShared rx wx *st) !(ReadWriteShared ry wy *st) -> ReadWriteShared (rx,ry) wx *st
(>+|) srcX srcY = ComposedSource {srcX = srcX, srcY = toReadOnlyShared srcY, get = tuple, putback = \w _ _ -> (w, Void)}

(|+<) infixl 6 :: !(ReadWriteShared rx wx *st) !(ReadWriteShared ry wy *st) -> ReadWriteShared (rx,ry) wy *st
(|+<) srcX srcY = ComposedSource {srcX = toReadOnlyShared srcX, srcY = srcY, get = tuple, putback = \w _ _ -> (Void, w)}

(|+|) infixl 6 :: !(ReadWriteShared rx wx *st) !(ReadWriteShared ry wy *st) -> ReadWriteShared (rx,ry) Void *st
(|+|) srcX srcY = ComposedSource {srcX = toReadOnlyShared srcX, srcY = toReadOnlyShared srcY, get = tuple, putback = \_ _ _ -> (Void, Void)}

// STM
:: *Transaction *st =	{ st	:: !st
						, log	:: !*'Map'.Map SharedId (TLogEntry st)
						}
:: TLogEntry *st = E.b: TLogEntry !b !SharedVer !Bool !(BasicSourceOps b st) & TC b

atomic :: !((*Transaction *st) -> (!TRes a, !*Transaction *st)) !*st -> (!a, !*st)
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

transactionRead :: !(ReadWriteShared r w *st) !(Transaction *st) -> (!r, !(Transaction *st))	
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
	
transactionWrite :: !w !(ReadWriteShared r w *st) !(Transaction *st) -> (Transaction *st)
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