implementation module SharedDataSource

import _SharedDataSourceTypes, _SharedDataSourceOsDependent, StdTuple, FilePath, Void, Maybe, StdBool, StdMisc, StdList, StdFunc, StdString, StdOrdList, Tuple, Func
from Map import qualified :: Map, newMap, toList, getU, put

createBasicDataSource ::
	!String
	!String
	!(*st -> *(!BasicSourceOps b *st, !*st))
	!(b -> r)
	!(w b -> b)
	->
	RWShared r w *st
createBasicDataSource type id mkOps get putback = BasicSource
	{ id = type +++ ":" +++ id
	, mkOps = mkOps
	, get = get
	, putback = \w b -> Just (putback w b)
	}
		
read :: !(RWShared r w *st) !*st ->(!r, !Version, !*st)
read shared st
	# (ops, st)		= lock False shared st
	# (r, ver, st)	= readSharedData` ops st
	# st			= close ops st
	= (r, ver, st)
	
write :: !w !(RWShared r w *st) !*st -> *st
write w shared st
	# (ops, st)		= lock True shared st
	# st			= writeSharedData` w ops st
	# st			= close ops st
	= st

getVersion :: !(RWShared r w *st) !*st -> (!Version, !*st)
getVersion shared st
	# (ops, st)		= lock False shared st
	# (ver, st)		= getVersion` ops st
	# st			= close ops st
	= (ver, st)

readWrite :: !(r Version -> (RWRes w a)) !(RWShared r w *st) !*st -> (!a, !*st)
readWrite f shared st = unsafeRW (\r ver st -> (f r ver, st)) shared st
			
unsafeRW :: !(r Version *st -> (RWRes w a, *st))	!(RWShared r w *st)	!*st -> (!a, !*st)
unsafeRW f shared st
	# (ops, st)			= lock True shared st
	# (r, ver, st)		= readSharedData` ops st
	= case f r ver st of
		(YieldResult a, st)
			# st		= close ops st
			= (a, st)
		(Write w a, st)
			# st		= writeSharedData` w ops st
			# st		= close ops st
			= (a, st)
		(Redo, st)
			# st		= wait ops st
			= unsafeRW f shared st
			
lock :: !Bool !(RWShared r w *st) !*st -> (SharedOps r w *st, !*st)
lock exclusive shared st
	# (locks, ops, st)	= lock` shared st
	// sort locks to avoid deadlocks
	#locks				= sortBy (\x y -> fst x < fst y) (removeDup` locks)
	# st				= seqSt (\(_,lock) st -> lock st) locks st
	= (ops, st)
where
	lock` :: !(RWShared r w *st) !*st -> ([(SharedId, *st -> *st)], SharedOps r w *st, !*st)
	lock` (BasicSource {BasicSource|id, mkOps, get, putback}) st
		# (ops, st) = mkOps st
		# lockF		= if exclusive ops.lockExcl ops.lock
		= ([(id, lockF)], BasicSourceOps get putback ops, st)
	lock` (ComposedSource {srcX, srcY, get, putback}) st
		# (lockX, opsX, st)	= lock` srcX st
		# (lockY, opsY, st)	= lock` srcY st
		# cops =	{ opsX = opsX
					, opsY = opsY
					, get = get
					, putback = putback
					, addObserver = \obs st -> addObserver opsY obs (addObserver opsX obs st)
					}
		= (lockX ++ lockY, ComposedSourceOps cops, st)
		
	removeDup` [x:xs] = [x:removeDup` (filter (\y -> fst x <> fst y) xs)]
	removeDup` _      = []
	
	addObserver :: !(SharedOps r w *st) !OBSERVER !*st -> *st
	addObserver (BasicSourceOps _ putback {BasicSourceOps|addObserver}) waiter st
		= addObserver waiter st
	addObserver (ComposedSourceOps {opsX, opsY}) waiter st
		= addObserver opsY waiter (addObserver opsX waiter st)
	
readSharedData` :: !(SharedOps r w *st) !*st ->(!r, !Version, !*st)	
readSharedData` (BasicSourceOps get _ {BasicSourceOps|read}) st
	= appFst3 get (read st)
readSharedData` (ComposedSourceOps {opsX, opsY, get}) st
	# (rx, verx, st)	= readSharedData` opsX st
	# (ry, very, st)	= readSharedData` opsY st
	= (get rx ry, verx + very, st)
	
writeSharedData` :: !w !(SharedOps r w *st) !*st -> *st	
writeSharedData` w (BasicSourceOps _ putback {BasicSourceOps|read, write}) st
	# (b, _, st) = read st
	= case putback w b of
		Nothing	= st
		Just b	= write b st
writeSharedData` w (ComposedSourceOps {opsX, opsY, get, putback}) st
	# (rx, _, st)		= readSharedData` opsX st
	# (ry, _, st)		= readSharedData` opsY st
	= case putback w rx ry of
		Nothing
			= st
		Just (wx, wy)
			# st		= writeSharedData` wx opsX st
			# st		= writeSharedData` wy opsY st
			= st

getVersion` :: !(SharedOps r w *st) !*st ->(!Version, !*st)	
getVersion` (BasicSourceOps get _ {BasicSourceOps|getVersion}) st
	= getVersion st
getVersion` (ComposedSourceOps {opsX, opsY, get}) st
	# (verx, st)	= getVersion` opsX st
	# (very, st)	= getVersion` opsY st
	= (verx + very, st)

wait :: !(SharedOps r w *st) !*st -> *st
wait ops st = waitOsDependent [addObs] (close ops) st
where
	addObs = case ops of
		BasicSourceOps _ _ {BasicSourceOps|addObserver}		= addObserver
		ComposedSourceOps {ComposedSourceOps|addObserver}	= addObserver
		
mapRead :: !(r -> r`) !(RWShared r w *st) -> RWShared r` w *st
mapRead get` (BasicSource shared=:{BasicSource|get}) = BasicSource
	{ BasicSource
	| shared
	& get = get` o get
	}
mapRead get` (ComposedSource shared=:{ComposedSource|get}) = ComposedSource
	{ ComposedSource
	| shared
	& get = \rx ry -> get` (get rx ry)
	}

mapWrite :: !(w` r -> Maybe w) !(RWShared r w *st) -> RWShared r w` *st
mapWrite putback` (BasicSource shared=:{BasicSource|get, putback}) = BasicSource
	{ BasicSource
	| shared
	& putback = \w` b -> maybe Nothing (\w -> putback w b) (putback` w` (get b))
	}
mapWrite putback` (ComposedSource shared=:{ComposedSource|get, putback}) = ComposedSource
	{ ComposedSource
	| shared
	& putback = \w` rx ry -> maybe Nothing (\w -> putback w rx ry) (putback` w` (get rx ry))
	}

mapReadWrite :: !(!r -> r`,!w` r -> Maybe w) !(RWShared r w *st) -> RWShared r` w` *st
mapReadWrite (readMap,writeMap) shared = mapRead readMap (mapWrite writeMap shared)

symmetricLens :: !(a b -> b) !(b a -> a) !(Shared a *st) !(Shared b *st) -> (!Shared a *st, !Shared b *st)
symmetricLens putr putl sharedA sharedB = (newSharedA,newSharedB)
where
	sharedAll = sharedA >+< sharedB 
	newSharedA = mapReadWrite (fst,\a (_,b) -> Just (a,putr a b)) sharedAll
	newSharedB = mapReadWrite (snd,\b (a,_) -> Just (putl b a,b)) sharedAll

toReadOnly :: !(RWShared r w *st) -> ROShared r *st
toReadOnly share = mapWrite (\_ _ -> Nothing) share

(>+<) infixl 6 :: !(RWShared rx wx *st) !(RWShared ry wy *st) -> RWShared (rx,ry) (wx,wy) *st
(>+<) srcX srcY = ComposedSource {srcX = srcX, srcY = srcY, get = tuple, putback = \w _ _ -> Just w}

(>+|) infixl 6 :: !(RWShared rx wx *st) !(RWShared ry wy *st) -> RWShared (rx,ry) wx *st
(>+|) srcX srcY = mapWrite (\wx _ -> Just (wx, Void)) (srcX >+< toReadOnly srcY)

(|+<) infixl 6 :: !(RWShared rx wx *st) !(RWShared ry wy *st) -> RWShared (rx,ry) wy *st
(|+<) srcX srcY = mapWrite (\wy _ -> Just (Void, wy)) (toReadOnly srcX >+< srcY)

(|+|) infixl 6 :: !(RWShared rx wx *st) !(RWShared ry wy *st) -> RWShared (rx,ry) Void *st
(|+|) srcX srcY = toReadOnly (srcX >+< srcY)

// STM
:: *Trans *st =	{ st	:: !st
				, log	:: !*'Map'.Map SharedId (TLogEntry st)
				}
:: TLogEntry *st = E.b: TLogEntry !b !Version !Bool !(BasicSourceOps b st)

atomic :: !((*Trans *st) -> (!TRes a, !*Trans *st)) !*st -> (!a, !*st)
atomic trF st
	# tr					 =	{ st	= st
								, log	= 'Map'.newMap
								}
	# (res, {st, log})		= trF tr
	// list retrieved from log is already sorted
	# entries				= map snd ('Map'.toList log)
	= case res of
		Retry
			# st			= waitOsDependent [addObserver \\ (TLogEntry _ _ _ {BasicSourceOps|addObserver}) <- entries] (seqSt (\(TLogEntry _ _ _ {close}) st -> close st) entries) st
			= atomic trF st
		TYieldResult a
			# st			= seqSt (\(TLogEntry _ _ _ {lockExcl}) st -> lockExcl st) entries st
			# (retry, st)	= checkConsistency entries st
			| retry
				# st		= seqSt (\(TLogEntry _ _ _ {unlock, close}) st -> close (unlock st)) entries st
				= atomic trF st
			| otherwise
				# st		= seqSt commit entries st
				# st		= seqSt (\(TLogEntry _ _ _ {unlock, close}) st -> close (unlock st)) entries st
				= (a, st)
where
	checkConsistency :: ![TLogEntry *env] !*env -> *(!Bool,!*env)
	checkConsistency [] st = (False, st)
	checkConsistency [TLogEntry _ lVer _ {getVersion}:entries] st
		# (cVer, st)		= getVersion st
		| cVer == lVer		= checkConsistency entries st
		| otherwise			= (True, st)
		
	commit :: !(TLogEntry *env) !*env -> *env
	commit (TLogEntry b _ doWrite {write,unlock}) st
		| doWrite			= write b st
		| otherwise			= st

transRead :: !(RWShared r w *st) !(Trans *st) -> (!r, !(Trans *st))	
transRead (BasicSource {BasicSource|id, get, mkOps}) tr=:{log, st}
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
			= (get (forceType b), {tr & log = log})

transRead (ComposedSource {srcX, srcY, get}) tr
	# (rx, tr)	= transRead srcX tr
	# (ry, tr)	= transRead srcY tr
	= (get rx ry, tr)
	
transWrite :: !w !(RWShared r w *st) !(Trans *st) -> (Trans *st)
transWrite w (BasicSource {BasicSource|id, putback, mkOps}) tr=:{log, st}
	# (mbEntry, log)	= 'Map'.getU id log
	= case mbEntry of
		Nothing
			# (ops, st)		= mkOps st
			# st			= ops.lock st
			# (b, ver, st)	= ops.read st
			# st			= ops.unlock st
			# b = case putback w b of
				Nothing	= b
				Just b	= b
			# log			= 'Map'.put id (TLogEntry b ver True ops) log
			= {tr & log = log, st = st}
		Just (TLogEntry b ver _ ops) = case putback w (forceType b) of
			Nothing
				= tr
			Just b
				# log = 'Map'.put id (TLogEntry (forceType b) ver True ops) log
				= {tr & log = log}

transWrite w (ComposedSource {srcX, srcY, putback}) tr
	# (rx, tr)	= transRead srcX tr
	# (ry, tr)	= transRead srcY tr
	= case putback w rx ry of
		Nothing
			= tr
		Just (wx, wy)
			# tr		= transWrite wx srcX tr
			# tr		= transWrite wy srcY tr
			= tr
			
null :: WOShared a *env
null = createBasicDataSource "Null" "" mkOps (const Void) (\_ b -> b)
where
	mkOps env = (ops, env)
	ops =	{ read			= \env -> (Void, 0, env)
			, write			= \_ env -> env
			, getVersion	= \env -> (0, env)
			, lock			= id
			, unlock		= id
			, lockExcl		= id
			, close			= id
			, addObserver	= \_ env -> env
			}

import dynamic_string
// very unsafe operation
forceType :: !a -> b
forceType a = fst (copy_from_string (copy_to_string a))