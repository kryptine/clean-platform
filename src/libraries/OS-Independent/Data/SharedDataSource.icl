implementation module SharedDataSource

import _SharedDataSourceTypes, _SharedDataSourceOsDependent, StdTuple, FilePath, Void, Maybe, StdBool, StdMisc, StdList, StdFunc, StdString, StdOrdList, Tuple, Func, Error
from Map import qualified :: Map, newMap, toList, getU, put

createBasicDataSource ::
	!String
	!String
	!(*env -> *(!BasicSourceOps b *env, !*env))
	!(b -> r)
	!(w b -> b)
	->
	RWShared r w *env
createBasicDataSource type id mkOps get putback = BasicSource
	{ id = type +++ ":" +++ id
	, mkOps = mkOps
	, get = get
	, putback = \w b -> Just (putback w b)
	}
	
createProxyDataSource :: !(*env -> *(!RWShared r` w` *env, !*env)) !(r` -> r) !(w r` -> w`) -> RWShared r w *env
createProxyDataSource getSource get put = ProxySource
	{ getSource	= getSource
	, get		= get
	, put		= \r w -> Just (put r w)
	}
		
read :: !(RWShared r w *env) !*env -> (!MaybeErrorString (!r, !Version), !*env)
read shared env
	# (ops, env)	= lock False shared env
	# (res, env)	= readSharedData` ops env
	# env			= close ops env
	= (res, env)
	
write :: !w !(RWShared r w *env) !*env -> (!MaybeErrorString Void, !*env)
write w shared env
	# (ops, env)	= lock True shared env
	# (res,env)		= writeSharedData` w ops env
	# env			= close ops env
	= (res,env)

getVersion :: !(RWShared r w *env) !*env -> (!MaybeErrorString Version, !*env)
getVersion shared env
	# (ops, env)		= lock False shared env
	# (ver, env)		= getVersion` ops env
	# env				= close ops env
	= (ver, env)

readWrite :: !(r Version -> (RWRes w a)) !(RWShared r w *env) !*env -> (!MaybeErrorString a, !*env)
readWrite f shared env = unsafeRW (\r ver env -> (f r ver, env)) shared env
			
unsafeRW :: !(r Version *env -> (RWRes w a, *env))	!(RWShared r w *env)	!*env -> (!MaybeErrorString a, !*env)
unsafeRW f shared env
	# (ops, env)		= lock True shared env
	# (res, env)		= readSharedData` ops env
	| isError res = (liftError res, env)
	# (r, ver) = fromOk res
	= case f r ver env of
		(YieldResult a, env)
			# env		= close ops env
			= (Ok a, env)
		(Write w a, env)
			# (res,env)	= writeSharedData` w ops env
			# env		= close ops env
			| isError res = (liftError res, env)
			= (Ok a, env)
		(Redo, env)
			# env		= wait ops env
			= unsafeRW f shared env
			
lock :: !Bool !(RWShared r w *env) !*env -> (SharedOps r w *env, !*env)
lock exclusive shared env
	# (locks, ops, env)	= lock` shared env
	// sort locks to avoid deadlocks
	#locks				= sortBy (\x y -> fst x < fst y) (removeDup` locks)
	# env				= seqSt (\(_,lock) env -> lock env) locks env
	= (ops, env)
where
	lock` :: !(RWShared r w *env) !*env -> ([(SharedId, *env -> *env)], SharedOps r w *env, !*env)
	lock` (BasicSource {BasicSource|id, mkOps, get, putback}) env
		# (ops, env) = mkOps env
		# lockF		= if exclusive ops.lockExcl ops.lock
		= ([(id, lockF)], BasicSourceOps get putback ops, env)
	lock` (ComposedSource {srcX, srcY, get, putback}) env
		# (lockX, opsX, env)	= lock` srcX env
		# (lockY, opsY, env)	= lock` srcY env
		# cops =	{ opsX = opsX
					, opsY = opsY
					, get = get
					, putback = putback
					, addObserver = \obs env -> addObserver opsY obs (addObserver opsX obs env)
					}
		= (lockX ++ lockY, ComposedSourceOps cops, env)
	lock` (ProxySource {getSource,get,put}) env
		# (src, env) = getSource env
		= lock` (mapReadWrite (get,put) src) env
		
	removeDup` [x:xs] = [x:removeDup` (filter (\y -> fst x <> fst y) xs)]
	removeDup` _      = []
	
	addObserver :: !(SharedOps r w *env) !OBSERVER !*env -> *env
	addObserver (BasicSourceOps _ putback {BasicSourceOps|addObserver}) waiter env
		= addObserver waiter env
	addObserver (ComposedSourceOps {opsX, opsY}) waiter env
		= addObserver opsY waiter (addObserver opsX waiter env)
	
readSharedData` :: !(SharedOps r w *env) !*env ->(!MaybeErrorString (!r, !Version), !*env)	
readSharedData` (BasicSourceOps get _ {BasicSourceOps|read}) env
	= appFst (fmap (\(r,v) -> (get r, v))) (read env)
readSharedData` (ComposedSourceOps {opsX, opsY, get}) env
	# (x, env)	= readSharedData` opsX env
	| isError x = (liftError x, env)
	# (y, env)	= readSharedData` opsY env
	| isError y = (liftError y, env)
	# (rx, verx) = fromOk x
	# (ry, very) = fromOk y
	= (Ok (get rx ry, verx + very), env)
	
writeSharedData` :: !w !(SharedOps r w *env) !*env -> (!MaybeErrorString Void, !*env)	
writeSharedData` w (BasicSourceOps _ putback {BasicSourceOps|read, write}) env
	# (res, env)	= read env
	| isError res = (liftError res, env)
	# (b, _) 		= fromOk res
	= case putback w b of
		Nothing	= (Ok Void, env)
		Just b	= write b env
writeSharedData` w (ComposedSourceOps {opsX, opsY, get, putback}) env
	# (x, env)		= readSharedData` opsX env
	| isError x = (liftError x, env)
	# (y, env)		= readSharedData` opsY env
	| isError y = (liftError y, env)
	# (rx, verx) 	= fromOk x
	# (ry, very) 	= fromOk y
	= case putback w rx ry of
		Nothing
			= (Ok Void, env)
		Just (wx, wy)
			# (x,env)	= writeSharedData` wx opsX env
			| isError x = (liftError x, env)
			= writeSharedData` wy opsY env

getVersion` :: !(SharedOps r w *env) !*env -> (!MaybeErrorString Version, !*env)	
getVersion` (BasicSourceOps get _ {BasicSourceOps|getVersion}) env
	= getVersion env
getVersion` (ComposedSourceOps {opsX, opsY, get}) env
	# (verx, env)	= getVersion` opsX env
	| isError verx = (liftError verx, env)
	# (very, env)	= getVersion` opsY env
	| isError very = (liftError very, env)
	= (Ok (fromOk verx + fromOk very), env)

wait :: !(SharedOps r w *env) !*env -> *env
wait ops env = waitOsDependent [addObs] (close ops) env
where
	addObs = case ops of
		BasicSourceOps _ _ {BasicSourceOps|addObserver}		= addObserver
		ComposedSourceOps {ComposedSourceOps|addObserver}	= addObserver
		
mapRead :: !(r -> r`) !(RWShared r w *env) -> RWShared r` w *env
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
mapRead get` (ProxySource share=:{ProxySource|get}) = ProxySource
	{ ProxySource
	| share
	& get = get` o get
	}

mapWrite :: !(w` r -> Maybe w) !(RWShared r w *env) -> RWShared r w` *env
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
mapWrite put` (ProxySource share=:{ProxySource|put,get}) = ProxySource
	{ ProxySource
	| share
	& put = \w` b -> maybe Nothing (\w -> put w b) (put` w` (get b))
	}

mapReadWrite :: !(!r -> r`,!w` r -> Maybe w) !(RWShared r w *env) -> RWShared r` w` *env
mapReadWrite (readMap,writeMap) shared = mapRead readMap (mapWrite writeMap shared)

symmetricLens :: !(a b -> b) !(b a -> a) !(Shared a *env) !(Shared b *env) -> (!Shared a *env, !Shared b *env)
symmetricLens putr putl sharedA sharedB = (newSharedA,newSharedB)
where
	sharedAll = sharedA >+< sharedB 
	newSharedA = mapReadWrite (fst,\a (_,b) -> Just (a,putr a b)) sharedAll
	newSharedB = mapReadWrite (snd,\b (a,_) -> Just (putl b a,b)) sharedAll

toReadOnly :: !(RWShared r w *env) -> ROShared r *env
toReadOnly share = mapWrite (\_ _ -> Nothing) share

(>+<) infixl 6 :: !(RWShared rx wx *env) !(RWShared ry wy *env) -> RWShared (rx,ry) (wx,wy) *env
(>+<) srcX srcY = ComposedSource {srcX = srcX, srcY = srcY, get = tuple, putback = \w _ _ -> Just w}

(>+|) infixl 6 :: !(RWShared rx wx *env) !(RWShared ry wy *env) -> RWShared (rx,ry) wx *env
(>+|) srcX srcY = mapWrite (\wx _ -> Just (wx, Void)) (srcX >+< toReadOnly srcY)

(|+<) infixl 6 :: !(RWShared rx wx *env) !(RWShared ry wy *env) -> RWShared (rx,ry) wy *env
(|+<) srcX srcY = mapWrite (\wy _ -> Just (Void, wy)) (toReadOnly srcX >+< srcY)

(|+|) infixl 6 :: !(RWShared rx wx *env) !(RWShared ry wy *env) -> RWShared (rx,ry) Void *env
(|+|) srcX srcY = toReadOnly (srcX >+< srcY)

// STM
:: *Trans *env =	{ env	:: !env
				, log	:: !*'Map'.Map SharedId (TLogEntry env)
				}
:: TLogEntry *env = E.b: TLogEntry !b !Version !Bool !(BasicSourceOps b env)

atomic :: !((*Trans *env) -> (!TRes a, !*Trans *env)) !*env -> (!MaybeErrorString a, !*env)
atomic trF env
	# tr					 =	{ env	= env
								, log	= 'Map'.newMap
								}
	# (res, {env, log})		= trF tr
	// list retrieved from log is already sorted
	# entries				= map snd ('Map'.toList log)
	= case res of
		Retry
			# env			= waitOsDependent [addObserver \\ (TLogEntry _ _ _ {BasicSourceOps|addObserver}) <- entries] (seqSt (\(TLogEntry _ _ _ {close}) env -> close env) entries) env
			= atomic trF env
		TYieldResult a
			# env			= seqSt (\(TLogEntry _ _ _ {lockExcl}) env -> lockExcl env) entries env
			# (retry, env)	= checkConsistency entries env
			| isError retry = (liftError retry, env)
			| fromOk retry
				# env		= seqSt (\(TLogEntry _ _ _ {unlock, close}) env -> close (unlock env)) entries env
				= atomic trF env
			| otherwise
				# (res,env)	= seqSt commit entries (Ok Void, env)
				# env		= seqSt (\(TLogEntry _ _ _ {unlock, close}) env -> close (unlock env)) entries env
				| isError res = (liftError res, env)
				= (Ok a, env)
where
	checkConsistency :: ![TLogEntry *env] !*env -> *(!MaybeErrorString Bool,!*env)
	checkConsistency [] env = (Ok False, env)
	checkConsistency [TLogEntry _ lVer _ {getVersion}:entries] env
		# (cVer, env)			= getVersion env
		| isError cVer			= (liftError cVer, env)
		| fromOk cVer == lVer	= checkConsistency entries env
		| otherwise				= (Ok True, env)
		
	commit :: !(TLogEntry *env) !(!MaybeErrorString Void, !*env) -> (!MaybeErrorString Void, !*env)
	commit (TLogEntry b _ doWrite {write,unlock}) (res,env)
		| isError res		= (res, env)
		| doWrite			= write b env
		| otherwise			= (Ok Void, env)

transRead :: !(RWShared r w *env) !(Trans *env) -> (!r, !(Trans *env))	
transRead (BasicSource {BasicSource|id, get, mkOps}) tr=:{log, env}
	# (mbEntry, log)	= 'Map'.getU id log
	= case mbEntry of
		Nothing
			# (ops, env)	= mkOps env
			# env			= ops.lock env
			# (res, env)	= ops.read env
			| isError res = abort "error during transRead"
			# (b, ver)		= fromOk res
			# env			= ops.unlock env
			# log			= 'Map'.put id (TLogEntry b ver False ops) log
			= (get b, {tr & log = log, env = env})
		Just (TLogEntry b _ _ _)
			= (get (forceType b), {tr & log = log})

transRead (ComposedSource {srcX, srcY, get}) tr
	# (rx, tr)	= transRead srcX tr
	# (ry, tr)	= transRead srcY tr
	= (get rx ry, tr)
	
transRead (ProxySource {getSource,get,put}) tr=:{env}
		# (src, env) = getSource env
		= transRead (mapReadWrite (get,put) src) {tr & env = env}
	
transWrite :: !w !(RWShared r w *env) !(Trans *env) -> (Trans *env)
transWrite w (BasicSource {BasicSource|id, putback, mkOps}) tr=:{log, env}
	# (mbEntry, log)	= 'Map'.getU id log
	= case mbEntry of
		Nothing
			# (ops, env)	= mkOps env
			# env			= ops.lock env
			# (res, env)	= ops.read env
			| isError res = abort "error during transRead"
			# (b, ver)		= fromOk res
			# env			= ops.unlock env
			# b = case putback w b of
				Nothing	= b
				Just b	= b
			# log			= 'Map'.put id (TLogEntry b ver True ops) log
			= {tr & log = log, env = env}
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
	ops =	{ read			= \env -> (Error "reading null share", env)
			, write			= \_ env -> (Ok Void, env)
			, getVersion	= \env -> (Ok 0, env)
			, lock			= id
			, unlock		= id
			, lockExcl		= id
			, close			= id
			, addObserver	= \_ env -> env
			}
			
constShare :: !a -> ROShared a *env
constShare v = createBasicDataSource "constShare" "" mkOps id (\_ b -> b)
where
	mkOps env = (ops, env)
	ops =	{ read			= \env -> (Ok (v, 0), env)
			, write			= \_ env -> (Ok Void, env)
			, getVersion	= \env -> (Ok 0, env)
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