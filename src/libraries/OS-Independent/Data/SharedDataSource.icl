implementation module SharedDataSource

import _SharedDataSourceTypes, StdString, StdFunc, Error, StdTuple, Func, Tuple, StdMisc

createChangeOnWriteSDS ::
	!String
	!String
	!(*env -> *(!MaybeErrorString r, !*env))
	!(w *env -> *(!MaybeErrorString Void, !*env))
	->
	RWShared r w *env
createChangeOnWriteSDS type id read write
	= createSDS (Just basicId) (\env -> appFst (fmap (\r -> (r,RegisterId basicId))) (read env)) write
where
	basicId = type +++ ":" +++ id
	
createReadOnlySDS ::
	!(*env -> *(!r, !*env))
	->
	ROShared r *env
createReadOnlySDS read
	= createReadOnlySDSError (appFst Ok o read)
	
createReadOnlySDSError ::
	!(*env -> *(!MaybeErrorString r, !*env))
	->
	ROShared r *env
createReadOnlySDSError read
	= createSDS Nothing (\env -> appFst (fmap (\r -> (r, None))) (read env)) (\_ env -> (Ok Void, env))

createReadOnlySDSPredictable ::
	!(*env -> *(!(!a, !Timestamp), !*env))
	->
	ROShared a *env
createReadOnlySDSPredictable read
	= createReadOnlySDSErrorPredictable (appFst Ok o read)
	
createReadOnlySDSErrorPredictable ::
	!(*env -> *(!MaybeErrorString (!a, !Timestamp), !*env))
	->
	ROShared a *env
createReadOnlySDSErrorPredictable read
	= createSDS Nothing (\env -> appFst (fmap (appSnd Timer)) (read env)) (\_ env -> (Ok Void, env))

createSDS ::
	!(Maybe BasicShareId)
	!(*env -> *(!MaybeErrorString (!r, !ChangeNotification), !*env))
	!(w *env -> *(!MaybeErrorString Void, !*env))
	->
	RWShared r w *env
createSDS id read write = BasicSource
	{ BasicSource
	| read = read
	, write = write
	, mbId = id
	}
		
read :: !(RWShared r w *env) !*env -> (!MaybeErrorString r, !*env)
read sds env = read` Nothing sds env

readRegister :: !msg !(RWShared r w *env) !*env -> (!MaybeErrorString r, !*env) | registerSDSMsg msg env
readRegister msg sds env = read` (Just notify) sds env
where
	notify None				env = env
	notify (RegisterId id)	env = registerDependency id msg env
	notify (Timer t)		env = registerTimedMsg t msg env

read` :: !(Maybe (ChangeNotification *env -> *env)) !(RWShared r w *env) !*env -> (!MaybeErrorString r, !*env)
read` mbNotificationF (BasicSource {read}) env = case read env of
	(Ok (r, notification), env)
		# env = case mbNotificationF of
			Just notificationF 	= notificationF notification env
			Nothing				= env
		= (Ok r, env)
	(err, env)
		= (liftError err, env)
read` mbNotificationF (ComposedRead share cont) env = seqErrorsSt (read` mbNotificationF share) (f mbNotificationF cont) env
where
	f :: !(Maybe (ChangeNotification *env -> *env))  !(x -> MaybeErrorString (RWShared r w *env)) !x !*env -> (!MaybeErrorString r, !*env)
	f mbNotificationF cont x env = seqErrorsSt (\env -> (cont x, env)) (read` mbNotificationF) env
read` mbNotificationF (ComposedWrite share _ _) env = read` mbNotificationF share env
	
write :: !w !(RWShared r w *env) !*env -> (!MaybeErrorString Void, !*env) | reportSDSChange Void env
write w sds env = write` w sds filter env
where
	filter :: !Void -> Bool
	filter _ = True
	
writeFilterMsg :: !w !(msg -> Bool) !(RWShared r w *env) !*env -> (!MaybeErrorString Void, !*env) | reportSDSChange msg env
writeFilterMsg w filter sds env = write` w sds filter env
	
write` :: !w !(RWShared r w *env) !(msg -> Bool) !*env -> (!MaybeErrorString Void, !*env) | reportSDSChange msg env
write` w (BasicSource {mbId,write}) filter env
	# (mbErr, env) = write w env
	# env = case mbId of
		Just id	= reportSDSChange id filter env
		Nothing	= env
	= (mbErr, env)
write` w (ComposedRead share _) filter env = write` w share filter env
write` w (ComposedWrite _ readCont writeOp) filter env
	# (er, env)	= seqErrorsSt (\env -> (readCont w, env)) read env
	| isError er = (liftError er, env)
	# ewrites	= writeOp w (fromOk er)
	| isError ewrites = (liftError ewrites, env)
	# (res,env)	= mapSt (\(Write w share) -> write` w share filter) (fromOk ewrites) env
	// TODO: check for errors in res
	= (Ok Void, env)

/*getHash :: !(RWShared r w *env) !*env -> (!MaybeErrorString Hash, !*env)
getHash share env
	# (res,env) = read share env
	= (fmap snd res, env)*/
	
(>?>) infixl 6 :: !(RWShared rx wx *env) !(rx -> MaybeErrorString (RWShared ry wy *env)) -> RWShared ry wx *env
(>?>) sharex cont = ComposedRead sharex cont

(>!>) infixl 6 :: !(RWShared r w` *env) !(!w -> MaybeErrorString (RWShared r` w`` *env), !w r` -> MaybeErrorString [WriteShare *env]) -> RWShared r w *env
(>!>) share (readOp,writeOp) = ComposedWrite share readOp writeOp

mapRead :: !(r -> r`) !(RWShared r w *env) -> RWShared r` w *env
mapRead get share = mapReadError (Ok o get) share

mapWrite :: !(w` r -> Maybe w) !(RWShared r w *env) -> RWShared r w` *env
mapWrite put share = mapWriteError (\w` r -> Ok (put w` r)) share

mapReadWrite :: !(!r -> r`,!w` r -> Maybe w) !(RWShared r w *env) -> RWShared r` w` *env
mapReadWrite (readMap,writeMap) shared = mapRead readMap (mapWrite writeMap shared)

mapReadError :: !(r -> MaybeErrorString r`) !(RWShared r w *env) -> RWShared r` w *env
mapReadError proj share = share >?> \r -> fmap constShare (proj r)

mapWriteError :: !(w` r -> MaybeErrorString (Maybe w)) !(RWShared r w *env) -> RWShared r w` *env
mapWriteError proj share = share >!> (const (Ok share),\w` r -> fmap (maybe [] (\w -> [Write w share])) (proj w` r))
	
mapReadWriteError :: !(!r -> MaybeErrorString r`,!w` r -> MaybeErrorString (Maybe w)) !(RWShared r w *env) -> RWShared r` w` *env
mapReadWriteError (readMap,writeMap) shared = mapReadError readMap (mapWriteError writeMap shared)

symmetricLens :: !(a b -> b) !(b a -> a) !(Shared a *env) !(Shared b *env) -> (!Shared a *env, !Shared b *env)
symmetricLens putr putl sharedA sharedB = (newSharedA,newSharedB)
where
	sharedAll = sharedA >+< sharedB 
	newSharedA = mapReadWrite (fst,\a (_,b) -> Just (a,putr a b)) sharedAll
	newSharedB = mapReadWrite (snd,\b (a,_) -> Just (putl b a,b)) sharedAll

toReadOnly :: !(RWShared r w *env) -> ROShared r *env
toReadOnly share = mapWrite (\_ _ -> Nothing) share

(>+<) infixl 6 :: !(RWShared rx wx *env) !(RWShared ry wy *env) -> RWShared (rx,ry) (wx,wy) *env
(>+<) shareX shareY = (shareX >?> \rx -> Ok (mapRead (\ry -> (rx,ry)) shareY)) >!> (const (Ok (constShare Void)),\(wx,wy) _ -> Ok [Write wx shareX, Write wy shareY])

(>+|) infixl 6 :: !(RWShared rx wx *env) !(RWShared ry wy *env) -> RWShared (rx,ry) wx *env
(>+|) srcX srcY = mapWrite (\wx _ -> Just (wx, Void)) (srcX >+< toReadOnly srcY)

(|+<) infixl 6 :: !(RWShared rx wx *env) !(RWShared ry wy *env) -> RWShared (rx,ry) wy *env
(|+<) srcX srcY = mapWrite (\wy _ -> Just (Void, wy)) (toReadOnly srcX >+< srcY)

(|+|) infixl 6 :: !(RWShared rx wx *env) !(RWShared ry wy *env) -> RWShared (rx,ry) Void *env
(|+|) srcX srcY = toReadOnly (srcX >+< srcY)

null :: WOShared a *env
null = createSDS Nothing (\env -> (Ok (Void, None), env)) (\_ env -> (Ok Void, env))
			
constShare :: !a -> ROShared a *env
constShare v = createReadOnlySDS (\env -> (v, env))

import dynamic_string

genHash :: !a -> Hash
genHash x = copy_to_string x // fake hash implementation
