implementation module SharedDataSource

import _SharedDataSourceTypes, StdString, StdFunc, Error, StdTuple, Func, Tuple

createBasicSDS ::
	!String
	!String
	!(*env -> *(!MaybeErrorString r, !*env))
	!(w *env -> *(!MaybeErrorString Void, !*env))
	->
	RWShared r w *env
createBasicSDS type id read write = BasicSource
	{ BasicSource
	| id = type +++ ":" +++ id
	, read = read
	, write = write
	}

createReadOnlySDS ::
	!String
	!String
	!(*env -> *(!a, !*env))
	->
	ROShared a *env
createReadOnlySDS type ident read
	= createReadOnlySDSError type ident (appFst Ok o read)
	
createReadOnlySDSError ::
	!String
	!String
	!(*env -> *(!MaybeErrorString a, !*env))
	->
	ROShared a *env
createReadOnlySDSError type ident read = createBasicSDS type ident read (\_ env -> (Ok Void, env))
		
read :: !(RWShared r w *env) !*env -> (!MaybeErrorString r, !*env)
read (BasicSource {read}) env = read env
read (ComposedRead share cont) env = seqErrorsSt (read share) (f cont) env
where
	f :: !(x -> MaybeErrorString (RWShared r w *env)) !x !*env -> (!MaybeErrorString r, !*env)
	f cont x env = seqErrorsSt (\env -> (cont x, env)) read env
read (ComposedWrite share _ _) env = read share env
	
write :: !w !(RWShared r w *env) !*env -> (!MaybeErrorString Void, !*env)
write w (BasicSource {write}) env = write w env
write w (ComposedRead share _) env = write w share env
write w (ComposedWrite _ readCont writeOp) env
	# (er, env)	= seqErrorsSt (\env -> (readCont w, env)) read env
	| isError er = (liftError er, env)
	# ewrites	= writeOp w (fromOk er)
	| isError ewrites = (liftError ewrites, env)
	# (res,env)	= mapSt (\(Write w share) -> write w share) (fromOk ewrites) env
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
null = createBasicSDS "Null" "" (\env -> (Ok Void, env)) (\_ env -> (Ok Void, env))
			
constShare :: !a -> ROShared a *env
constShare v = createReadOnlySDS "constShare" (genHash v) (\env -> (v, env))

import dynamic_string

genHash :: !a -> Hash
genHash x = copy_to_string x // fake hash implementation
