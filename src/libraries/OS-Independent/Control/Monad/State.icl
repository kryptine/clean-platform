implementation module State

from Func import $
import Monad
import Identity
import Trans
from Void import :: Void(..)
from StdFunc import o
from StdTuple import fst, snd
from StdMisc import abort

:: StateT s m a = StateT (s -> m (a, s))

:: State s a :== StateT s Identity a

instance Monad (StateT s m) | Monad m where
    return a = state $ \s -> (a, s)
    (>>=) m k  = StateT $ \s -> (runStateT m s >>= \(a, s`) -> runStateT (k a) s`)

instance MonadTrans (StateT s) where
  lift m = StateT $ \s -> m >>= \a -> return (a, s)

state :: (a -> .(b,a)) -> .(StateT a c b) | Monad c
state f = StateT (return o f)

get :: .(StateT a b a) | Monad b
get = state $ \s -> (s, s)

put :: a -> .(StateT a b Void) | Monad b
put s = state $ \_ -> (Void, s)

modify :: (a -> a) -> .(StateT a b Void) | Monad b
modify f = state $ \s -> (Void, f s)

gets :: (a -> b) -> .(StateT a c b) | Monad c
gets f = state $ \s -> (f s, s)

runState :: .(StateT a .Identity b) -> .(a -> (b,a))
runState m = runIdentity o runStateT m

runStateT :: .(StateT a u:b c) -> a -> u:(b (c,a))
runStateT (StateT f) = f

evalState :: .(StateT a .Identity b) a -> b
evalState m s = fst (runState m s)

evalStateT :: .(StateT a b c) a -> b c | Monad b
evalStateT m s = runStateT m s >>= \(a, _) -> return a

execState :: .(StateT a .Identity b) a -> a
execState m s = snd (runState m s)

execStateT :: .(StateT a b c) a -> b a | Monad b
execStateT m s = runStateT m s >>= \(_, s`) -> return s`

mapState :: ((a,b) -> .(c,b)) -> .(.(StateT b .Identity a) -> .(StateT b .Identity c))
mapState f = mapStateT (Identity o f o runIdentity)

mapStateT :: (u:(a (b,c)) -> v:(d (e,c))) .(StateT c u:a b) -> .(StateT c v:d e)
mapStateT f m = StateT $ f o runStateT m

withState :: u:((a -> a) -> v:(.(StateT a .b c) -> .(StateT a .b c))), [v <= u]
withState = withStateT

withStateT :: (a -> a) .(StateT a .b c) -> .(StateT a .b c)
withStateT f m = StateT $ runStateT m o f

