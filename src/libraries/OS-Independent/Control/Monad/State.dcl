definition module Control.Monad.State

from Data.Void import :: Void
from Control.Monad import class Monad
from Data.Functor.Identity import :: Identity

:: StateT s m a = StateT (s -> m (a, s))

:: State s a :== StateT s Identity a

state       :: (a -> .(b,a)) -> .(StateT a c b) | Monad c
put         :: a -> .(StateT a b Void) | Monad b
modify      :: (a -> a) -> .(StateT a b Void) | Monad b
gets        :: (a -> b) -> .(StateT a c b) | Monad c
runStateT   :: .(StateT a u:b c) -> a -> u:(b (c,a))
runState    :: .(StateT a .Identity b) -> .(a -> (b,a))
evalState   :: .(StateT a .Identity b) a -> b
evalStateT  :: .(StateT a b c) a -> b c | Monad b
execState   :: .(StateT a .Identity b) a -> a
execStateT  :: .(StateT a b c) a -> b a | Monad b
mapStateT   :: (u:(a (b,c)) -> v:(d (e,c))) .(StateT c u:a b) -> .(StateT c v:d e)
mapState    :: ((a,b) -> .(c,b)) -> .(.(StateT b .Identity a) -> .(StateT b .Identity c))
withStateT  :: (a -> a) .(StateT a .b c) -> .(StateT a .b c)
withState   :: u:((a -> a) -> v:(.(StateT a .b c) -> .(StateT a .b c))), [v <= u]

instance Monad (StateT s m) | Monad m
