implementation module Control.Monad.Reader

from Data.Func import $
import Data.Functor.Identity
import Control.Monad
from StdFunc import o, const
import Control.Monad.Trans

:: ReaderT r m a = ReaderT (r -> m a)

:: Reader r a :== ReaderT r Identity a

instance Monad (ReaderT r m) | Monad m where
  return x = (liftT o return) x
  (>>=) m k = ReaderT (\r -> runReaderT m r >>= \a -> runReaderT (k a) r)

instance MonadTrans (ReaderT r) where
  liftT r = liftReaderT r

runReaderT :: .(ReaderT .a u:b .c) -> .a -> u:(b .c)
runReaderT (ReaderT f) = f

reader :: (.a -> .b) -> .(ReaderT .a .Identity .b)
reader f = ReaderT (Identity o f)

runReader :: .(ReaderT .a u:Identity v:b) -> .(.a -> v:b), [u <= v]
runReader m = runIdentity o runReaderT m

mapReader :: (u:a -> .b) -> .(.(ReaderT .c v:Identity u:a) -> .(ReaderT .c .Identity .b)), [v <= u]
mapReader f = mapReaderT (Identity o f o runIdentity)

withReader :: u:((.a -> .b) -> v:(.(ReaderT .b .c .d) -> .(ReaderT .a .c .d))), [v <= u]
withReader = withReaderT

mapReaderT :: (u:(a .b) -> v:(c .d)) .(ReaderT .e u:a .b) -> .(ReaderT .e v:c .d)
mapReaderT f m = ReaderT $ f o runReaderT m

withReaderT :: (.a -> .b) .(ReaderT .b .c .d) -> .(ReaderT .a .c .d)
withReaderT f m = ReaderT $ runReaderT m o f

liftReaderT :: (a .b) -> .(ReaderT .c a .b)
liftReaderT m = ReaderT (const m)

ask :: .(ReaderT a b a) | Monad b
ask = ReaderT return

local :: u:((.a -> .b) -> v:(.(ReaderT .b .c .d) -> .(ReaderT .a .c .d))), [v <= u]
local = withReaderT

asks :: (a -> b) -> ReaderT a c b | Monad c
asks f = liftM f ask
