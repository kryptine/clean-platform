implementation module Control.Monad.Writer

from Data.Func import $
import Data.Functor.Identity
import Control.Monad
import Data.Monoid
import Control.Monad.Trans
import Data.Void
from StdFunc import o
from StdTuple import fst, snd

:: WriterT w m a = WriterT (m (a, w))

:: Writer w a :== WriterT w Identity a

instance Functor (WriterT w m) | Monad m & Monoid w where
  fmap f m = liftM f m

instance Applicative (WriterT w m) | Monad m & Monoid w where
  pure x = WriterT (return (x, mempty))
  <*> mf mx = ap mf mx

instance Monad (WriterT w m) | Monad m & Monoid w where
  bind m k = WriterT (runWriterT m >>= \(a, w) ->
              runWriterT (k a) >>= \(b, w`) ->
              return (b, mappend w w`))

instance MonadTrans (WriterT w) | Monoid w where
  liftT m = WriterT $ m >>= \a -> return (a, mempty)

runWriterT :: (WriterT a u:b c) -> u:(b (c,a))
runWriterT (WriterT w) = w

writer :: .(.(a,b) -> WriterT b .Identity a)
writer = WriterT o Identity

runWriter :: .((WriterT a .Identity b) -> (b,a))
runWriter = runIdentity o runWriterT

execWriter :: (WriterT a .Identity b) -> a
execWriter m = snd (runWriter m)

mapWriter :: u:((a,b) -> .(c,d)) -> v:((WriterT b .Identity a) -> WriterT d .Identity c), [v <= u]
mapWriter f = mapWriterT (Identity o f o runIdentity)

execWriterT :: .(WriterT a b c) -> b a | Monad b
execWriterT m = runWriterT m >>= \(_, w) -> return w

mapWriterT :: .(u:(a (b,c)) -> v:(d (e,f))) (WriterT c u:a b) -> WriterT f v:d e
mapWriterT f m = WriterT $ f (runWriterT m)

tell :: a -> .(WriterT a b Void) | Monad b
tell w = WriterT $ return (Void, w)

listen :: .(WriterT a b c) -> .(WriterT a b (c,a)) | Monad b
listen m = WriterT $ runWriterT m >>= \(a, w) ->
                     return ((a, w), w)

pass :: .(WriterT a b (c,a -> d)) -> .(WriterT d b c) | Monad b
pass m = WriterT $ runWriterT m >>= \((a, f), w) ->
                     return (a, f w)

listens :: (a -> b) .(WriterT a c d) -> WriterT a c (d,b) | Monad c & Monoid a
listens f m = listen m >>= \(a, w) -> return (a, f w)

censor :: (a -> b) .(WriterT a c d) -> .(WriterT b c d) | Monad c & Monoid a
censor f m = pass $ m >>= \a -> return (a, f)
