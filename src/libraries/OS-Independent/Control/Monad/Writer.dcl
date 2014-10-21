definition module Control.Monad.Writer

import Control.Monad
import Data.Monoid
import Control.Monad.Trans
import Data.Functor.Identity
from Data.Void import :: Void

:: WriterT w m a = WriterT (m (a, w))

:: Writer w a :== WriterT w Identity a

instance Monad (WriterT w m) | Monad m & Monoid w

instance MonadTrans (WriterT w) | Monoid w

runWriterT   :: (WriterT a u:b c) -> u:(b (c,a))
writer       :: .(.(a,b) -> WriterT b .Identity a)
runWriter    :: .((WriterT a .Identity b) -> (b,a))
execWriter   :: (WriterT a .Identity b) -> a
mapWriter    :: u:((a,b) -> .(c,d)) -> v:((WriterT b .Identity a) -> WriterT d .Identity c), [v <= u]
execWriterT  :: .(WriterT a b c) -> b a | Monad b
mapWriterT   :: .(u:(a (b,c)) -> v:(d (e,f))) (WriterT c u:a b) -> WriterT f v:d e
tell         :: a -> .(WriterT a b Void) | Monad b
listen       :: .(WriterT a b c) -> .(WriterT a b (c,a)) | Monad b
pass         :: .(WriterT a b (c,a -> d)) -> .(WriterT d b c) | Monad b
listens      :: (a -> b) .(WriterT a c d) -> WriterT a c (d,b) | Monad c & Monoid a
censor       :: (a -> b) .(WriterT a c d) -> .(WriterT b c d) | Monad c & Monoid a

