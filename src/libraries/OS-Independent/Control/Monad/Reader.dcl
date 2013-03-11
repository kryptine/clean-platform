definition module Reader

from Monad import class Monad
import Identity
import Trans

:: ReaderT r m a = ReaderT (r -> m a)

:: Reader r a :== ReaderT r Identity a

runReaderT   :: .(ReaderT .a u:b .c) -> .a -> u:(b .c)
reader       :: (.a -> .b) -> .(ReaderT .a .Identity .b)
runReader    :: .(ReaderT .a u:Identity v:b) -> .(.a -> v:b), [u <= v]
mapReaderT   :: (u:(a .b) -> v:(c .d)) .(ReaderT .e u:a .b) -> .(ReaderT .e v:c .d)
mapReader    :: (u:a -> .b) -> .(.(ReaderT .c v:Identity u:a) -> .(ReaderT .c .Identity .b)), [v <= u]
withReaderT  :: (.a -> .b) .(ReaderT .b .c .d) -> .(ReaderT .a .c .d)
withReader   :: u:((.a -> .b) -> v:(.(ReaderT .b .c .d) -> .(ReaderT .a .c .d))), [v <= u]
liftReaderT  :: (a .b) -> .(ReaderT .c a .b)
ask          :: .(ReaderT a b a) | Monad b
local        :: u:((.a -> .b) -> v:(.(ReaderT .b .c .d) -> .(ReaderT .a .c .d))), [v <= u]
asks         :: (a -> b) -> ReaderT a c b | Monad c

instance Monad (ReaderT r m) | Monad m

instance MonadTrans (ReaderT r)