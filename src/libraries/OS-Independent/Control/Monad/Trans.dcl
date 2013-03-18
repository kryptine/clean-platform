definition module Control.Monad.Trans

from Monad import class Monad

class MonadTrans t where
  lift :: (m a) -> t m a | Monad m
