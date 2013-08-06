implementation module Control.Monad.Trans

from Control.Monad import class Monad

class MonadTrans t where
  liftT :: (m a) -> t m a | Monad m
