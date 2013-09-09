definition module Control.Monad.Fix

from Control.Monad import class Monad
from Control.Applicative import class Applicative
from Data.Functor import class Functor
from Data.Maybe import :: Maybe

class MonadFix m | Monad m where
  mfix :: (a -> m a) -> m a

instance MonadFix Maybe

instance MonadFix []