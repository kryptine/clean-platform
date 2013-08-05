implementation module Control.Monad.Identity

import Data.Functor
import Control.Monad
import Control.Monad.Fix

runIdentity :: (Identity a) -> a
runIdentity (Identity x) = x

instance Functor Identity where
  fmap f m = Identity (f (runIdentity m))

instance Monad Identity where
  return a = Identity a
  (>>=) m k = k (runIdentity m)

instance MonadFix Identity where
  mfix f = Identity (fix (runIdentity o f))
