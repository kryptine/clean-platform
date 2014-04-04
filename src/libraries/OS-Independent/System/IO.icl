implementation module System.IO

import Control.Applicative
import Control.Monad
import Data.Functor
from StdFunc import o, id

:: IO a = IO (*World -> *(a, *World))

execIO :: (IO a) *World -> *World
execIO (IO f) world
  # (_, world) = f world
  = world

evalIO :: (IO a) *World -> *(a, *World)
evalIO (IO f) world = f world

withWorld :: (*World -> *(a, !*World)) -> IO a
withWorld f = IO f

instance Applicative IO where
  pure x     = IO (\s -> (x, s))
  (<*>) f g  = liftA2 id f g

instance Functor IO where
  fmap f x = x >>= (lift o f)

instance Monad IO where
  (>>=) (IO f) a2mb = IO run
    where
      run world
        # (x, world) = f world
        # (IO g)     = a2mb x
        = g world

