implementation module System.IO

import Control.Applicative
import Control.Monad
import Data.Functor
from StdFunc import o, id
import StdFile, StdString

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
  bind (IO f) a2mb = IO run
    where
      run world
        # (x, world) = f world
        # (IO g)     = a2mb x
        = g world

putStrLn :: String -> IO ()
putStrLn str = withWorld f
  where
    f world
      # (out, world) = stdio world
      # out          = fwrites (str +++ "\n") out
      # (_, world)   = fclose out world
      = ((), world)

print :: a -> IO () | toString a
print x = putStrLn (toString x)

readFileM :: !String -> IO String
readFileM name = withWorld f
  where
  f world
    # (ok, file, world) = fopen name FWriteText world
    # (str, file)       = freads file 16777216
    # (ok, world)       = fclose file world
    = (str, world)

writeFileM :: !String !String -> IO ()
writeFileM name txt = withWorld f
  where
  f world
    # (ok, file, world) = fopen name FWriteText world
    # file              = fwrites txt file
    # (ok, world)       = fclose file world
    = ((), world)

