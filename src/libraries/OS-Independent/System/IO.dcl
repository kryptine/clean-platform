definition module System.IO

from Control.Applicative import class Applicative
from Data.Functor import class Functor
from Control.Monad import class Monad

:: IO a

execIO :: (IO a) *World -> *World

evalIO :: (IO a) *World -> *(a, *World)

withWorld :: (*World -> *(a, !*World)) -> IO a

instance Applicative IO
instance Functor IO
instance Monad IO
