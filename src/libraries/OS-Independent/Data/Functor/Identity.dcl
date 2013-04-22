definition module Data.Functor.Identity

from Data.Functor import class Functor
from Control.Monad import class Monad

:: Identity a = Identity a

instance Functor Identity

instance Monad Identity

runIdentity :: (Identity .a) -> .a

