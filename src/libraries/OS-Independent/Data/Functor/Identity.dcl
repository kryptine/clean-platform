definition module Data.Functor.Identity

from Functor import class Functor
from Monad import class Monad

:: Identity a = Identity a

instance Functor Identity

instance Monad Identity

runIdentity :: (Identity .a) -> .a

