implementation module Identity

from Functor import class Functor
from Monad import class Monad

:: Identity a = Identity a

runIdentity :: (Identity .a) -> .a
runIdentity (Identity a) = a

instance Functor Identity where
  fmap f (Identity m) = Identity (f m)

instance Monad Identity where
    return a   = Identity a
    (>>=) (Identity m) k  = k m

