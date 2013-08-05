implementation module Data.Functor.Identity

from Data.Functor import class Functor
from Control.Monad import class Monad

:: Identity a = Identity a

runIdentity :: (Identity .a) -> .a
runIdentity (Identity a) = a

instance Functor Identity where
  fmap f (Identity m) = Identity (f m)

instance Applicative Identity where
  pure x = Identity x
  (<*>) (Identity f) (Identity x) = Identity (f x)

instance Monad Identity where
    return a   = Identity a
    (>>=) (Identity m) k  = k m

