implementation module Data.Either

from Control.Applicative import class Applicative (..)
from Control.Monad import class Monad (..)
from Data.Functor import class Functor (..)

instance Functor (Either a) where
  fmap f (Left l)  = Left l
  fmap f (Right r) = Right (f r)

instance Applicative (Either e) where
  pure x        = Right x
  (<*>) (Left  e) _ = Left e
  (<*>) (Right f) r = fmap f r

instance Monad (Either e) where
    (>>=) (Left  l) _ = Left l
    (>>=) (Right r) k = k r

either :: (a -> c) (b -> c) (Either a b) -> c
either f _ (Left x)     =  f x
either _ g (Right y)    =  g y
