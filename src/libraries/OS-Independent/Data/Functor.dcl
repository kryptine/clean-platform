definition module Data.Functor

from Data.IO import :: IO

class Functor f where
  fmap :: (a -> b) (f a) -> f b

instance Functor ((->) r)
instance Functor ((,) a)
instance Functor IO

(<$>) infixl 4 :: (a -> b) (f a) -> f b | Functor f

