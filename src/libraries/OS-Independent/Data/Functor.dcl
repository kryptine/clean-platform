definition module Data.Functor

from System.IO import :: IO
from StdFunc import const

class Functor f where
    fmap :: (a -> b) !(f a) -> f b

    (<$>) infixl 4 :: (a -> b) !(f a) -> f b
    (<$>) f fa :== fmap f fa

    (<$) infixl 4 :: a !(f b) -> f a
    (<$) x fa :== fmap (const x) fa

    ($>) infixl 4 :: !(f b) a -> f a
    ($>) fa x :== x <$ fa

    void :: !(f a) -> f ()
    void x :== () <$ x

instance Functor ((->) r)
instance Functor ((,) a)

