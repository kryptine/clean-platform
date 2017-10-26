definition module Data.Functor

from System.IO import :: IO
from StdFunc import const

class Functor f where
    fmap :: (a -> b) !(f a) -> f b

    (<$>) infixl 4 :: (a -> b) !(f a) -> f b | Functor f
    (<$>) f fa :== fmap f fa

    (<$) infixl 4 :: a !(f b) -> f a | Functor f
    (<$) x fa :== fmap (const x) fa

    ($>) infixl 4 :: !(f b) a -> f a | Functor f
    ($>) fa x :== x <$ fa

    void :: !(f a) -> f () | Functor f
    void x :== () <$ x

instance Functor ((->) r)
instance Functor ((,) a)

