implementation module Data.Functor

from StdFunc import o
import Control.Applicative
import Control.Monad
import Data.IO

instance Functor ((->) r) where
  fmap f g = \x -> (f o g) x

instance Functor ((,) a) where
  fmap f (x, y) = (x, f y)

instance Functor IO where
  fmap f x = x >>= (lift o f)

(<$>) infixl 4 :: (a -> b) (f a) -> (f b) | Functor f
(<$>) f fa = fmap f fa
