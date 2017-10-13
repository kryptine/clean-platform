implementation module Data.Functor

from StdFunc import o
import Control.Applicative

instance Functor ((->) r) where
  fmap f g = \x -> (f o g) x

instance Functor ((,) a) where
  fmap f (x, y) = (x, f y)

