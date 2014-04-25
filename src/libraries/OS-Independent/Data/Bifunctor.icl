implementation module Data.Bifunctor

from StdFunc import o, id
import Data.Either

instance Bifunctor (,) where
  bifmap f g t = let (a, b) = t in (f a, g b)
  first f d = bifmap f id d
  second g d = bifmap id g d

instance Bifunctor ((,,) x) where
  bifmap f g t = let (x, a, b) = t in (x, f a, g b)
  first f d = bifmap f id d
  second g d = bifmap id g d

instance Bifunctor ((,,,) x y) where
  bifmap f g t = let (x, y, a, b) = t in (x, y, f a, g b)
  first f d = bifmap f id d
  second g d = bifmap id g d

instance Bifunctor ((,,,,) x y z) where
  bifmap f g t = let (x, y, z, a, b) = t in (x, y, z, f a, g b)
  first f d = bifmap f id d
  second g d = bifmap id g d

instance Bifunctor Either where
  bifmap f _ (Left a) = Left (f a)
  bifmap _ g (Right b) = Right (g b)
  first f d = bifmap f id d
  second g d = bifmap id g d

