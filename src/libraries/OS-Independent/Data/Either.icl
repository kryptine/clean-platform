implementation module Data.Either

from Data.Func import abort

from Data.Functor import class Functor (..)

from Control.Applicative import class Applicative (..)
from Control.Monad import class Monad (..)

instance Functor (Either a) where
  fmap f (Left l) = Left l
  fmap f (Right r) = Right (f r)

instance Applicative (Either e) where
  pure x = Right x

  (<*>) (Left  e) _ = Left e
  (<*>) (Right f) r = fmap f r

instance Monad (Either e) where
  bind (Left  l) _ = Left l
  bind (Right r) k = k r

either :: (a -> c) (b -> c) (Either a b) -> c
either f _ (Left x) =  f x
either _ g (Right y) =  g y

isLeft :: !(Either a b) -> Bool
isLeft (Left _) = True
isLeft _ = False

isRight :: !(Either a b) -> Bool
isRight (Right _) = True
isRight _ = False

fromLeft :: !(Either .a .b) -> .a
fromLeft (Left x) = x
fromLeft _ = abort "Data.Either.fromLeft: argument is Right"

fromRight :: !(Either .a .b) -> .b
fromRight (Right x) = x
fromRight _ = abort "Data.Either.fromRight: argument is Left"

