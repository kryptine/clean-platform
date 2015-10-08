definition module Data.Either
/**
* This module defines the "Either" type to represent binary choice.
* Clean's generics define a similar type EITHER, but this should only be
* used inside generic functions, since most generic functions treat this
* type in a special way which may lead to strange behavior.
*/

from Data.Functor import class Functor (..)

from Control.Applicative import class Applicative (..)
from Control.Monad import class Monad (..)

:: Either a b = Left a | Right b

instance Functor (Either a)
instance Applicative (Either e)
instance Monad (Either e)

either :: (a -> c) (b -> c) (Either a b) -> c

// lefts :: [Either a b] -> [a]
// rights :: [Either a b] -> [b]
// partitionEithers :: [Either a b] -> ([a], [b])

isLeft :: !(Either a b) -> Bool
isRight :: !(Either a b) -> Bool

// Non Haskell functions //

fromLeft :: !(Either .a .b) -> .a
fromRight :: !(Either .a .b) -> .b

