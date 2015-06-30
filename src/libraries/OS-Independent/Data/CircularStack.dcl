definition module Data.CircularStack

import StdArray

:: CircularStack a =
  { maxSize    :: !Int
  , actualSize :: !Int
  , currIdx    :: !Int
  , stackData  :: !.{a}
  }

newStack :: !Int -> .(CircularStack a)

push     :: a *(CircularStack a) -> *CircularStack a

pop      :: .(CircularStack a) -> (a, CircularStack a)

peek     :: .(CircularStack a) -> a

isEmpty  :: .(CircularStack a) -> Bool

toList   :: .(CircularStack a) -> [a]
