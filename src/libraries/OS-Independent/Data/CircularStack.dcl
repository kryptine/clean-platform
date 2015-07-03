definition module Data.CircularStack

import StdArray

:: CircularStack a

newStack   :: !Int -> CircularStack a

push       :: a (CircularStack a) -> CircularStack a

pop        :: (CircularStack a) -> (a, CircularStack a)

peek       :: (CircularStack a) -> a

emptyStack :: (CircularStack a) -> Bool

fromList   :: [a] -> CircularStack a

toList     :: (CircularStack a) -> [a]
