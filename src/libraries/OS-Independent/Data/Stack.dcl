definition module Data.Stack
from Data.Maybe import :: Maybe
from StdOverloaded import class length

:: Stack a = Stack [a]

newStack :: Stack a

empty a :== case q of
	Stack [] -> True
	_	 -> False

instance length Stack

// Add element to the Stack
push :: a (Stack a) -> (Stack a)

// Remove element from the Stack and return it if the Stack is not empty
pop :: (Stack a) -> (Maybe a, LIFO a)
