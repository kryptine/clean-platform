implementation module Data.CircularStack

import StdInt, StdOverloaded, StdArray, StdMisc, StdList

:: CircularStack a =
  { maxSize    :: !Int
  , actualSize :: !Int
  , currIdx    :: !Int
  , stackData  :: !.{a}
  }

newStack :: !Int -> .(CircularStack a)
newStack n = { CircularStack
             | maxSize    = n
             , actualSize = 0
             , currIdx    = 0
             , stackData  = createArray n undef
             }

push :: a *(CircularStack a) -> *CircularStack a
push x stack
  = { stack
    & stackData.[stack.currIdx] = x
    , actualSize                = if (stack.actualSize == stack.maxSize)
                                    stack.actualSize
                                    (stack.actualSize + 1)
    , currIdx                   = (stack.currIdx + 1) modulo stack.maxSize
    }

pop :: .(CircularStack a) -> (a, CircularStack a)
pop stack
  | isEmpty stack = abort "Cannot pop from empty stack"
  | otherwise     = (stack.stackData.[stack.currIdx], {stack & currIdx = topIdx stack})

peek :: .(CircularStack a) -> a
peek stack
  | isEmpty stack = abort "Cannot peek in empty stack"
  | otherwise     = stack.stackData.[topIdx stack]

topIdx :: .(CircularStack a) -> Int
topIdx stack
  | stack.currIdx == 0 = stack.maxSize - 1
  | otherwise          = stack.currIdx - 1

isEmpty :: .(CircularStack a) -> Bool
isEmpty stack = stack.actualSize == 0

toList :: .(CircularStack a) -> [a]
toList stack
  | isEmpty stack = []
  | otherwise
      # (x, stack) = pop stack
      = [x : toList stack]

fromList :: [a] -> CircularStack a
fromList xs = foldr push (newStack (length xs)) xs

(modulo) infixr 4 :: !Int !Int -> Int
(modulo) i n
  | n == 0    = abort "Division by zero"
  | n == -1   = 0
  | i == n    = 0
  | i < n     = i
  | otherwise = i - ((i / n) * n)

