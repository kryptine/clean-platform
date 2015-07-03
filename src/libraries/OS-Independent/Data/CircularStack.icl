implementation module Data.CircularStack

//import StdInt, StdOverloaded, StdArray, StdMisc, StdList
import StdInt, StdList, StdMisc, Data.Maybe
from Data.IntMap.Strict import :: IntMap
import qualified Data.IntMap.Strict as DIS

:: CircularStack a =
  { maxSize    :: !Int
  , actualSize :: !Int
  , nextIdx    :: !Int
  , stackData  :: !IntMap a
  }


newStack :: !Int -> CircularStack a
newStack n = { CircularStack
             | maxSize    = n
             , actualSize = 0
             , nextIdx    = 0
             , stackData  = 'DIS'.newMap
             }

push :: a (CircularStack a) -> CircularStack a
push x stack
  = { stack
    & stackData  = 'DIS'.put stack.nextIdx x stack.stackData
    , actualSize = if (stack.actualSize == stack.maxSize)
                     stack.actualSize
                     (stack.actualSize + 1)
    , nextIdx    = (stack.nextIdx + 1) modulo stack.maxSize
    }

pop :: (CircularStack a) -> (a, CircularStack a)
pop stack
  | emptyStack stack = abort "Cannot pop from empty stack"
  | otherwise
      # topIdx = topElemIdx stack
      = ( fromJust ('DIS'.get topIdx stack.stackData)
        , { stack
          & nextIdx = topIdx
          , actualSize = stack.actualSize - 1})

peek :: (CircularStack a) -> a
peek stack
  | emptyStack stack = abort "Cannot peek in empty stack"
  | otherwise        = fromJust ('DIS'.get (topElemIdx stack) stack.stackData)

topElemIdx :: (CircularStack a) -> Int
topElemIdx stack
  | stack.nextIdx == 0 = stack.maxSize - 1
  | otherwise          = stack.nextIdx - 1

emptyStack :: (CircularStack a) -> Bool
emptyStack stack = stack.actualSize == 0

toList :: (CircularStack a) -> [a]
toList stack
  | emptyStack stack = []
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

