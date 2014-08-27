definition module Data.Heap

from StdOverloaded import class < (..)
import StdClass
from Data.Maybe import :: Maybe
from StdFunc import o
import qualified Data.List as L

/**
 * Ported from Edward Kmett's Data.Heap by Jurriën Stutterheim 15-08-2014
 */

:: Heap a
  = Empty
  | Heap !Int (a -> a -> Bool) !(Tree a)

:: Tree a = Node Int a !(Forest a)

:: Forest a
  = Cons !(Tree a) !(Forest a)
  | Nil

instance == (Heap a)

instance < (Heap a)

:: Entry p a = Entry p a

null :: (Heap a) -> Bool

size :: (Heap a) -> Int

//empty :: Heap a
empty :== Empty

//singleton :: a -> Heap a | Ord a
singleton x :== singletonWith (<=) x

//singletonWith :: (a a -> Bool) a -> Heap a
singletonWith f a :== Heap 1 f (Node 0 a Nil)

//insert :: a (Heap a) -> (Heap a) | Ord a
insert :== insertWith (<=)

insertWith :: (a a -> Bool) a (Heap a) -> Heap a

union :: (Heap a) (Heap a) -> Heap a

replicate :: a Int -> Heap a | Ord a

uncons :: (Heap a) -> Maybe (a, Heap a) | Ord a

//viewMin :: (Heap a) -> Maybe (a, Heap a) | Ord a
viewMin :== uncons

minimum :: (Heap a) -> a

trees :: (Forest a) -> [Tree a]

deleteMin :: (Heap a) -> Heap a

map :: (a -> b) (Heap a) -> Heap b | Ord b

mapMonotonic :: (a -> b) (Heap a) -> Heap b | Ord b

filter :: (a -> Bool) (Heap a) -> Heap a

partition :: (a -> Bool) (Heap a) -> (Heap a, Heap a)

split :: a (Heap a) -> (Heap a, Heap a, Heap a)

take :== withList o 'L'.take

drop :== withList o 'L'.drop

splitAt :== splitWithList o 'L'.splitAt

break :== splitWithList o 'L'.break

span :== splitWithList o 'L'.span

takeWhile :== withList o 'L'.takeWhile

dropWhile :== withList o 'L'.dropWhile

nub :: (Heap a) -> Heap a

concatMap :: (a -> Heap b) (Heap a) -> Heap b | Ord b

group :: (Heap a) -> Heap (Heap a)

groupBy :: (a a -> Bool) (Heap a) -> Heap (Heap a)

intersect :: (Heap a) (Heap a) -> Heap a

intersectWith :: (a a -> b) (Heap a) (Heap a) -> Heap b | Ord b

withList :: ([a] -> [a]) (Heap a) -> Heap a

splitWithList :: ([a] -> ([a], [a])) (Heap a) -> (Heap a, Heap a)