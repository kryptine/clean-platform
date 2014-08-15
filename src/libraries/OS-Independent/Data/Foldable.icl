implementation module Data.Foldable

from StdFunc import o, id, flip
from StdMisc import abort
import Control.Applicative
import Control.Monad
import qualified Data.List as DL
import Data.Either
import Data.Monoid
import Data.Maybe
import qualified StdList as SL
import StdClass
from StdOverloaded import class < (..)
from StdBool import not

instance Foldable Maybe where
    fold x = foldMap id x
    foldMap f x = foldr (mappend o f) mempty x
    foldr _ z Nothing = z
    foldr f z (Just x) = f x z
    foldr` f z0 xs = foldl f` id xs z0
      where f` k x z = k (f x z)

    foldl _ z Nothing = z
    foldl f z (Just x) = f z x
    foldl` f z0 xs = foldr f` id xs z0
      where f` x k z = k (f z x)
    foldr1 f xs = fromMaybe (abort "foldr1: empty structure")
                    (foldr mf Nothing xs)
      where
        mf x Nothing = Just x
        mf x (Just y) = Just (f x y)
    foldl1 f xs = fromMaybe (abort "foldl1: empty structure")
                    (foldl mf Nothing xs)
      where
        mf Nothing y = Just y
        mf (Just x) y = Just (f x y)

instance Foldable [] where
    fold x = foldMap id x
    foldMap f x = foldr (mappend o f) mempty x
    foldr f x y = 'SL'.foldr f x y
    foldr` f z0 xs = foldl f` id xs z0
      where f` k x z = k (f x z)
    foldl f x y = 'SL'.foldl f x y
    foldl` f x y = 'DL'.foldl f x y
    foldr1 f x = 'DL'.foldr1 f x
    foldl1 f x = 'DL'.foldl1 f x

instance Foldable (Either a) where
    foldMap _ (Left _) = mempty
    foldMap f (Right y) = f y
    fold x = foldMap id x

    foldr _ z (Left _) = z
    foldr f z (Right y) = f y z
    foldr` f z0 xs = foldl f` id xs z0
      where f` k x z = k (f x z)
    foldl f z t = appEndo (getDual (foldMap (Dual o Endo o flip f) t)) z
    foldl` f z0 xs = foldr f` id xs z0
      where f` x k z = k (f z x)
    foldr1 f xs = fromMaybe (abort "foldr1: empty structure")
                    (foldr mf Nothing xs)
      where
        mf x Nothing = Just x
        mf x (Just y) = Just (f x y)
    foldl1 f xs = fromMaybe (abort "foldl1: empty structure")
                    (foldl mf Nothing xs)
      where
        mf Nothing y = Just y
        mf (Just x) y = Just (f x y)

instance Foldable ((,) a) where
    foldMap f (_, y) = f y
    fold x = foldMap id x

    foldr f z (_, y) = f y z
    foldr` f z0 xs = foldl f` id xs z0
      where f` k x z = k (f x z)
    foldl f z t = appEndo (getDual (foldMap (Dual o Endo o flip f) t)) z
    foldl` f z0 xs = foldr f` id xs z0
      where f` x k z = k (f z x)
    foldr1 f xs = fromMaybe (abort "foldr1: empty structure")
                    (foldr mf Nothing xs)
      where
        mf x Nothing = Just x
        mf x (Just y) = Just (f x y)
    foldl1 f xs = fromMaybe (abort "foldl1: empty structure")
                    (foldl mf Nothing xs)
      where
        mf Nothing y = Just y
        mf (Just x) y = Just (f x y)

// TODO Cleanify
//instance Ix i => Foldable (Array i) where
    //foldr f z = Prelude.foldr f z o elems
    //foldl f z = Prelude.foldl f z o elems
    //foldr1 f = Prelude.foldr1 f o elems
    //foldl1 f = Prelude.foldl1 f o elems

instance Foldable (Const m) where
    foldMap _ _ = mempty
    fold x = foldMap id x
    foldr f z t = appEndo (foldMap (Endo o f) t) z
    foldr` f z0 xs = foldl f` id xs z0
      where f` k x z = k (f x z)
    foldl f z t = appEndo (getDual (foldMap (Dual o Endo o flip f) t)) z
    foldl` f z0 xs = foldr f` id xs z0
      where f` x k z = k (f z x)
    foldr1 f xs = fromMaybe (abort "foldr1: empty structure")
                    (foldr mf Nothing xs)
      where
        mf x Nothing = Just x
        mf x (Just y) = Just (f x y)
    foldl1 f xs = fromMaybe (abort "foldl1: empty structure")
                    (foldl mf Nothing xs)
      where
        mf Nothing y = Just y
        mf (Just x) y = Just (f x y)

// Monadic fold over the elements of a structure,
// associating to the right, i.e. from right to left.
foldrM :: (a b -> m b) b (t a) -> m b | Foldable t & Monad m
foldrM f z0 xs = foldl f` pure xs z0
  where f` k x z = f x z >>= k

// Monadic fold over the elements of a structure,
// associating to the left, i.e. from left to right.
foldlM :: (b a -> m b) b (t a) -> m b | Foldable t & Monad m
foldlM f z0 xs = foldr f` pure xs z0
  where f` x k z = f z x >>= k

// Map each element of a structure to an action, evaluate
// these actions from left to right, and ignore the results.
traverse_ :: (a -> f b) (t a) -> f () | Foldable t & Applicative f
traverse_ f x = foldr ((*>) o f) (pure ()) x

// 'for_' is 'traverse_' with its arguments flipped.
//for_ :: (t a) (a -> f b) -> f () | Foldable t, Applicative f
for_ x f :== flip traverse_ x f

// Map each element of a structure to a monadic action, evaluate
// these actions from left to right, and ignore the results.
mapM_ :: (a -> m b) (t a) -> m () | Foldable t & Monad m
mapM_ f x = foldr ((\ma mb -> ma >>= \_ -> mb) o f) (pure ()) x

// 'forM_' is 'mapM_' with its arguments flipped.
//forM_ :: (t a) (a -> m b) -> m () | Foldable t & Monad m
forM_ x f :== flip mapM_ x f

// Evaluate each action in the structure from left to right,
// and ignore the results.
sequenceA_ :: (t (f a)) -> f () | Foldable t & Applicative f
sequenceA_ x = foldr (*>) (pure ()) x

// Evaluate each monadic action in the structure from left to right,
// and ignore the results.
//sequence_ :: (t (m a)) -> m () | Foldable t & Monad m
sequence_ x :== foldr (\ma mb -> ma >>= \_ -> mb) (pure ()) x

// The sum of a collection of actions, generalizing 'concat'.
//asum :: (t (f a)) -> f a | Foldable t & Alternative f
asum x :== foldr (<|>) empty x

// The sum of a collection of actions, generalizing 'concat'.
//msum :: (t (m a)) -> m a | Foldable t & MonadPlus m
msum x :== foldr mplus mzero x

// These use foldr rather than foldMap to avoid repeated concatenation.

// List of elements of a structure.
//toList :: (t a) -> [a] | Foldable t
toList t :== build (\ c n -> foldr c n t)

//build :: (A.: (a b -> b) b -> b) -> [a]
build g :== g (\x xs -> [x:xs]) []


// The concatenation of all the elements of a container of lists.
concat :: (t [a]) -> [a] | Foldable t
concat x = fold x

// Map a function over all the elements of a container and concatenate
// the resulting lists.
concatMap :: (a -> [b]) (t a) -> [b] | Foldable t
concatMap f x = foldMap f x

// 'and' returns the conjunction of a container of Bools.  For the
// result to be 'True', the container must be finite; 'False', however,
// results from a 'False' value finitely far from the left end.
and :: (t Bool) -> Bool | Foldable t
and x = getAll (foldMap All x)

// 'or' returns the disjunction of a container of Bools.  For the
// result to be 'False', the container must be finite; 'True', however,
// results from a 'True' value finitely far from the left end.
or :: (t Bool) -> Bool | Foldable t
or x = getAny (foldMap Any x)

// Determines whether any element of the structure satisfies the predicate.
any :: (a -> Bool) (t a) -> Bool | Foldable t
any p x = getAny (foldMap (Any o p) x)

// Determines whether all elements of the structure satisfy the predicate.
all :: (a -> Bool) (t a) -> Bool | Foldable t
all p x = getAll (foldMap (All o p) x)

// The 'sum' function computes the sum of the numbers of a structure.
sum :: (t a) -> a | Foldable t & + a & zero a
sum x = getSum (foldMap Sum x)

// The 'product' function computes the product of the numbers of a structure.
product :: (t a) -> a | Foldable t & * a & one a
product x = getProduct (foldMap Product x)

// The largest element of a non-empty structure.
maximum :: (t a) -> a | Foldable t & Ord a
maximum x = foldr1 max x

// The largest element of a non-empty structure with respect to the
// given greater-than function.
maximumBy :: (a a -> Bool) (t a) -> a | Foldable t
maximumBy cmp x = foldr1 max` x
  where max` x y = if (cmp x y) x y

// The least element of a non-empty structure.
minimum :: (t a) -> a | Foldable t & Ord a
minimum x = foldr1 min x

// The least element of a non-empty structure with respect to the
// given lesser-than function.
minimumBy :: (a a -> Bool) (t a) -> a | Foldable t
minimumBy cmp x = foldr1 min` x
  where min` x y = if (cmp x y) x y

// Does the element occur in the structure?
elem :: a (t a) -> Bool | Foldable t & == a
elem x y = any (\z -> x == z) y

// 'notElem' is the negation of 'elem'.
notElem ::  a (t a) -> Bool | Foldable t & == a
notElem x y = not (elem x y)

// The 'find' function takes a predicate and a structure and returns
// the leftmost element of the structure matching the predicate, or
// 'Nothing' if there is no such element.
find :: (a -> Bool) (t a) -> Maybe a | Foldable t
find p x = listToMaybe (concatMap (\x -> if (p x) [x] []) x)
