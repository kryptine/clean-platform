implementation module Data.Foldable

from StdFunc import o, id, flip
from StdMisc import abort
import Control.Applicative
from Control.Monad import class Monad(..), >>=
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

foldrM :: (a b -> m b) b (t a) -> m b | Foldable t & Monad m
foldrM f z0 xs = foldl f` pure xs z0
  where f` k x z = f x z >>= k

foldlM :: (b a -> m b) b (t a) -> m b | Foldable t & Monad m
foldlM f z0 xs = foldr f` pure xs z0
  where f` x k z = f z x >>= k

traverse_ :: (a -> f b) (t a) -> f () | Foldable t & Applicative, *> f
traverse_ f x = foldr ((*>) o f) (pure ()) x

mapM_ :: (a -> m b) (t a) -> m () | Foldable t & Monad m
mapM_ f x = foldr ((\ma mb -> ma >>= \_ -> mb) o f) (pure ()) x

sequenceA_ :: (t (f a)) -> f () | Foldable t & Applicative, *> f
sequenceA_ x = foldr (*>) (pure ()) x

concat :: (t [a]) -> [a] | Foldable t
concat x = fold x

concatMap :: (a -> [b]) (t a) -> [b] | Foldable t
concatMap f x = foldMap f x

and :: (t Bool) -> Bool | Foldable t
and x = getAll (foldMap All x)

or :: (t Bool) -> Bool | Foldable t
or x = getAny (foldMap Any x)

any :: (a -> Bool) (t a) -> Bool | Foldable t
any p x = getAny (foldMap (Any o p) x)

all :: (a -> Bool) (t a) -> Bool | Foldable t
all p x = getAll (foldMap (All o p) x)

sum :: (t a) -> a | Foldable t & + a & zero a
sum x = getSum (foldMap Sum x)

product :: (t a) -> a | Foldable t & * a & one a
product x = getProduct (foldMap Product x)

maximum :: (t a) -> a | Foldable t & Ord a
maximum x = foldr1 max x

maximumBy :: (a a -> Bool) (t a) -> a | Foldable t
maximumBy cmp x = foldr1 max` x
  where max` x y = if (cmp x y) y x

minimum :: (t a) -> a | Foldable t & Ord a
minimum x = foldr1 min x

minimumBy :: (a a -> Bool) (t a) -> a | Foldable t
minimumBy cmp x = foldr1 min` x
  where min` x y = if (cmp x y) x y

elem :: a (t a) -> Bool | Foldable t & == a
elem x y = any (\z -> x == z) y

notElem ::  a (t a) -> Bool | Foldable t & == a
notElem x y = not (elem x y)

find :: (a -> Bool) (t a) -> Maybe a | Foldable t
find p x = listToMaybe (concatMap (\x -> if (p x) [x] []) x)
