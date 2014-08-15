implementation module Data.Foldable

from StdFunc import o, id, flip
from StdMisc import abort
import Control.Applicative
import qualified Data.List as DL
import Data.Either
import Data.Monoid
import Data.Maybe
import qualified StdList as SL

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
