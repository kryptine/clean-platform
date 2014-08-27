implementation module Data.Traversable

/**
 * Ported from Haskell's Data.Traversable by Jurriën Stutterheim 15-08-2014
 */


import Control.Applicative
import Data.Either
//from Data.Foldable import class Foldable
import Data.Foldable
import Data.Functor
from Data.List import instance Functor []
from Data.Either import instance Functor (Either a)
from Control.Monad import class Monad
import qualified Control.Monad as CM
from Data.Monoid import class Monoid
import Data.Maybe
from StdFunc import o, id, flip

//instance Traversable t where
    //traverse f = sequenceA o fmap f

    //sequenceA = traverse id

    //mapM f = unwrapMonad o traverse (WrapMonad o f)

    //sequence = mapM id

instance Traversable Maybe where
    traverse _ Nothing = pure Nothing
    traverse f (Just x) = Just <$> f x
    sequenceA f = traverse id f
    mapM f x = unwrapMonad (traverse (WrapMonad o f) x)
    sequence x = mapM id x

instance Traversable [] where
    traverse f x = foldr cons_f (pure []) x
      where cons_f x ys = (\x xs -> [x:xs]) <$> f x <*> ys
    mapM f x = 'CM'.mapM f x
    sequenceA f = traverse id f
    sequence x = mapM id x

instance Traversable (Either a) where
    traverse _ (Left x) = pure (Left x)
    traverse f (Right y) = Right <$> f y
    sequenceA f = traverse id f
    mapM f x = unwrapMonad (traverse (WrapMonad o f) x)
    sequence x = mapM id x

instance Traversable ((,) a) where
    traverse f (x, y) = (\x y -> (x, y)) x <$> f y
    sequenceA f = traverse id f
    mapM f x = unwrapMonad (traverse (WrapMonad o f) x)
    sequence x = mapM id x

// TODO Cleanify
//instance Ix i => Traversable (Array i) where
    //traverse f arr = listArray (bounds arr) `fmap` traverse f (elems arr)

// general functions

// 'for' is 'traverse' with its arguments flipped.
for :: (t a) (a -> f b) -> f (t b) | Traversable t & Applicative f
for x f = flip traverse x f

// 'forM' is 'mapM' with its arguments flipped.
forM :: (t a) (a -> m b) -> m (t b) | Traversable t & Monad m
forM x f = flip mapM x f

/// left-to-right state transformer
:: StateL s a = StateL (s -> (s, a))

runStateL (StateL f) = f

instance Functor (StateL s) where
    fmap f (StateL k) = StateL (\s -> let (s`, v) = k s in (s`, f v))

instance Applicative (StateL s) where
    pure x = StateL (\ s -> (s, x))
    (<*>) (StateL kf) (StateL kv) = StateL (\s ->
        let (s`, f) = kf s
            (s``, v) = kv s`
        in (s``, f v))

// The 'mapAccumL' function behaves like a combination of 'fmap'
// and 'foldl'; it applies a function to each element of a structure,
// passing an accumulating parameter from left to right, and returning
// a final value of this accumulator together with the new structure.
mapAccumL :: (a b -> (a, c)) a (t b) -> (a, t c) | Traversable t
mapAccumL f s t = runStateL (traverse (StateL o flip f) t) s

// right-to-left state transformer
:: StateR s a = StateR (s -> (s, a))

runStateR (StateR f) = f

instance Functor (StateR s) where
    fmap f (StateR k) = StateR (\s -> let (s`, v) = k s in (s`, f v))

instance Applicative (StateR s) where
    pure x = StateR (\ s -> (s, x))
    (<*>) (StateR kf) (StateR kv) = StateR (\s ->
        let (s`, v) = kv s
            (s``, f) = kf s`
        in (s``, f v))

// The 'mapAccumR' function behaves like a combination of 'fmap'
// and 'foldr'; it applies a function to each element of a structure,
// passing an accumulating parameter from right to left, and returning
// a final value of this accumulator together with the new structure.
mapAccumR :: (a b -> (a, c)) a (t b) -> (a, t c) | Traversable t
mapAccumR f s t = runStateR (traverse (StateR o flip f) t) s

// This function may be used as a value for `fmap` in a `Functor`
// instance, provided that 'traverse' is defined. (Using
// `fmapDefault` with a `Traversable` instance defined only by
// 'sequenceA' will result in infinite recursion.)
fmapDefault :: (a -> b) (t a) -> t b | Traversable t
fmapDefault f x = getId (traverse (Id o f) x)

// This function may be used as a value for `Data.Foldable.foldMap`
// in a `Foldable` instance.
foldMapDefault :: (a -> m) (t a) -> m | Traversable t & Monoid m
foldMapDefault f x = getConst (traverse (Const o f) x)

// local instances

:: Id a = Id a

getId (Id x) =x

instance Functor Id where
    fmap f (Id x) = Id (f x)

instance Applicative Id where
    pure x = Id x
    (<*>) (Id f) (Id x) = Id (f x)