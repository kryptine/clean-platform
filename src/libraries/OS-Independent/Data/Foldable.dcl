definition module Data.Foldable

from Control.Applicative import :: Const
from Data.Either import :: Either
from Data.Monoid import class Monoid (..)
from Data.Maybe import :: Maybe

/**
 * Ported from Haskell's Data.Foldable by Jurriën Stutterheim 15-08-2014
 */

// Data structures that can be folded.
//
// Minimal complete definition: 'foldMap' or 'foldr'.
//
// For example, given a data type
//
// > data Tree a = Empty | Leaf a | Node (Tree a) a (Tree a)
//
// a suitable instance would be
//
// > instance Foldable Tree where
// >    foldMap f Empty = mempty
// >    foldMap f (Leaf x) = f x
// >    foldMap f (Node l k r) = foldMap f l `mappend` f k `mappend` foldMap f r
//
// This is suitable even for abstract types, as the monoid is assumed
// to satisfy the monoid laws.  Alternatively, one could define @foldr@:
//
// > instance Foldable Tree where
// >    foldr f z Empty = z
// >    foldr f z (Leaf x) = f x z
// >    foldr f z (Node l k r) = foldr f (f k (foldr f z r)) l
//
class Foldable t where
    // Combine the elements of a structure using a monoid.
    fold :: (t m) -> m | Monoid m

    // Map each element of the structure to a monoid,
    // and combine the results.
    foldMap :: (a -> m) (t a) -> m | Monoid m

    // Right-associative fold of a structure.
    //
    // @'foldr' f z = 'Prelude.foldr' f z . 'toList'@
    foldr :: (a b -> b) b (t a) -> b

    // Right-associative fold of a structure, 
    // but with strict application of the operator.
    foldr` :: (a b -> b) b (t a) -> b

    // Left-associative fold of a structure.
    //
    // @'foldl' f z = 'Prelude.foldl' f z . 'toList'@
    foldl :: (b a -> b) b (t a) -> b

    // Left-associative fold of a structure.
    // but with strict application of the operator.
    //
    // @'foldl' f z = 'List.foldl'' f z . 'toList'@
    foldl` :: (b a -> b) b (t a) -> b

    // A variant of 'foldr' that has no base case,
    // and thus may only be applied to non-empty structures.
    //
    // @'foldr1' f = 'Prelude.foldr1' f . 'toList'@
    foldr1 :: (a a -> a) (t a) -> a

    // A variant of 'foldl' that has no base case,
    // and thus may only be applied to non-empty structures.
    //
    // @'foldl1' f = 'Prelude.foldl1' f . 'toList'@
    foldl1 :: (a a -> a) (t a) -> a

instance Foldable Maybe
instance Foldable []
instance Foldable (Either a)
instance Foldable ((,) a)

// TODO Cleanify
//instance Ix i => Foldable (Array i)

instance Foldable (Const m)
