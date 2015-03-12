definition module Data.Tree

// Ported from Haskell's Data.RTree by Jurriën Stutterheim
from Data.Functor import class Functor
from Control.Applicative import class Applicative
from Control.Monad import class Monad
from Data.Monoid import class Monoid

// | Multi-way trees, also known as /rose trees/.
:: RTree a
  = RNode
    a // label value
    (RForest a) // zero or more child trees

rootLabel :: (RTree a) -> a

subRForest :: (RTree a) -> RForest a

:: RForest a :== [RTree a]

instance Functor RTree

fmapRTree :: (a -> b) (RTree a) -> RTree b

instance Applicative RTree

instance Monad RTree

instance Monoid (RForest a) | == a

mergeRForests :: (RForest a) (RForest a) -> RForest a | == a

unlines :: [String] -> String

// | Neat 2-dimensional drawing of a tree.
drawRTree :: (RTree String) -> String

// | Neat 2-dimensional drawing of a forest.
drawRForest :: (RForest String) -> String

draw :: (RTree String) -> [String]

// | The elements of a tree in pre-order.

// | Lists of nodes at each level of the tree.
levels :: (RTree a) -> [[a]]

// | Build a tree from a seed value
unfoldRTree :: (b -> (a, [b])) b -> RTree a

// | Build a forest from a list of seed values
unfoldRForest :: (b -> (a, [b])) [b] -> RForest a

// | Monadic tree builder, in depth-first order
unfoldRTreeM :: (b -> m (a, [b])) b -> m (RTree a) | Monad m

// | Monadic forest builder, in depth-first order
unfoldRForestM :: (b -> m (a, [b])) [b] -> m (RForest a) | Monad m

