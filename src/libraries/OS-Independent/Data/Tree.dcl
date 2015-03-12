definition module Data.Tree

// Ported from Haskell's Data.Tree by Jurriën Stutterheim
from Data.Functor import class Functor
from Control.Applicative import class Applicative
from Control.Monad import class Monad

// | Multi-way trees, also known as /rose trees/.
:: Tree a
  = Node
    a // label value
    (Forest a) // zero or more child trees

rootLabel :: (Tree a) -> a

subForest :: (Tree a) -> Forest a

:: Forest a :== [Tree a]

instance Functor Tree

fmapTree :: (a -> b) (Tree a) -> Tree b

instance Applicative Tree

instance Monad Tree

unlines :: [String] -> String

// | Neat 2-dimensional drawing of a tree.
drawTree :: (Tree String) -> String

// | Neat 2-dimensional drawing of a forest.
drawForest :: (Forest String) -> String

draw :: (Tree String) -> [String]

// | The elements of a tree in pre-order.

// | Lists of nodes at each level of the tree.
levels :: (Tree a) -> [[a]]

// | Build a tree from a seed value
unfoldTree :: (b -> (a, [b])) b -> Tree a

// | Build a forest from a list of seed values
unfoldForest :: (b -> (a, [b])) [b] -> Forest a

// | Monadic tree builder, in depth-first order
unfoldTreeM :: (b -> m (a, [b])) b -> m (Tree a) | Monad m

// | Monadic forest builder, in depth-first order
unfoldForestM :: (b -> m (a, [b])) [b] -> m (Forest a) | Monad m

