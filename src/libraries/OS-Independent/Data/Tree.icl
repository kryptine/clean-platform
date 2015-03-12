implementation module Data.Tree

// Ported from Haskell's Data.Tree by Jurriën Stutterheim
from Data.Functor import class Functor (..), <$>
from Control.Applicative import class Applicative (..)
from Control.Monad import class Monad (..), liftM, `b`, mapM
from StdList import map, ++
from StdOverloaded import class +++ (..)
from StdFunc import o
from Data.List import zipWith, iterate, foldr, repeat, concatMap, takeWhile, isEmpty
from StdBool import not
import StdString

rootLabel :: (Tree a) -> a
rootLabel (Node x _) = x

subForest :: (Tree a) -> Forest a
subForest (Node _ xs) = xs

instance Functor Tree where
  fmap f t = fmapTree f t

fmapTree :: (a -> b) (Tree a) -> Tree b
fmapTree f (Node x ts) = Node (f x) (map (fmapTree f) ts)

instance Applicative Tree where
  pure x = Node x []
  (<*>) (Node f tfs) tx=:(Node x txs) =
      Node (f x) (map (\x -> f <$> x) txs ++ map (\x -> x <*> tx) tfs)

instance Monad Tree where
  bind (Node x ts) f
    # (Node x` ts`) = f x
    = Node x` (ts` ++ map (\x -> bind x f) ts)

//instance Traversable Tree where
  //traverse f (Node x ts) = Node <$> f x <*> traverse (traverse f) ts

//instance Foldable Tree where
    //foldMap f (Node x ts) = mappend (f x) (foldMap (foldMap f) ts)
unlines :: [String] -> String
unlines xs = foldr (\x acc -> x +++ "\n" +++ acc) "" xs

// | Neat 2-dimensional drawing of a tree.
drawTree :: (Tree String) -> String
drawTree x = unlines (draw x)

// | Neat 2-dimensional drawing of a forest.
drawForest :: (Forest String) -> String
drawForest x = unlines (map drawTree x)

draw :: (Tree String) -> [String]
draw (Node x ts0) = [x : drawSubTrees ts0]
  where
  drawSubTrees [] = []
  drawSubTrees [t] =
      ["|" : shift "`- " "   " (draw t)]
  drawSubTrees [t:ts] =
      ["|" : shift "+- " "|  " (draw t) ++ drawSubTrees ts]

  shift first other xs = zipWith (+++) [first : repeat other] xs

// | The elements of a tree in pre-order.
flatten :: (Tree a) -> [a]
flatten t = squish t []
  where
  squish (Node x ts) xs = [x : foldr squish xs ts]

// | Lists of nodes at each level of the tree.
levels :: (Tree a) -> [[a]]
levels t =
    map (map rootLabel) (
        takeWhile (not o isEmpty) (
        iterate (concatMap subForest) [t]))

// | Build a tree from a seed value
unfoldTree :: (b -> (a, [b])) b -> Tree a
unfoldTree f b
  # (a, bs) = f b
  = Node a (unfoldForest f bs)

// | Build a forest from a list of seed values
unfoldForest :: (b -> (a, [b])) [b] -> Forest a
unfoldForest f xs = map (unfoldTree f) xs

// | Monadic tree builder, in depth-first order
unfoldTreeM :: (b -> m (a, [b])) b -> m (Tree a) | Monad m
unfoldTreeM f b =
                  f b
  `b` \(a, bs) -> unfoldForestM f bs
  `b` \ts      -> pure (Node a ts)

// | Monadic forest builder, in depth-first order
unfoldForestM :: (b -> m (a, [b])) [b] -> m (Forest a) | Monad m
unfoldForestM f xs = mapM (unfoldTreeM f) xs
