// (c) 2002 by Martin Erwig [see file COPYRIGHT]
// | Monadic Graphs
implementation module Data.Graph.Inductive.Monad

import Data.Graph.Inductive.Graph
from Control.Applicative import class Applicative (..)
import Control.Monad
import Data.Functor
from StdFunc import o, id
import StdList, StdTuple, StdOrdList, StdMisc, StdEnum
import Data.Maybe

//--------------------------------------------------------------------
// MONADIC GRAPH CLASS
//--------------------------------------------------------------------

//
// Currently, we define just one monadic graph class:
//
//   GraphM:    static, decomposable graphs
//              static means that a graph itself cannot be changed
//
// Later we might also define DynGraphM for dynamic, extensible graphs
//



// Monadic Graph
//
class GraphM m gr | Monad m where
  emptyM     :: m (gr a b)

  isEmptyM   :: (m (gr a b)) -> m Bool

  matchM     :: Node (m (gr a b)) -> m (Decomp gr a b)

  mkGraphM   :: [LNode a] [LEdge b] -> m (gr a b)

  labNodesM  :: (m (gr a b)) -> m [LNode a]

  matchAnyM  :: (m (gr a b)) -> m (GDecomp gr a b)

  noNodesM   :: (m (gr a b)) -> m Int

  nodeRangeM :: (m (gr a b)) -> m (Node,Node)

  labEdgesM  :: m (gr a b) -> m [LEdge b]

defLabEdgesM  :: (m (gr a b)) -> m [LEdge b] | GraphM m gr
defLabEdgesM m = ufoldM (\(p,v,_,s)->(\xs -> (map (fi v) p ++ map (fo v) s)++xs)) [] m
    where
      fo v = \(l,w)->(v,w,l)
      fi v = \(l,w)->(w,v,l)

defMatchAnyM  :: (m (gr a b)) -> m (GDecomp gr a b) | GraphM m gr
defMatchAnyM g = labNodesM g >>= \vs ->
                   case vs of
                     []        -> abort "Match Exception, Empty Graph"
                     [(v,_):_] -> matchM v g >>= \(Just c,g`) -> pure (c,g`)

defNoNodesM   :: (m (gr a b)) -> m Int | GraphM m gr
defNoNodesM m = (labNodesM >>. length) m

defNodeRangeM :: (m (gr a b)) -> m (Node,Node) | GraphM m gr
defNodeRangeM g = isEmptyM g >>= \isE ->
                    if isE
                       (abort "nodeRangeM of empty graph")
                       (nodesM g >>= \vs -> pure (minList vs,maxList vs))

// composing a monadic function with a non-monadic one
//
(>>.) :: ((m a) -> m b) (b -> c) (m a) -> m c | Monad m
(>>.) f g m = ((\x -> x >>= pure o g) o f) m


//--------------------------------------------------------------------
// DERIVED GRAPH OPERATIONS
//--------------------------------------------------------------------

// graph folds and maps
//

// | graph fold
ufoldM :: ((Context a b) c -> c) c (m (gr a b)) -> m c | GraphM m gr
ufoldM f u g = isEmptyM g >>= \b ->
                  if b (pure u)
                       (matchAnyM g >>= \(c, g`) -> ufoldM f u (pure g`) >>= \x -> pure (f c x))


// (additional) graph projection
// [noNodes, nodeRange, labNodes, labEdges are defined in class Graph]
//
nodesM :: (m (gr a b)) -> m [Node] | GraphM m gr
nodesM m = (labNodesM >>. map fst) m

edgesM :: (m (gr a b)) -> m [Edge] | GraphM m gr
edgesM m = (labEdgesM >>. map (\(v,w,_)->(v,w))) m

newNodesM :: Int (m (gr a b)) -> m [Node] | GraphM m gr
newNodesM i g = isEmptyM g >>= \isE ->
                   if isE
                      (pure [0..i-1])
                      (nodeRangeM g >>= \(_, n) -> pure [n+1..n+i])


// graph construction & destruction
//
delNodeM :: Node (m (gr a b)) -> m (gr a b) | GraphM m gr
delNodeM v m = delNodesM [v] m

delNodesM :: [Node] (m (gr a b)) -> m (gr a b) | GraphM m gr
delNodesM []     g = g
delNodesM [v:vs] g = matchM v g >>= \(_, g`) -> delNodesM vs (pure g`)

mkUGraphM :: [Node] [Edge] -> m (gr () ()) | GraphM m gr
mkUGraphM vs es = mkGraphM (labUNodes vs) (labUEdges es)

labUEdges :: [Edge] -> [LEdge ()]
labUEdges es = map (\x -> toLEdge x ()) es

labUNodes :: [Node] -> [LNode ()]
labUNodes ns = map (\v->(v,())) ns


// graph inspection (for a particular node)
//
onMatch :: ((Context a b) -> c) c (m (gr a b)) Node -> m c | GraphM m gr
onMatch f u g v = matchM v g >>= \(x, _) ->
                     pure (case x of
                             Nothing -> u
                             Just c -> f c)

contextM :: (m (gr a b)) Node -> m (Context a b) | GraphM m gr
contextM g v = onMatch id (abort ("Match Exception, Node: "++toString v)) g v

labM :: (m (gr a b)) Node -> m (Maybe a) | GraphM m gr
labM m n = onMatch (Just o lab`) Nothing m n

/*
neighbors :: (GraphM m gr) => m (gr a b) -> Node -> [Node] | GraphM m gr
neighbors = (\(p,_,_,s) -> map snd (p++s)) .: context

suc :: (GraphM m gr) => m (gr a b) -> Node -> [Node] | GraphM m gr
suc = map snd .: context4

pre :: (GraphM m gr) => m (gr a b) -> Node -> [Node] | GraphM m gr
pre = map snd .: context1

lsuc :: (GraphM m gr) => m (gr a b) -> Node -> [(Node,b)] | GraphM m gr
lsuc = map flip2 .: context4

lpre :: (GraphM m gr) => m (gr a b) -> Node -> [(Node,b)] | GraphM m gr
lpre = map flip2 .: context1

out :: (GraphM m gr) => m (gr a b) -> Node -> [LEdge b] | GraphM m gr
out g v = map (\(l,w)->(v,w,l)) (context4 g v)

inn :: (GraphM m gr) => m (gr a b) -> Node -> [LEdge b] | GraphM m gr
inn g v = map (\(l,w)->(w,v,l)) (context1 g v)

outdeg :: (GraphM m gr) => m (gr a b) -> Node -> Int | GraphM m gr
outdeg = length .: context4

indeg :: (GraphM m gr) => m (gr a b) -> Node -> Int | GraphM m gr
indeg  = length .: context1

deg :: (GraphM m gr) => m (gr a b) -> Node -> Int | GraphM m gr
deg = (\(p,_,_,s) -> length p+length s) .: context
//

// -- context inspection
// --
// node` :: Context a b -> Node
// node` (_,v,_,_) = v
//
// lab` :: Context a b -> a
// lab` (_,_,l,_) = l
//
// labNode` :: Context a b -> LNode a
// labNode` (_,v,l,_) = (v,l)
//
// neighbors` :: Context a b -> [Node]
// neighbors` (p,_,_,s) = map snd p++map snd s
//
// suc` :: Context a b -> [Node]
// suc` (_,_,_,s) = map snd s
//
// pre` :: Context a b -> [Node]
// pre` (p,_,_,_) = map snd p
//
// lpre` :: Context a b -> [(Node,b)]
// lpre` (p,_,_,_) = map flip2 p
//
// lsuc` :: Context a b -> [(Node,b)]
// lsuc` (_,_,_,s) = map flip2 s
//
// out` :: Context a b -> [LEdge b]
// out` (_,v,_,s) = map (\(l,w)->(v,w,l)) s
//
// inn` :: Context a b -> [LEdge b]
// inn` (p,v,_,_) = map (\(l,w)->(w,v,l)) p
//
// outdeg` :: Context a b -> Int
// outdeg` (_,_,_,s) = length s
//
// indeg` :: Context a b -> Int
// indeg` (p,_,_,_) = length p
//
// deg` :: Context a b -> Int
// deg` (p,_,_,s) = length p+length s


// graph equality
//
nodeComp :: (Eq b) => LNode b -> LNode b -> Ordering
nodeComp n@(v,a) n`@(w,b) | n == n`   = EQ
                          | v<w       = LT
                          | otherwise = GT

slabNodes :: (Eq a,Graph gr) => m (gr a b) -> [LNode a]
slabNodes = sortBy nodeComp . labNodes

edgeComp :: (Eq b) => LEdge b -> LEdge b -> Ordering
edgeComp e@(v,w,a) e`@(x,y,b) | e == e`              = EQ
                              | v<x || (v==x && w<y) = LT
                              | otherwise            = GT

slabEdges :: (Eq b,Graph gr) => m (gr a b) -> [LEdge b]
slabEdges = sortBy edgeComp . labEdges

instance (Eq a,Eq b,Graph gr) => Eq (m (gr a b)) where
  g == g` = slabNodes g == slabNodes g` && slabEdges g == slabEdges g`

*/
