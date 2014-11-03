implementation module Data.Set

import StdClass, StdMisc, StdBool, StdList, StdFunc, StdInt, StdTuple
import Data.Maybe

:: Ordering = LT | GT | EQ

mapSet :: (a -> b) (Set a) -> Set b | < a & == a & < b & == b
mapSet f s = fromList (map f (toList s))

mapSetMonotonic :: (a -> b) (Set a) -> Set b
mapSetMonotonic _ Tip = Tip
mapSetMonotonic f (Bin n x l r) = Bin n (f x) (mapSetMonotonic f l) (mapSetMonotonic f r)

compare :: !a !a -> Ordering | < a & == a
compare x y = if (x < y) LT (if (x > y) GT EQ)

/*
 * Sets are size balanced trees.
 * A set of values @a@.
 */
:: Set a = Tip
         | Bin !Int a !(Set a) !(Set a)

instance == (Set a) | == a where
  (==) t1 t2  = (size t1 == size t2) && (toAscList t1 == toAscList t2)

instance < (Set a) | < a where
  (<) s1 s2 = compare (toAscList s1) (toAscList s2)
    where
    compare []     [] = False
    compare []     _  = True
    compare [_:_]  [] = False
    compare [a:as] [b:bs]
      | a < b     = True
      | a > b     = False
      | otherwise = compare as bs

/*--------------------------------------------------------------------
 * Query
 *--------------------------------------------------------------------*/
  
// | /O(1)/. Is this the empty set?
null :: (Set a) -> Bool
null t
  = case t of
      Tip         -> True
      Bin _ _ _ _ -> False

// | /O(1)/. The number of elements in the set.
size :: (Set a) -> Int
size t
  = case t of
      Tip          -> 0
      Bin sz _ _ _ -> sz

// | /O(log n)/. Is the element in the set?
member :: !a (Set a) -> Bool | < a & == a
member x t
  = case t of
      Tip -> False
      Bin _ y l r
          -> case compare x y of
               LT -> member x l
               GT -> member x r
               EQ -> True    
               
// | /O(log n)/. Is the element not in the set?
notMember :: !a (Set a) -> Bool | < a & == a
notMember x t = not (member x t)

/*--------------------------------------------------------------------
 * Construction
 *--------------------------------------------------------------------*/
 
// | /O(1)/. The empty set.
empty :: Set a
empty
  = Tip

newSet :: Set a
newSet = empty

// | /O(1)/. Create a singleton set.
singleton :: u:a -> w:(Set u:a), [w <= u]
singleton x 
  = Bin 1 x Tip Tip

/*--------------------------------------------------------------------
 * Insertion, Deletion
 *--------------------------------------------------------------------*/

// | /O(log n)/. Insert an element in a set.
// If the set already contains an element equal to the given value,
// it is replaced with the new value.
insert :: !a .(Set a) -> Set a | < a & == a
insert x t
  = case t of
      Tip -> singleton x
      Bin sz y l r
          -> case compare x y of
               LT -> balance y (insert x l) r
               GT -> balance y l (insert x r)
               EQ -> Bin sz x l r
               
// | /O(log n)/. Delete an element from a set.
delete :: !a .(Set a) -> Set a | < a & == a
delete x t
  = case t of
      Tip -> Tip
      Bin _ y l r
          -> case compare x y of
               LT -> balance y (delete x l) r
               GT -> balance y l (delete x r)
               EQ -> glue l r
               
/*--------------------------------------------------------------------
 * Subset
 *--------------------------------------------------------------------*/

// | /O(n+m)/. Is this a proper subset? (ie. a subset but not equal).
isProperSubsetOf :: (Set a) (Set a) -> Bool | < a & == a
isProperSubsetOf s1 s2
    = (size s1 < size s2) && (isSubsetOf s1 s2)

// | /O(n+m)/. Is this a subset?
// @(s1 `isSubsetOf` s2)@ tells whether @s1@ is a subset of @s2@.
isSubsetOf :: (Set a) (Set a) -> Bool | < a & == a
isSubsetOf t1 t2
  = (size t1 <= size t2) && (isSubsetOfX t1 t2)

isSubsetOfX :: !(Set a) (Set a) -> Bool | < a & == a
isSubsetOfX Tip _ = True
isSubsetOfX _ Tip = False
isSubsetOfX (Bin _ x l r) t
  = found && isSubsetOfX l lt && isSubsetOfX r gt
where
    (lt, found, gt) = splitMember x t

/*--------------------------------------------------------------------
 * Minimal, Maximal
 *--------------------------------------------------------------------*/
 
// | /O(log n)/. The minimal element of a set.
findMin :: (Set a) -> a
findMin (Bin _ x Tip _) = x
findMin (Bin _ _ l _)   = findMin l
findMin Tip             = abort "Set.findMin: empty set has no minimal element"

// | /O(log n)/. The maximal element of a set.
findMax :: (Set a) -> a
findMax (Bin _ x _ Tip)  = x
findMax (Bin _ _ _ r)    = findMax r
findMax Tip              = abort "Set.findMax: empty set has no maximal element"

// | /O(log n)/. Delete the minimal element.
deleteMin :: .(Set a) -> Set a
deleteMin (Bin _ _ Tip r) = r
deleteMin (Bin _ x l r)   = balance x (deleteMin l) r
deleteMin Tip             = Tip

// | /O(log n)/. Delete the maximal element.
deleteMax :: .(Set a) -> Set a
deleteMax (Bin _ _ l Tip) = l
deleteMax (Bin _ x l r)   = balance x l (deleteMax r)
deleteMax Tip             = Tip

/*--------------------------------------------------------------------
 * Union. 
 *--------------------------------------------------------------------*/
 
// | The union of a list of sets: (@'unions' == 'foldl' 'union' 'empty'@).
unions :: u:[v:(Set a)] -> Set a | < a & == a, [u <= v]
unions ts
  = foldl union empty ts

// | /O(n+m)/. The union of two sets, preferring the first set when
// equal elements are encountered.
// The implementation uses the efficient /hedge-union/ algorithm.
// Hedge-union is more efficient on (bigset `union` smallset).
union :: u:(Set a) u:(Set a) -> Set a | < a & == a
union Tip t2  = t2
union t1 Tip  = t1
union t1 t2 = hedgeUnion (const LT) (const GT) t1 t2

hedgeUnion :: (a -> Ordering) (a -> Ordering) u:(Set a) u:(Set a) -> Set a | < a & == a
hedgeUnion _     _     t1 Tip
  = t1
hedgeUnion cmplo cmphi Tip (Bin _ x l r)
  = join x (filterGt cmplo l) (filterLt cmphi r)
hedgeUnion cmplo cmphi (Bin _ x l r) t2
  = join x (hedgeUnion cmplo cmpx l (trim cmplo cmpx t2)) 
           (hedgeUnion cmpx cmphi r (trim cmpx cmphi t2))
where
    cmpx y  = compare x y

/*--------------------------------------------------------------------
 * Difference
 *--------------------------------------------------------------------*/
 
// | /O(n+m)/. Difference of two sets. 
// The implementation uses an efficient /hedge/ algorithm comparable with /hedge-union/.
difference :: (Set a) (Set a) -> Set a | < a & == a
difference Tip _   = Tip
difference t1 Tip  = t1
difference t1 t2   = hedgeDiff (const LT) (const GT) t1 t2

hedgeDiff :: (a -> Ordering) (a -> Ordering) (Set a) (Set a) -> Set a | < a & == a
hedgeDiff _ _ Tip _
  = Tip
hedgeDiff cmplo cmphi (Bin _ x l r) Tip 
  = join x (filterGt cmplo l) (filterLt cmphi r)
hedgeDiff cmplo cmphi t (Bin _ x l r) 
  = merge (hedgeDiff cmplo cmpx (trim cmplo cmpx t) l) 
          (hedgeDiff cmpx cmphi (trim cmpx cmphi t) r)
where
    cmpx y = compare x y


/*--------------------------------------------------------------------
 * Intersection
 *--------------------------------------------------------------------*/

intersections :: [Set a] -> Set a | < a & == a
intersections [t] = t
intersections [t:ts]
  = foldl intersection t ts

// | /O(n+m)/. The intersection of two sets.
// Elements of the result come from the first set, so for example
//
// > import qualified Data.Set as S
// > data AB = A | B deriving Show
// > instance Ord AB where compare _ _ = EQ
// > instance Eq AB where _ == _ = True
// > main = print (S.singleton A `S.intersection` S.singleton B,
// >               S.singleton B `S.intersection` S.singleton A)
//
// prints @(fromList [A],fromList [B])@.
intersection :: (Set a) (Set a) -> Set a | < a & == a
intersection Tip _ = Tip
intersection _ Tip = Tip
intersection t1=:(Bin s1 x1 l1 r1) t2=:(Bin s2 x2 l2 r2) =
   if (s1 >= s2) then1 else1
where
	then1 = let (lt,found,gt) = splitLookup x2 t1
          		tl            = intersection lt l2
          		tr            = intersection gt r2
	        in case found of
		 		     (Just x) -> join x tl tr
				     Nothing -> merge tl tr

	else1 = let (lt,found,gt) = splitMember x1 t2
	            tl            = intersection l1 lt
	            tr            = intersection r1 gt
	        in (if found (join x1 tl tr) (merge tl tr))

/*--------------------------------------------------------------------
 * Filter and partition
 *--------------------------------------------------------------------*/

// | /O(n)/. Filter all elements that satisfy the predicate.
filter :: (a -> Bool) (Set a) -> Set a | < a & == a
filter _ Tip = Tip
filter p (Bin _ x l r)
  | p x       = join x (filter p l) (filter p r)
  | otherwise = merge (filter p l) (filter p r)

// | /O(n)/. Partition the set into two sets, one with all elements that satisfy
// the predicate and one with all elements that don't satisfy the predicate.
// See also 'split'.
partition :: (a -> Bool) (Set a) -> (Set a,Set a) | < a & == a
partition _ Tip = (Tip,Tip)
partition p (Bin _ x l r)
  | p x       = (join x l1 r1,merge l2 r2)
  | otherwise = (merge l1 r1,join x l2 r2)
  where
    (l1,l2) = partition p l
    (r1,r2) = partition p r

/*--------------------------------------------------------------------
 * Fold
 *--------------------------------------------------------------------*/

// | /O(n)/. Post-order fold.
fold :: (a -> .b -> .b) .b .(Set a) -> .b
fold _ z Tip           = z
fold f z (Bin _ x l r) = fold f (f x (fold f z r)) l

/*--------------------------------------------------------------------
 * Lists 
 *--------------------------------------------------------------------*/

// | /O(n)/. Convert the set to a list of elements.
toList :: (Set a) -> [a]
toList s
  = toAscList s

// | /O(n)/. Convert the set to an ascending list of elements.
toAscList :: (Set a) -> [a]
toAscList t   
  = fold (\a as -> [a:as]) [] t

// | /O(n*log n)/. Create a set from a list of elements.
fromList :: [a] -> Set a | < a & == a
fromList xs 
  = foldl ins empty xs
  where
    ins t x = insert x t

/*--------------------------------------------------------------------
  Utility functions that return sub-ranges of the original
  tree. Some functions take a comparison function as argument to
  allow comparisons against infinite values. A function [cmplo x]
  should be read as [compare lo x].

  [trim cmplo cmphi t]  A tree that is either empty or where [cmplo x == LT]
                        and [cmphi x == GT] for the value [x] of the root.
  [filterGt cmp t]      A tree where for all values [k]. [cmp k == LT]
  [filterLt cmp t]      A tree where for all values [k]. [cmp k == GT]

  [split k t]           Returns two trees [l] and [r] where all values
                        in [l] are <[k] and all keys in [r] are >[k].
  [splitMember k t]     Just like [split] but also returns whether [k]
                        was found in the tree.
--------------------------------------------------------------------*/

/*--------------------------------------------------------------------
  [trim lo hi t] trims away all subtrees that surely contain no
  values between the range [lo] to [hi]. The returned tree is either
  empty or the key of the root is between @lo@ and @hi@.
--------------------------------------------------------------------*/
trim :: (a -> Ordering) (a -> Ordering) (Set a) -> Set a
trim _     _     Tip = Tip
trim cmplo cmphi t=:(Bin _ x l r)
  = case cmplo x of
      LT -> case cmphi x of
              GT -> t
              _  -> trim cmplo cmphi l
      _  -> trim cmplo cmphi r

/*--------------------------------------------------------------------
 * [filterGt x t] filter all values >[x] from tree [t]
 * [filterLt x t] filter all values <[x] from tree [t]
 *--------------------------------------------------------------------*/
filterGt :: (a -> Ordering) (Set a) -> Set a
filterGt _ Tip = Tip
filterGt cmp (Bin _ x l r)
  = case cmp x of
      LT -> join x (filterGt cmp l) r
      GT -> filterGt cmp r
      EQ -> r
      
filterLt :: (a -> Ordering) (Set a) -> Set a
filterLt _ Tip = Tip
filterLt cmp (Bin _ x l r)
  = case cmp x of
      LT -> filterLt cmp l
      GT -> join x l (filterLt cmp r)
      EQ -> l

/*--------------------------------------------------------------------
 * Split
 *--------------------------------------------------------------------*/

// | /O(log n)/. The expression (@'split' x set@) is a pair @(set1,set2)@
// where @set1@ comprises the elements of @set@ less than @x@ and @set2@
// comprises the elements of @set@ greater than @x@.
split :: !a (Set a) -> (Set a,Set a) | < a & == a
split _ Tip = (Tip,Tip)
split x (Bin _ y l r)
  = case compare x y of
      LT -> let (lt,gt) = split x l in (lt,join y gt r)
      GT -> let (lt,gt) = split x r in (join y l lt,gt)
      EQ -> (l,r)

// | /O(log n)/. Performs a 'split' but also returns whether the pivot
// element was found in the original set.
splitMember :: !a (Set a) -> (Set a,Bool,Set a) | < a & == a
splitMember x t = let (l, m, r) = splitLookup x t in
     (l,maybe False (const True) m,r)

// | /O(log n)/. Performs a 'split' but also returns the pivot
// element that was found in the original set.
splitLookup :: !a (Set a) -> (Set a, Maybe a, Set a) | < a & == a
splitLookup _ Tip = (Tip, Nothing, Tip)
splitLookup x (Bin _ y l r)
   = case compare x y of
       LT -> let (lt,found,gt) = splitLookup x l in (lt,found,join y gt r)
       GT -> let (lt,found,gt) = splitLookup x r in (join y l lt,found,gt)
       EQ -> (l,Just y,r)

/*--------------------------------------------------------------------
  Utility functions that maintain the balance properties of the tree.
  All constructors assume that all values in [l] < [x] and all values
  in [r] > [x], and that [l] and [r] are valid trees.
  
  In order of sophistication:
    [Bin sz x l r]    The type constructor.
    [bin x l r]       Maintains the correct size, assumes that both [l]
                      and [r] are balanced with respect to each other.
    [balance x l r]   Restores the balance and size.
                      Assumes that the original tree was balanced and
                      that [l] or [r] has changed by at most one element.
    [join x l r]      Restores balance and size. 

  Furthermore, we can construct a new tree from two trees. Both operations
  assume that all values in [l] < all values in [r] and that [l] and [r]
  are valid:
    [glue l r]        Glues [l] and [r] together. Assumes that [l] and
                      [r] are already balanced with respect to each other.
    [merge l r]       Merges two trees and restores balance.

  Note: in contrast to Adam's paper, we use (<=) comparisons instead
  of (<) comparisons in [join], [merge] and [balance]. 
  Quickcheck (on [difference]) showed that this was necessary in order 
  to maintain the invariants. It is quite unsatisfactory that I haven't 
  been able to find out why this is actually the case! Fortunately, it 
  doesn't hurt to be a bit more conservative.
--------------------------------------------------------------------*/

/*--------------------------------------------------------------------
 * Join 
 *--------------------------------------------------------------------*/
join :: !a (Set a) (Set a) -> Set a
join x Tip r  = insertMin x r
join x l Tip  = insertMax x l
join x l=:(Bin sizeL y ly ry) r=:(Bin sizeR z lz rz)
  | delta*sizeL <= sizeR  = balance z (join x l lz) rz
  | delta*sizeR <= sizeL  = balance y ly (join x ry r)
  | otherwise             = bin x l r

// insertMin and insertMax don't perform potentially expensive comparisons.
insertMax :: !a (Set a) -> Set a 
insertMax x t
  = case t of
      Tip -> singleton x
      Bin _ y l r
          -> balance y l (insertMax x r)
            
insertMin :: !a (Set a) -> Set a 
insertMin x t
  = case t of
      Tip -> singleton x
      Bin _ y l r
          -> balance y (insertMin x l) r
         
/*--------------------------------------------------------------------
 * [merge l r]: merges two trees.
 *--------------------------------------------------------------------*/
merge :: (Set a) (Set a) -> Set a
merge Tip r   = r
merge l Tip   = l
merge l=:(Bin sizeL x lx rx) r=:(Bin sizeR y ly ry)
  | delta*sizeL <= sizeR = balance y (merge l ly) ry
  | delta*sizeR <= sizeL = balance x lx (merge rx r)
  | otherwise            = glue l r

/*--------------------------------------------------------------------
 * [glue l r]: glues two trees together.
 * Assumes that [l] and [r] are already balanced with respect to each other.
 *--------------------------------------------------------------------*/
glue :: .(Set a) .(Set a) -> Set a
glue Tip r = r
glue l Tip = l
glue l r
  | size l > size r = let (m, l`) = deleteFindMax l in balance m l` r
  | otherwise       = let (m, r`) = deleteFindMin r in balance m l r`

// | /O(log n)/. Delete and find the minimal element.
// 
// > deleteFindMin set = (findMin set, deleteMin set)
deleteFindMin :: .(Set a) -> (a, Set a)
deleteFindMin t
  = case t of
      Bin _ x Tip r -> (x, r)
      Bin _ x l r   -> let (xm, l`) = deleteFindMin l in (xm, balance x l` r)
      Tip           -> (abort "Set.deleteFindMin: can not return the minimal element of an empty set", Tip)

// | /O(log n)/. Delete and find the maximal element.
//
// > deleteFindMax set = (findMax set, deleteMax set)
deleteFindMax :: .(Set a) -> (a, Set a)
deleteFindMax t
  = case t of
      Bin _ x l Tip -> (x, l)
      Bin _ x l r   -> let (xm, r`) = deleteFindMax r in (xm, balance x l r`)
      Tip           -> (abort "Set.deleteFindMax: can not return the maximal element of an empty set", Tip)

// | /O(log n)/. Retrieves the minimal key of the set, and the set
// stripped of that element, or 'Nothing' if passed an empty set.
minView :: .(Set a) -> .(Maybe (a,Set a))
minView Tip = Nothing
minView x = Just (deleteFindMin x)

// | /O(log n)/. Retrieves the maximal key of the set, and the set
// stripped of that element, or 'Nothing' if passed an empty set.
maxView :: .(Set a) -> .(Maybe (a,Set a))
maxView Tip = Nothing
maxView x = Just (deleteFindMax x)

/*--------------------------------------------------------------------
  [balance x l r] balances two trees with value x.
  The sizes of the trees should balance after decreasing the
  size of one of them. (a rotation).

  [delta] is the maximal relative difference between the sizes of
          two trees, it corresponds with the [w] in Adams' paper,
          or equivalently, [1/delta] corresponds with the $\alpha$
          in Nievergelt's paper. Adams shows that [delta] should
          be larger than 3.745 in order to garantee that the
          rotations can always restore balance.         

  [ratio] is the ratio between an outer and inner sibling of the
          heavier subtree in an unbalanced setting. It determines
          whether a double or single rotation should be performed
          to restore balance. It is correspondes with the inverse
          of $\alpha$ in Adam's article.

  Note that:
  - [delta] should be larger than 4.646 with a [ratio] of 2.
  - [delta] should be larger than 3.745 with a [ratio] of 1.534.
  
  - A lower [delta] leads to a more 'perfectly' balanced tree.
  - A higher [delta] performs less rebalancing.

  - Balancing is automatic for random data and a balancing
    scheme is only necessary to avoid pathological worst cases.
    Almost any choice will do in practice
    
  - Allthough it seems that a rather large [delta] may perform better 
    than smaller one, measurements have shown that the smallest [delta]
    of 4 is actually the fastest on a wide range of operations. It
    especially improves performance on worst-case scenarios like
    a sequence of ordered insertions.

  Note: in contrast to Adams' paper, we use a ratio of (at least) 2
  to decide whether a single or double rotation is needed. Allthough
  he actually proves that this ratio is needed to maintain the
  invariants, his implementation uses a (invalid) ratio of 1. 
  He is aware of the problem though since he has put a comment in his 
  original source code that he doesn't care about generating a 
  slightly inbalanced tree since it doesn't seem to matter in practice. 
  However (since we use quickcheck :-) we will stick to strictly balanced 
  trees.
--------------------------------------------------------------------*/
delta :== 4
ratio :== 2

balance :: a (Set a) (Set a) -> Set a
balance x l r
  | sizeL + sizeR <= 1    = Bin sizeX x l r
  | sizeR >= delta*sizeL  = rotateL x l r
  | sizeL >= delta*sizeR  = rotateR x l r
  | otherwise             = Bin sizeX x l r
  where
    sizeL = size l
    sizeR = size r
    sizeX = sizeL + sizeR + 1

// rotate
rotateL :: a (Set a) (Set a) -> Set a
rotateL x l r=:(Bin _ _ ly ry)
  | size ly < ratio*size ry = singleL x l r
  | otherwise               = doubleL x l r
rotateL _ _ Tip = abort "rotateL Tip"

rotateR :: a (Set a) (Set a) -> Set a
rotateR x l=:(Bin _ _ ly ry) r
  | size ry < ratio*size ly = singleR x l r
  | otherwise               = doubleR x l r
rotateR _ Tip _ = abort "rotateL Tip"

// basic rotations
singleL :: a (Set a) (Set a) -> Set a
singleL x1 t1 (Bin _ x2 t2 t3)  = bin x2 (bin x1 t1 t2) t3
singleL _  _  Tip               = abort "singleL"

singleR :: a (Set a) (Set a) -> Set a
singleR x1 (Bin _ x2 t1 t2) t3  = bin x2 t1 (bin x1 t2 t3)
singleR _  Tip              _   = abort "singleR"

doubleL :: !a (Set a) (Set a) -> Set a
doubleL x1 t1 (Bin _ x2 (Bin _ x3 t2 t3) t4) = bin x3 (bin x1 t1 t2) (bin x2 t3 t4)
doubleL _ _ _ = abort "doubleL"
doubleR :: !a (Set a) (Set a) -> Set a
doubleR x1 (Bin _ x2 t1 (Bin _ x3 t2 t3)) t4 = bin x3 (bin x2 t1 t2) (bin x1 t3 t4)
doubleR _ _ _ = abort "doubleR"

/*--------------------------------------------------------------------
 * The bin constructor maintains the size of the tree
 *--------------------------------------------------------------------*/
bin :: a (Set a) (Set a) -> Set a
bin x l r
  = Bin (size l + size r + 1) x l r
