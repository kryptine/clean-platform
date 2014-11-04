implementation module Data.Set

import StdClass, StdMisc, StdBool, StdList, StdFunc, StdInt, StdTuple
import Data.Maybe

//mapSet :: !(a -> b) !(Set a) -> Set b | < a & == a & < b & == b
//mapSet f s = fromList (map f (toList s))

mapSetMonotonic :: !(a -> b) !(Set a) -> Set b
mapSetMonotonic _ Tip = Tip
mapSetMonotonic f (Bin n x l r) = Bin n (f x) (mapSetMonotonic f l) (mapSetMonotonic f r)

//compare :: !a !a -> Ordering | < a & == a
//compare x y = if (x < y) LT (if (x > y) GT EQ)

:: Ordering = LT | GT | EQ

compare x y :== if (x < y) LT (if (x > y) GT EQ)


/*
 * Sets are size balanced trees.
 * A set of values @a@.
 */
:: Set a = Tip
         | Bin !Int !a !(Set a) !(Set a)

instance == (Set a) | == a where
  (==) :: !(Set a) !(Set a) -> Bool | == a
  (==) t1 t2  = (size t1 == size t2) && (toAscList t1 == toAscList t2)

instance < (Set a) | < a where
  (<) :: !(Set a) !(Set a) -> Bool | < a
  (<) s1 s2 = compare (toAscList s1) (toAscList s2)
    where
    compare :: ![a] ![a] -> Bool | < a
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
//null :: !(Set a) -> Bool
//null Tip = True
//null (Bin _ _ _ _) = False

// | /O(1)/. The number of elements in the set.
//size s :== case s of
             //Tip -> 0
             //(Bin sz _ _ _) -> sz
// TODO Remove
//size :: !(Set a) -> Int
//size Tip = 0
//size (Bin sz _ _ _) = sz

// | /O(log n)/. Is the element in the set?
member :: !a !(Set a) -> Bool | < a & == a
member x Tip = False
member x (Bin _ y l r) =
          case compare x y of
            LT -> member x l
            GT -> member x r
            EQ -> True

// | /O(log n)/. Is the element not in the set?
//notMember :: !a !(Set a) -> Bool | < a & == a
//notMember x t = not (member x t)

/*--------------------------------------------------------------------
 * Construction
 *--------------------------------------------------------------------*/
 
// | /O(1)/. The empty set.
newSet :: Set a
newSet = Tip

// | /O(1)/. Create a singleton set.
singleton :: !u:a -> w:(Set u:a), [w <= u]
singleton x = Bin 1 x Tip Tip

/*--------------------------------------------------------------------
 * Insertion, Deletion
 *--------------------------------------------------------------------*/

// | /O(log n)/. Insert an element in a set.
// If the set already contains an element equal to the given value,
// it is replaced with the new value.
insert :: !a !.(Set a) -> Set a | < a & == a
insert x Tip = singleton x
insert x (Bin sz y l r) =
  case compare x y of
    LT -> balance y (insert x l) r
    GT -> balance y l (insert x r)
    EQ -> Bin sz x l r

insertR :: !a !(Set a) -> Set a | < a & == a
insertR x Tip = singleton x
insertR x t=:(Bin _ y l r) = case compare x y of
        LT -> balanceL y (insertR x l) r
        GT -> balanceR y l (insertR x r)
        EQ -> t

// | /O(log n)/. Delete an element from a set.
delete :: !a !.(Set a) -> Set a | < a & == a
delete x Tip = Tip
delete x (Bin _ y l r) =
  case compare x y of
    LT -> balance y (delete x l) r
    GT -> balance y l (delete x r)
    EQ -> glue l r

/*--------------------------------------------------------------------
 * Subset
 *--------------------------------------------------------------------*/

// | /O(n+m)/. Is this a proper subset? (ie. a subset but not equal).
//isProperSubsetOf :: !(Set a) !(Set a) -> Bool | < a & == a
//isProperSubsetOf s1 s2 = (size s1 < size s2) && (isSubsetOf s1 s2)

// | /O(n+m)/. Is this a subset?
// @(s1 `isSubsetOf` s2)@ tells whether @s1@ is a subset of @s2@.
//isSubsetOf :: !(Set a) !(Set a) -> Bool | < a & == a
//isSubsetOf t1 t2 = (size t1 <= size t2) && (isSubsetOfX t1 t2)

isSubsetOfX :: !(Set a) !(Set a) -> Bool | < a & == a
isSubsetOfX Tip _ = True
isSubsetOfX _ Tip = False
isSubsetOfX (Bin _ x l r) t
  #! (lt, found, gt) = splitMember x t
  = found && isSubsetOfX l lt && isSubsetOfX r gt

/*--------------------------------------------------------------------
 * Minimal, Maximal
 *--------------------------------------------------------------------*/
 
// | /O(log n)/. The minimal element of a set.
findMin :: !(Set a) -> a
findMin (Bin _ x Tip _) = x
findMin (Bin _ _ l _)   = findMin l
findMin Tip             = abort "Set.findMin: empty set has no minimal element"

// | /O(log n)/. The maximal element of a set.
findMax :: !(Set a) -> a
findMax (Bin _ x _ Tip)  = x
findMax (Bin _ _ _ r)    = findMax r
findMax Tip              = abort "Set.findMax: empty set has no maximal element"

// | /O(log n)/. Delete the minimal element.
deleteMin :: !.(Set a) -> Set a
deleteMin (Bin _ _ Tip r) = r
deleteMin (Bin _ x l r)   = balance x (deleteMin l) r
deleteMin Tip             = Tip

// | /O(log n)/. Delete the maximal element.
deleteMax :: !.(Set a) -> Set a
deleteMax (Bin _ _ l Tip) = l
deleteMax (Bin _ x l r)   = balance x l (deleteMax r)
deleteMax Tip             = Tip

/*--------------------------------------------------------------------
 * Union. 
 *--------------------------------------------------------------------*/
 
// | The union of a list of sets: (@'unions' == 'foldl' 'union' 'empty'@).
//unions :: !u:[v:(Set a)] -> Set a | < a & == a, [u <= v]
//unions ts = foldl union newSet ts

// | /O(n+m)/. The union of two sets, preferring the first set when
// equal elements are encountered.
// The implementation uses the efficient /hedge-union/ algorithm.
// Hedge-union is more efficient on (bigset `union` smallset).
union :: !u:(Set a) !u:(Set a) -> Set a | < a & == a
union Tip t2  = t2
union t1 Tip  = t1
union t1 t2 = hedgeUnion NothingS NothingS t1 t2

hedgeUnion :: !(MaybeS a) !(MaybeS a) !(Set a) !(Set a) -> Set a | < a & == a
hedgeUnion _   _   t1  Tip = t1
hedgeUnion blo bhi Tip (Bin _ x l r) = link x (filterGt blo l) (filterLt bhi r)
hedgeUnion _   _   t1  (Bin _ x Tip Tip) = insertR x t1
hedgeUnion blo bhi (Bin _ x l r) t2 = link x (hedgeUnion blo bmi l (trim blo bmi t2))
                                             (hedgeUnion bmi bhi r (trim bmi bhi t2))
  where bmi = JustS x

/*--------------------------------------------------------------------
 * Difference
 *--------------------------------------------------------------------*/
 
// | /O(n+m)/. Difference of two sets. 
// The implementation uses an efficient /hedge/ algorithm comparable with /hedge-union/.
difference :: !(Set a) !(Set a) -> Set a | < a & == a
difference Tip _   = Tip
difference t1 Tip  = t1
difference t1 t2   = hedgeDiff NothingS NothingS t1 t2

hedgeDiff :: !(MaybeS a) !(MaybeS a) !(Set a) !(Set a) -> Set a | < a & == a
hedgeDiff _   _   Tip           _ = Tip
hedgeDiff blo bhi (Bin _ x l r) Tip = link x (filterGt blo l) (filterLt bhi r)
hedgeDiff blo bhi t (Bin _ x l r) = merge (hedgeDiff blo bmi (trim blo bmi t) l)
                                          (hedgeDiff bmi bhi (trim bmi bhi t) r)
  where bmi = JustS x


/*--------------------------------------------------------------------
 * Intersection
 *--------------------------------------------------------------------*/

intersections :: ![Set a] -> Set a | < a & == a
intersections [t] = t
intersections [t:ts] = foldl intersection t ts

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

intersection :: !(Set a) !(Set a) -> Set a | < a & == a
intersection Tip _ = Tip
intersection _ Tip = Tip
intersection t1 t2 = hedgeInt NothingS NothingS t1 t2

hedgeInt :: !(MaybeS a) !(MaybeS a) !(Set a) !(Set a) -> Set a | < a & == a
hedgeInt _ _ _   Tip = Tip
hedgeInt _ _ Tip _   = Tip
hedgeInt blo bhi (Bin _ x l r) t2
  #! bmi = JustS x
  #! l` = hedgeInt blo bmi l (trim blo bmi t2)
  #! r` = hedgeInt bmi bhi r (trim bmi bhi t2)
  = if (member x t2)
      (link x l` r`)
      (merge l` r`)

/*--------------------------------------------------------------------
 * Filter and partition
 *--------------------------------------------------------------------*/

// | /O(n)/. Filter all elements that satisfy the predicate.
filter :: !(a -> Bool) !(Set a) -> Set a | < a & == a
filter _ Tip = Tip
filter p (Bin _ x l r)
  | p x       = link x (filter p l) (filter p r)
  | otherwise = merge (filter p l) (filter p r)

// | /O(n)/. Partition the set into two sets, one with all elements that satisfy
// the predicate and one with all elements that don't satisfy the predicate.
// See also 'split'.
partition :: !(a -> Bool) !(Set a) -> (!Set a, !Set a) | < a & == a
partition _ Tip = (Tip,Tip)
partition p (Bin _ x l r)
  | p x       = (link x l1 r1,merge l2 r2)
  | otherwise = (merge l1 r1,link x l2 r2)
  where
    (l1,l2) = partition p l
    (r1,r2) = partition p r

/*--------------------------------------------------------------------
 * Fold
 *--------------------------------------------------------------------*/

// | /O(n)/. Post-order fold.
fold :: !(a -> .b -> .b) !.b !.(Set a) -> .b
fold _ z Tip           = z
fold f z (Bin _ x l r) = fold f (f x (fold f z r)) l

/*--------------------------------------------------------------------
 * Lists 
 *--------------------------------------------------------------------*/

// | /O(n)/. Convert the set to a list of elements.
//toList :: !(Set a) -> [a]
//toList s = toAscList s

// | /O(n)/. Convert the set to an ascending list of elements.
//toAscList :: !(Set a) -> [a]
//toAscList t = fold (\a as -> [a:as]) [] t

// | /O(n*log n)/. Create a set from a list of elements.
fromList :: ![a] -> Set a | < a & == a
fromList xs = foldl ins newSet xs
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

:: MaybeS a = NothingS | JustS !a

/*--------------------------------------------------------------------
  [trim lo hi t] trims away all subtrees that surely contain no
  values between the range [lo] to [hi]. The returned tree is either
  empty or the key of the root is between @lo@ and @hi@.
--------------------------------------------------------------------*/

trim :: !(MaybeS a) !(MaybeS a) !(Set a) -> Set a | < a & == a
trim NothingS   NothingS   t = t
trim (JustS lx) NothingS   t = greater lx t
  where
  greater lo (Bin _ x _ r) | x <= lo = greater lo r
  greater _  t` = t`
trim NothingS   (JustS hx) t = lesser hx t
  where
  lesser  hi (Bin _ x l _) | x >= hi = lesser  hi l
  lesser  _  t` = t`
trim (JustS lx) (JustS hx) t = middle lx hx t
  where
  middle lo hi (Bin _ x _ r) | x <= lo = middle lo hi r
  middle lo hi (Bin _ x l _) | x >= hi = middle lo hi l
  middle _  _  t` = t`

/*--------------------------------------------------------------------
 * [filterGt x t] filter all values >[x] from tree [t]
 * [filterLt x t] filter all values <[x] from tree [t]
 *--------------------------------------------------------------------*/

filterGt :: !(MaybeS a) !(Set a) -> Set a | < a & == a
filterGt NothingS t = t
filterGt (JustS b) t = filter` b t
  where filter` _   Tip = Tip
        filter` b` (Bin _ x l r) =
          case compare b` x of LT -> link x (filter` b` l) r
                               EQ -> r
                               GT -> filter` b` r

filterLt :: !(MaybeS a) !(Set a) -> Set a | < a & == a
filterLt NothingS t = t
filterLt (JustS b) t = filter` b t
  where filter` _   Tip = Tip
        filter` b` (Bin _ x l r) =
          case compare x b` of LT -> link x l (filter` b` r)
                               EQ -> l
                               GT -> filter` b` l

/*--------------------------------------------------------------------
 * Split
 *--------------------------------------------------------------------*/

// | /O(log n)/. The expression (@'split' x set@) is a pair @(set1,set2)@
// where @set1@ comprises the elements of @set@ less than @x@ and @set2@
// comprises the elements of @set@ greater than @x@.
split :: !a !(Set a) -> (!Set a, !Set a) | < a & == a
split _ Tip = (Tip,Tip)
split x (Bin _ y l r)
  = case compare x y of
      LT
        #! (lt, gt) = split x l
        = (lt, link y gt r)
      GT
        #! (lt,gt) = split x r
        = (link y l lt,gt)
      EQ
        = (l,r)

// | /O(log n)/. Performs a 'split' but also returns whether the pivot
// element was found in the original set.
splitMember :: !a !(Set a) -> (!Set a, !Bool, !Set a) | < a & == a
splitMember _ Tip = (Tip, False, Tip)
splitMember x (Bin _ y l r)
  = case compare x y of
      LT
        #! (lt, found, gt) = splitMember x l
        = (lt, found, link y gt r)
      GT
        #! (lt, found, gt) = splitMember x r
        = (link y l lt, found, gt)
      EQ
        = (l, True, r)


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
link :: !a !(Set a) !(Set a) -> Set a
link x Tip r  = insertMin x r
link x l Tip  = insertMax x l
link x l=:(Bin sizeL y ly ry) r=:(Bin sizeR z lz rz)
  | delta*sizeL <= sizeR  = balance z (link x l lz) rz
  | delta*sizeR <= sizeL  = balance y ly (link x ry r)
  | otherwise             = bin x l r

// insertMin and insertMax don't perform potentially expensive comparisons.
insertMax :: !a !(Set a) -> Set a
insertMax x Tip = singleton x
insertMax x (Bin _ y l r) = balance y l (insertMax x r)

insertMin :: !a !(Set a) -> Set a
insertMin x Tip = singleton x
insertMin x (Bin _ y l r) = balance y (insertMin x l) r
         
/*--------------------------------------------------------------------
 * [merge l r]: merges two trees.
 *--------------------------------------------------------------------*/
merge :: !(Set a) !(Set a) -> Set a
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
glue :: !.(Set a) !.(Set a) -> Set a
glue Tip r = r
glue l Tip = l
glue l r
  | size l > size r
      #! (m, l`) = deleteFindMax l
      = balance m l` r
  | otherwise
      #! (m, r`) = deleteFindMin r
      = balance m l r`

// | /O(log n)/. Delete and find the minimal element.
// 
// > deleteFindMin set = (findMin set, deleteMin set)
deleteFindMin :: !.(Set a) -> (!a, !Set a)
deleteFindMin (Bin _ x Tip r) = (x, r)
deleteFindMin (Bin _ x l r)
  #! (xm, l`) = deleteFindMin l
  = (xm, balance x l` r)
deleteFindMin Tip = (abort "Set.deleteFindMin: can not return the minimal element of an empty set", Tip)

// | /O(log n)/. Delete and find the maximal element.
//
// > deleteFindMax set = (findMax set, deleteMax set)
deleteFindMax :: !.(Set a) -> (!a, !Set a)
deleteFindMax (Bin _ x l Tip ) = (x, l)
deleteFindMax (Bin _ x l r)
  #! (xm, r`) = deleteFindMax r
  = (xm, balance x l r`)
deleteFindMax Tip = (abort "Set.deleteFindMax: can not return the maximal element of an empty set", Tip)

// | /O(log n)/. Retrieves the minimal key of the set, and the set
// stripped of that element, or 'Nothing' if passed an empty set.
minView :: !.(Set a) -> .(Maybe (!a, !Set a))
minView Tip = Nothing
minView x = Just (deleteFindMin x)

// | /O(log n)/. Retrieves the maximal key of the set, and the set
// stripped of that element, or 'Nothing' if passed an empty set.
maxView :: !.(Set a) -> .(Maybe (!a, !Set a))
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

balance :: !a !(Set a) !(Set a) -> Set a
balance x l r
  #! sizeL = size l
  #! sizeR = size r
  #! sizeX = sizeL + sizeR + 1
  | sizeL + sizeR <= 1    = Bin sizeX x l r
  | sizeR >= delta*sizeL  = rotateL x l r
  | sizeL >= delta*sizeR  = rotateR x l r
  | otherwise             = Bin sizeX x l r

// Functions balanceL and balanceR are specialised versions of balance.
// balanceL only checks whether the left subtree is too big,
// balanceR only checks whether the right subtree is too big.

// balanceL is called when left subtree might have been inserted to or when
// right subtree might have been deleted from.
balanceL :: !a !(Set a) !(Set a) -> Set a
balanceL x l r = case r of
  Tip -> case l of
           Tip -> Bin 1 x Tip Tip
           (Bin _ _ Tip Tip) -> Bin 2 x l Tip
           (Bin _ lx Tip (Bin _ lrx _ _)) -> Bin 3 lrx (Bin 1 lx Tip Tip) (Bin 1 x Tip Tip)
           (Bin _ lx ll=:(Bin _ _ _ _) Tip) -> Bin 3 lx ll (Bin 1 x Tip Tip)
           (Bin ls lx ll=:(Bin lls _ _ _) lr=:(Bin lrs lrx lrl lrr))
             | lrs < ratio*lls -> Bin (1+ls) lx ll (Bin (1+lrs) x lr Tip)
             | otherwise -> Bin (1+ls) lrx (Bin (1+lls+size lrl) lx ll lrl) (Bin (1+size lrr) x lrr Tip)

  (Bin rs _ _ _) -> case l of
           Tip -> Bin (1+rs) x Tip r

           (Bin ls lx ll lr)
              | ls > delta*rs  -> case (ll, lr) of
                   (Bin lls _ _ _, Bin lrs lrx lrl lrr)
                     | lrs < ratio*lls -> Bin (1+ls+rs) lx ll (Bin (1+rs+lrs) x lr r)
                     | otherwise -> Bin (1+ls+rs) lrx (Bin (1+lls+size lrl) lx ll lrl) (Bin (1+rs+size lrr) x lrr r)
                   (_, _) -> abort "Failure in Data.Map.balanceL"
              | otherwise -> Bin (1+ls+rs) x l r

// balanceR is called when right subtree might have been inserted to or when
// left subtree might have been deleted from.
balanceR :: !a !(Set a) !(Set a) -> Set a
balanceR x l r = case l of
  Tip -> case r of
           Tip -> Bin 1 x Tip Tip
           (Bin _ _ Tip Tip) -> Bin 2 x Tip r
           (Bin _ rx Tip rr=:(Bin _ _ _ _)) -> Bin 3 rx (Bin 1 x Tip Tip) rr
           (Bin _ rx (Bin _ rlx _ _) Tip) -> Bin 3 rlx (Bin 1 x Tip Tip) (Bin 1 rx Tip Tip)
           (Bin rs rx rl=:(Bin rls rlx rll rlr) rr=:(Bin rrs _ _ _))
             | rls < ratio*rrs -> Bin (1+rs) rx (Bin (1+rls) x Tip rl) rr
             | otherwise -> Bin (1+rs) rlx (Bin (1+size rll) x Tip rll) (Bin (1+rrs+size rlr) rx rlr rr)

  (Bin ls _ _ _) -> case r of
           Tip -> Bin (1+ls) x l Tip

           (Bin rs rx rl rr)
              | rs > delta*ls  -> case (rl, rr) of
                   (Bin rls rlx rll rlr, Bin rrs _ _ _)
                     | rls < ratio*rrs -> Bin (1+ls+rs) rx (Bin (1+ls+rls) x l rl) rr
                     | otherwise -> Bin (1+ls+rs) rlx (Bin (1+ls+size rll) x l rll) (Bin (1+rrs+size rlr) rx rlr rr)
                   (_, _) -> abort "Failure in Data.Map.balanceR"
              | otherwise -> Bin (1+ls+rs) x l r

// rotate
rotateL :: !a !(Set a) !(Set a) -> Set a
rotateL x l r=:(Bin _ _ ly ry)
  | size ly < ratio*size ry = singleL x l r
  | otherwise               = doubleL x l r
rotateL _ _ Tip = abort "rotateL Tip"

rotateR :: !a !(Set a) !(Set a) -> Set a
rotateR x l=:(Bin _ _ ly ry) r
  | size ry < ratio*size ly = singleR x l r
  | otherwise               = doubleR x l r
rotateR _ Tip _ = abort "rotateL Tip"

// basic rotations
singleL :: !a !(Set a) !(Set a) -> Set a
singleL x1 t1 (Bin _ x2 t2 t3)  = bin x2 (bin x1 t1 t2) t3
singleL _  _  Tip               = abort "singleL"

singleR :: !a !(Set a) !(Set a) -> Set a
singleR x1 (Bin _ x2 t1 t2) t3  = bin x2 t1 (bin x1 t2 t3)
singleR _  Tip              _   = abort "singleR"

doubleL :: !a !(Set a) !(Set a) -> Set a
doubleL x1 t1 (Bin _ x2 (Bin _ x3 t2 t3) t4) = bin x3 (bin x1 t1 t2) (bin x2 t3 t4)
doubleL _ _ _ = abort "doubleL"

doubleR :: !a !(Set a) !(Set a) -> Set a
doubleR x1 (Bin _ x2 t1 (Bin _ x3 t2 t3)) t4 = bin x3 (bin x2 t1 t2) (bin x1 t3 t4)
doubleR _ _ _ = abort "doubleR"

/*--------------------------------------------------------------------
 * The bin constructor maintains the size of the tree
 *--------------------------------------------------------------------*/
//bin :: !a !(Set a) !(Set a) -> Set a
bin x l r :== Bin (size l + size r + 1) x l r
