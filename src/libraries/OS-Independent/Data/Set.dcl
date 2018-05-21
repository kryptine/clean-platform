definition module Data.Set

/**
 * An efficient implementation of sets.
 *
 * The implementation of `Set` is based on size balanced binary trees (or trees
 * of bounded balance) as described by:
 *
 * - Stephen Adams, "Efficient sets: a balancing act",
 *	 Journal of Functional Programming 3(4):553-562, October 1993,
 *	 <http://www.swiss.ai.mit.edu/~adams/BB/>.
 *
 * - J. Nievergelt and E.M. Reingold, "Binary search trees of bounded balance",
 *	 SIAM journal of computing 2(1), March 1973.
 *
 * Note that the implementation is left-biased -- the elements of a first
 * argument are always preferred to the second, for example in {{`union`}} or
 * {{`insert`}}. Of course, left-biasing can only be observed when equality is
 * an equivalence relation instead of structural equality.
 *
 * This module is ported from Haskell's Data.Set (copyright Daan Leijen 2002)
 * by László Domoszlai, 2013.
 *
 * @property-bootstrap
 *   import StdBool, StdChar, StdInt, StdOrdList, StdString, StdTuple
 *   import StdList => qualified insert, filter
 *   from Data.Func import on, `on`
 *   import Data.GenLexOrd
 *   import Data.Set
 *
 *   derive genShow Maybe
 *   genShow{|Set|} show sep p xs rest = ["Set{":showList (toList xs) ["}":rest]]
 *   where
 *     showList [x]    rest = show sep False x rest
 *     showList [x:xs] rest = show sep False x [",":showList xs rest]
 *     showList []     rest = rest
 *
 *   derive bimap []
 *
 *   :: Predicate a = ConstTrue | IsMember [a]
 *
 *   pred :: (Predicate a) a -> Bool | Eq a
 *   pred ConstTrue     _ = True
 *   pred (IsMember cs) c = isMember c cs
 *
 *   derive ggen Predicate
 *   derive genShow Predicate
 *   derive JSONEncode Predicate, Set
 *
 *   // Check that all elements from a list are in a set.
 *   contains :: (Set a) [a] -> Bool | < a
 *   contains d xs = all (\x -> member x d) xs
 *
 *   // Check that no elements from a list are in a set.
 *   does_not_contain :: (Set a) [a] -> Bool | < a
 *   does_not_contain d ys = all (\y -> notMember y d) ys
 *
 *   // Check that all elements from a set are in a list.
 *   all_in :: (Set a) [a] -> Bool | Eq a
 *   all_in s xs = all (\e -> isMember e xs) (toList s)
 *
 *   // Check that the data structure is still correct.
 *   integrity :: (Set a) -> Property | Eq, genShow{|*|}, JSONEncode{|*|} a
 *   integrity s =
 *     name "no_duplicates" (no_duplicates s) /\
 *     name "log_size"      (log_size s) /\
 *     name "sizes_correct" (sizes_correct s)
 *
 *   // Check that a set contains no duplicates.
 *   no_duplicates :: (Set a) -> Property | Eq, genShow{|*|}, JSONEncode{|*|} a
 *   no_duplicates s = xs =.= removeDup xs where xs = toList s
 *
 *   // Check that a set is log(n) in depth.
 *   log_size :: (Set a) -> Property
 *   log_size s = check (<) nelem (2 ^ depth s)
 *   where
 *     nelem = size s
 *
 *     depth :: (Set a) -> Int
 *     depth Tip = 0
 *     depth (Bin _ _ l r) = 1 + (max `on` depth) l r
 *
 *   // Check that the sizes in a set are correct.
 *   sizes_correct :: (Set a) -> Property
 *   sizes_correct Tip = prop True
 *   sizes_correct b=:(Bin _ _ l r) =
 *     size b =.= 1 + size l + size r /\
 *     sizes_correct l /\
 *     sizes_correct r
 * @property-test-with a = Char
 */

from StdOverloaded	import class ==, class < (..)
from StdClass import class Ord (..), <=, >
from Data.Maybe		import :: Maybe
from Data.List import foldl, map
from StdBool import not, &&
from Data.GenEq import generic gEq
from Data.GenLexOrd import generic gLexOrd, :: LexOrd
from Data.Foldable import class Foldable

:: Set a = Tip
         | Bin !Int !a !(Set a) !(Set a)

instance == (Set a) | == a
instance < (Set a) | < a
derive gEq Set
derive gLexOrd Set
instance Foldable Set

/**
 * True iff this is the empty set.
 * @type (Set a) -> Bool
 * @property null_correct: A.xs :: [a]:
 *   (size xs` == 0 <==> null xs`)
 *     /\ (xs` == newSet <==> null xs`)
 *   where xs` = fromList xs
 */
null s :== case s of
             Tip -> True
             (Bin sz _ _ _) -> False

/**
 * The number of elements in the set.
 * @type (Set a) -> Int
 * @property size_correct: A.xs :: [a]:
 *   size (fromList xs) =.= length (removeDup xs)
 */
size s :== case s of
             Tip -> 0
             (Bin sz _ _ _) -> sz

/**
 * Is the element in the set?
 * @complexity O(log n)
 * @property member_correct: A. x :: a; xs :: [a]:
 *   member x (fromList xs) <==> isMember x xs
 */
member    :: !a !(Set a) -> Bool | < a

/**
 * Checks if an element is not in the set.
 * @complexity O(log n)
 * @type a (Set a) -> Bool | <, == a
 */
notMember x t :== not (member x t)

/**
 * Is t1 a subset of t2?
 * @type (Set a) (Set a) -> Bool | <, == a
 * @complexity O(n+m)
 * @property isSubsetOf_correct: A.xs :: [a]; ys :: [a]:
 *   isSubsetOf (fromList xs) (fromList ys) <==> all (\x -> isMember x ys) xs
 */
isSubsetOf t1 t2 :== (size t1 <= size t2) && (isSubsetOfX t1 t2)

isSubsetOfX :: !(Set a) !(Set a) -> Bool | < a

/**
 * Is t1 a proper subset of t2?
 * @type (Set a) (Set a) -> Bool | <, == a
 * @complexity O(n+m)
 * @property isProperSubsetOf_correct: A.xs :: [a]; ys :: [a]:
 *   isProperSubsetOf (fromList xs) (fromList ys)
 *     <==> all (\x -> isMember x ys) xs && not (all (\y -> isMember y xs) ys)
 */
isProperSubsetOf s1 s2 :== (size s1 < size s2) && (isSubsetOf s1 s2)

/**
 * The empty set.
 * @complexity O(1)
 * @property newSet_null:
 *   name "newSet_null" (null newSet)
 */
newSet :: Set a

/**
 * Create a singleton set.
 * @complexity O(1)
 */
singleton :: !u:a -> w:(Set u:a), [w <= u]

/**
 * Insert an element in a set. If the set already contains an element equal to
 * the given value, it is replaced with the new value.
 *
 * @complexity O(log n)
 * @property insert_correct: A.x :: a; xs :: [a]:
 *   let xs` = insert x (fromList xs) in
 *     check member x xs`    /\ // Membership
 *     check contains xs` xs /\ // Rest untouched
 *     integrity xs`            // Data structure integrity
 */
insert :: !a !.(Set a) -> Set a | < a

/**
 * Delete an element from a set.
 *
 * @complexity O(log n)
 * @property delete_correct: A.x :: a; xs :: [a]:
 *   let xs` = delete x (fromList xs) in
 *     check notMember x xs` /\                         // Membership
 *     check contains xs` [x` \\ x` <- xs | x` <> x] /\ // Rest untouched
 *     integrity xs`                                    // Data structure integrity
 */
delete :: !a !.(Set a) -> Set a | < a

/**
 * The minimal element of a set.
 * @complexity O(log n)
 * @property findMin_correct: A.xs :: [a]:
 *   not (isEmpty xs) ==> minList xs == findMin (fromList xs)
 */
findMin :: !(Set a) -> a

/**
 * The maximal element of a set.
 * @complexity O(log n)
 * @property findMax_correct: A.xs :: [a]:
 *   not (isEmpty xs) ==> maxList xs == findMax (fromList xs)
 */
findMax :: !(Set a) -> a

/**
 * Delete the minimal element.
 * @complexity O(log n)
 * @property deleteMin_correct: A.xs :: [a]:
 *   case xs of
 *     [] -> prop (null (deleteMin newSet))
 *     xs ->
 *       check notMember (minList xs) after /\
 *       size after =.= size before - 1 /\
 *       integrity after
 *     with
 *       before = fromList xs
 *       after = deleteMin before
 */
deleteMin :: !.(Set a) -> Set a

/**
 * Delete the maximal element.
 * @complexity O(log n)
 * @property deleteMax_correct: A.xs :: [a]:
 *   case xs of
 *     [] -> prop (null (deleteMax newSet))
 *     xs ->
 *       check notMember (maxList xs) after /\
 *       size after =.= size before - 1 /\
 *       integrity after
 *     with
 *       before = fromList xs
 *       after = deleteMax before
 */
deleteMax :: !.(Set a) -> Set a

/**
 * deleteFindMin set = (findMin set, deleteMin set)
 * @complexity O(log n)
 * @property deleteFindMin_correct: A.xs :: [a]:
 *   not (isEmpty xs) ==>
 *     let (m,xs`) = deleteFindMin (fromList xs) in
 *     m =.= min /\ notMember min xs`
 *   where min = minList xs
 */
deleteFindMin :: !.(Set a) -> (!a, !Set a)

/**
 * deleteFindMax set = (findMax set, deleteMax set)
 * @complexity O(log n)
 * @property deleteFindMax_correct: A.xs :: [a]:
 *   not (isEmpty xs) ==>
 *     let (m,xs`) = deleteFindMax (fromList xs) in
 *     m =.= max /\ notMember max xs`
 *   where max = maxList xs
 */
deleteFindMax :: !.(Set a) -> (!a, !Set a)

/**
 * Retrieves the minimal key of the set, and the set stripped of that element,
 * or 'Nothing' if passed an empty set.
 * @complexity O(log n)
 * @property minView_correct: A.xs :: [a]:
 *   case xs of
 *     [] -> Nothing =.= minView xs`
 *     xs -> Just (deleteFindMin xs`) =.= minView xs`
 *   where xs` = fromList xs
 */
minView :: !.(Set a) -> .(Maybe (!a, !Set a))

/**
 * Retrieves the maximal key of the set, and the set stripped of that element,
 * or 'Nothing' if passed an empty set.
 * @complexity O(log n)
 * @property maxView_correct: A.xs :: [a]:
 *   case xs of
 *     [] -> Nothing =.= maxView xs`
 *     xs -> Just (deleteFindMax xs`) =.= maxView xs`
 *   where xs` = fromList xs
 */
maxView :: !.(Set a) -> .(Maybe (!a, !Set a))

/**
 * The union of two sets, preferring the first set when equal elements are
 * encountered.
 * @complexity O(n+m)
 * @property union_correct: A.xs :: [a]; ys :: [a]:
 *   check contains u (xs ++ ys)    // No missing elements
 *     /\ check all_in u (xs ++ ys) // No junk
 *     /\ integrity u               // Data structure integrity
 *   where u = union (fromList xs) (fromList ys)
 */
union :: !u:(Set a) !u:(Set a) -> Set a | < a & == a

/**
 * The union of a list of sets.
 * @type !u:[v:(Set a)] -> Set a | < a & == a, [u <= v]
 */
unions ts :== foldl union newSet ts

/**
 * Difference of two sets.
 * @complexity O(n+m)
 * @property difference_correct: A.xs :: [a]; ys :: [a]:
 *   check does_not_contain d ys                                // No remaining elements
 *     /\ check contains d [x \\ x <- xs | not (isMember x ys)] // All good elements
 *     /\ integrity d                                           // Data structure integrity
 *   where d = difference (fromList xs) (fromList ys)
 */
difference :: !(Set a) !(Set a) -> Set a | < a & == a

/**
 * The intersection of two sets.
 * Elements of the result come from the first set.
 * @complexity O(n+m)
 * @property intersection_correct: A.xs :: [a]; ys :: [a]:
 *   check does_not_contain i [x \\ x <- xs | not (isMember x ys)]      // No junk
 *     /\ check does_not_contain i [y \\ y <- ys | not (isMember y xs)] // No junk
 *     /\ check contains i [x \\ x <- xs | isMember x ys]               // All good elements
 *     /\ integrity i                                                   // Data structure integrity
 *   where i = intersection (fromList xs) (fromList ys)
 */
intersection :: !(Set a) !(Set a) -> Set a | < a & == a

/**
 * The intersection of a list of sets.
 * Elements of the result come from the first set
 */
intersections :: ![Set a] -> Set a | < a & == a

/**
 * Filter all elements that satisfy the predicate.
 * @complexity O(n)
 * @property filter_correct: A.p :: Predicate a; xs :: [a]:
 *   sort (toList (filter (pred p) (fromList xs)))
 *     =.= sort (removeDup ('StdList'.filter (pred p) xs))
 */
filter :: !(a -> Bool) !(Set a) -> Set a | < a

/**
 * Partition the set into two sets, one with all elements that satisfy the
 * predicate and one with all elements that don't satisfy the predicate.
 * See also {{`split`}}.
 *
 * @complexity O(n)
 * @property partition_correct: A.p :: Predicate a; xs :: [a]:
 *   all p` true`                       // Right split
 *     /\ all (\x -> not (p` x)) false`
 *     /\ all (\x -> isMember x xs) xs` // No junk
 *     /\ all (\x -> isMember x xs`) xs // All members used
 *     /\ integrity true                // Data structure integrity
 *     /\ integrity false
 * where
 *   p` = pred p
 *   (true,false) = partition p` (fromList xs)
 *   (true`,false`) = (toList true, toList false)
 *   xs` = true` ++ false`
 */
partition :: !(a -> Bool) !(Set a) -> (!Set a, !Set a) | < a

/**
 * Split a set in elements less and elements greater than a certain pivot.
 *
 * @param The pivot.
 * @param The set.
 * @result A tuple of two sets containing small and large values.
 * @complexity O(log n)
 *
 * @property split_correct: A.p :: a; xs :: [a]:
 *   all ((>) p) lt`                        // Right split
 *     /\ all ((<) p) gt`
 *     /\ all (\x -> isMember x xsminp) xs` // No junk
 *     /\ all (\x -> isMember x xs`) xsminp // All members used
 *     /\ integrity lt                      // Data structure integrity
 *     /\ integrity gt
 *   where
 *     xsminp = 'StdList'.filter ((<>) p) xs
 *     (lt,gt) = split p (fromList xs)
 *     (lt`,gt`) = (toList lt, toList gt)
 *     xs` = lt` ++ gt`
 */
split :: !a !(Set a) -> (!Set a, !Set a) | < a

/**
 * Performs a 'split' but also returns whether the pivot element was found in
 * the original set.
 *
 * @complexity O(log n)
 * @property splitMember_correct: A.p :: a; xs :: [a]:
 *   all ((>) p) lt`                        // Right split
 *     /\ all ((<) p) gt`
 *     /\ all (\x -> isMember x xsminp) xs` // No junk
 *     /\ all (\x -> isMember x xs`) xsminp // All members used
 *     /\ bool =.= isMember p xs            // Boolean is correct
 *     /\ integrity lt                      // Data structure integrity
 *     /\ integrity gt
 *   where
 *     xsminp = 'StdList'.filter ((<>) p) xs
 *     (lt,bool,gt) = splitMember p (fromList xs)
 *     (lt`,gt`) = (toList lt, toList gt)
 *     xs` = lt` ++ gt`
 */
splitMember :: !a !(Set a) -> (!Set a, !Bool, !Set a) | < a

/**
 * Post-order fold.
 * @complexity O(n)
 */
fold :: !(a -> .b -> .b) !.b !.(Set a) -> .b

/**
 * Convert the set to an ascending list of elements.
 * @type (Set a) -> [a]
 * @complexity O(n)
 * @property fromList_toList: A.xs :: [a]:
 *   xs` =.= fromList (toList xs`) where xs` = fromList xs
 */
toList s :== toAscList s

/**
 * Same as toList.
 * @complexity O(n)
 * @type (Set a) -> [a]
 */
toAscList t :== fold (\a as -> [a:as]) [] t

/**
 * Create a set from a list of elements.
 * @complexity O(n*log n)
 */
fromList :: ![a] -> Set a | < a

/**
 * Map a function to all elements in a set.
 * @type (a -> b) (Set a) -> Set b | <, == a & <, == b
 */
mapSet f s :== fromList (map f (toList s))

/**
 * Map a set without converting it to and from a list.
 */
mapSetMonotonic :: !(a -> b) !(Set a) -> Set b
