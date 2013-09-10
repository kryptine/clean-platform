definition module Set

from StdOverloaded	import class ==, class <
from Maybe			import :: Maybe

// This module is ported from Haskell Data.Set by László Domoszlai. 2013.sep.6

/*-----------------------------------------------------------------------------
 * |
 * Module      :  Data.Set
 * Copyright   :  (c) Daan Leijen 2002
 * License     :  BSD-style
 * Maintainer  :  libraries@haskell.org
 * Stability   :  provisional
 * Portability :  portable

 * An efficient implementation of sets.
 *
 * Since many function names (but not the type name) clash with
 * "Prelude" names, this module is usually imported @qualified@, e.g.
 *
 * >  import Data.Set (Set)
 * >  import qualified Data.Set as Set
 *
 * The implementation of 'Set' is based on /size balanced/ binary trees (or
 * trees of /bounded balance/) as described by:
 *
 *    * Stephen Adams, \"/Efficient sets: a balancing act/\",
 *	Journal of Functional Programming 3(4):553-562, October 1993,
 *	<http://www.swiss.ai.mit.edu/~adams/BB/>.
 *
 *    * J. Nievergelt and E.M. Reingold,
 *	\"/Binary search trees of bounded balance/\",
 *	SIAM journal of computing 2(1), March 1973.
 *
 * Note that the implementation is /left-biased/ -- the elements of a
 * first argument are always preferred to the second, for example in
 * 'union' or 'insert'.  Of course, left-biasing can only be observed
 * when equality is an equivalence relation instead of structural
 * equality.
 *---------------------------------------------------------------------------*/

:: Set a

// Is this the empty set?
null :: (Set a) -> Bool
// The number of elements in the set.
size :: (Set a) -> Int
// Is the element in the set?
member    :: a (Set a) -> Bool | < a & == a
notMember :: a (Set a) -> Bool | < a & == a
// Is this a subset?
isSubsetOf :: (Set a) (Set a) -> Bool | < a & == a
// Is this a proper subset? (ie. a subset but not equal).
isProperSubsetOf :: (Set a) (Set a) -> Bool | < a & == a

// The empty set.
newSet :: Set a
// Create a singleton set.
singleton :: a -> Set a
// Insert an element in a set.
// If the set already contains an element equal to the given value,
// it is replaced with the new value.
insert :: a (Set a) -> Set a | < a & == a
// Delete an element from a set.
delete :: a (Set a) -> Set a | < a & == a

// The minimal element of a set.
findMin :: (Set a) -> a
// The maximal element of a set.
findMax :: (Set a) -> a
// Delete the minimal element.
deleteMin :: (Set a) -> Set a
// Delete the maximal element.
deleteMax :: (Set a) -> Set a
// deleteFindMin set = (findMin set, deleteMin set)
deleteFindMin :: (Set a) -> (a,Set a)
// deleteFindMax set = (findMax set, deleteMax set)
deleteFindMax :: (Set a) -> (a,Set a)
// Retrieves the minimal key of the set, and the set
// stripped of that element, or 'Nothing' if passed an empty set.
minView :: (Set a) -> Maybe (a, Set a)
// Retrieves the maximal key of the set, and the set
// stripped of that element, or 'Nothing' if passed an empty set.
maxView :: (Set a) -> Maybe (a, Set a)

// The union of two sets, preferring the first set when
// equal elements are encountered.
// The implementation uses the efficient /hedge-union/ algorithm.
// Hedge-union is more efficient on (bigset `union` smallset).
union :: (Set a) (Set a) -> Set a | < a & == a
// The union of a list of sets: (@'unions' == 'foldl' 'union' 'empty'@).
unions :: [Set a] -> Set a | < a & == a
// Difference of two sets. 
difference :: (Set a) (Set a) -> Set a | < a & == a
// The intersection of two sets.
// Elements of the result come from the first set
intersection :: (Set a) (Set a) -> Set a | < a & == a

// Filter all elements that satisfy the predicate.
filter :: (a -> Bool) (Set a) -> Set a | < a & == a
// Partition the set into two sets, one with all elements that satisfy
// the predicate and one with all elements that don't satisfy the predicate.
partition :: (a -> Bool) (Set a) -> (Set a,Set a) | < a & == a
// The expression (@'split' x set@) is a pair @(set1,set2)@
// where @set1@ comprises the elements of @set@ less than @x@ and @set2@
// comprises the elements of @set@ greater than @x@.
split :: a (Set a) -> (Set a,Set a) | < a & == a
// Performs a 'split' but also returns whether the pivot
// element was found in the original set.
splitMember :: a (Set a) -> (Set a,Bool,Set a) | < a & == a

// Convert the set to an ascending list of elements.
toList :: (Set a) -> [a]
// Create a set from a list of elements.
fromList :: [a] -> Set a | < a & == a

