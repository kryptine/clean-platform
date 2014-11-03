definition module Data.Map
/**
* This module provides a dynamic Map type for creating mappings from keys to values
* Internally it uses an AVL tree to organize the key-value pairs stored in the mapping
* such that lookup, insert and delete operations can be performed in O(log n).
*/

from Data.Maybe		import :: Maybe
from StdClass		import class Eq, class Ord
from StdOverloaded	import class ==, class <
from Text.JSON      import generic JSONEncode, generic JSONDecode, :: JSONNode
from GenEq import generic gEq
from Data.Monoid    import class Monoid

/**
* The abstract Map type provides the mapping.
* The parameter k is the key type on which the data structure
* is indexed. The parameter v is the type of the values
* stored in the mapping. For example "Map Int String" is a mapping
* "from" integers "to" strings.
*/
//:: Map k v	= MLeaf
            //| MNode !(Map k v) !k !Int v !(Map k v)

:: Map k a
  = Bin !Size !k a !(Map k a) !(Map k a)
  | Tip

:: Size   :== Int

instance Monoid (Map k v) | < k

//Basic functions

null :: !(Map k a) -> Bool

/**
* Create an empty Map
*
* @return An empty map
*/
newMap      :: w:(Map k u:v), [ w <= u]

singleton   :: !k !a -> Map k a

mapSize     :: !(Map k v) -> Int

/**
* Adds or replaces the value for a given key.
*
* @param The key value to add/update
* @param The value to add/update at the key position
* @param The original mapping
* @return The modified mapping with the added value
*/
put :: !k !a !(Map k a) -> Map k a | < k
/**
* Searches for a value at a given key position. Works only for non-unique
* mappings.
*
* @param The key to look for
* @param The orginal mapping
* @return When found, the value at the key position, if not: Nothing
*/

get :: !k !(Map k a) -> Maybe a | < k

getU :: !k !w:(Map k v) -> x:(!Maybe v, !y:(Map k v)) | == k & < k, [ x <= y, w <= y]
/**
* Removes the value at a given key position. The mapping itself can be spine unique.
*
* @param The key to remove
* @param The original mapping
* @return The modified mapping with the value/key removed
*/
del :: !k !(Map k a) -> Map k a | < k

delU :: !a !.(Map a b) -> u:(!v:(Maybe b), !Map a b) | == a & < a, [u <= v] // !k !w:(Map k u:v) -> x:(Maybe u:v, !y:(Map k u:v)) | == k & < k, [ w y <= u, x <= y, w <= y]

foldrWithKey :: !(k v u:a -> u:a) !u:a !(Map k v) -> u:a
foldrNoKey   :: !(v u:a -> u:a) !u:a !(Map k v) -> u:a
foldlWithKey :: !(u:a k v -> u:a) !u:a !(Map k v) -> u:a
foldlNoKey   :: !(a -> b -> a) !a !(Map c b) -> a

filterWithKey :: !(k a -> Bool) !(Map k a) -> Map k a

keys  :: !(Map k a) -> [k]
elems :: !(Map k a) -> [a]

//Conversion functions

/**
* Converts a mapping to a list of key value pairs.
* Because of the internal ordering of the mapping the resulting
* list is sorted ascending on the key part of the tuple.
*
* @param The original mapping
* @return A list of key/value tuples in the mapping
*/
toList :: !(Map k a) -> [(!k, !a)]

/**
* Converts a list of key/value tuples to a mapping.
*
* @param A list of key/value tuples
* @return A mapping containing all the tuples in the list
*/
fromList :: !u:[v:(!a, !b)] -> Map a b | == a & < a, [u <= v]

/**
* Adds or replaces a list of key/value pairs.
*
* @param A list of key/value tuples
* @param The original mapping
* @return The modified mapping with the added values
*/
putList :: !u:[v:(!a, !b)] !u:(Map a b) -> Map a b | == a & < a, [u <= v]

/**
* Removes the values at given key positions. The mapping itself can be spine unique.
*
* @param The list of keys to remove
* @param The original mapping
* @return The modified mapping with the values/keys removed
*/
delList :: ![a] !.(Map a b) -> Map a b | == a & < a

derive JSONEncode Map
derive JSONDecode Map
derive gEq Map

member :: !k !(Map k a) -> Bool | < k

find :: !k !(Map k a) -> a | < k

findWithDefault :: !a !k !(Map k a) -> a | < k

alter :: !((Maybe a) -> Maybe a) !k !(Map k a) -> Map k a | < k

elemAt :: !Int !(Map k a) -> (!k, !a)

findMin :: !(Map k a) -> (!k, !a)

findMax :: !(Map k a) -> (!k, !a)

unions :: ![Map k a] -> Map k a | < k

unionsWith :: !(a a -> a) ![Map k a] -> Map k a | < k

union :: !(Map k a) !(Map k a) -> Map k a | < k
