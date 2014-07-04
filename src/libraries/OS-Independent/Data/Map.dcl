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
from GenLexOrd import generic gLexOrd, :: LexOrd

/**
* The abstract Map type provides the mapping.
* The parameter k is the key type on which the data structure
* is indexed. The parameter v is the type of the values
* stored in the mapping. For example "Map Int String" is a mapping
* "from" integers "to" strings.
*/
:: Map k v	= MLeaf
            | MNode !(Map k v) !k !Int v !(Map k v)

//Basic functions

/**
* Create an empty Map
*
* @return An empty map
*/
newMap		:: w:(Map k u:v), [ w <= u]

singleton   :: k v -> Map k v | Eq k & Ord k

empty       :: (Map k v) -> Bool

/**
* Adds or replaces the value for a given key.
*
* @param The key value to add/update
* @param The value to add/update at the key position
* @param The original mapping
* @return The modified mapping with the added value
*/
put 		:: !k u:v !w:(Map k u:v) -> x:(Map k u:v) | Eq k & Ord k, [ w x <= u, w <= x]
/**
* Searches for a value at a given key position. Works only for non-unique
* mappings.
*
* @param The key to look for
* @param The orginal mapping
* @return When found, the value at the key position, if not: Nothing
*/
get			:: !k !(Map k v) -> Maybe v | Eq k & Ord k
/**
* Searches for a value at a given key position and returns the mapping
* as a result as well. This makes it possible to have use mappings with a unique spine
*
* @param The key to look for
* @param The orginal mapping
* @return When found, the value at the key position, if not: Nothing
* @return The original mapping (to enable Maps wth a unique spine, !but without unique values!)
*/
getU		:: !k !w:(Map k v) -> x:(Maybe v, !y:(Map k v)) | Eq k & Ord k, [ x <= y, w <= y ]
/**
* Removes the value at a given key position. The mapping itself can be spine unique.
*
* @param The key to remove
* @param The original mapping
* @return The modified mapping with the value/key removed
*/
del			:: !k !w:(Map k v) -> x:(Map k v) | Eq k & Ord k, [ w <= x]
/**
* Removes and returns the value at a given key position. Because the value is returned this
* makes it possible to store unique values in the mapping and safely remove them without losing
* their references.
*
* @param The key to remove
* @param The original mapping
* @return When found, the value removed at the key position, if not: Nothing
* @return The modified mapping with the value/key removed
*/
delU		:: !k !w:(Map k u:v) -> x:(Maybe u:v, !y:(Map k u:v)) | Eq k & Ord k, [ w y <= u, x <= y, w <= y]

foldrWithKey :: (k v u:a -> u:a) u:a (Map k v) -> u:a
foldrNoKey   :: (v u:a -> u:a) u:a (Map k v) -> u:a
foldlWithKey :: (u:a k v -> u:a) u:a (Map k v) -> u:a
foldlNoKey   :: (u:a v -> u:a) u:a (Map k v) -> u:a

keys  :: (Map k a) -> [k]
elems :: (Map k a) -> [a]

//Conversion functions

/**
* Converts a mapping to a list of key value pairs.
* Because of the internal ordering of the mapping the resulting
* list is sorted ascending on the key part of the tuple.
*
* @param The original mapping
* @return A list of key/value tuples in the mapping
*/
toList		:: 		!w:(Map k u:v)	-> x:[y:(!k,u:v)] , [w y <= u, x <= y, w <= x]

/**
* Converts a list of key/value tuples to a mapping.
*
* @param A list of key/value tuples
* @return A mapping containing all the tuples in the list
*/
fromList	:: !w:[x:(!k,u:v)]		-> y:(Map k u:v) | Eq k & Ord k, [x y <= u, w <= x, w <= y]

/**
* Adds or replaces a list of key/value pairs.
*
* @param A list of key/value tuples
* @param The original mapping
* @return The modified mapping with the added values
*/
putList		:: !w:[x:(!k,u:v)] !w:(Map k u:v) -> y:(Map k u:v) | Eq k & Ord k, [x y <= u, w <= x, w <= y]

/**
* Removes the values at given key positions. The mapping itself can be spine unique.
*
* @param The list of keys to remove
* @param The original mapping
* @return The modified mapping with the values/keys removed
*/
delList 	:: ![k] !w:(Map k u:v) -> y:(Map k u:v) | Eq k & Ord k, [w y <= u, w <= y]

derive JSONEncode Map
derive JSONDecode Map
derive gEq Map

// Generic Map functions
gPut      :: !k u:v !w:(Map k u:v) -> x:(Map k u:v) | gEq{|*|} k & gLexOrd{|*|} k, [w x <= u, w <= x]
gGet      :: !k !(Map k v) -> Maybe v | gEq{|*|} k & gLexOrd{|*|} k
gGetU     :: !k !w:(Map k v) -> x:(Maybe v,!y:(Map k v)) | gEq{|*|} k & gLexOrd{|*|} k, [ x <= y, w <= y]
gDel      :: !k !w:(Map k v) -> x:(Map k v) | gEq{|*|} k & gLexOrd{|*|} k, [ w <= x]
gDelU     :: !k !w:(Map k u:v) -> x:(Maybe u:v, !y:(Map k u:v)) | gEq{|*|} k & gLexOrd{|*|} k, [ w y <= u, x <= y, w <= y]
gFromList :: !w:[x:(!k,u:v)] -> y:(Map k u:v) | gEq{|*|} k & gLexOrd{|*|} k, [x y <= u, w <= x, w <= y]
gPutList  :: !w:[x:(!k,u:v)] !w:(Map k u:v) -> y:(Map k u:v) | gEq{|*|} k & gLexOrd{|*|} k, [x y <= u, w <= x, w <= y]
gDelList  :: ![k] !w:(Map k u:v) -> y:(Map k u:v) | gEq{|*|} k & gLexOrd{|*|} k, [w y <= u, w <= y]
