definition module Map
/**
* This module provides a dynamic Map type for creating mappings from keys to values
* Inernally it uses an AVL tree to organize the key-value pairs stored in the mapping
* such that lookup, insert and delete operations can be performed in O(log n).
*/

from Maybe			import :: Maybe
from StdClass		import class Eq, class Ord
from StdOverloaded	import class ==, class <

/**
* The abstract Map type provides the mapping.
* The parameter k is the key type on which the data structure
* is indexed. The parameter v is the type of the values
* stored in the mapping. For example "Map Int String" is a mapping
* "from" integers "to" strings.
*/
:: Map k v

//Basic functions

/**
* Create an empty Map
*
* @return An empty map
*/
empty		::									.(Map k v)
/**
* Adds or replaces the value for a given key.
*
* @param The key value to add/update
* @param The value to add/update at the key position
* @param The original mapping
* @return The modified mapping with the added value
*/
put			:: k v 	(Map k v)	-> (			Map k v)	| Eq k & Ord k
/**
* Searches for a value at a given key position.
*
* @param The key to look for
* @param The orginal mapping
* @return When found, the value at the key position, if not: Nothing
* @return The original mapping (to enable unique maps)
*/
get			:: k	(Map k v)	-> (Maybe v,	(Map k v))	| Eq k & Ord k
/**
* Removes the value at a given key position.
*
* @param The key to remove
* @param The original mapping
* @return The modified mapping with the value/key removed
*/
del			:: k	(Map k v)	-> (			Map k v)	| Eq k & Ord k

//Conversion functions

/**
* Converts a mapping to a list of key value pairs.
* Because of the internal ordering of the mapping the resulting
* list is sorted ascending on the key part of the tuple.
*
* @param The original mapping
* @return A list of key/value tuples in the mapping
*/
toList		:: 		(Map k v)	-> [(k,v)] 

/**
* Converts a list of key/value tuples to a mapping.
*
* @param A list of key/value tuples
* @return A mapping containing all the tuples in the list
*/
fromList	:: [(k,v)]			-> 			   (Map k v)	| Eq k & Ord k

