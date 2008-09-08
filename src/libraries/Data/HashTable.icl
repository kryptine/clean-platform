implementation module HashTable
/**
* THIS IMPLEMENTATION IS NOT A REAL HASHTABLE!!!
*
* It is just a stub implemented with a list of pairs and O(n) search.
* This module allows the use of the HashTable interface already, until
* someone finds the time to implement a nice efficient hashtable.
*/
import StdOverloaded, StdInt, StdString, StdList
import StdMaybe

:: HashTable k v	= HashTable [(k,v)]

class hash k
where
	hash :: k -> Int

instance hash Int
where
	hash _	= 0

instance hash String
where
	hash _	= 0

newHashTable :: 						   HashTable k v	| == k & hash k
newHashTable = HashTable []

get :: !k (HashTable k v)				-> Maybe v			| == k & hash k
get k (HashTable [])					= Nothing
get k (HashTable [(xk,xv):xs])
	| k == xk							= Just xv
										= get k (HashTable xs)

put :: !k !v (HashTable k v)			-> HashTable k v	| == k & hash k
put k v (HashTable [])					= (HashTable [(k,v)])
put k v (HashTable [(xk,xv):xs])			
	| k == xk							= (HashTable [(k,v):xs])
										= case put k v (HashTable xs) of (HashTable ps) = HashTable [(xk,xv):ps]

del :: !k (HashTable k v)				-> HashTable k v	| == k & hash k
del k (HashTable [])					= (HashTable [])
del k (HashTable [(xk,xv):xs])
	| k == xk							= HashTable xs
										= case del k (HashTable xs) of (HashTable ds) = HashTable [(xk,xv):ds]

keys :: !(HashTable k v)				-> [k]
keys (HashTable [])						= []
keys (HashTable [(xk,xv):xs])			= [xk:keys (HashTable xs)]

containsKey :: !k (HashTable k v)		-> Bool				| == k & hash k
containsKey k (HashTable [])			= False
containsKey k (HashTable [(xk,xv):xs])
	| k == xk							= True
										= containsKey k (HashTable xs)

containsValue :: !v (HashTable k v)		-> Bool				| == v
containsValue v (HashTable [])			= False
containsValue v (HashTable [(xk,xv):xs])
	| v == xv							= True
										= containsValue v (HashTable xs)


toList :: !(HashTable k v)				-> [(k,v)]
toList (HashTable xs)					= xs

fromList :: ![(k,v)]					-> HashTable k v
fromList xs								= HashTable xs
