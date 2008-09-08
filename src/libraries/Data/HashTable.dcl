definition module HashTable

import StdMaybe
import StdOverloaded

:: HashTable k v

class hash k where
	hash :: k -> Int

instance hash Int
instance hash String

newHashTable :: 						   HashTable k v	| == k & hash k
get :: !k (HashTable k v)				-> Maybe v			| == k & hash k
put :: !k !v (HashTable k v)			-> HashTable k v	| == k & hash k
del :: !k (HashTable k v)				-> HashTable k v	| == k & hash k

keys :: !(HashTable k v)				-> [k]

containsKey :: !k (HashTable k v)		-> Bool				| == k & hash k
containsValue :: !v (HashTable k v)		-> Bool				| == v

toList :: !(HashTable k v)				-> [(k,v)]
fromList :: ![(k,v)]					-> HashTable k v
