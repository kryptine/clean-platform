definition module Data.IntMap.Strict

from Data.Maybe		import :: Maybe (..)
from StdClass		import class Eq, class Ord
from StdOverloaded	import class ==, class <
from StdBool        import not
from StdFunc        import id
from Text.JSON      import generic JSONEncode, generic JSONDecode, :: JSONNode
from GenEq import generic gEq
from Data.Monoid    import class Monoid, class Semigroup
import qualified StdList as SL
from Data.List import foldr
from Data.Functor import class Functor (..)
from Data.IntMap.Base import :: IntMap (..), :: Mask, :: Prefix

null :: !(IntMap a) -> Bool

/**
* Create an empty Map
*
* @return An empty map
*/
newMap      :: w:(IntMap u:v), [ w <= u]

singleton   :: !Int !.a -> .(IntMap .a)

mapSize     :: !(IntMap v) -> Int

/**
* Adds or replaces the value for a given key.
*
* @param The key value to add/update
* @param The value to add/update at the key position
* @param The original mapping
* @return The modified mapping with the added value
*/
put :: !Int !u:a !v:(IntMap u:a) -> w:(IntMap u:a), [w <= u,v <= w]
/**
* Searches for a value at a given key position. Works only for non-unique
* mappings.
*
* @param The key to look for
* @param The orginal mapping
* @return When found, the value at the key position, if not: Nothing
*/

get :: !Int !.(IntMap .a) -> Maybe .a

getU :: !Int !u:(IntMap a) -> v:(!w:(Maybe a), !x:(IntMap a)), [v <= w,v u <= x]

/**
* Removes the value at a given key position. The mapping itself can be spine unique.
*
* @param The key to remove
* @param The original mapping
* @return The modified mapping with the value/key removed
*/
del :: !Int !(IntMap a) -> IntMap a

keys  :: !(IntMap a) -> [Int]
elems :: !(IntMap a) -> [a]

derive JSONEncode IntMap
derive JSONDecode IntMap
derive gEq IntMap

member :: !Int !(IntMap a) -> Bool

notMember k m :== not (member k m)

find :: !Int !(IntMap a) -> a

findWithDefault :: a !Int !(IntMap a) -> a

alter :: !((Maybe a) -> Maybe a) !Int !(IntMap a) -> IntMap a

unionsWith :: !(a a -> a) ![IntMap a] -> IntMap a

mergeWithKey :: !(Int a b -> Maybe c) !((IntMap a) -> IntMap c) !((IntMap b) -> IntMap c)
             !(IntMap a) !(IntMap b) -> IntMap c

foldlStrict :: !(a b -> a) !a ![b] -> a

foldrWithKey :: !(Int a .b -> .b) !.b !.(IntMap a) -> .b

union :: !(IntMap a) !(IntMap a) -> IntMap a

unions :: ![IntMap a] -> IntMap a

instance Functor IntMap

toList :: !(IntMap a) -> [(!Int, !a)]

toAscList :: !(IntMap a) -> [(!Int, !a)]
