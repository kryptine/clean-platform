definition module Data.List

from Data.Functor import class Functor
from Data.Maybe import :: Maybe
import StdList

head            :: ![.a] -> .a
tail            :: !u:[.a] -> u:[.a]
null            :: ![.a] -> Bool
unzip3          :: ![(.a,.b,.c)] -> ([.a],[.b],[.c])
unzip4          :: ![(.a,.b,.c,.d)] -> ([.a],[.b],[.c],[.d])
unzip5          :: ![(.a,.b,.c,.d,.e)] -> ([.a],[.b],[.c],[.d],[.e])
replaceInList   :: !(a a -> Bool) !a ![a] -> [a]
splitWith       :: !(a -> Bool) ![a] -> (![a],![a])
sortByIndex     :: ![(!Int,!a)] -> [a]
intersperse     :: !a ![a] -> [a]
intercalate     :: .[a] [.[a]] -> .[a]
transpose       :: ![[a]] -> [.[a]]
subsequences    :: .[a] -> .[[a]]
permutations    :: [a] -> .[[a]]
foldl1          :: (.a -> .(.a -> .a)) [.a] -> .a
concatMap       :: (.a -> [.b]) [.a] -> [.b]
maximum         :: .[a] -> a | < a
minimum         :: .[a] -> a | Ord a
getItems        :: ![a] ![Int] -> [a]
scanl           :: (a -> .(.b -> a)) a [.b] -> .[a]
scanl1          :: (a -> .(a -> a)) .[a] -> .[a]
foldr1          :: (.a -> .(.a -> .a)) [.a] -> .a
replicate       :: .Int a -> .[a]
cycle           :: .[a] -> [a]
unfoldr         :: (.a -> Maybe (.b,.a)) .a -> [.b]
break           :: (a -> .Bool) .[a] -> .([a],[a])
stripPrefix     :: .[a] u:[a] -> Maybe v:[a] | == a, [u <= v]
group           :: .(.[a] -> [.[a]]) | == a
groupBy         :: (a -> a -> .Bool) .[a] -> [.[a]]
inits           :: .[a] -> [.[a]]
tails           :: [a] -> .[[a]]
isPrefixOf      :: .[a] .[a] -> .Bool | == a
isSuffixOf      :: .[a] .[a] -> .Bool | == a
isInfixOf       :: .[a] .[a] -> Bool | == a
elem            :: a .[a] -> .Bool | == a
notElem         :: a .[a] -> .Bool | == a
lookup          :: a [(a,.b)] -> Maybe .b | == a
find            :: (a -> .Bool) -> .(.[a] -> .(Maybe a))
partition       :: (a -> .Bool) .[a] -> (.[a],.[a])
elemIndex       :: a -> .(.[a] -> .(Maybe Int)) | == a
elemIndices     :: a -> .(.[a] -> .[Int]) | == a
findIndex       :: (.a -> .Bool) -> .([.a] -> .(Maybe Int))
findIndices     :: (.a -> .Bool) [.a] -> .[Int]
zip3            :: ![.a] [.b] [.c] -> [(.a,.b,.c)]
zip4            :: ![.a] [.b] [.c] [.d] -> [(.a,.b,.c,.d)]
zip5            :: ![.a] [.b] [.c] [.d] [.e] -> [(.a,.b,.c,.d,.e)]
zipWith         :: (.a -> .(.b -> .h)) ![.a] [.b] -> [.h]
zipWith3        :: (.a -> .(.b -> .(.c -> .h))) ![.a] [.b] [.c] -> [.h]
zipWith4        :: (.a -> .(.b -> .(.c -> .(.d -> .h)))) ![.a] [.b] [.c] [.d] -> [.h]
zipWith5        :: (.a -> .(.b -> .(.c -> .(.d -> .(.e -> .h)))))
                   ![.a] [.b] [.c] [.d] [.e] -> [.h]
nub             :: .[a] -> .[a] | == a
nubBy           :: (a -> .(a -> .Bool)) .[a] -> .[a]
elem_by         :: (a -> .(.b -> .Bool)) a [.b] -> .Bool
delete          :: u:(a -> v:(w:[a] -> x:[a])) | == a, [v <= u,w <= x]
deleteBy        :: (a -> .(b -> .Bool)) a u:[b] -> v:[b], [u <= v]
deleteFirstsBy  :: (a -> .(b -> .Bool)) -> u:(v:[b] -> w:(.[a] -> x:[b])), [w <= u,w v <= x]
difference      :: u:(v:[a] -> w:(.[a] -> x:[a])) | == a, [w <= u,w v <= x]
intersect       :: u:(.[a] -> v:(.[a] -> .[a])) | == a, [v <= u]
intersectBy     :: (a -> b -> .Bool) .[a] .[b] -> .[a]
union           :: u:(.[a] -> v:(.[a] -> .[a])) | == a, [v <= u]
unionBy         :: (a -> .(a -> .Bool)) .[a] .[a] -> .[a]


instance Functor []
