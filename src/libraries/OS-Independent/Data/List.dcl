definition module Data.List

from Data.Functor import class Functor
from Data.Maybe import :: Maybe
import StdList, GenEq

cons            :: u:a v:[u:a] -> w:[u:a], [w <= u,v <= w]
singleton       :: .a -> [.a]
head            :: ![.a] -> .a
headDef         :: a [a] -> a
tail            :: !u:[.a] -> u:[.a]
isnull          :: ![.a] -> Bool
keep            :: Int [a] -> [a]
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
levenshtein     :: .[a] .[a] -> Int | == a
elem            :: a .[a] -> .Bool | == a
notElem         :: a .[a] -> .Bool | == a
lookup          :: a [(a,.b)] -> Maybe .b | == a
find            :: (a -> .Bool) -> .(.[a] -> .(Maybe a))
partition       :: !(a -> .Bool) !.[a] -> (!.[a], !.[a])
elemIndex       :: a -> .(.[a] -> .(Maybe Int)) | == a
elemIndices     :: a -> .(.[a] -> .[Int]) | == a
findIndex       :: (.a -> .Bool) -> .([.a] -> .(Maybe Int))
findIndices     :: (.a -> .Bool) [.a] -> .[Int]
zip3            :: ![.a] [.b] [.c] -> [(.a,.b,.c)]
zip4            :: ![.a] [.b] [.c] [.d] -> [(.a,.b,.c,.d)]
zip5            :: ![.a] [.b] [.c] [.d] [.e] -> [(.a,.b,.c,.d,.e)]
zipSt           :: (.a -> .(.b -> (.st -> .st))) ![.a] [.b] .st -> .st
zipWith         :: (.a -> .(.b -> .h)) ![.a] [.b] -> [.h]
zipWithSt       :: (.a -> .(.b -> (.st -> .(.h, .st)))) ![.a] [.b] .st -> .([.h], .st)
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
differenceBy 	:: (a -> a -> .Bool) u:[a] .[a] -> v:[a], [u <= v]
intersect       :: u:(.[a] -> v:(.[a] -> .[a])) | == a, [v <= u]
intersectBy     :: (a -> b -> .Bool) .[a] .[b] -> .[a]
union           :: u:(.[a] -> v:(.[a] -> .[a])) | == a, [v <= u]
unionBy         :: (a -> .(a -> .Bool)) .[a] .[a] -> .[a]

isMemberGen :: !a !.[a] -> Bool | gEq{|*|} a

strictFoldr         :: !(.a -> .(.b -> .b)) !.b ![.a] -> .b
strictFoldl         :: !(.a -> .(.b -> .a)) !.a ![.b] -> .a
strictTRMapRev      :: !(.a -> .b) ![.a] -> [.b]
strictTRMapAcc      :: !(u:a -> v:b) !w:[u:a] !x:[v:b] -> y:[v:b], [w <= u,y <= v,x <= y]
strictTRMap         :: !(.a -> .b) ![.a] -> [.b]
reverseTR           :: ![.a] -> [.a]
flattenTR           :: ![[a]] -> [a]
strictFoldrSt       :: !(.a -> .(.b *st -> *(.b, *st))) !.b ![.a] *st -> *(.b, *st)
strictFoldlSt       :: !(.a -> .(.b *st -> *(.a, *st))) !.a ![.b] *st -> *(.a, *st)
strictTRMapSt       :: !(a .st -> (!b, !.st)) ![a] !.st -> (![b], !.st)
strictTRMapStAcc    :: !(a .st -> (!b, !.st)) ![a] ![b] !.st -> (![b], !.st)
strictTRZipWith     :: !(a b -> c) ![a] ![b] -> [c]
strictTRZipWithRev  :: !(a b -> c) ![a] ![b] -> [c]
strictTRZipWithAcc  :: !(a b -> c) ![a] ![b] ![c] -> [c]
strictTRZip4        :: ![a] ![b] ![c] ![d] -> [(!a, !b, !c, !d)]
strictTRZip4Rev     :: ![a] ![b] ![c] ![d] -> [(!a, !b, !c, !d)]
strictTRZip4Acc     :: ![a] ![b] ![c] ![d] ![(!a, !b, !c, !d)] -> [(!a, !b, !c, !d)]
strictTRZip2        :: ![a] ![b]-> [(!a, !b)]
strictTRZip2Rev     :: ![a] ![b]-> [(!a, !b)]
strictTRZip2Acc     :: ![a] ![b] ![(!a, !b)] -> [(!a, !b)]
strictTRZipWith3    :: !(a b c -> d) ![a] ![b] ![c] -> [d]
strictTRZipWith3Rev :: !(a b c -> d) ![a] ![b] ![c] -> [d]
strictTRZipWith3Acc :: !(a b c -> d) ![a] ![b] ![c] ![d] -> [d]

instance Functor []

//null :: [.a] -> Bool
null xs :== isnull xs

