module _Tests.Data.Set

/**
 * Many tests are taken from Peter Achten's StdSetTest in
 * https://gitlab.science.ru.nl/peter88/FP_Example_Solutions/.
 */

import StdBool
from StdFunc import flip
import StdList
import StdOrdList
import StdString
import StdTuple

import Data.Generics.GenLexOrd
from Data.Set import :: Set, instance == (Set a)
import qualified Data.Set as S

import Gast
import Gast.CommandLine

derive genShow Maybe, Set
derive JSONEncode Set

Start w = exposeProperties [Quiet, Tests 500000]
	[ EP member
	, EP fromList_toList
	, EP size
	, EP isSubsetOf
	, EP isProperSubsetOf
	, EP newSet_null
	, EP null
	, EP insert_member
	, EP delete_notMember
	, EP findMin
	, EP findMax
	, EP deleteMin
	, EP deleteMax
	, EP deleteFindMin
	, EP deleteFindMax
	, EP minView
	, EP maxView
	, EP union
	]
	w

:: Elem :== Char

derive bimap []

member :: Elem [Elem] -> Property
member x xs = 'S'.member x ('S'.fromList xs) <==> isMember x xs

fromList_toList :: [Elem] -> Bool
fromList_toList xs = xs` == 'S'.fromList ('S'.toList xs`)
where xs` = 'S'.fromList xs

size :: [Elem] -> Bool
size xs = 'S'.size ('S'.fromList xs) == length (removeDup xs)

isSubsetOf :: [Elem] [Elem] -> Property
isSubsetOf xs ys = 'S'.isSubsetOf ('S'.fromList xs) ('S'.fromList ys)
	<==> all (flip isMember ys) xs

isProperSubsetOf :: [Elem] [Elem] -> Property
isProperSubsetOf xs ys = 'S'.isProperSubsetOf ('S'.fromList xs) ('S'.fromList ys)
	<==> all (flip isMember ys) xs && not (all (flip isMember xs) ys)

newSet_null :: Property
newSet_null = name "newSet_null" ('S'.null 'S'.newSet)

null :: [Elem] -> Property
null xs =
	('S'.size xs` == 0 <==> 'S'.null xs`) /\
	(xs` == 'S'.newSet <==> 'S'.null xs`)
where xs` = 'S'.fromList xs

insert_member :: Elem [Elem] -> Bool
insert_member x xs = 'S'.member x ('S'.insert x ('S'.fromList xs))

delete_notMember :: Elem [Elem] -> Bool
delete_notMember x xs = 'S'.notMember x ('S'.delete x ('S'.fromList xs))

findMin :: [Elem] -> Property
findMin xs = not (isEmpty xs) ==>
	minList xs == 'S'.findMin ('S'.fromList xs)

findMax :: [Elem] -> Property
findMax xs = not (isEmpty xs) ==>
	maxList xs == 'S'.findMax ('S'.fromList xs)

deleteMin :: [Elem] -> Property
deleteMin xs = not (isEmpty xs) ==>
	'S'.notMember (minList xs) ('S'.deleteMin ('S'.fromList xs))

deleteMax :: [Elem] -> Property
deleteMax xs = not (isEmpty xs) ==>
	'S'.notMember (maxList xs) ('S'.deleteMax ('S'.fromList xs))

deleteFindMin :: [Elem] -> Property
deleteFindMin xs = not (isEmpty xs) ==>
	let (m,xs`) = 'S'.deleteFindMin ('S'.fromList xs) in
	m =.= min /\ 'S'.notMember min xs`
where min = minList xs

deleteFindMax :: [Elem] -> Property
deleteFindMax xs = not (isEmpty xs) ==>
	let (m,xs`) = 'S'.deleteFindMax ('S'.fromList xs) in
	m =.= max /\ 'S'.notMember max xs`
where max = maxList xs

minView :: [Elem] -> Property
minView xs = case xs of
	[] -> Nothing =.= 'S'.minView xs`
	xs -> Just ('S'.deleteFindMin xs`) =.= 'S'.minView xs`
where xs` = 'S'.fromList xs

maxView :: [Elem] -> Property
maxView xs = case xs of
	[] -> Nothing =.= 'S'.maxView xs`
	xs -> Just ('S'.deleteFindMax xs`) =.= 'S'.maxView xs`
where xs` = 'S'.fromList xs

union :: [Elem] [Elem] -> Property
union xs ys =
	all (flip 'S'.member u) (xs ++ ys) /\
	all (\x -> isMember x xs || isMember x ys) ('S'.toList u)
where u = 'S'.union ('S'.fromList xs) ('S'.fromList ys)
