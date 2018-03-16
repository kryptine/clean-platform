module _Tests.Data.Set

/**
 * Some tests are taken from Peter Achten's StdSetTest in
 * https://gitlab.science.ru.nl/peter88/FP_Example_Solutions/.
 */

import StdBool
from StdFunc import flip
import StdList
import StdOrdList
import StdString
import StdTuple

from Data.Func import on, `on`
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
	, EP difference
	, EP intersection
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
	check contains u (xs ++ ys) /\ // No missing elements
	check all_in u (xs ++ ys) /\   // No junk
	no_duplicates u                // Data structure integrity
where u = 'S'.union ('S'.fromList xs) ('S'.fromList ys)

difference :: [Elem] [Elem] -> Property
difference xs ys =
	check does_not_contain d ys /\                           // No remaining elements
	check contains d [x \\ x <- xs | not (isMember x ys)] /\ // All good elements
	no_duplicates d                                          // Data structure integrity
where d = 'S'.difference ('S'.fromList xs) ('S'.fromList ys)

intersection :: [Elem] [Elem] -> Property
intersection xs ys =
	check does_not_contain i [x \\ x <- xs | not (isMember x ys)] /\ // No junk
	check does_not_contain i [y \\ y <- ys | not (isMember y xs)] /\ // No junk
	check contains i [x \\ x <- xs | isMember x ys] /\               // All good elements
	no_duplicates i                                                  // Data structure integrity
where i = 'S'.intersection ('S'.fromList xs) ('S'.fromList ys)

// Helpers
does_not_contain :: ('S'.Set Elem) [Elem] -> Bool
does_not_contain d ys = all (flip 'S'.notMember d) ys

contains :: ('S'.Set Elem) [Elem] -> Bool
contains d xs = all (flip 'S'.member d) xs

all_in :: ('S'.Set Elem) [Elem] -> Bool
all_in s xs = all (flip isMember xs) ('S'.toList s)

no_duplicates :: ('S'.Set Elem) -> Property
no_duplicates s = xs =.= removeDup xs where xs = 'S'.toList s
