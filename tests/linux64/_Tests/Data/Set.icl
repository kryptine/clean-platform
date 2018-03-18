module _Tests.Data.Set

/**
 * Some tests are taken from Peter Achten's StdSetTest in
 * https://gitlab.science.ru.nl/peter88/FP_Example_Solutions/.
 */

import StdBool
from StdFunc import const, flip, o
import StdList
import qualified StdList
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

derive bimap []

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
	, EP filter
	, EP partition
	, EP split
	, EP splitMember
	]
	w

:: Elem :== Char

:: Predicate a
	= ConstTrue
	| IsMember [a]

pred :: (Predicate a) -> a -> Bool | Eq a
pred ConstTrue     = const True
pred (IsMember cs) = flip isMember cs

derive ggen Predicate
derive genShow Predicate
derive JSONEncode Predicate

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

filter :: (Predicate Elem) [Elem] -> Property
filter p xs = sort ('S'.toList ('S'.filter (pred p) ('S'.fromList xs))) =.= sort (removeDup ('StdList'.filter (pred p) xs))

partition :: (Predicate Elem) [Elem] -> Property
partition p xs =
	all p` true` /\               // Right split
	all (not o p`) false` /\
	all (flip isMember xs) xs` /\ // No junk
	all (flip isMember xs`) xs /\ // All members used
	no_duplicates true /\         // Data structure integrity
	no_duplicates false
where
	p` = pred p
	(true,false) = 'S'.partition p` ('S'.fromList xs)
	(true`,false`) = ('S'.toList true, 'S'.toList false)
	xs` = true` ++ false`

split :: Elem [Elem] -> Property
split p xs =
	all ((>) p) lt` /\                // Right split
	all ((<) p) gt` /\
	all (flip isMember xsminp) xs` /\ // No junk
	all (flip isMember xs`) xsminp /\ // All members used
	no_duplicates lt /\               // Data structure integrity
	no_duplicates gt
where
	xsminp = 'StdList'.filter ((<>) p) xs
	(lt,gt) = 'S'.split p ('S'.fromList xs)
	(lt`,gt`) = ('S'.toList lt, 'S'.toList gt)
	xs` = lt` ++ gt`

splitMember :: Elem [Elem] -> Property
splitMember p xs =
	all ((>) p) lt` /\                // Right split
	all ((<) p) gt` /\
	all (flip isMember xsminp) xs` /\ // No junk
	all (flip isMember xs`) xsminp /\ // All members used
	bool =.= isMember p xs /\         // Boolean is correct
	no_duplicates lt /\               // Data structure integrity
	no_duplicates gt
where
	xsminp = 'StdList'.filter ((<>) p) xs
	(lt,bool,gt) = 'S'.splitMember p ('S'.fromList xs)
	(lt`,gt`) = ('S'.toList lt, 'S'.toList gt)
	xs` = lt` ++ gt`

// Helpers
does_not_contain :: ('S'.Set Elem) [Elem] -> Bool
does_not_contain d ys = all (flip 'S'.notMember d) ys

contains :: ('S'.Set Elem) [Elem] -> Bool
contains d xs = all (flip 'S'.member d) xs

all_in :: ('S'.Set Elem) [Elem] -> Bool
all_in s xs = all (flip isMember xs) ('S'.toList s)

no_duplicates :: ('S'.Set Elem) -> Property
no_duplicates s = xs =.= removeDup xs where xs = 'S'.toList s
