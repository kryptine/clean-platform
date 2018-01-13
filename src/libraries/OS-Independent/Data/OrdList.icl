implementation module Data.OrdList

import StdList, StdOrdList

removeMembersSortedList :: ![a] ![a] -> [a] | Eq, Ord a
removeMembersSortedList [] ys
	= []
removeMembersSortedList xs []
	= xs
removeMembersSortedList [x:xs] [y:ys]
| x  < y    = [x : removeMembersSortedList xs [y:ys]]
| x == y    = removeMembersSortedList xs ys
| otherwise = removeMembersSortedList [x:xs] ys

removeDupSortedList :: ![a] -> [a] | Eq a
removeDupSortedList []
	= []
removeDupSortedList [x:xs]
	= [x : removeDupSortedList (dropWhile ((==) x) xs)]

