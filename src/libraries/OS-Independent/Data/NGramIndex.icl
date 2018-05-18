implementation module Data.NGramIndex

import _SystemArray
import StdChar
from StdFunc import flip, o
import StdInt
from StdList import filter, flatten, isMember, map, removeDup, span, take, ++,
	instance length [], instance == [a], instance < [a], instance fromString [Char]
import StdOrdList

from Data.Func import $
from Data.List import concatMap, tails
import Data.Map
import Data.Maybe
import Data.Monoid

newNGramIndex :: !Int !Bool -> NGramIndex v
newNGramIndex n ci = {n=n, ci=ci, idx=newMap}

ngramSize :: !(NGramIndex v) -> Int
ngramSize ngi = mapSize ngi.idx

index :: !String !v !(NGramIndex v) -> NGramIndex v | Eq v
index s v ngi=:{n,ci,idx} = {ngi & idx=foldr add idx (ngrams` ci n s)}
where
	add = alter \vs -> Just case vs of
		Nothing -> [v]
		Just vs -> if (isMember v vs) vs [v:vs]
	ngrams` ci n s = flatten [ngrams ci i s \\ i <- [1..n]]

search :: !String !(NGramIndex v) -> [(v,Int)] | Eq, Ord v
search s {n,ci,idx} = count
	$ foldr merge []
	$ map (fromMaybe [] o flip get idx)
	$ if (size s >= n) (ngrams ci n s) [map toLower $ fromString s]
where
	count :: [v] -> [(v,Int)] | == v
	count [] = []
	count [x:xs] = [(x,length yes + 1):count no]
	where
		(yes,no) = span ((==) x) xs

ngrams :: !Bool !Int !String -> [[Char]]
ngrams ci n s = removeDup
	$ filter ((==) n o length)
	$ map (take n)
	$ tails
	$ if ci (map toLower) id
	$ fromString s
