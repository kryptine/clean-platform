definition module HsCompat

/* 
 * Haskell compatibility library to be easier to port Haskell libraries to Clean.
 *
 * It contains various basic Haskell function from Prelude which has no
 * equvalent (or has but its name is different) in Clean.
 *
 * 04th March 2013: add elemIndex, elemIndices, find, findIndex, findIndices,
 *                      intersperse, transpose, partition,
 *                      group, groupBy, inits, tails,
 *                      isPrefixOf, isSuffixOf,
 *                      zip3, zip4, zip5, zipWith,
 *                      zipWith3, zipWith4, zipWith5,
 *                      unzip3, unzip4, unzip5
 * 
 * 28th Feb 2013: add concatMap, lines, unlines, maximum, null
 *
 */

import StdEnv, Maybe

// creates a list from a list generating function by application of this function on all elements in a list passed as the second argument
concatMap :: (a -> [b]) [a] -> [b]

// returns the maximum value from the list 
maximum :: [a] -> a | Ord a

// returns True if a list is empty, otherwise False 
null :: [a] -> Bool

elemIndex :: a ![a] -> .Maybe Int | == a
elemIndices :: a ![a] -> .[Int] | == a
find :: (a -> Bool) ![a] -> .Maybe a
findIndex :: (a -> Bool) ![a] -> .Maybe Int
findIndices :: (a -> Bool) ![a] -> .[Int]

intersperse :: a ![a] -> .[a]
transpose :: ![[a]] -> [.[a]]
partition :: (a -> Bool) !.[a] -> (.[a],.[a])

group :: ![a] -> [.[a]] | == a
groupBy :: (a a -> Bool) ![a] -> [.[a]]

inits :: ![a] -> [.[a]]
tails :: ![a] -> .[[a]]

isPrefixOf :: ![a] ![a] -> Bool | == a
isSuffixOf :: ![a] ![a] -> Bool | == a

zip3 :: ![.a] [.b] [.c] -> [(.a,.b,.c)]
zip4 :: ![.a] [.b] [.c] [.d] -> [(.a,.b,.c,.d)]
zip5 :: ![.a] [.b] [.c] [.d] [.e] -> [(.a,.b,.c,.d,.e)]

zipWith :: (.a -> .(.b -> .h)) ![.a] [.b] -> [.h]
zipWith3 :: (.a -> .(.b -> .(.c -> .h))) ![.a] [.b] [.c] -> [.h]
zipWith4 :: (.a -> .(.b -> .(.c -> .(.d -> .h)))) ![.a] [.b] [.c] [.d] -> [.h]
zipWith5 :: (.a -> .(.b -> .(.c -> .(.d -> .(.e -> .h))))) ![.a] [.b] [.c] [.d] [.e] -> [.h]

unzip3 :: ![(.a,.b,.c)] -> ([.a],[.b],[.c])
unzip4 :: ![(.a,.b,.c,.d)] -> ([.a],[.b],[.c],[.d])
unzip5 :: ![(.a,.b,.c,.d,.e)] -> ([.a],[.b],[.c],[.d],[.e])

