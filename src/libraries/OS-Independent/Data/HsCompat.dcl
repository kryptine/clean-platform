definition module HsCompat

/* 
 * Haskell compatibility library to be easier to port Haskell libraries to Clean.
 *
 * It contains various basic Haskell function from Prelude which has no
 * equvalent (or has but its name is different) in Clean.
 *
 * 28th Feb 2013: add concatMap, lines, unlines, maximum, null
 *
 */

import StdEnv

// creates a list from a list generating function by application of this function on all elements in a list passed as the second argument
concatMap :: (a -> [b]) [a] -> [b]

// creates a string from an array of strings, it inserts new line characters between original strings
unlines :: [String] -> String

// creates an array of string from the original one, new line characters serving as separators 
lines :: String -> [String]

// returns the maximum value from the list 
maximum :: [a] -> a | Ord a

// returns True if a list is empty, otherwise False 
null :: [a] -> Bool
