implementation module HsCompat

import StdEnv, Text

concatMap :: (a -> [b]) [a] -> [b]
concatMap f ls = flatten (map f ls)

unlines :: [String] -> String
unlines [] = ""
unlines [s] = s
unlines [s:ss] = s +++ "\n" +++ unlines ss

lines :: String -> [String]
lines s = split "\n" s

maximum :: [a] -> a | Ord a
maximum [x] = x
maximum [x:xs] = max x (maximum xs)

null :: [a] -> Bool
null ls = isEmpty ls

