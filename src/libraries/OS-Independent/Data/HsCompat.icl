implementation module Data.HsCompat

import StdEnv

concat :: ![[.a]] -> [.a]	
concat xs = flatten xs

