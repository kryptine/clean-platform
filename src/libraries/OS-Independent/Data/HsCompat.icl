implementation module HsCompat

import StdEnv

concat :: ![[.a]] -> [.a]	
concat xs = flatten xs

