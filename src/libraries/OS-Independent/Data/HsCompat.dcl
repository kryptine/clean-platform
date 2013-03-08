definition module HsCompat

/* 
 * Haskell compatibility library to be easier to port Haskell libraries to Clean.
 *
 * It contains various basic Haskell function from Prelude which has no
 * equvalent (or has but its name is different) in Clean.
 *
 */

import StdEnv

concat :: ![[.a]] -> [.a]	

