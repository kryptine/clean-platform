module _Tests.Data.Map

import StdBool
import StdChar
import StdInt
import StdList
import StdString
import StdTuple

from Data.Func import on, `on`
import Data.List
import qualified Data.Map as M
from Data.Map import :: Map(..), instance == (Map k v)

import Gast
import Gast.CommandLine

Start w = exposeProperties [Quiet] [Tests 500000]
	[ EP newMap_null
	, EP null
	, EP mapSize
	, EP fromList
	, EP put
	, EP del
	]
	w

:: Key :== Char
:: Value :== Char
:: KVs :== [(Key, Value)]

:: GenMap =
	{ gma  :: !Value
	, gmb  :: !Value
	, gmc  :: !Value
	, gmd  :: !Value
	, gme  :: !Value
	, gmf  :: !Value
	, rest :: !KVs
	}

derive ggen GenMap
derive genShow GenMap, Map, Maybe
derive JSONEncode GenMap
derive bimap []

kvs :: GenMap -> KVs
kvs gm =
	[ ('a',gm.gma)
	, ('b',gm.gmb)
	, ('c',gm.gmc)
	, ('d',gm.gmd)
	, ('e',gm.gme)
	, ('f',gm.gmf)
	: gm.rest
	]

toMap :: GenMap -> 'M'.Map Key Value
toMap gm = 'M'.fromList (kvs gm)

newMap_null :: Property
newMap_null = name "newMap_null" ('M'.null 'M'.newMap)

null :: GenMap -> Property
null gm = let m = toMap gm in
	('M'.mapSize m == 0 <==> 'M'.null m) /\
	(m == 'M'.newMap <==> 'M'.null m)

mapSize :: GenMap -> Property
mapSize gm = 'M'.mapSize (toMap gm) =.= length (removeDup [k \\ (k,_) <- kvs gm])

fromList :: GenMap -> Property
fromList gm = let m = toMap gm in
	check all_present (kvs gm) m /\ // All elements put
	check all_from m (kvs gm) /\    // No other elements
	log_size m                      // Data structure integrity

put :: GenMap Key Value -> Property
put gm k v =
	'M'.get k m` =.= Just v /\                                           // Correctly put
	check all_present [kv \\ kv=:(k`,_) <- 'M'.toList m | k <> k`] m` /\ // Other elements untouched
	log_size m` /\                                                       // Data structure integrity
	sizes_correct m`
where
	m = toMap gm
	m` = 'M'.put k v m

del :: GenMap Key -> Property
del gm k =
	'M'.get k m` =.= Nothing /\                                          // Correctly deleted
	check all_present [kv \\ kv=:(k`,_) <- 'M'.toList m | k <> k`] m` /\ // Other elements untouched
	log_size m` /\                                                       // Data structure integrity
	sizes_correct m`
where
	m = toMap gm
	m` = 'M'.del k m

// Helpers
all_present :: KVs ('M'.Map Key Value) -> Bool
all_present kvs m = all (\(k,v) -> 'M'.get k m == Just v) kvs`
where
	kvs` = nubBy ((==) `on` fst) (reverse kvs) // Remove duplicate keys, assuming the last takes precedence

all_from :: ('M'.Map Key Value) KVs -> Bool
all_from Tip _ = True
all_from (Bin _ k v l r) kvs = isMember (k,v) kvs && all_from l kvs && all_from r kvs

log_size :: ('M'.Map a b) -> Property
log_size m = check (<) nelem (2 ^ depth m)
where
	nelem = 'M'.mapSize m

	depth :: ('M'.Map a b) -> Int
	depth Tip = 0
	depth (Bin _ _ _ l r) = 1 + (max `on` depth) l r

sizes_correct :: ('M'.Map a b) -> Property
sizes_correct Tip = prop True
sizes_correct b=:(Bin _ _ _ l r) =
	'M'.mapSize b =.= 1 + 'M'.mapSize l + 'M'.mapSize r /\
	sizes_correct l /\
	sizes_correct r
