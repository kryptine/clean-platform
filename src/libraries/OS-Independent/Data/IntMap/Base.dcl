definition module Data.IntMap.Base


// A map of integers to values @a@.

:: IntMap a = Bin !Prefix
                  !Mask
                  !(IntMap a)
                  !(IntMap a)
            | Tip !Int a
            | Nil

:: Prefix :== Int
:: Mask   :== Int


bin :: !Prefix !Mask !(IntMap a) !(IntMap a) -> IntMap a

nomatch :: !Int !Prefix !Mask -> Bool

empty :: IntMap a

link :: !Prefix !(IntMap a) !Prefix !(IntMap a) -> IntMap a

zero :: !Int !Mask -> Bool

mergeWithKey` :: !(Prefix Mask (IntMap c) (IntMap c) -> IntMap c)
                 !((IntMap a) (IntMap b) -> IntMap c) !((IntMap a) -> IntMap c)
                 !((IntMap b) -> IntMap c) !(IntMap a) !(IntMap b) -> IntMap c

foldrWithKey :: !(Int a b -> b) !b !(IntMap a) -> b

fromDistinctAscList :: ![(!Int, !a)] -> IntMap a
