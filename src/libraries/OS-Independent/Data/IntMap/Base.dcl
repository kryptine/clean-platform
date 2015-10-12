definition module Data.IntMap.Base


// A map of integers to values @a@.

:: IntMap a
  = Nil
  | Tip !Int a
  | Bin !Prefix
        !Mask
        !(IntMap a)
        !(IntMap a)

:: Prefix :== Int
:: Mask   :== Int


bin :: !Prefix !Mask !(IntMap a) !(IntMap a) -> IntMap a

nomatch :: !Int !Prefix !Mask -> Bool

empty :: IntMap a

foldrWithKey :: (Int a b -> b) b (IntMap a) -> b

fromDistinctAscList :: ![(!Int, !a)] -> IntMap a

union :: (IntMap a) (IntMap a) -> IntMap a

unions :: [IntMap a] -> IntMap a

mask :: !Int !Mask -> Prefix

shorter :: !Mask !Mask -> Bool

branchMask :: !Prefix !Prefix -> Mask
