implementation module Data.IntMap.Base

import StdEnv

import Control.Applicative
import Control.Monad
import Data.Either
import Data.Functor
import Data.Maybe
import Data.Monoid

// Types
instance Semigroup (IntMap a) where
    mappend x y = union x y

instance Monoid (IntMap a) where
    mempty  = empty

instance Functor IntMap
where
	fmap :: (a -> b) !(IntMap a) -> IntMap b
    fmap f xs = map f xs

//instance Foldable.Foldable IntMap where
  //fold = go
    //where go Nil = mempty
          //go (Tip _ v) = v
          //go (Bin _ _ l r) = go l `mappend` go r
  //{-# INLINABLE fold #-}
  //foldr = foldr
  //{-# INLINE foldr #-}
  //foldl = foldl
  //{-# INLINE foldl #-}
  //foldMap f t = go t
    //where go Nil = mempty
          //go (Tip _ v) = f v
          //go (Bin _ _ l r) = go l `mappend` go r
  //{-# INLINE foldMap #-}

//#if MIN_VERSION_base(4,6,0)
  //foldl' = foldl'
  //{-# INLINE foldl' #-}
  //foldr' = foldr'
  //{-# INLINE foldr' #-}
//#endif
//#if MIN_VERSION_base(4,8,0)
  //length = size
  //{-# INLINE length #-}
  //null   = null
  //{-# INLINE null #-}
  //toList = elems -- NB: Foldable.toList /= IntMap.toList
  //{-# INLINE toList #-}
  //elem = go
    //where STRICT_1_OF_2(go)
          //go _ Nil = False
          //go x (Tip _ y) = x == y
          //go x (Bin _ _ l r) = go x l || go x r
  //{-# INLINABLE elem #-}
  //maximum = start
    //where start Nil = abort "IntMap.Foldable.maximum: called with empty map"
          //start (Tip _ y) = y
          //start (Bin _ _ l r) = go (start l) r

          //STRICT_1_OF_2(go)
          //go m Nil = m
          //go m (Tip _ y) = max m y
          //go m (Bin _ _ l r) = go (go m l) r
  //{-# INLINABLE maximum #-}
  //minimum = start
    //where start Nil = abort "IntMap.Foldable.minimum: called with empty map"
          //start (Tip _ y) = y
          //start (Bin _ _ l r) = go (start l) r

          //STRICT_1_OF_2(go)
          //go m Nil = m
          //go m (Tip _ y) = min m y
          //go m (Bin _ _ l r) = go (go m l) r
  //{-# INLINABLE minimum #-}
  //sum = foldl' (+) 0
  //{-# INLINABLE sum #-}
  //product = foldl' (*) 1
  //{-# INLINABLE product #-}
//#endif

//instance Traversable IntMap where
    //traverse f = traverseWithKey (\_ -> f)
    //{-# INLINE traverse #-}

//  Query
// | /O(1)/. Is the map empty?
//
// > Data.IntMap.null (empty)           == True
// > Data.IntMap.null (singleton 1 'a') == False

null :: (IntMap a) -> Bool
null Nil = True
null _   = False

// | /O(n)/. Number of elements in the map.
//
// > size empty                                   == 0
// > size (singleton 1 'a')                       == 1
// > size (fromList([(1,'a'), (2,'c'), (3,'b')])) == 3
size :: (IntMap a) -> Int
size t
  = case t of
      Bin _ _ l r -> size l + size r
      Tip _ _ -> 1
      Nil     -> 0

// | /O(min(n,W))/. Is the key a member of the map?
//
// > member 5 (fromList [(5,'a'), (3,'b')]) == True
// > member 1 (fromList [(5,'a'), (3,'b')]) == False

member :: !Int (IntMap a) -> Bool
member k (Bin p m l r)
  | nomatch k p m = False
  | zero k m  = member k l
  | otherwise = member k r
member k (Tip kx _) = k == kx
member k Nil = False

// | /O(min(n,W))/. Is the key not a member of the map?
//
// > notMember 5 (fromList [(5,'a'), (3,'b')]) == False
// > notMember 1 (fromList [(5,'a'), (3,'b')]) == True

notMember :: Int (IntMap a) -> Bool
notMember k m = not (member k m)

// | /O(min(n,W))/. Lookup the value at a key in the map. See also 'Data.Map.lookup'.
lookup :: !Int !(IntMap a) -> Maybe a
lookup k (Bin p m l r)
  | nomatch k p m = Nothing
  | zero k m  = lookup k l
  | otherwise = lookup k r
lookup k (Tip kx x)
  | k == kx   = Just x
  | otherwise = Nothing
lookup k Nil = Nothing


find :: !Int (IntMap a) -> a
find k (Bin p m l r)
  | nomatch k p m = not_found k
  | zero k m  = find k l
  | otherwise = find k r
find k (Tip kx x)
  | k == kx   = x
  | otherwise = not_found k
find k Nil = not_found k

not_found k = abort ("IntMap.!: key is not an element of the map")

// | /O(min(n,W))/. The expression @('findWithDefault' def k map)@
// returns the value at key @k@ or returns @def@ when the key is not an
// element of the map.
//
// > findWithDefault 'x' 1 (fromList [(5,'a'), (3,'b')]) == 'x'
// > findWithDefault 'x' 5 (fromList [(5,'a'), (3,'b')]) == 'a'
findWithDefault :: a !Int (IntMap a) -> a
findWithDefault def k (Bin p m l r)
  | nomatch k p m = def
  | zero k m  = findWithDefault def k l
  | otherwise = findWithDefault def k r
findWithDefault def k (Tip kx x)
  | k == kx   = x
  | otherwise = def
findWithDefault def k Nil = def

// | /O(log n)/. Find largest key smaller than the given one and return the
// corresponding (key, value) pair.
//
// > lookupLT 3 (fromList [(3,'a'), (5,'b')]) == Nothing
// > lookupLT 4 (fromList [(3,'a'), (5,'b')]) == Just (3, 'a')
lookupLT :: !Int (IntMap a) -> Maybe (Int, a)
lookupLT k t
  = case t of
      Bin _ m l r
        | m < 0 -> if (k >= 0) (go r l) (go Nil r)
      _         -> go Nil t
  where
  go def (Bin p m l r)
    | nomatch k p m = if (k < p) (unsafeFindMax def) (unsafeFindMax r)
    | zero k m  = go def l
    | otherwise = go l r
  go def (Tip ky y)
    | k <= ky   = unsafeFindMax def
    | otherwise = Just (ky, y)
  go def Nil = unsafeFindMax def

// | /O(log n)/. Find smallest key greater than the given one and return the
// corresponding (key, value) pair.
//
// > lookupGT 4 (fromList [(3,'a'), (5,'b')]) == Just (5, 'b')
// > lookupGT 5 (fromList [(3,'a'), (5,'b')]) == Nothing
lookupGT :: !Int (IntMap a) -> Maybe (Int, a)
lookupGT k t
  = case t of
      Bin _ m l r
        | m < 0 -> if (k >= 0) (go Nil l) (go l r)
      _         -> go Nil t
  where
  go def (Bin p m l r)
    | nomatch k p m = if (k < p) (unsafeFindMin l) (unsafeFindMin def)
    | zero k m  = go r l
    | otherwise = go def r
  go def (Tip ky y)
    | k >= ky   = unsafeFindMin def
    | otherwise = Just (ky, y)
  go def Nil = unsafeFindMin def

// | /O(log n)/. Find largest key smaller or equal to the given one and return
// the corresponding (key, value) pair.
//
// > lookupLE 2 (fromList [(3,'a'), (5,'b')]) == Nothing
// > lookupLE 4 (fromList [(3,'a'), (5,'b')]) == Just (3, 'a')
// > lookupLE 5 (fromList [(3,'a'), (5,'b')]) == Just (5, 'b')
lookupLE :: !Int (IntMap a) -> Maybe (Int, a)
lookupLE k t
  = case t of
      Bin _ m l r
        | m < 0 -> if (k >= 0) (go r l) (go Nil r)
      _         -> go Nil t
  where
  go def (Bin p m l r)
    | nomatch k p m = if (k < p) (unsafeFindMax def) (unsafeFindMax r)
    | zero k m  = go def l
    | otherwise = go l r
  go def (Tip ky y)
    | k < ky    = unsafeFindMax def
    | otherwise = Just (ky, y)
  go def Nil = unsafeFindMax def

// | /O(log n)/. Find smallest key greater or equal to the given one and return
// the corresponding (key, value) pair.
//
// > lookupGE 3 (fromList [(3,'a'), (5,'b')]) == Just (3, 'a')
// > lookupGE 4 (fromList [(3,'a'), (5,'b')]) == Just (5, 'b')
// > lookupGE 6 (fromList [(3,'a'), (5,'b')]) == Nothing
lookupGE :: !Int (IntMap a) -> Maybe (Int, a)
lookupGE k t
  = case t of
      Bin _ m l r
        | m < 0 -> if (k >= 0) (go Nil l) (go l r)
      _         -> go Nil t
  where
    go def (Bin p m l r)
      | nomatch k p m = if (k < p) (unsafeFindMin l) (unsafeFindMin def)
      | zero k m  = go r l
      | otherwise = go def r
    go def (Tip ky y)
      | k > ky    = unsafeFindMin def
      | otherwise = Just (ky, y)
    go def Nil = unsafeFindMin def

// Helper function for lookupGE and lookupGT. It assumes that if a Bin node is
// given, it has m > 0.
unsafeFindMin :: (IntMap a) -> Maybe (Int, a)
unsafeFindMin Nil = Nothing
unsafeFindMin (Tip ky y) = Just (ky, y)
unsafeFindMin (Bin _ _ l _) = unsafeFindMin l

// Helper function for lookupLE and lookupLT. It assumes that if a Bin node is
// given, it has m > 0.
unsafeFindMax :: (IntMap a) -> Maybe (Int, a)
unsafeFindMax Nil = Nothing
unsafeFindMax (Tip ky y) = Just (ky, y)
unsafeFindMax (Bin _ _ _ r) = unsafeFindMax r

// | /O(1)/. The empty map.
//
// > empty      == fromList []
// > size empty == 0
empty :: IntMap a
empty
  = Nil

// | /O(1)/. A map of one element.
//
// > singleton 1 'a'        == fromList [(1, 'a')]
// > size (singleton 1 'a') == 1
singleton :: Int a -> IntMap a
singleton k x
  = Tip k x

// | /O(min(n,W))/. Insert a new key\/value pair in the map.
// If the key is already present in the map, the associated value is
// replaced with the supplied value, i.e. 'insert' is equivalent to
// @'insertWith' 'const'@.
//
// > insert 5 'x' (fromList [(5,'a'), (3,'b')]) == fromList [(3, 'b'), (5, 'x')]
// > insert 7 'x' (fromList [(5,'a'), (3,'b')]) == fromList [(3, 'b'), (5, 'a'), (7, 'x')]
// > insert 5 'x' empty                         == singleton 5 'x'
insert :: !Int a (IntMap a) -> IntMap a
insert k x t =
  case t of
    Bin p m l r
      | nomatch k p m -> link k (Tip k x) p t
      | zero k m      -> Bin p m (insert k x l) r
      | otherwise     -> Bin p m l (insert k x r)
    Tip ky _
      | k==ky         -> Tip k x
      | otherwise     -> link k (Tip k x) ky t
    Nil -> Tip k x

// right-biased insertion, used by 'union'
// | /O(min(n,W))/. Insert with a combining function.
// @'insertWith' f key value mp@
// will insert the pair (key, value) into @mp@ if key does
// not exist in the map. If the key does exist, the function will
// insert @f new_value old_value@.
//
// > insertWith (++) 5 "xxx" (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "xxxa")]
// > insertWith (++) 7 "xxx" (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "a"), (7, "xxx")]
// > insertWith (++) 5 "xxx" empty                         == singleton 5 "xxx"
insertWith :: (a a -> a) Int a (IntMap a) -> IntMap a
insertWith f k x t
  = insertWithKey (\_ x` y` -> f x` y`) k x t
undef = undef
// | /O(min(n,W))/. Insert with a combining function.
// @'insertWithKey' f key value mp@
// will insert the pair (key, value) into @mp@ if key does
// not exist in the map. If the key does exist, the function will
// insert @f key new_value old_value@.
//
// > let f key new_value old_value = (show key) ++ ":" ++ new_value ++ "|" ++ old_value
// > insertWithKey f 5 "xxx" (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "5:xxx|a")]
// > insertWithKey f 7 "xxx" (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "a"), (7, "xxx")]
// > insertWithKey f 5 "xxx" empty                         == singleton 5 "xxx"
insertWithKey :: (Int a a -> a) !Int a (IntMap a) -> IntMap a
insertWithKey f k x t =
  case t of
    Bin p m l r
      | nomatch k p m -> link k (Tip k x) p t
      | zero k m      -> Bin p m (insertWithKey f k x l) r
      | otherwise     -> Bin p m l (insertWithKey f k x r)
    Tip ky y
      | k==ky         -> Tip k (f k x y)
      | otherwise     -> link k (Tip k x) ky t
    Nil -> Tip k x

// | /O(min(n,W))/. The expression (@'insertLookupWithKey' f k x map@)
// is a pair where the first element is equal to (@'lookup' k map@)
// and the second element equal to (@'insertWithKey' f k x map@).
//
// > let f key new_value old_value = (show key) ++ ":" ++ new_value ++ "|" ++ old_value
// > insertLookupWithKey f 5 "xxx" (fromList [(5,"a"), (3,"b")]) == (Just "a", fromList [(3, "b"), (5, "5:xxx|a")])
// > insertLookupWithKey f 7 "xxx" (fromList [(5,"a"), (3,"b")]) == (Nothing,  fromList [(3, "b"), (5, "a"), (7, "xxx")])
// > insertLookupWithKey f 5 "xxx" empty                         == (Nothing,  singleton 5 "xxx")
//
// This is how to define @insertLookup@ using @insertLookupWithKey@:
//
// > let insertLookup kx x t = insertLookupWithKey (\_ a _ -> a) kx x t
// > insertLookup 5 "x" (fromList [(5,"a"), (3,"b")]) == (Just "a", fromList [(3, "b"), (5, "x")])
// > insertLookup 7 "x" (fromList [(5,"a"), (3,"b")]) == (Nothing,  fromList [(3, "b"), (5, "a"), (7, "x")])

insertLookupWithKey :: (Int a a -> a) !Int a (IntMap a) -> (Maybe a, IntMap a)
insertLookupWithKey f k x t =
  case t of
    Bin p m l r
      | nomatch k p m -> (Nothing,link k (Tip k x) p t)
      | zero k m      -> let (found,l`) = insertLookupWithKey f k x l in (found,Bin p m l` r)
      | otherwise     -> let (found,r`) = insertLookupWithKey f k x r in (found,Bin p m l r`)
    Tip ky y
      | k==ky         -> (Just y,Tip k (f k x y))
      | otherwise     -> (Nothing,link k (Tip k x) ky t)
    Nil -> (Nothing,Tip k x)

// | /O(min(n,W))/. Delete a key and its value from the map. When the key is not
// a member of the map, the original map is returned.
//
// > delete 5 (fromList [(5,"a"), (3,"b")]) == singleton 3 "b"
// > delete 7 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "a")]
// > delete 5 empty                         == empty
delete :: !Int (IntMap a) -> IntMap a
delete k t =
  case t of
    Bin p m l r
      | nomatch k p m -> t
      | zero k m      -> bin p m (delete k l) r
      | otherwise     -> bin p m l (delete k r)
    Tip ky _
      | k==ky         -> Nil
      | otherwise     -> t
    Nil -> Nil

// | /O(min(n,W))/. Adjust a value at a specific key. When the key is not
// a member of the map, the original map is returned.
//
// > adjust ("new " ++) 5 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "new a")]
// > adjust ("new " ++) 7 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "a")]
// > adjust ("new " ++) 7 empty                         == empty
adjust ::  (a -> a) !Int (IntMap a) -> IntMap a
adjust f k m
  = adjustWithKey (\_ x -> f x) k m

// | /O(min(n,W))/. Adjust a value at a specific key. When the key is not
// a member of the map, the original map is returned.
//
// > let f key x = (show key) ++ ":new " ++ x
// > adjustWithKey f 5 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "5:new a")]
// > adjustWithKey f 7 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "a")]
// > adjustWithKey f 7 empty                         == empty
adjustWithKey ::  (Int a -> a) !Int (IntMap a) -> IntMap a
adjustWithKey f k m = updateWithKey (\k` x -> Just (f k` x)) k m

// | /O(min(n,W))/. The expression (@'update' f k map@) updates the value @x@
// at @k@ (if it is in the map). If (@f x@) is 'Nothing', the element is
// deleted. If it is (@'Just' y@), the key @k@ is bound to the new value @y@.
//
// > let f x = if x == "a" then Just "new a" else Nothing
// > update f 5 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "new a")]
// > update f 7 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "a")]
// > update f 3 (fromList [(5,"a"), (3,"b")]) == singleton 5 "a"

update ::  (a -> Maybe a) !Int (IntMap a) -> IntMap a
update f k m = updateWithKey (\_ x -> f x) k m

// | /O(min(n,W))/. The expression (@'update' f k map@) updates the value @x@
// at @k@ (if it is in the map). If (@f k x@) is 'Nothing', the element is
// deleted. If it is (@'Just' y@), the key @k@ is bound to the new value @y@.
//
// > let f k x = if x == "a" then Just ((show k) ++ ":new a") else Nothing
// > updateWithKey f 5 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "5:new a")]
// > updateWithKey f 7 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "a")]
// > updateWithKey f 3 (fromList [(5,"a"), (3,"b")]) == singleton 5 "a"
updateWithKey ::  (Int a -> Maybe a) !Int (IntMap a) -> IntMap a
updateWithKey f k t =
  case t of
    Bin p m l r
      | nomatch k p m -> t
      | zero k m      -> bin p m (updateWithKey f k l) r
      | otherwise     -> bin p m l (updateWithKey f k r)
    Tip ky y
      | k==ky         -> case (f k y) of
                           Just y` -> Tip ky y`
                           Nothing -> Nil
      | otherwise     -> t
    Nil -> Nil

// | /O(min(n,W))/. Lookup and update.
// The function returns original value, if it is updated.
// This is different behavior than 'Data.Map.updateLookupWithKey'.
// Returns the original key value if the map entry is deleted.
//
// > let f k x = if x == "a" then Just ((show k) ++ ":new a") else Nothing
// > updateLookupWithKey f 5 (fromList [(5,"a"), (3,"b")]) == (Just "a", fromList [(3, "b"), (5, "5:new a")])
// > updateLookupWithKey f 7 (fromList [(5,"a"), (3,"b")]) == (Nothing,  fromList [(3, "b"), (5, "a")])
// > updateLookupWithKey f 3 (fromList [(5,"a"), (3,"b")]) == (Just "b", singleton 5 "a")
updateLookupWithKey :: (Int a -> Maybe a) !Int (IntMap a) -> (Maybe a,IntMap a)
updateLookupWithKey f k t =
  case t of
    Bin p m l r
      | nomatch k p m -> (Nothing,t)
      | zero k m      -> let (found,l`) = updateLookupWithKey f k l in (found,bin p m l` r)
      | otherwise     -> let (found,r`) = updateLookupWithKey f k r in (found,bin p m l r`)
    Tip ky y
      | k==ky         -> case (f k y) of
                           Just y` -> (Just y,Tip ky y`)
                           Nothing -> (Just y,Nil)
      | otherwise     -> (Nothing,t)
    Nil -> (Nothing,Nil)



// | /O(min(n,W))/. The expression (@'alter' f k map@) alters the value @x@ at @k@, or absence thereof.
// 'alter' can be used to insert, delete, or update a value in an 'IntMap'.
// In short : @'lookup' k ('alter' f k m) = f ('lookup' k m)@.
alter :: ((Maybe a) -> Maybe a) !Int (IntMap a) -> IntMap a
alter f k t =
  case t of
    Bin p m l r
      | nomatch k p m -> case f Nothing of
                           Nothing -> t
                           Just x -> link k (Tip k x) p t
      | zero k m      -> bin p m (alter f k l) r
      | otherwise     -> bin p m l (alter f k r)
    Tip ky y
      | k==ky         -> case f (Just y) of
                           Just x -> Tip ky x
                           Nothing -> Nil
      | otherwise     -> case f Nothing of
                           Just x -> link k (Tip k x) ky t
                           Nothing -> Tip ky y
    Nil               -> case f Nothing of
                           Just x -> Tip k x
                           Nothing -> Nil


// | The union of a list of maps.
//
// > unions [(fromList [(5, "a"), (3, "b")]), (fromList [(5, "A"), (7, "C")]), (fromList [(5, "A3"), (3, "B3")])]
// >     == fromList [(3, "b"), (5, "a"), (7, "C")]
// > unions [(fromList [(5, "A3"), (3, "B3")]), (fromList [(5, "A"), (7, "C")]), (fromList [(5, "a"), (3, "b")])]
// >     == fromList [(3, "B3"), (5, "A3"), (7, "C")]
unions :: ![IntMap a] -> IntMap a
unions xs = foldl union empty xs

// | The union of a list of maps, with a combining operation.
//
// > unionsWith (++) [(fromList [(5, "a"), (3, "b")]), (fromList [(5, "A"), (7, "C")]), (fromList [(5, "A3"), (3, "B3")])]
// >     == fromList [(3, "bB3"), (5, "aAA3"), (7, "C")]
unionsWith :: (a a -> a) [IntMap a] -> IntMap a
unionsWith f ts = foldl (unionWith f) empty ts

// | /O(n+m)/. The (left-biased) union of two maps.
// It prefers the first map when duplicate keys are encountered,
// i.e. (@'union' == 'unionWith' 'const'@).
//
// > union (fromList [(5, "a"), (3, "b")]) (fromList [(5, "A"), (7, "C")]) == fromList [(3, "b"), (5, "a"), (7, "C")]
union :: !(IntMap a) !(IntMap a) -> IntMap a
union m1 m2 = mergeWithKey` Bin const id id m1 m2

// | /O(n+m)/. The union with a combining function.
//
// > unionWith (++) (fromList [(5, "a"), (3, "b")]) (fromList [(5, "A"), (7, "C")]) == fromList [(3, "b"), (5, "aA"), (7, "C")]
unionWith :: (a a -> a) (IntMap a) (IntMap a) -> IntMap a
unionWith f m1 m2 = unionWithKey (\_ x y -> f x y) m1 m2

// | /O(n+m)/. The union with a combining function.
//
// > let f key left_value right_value = (show key) ++ ":" ++ left_value ++ "|" ++ right_value
// > unionWithKey f (fromList [(5, "a"), (3, "b")]) (fromList [(5, "A"), (7, "C")]) == fromList [(3, "b"), (5, "5:a|A"), (7, "C")]
unionWithKey :: (Int a a -> a) (IntMap a) (IntMap a) -> IntMap a
unionWithKey f m1 m2
  = mergeWithKey` Bin (\(Tip k1 x1) (Tip _ x2) -> Tip k1 (f k1 x1 x2)) id id m1 m2

// | /O(n+m)/. Difference between two maps (based on keys).
//
// > difference (fromList [(5, "a"), (3, "b")]) (fromList [(5, "A"), (7, "C")]) == singleton 3 "b"
difference :: (IntMap a) (IntMap b) -> IntMap a
difference m1 m2
  = mergeWithKey (\_ _ _ -> Nothing) id (const Nil) m1 m2

// | /O(n+m)/. Difference with a combining function.
//
// > let f al ar = if al == "b" then Just (al ++ ":" ++ ar) else Nothing
// > differenceWith f (fromList [(5, "a"), (3, "b")]) (fromList [(5, "A"), (3, "B"), (7, "C")])
// >     == singleton 3 "b:B"
differenceWith :: (a b -> Maybe a) (IntMap a) (IntMap b) -> IntMap a
differenceWith f m1 m2
  = differenceWithKey (\_ x y -> f x y) m1 m2

// | /O(n+m)/. Difference with a combining function. When two equal keys are
// encountered, the combining function is applied to the key and both values.
// If it returns 'Nothing', the element is discarded (proper set difference).
// If it returns (@'Just' y@), the element is updated with a new value @y@.
//
// > let f k al ar = if al == "b" then Just ((show k) ++ ":" ++ al ++ "|" ++ ar) else Nothing
// > differenceWithKey f (fromList [(5, "a"), (3, "b")]) (fromList [(5, "A"), (3, "B"), (10, "C")])
// >     == singleton 3 "3:b|B"
differenceWithKey :: (Int a b -> Maybe a) (IntMap a) (IntMap b) -> IntMap a
differenceWithKey f m1 m2
  = mergeWithKey f id (const Nil) m1 m2

// | /O(n+m)/. The (left-biased) intersection of two maps (based on keys).
//
// > intersection (fromList [(5, "a"), (3, "b")]) (fromList [(5, "A"), (7, "C")]) == singleton 5 "a"
intersection :: (IntMap a) (IntMap b) -> IntMap a
intersection m1 m2
  = mergeWithKey` bin const (const Nil) (const Nil) m1 m2

// | /O(n+m)/. The intersection with a combining function.
//
// > intersectionWith (++) (fromList [(5, "a"), (3, "b")]) (fromList [(5, "A"), (7, "C")]) == singleton 5 "aA"
intersectionWith :: (a b -> c) (IntMap a) (IntMap b) -> IntMap c
intersectionWith f m1 m2
  = intersectionWithKey (\_ x y -> f x y) m1 m2

// | /O(n+m)/. The intersection with a combining function.
//
// > let f k al ar = (show k) ++ ":" ++ al ++ "|" ++ ar
// > intersectionWithKey f (fromList [(5, "a"), (3, "b")]) (fromList [(5, "A"), (7, "C")]) == singleton 5 "5:a|A"
intersectionWithKey :: (Int a b -> c) (IntMap a) (IntMap b) -> IntMap c
intersectionWithKey f m1 m2
  = mergeWithKey` bin (\(Tip k1 x1) (Tip _ x2) -> Tip k1 (f k1 x1 x2)) (const Nil) (const Nil) m1 m2

// | /O(n+m)/. A high-performance universal combining function. Using
// 'mergeWithKey', all combining functions can be defined without any loss of
// efficiency (with exception of 'union', 'difference' and 'intersection',
// where sharing of some nodes is lost with 'mergeWithKey').
//
// Please make sure you know what is going on when using 'mergeWithKey',
// otherwise you can be surprised by unexpected code growth or even
// corruption of the data structure.
//
// When 'mergeWithKey' is given three arguments, it is inlined to the call
// site. You should therefore use 'mergeWithKey' only to define your custom
// combining functions. For example, you could define 'unionWithKey',
// 'differenceWithKey' and 'intersectionWithKey' as
//
// > myUnionWithKey f m1 m2 = mergeWithKey (\k x1 x2 -> Just (f k x1 x2)) id id m1 m2
// > myDifferenceWithKey f m1 m2 = mergeWithKey f id (const empty) m1 m2
// > myIntersectionWithKey f m1 m2 = mergeWithKey (\k x1 x2 -> Just (f k x1 x2)) (const empty) (const empty) m1 m2
//
// When calling @'mergeWithKey' combine only1 only2@, a function combining two
// 'IntMap's is created, such that
//
// * if a key is present in both maps, it is passed with both corresponding
//   values to the @combine@ function. Depending on the result, the key is either
//   present in the result with specified value, or is left out;
//
// * a nonempty subtree present only in the first map is passed to @only1@ and
//   the output is added to the result;
//
// * a nonempty subtree present only in the second map is passed to @only2@ and
//   the output is added to the result.
//
// The @only1@ and @only2@ methods /must return a map with a subset (possibly empty) of the keys of the given map/.
// The values can be modified arbitrarily. Most common variants of @only1@ and
// @only2@ are 'id' and @'const' 'empty'@, but for example @'map' f@ or
// @'filterWithKey' f@ could be used for any @f@.
mergeWithKey :: (Int a b -> Maybe c) ((IntMap a) -> IntMap c) ((IntMap b) -> IntMap c)
             (IntMap a) (IntMap b) -> IntMap c
mergeWithKey f g1 g2 m1 m2 = mergeWithKey` bin combine g1 g2 m1 m2
  where
  combine (Tip k1 x1) (Tip _ x2) = case f k1 x1 x2 of
                                     Nothing -> Nil
                                     Just x -> Tip k1 x

// Slightly more general version of mergeWithKey. It differs in the following:
//
// * the combining function operates on maps instead of keys and values. The
//   reason is to enable sharing in union, difference and intersection.
//
// * mergeWithKey' is given an equivalent of bin. The reason is that in union*,
//   Bin constructor can be used, because we know both subtrees are nonempty.
mergeWithKey` :: (Prefix Mask (IntMap c) (IntMap c) -> IntMap c)
                 ((IntMap a) (IntMap b) -> IntMap c) ((IntMap a) -> IntMap c)
                 ((IntMap b) -> IntMap c) (IntMap a) (IntMap b) -> IntMap c
mergeWithKey` bin` f g1 g2 m1 m2 = go m1 m2
  where
    go t1=:(Bin p1 m1 l1 r1) t2=:(Bin p2 m2 l2 r2)
      | shorter m1 m2  = merge1
      | shorter m2 m1  = merge2
      | p1 == p2       = bin` p1 m1 (go l1 l2) (go r1 r2)
      | otherwise      = maybe_link p1 (g1 t1) p2 (g2 t2)
      where
        merge1 | nomatch p2 p1 m1  = maybe_link p1 (g1 t1) p2 (g2 t2)
               | zero p2 m1        = bin` p1 m1 (go l1 t2) (g1 r1)
               | otherwise         = bin` p1 m1 (g1 l1) (go r1 t2)
        merge2 | nomatch p1 p2 m2  = maybe_link p1 (g1 t1) p2 (g2 t2)
               | zero p1 m2        = bin` p2 m2 (go t1 l2) (g2 r2)
               | otherwise         = bin` p2 m2 (g2 l2) (go t1 r2)

    go t1`=:(Bin _ _ _ _) t2`=:(Tip k2` _) = merge t2` k2` t1`
      where
      merge t2 k2 t1=:(Bin p1 m1 l1 r1)
        | nomatch k2 p1 m1 = maybe_link p1 (g1 t1) k2 (g2 t2)
        | zero k2 m1 = bin` p1 m1 (merge t2 k2 l1) (g1 r1)
        | otherwise  = bin` p1 m1 (g1 l1) (merge t2 k2 r1)
      merge t2 k2 t1=:(Tip k1 _)
        | k1 == k2 = f t1 t2
        | otherwise = maybe_link k1 (g1 t1) k2 (g2 t2)
      merge t2 _  Nil = g2 t2

    go t1=:(Bin _ _ _ _) Nil = g1 t1

    go t1`=:(Tip k1` _) t2` = merge t1` k1` t2`
      where
      merge t1 k1 t2=:(Bin p2 m2 l2 r2)
        | nomatch k1 p2 m2 = maybe_link k1 (g1 t1) p2 (g2 t2)
        | zero k1 m2 = bin` p2 m2 (merge t1 k1 l2) (g2 r2)
        | otherwise  = bin` p2 m2 (g2 l2) (merge t1 k1 r2)
      merge t1 k1 t2=:(Tip k2 _)
        | k1 == k2 = f t1 t2
        | otherwise = maybe_link k1 (g1 t1) k2 (g2 t2)
      merge t1 _  Nil = g1 t1

    go Nil t2 = g2 t2

    maybe_link _ Nil _ t2 = t2
    maybe_link _ t1 _ Nil = t1
    maybe_link p1 t1 p2 t2 = link p1 t1 p2 t2

// | /O(min(n,W))/. Update the value at the minimal key.
//
// > updateMinWithKey (\ k a -> Just ((show k) ++ ":" ++ a)) (fromList [(5,"a"), (3,"b")]) == fromList [(3,"3:b"), (5,"a")]
// > updateMinWithKey (\ _ _ -> Nothing)                     (fromList [(5,"a"), (3,"b")]) == singleton 5 "a"
updateMinWithKey :: (Int a -> Maybe a) (IntMap a) -> IntMap a
updateMinWithKey f t =
  case t of Bin p m l r | m < 0 -> bin p m l (go f r)
            _ -> go f t
  where
    go f` (Bin p m l r) = bin p m (go f` l) r
    go f` (Tip k y) = case f` k y of
                        Just y` -> Tip k y`
                        Nothing -> Nil
    go _ Nil = abort "updateMinWithKey Nil"

// | /O(min(n,W))/. Update the value at the maximal key.
//
// > updateMaxWithKey (\ k a -> Just ((show k) ++ ":" ++ a)) (fromList [(5,"a"), (3,"b")]) == fromList [(3,"b"), (5,"5:a")]
// > updateMaxWithKey (\ _ _ -> Nothing)                     (fromList [(5,"a"), (3,"b")]) == singleton 3 "b"
updateMaxWithKey :: (Int a -> Maybe a) (IntMap a) -> IntMap a
updateMaxWithKey f t =
  case t of Bin p m l r | m < 0 -> bin p m (go f l) r
            _ -> go f t
  where
    go f` (Bin p m l r) = bin p m l (go f` r)
    go f` (Tip k y) = case f` k y of
                        Just y` -> Tip k y`
                        Nothing -> Nil
    go _ Nil = abort "updateMaxWithKey Nil"

// | /O(min(n,W))/. Retrieves the maximal (key,value) pair of the map, and
// the map stripped of that element, or 'Nothing' if passed an empty map.
//
// > maxViewWithKey (fromList [(5,"a"), (3,"b")]) == Just ((5,"a"), singleton 3 "b")
// > maxViewWithKey empty == Nothing
maxViewWithKey :: !(IntMap a) -> Maybe ((Int, a), IntMap a)
maxViewWithKey t =
  case t of Nil -> Nothing
            Bin p m l r | m < 0 -> case go l of (result, l`) -> Just (result, bin p m l` r)
            _ -> Just (go t)
  where
    go (Bin p m l r) = case go r of (result, r`) -> (result, bin p m l r`)
    go (Tip k y) = ((k, y), Nil)
    go Nil = abort "maxViewWithKey Nil"

// | /O(min(n,W))/. Retrieves the minimal (key,value) pair of the map, and
// the map stripped of that element, or 'Nothing' if passed an empty map.
//
// > minViewWithKey (fromList [(5,"a"), (3,"b")]) == Just ((3,"b"), singleton 5 "a")
// > minViewWithKey empty == Nothing
minViewWithKey :: !(IntMap a) -> Maybe ((Int, a), IntMap a)
minViewWithKey t =
  case t of Nil -> Nothing
            Bin p m l r | m < 0 -> case go r of (result, r`) -> Just (result, bin p m l r`)
            _ -> Just (go t)
  where
    go (Bin p m l r) = case go l of (result, l`) -> (result, bin p m l` r)
    go (Tip k y) = ((k, y), Nil)
    go Nil = abort "minViewWithKey Nil"

// | /O(min(n,W))/. Update the value at the maximal key.
//
// > updateMax (\ a -> Just ("X" ++ a)) (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "Xa")]
// > updateMax (\ _ -> Nothing)         (fromList [(5,"a"), (3,"b")]) == singleton 3 "b"
updateMax :: (a -> Maybe a) (IntMap a) -> IntMap a
updateMax f m = updateMaxWithKey (const f) m

// | /O(min(n,W))/. Update the value at the minimal key.
//
// > updateMin (\ a -> Just ("X" ++ a)) (fromList [(5,"a"), (3,"b")]) == fromList [(3, "Xb"), (5, "a")]
// > updateMin (\ _ -> Nothing)         (fromList [(5,"a"), (3,"b")]) == singleton 5 "a"
updateMin :: (a -> Maybe a) (IntMap a) -> IntMap a
updateMin f m = updateMinWithKey (const f) m

// Similar to the Arrow instance.
first :: (a -> c) (a, b) -> (c, b)
first f (x,y) = (f x,y)

// | /O(min(n,W))/. Retrieves the maximal key of the map, and the map
// stripped of that element, or 'Nothing' if passed an empty map.
maxView :: (IntMap a) -> Maybe (a, IntMap a)
maxView t = liftM (first snd) (maxViewWithKey t)

// | /O(min(n,W))/. Retrieves the minimal key of the map, and the map
// stripped of that element, or 'Nothing' if passed an empty map.
minView :: (IntMap a) -> Maybe (a, IntMap a)
minView t = liftM (first snd) (minViewWithKey t)

// | /O(min(n,W))/. Delete and find the maximal element.
deleteFindMax :: (IntMap a) -> ((Int, a), IntMap a)
deleteFindMax m = fromMaybe (abort "deleteFindMax: empty map has no maximal element") (maxViewWithKey m)

// | /O(min(n,W))/. Delete and find the minimal element.
deleteFindMin :: (IntMap a) -> ((Int, a), IntMap a)
deleteFindMin m = fromMaybe (abort "deleteFindMin: empty map has no minimal element") (minViewWithKey m)

// | /O(min(n,W))/. The minimal key of the map.
findMin :: (IntMap a) -> (Int, a)
findMin Nil = abort "findMin: empty map has no minimal element"
findMin (Tip k v) = (k,v)
findMin (Bin _ m l r)
  |   m < 0   = go r
  | otherwise = go l
    where go (Tip k v)      = (k,v)
          go (Bin _ _ l` _) = go l`
          go Nil            = abort "findMax Nil"

// | /O(min(n,W))/. The maximal key of the map.
findMax :: (IntMap a) -> (Int, a)
findMax Nil = abort "findMax: empty map has no maximal element"
findMax (Tip k v) = (k,v)
findMax (Bin _ m l r)
  |   m < 0   = go l
  | otherwise = go r
    where go (Tip k v)      = (k,v)
          go (Bin _ _ _ r`) = go r`
          go Nil            = abort "findMax Nil"

// | /O(min(n,W))/. Delete the minimal key. Returns an empty map if the map is empty.
//
// Note that this is a change of behaviour for consistency with 'Data.Map.Map' &#8211;
// versions prior to 0.5 threw an abort if the 'IntMap' was already empty.
deleteMin :: (IntMap a) -> IntMap a
deleteMin m = maybe Nil snd (minView m)

// | /O(min(n,W))/. Delete the maximal key. Returns an empty map if the map is empty.
//
// Note that this is a change of behaviour for consistency with 'Data.Map.Map' &#8211;
// versions prior to 0.5 threw an abort if the 'IntMap' was already empty.
deleteMax :: (IntMap a) -> IntMap a
deleteMax m = maybe Nil snd (maxView m)


// | /O(n+m)/. Is this a proper submap? (ie. a submap but not equal).
// Defined as (@'isProperSubmapOf' = 'isProperSubmapOfBy' (==)@).
isProperSubmapOf :: (IntMap a) (IntMap a) -> Bool | == a
isProperSubmapOf m1 m2
  = isProperSubmapOfBy (==) m1 m2

// | /O(n+m)/. Is this a proper submap? (ie. a submap but not equal).
// The expression (@'isProperSubmapOfBy' f m1 m2@) returns 'True' when
// @m1@ and @m2@ are not equal,
// all keys in @m1@ are in @m2@, and when @f@ returns 'True' when
// applied to their respective values. For example, the following
// expressions are all 'True':
//
//  > isProperSubmapOfBy (==) (fromList [(1,1)]) (fromList [(1,1),(2,2)])
//  > isProperSubmapOfBy (<=) (fromList [(1,1)]) (fromList [(1,1),(2,2)])
//
// But the following are all 'False':
//
//  > isProperSubmapOfBy (==) (fromList [(1,1),(2,2)]) (fromList [(1,1),(2,2)])
//  > isProperSubmapOfBy (==) (fromList [(1,1),(2,2)]) (fromList [(1,1)])
//  > isProperSubmapOfBy (<)  (fromList [(1,1)])       (fromList [(1,1),(2,2)])
isProperSubmapOfBy :: (a b -> Bool) (IntMap a) (IntMap b) -> Bool
isProperSubmapOfBy predicate t1 t2
  = case submapCmp predicate t1 t2 of
      LT -> True
      _  -> False

:: Ordering = LT | GT | EQ

submapCmp :: (a b -> Bool) (IntMap a) (IntMap b) -> Ordering
submapCmp predicate t1=:(Bin p1 m1 l1 r1) (Bin p2 m2 l2 r2)
  | shorter m1 m2  = GT
  | shorter m2 m1  = submapCmpLt
  | p1 == p2       = submapCmpEq
  | otherwise      = GT  // disjoint
  where
    submapCmpLt
      | nomatch p1 p2 m2  = GT
      | zero p1 m2        = submapCmp predicate t1 l2
      | otherwise         = submapCmp predicate t1 r2
    submapCmpEq = case (submapCmp predicate l1 l2, submapCmp predicate r1 r2) of
                    (GT,_ ) -> GT
                    (_ ,GT) -> GT
                    (EQ,EQ) -> EQ
                    _       -> LT

submapCmp _         (Bin _ _ _ _) _  = GT
submapCmp predicate (Tip kx x) (Tip ky y)
  | (kx == ky) && predicate x y = EQ
  | otherwise                   = GT  // disjoint
submapCmp predicate (Tip k x) t
  = case lookup k t of
     Just y | predicate x y -> LT
     _                      -> GT // disjoint
submapCmp _    Nil Nil = EQ
submapCmp _    Nil _   = LT

// | /O(n+m)/. Is this a submap?
// Defined as (@'isSubmapOf' = 'isSubmapOfBy' (==)@).
isSubmapOf :: (IntMap a) (IntMap a) -> Bool | == a
isSubmapOf m1 m2 = isSubmapOfBy (==) m1 m2

// | /O(n+m)/.
// The expression (@'isSubmapOfBy' f m1 m2@) returns 'True' if
// all keys in @m1@ are in @m2@, and when @f@ returns 'True' when
// applied to their respective values. For example, the following
// expressions are all 'True':
//
//  > isSubmapOfBy (==) (fromList [(1,1)]) (fromList [(1,1),(2,2)])
//  > isSubmapOfBy (<=) (fromList [(1,1)]) (fromList [(1,1),(2,2)])
//  > isSubmapOfBy (==) (fromList [(1,1),(2,2)]) (fromList [(1,1),(2,2)])
//
// But the following are all 'False':
//
//  > isSubmapOfBy (==) (fromList [(1,2)]) (fromList [(1,1),(2,2)])
//  > isSubmapOfBy (<) (fromList [(1,1)]) (fromList [(1,1),(2,2)])
//  > isSubmapOfBy (==) (fromList [(1,1),(2,2)]) (fromList [(1,1)])
isSubmapOfBy :: (a b -> Bool) (IntMap a) (IntMap b) -> Bool
isSubmapOfBy predicate t1=:(Bin p1 m1 l1 r1) (Bin p2 m2 l2 r2)
  | shorter m1 m2  = False
  | shorter m2 m1  = match p1 p2 m2 && (if (zero p1 m2) (isSubmapOfBy predicate t1 l2)
                                                        (isSubmapOfBy predicate t1 r2))
  | otherwise      = (p1==p2) && isSubmapOfBy predicate l1 l2 && isSubmapOfBy predicate r1 r2
isSubmapOfBy _         (Bin _ _ _ _) _ = False
isSubmapOfBy predicate (Tip k x) t     = case lookup k t of
                                         Just y  -> predicate x y
                                         Nothing -> False
isSubmapOfBy _         Nil _           = True

// | /O(n)/. Map a function over all values in the map.
//
// > map (++ "x") (fromList [(5,"a"), (3,"b")]) == fromList [(3, "bx"), (5, "ax")]
map :: (a -> b) (IntMap a) -> IntMap b
map f t
  = case t of
      Bin p m l r -> Bin p m (map f l) (map f r)
      Tip k x     -> Tip k (f x)
      Nil         -> Nil

// | /O(n)/. Map a function over all values in the map.
//
// > let f key x = (show key) ++ ":" ++ x
// > mapWithKey f (fromList [(5,"a"), (3,"b")]) == fromList [(3, "3:b"), (5, "5:a")]
mapWithKey :: (Int a -> b) (IntMap a) -> IntMap b
mapWithKey f t
  = case t of
      Bin p m l r -> Bin p m (mapWithKey f l) (mapWithKey f r)
      Tip k x     -> Tip k (f k x)
      Nil         -> Nil

// | /O(n)/.
// @'traverseWithKey' f s == 'fromList' <$> 'traverse' (\(k, v) -> (,) k <$> f k v) ('toList' m)@
// That is, behaves exactly like a regular 'traverse' except that the traversing
// function also has access to the key associated with a value.
//
// > traverseWithKey (\k v -> if odd k then Just (succ v) else Nothing) (fromList [(1, 'a'), (5, 'e')]) == Just (fromList [(1, 'b'), (5, 'f')])
// > traverseWithKey (\k v -> if odd k then Just (succ v) else Nothing) (fromList [(2, 'c')])           == Nothing
traverseWithKey :: (Int a -> t b) (IntMap a) -> t (IntMap b) | Applicative t
traverseWithKey f m = go m
  where
    go Nil = pure Nil
    go (Tip k v) = Tip k <$> f k v
    go (Bin p m l r) = Bin p m <$> go l <*> go r

// | /O(n)/. The function @'mapAccum'@ threads an accumulating
// argument through the map in ascending order of keys.
//
// > let f a b = (a ++ b, b ++ "X")
// > mapAccum f "Everything: " (fromList [(5,"a"), (3,"b")]) == ("Everything: ba", fromList [(3, "bX"), (5, "aX")])
mapAccum :: (a b -> (a,c)) a (IntMap b) -> (a,IntMap c)
mapAccum f x m = mapAccumWithKey (\a` _ x -> f a` x) x m

// | /O(n)/. The function @'mapAccumWithKey'@ threads an accumulating
// argument through the map in ascending order of keys.
//
// > let f a k b = (a ++ " " ++ (show k) ++ "-" ++ b, b ++ "X")
// > mapAccumWithKey f "Everything:" (fromList [(5,"a"), (3,"b")]) == ("Everything: 3-b 5-a", fromList [(3, "bX"), (5, "aX")])
mapAccumWithKey :: (a Int b -> (a,c)) a (IntMap b) -> (a,IntMap c)
mapAccumWithKey f a t
  = mapAccumL f a t

// | /O(n)/. The function @'mapAccumL'@ threads an accumulating
// argument through the map in ascending order of keys.
mapAccumL :: (a Int b -> (a,c)) a (IntMap b) -> (a,IntMap c)
mapAccumL f a t
  = case t of
      Bin p m l r -> let (a1,l`) = mapAccumL f a l
                         (a2,r`) = mapAccumL f a1 r
                     in (a2,Bin p m l` r`)
      Tip k x     -> let (a`,x`) = f a k x in (a`,Tip k x`)
      Nil         -> (a,Nil)

// | /O(n)/. The function @'mapAccumR'@ threads an accumulating
// argument through the map in descending order of keys.
mapAccumRWithKey :: (a Int b -> (a,c)) a (IntMap b) -> (a,IntMap c)
mapAccumRWithKey f a t
  = case t of
      Bin p m l r -> let (a1,r`) = mapAccumRWithKey f a r
                         (a2,l`) = mapAccumRWithKey f a1 l
                     in (a2,Bin p m l` r`)
      Tip k x     -> let (a`,x`) = f a k x in (a`,Tip k x`)
      Nil         -> (a,Nil)

// | /O(n*min(n,W))/.
// @'mapKeys' f s@ is the map obtained by applying @f@ to each key of @s@.
//
// The size of the result may be smaller if @f@ maps two or more distinct
// keys to the same new key.  In this case the value at the greatest of the
// original keys is retained.
//
// > mapKeys (+ 1) (fromList [(5,"a"), (3,"b")])                        == fromList [(4, "b"), (6, "a")]
// > mapKeys (\ _ -> 1) (fromList [(1,"b"), (2,"a"), (3,"d"), (4,"c")]) == singleton 1 "c"
// > mapKeys (\ _ -> 3) (fromList [(1,"b"), (2,"a"), (3,"d"), (4,"c")]) == singleton 3 "c"
mapKeys :: (Int -> Int) (IntMap a) -> IntMap a
mapKeys f m = fromList (foldrWithKey (\k x xs -> [(f k, x) : xs]) [] m)

// | /O(n*min(n,W))/.
// @'mapKeysWith' c f s@ is the map obtained by applying @f@ to each key of @s@.
//
// The size of the result may be smaller if @f@ maps two or more distinct
// keys to the same new key.  In this case the associated values will be
// combined using @c@.
//
// > mapKeysWith (++) (\ _ -> 1) (fromList [(1,"b"), (2,"a"), (3,"d"), (4,"c")]) == singleton 1 "cdab"
// > mapKeysWith (++) (\ _ -> 3) (fromList [(1,"b"), (2,"a"), (3,"d"), (4,"c")]) == singleton 3 "cdab"
//mapKeysWith :: (a a -> a) (Int -> Int) (IntMap a) -> IntMap a
//mapKeysWith c f m = fromListWith c o foldrWithKey (\k x xs -> [(f k, x) : xs]) [] m

// | /O(n*min(n,W))/.
// @'mapKeysMonotonic' f s == 'mapKeys' f s@, but works only when @f@
// is strictly monotonic.
// That is, for any values @x@ and @y@, if @x@ < @y@ then @f x@ < @f y@.
// /The precondition is not checked./
// Semi-formally, we have:
//
// > and [x < y ==> f x < f y | x <- ls, y <- ls]
// >                     ==> mapKeysMonotonic f s == mapKeys f s
// >     where ls = keys s
//
// This means that @f@ maps distinct original keys to distinct resulting keys.
// This function has slightly better performance than 'mapKeys'.
//
// > mapKeysMonotonic (\ k -> k * 2) (fromList [(5,"a"), (3,"b")]) == fromList [(6, "b"), (10, "a")]
mapKeysMonotonic :: (Int -> Int) (IntMap a) -> IntMap a
mapKeysMonotonic f m = fromDistinctAscList (foldrWithKey (\k x xs -> [(f k, x) : xs]) [] m)

// | /O(n)/. Filter all values that satisfy some predicate.
//
// > filter (> "a") (fromList [(5,"a"), (3,"b")]) == singleton 3 "b"
// > filter (> "x") (fromList [(5,"a"), (3,"b")]) == empty
// > filter (< "a") (fromList [(5,"a"), (3,"b")]) == empty
filter :: (a -> Bool) (IntMap a) -> IntMap a
filter p m
  = filterWithKey (\_ x -> p x) m

// | /O(n)/. Filter all keys\/values that satisfy some predicate.
//
// > filterWithKey (\k _ -> k > 4) (fromList [(5,"a"), (3,"b")]) == singleton 5 "a"
filterWithKey :: (Int a -> Bool) (IntMap a) -> IntMap a
filterWithKey predicate t
  = case t of
      Bin p m l r
        -> bin p m (filterWithKey predicate l) (filterWithKey predicate r)
      Tip k x
        | predicate k x -> t
        | otherwise     -> Nil
      Nil -> Nil

// | /O(n)/. Partition the map according to some predicate. The first
// map contains all elements that satisfy the predicate, the second all
// elements that fail the predicate. See also 'split'.
//
// > partition (> "a") (fromList [(5,"a"), (3,"b")]) == (singleton 3 "b", singleton 5 "a")
// > partition (< "x") (fromList [(5,"a"), (3,"b")]) == (fromList [(3, "b"), (5, "a")], empty)
// > partition (> "x") (fromList [(5,"a"), (3,"b")]) == (empty, fromList [(3, "b"), (5, "a")])
partition :: (a -> Bool) (IntMap a) -> (IntMap a,IntMap a)
partition p m
  = partitionWithKey (\_ x -> p x) m

// | /O(n)/. Partition the map according to some predicate. The first
// map contains all elements that satisfy the predicate, the second all
// elements that fail the predicate. See also 'split'.
//
// > partitionWithKey (\ k _ -> k > 3) (fromList [(5,"a"), (3,"b")]) == (singleton 5 "a", singleton 3 "b")
// > partitionWithKey (\ k _ -> k < 7) (fromList [(5,"a"), (3,"b")]) == (fromList [(3, "b"), (5, "a")], empty)
// > partitionWithKey (\ k _ -> k > 7) (fromList [(5,"a"), (3,"b")]) == (empty, fromList [(3, "b"), (5, "a")])
partitionWithKey :: (Int a -> Bool) (IntMap a) -> (IntMap a,IntMap a)
partitionWithKey predicate (Bin p m l r)
  # (l1, l2) = partitionWithKey predicate l
  # (r1, r2) = partitionWithKey predicate r
  = (bin p m l1 r1, bin p m l2 r2)
partitionWithKey predicate t=:(Tip k x)
  | predicate k x = (t, Nil)
  | otherwise     = (Nil, t)
partitionWithKey predicate _ = (Nil, Nil)

// | /O(n)/. Map values and collect the 'Just' results.
//
// > let f x = if x == "a" then Just "new a" else Nothing
// > mapMaybe f (fromList [(5,"a"), (3,"b")]) == singleton 5 "new a"
mapMaybe :: (a -> Maybe b) (IntMap a) -> IntMap b
mapMaybe f m = mapMaybeWithKey (\_ x -> f x) m

// | /O(n)/. Map keys\/values and collect the 'Just' results.
//
// > let f k _ = if k < 5 then Just ("key : " ++ (show k)) else Nothing
// > mapMaybeWithKey f (fromList [(5,"a"), (3,"b")]) == singleton 3 "key : 3"
mapMaybeWithKey :: (Int a -> Maybe b) (IntMap a) -> IntMap b
mapMaybeWithKey f (Bin p m l r)
  = bin p m (mapMaybeWithKey f l) (mapMaybeWithKey f r)
mapMaybeWithKey f (Tip k x) = case f k x of
  Just y  -> Tip k y
  Nothing -> Nil
mapMaybeWithKey _ Nil = Nil

// | /O(n)/. Map values and separate the 'Left' and 'Right' results.
//
// > let f a = if a < "c" then Left a else Right a
// > mapEither f (fromList [(5,"a"), (3,"b"), (1,"x"), (7,"z")])
// >     == (fromList [(3,"b"), (5,"a")], fromList [(1,"x"), (7,"z")])
// >
// > mapEither (\ a -> Right a) (fromList [(5,"a"), (3,"b"), (1,"x"), (7,"z")])
// >     == (empty, fromList [(5,"a"), (3,"b"), (1,"x"), (7,"z")])
mapEither :: (a -> Either b c) (IntMap a) -> (IntMap b, IntMap c)
mapEither f m
  = mapEitherWithKey (\_ x -> f x) m

// | /O(n)/. Map keys\/values and separate the 'Left' and 'Right' results.
//
// > let f k a = if k < 5 then Left (k * 2) else Right (a ++ a)
// > mapEitherWithKey f (fromList [(5,"a"), (3,"b"), (1,"x"), (7,"z")])
// >     == (fromList [(1,2), (3,6)], fromList [(5,"aa"), (7,"zz")])
// >
// > mapEitherWithKey (\_ a -> Right a) (fromList [(5,"a"), (3,"b"), (1,"x"), (7,"z")])
// >     == (empty, fromList [(1,"x"), (3,"b"), (5,"a"), (7,"z")])
mapEitherWithKey :: (Int a -> Either b c) (IntMap a) -> (IntMap b, IntMap c)
mapEitherWithKey f (Bin p m l r)
  # (l1, l2) = mapEitherWithKey f l
  # (r1, r2) = mapEitherWithKey f r
  = (bin p m l1 r1, bin p m l2 r2)
mapEitherWithKey f (Tip k x)
  = case f k x of
      Left y  -> (Tip k y, Nil)
      Right z -> (Nil, Tip k z)
mapEitherWithKey _ Nil = (Nil, Nil)

// | /O(min(n,W))/. The expression (@'split' k map@) is a pair @(map1,map2)@
// where all keys in @map1@ are lower than @k@ and all keys in
// @map2@ larger than @k@. Any key equal to @k@ is found in neither @map1@ nor @map2@.
//
// > split 2 (fromList [(5,"a"), (3,"b")]) == (empty, fromList [(3,"b"), (5,"a")])
// > split 3 (fromList [(5,"a"), (3,"b")]) == (empty, singleton 5 "a")
// > split 4 (fromList [(5,"a"), (3,"b")]) == (singleton 3 "b", singleton 5 "a")
// > split 5 (fromList [(5,"a"), (3,"b")]) == (singleton 3 "b", empty)
// > split 6 (fromList [(5,"a"), (3,"b")]) == (fromList [(3,"b"), (5,"a")], empty)
split :: Int (IntMap a) -> (IntMap a, IntMap a)
split k t =
  case t of
      Bin _ m l r
          | m < 0 -> if (k >= 0) // handle negative numbers.
                       (case go k l of
                          (lt, gt)
                            #! lt` = union r lt
                            = (lt`, gt))
                       (case go k r of
                          (lt, gt)
                            #! gt` = union gt l
                            = (lt, gt`))
      _ -> case go k t of
             (lt, gt) -> (lt, gt)
  where
    go k` t`=:(Bin p m l r)
      | nomatch k` p m = if (k` > p) (t`, Nil) (Nil, t`)
      | zero k` m = case go k` l of (lt, gt) -> (lt, union gt r)
      | otherwise = case go k` r of (lt, gt) -> (union l lt, gt)
    go k` t`=:(Tip ky _)
      | k` > ky   = (t`, Nil)
      | k` < ky   = (Nil, t`)
      | otherwise = (Nil, Nil)
    go _ Nil = (Nil, Nil)

// | /O(min(n,W))/. Performs a 'split' but also returns whether the pivot
// key was found in the original map.
//
// > splitLookup 2 (fromList [(5,"a"), (3,"b")]) == (empty, Nothing, fromList [(3,"b"), (5,"a")])
// > splitLookup 3 (fromList [(5,"a"), (3,"b")]) == (empty, Just "b", singleton 5 "a")
// > splitLookup 4 (fromList [(5,"a"), (3,"b")]) == (singleton 3 "b", Nothing, singleton 5 "a")
// > splitLookup 5 (fromList [(5,"a"), (3,"b")]) == (singleton 3 "b", Just "a", empty)
// > splitLookup 6 (fromList [(5,"a"), (3,"b")]) == (fromList [(3,"b"), (5,"a")], Nothing, empty)
splitLookup :: Int (IntMap a) -> (IntMap a, Maybe a, IntMap a)
splitLookup k t =
  case t of
      Bin _ m l r
          | m < 0 -> if (k >= 0) // handle negative numbers.
                     (case go k l of
                         (lt, fnd, gt)
                            #! lt` = union r lt
                            = (lt`, fnd, gt))
                     (case go k r of
                         (lt, fnd, gt)
                           #! gt` = union gt l
                           = (lt, fnd, gt`))
      _ -> go k t
  where
    go k` t`=:(Bin p m l r)
        | nomatch k` p m = if (k` > p) (t`, Nothing, Nil) (Nil, Nothing, t`)
        | zero k` m      = case go k` l of
            (lt, fnd, gt)
              #! gt` = union gt r
              = (lt, fnd, gt`)
        | otherwise      = case go k` r of
            (lt, fnd, gt)
              #! lt` = union l lt
              = (lt`, fnd, gt)
    go k` t`=:(Tip ky y)
      | k` > ky   = (t`, Nothing, Nil)
      | k` < ky   = (Nil, Nothing, t`)
      | otherwise = (Nil, Just y, Nil)
    go _ Nil = (Nil, Nothing, Nil)

// | /O(n)/. Fold the values in the map using the given right-associative
// binary operator, such that @'foldr' f z == 'Prelude.foldr' f z . 'elems'@.
//
// For example,
//
// > elems map = foldr (:) [] map
//
// > let f a len = len + (length a)
// > foldr f 0 (fromList [(5,"a"), (3,"bbb")]) == 4
foldr :: (a b -> b) b (IntMap a) -> b
foldr f z t =
  case t of Bin _ m l r | m < 0 -> go (go z l) r // put negative numbers before
                        | otherwise -> go (go z r) l
            _ -> go z t
  where
    go z` Nil           = z`
    go z` (Tip _ x)     = f x z`
    go z` (Bin _ _ l r) = go (go z` r) l

// | /O(n)/. A strict version of 'foldr'. Each application of the operator is
// evaluated before using the result in the next application. This
// function is strict in the starting value.
foldr` :: (a b -> b) b (IntMap a) -> b
foldr` f z t =
  case t of Bin _ m l r | m < 0 -> go f (go f z l) r // put negative numbers before
                        | otherwise -> go f (go f z r) l
            _ -> go f z t
  where
  go :: (a b -> b) !b (IntMap a) -> b
  go _ z` Nil           = z`
  go f z` (Tip _ x)     = f x z`
  go f z` (Bin _ _ l r) = go f (go f z` r) l

// | /O(n)/. Fold the values in the map using the given left-associative
// binary operator, such that @'foldl' f z == 'Prelude.foldl' f z . 'elems'@.
//
// For example,
//
// > elems = reverse . foldl (flip (:)) []
//
// > let f len a = len + (length a)
// > foldl f 0 (fromList [(5,"a"), (3,"bbb")]) == 4
/*foldl :: (a  b -> a) a (IntMap b) -> a
foldl f z t =
  case t of Bin _ m l r | m < 0 -> go (go z r) l // put negative numbers before
                        | otherwise -> go (go z l) r
            _ -> go z t
  where
    go z` Nil           = z`
    go z` (Tip _ x)     = f z` x
    go z` (Bin _ _ l r) = go (go z` l) r*/

// | /O(n)/. A strict version of 'foldl'. Each application of the operator is
// evaluated before using the result in the next application. This
// function is strict in the starting value.
foldl` :: (a b -> a) a (IntMap b) -> a
foldl` f z t =
  case t of Bin _ m l r | m < 0 -> go f (go f z r) l // put negative numbers before
                        | otherwise -> go f (go f z l) r
            _ -> go f z t
  where
  go :: (a b -> a) !a (IntMap b) -> a
  go _ z` Nil           = z`
  go f z` (Tip _ x)     = f z` x
  go f z` (Bin _ _ l r) = go f (go f z` l) r

// | /O(n)/. Fold the keys and values in the map using the given right-associative
// binary operator, such that
// @'foldrWithKey' f z == 'Prelude.foldr' ('uncurry' f) z . 'toAscList'@.
//
// For example,
//
// > keys map = foldrWithKey (\k x ks -> k:ks) [] map
//
// > let f k a result = result ++ "(" ++ (show k) ++ ":" ++ a ++ ")"
// > foldrWithKey f "Map: " (fromList [(5,"a"), (3,"b")]) == "Map: (5:a)(3:b)"
foldrWithKey :: (Int a b -> b) b !(IntMap a) -> b
foldrWithKey f z t =
  case t of Bin _ m l r | m < 0 -> go (go z l) r // put negative numbers before
                        | otherwise -> go (go z r) l
            _ -> go z t
  where
    go z` Nil           = z`
    go z` (Tip kx x)    = f kx x z`
    go z` (Bin _ _ l r) = go (go z` r) l

// | /O(n)/. A strict version of 'foldrWithKey'. Each application of the operator is
// evaluated before using the result in the next application. This
// function is strict in the starting value.
foldrWithKey` :: (Int a b -> b) b (IntMap a) -> b
foldrWithKey` f z t =
  case t of Bin _ m l r | m < 0 -> go f (go f z l) r // put negative numbers before
                        | otherwise -> go f (go f z r) l
            _ -> go f z t
  where
  go :: (Int a b -> b) !b (IntMap a) -> b
  go _ z` Nil           = z`
  go f z` (Tip kx x)    = f kx x z`
  go f z` (Bin _ _ l r) = go f (go f z` r) l

// | /O(n)/. Fold the keys and values in the map using the given left-associative
// binary operator, such that
// @'foldlWithKey' f z == 'Prelude.foldl' (\\z' (kx, x) -> f z' kx x) z . 'toAscList'@.
//
// For example,
//
// > keys = reverse . foldlWithKey (\ks k x -> k:ks) []
//
// > let f result k a = result ++ "(" ++ (show k) ++ ":" ++ a ++ ")"
// > foldlWithKey f "Map: " (fromList [(5,"a"), (3,"b")]) == "Map: (3:b)(5:a)"
foldlWithKey :: (a Int b -> a) a (IntMap b) -> a
foldlWithKey f z t =
  case t of Bin _ m l r | m < 0 -> go (go z r) l // put negative numbers before
                        | otherwise -> go (go z l) r
            _ -> go z t
  where
    go z` Nil           = z`
    go z` (Tip kx x)    = f z` kx x
    go z` (Bin _ _ l r) = go (go z` l) r

// | /O(n)/. A strict version of 'foldlWithKey'. Each application of the operator is
// evaluated before using the result in the next application. This
// function is strict in the starting value.
foldlWithKey` :: (a Int b -> a) a (IntMap b) -> a
foldlWithKey` f z t =
  case t of Bin _ m l r | m < 0 -> go f (go f z r) l // put negative numbers before
                        | otherwise -> go f (go f z l) r
            _ -> go f z t
  where
  go :: (a Int b -> a) !a (IntMap b) -> a
  go f z` Nil           = z`
  go f z` (Tip kx x)    = f z` kx x
  go f z` (Bin _ _ l r) = go f (go f z` l) r

// | /O(n)/. Fold the keys and values in the map using the given monoid, such that
//
// @'foldMapWithKey' f = 'Prelude.fold' . 'mapWithKey' f@
//
// This can be an asymptotically faster than 'foldrWithKey' or 'foldlWithKey' for some monoids.
foldMapWithKey :: (Int a -> m) (IntMap a) -> m | Monoid m
foldMapWithKey _ Nil           = mempty
foldMapWithKey f (Tip kx x)    = f kx x
foldMapWithKey f (Bin _ _ l r) = mappend (foldMapWithKey f l) (foldMapWithKey f r)

// | /O(n)/.
// Return all elements of the map in the ascending order of their keys.
// Subject to list fusion.
//
// > elems (fromList [(5,"a"), (3,"b")]) == ["b","a"]
// > elems empty == []
elems :: (IntMap a) -> [a]
elems m = foldr (\x xs -> [x:xs]) [] m

// | /O(n)/. Return all keys of the map in ascending order. Subject to list
// fusion.
//
// > keys (fromList [(5,"a"), (3,"b")]) == [3,5]
// > keys empty == []
keys  :: (IntMap a) -> [Int]
keys m = foldrWithKey (\k _ ks -> [k : ks]) [] m

// | /O(n)/. An alias for 'toAscList'. Returns all key\/value pairs in the
// map in ascending key order. Subject to list fusion.
//
// > assocs (fromList [(5,"a"), (3,"b")]) == [(3,"b"), (5,"a")]
// > assocs empty == []
assocs :: (IntMap a) -> [(Int,a)]
assocs m = toAscList m

// | /O(n*min(n,W))/. The set of all keys of the map.
//
// > keysSet (fromList [(5,"a"), (3,"b")]) == Data.IntSet.fromList [3,5]
// > keysSet empty == Data.IntSet.empty
// TODO
//keysSet :: (IntMap a) -> IntSet.IntSet
//keysSet Nil = IntSet.Nil
//keysSet (Tip kx _) = IntSet.singleton kx
//keysSet (Bin p m l r)
  //| m .&. IntSet.suffixBitMask == 0 = IntSet.Bin p m (keysSet l) (keysSet r)
  //| otherwise = IntSet.Tip (p .&. IntSet.prefixBitMask) (computeBm (computeBm 0 l) r)
  //where
  // TODO 1st strict
        //computeBm acc (Bin _ _ l` r`) = computeBm (computeBm acc l`) r`
        //computeBm acc (Tip kx _) = acc .|. IntSet.bitmapOf kx
        //computeBm _   Nil = abort "Data.IntSet.keysSet: Nil"

// | /O(n)/. Build a map from a set of keys and a function which for each key
// computes its value.
//
// > fromSet (\k -> replicate k 'a') (Data.IntSet.fromList [3, 5]) == fromList [(5,"aaaaa"), (3,"aaa")]
// > fromSet undefined Data.IntSet.empty == empty
// TODO
//fromSet :: (Int -> a) IntSet.IntSet -> IntMap a
//fromSet _ IntSet.Nil = Nil
//fromSet f (IntSet.Bin p m l r) = Bin p m (fromSet f l) (fromSet f r)
//fromSet f (IntSet.Tip kx bm) = buildTree f kx bm (IntSet.suffixBitMask + 1)
  //where // This is slightly complicated, as we to convert the dense
        //// representation of IntSet into tree representation of IntMap.
        ////
        //// We are given a nonzero bit mask 'bmask' of 'bits' bits with prefix 'prefix'.
        //// We split bmask into halves corresponding to left and right subtree.
        //// If they are both nonempty, we create a Bin node, otherwise exactly
        //// one of them is nonempty and we construct the IntMap from that half.
        //buildTree g prefix bmask bits = prefix `seq` bmask `seq` case bits of
          //0 -> Tip prefix (g prefix)
          //_ -> case (bits `shiftRL` 1) of
                 //bits2 | bmask .&. ((1 `shiftLL` bits2) - 1) == 0 ->
                           //buildTree g (prefix + bits2) (bmask `shiftRL` bits2) bits2
                       //| (bmask `shiftRL` bits2) .&. ((1 `shiftLL` bits2) - 1) == 0 ->
                           //buildTree g prefix bmask bits2
                       //| otherwise ->
                           //Bin prefix bits2 (buildTree g prefix bmask bits2) (buildTree g (prefix + bits2) (bmask `shiftRL` bits2) bits2)

// | /O(n)/. Convert the map to a list of key\/value pairs. Subject to list
// fusion.
//
// > toList (fromList [(5,"a"), (3,"b")]) == [(3,"b"), (5,"a")]
// > toList empty == []
toList :: (IntMap a) -> [(Int,a)]
toList m = toAscList m

// | /O(n)/. Convert the map to a list of key\/value pairs where the
// keys are in ascending order. Subject to list fusion.
//
// > toAscList (fromList [(5,"a"), (3,"b")]) == [(3,"b"), (5,"a")]
toAscList :: (IntMap a) -> [(Int,a)]
toAscList m = foldrWithKey (\k x xs -> [(k,x):xs]) [] m

// | /O(n)/. Convert the map to a list of key\/value pairs where the keys
// are in descending order. Subject to list fusion.
//
// > toDescList (fromList [(5,"a"), (3,"b")]) == [(5,"a"), (3,"b")]
toDescList :: (IntMap a) -> [(Int,a)]
toDescList m = foldlWithKey (\xs k x -> [(k,x):xs]) [] m

// | /O(n*min(n,W))/. Create a map from a list of key\/value pairs.
//
// > fromList [] == empty
// > fromList [(5,"a"), (3,"b"), (5, "c")] == fromList [(5,"c"), (3,"b")]
// > fromList [(5,"c"), (3,"b"), (5, "a")] == fromList [(5,"a"), (3,"b")]
fromList :: [(Int,a)] -> IntMap a
fromList xs
  = foldl ins empty xs
  where
    ins t (k,x)  = insert k x t

// | /O(n*min(n,W))/. Create a map from a list of key\/value pairs with a combining function. See also 'fromAscListWith'.
//
// > fromListWith (++) [(5,"a"), (5,"b"), (3,"b"), (3,"a"), (5,"c")] == fromList [(3, "ab"), (5, "cba")]
// > fromListWith (++) [] == empty
fromListWith :: (a a -> a) [(Int,a)] -> IntMap a
fromListWith f xs
  = fromListWithKey (\_ x y -> f x y) xs

//// | /O(n*min(n,W))/. Build a map from a list of key\/value pairs with a combining function. See also fromAscListWithKey'.
////
//// > let f key new_value old_value = (show key) ++ ":" ++ new_value ++ "|" ++ old_value
//// > fromListWithKey f [(5,"a"), (5,"b"), (3,"b"), (3,"a"), (5,"c")] == fromList [(3, "3:a|b"), (5, "5:c|5:b|a")]
//// > fromListWithKey f [] == empty
fromListWithKey :: (Int a a -> a) [(Int,a)] -> IntMap a
fromListWithKey f xs
  = foldl ins empty xs
  where
    ins t (k,x) = insertWithKey f k x t

// | /O(n)/. Build a map from a list of key\/value pairs where
// the keys are in ascending order.
//
// > fromAscList [(3,"b"), (5,"a")]          == fromList [(3, "b"), (5, "a")]
// > fromAscList [(3,"b"), (5,"a"), (5,"b")] == fromList [(3, "b"), (5, "b")]
fromAscList :: [(Int,a)] -> IntMap a
fromAscList xs
  = fromAscListWithKey (\_ x _ -> x) xs

// | /O(n)/. Build a map from a list of key\/value pairs where
// the keys are in ascending order, with a combining function on equal keys.
// /The precondition (input list is ascending) is not checked./
//
// > fromAscListWith (++) [(3,"b"), (5,"a"), (5,"b")] == fromList [(3, "b"), (5, "ba")]
fromAscListWith :: (a a -> a) [(Int,a)] -> IntMap a
fromAscListWith f xs
  = fromAscListWithKey (\_ x y -> f x y) xs

// | /O(n)/. Build a map from a list of key\/value pairs where
// the keys are in ascending order, with a combining function on equal keys.
// /The precondition (input list is ascending) is not checked./
//
// > let f key new_value old_value = (show key) ++ ":" ++ new_value ++ "|" ++ old_value
// > fromAscListWithKey f [(3,"b"), (5,"a"), (5,"b")] == fromList [(3, "b"), (5, "5:b|a")]
fromAscListWithKey :: (Int a a -> a) [(Int,a)] -> IntMap a
fromAscListWithKey _ []         = Nil
fromAscListWithKey f [x0 : xs0] = fromDistinctAscList (combineEq x0 xs0)
  where
    // [combineEq f xs] combines equal elements with function [f] in an ordered list [xs]
    combineEq z [] = [z]
    combineEq z=:(kz,zz) [x=:(kx,xx):xs]
      | kx==kz    = let yy = f kx xx zz in combineEq (kx,yy) xs
      | otherwise = [z:combineEq x xs]

// | /O(n)/. Build a map from a list of key\/value pairs where
// the keys are in ascending order and all distinct.
// /The precondition (input list is strictly ascending) is not checked./
//
// > fromDistinctAscList [(3,"b"), (5,"a")] == fromList [(3, "b"), (5, "a")]
fromDistinctAscList :: ![(Int, a)] -> IntMap a
fromDistinctAscList []         = Nil
fromDistinctAscList [z0 : zs0] = work z0 zs0 Nada
  where
    work :: !(!Int, !a) ![(Int, a)] !(Stack a) -> IntMap a
    work (kx,vx) []             stk = finish kx (Tip kx vx) stk
    work (kx,vx) [z=:(kz,_):zs] stk = reduce z zs (branchMask kx kz) kx (Tip kx vx) stk

    reduce :: !(!Int, !a) ![(Int, a)] !Mask !Prefix !(IntMap a) !(Stack a) -> IntMap a
    reduce z zs _ px tx Nada = work z zs (Push px tx Nada)
    reduce z zs m px tx stk=:(Push py ty stk`) =
        let mxy = branchMask px py
            pxy = mask px mxy
        in  if (shorter m mxy)
                 (reduce z zs m pxy (Bin pxy mxy ty tx) stk`)
                 (work z zs (Push px tx stk))

    finish :: !Prefix !(IntMap a) !(Stack a) -> IntMap a
    finish _  t  Nada = t
    finish px tx (Push py ty stk) = finish p (link py ty px tx) stk
        where m = branchMask px py
              p = mask px m

:: Stack a = Push !Prefix !(IntMap a) !(Stack a) | Nada

instance == (IntMap a) | == a where
  (==) t1 t2  = equal t1 t2

equal :: !(IntMap a) !(IntMap a) -> Bool | == a
equal (Bin p1 m1 l1 r1) (Bin p2 m2 l2 r2)
  = (m1 == m2) && (p1 == p2) && (equal l1 l2) && (equal r1 r2)
equal (Tip kx x) (Tip ky y)
  = (kx == ky) && (x==y)
equal Nil Nil = True
equal _   _   = False

nequal :: (IntMap a) (IntMap a) -> Bool | Eq a
nequal (Bin p1 m1 l1 r1) (Bin p2 m2 l2 r2)
  = (m1 <> m2) || (p1 <> p2) || (nequal l1 l2) || (nequal r1 r2)
nequal (Tip kx x) (Tip ky y)
  = (kx <> ky) || (x <> y)
nequal Nil Nil = False
nequal _   _   = True

link :: !Prefix !(IntMap a) !Prefix !(IntMap a) -> IntMap a
link p1 t1 p2 t2
  | zero p1 m = Bin p m t1 t2
  | otherwise = Bin p m t2 t1
  where
    m = branchMask p1 p2
    p = mask p1 m

bin :: !Prefix !Mask !(IntMap a) !(IntMap a) -> IntMap a
bin _ _ l Nil = l
bin _ _ Nil r = r
bin p m l r   = Bin p m l r

zero :: !Int !Mask -> Bool
zero i m = (i bitand m) == 0

nomatch :: !Int !Prefix !Mask -> Bool
nomatch i p m = mask i m <> p

match :: !Int !Prefix !Mask -> Bool
match i p m = mask i m == p

mask :: !Int !Mask -> Prefix
mask i m = i bitand (~m bitxor m)

// we have to treat the masks as unsigned ints
// this means that the sign bit has to be inverted to preserve order
shorter :: !Mask !Mask -> Bool
shorter m1 m2 = (m1 bitxor signBitOnly) > (m2 bitxor signBitOnly)

branchMask :: !Prefix !Prefix -> Mask
branchMask p1 p2 = highestBitMask (p1 bitxor p2)

highestBitMask :: !Int -> Int
highestBitMask x0 =
	// for the right shift `x6` has to be treated as unsigned int, so the highest bit has to be set to 0
	x6 bitxor (allExceptSignBit bitand (x6 >> 1))
where
	x1 = x0 bitor (x0 >> 1)
	x2 = x1 bitor (x1 >> 2)
	x3 = x2 bitor (x2 >> 4)
	x4 = x3 bitor (x3 >> 8)
	x5 = x4 bitor (x4 >> 16)
	x6 = x5 bitor (x5 >> 32)

signBitOnly      =: ~2^(IF_INT_64_OR_32 63 31)
allExceptSignBit =: bitnot signBitOnly
