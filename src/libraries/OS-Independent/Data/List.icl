implementation module Data.List

import Data.Maybe, StdTuple, StdBool, StdEnum, StdFunc, StdList, StdOrdList, Data.Functor

// Haskell Data.List compat

head :: ![.a] -> .a
head xs = hd xs

tail :: !u:[.a] -> u:[.a]
tail xs = tl xs

null :: ![.a] -> Bool
null xs = isEmpty xs

product :: !.[a] -> a | * , one  a
product xs = prod xs

// End Haskell Data.List compat

unzip3 :: ![(.a,.b,.c)] -> ([.a],[.b],[.c])
unzip3 []        = ([], [], [])
unzip3 [(x,y,z) : xyzs]  = ([x : xs],[y : ys],[z : zs])
where
  (xs,ys,zs) = unzip3 xyzs

unzip4 :: ![(.a,.b,.c,.d)] -> ([.a],[.b],[.c],[.d])
unzip4 []          = ([], [], [], [])
unzip4 [(w,x,y,z) : wxyzs]  = ([w : ws],[x : xs],[y : ys],[z : zs])
where
  (ws,xs,ys,zs) = unzip4 wxyzs
unzip5 :: ![(.a,.b,.c,.d,.e)] -> ([.a],[.b],[.c],[.d],[.e])
unzip5 []            = ([], [], [], [], [])
unzip5 [(v,w,x,y,z) : vwxyzs]  = ([v : vs],[w : ws],[x : xs],[y : ys],[z : zs])
where
  (vs,ws,xs,ys,zs) = unzip5 vwxyzs

replaceInList :: !(a a -> Bool) !a ![a] -> [a]
replaceInList cond new []         = [new]
replaceInList cond new [x:xs]
    | cond new x            = [new : xs]
    | otherwise             = [x : replaceInList cond new xs]

splitWith :: !(a -> Bool) ![a] -> (![a],![a])
splitWith f [] = ([],[])
splitWith f [x:xs]
  | f x  = let (y,n) = splitWith f xs in ([x:y],n)
      = let (y,n)  = splitWith f xs in (y,[x:n])

sortByIndex :: ![(!Int,!a)] -> [a]
sortByIndex l = map snd (sortBy (\(a,_) (b,_) -> a < b) l)

intersperse :: !a ![a] -> [a]
intersperse i []      = []
intersperse i [x]     = [x]
intersperse i [x:xs]  = [x,i:intersperse i xs]

intercalate :: .[a] [.[a]] -> .[a]
intercalate xs xss = flatten (intersperse xs xss)

transpose :: ![[a]] -> [.[a]]
transpose []  = []
transpose [[]     : xss] = transpose xss
transpose [[x:xs] : xss] = [[x : [h \\ [h:t] <- xss]] : transpose [xs : [t \\ [h:t] <- xss]]]

subsequences :: .[a] -> .[[a]]
subsequences xs = [[] : nonEmptySubsequences xs]

nonEmptySubsequences :: .[a] -> .[[a]]
nonEmptySubsequences []      =  []
nonEmptySubsequences [x:xs]  =  [[x] : foldr f [] (nonEmptySubsequences xs)]
  where f ys r = [ys : [x : ys] : r]

permutations :: [a] -> .[[a]]
permutations xs0        =  [xs0 : perms xs0 []]
  where
    perms []     _  = []
    perms [t:ts] is = foldr interleave (perms ts [t:is]) (permutations is)
      where interleave    xs     r = let (_,zs) = interleave` id xs r in zs
            interleave` _ []     r = (ts, r)
            interleave` f [y:ys] r = let (us,zs) = interleave` (f o (\xs -> [y:xs])) ys r
                                     in  ([y:us], [f [t:y:us] : zs])

foldl1 :: (.a -> .(.a -> .a)) [.a] -> .a
foldl1 f [x:xs]         =  foldl f x xs

concatMap :: (.a -> [.b]) [.a] -> [.b]
concatMap f ls = flatten (map f ls)

maximum :: .[a] -> a | < a
maximum [x]     = x
maximum [x:xs]  = max x (maximum xs)

minimum :: .[a] -> a | Ord a
minimum xs =  foldl1 min xs

getItems :: ![a] ![Int] -> [a]
getItems list indexes = [x \\ x <- list & idx <- [0..] | isMember idx indexes]

instance Functor []
where
  fmap f l = [f e \\ e <- l]

scanl :: (a -> .(.b -> a)) a [.b] -> .[a]
scanl f q ls            =  [q : (case ls of
                                  []     -> []
                                  [x:xs] -> scanl f (f q x) xs)]

scanl1 :: (a -> .(a -> a)) .[a] -> .[a]
scanl1 f [x:xs]         =  scanl f x xs
scanl1 _ []             =  []

foldr1 :: (.a -> .(.a -> .a)) [.a] -> .a
foldr1 _ [x]            =  x
foldr1 f [x:xs]         =  f x (foldr1 f xs)

replicate :: .Int a -> .[a]
replicate n x           =  take n (repeat x)

cycle :: .[a] -> [a]
cycle xs                = xs`
  where xs` = xs ++ xs`

unfoldr :: (.a -> Maybe (.b,.a)) .a -> [.b]
unfoldr f b  =
  case f b of
   Just (a,new_b) -> [a : unfoldr f new_b]
   Nothing        -> []

break :: (a -> .Bool) .[a] -> .([a],[a])
break _ xs=:[]           =  (xs, xs)
break p xs=:[x:xs`]
           | p x        =  ([],xs)
           | otherwise  =  let (ys,zs) = break p xs` in ([x:ys],zs)

stripPrefix :: .[a] u:[a] -> Maybe v:[a] | == a, [u <= v]
stripPrefix [] ys = Just ys
stripPrefix [x:xs] [y:ys]
 | x == y = stripPrefix xs ys
stripPrefix _ _ = Nothing

group :: .(.[a] -> [.[a]]) | == a
group                   =  groupBy (==)

groupBy :: (a -> a -> .Bool) .[a] -> [.[a]]
groupBy _  []           =  []
groupBy eq [x:xs]       =  [[x:ys] : groupBy eq zs]
                           where (ys,zs) = span (eq x) xs

inits :: .[a] -> [.[a]]
inits xs                =  [[] : case xs of
                                  []        -> []
                                  [x : xs`] -> map (\ys -> [x : ys]) (inits xs`)]

tails :: [a] -> .[[a]]
tails xs                =  [xs : case xs of
                                  []        -> []
                                  [_ : xs`] -> tails xs`]

isPrefixOf :: .[a] .[a] -> .Bool | == a
isPrefixOf [] _          =  True
isPrefixOf _  []         =  False
isPrefixOf [x:xs] [y:ys] =  x == y && isPrefixOf xs ys

isSuffixOf :: .[a] .[a] -> .Bool | == a
isSuffixOf x y          =  isPrefixOf (reverse x) (reverse y)

isInfixOf :: .[a] .[a] -> Bool | == a
isInfixOf needle haystack = any (isPrefixOf needle) (tails haystack)

elem :: a .[a] -> .Bool | == a
elem _ []       = False
elem x [y:ys]   = x == y || elem x ys

notElem :: a .[a] -> .Bool | == a
notElem _ []     =  True
notElem x [y:ys] =  x <> y && notElem x ys

lookup :: a [(a,.b)] -> Maybe .b | == a
lookup  _   []          =  Nothing
lookup  key [(x,y):xys]
    | key == x          =  Just y
    | otherwise         =  lookup key xys

find :: (a -> .Bool) -> .(.[a] -> .(Maybe a))
find p          = listToMaybe o filter p

partition :: (a -> .Bool) .[a] -> (.[a],.[a])
partition p xs = foldr (select p) ([],[]) xs
  where select :: .(a -> .Bool) a (u:[a],v:[a]) -> (w:[a],x:[a]), [u <= w,v <= x]
        select p x t =
          let (ts,fs) = t
          in if (p x) ([x:ts],fs) (ts, [x:fs])

elemIndex :: a -> .(.[a] -> .(Maybe Int)) | == a
elemIndex x     = findIndex (\y -> x==y)

elemIndices :: a -> .(.[a] -> .[Int]) | == a
elemIndices x   = findIndices (\y -> x==y)

findIndex :: (.a -> .Bool) -> .([.a] -> .(Maybe Int))
findIndex p     = listToMaybe o findIndices p

findIndices :: (.a -> .Bool) [.a] -> .[Int]
findIndices p xs = [ i \\ (x,i) <- zip2 xs [0..] | p x]

zip3 :: ![.a] [.b] [.c] -> [(.a,.b,.c)]
zip3 [a:as] [b:bs] [c:cs]
  = [(a, b, c): zip3 as bs cs]
zip3 _ _ _
  = []

zip4 :: ![.a] [.b] [.c] [.d] -> [(.a,.b,.c,.d)]
zip4 [a:as] [b:bs] [c:cs] [d:ds]
  = [(a, b, c, d): zip4 as bs cs ds]
zip4 _ _ _ _
  = []

zip5 :: ![.a] [.b] [.c] [.d] [.e] -> [(.a,.b,.c,.d,.e)]
zip5 [a:as] [b:bs] [c:cs] [d:ds] [e:es]
  = [(a, b, c, d, e): zip5 as bs cs ds es]
zip5 _ _ _ _ _
  = []

zipWith :: (.a -> .(.b -> .h)) ![.a] [.b] -> [.h]
zipWith z [a:as] [b:bs]
                   = [ z a b : zipWith z as bs]
zipWith _ _ _ = []

zipWith3 :: (.a -> .(.b -> .(.c -> .h))) ![.a] [.b] [.c] -> [.h]
zipWith3 z [a:as] [b:bs] [c:cs]
                   = [ z a b c : zipWith3 z as bs cs]
zipWith3 _ _ _ _ = []

zipWith4 :: (.a -> .(.b -> .(.c -> .(.d -> .h))))
      ![.a] [.b] [.c] [.d] -> [.h]
zipWith4 z [a:as] [b:bs] [c:cs] [d:ds]
                   = [ z a b c d : zipWith4 z as bs cs ds]
zipWith4 _ _ _ _ _ = []

zipWith5 :: (.a -> .(.b -> .(.c -> .(.d -> .(.e -> .h)))))
      ![.a] [.b] [.c] [.d] [.e] -> [.h]
zipWith5 z [a:as] [b:bs] [c:cs] [d:ds] [e:es]
                   = [ z a b c d e : zipWith5 z as bs cs ds es]
zipWith5 _ _ _ _ _ _ = []

nub :: .[a] -> .[a] | == a
nub l                   = nub` l []
  where
    nub` [] _           = []
    nub` [x:xs] ls
        | elem x  ls    = nub` xs ls
        | otherwise     = [x : nub` xs [x:ls]]

nubBy :: (a -> .(a -> .Bool)) .[a] -> .[a]
nubBy eq l              = nubBy` l []
  where
    nubBy` [] _         = []
    nubBy` [y:ys] xs
       | elem_by eq y xs = nubBy` ys xs
       | otherwise       = [y : nubBy` ys [y:xs]]

elem_by :: (a -> .(.b -> .Bool)) a [.b] -> .Bool
elem_by _  _ []         =  False
elem_by eq y [x:xs]     =  eq y x || elem_by eq y xs

delete :: u:(a -> v:(w:[a] -> x:[a])) | == a, [v <= u,w <= x]
delete                  =  deleteBy (==)

deleteBy :: (a -> .(b -> .Bool)) a u:[b] -> v:[b], [u <= v]
deleteBy _  _ []        = []
deleteBy eq x [y:ys]    = if (eq x y) ys [y : deleteBy eq x ys]

deleteFirstsBy :: (a -> .(b -> .Bool)) -> u:(v:[b] -> w:(.[a] -> x:[b])), [w <= u,w v <= x]
deleteFirstsBy eq       =  foldl (flip (deleteBy eq))

difference :: u:(v:[a] -> w:(.[a] -> x:[a])) | == a, [w <= u,w v <= x]
difference                    =  foldl (flip delete)

intersect :: u:(.[a] -> v:(.[a] -> .[a])) | == a, [v <= u]
intersect               =  intersectBy (==)

intersectBy :: (a -> b -> .Bool) .[a] .[b] -> .[a]
intersectBy _  [] _     =  []
intersectBy _  _  []    =  []
intersectBy eq xs ys    =  [x \\ x <- xs | any (eq x) ys]

union :: u:(.[a] -> v:(.[a] -> .[a])) | == a, [v <= u]
union                   = unionBy (==)

unionBy :: (a -> .(a -> .Bool)) .[a] .[a] -> .[a]
unionBy eq xs ys        =  xs ++ foldl (flip (deleteBy eq)) (nubBy eq ys) xs

