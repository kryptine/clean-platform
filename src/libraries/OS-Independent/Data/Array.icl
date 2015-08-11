implementation module Data.Array

import StdArray, StdInt

mapArrSt :: !(.a *st -> *(!.a, !*st)) !*(arr .a) !*st -> *(!*(arr .a), !*st) | Array arr a
mapArrSt f arr st
  #! (sz, arr) = usize arr
  = mapArrSt` sz 0 f arr st
  where
  mapArrSt` :: !Int !Int !(.a *st -> *(!.a, !*st)) !*(arr .a) !*st -> *(!*(arr .a), !*st) | Array arr a
  mapArrSt` sz idx f arr st
    | idx == sz = (arr, st)
    | otherwise
        #! (e, arr) = arr![idx]
        #! (e, st)  = f e st
        #! arr      = {arr & [idx] = e}
        = mapArrSt` sz (idx + 1) f arr st

foldrArr :: !(a .b -> .b) !.b !.(arr a) -> .b | Array arr a
foldrArr f b arr
  #! (arrSz, arr) = usize arr
  = foldrArr` arrSz 0 f b arr
  where
  foldrArr` :: !Int !Int !(a .b -> .b) !.b !.(arr a) -> .b | Array arr a
  foldrArr` arrSz idx f b arr
    | idx == arrSz = b
    | otherwise
        #! (e, arr) = arr![idx]
        = f e (foldrArr` arrSz (idx + 1) f b arr)
