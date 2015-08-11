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
foldrArr f b arr = foldrArrWithKey (\_ -> f) b arr

foldrArrWithKey :: !(Int a .b -> .b) !.b !.(arr a) -> .b | Array arr a
foldrArrWithKey f b arr
  #! (arrSz, arr) = usize arr
  = foldrArr` arrSz 0 f b arr
  where
  foldrArr` :: !Int !Int !(Int a .b -> .b) !.b !.(arr a) -> .b | Array arr a
  foldrArr` arrSz idx f b arr
    | idx == arrSz = b
    | otherwise
        #! (e, arr) = arr![idx]
        = f idx e (foldrArr` arrSz (idx + 1) f b arr)

foldrUArr :: !(a -> .(.b -> .(*(arr a) -> *(.b, *(arr a))))) .b *(arr a)
          -> *(.b, *(arr a)) | Array arr a
foldrUArr f b arr = foldrUArrWithKey (\_ -> f) b arr

foldrUArrWithKey :: !(Int a -> .(.b -> .(*(arr a) -> *(.b, *(arr a))))) .b *(arr a)
                 -> *(.b, *(arr a)) | Array arr a
foldrUArrWithKey f b arr
  # (sz, arr) = usize arr
  = foldUArr` sz 0 b arr
  where
  foldUArr` sz idx b arr
    | idx == sz = (b, arr)
    | otherwise
      #! (elem, arr) = uselect arr idx
      #! (res, arr)  = foldUArr` sz (idx + 1) b arr
      = f idx elem res arr
