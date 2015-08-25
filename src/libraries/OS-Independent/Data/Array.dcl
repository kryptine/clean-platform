definition module Data.Array

from StdArray import class Array

mapArrSt :: !(.a *st -> *(!.a, !*st)) !*(arr .a) !*st -> *(!*(arr .a), !*st) | Array arr a

foldrArr :: !(a .b -> .b) !.b !.(arr a) -> .b | Array arr a

foldrArrWithKey :: !(Int a .b -> .b) !.b !.(arr a) -> .b | Array arr a

foldrUArr :: !(a -> .(.b -> .(*(arr a) -> *(.b, *(arr a))))) .b *(arr a)
          -> *(.b, *(arr a)) | Array arr a

foldrUArrWithKey :: !(Int a -> .(.b -> .(*(arr a) -> *(.b, *(arr a))))) .b *(arr a)
                 -> *(.b, *(arr a)) | Array arr a

foldlArr :: !(.b a -> .b) !.b !.(arr a) -> .b | Array arr a

foldlArrWithKey :: !(Int .b a -> .b) !.b !.(arr a) -> .b | Array arr a
