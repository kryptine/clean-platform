definition module Data.Array

from StdArray import class Array

mapArrSt :: !(.a *st -> *(!.a, !*st)) !*(arr .a) !*st -> *(!*(arr .a), !*st) | Array arr a

foldrArr :: !(a .b -> .b) !.b !.(arr a) -> .b | Array arr a
