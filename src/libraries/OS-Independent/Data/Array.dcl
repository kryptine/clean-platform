definition module Data.Array

from StdArray import class Array
from StdOverloaded import class +++
from Data.Functor import class Functor
from Control.Applicative import class pure, class <*>, class Applicative
from Control.Monad import class Monad

mapArrSt :: !(.a -> .(*st -> *(.a, *st))) !*(arr .a) !*st -> *(!*(arr .a), !*st) | Array arr a

foldrArr :: !(a -> .(.b -> .b)) !.b !.(arr a) -> .b | Array arr a

foldrArrWithKey :: !(Int a -> .(.b -> .b)) !.b !.(arr a) -> .b | Array arr a

foldrUArr :: !(a -> .(.b -> .(*(arr a) -> *(.b, *(arr a))))) .b !*(arr a)
          -> *(.b, *(arr a)) | Array arr a

foldrUArrWithKey :: !(Int a -> .(.b -> .(*(arr a) -> *(.b, *(arr a))))) .b !*(arr a)
                 -> *(.b, *(arr a)) | Array arr a

foldlArr :: !(.b -> .(a -> .b)) !.b !.(arr a) -> .b | Array arr a

foldlArrWithKey :: !(Int .b -> .(a -> .b)) !.b !.(arr a) -> .b | Array arr a

reverseArr :: !.(arr a) -> .arr a | Array arr a

takeArr :: !Int !.(arr a) -> .arr a | Array arr a

mapArr :: !(a -> a) !(arr a) -> arr a | Array arr a

appendArr :: !.(arr1 a) !.(arr2 a) -> .(arr3 a) | Array arr1 a & Array arr2 a & Array arr3 a

instance +++ {a}
instance +++ {!a}

instance Functor {} where fmap :: (a -> b) !{a} -> {b}
instance Functor {!} where fmap :: (a -> b) !{!a} -> {!b}
instance pure {}
instance pure {!} where pure :: !a -> {!a}
instance <*> {}, {!}
instance Monad {}, {!}

reduceArray :: ((.a -> u:(b -> b)) -> .(b -> .(c -> .a))) (.a -> u:(b -> b)) b !.(d c) -> b | Array d c
