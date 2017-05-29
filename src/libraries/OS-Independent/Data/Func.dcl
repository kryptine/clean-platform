definition module Data.Func

from StdFunc import id, const, flip, o
from StdMisc import abort, undef
undefined :== undef

($) infixr 0
($) f :== f

($$) infixl 1 //:: a (a -> b) -> b
($$) a f :== f a

app f :== f

seqSt        :: !(a .st -> .st)       ![a] !.st -> .st
mapSt        :: !(a .st -> (!b,!.st)) ![a] !.st -> (![b],!.st)
fix          :: (a -> a) -> a
on           :: (b b -> c) (a -> b) -> (a a -> c)
