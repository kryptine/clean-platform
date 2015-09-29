definition module Data.Func

from StdFunc import id, const, o, flip
from StdMisc import undef

($) infixr 0 :: !(.a -> .b) !.a -> .b
fix          :: (a -> a) -> a
on           :: (b b -> c) (a -> b) -> (a a -> c)

app          :: !(.a -> .b) !.a -> .b
seqSt        :: !(a .st -> .st)       ![a] !.st -> .st
mapSt        :: !(a .st -> (!b,!.st)) ![a] !.st -> (![b],!.st)

undefined :== undef

