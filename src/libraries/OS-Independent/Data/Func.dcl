definition module Data.Func

from StdFunc import id, const, o, flip
from StdMisc import undef, abort

// ($) infixr 0 :: !(.a -> .b) !.a -> .b
// (&) infixl 1 :: !.a !(.a -> .b) -> .b

($) infixr 0 // :: (a -> b) a -> b
($) f a :== f a

($$) infixl 1 // :: a (a -> b) -> b
($$) a f :== f a

(?) infixr 1 // :: Bool a a -> a
(?) b t f :== if b t f

fix          :: (a -> a) -> a
on           :: (b b -> c) (a -> b) -> (a a -> c)

app          :: !(.a -> .b) !.a -> .b
seqSt        :: !(a .st -> .st)       ![a] !.st -> .st
mapSt        :: !(a .st -> (!b,!.st)) ![a] !.st -> (![b],!.st)

undefined :== undef
