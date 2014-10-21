definition module Data.Func

($) infixr 0 :: !(.a -> .b) !.a -> .b
app          :: !(.a -> .b) !.a -> .b
seqSt        :: !(a .st -> .st)       ![a] !.st -> .st
mapSt        :: !(a .st -> (!b,!.st)) ![a] !.st -> (![b],!.st)
fix          :: (a -> a) -> a
on           :: (b b -> c) (a -> b) -> (a a -> c)
