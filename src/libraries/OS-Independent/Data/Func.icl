implementation module Data.Func

seqSt :: !(a .st -> .st) ![a] !.st -> .st
seqSt f [] st = st
seqSt f [x:xs] st = seqSt f xs (f x st)

mapSt :: !(a .st -> (!b,!.st)) ![a] !.st -> (![b],!.st)
mapSt f [] st = ([], st)
mapSt f [x:xs] st
  #! (y, st)  = f x st
  #! (ys, st) = mapSt f xs st
  = ([y:ys], st)

fix :: (a -> a) -> a
fix f = let x = f x in x

on :: (b b -> c) (a -> b) -> (a a -> c)
on f g = \x y -> f (g x) (g y)
