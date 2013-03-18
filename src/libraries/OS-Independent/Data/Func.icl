implementation module Data.Func

($) infixr 0 :: !(.a -> .b) !.a -> .b
($) f x = f x

app :: !(.a -> .b) !.a -> .b
app f x = f x

seqSt :: !(a .st -> .st) ![a] !.st -> .st
seqSt f [] st = st
seqSt f [x:xs] st = seqSt f xs (f x st)

mapSt :: !(a .st -> (!b,!.st)) ![a] !.st -> (![b],!.st)
mapSt f [] st = ([], st)
mapSt f [x:xs] st
	# (y, st) = f x st
	# (ys, st) = mapSt f xs st
	= ([y:ys], st)
