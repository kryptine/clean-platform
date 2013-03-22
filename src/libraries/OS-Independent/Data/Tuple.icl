implementation module Data.Tuple

tuple :: !a !b -> (!a,!b)
tuple a b = (a,b)

tuple3 :: !a !b !c -> (!a,!b,!c)
tuple3 a b c = (a,b,c)

appFst	:: (.a -> .c) !(.a,.b) -> (.c,.b)
appFst f (a,b) = (f a,b)

appSnd	:: (.b -> .c) !(.a,.b) -> (.a,.c)
appSnd f (a,b) = (a,f b)

appFst3 :: (.a -> .d) !(.a,.b,.c) -> (.d,.b,.c)
appFst3 f (a,b,c) = (f a,b,c)

appSnd3 :: (.b -> .d) !(.a,.b,.c) -> (.a,.d,.c)
appSnd3 f (a,b,c) = (a,f b,c)

appThd3 :: (.c -> .d) !(.a,.b,.c) -> (.a,.b,.d)
appThd3 f (a,b,c) = (a,b,f c)

curry :: ((a, b) -> c) a b -> c
curry f x y =  f (x, y)

uncurry :: (a b -> c) -> ((a, b) -> c)
uncurry f p =  f (fst p) (snd p)

swap :: (a, b) -> (b, a)
swap (a,b) = (b,a)
