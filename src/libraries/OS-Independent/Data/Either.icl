implementation module Data.Either

either :: (a -> c) (b -> c) (Either a b) -> c
either f _ (Left x)     =  f x
either _ g (Right y)    =  g y