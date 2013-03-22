definition module Data.Functor

class Functor f
where
	fmap :: (a -> b) (f a) -> f b

(<$>) infixl 4 :: (a -> b) (f a) -> f b | Functor f

