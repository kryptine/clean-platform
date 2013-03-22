implementation module Data.Functor

(<$>) infixl 4 :: (a -> b) (f a) -> (f b) | Functor f
(<$>) f fa = fmap f fa
