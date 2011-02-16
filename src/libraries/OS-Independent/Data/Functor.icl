implementation module Functor

from Maybe import :: Maybe(..)
from Error import :: MaybeError(..)

class Functor f
where
	fmap :: (.a -> .b) (f .a) -> f .b

instance Functor Maybe
where
	fmap f Nothing	= Nothing
	fmap f (Just a)	= Just (f a)
	
instance Functor (MaybeError a)
where
	fmap f (Ok x)		= Ok (f x)
	fmap f (Error x)	= Error x