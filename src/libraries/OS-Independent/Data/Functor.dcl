definition module Functor

from Maybe import :: Maybe
from Error import :: MaybeError

class Functor f
where
	fmap :: (.a -> .b) (f .a) -> f .b
	
instance Functor Maybe
instance Functor (MaybeError a)