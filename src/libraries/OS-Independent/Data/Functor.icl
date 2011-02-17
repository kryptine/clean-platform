implementation module Functor

class Functor f
where
	fmap :: (.a -> .b) (f .a) -> f .b