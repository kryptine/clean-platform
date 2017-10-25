implementation module Data.Either

import Control.Applicative
import Control.Monad
import Data.Functor

instance Functor (Either a) where
  fmap f (Left l)  = Left l
  fmap f (Right r) = Right (f r)

instance Applicative (Either e) where
  pure x        = Right x
  (<*>) (Left  e) _ = Left e
  (<*>) (Right f) r = fmap f r

instance *> (Either e)
where
	*> (Right _) e = e
	*> (Left l)  _ = Left l

instance <* (Either e)
where
	<* (Left l)  _         = Left l
	<* _         (Left l)  = Left l
	<* x         _         = x

instance Monad (Either e) where
  bind (Left  l) _ = Left l
  bind (Right r) k = k r

either :: (.a -> .c) (.b -> .c) !(Either .a .b) -> .c
either f _ (Left x)     =  f x
either _ g (Right y)    =  g y
