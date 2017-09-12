implementation module Control.Applicative

import Control.Monad
import Data.Func, Data.Functor, System.IO, Data.List, Data.Maybe
from Data.Monoid import class Monoid, class Semigroup
import qualified Data.Monoid as DM
from StdFunc import id, o, flip, const

getConst :: (Const a b) -> a
getConst (Const x) = x

instance Functor (Const m) where
  fmap _ (Const v) = Const v

instance Semigroup (Const a b) | Semigroup a where
  mappend (Const a) (Const b) = Const ('DM'.mappend a b)

instance Monoid (Const a b) | Monoid a where
  mempty = Const 'DM'.mempty

instance Applicative (Const m) | Monoid m where
  pure _ = Const 'DM'.mempty
  (<*>) (Const f) (Const v) = Const ('DM'.mappend f v)

unwrapMonad :: (WrappedMonad m a) -> m a
unwrapMonad (WrapMonad x) = x

instance Functor (WrappedMonad m) | Monad m where
  fmap f (WrapMonad v) = WrapMonad (liftM f v)

instance Applicative (WrappedMonad m) | Monad m where
  pure x = WrapMonad (pure x)
  (<*>) (WrapMonad f) (WrapMonad v) = WrapMonad (ap f v)

instance Monad (WrappedMonad m) | Monad m where
  bind a f = WrapMonad (unwrapMonad a >>= unwrapMonad o f)

instance Alternative (WrappedMonad m) | MonadPlus m where
  empty = WrapMonad mzero
  (<|>) (WrapMonad u) (WrapMonad v) = WrapMonad (mplus u v)

instance Applicative ((->) r) where
  pure x      = const x
  (<*>) f g   = \x -> f x (g x)

instance Applicative Maybe where
  pure x              = Just x
  (<*>) Nothing   _   = Nothing
  (<*>) (Just f)  ma  = fmap f ma

instance Applicative [] where
  pure x      = [x]
  (<*>) xs x  = liftA2 id xs x

instance Alternative Maybe where
  empty             = Nothing
  (<|>) Nothing  r  = r
  (<|>) l        _  = l

instance Alternative [] where
  empty         = []
  (<|>) fa fa`  = fa ++ fa`

some :: (f a) -> f [a] | Alternative f
some v = some_v
  where  many_v  = some_v <|> lift []
         some_v  = (\x xs -> [x:xs]) <$> v <*> many_v

many :: (f a) -> f [a] | Alternative f
many v = many_v
  where  many_v  = some_v <|> lift []
         some_v  = (\x xs -> [x:xs]) <$> v <*> many_v

instance *> f where *> fa fb = id <$ fa <*> fb

instance *> Maybe
where
	*> (Just _) m = m
	*> _        _ = Nothing

instance <* f where <* fa fb = liftA2 const fa fb

instance <* Maybe
where
	<* m (Just _) = m
	<* _ _        = Nothing

(<**>) infixl 4 :: (f a) (f (a -> b)) -> f b | Applicative f
(<**>) fa fab = liftA2 (flip ($)) fa fab

lift :: a -> f a | Applicative f
lift x = pure x

liftA :: (a -> b) (f a) -> f b | Applicative f
liftA f a = lift f <*> a

liftA2 :: (a b -> c) (f a) (f b) -> f c | Applicative f
liftA2 f a b = f <$> a <*> b

liftA3 :: (a b c -> d) (f a) (f b) (f c) -> f d | Applicative f
liftA3 f a b c = f <$> a <*> b <*> c

optional :: (f a) -> f (Maybe a) | Alternative f
optional v = (Just <$> v) <|> (lift Nothing)
