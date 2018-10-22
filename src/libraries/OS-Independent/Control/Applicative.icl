implementation module Control.Applicative

import Control.Monad
import Data.Func, Data.Functor, System.IO, Data.List, Data.Maybe
import Data.Monoid
from StdFunc import id, o, flip, const

class Applicative f | Functor f
where
	pure           :: a -> f a
	(<*>) infixl 4 :: !(f (a -> b)) (f a) -> f b

	(<*) infixl 4  :: !(f a) (f b) -> f a
	(<*) fa fb = pure (\x _->x) <*> fa <*> fb

	(*>) infixl 4  :: !(f a) (f b) -> f b
	(*>) fa fb = pure (\_ x->x) <*> fa <*> fb

getConst :: !(Const a b) -> a
getConst (Const x) = x

instance Functor (Const m) where
  fmap _ (Const v) = Const v

instance Semigroup (Const a b) | Semigroup a where
  mappend (Const a) (Const b) = Const (mappend a b)

instance Monoid (Const a b) | Monoid a where
  mempty = Const mempty

instance Applicative (Const m) | Monoid m where
  pure _ = Const mempty
  (<*>) (Const f) (Const v) = Const (mappend f v)

unwrapMonad :: !(WrappedMonad m a) -> m a
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

some :: (f a) -> f [a] | Alternative f
some v = some_v
  where  many_v  = some_v <|> lift []
         some_v  = (\x xs -> [x:xs]) <$> v <*> many_v

many :: (f a) -> f [a] | Alternative f
many v = many_v
  where  many_v  = some_v <|> lift []
         some_v  = (\x xs -> [x:xs]) <$> v <*> many_v

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
