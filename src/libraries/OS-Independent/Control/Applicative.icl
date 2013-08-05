implementation module Control.Applicative

from Control.Monad import ap, class Monad, instance Monad []
import Data.Func, Data.Functor, Data.List, Data.Maybe
import StdFunc

instance Applicative Maybe where
  pure x = Just x
  (<*>) Nothing _ = Nothing
  (<*>) (Just f) ma = fmap f ma

instance Applicative [] where
  pure x     = [x]
  (<*>) xs x = ap xs x

instance Alternative Maybe where
  empty             = Nothing
  (<|>) Nothing  r  = r
  (<|>) l        _  = l

instance Alternative [] where
  empty         = []
  (<|>) fa fa`  = fa ++ fa`

some :: (f a) -> f [a] | Alternative f
some v = some_v
  where  many_v  = some_v <|> pure []
         some_v  = (\x xs -> [x:xs]) <$> v <*> many_v

many :: (f a) -> f [a] | Alternative f
many v = many_v
  where  many_v  = some_v <|> pure []
         some_v  = (\x xs -> [x:xs]) <$> v <*> many_v

(*>) infixl 4 :: (f a) (f b) -> f b | Applicative f
(*>) fa fb = liftA2 (const id) fa fb

(<*) infixl 4 :: (f a) (f b) -> f a | Applicative f
(<*) fa fb = liftA2 const fa fb

(<**>) infixl 4 :: (f a) (f (a -> b)) -> f b | Applicative f
(<**>) fa fab = liftA2 (flip ($)) fa fab

liftA :: (a -> b) (f a) -> f b | Applicative f
liftA f a = pure f <*> a

liftA2 :: (a b -> c) (f a) (f b) -> f c | Applicative f
liftA2 f a b = f <$> a <*> b

liftA3 :: (a b c -> d) (f a) (f b) (f c) -> f d | Applicative f
liftA3 f a b c = f <$> a <*> b <*> c

optional :: (f a) -> f (Maybe a) | Alternative f
optional v = (Just <$> v) <|> (pure Nothing)

app :: (f (a -> b)) (f a) -> f b | Applicative f
app fab fa = fab <*> fa
