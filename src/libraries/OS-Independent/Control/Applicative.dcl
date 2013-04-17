definition module Control.Applicative

from Data.Functor import class Functor
from Data.Maybe import :: Maybe

instance Applicative Maybe
instance Alternative Maybe
instance Alternative []

class Applicative f | Functor f where
  pure :: a -> f a
  (<*>) infixl 4 :: (f (a -> b)) (f a) -> f b

class Alternative f | Applicative f where
  empty :: f a
  (<|>) infixl 3 :: (f a) (f a) -> f a

some :: (f a) -> f [a] | Alternative f

many :: (f a) -> f [a] | Alternative f

(*>) infixl 4 :: (f a) (f b) -> f b | Applicative f

(<*) infixl 4 :: (f a) (f b) -> f a | Applicative f

(<**>) infixl 4 :: (f a) (f (a -> b)) -> f b | Applicative f

liftA :: (a -> b) (f a) -> f b | Applicative f

liftA2 :: (a b -> c) (f a) (f b) -> f c | Applicative f

liftA3 :: (a b c -> d) (f a) (f b) (f c) -> f d | Applicative f

optional :: (f a) -> f (Maybe a) | Alternative f

app :: (f (a -> b)) (f a) -> f b | Applicative f