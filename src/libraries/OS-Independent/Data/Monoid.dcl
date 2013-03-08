definition module Monoid

from Maybe import :: Maybe
from Monad import :: U1

class Monoid a where
  mempty :: a
  mappend :: a a -> a

mconcat        :: .[a] -> a | Monoid a
(<>) infixr 6  :: a a -> a | Monoid a

instance Monoid [a]
instance Monoid (a -> b) | Monoid b
instance Monoid U1
instance Monoid (a, b) | Monoid a & Monoid b
instance Monoid (a, b, c) | Monoid a & Monoid b & Monoid c
instance Monoid (a, b, c, d) | Monoid a & Monoid b & Monoid c & Monoid d
instance Monoid (a, b, c, d, e) | Monoid a & Monoid b & Monoid c & Monoid d & Monoid e
instance Monoid (Maybe a) | Monoid a
