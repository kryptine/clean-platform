implementation module Data.Monoid

from Maybe import :: Maybe(..)
from Void import :: Void(..)
from StdList import ++, foldr

class Monoid a where
  mempty  :: a
  mappend :: a a -> a

mconcat :: .[a] -> a | Monoid a
mconcat xs = foldr mappend mempty xs

(<>) infixr 6 :: a a -> a | Monoid a
(<>) ma mb = mappend ma mb

instance Monoid [a] where
  mempty         = []
  mappend xs ys  = xs ++ ys

instance Monoid (a -> b) | Monoid b where
  mempty      = \_ -> mempty
  mappend f g = \x -> mappend (f x) (g x)

instance Monoid Void where
  mempty       = Void
  mappend _ _  = Void

instance Monoid (a, b) | Monoid a & Monoid b where
  mempty                     = (mempty, mempty)
  mappend (a1, b1) (a2, b2)  = (mappend a1 a2, mappend b1 b2)

instance Monoid (a, b, c) | Monoid a & Monoid b & Monoid c where
  mempty                     = (mempty, mempty, mempty)
  mappend (a1, b1, c1) (a2, b2, c2)  = (mappend a1 a2, mappend b1 b2, mappend c1 c2)

instance Monoid (a, b, c, d) | Monoid a & Monoid b & Monoid c & Monoid d where
  mempty                     = (mempty, mempty, mempty, mempty)
  mappend (a1, b1, c1, d1) (a2, b2, c2, d2)  = (mappend a1 a2, mappend b1 b2, mappend c1 c2, mappend d1 d2)

instance Monoid (a, b, c, d, e) | Monoid a & Monoid b & Monoid c & Monoid d & Monoid e where
  mempty                     = (mempty, mempty, mempty, mempty, mempty)
  mappend (a1, b1, c1, d1, e1) (a2, b2, c2, d2, e2)  = (mappend a1 a2, mappend b1 b2, mappend c1 c2, mappend d1 d2, mappend e1 e2)

instance Monoid (Maybe a) | Monoid a where
  mempty                        = Nothing
  mappend Nothing    m          = m
  mappend m          Nothing    = m
  mappend (Just m1)  (Just m2)  = Just (mappend m1 m2)
