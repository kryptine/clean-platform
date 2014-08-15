implementation module Data.Monoid

from StdOverloaded import class + (..), class * (..), class zero (..), class one (..)
from StdBool import &&, ||
from StdFunc import o, id
from Data.Maybe import :: Maybe(..)
from Data.Void import :: Void(..)
from StdList import ++, foldr

class Monoid a where
  mempty  :: a
  mappend :: a a -> a

mconcat :: .[a] -> a | Monoid a
mconcat xs = foldr mappend mempty xs

(<++>) infixr 6 :: a a -> a | Monoid a
(<++>) ma mb = mappend ma mb

instance Monoid [a] where
  mempty         = []
  mappend xs ys  = xs ++ ys

instance Monoid (a -> b) | Monoid b where
  mempty      = \_ -> mempty
  mappend f g = \x -> mappend (f x) (g x)

instance Monoid () where
  mempty       = ()
  mappend _ _  = ()

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

instance Monoid (Dual a) | Monoid a where
  mempty = Dual mempty
  mappend (Dual x) (Dual y) = Dual (mappend y x)

instance Monoid (Endo a) where
  mempty = Endo id
  mappend (Endo f) (Endo g) = Endo (f o g)

instance Monoid All where
  mempty = All True
  mappend (All x) (All y) = All (x && y)

instance Monoid Any where
  mempty = Any False
  mappend (Any x) (Any y) = Any (x || y)

instance Monoid (Sum a) | + a & zero a where
  mempty = Sum zero
  mappend (Sum x) (Sum y) = Sum (x + y)

instance Monoid (Product a) | * a & one a where
  mempty = Product one
  mappend (Product x) (Product y) = Product (x * y)

instance Monoid (First a) where
  mempty = First Nothing
  mappend r=:(First (Just _)) _ = r
  mappend (First Nothing) r = r

instance Monoid (Last a) where
  mempty = Last Nothing
  mappend _ r=:(Last (Just _)) = r
  mappend r (Last Nothing) = r

getDual :: (Dual a) -> a
getDual (Dual x) = x

appEndo :: (Endo a) -> (a -> a)
appEndo (Endo f) = f

getAll :: All -> Bool
getAll (All b) = b

getAny :: Any -> Bool
getAny (Any b) = b

getSum :: (Sum a) -> a
getSum (Sum x) = x

getProduct :: (Product a) -> a
getProduct (Product x) = x

getFirst :: (First a) -> Maybe a
getFirst (First x) = x

getLast :: (Last a) -> Maybe a
getLast (Last x) = x

