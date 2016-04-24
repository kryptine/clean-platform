implementation module Data.Monoid

from StdOverloaded import class + (..), class * (..), class zero (..), class one (..)
from StdBool import &&, ||
from StdFunc import o, id
from Data.Maybe import :: Maybe(..)
from StdList import ++, foldr

mconcat :: .[a] -> a | Monoid a
mconcat xs = foldr mappend mempty xs

(<++>) infixr 6 :: a a -> a | Semigroup a
(<++>) ma mb = mappend ma mb

instance Semigroup [a] where
  mappend xs ys  = xs ++ ys

instance Monoid [a] where
  mempty = []

instance Semigroup (a -> b) | Semigroup b where
  mappend f g = \x -> mappend (f x) (g x)

instance Monoid (a -> b) | Monoid b where
  mempty = \_ -> mempty

instance Semigroup () where
  mappend _ _  = ()

instance Monoid () where
  mempty = ()

instance Semigroup (a, b) | Semigroup a & Semigroup b where
  mappend (a1, b1) (a2, b2)  = (mappend a1 a2, mappend b1 b2)

instance Monoid (a, b) | Monoid a & Monoid b where
  mempty = (mempty, mempty)

instance Semigroup (a, b, c) | Semigroup a & Semigroup b & Semigroup c where
  mappend (a1, b1, c1) (a2, b2, c2)  = (mappend a1 a2, mappend b1 b2, mappend c1 c2)

instance Monoid (a, b, c) | Monoid a & Monoid b & Monoid c where
  mempty = (mempty, mempty, mempty)

instance Semigroup (a, b, c, d) | Semigroup a & Semigroup b & Semigroup c & Semigroup d where
  mappend (a1, b1, c1, d1) (a2, b2, c2, d2)  = (mappend a1 a2, mappend b1 b2, mappend c1 c2, mappend d1 d2)

instance Monoid (a, b, c, d) | Monoid a & Monoid b & Monoid c & Monoid d where
  mempty = (mempty, mempty, mempty, mempty)

instance Semigroup (a, b, c, d, e) | Semigroup a & Semigroup b & Semigroup c & Semigroup d & Semigroup e where
  mappend (a1, b1, c1, d1, e1) (a2, b2, c2, d2, e2)  = (mappend a1 a2, mappend b1 b2, mappend c1 c2, mappend d1 d2, mappend e1 e2)

instance Monoid (a, b, c, d, e) | Monoid a & Monoid b & Monoid c & Monoid d & Monoid e where
  mempty = (mempty, mempty, mempty, mempty, mempty)

instance Semigroup (Maybe a) | Semigroup a where
  mappend Nothing   m         = m
  mappend m         Nothing   = m
  mappend (Just m1) (Just m2) = Just (mappend m1 m2)

instance Monoid (Maybe a) where
  mempty = Nothing

instance Semigroup (Dual a) | Semigroup a where
  mappend (Dual x) (Dual y) = Dual (mappend y x)

instance Monoid (Dual a) | Monoid a where
  mempty = Dual mempty

instance Semigroup (Endo a) where
  mappend (Endo f) (Endo g) = Endo (f o g)

instance Monoid (Endo a) where
  mempty = Endo id

instance Semigroup All where
  mappend (All x) (All y) = All (x && y)

instance Monoid All where
  mempty = All True

instance Semigroup Any where
  mappend (Any x) (Any y) = Any (x || y)

instance Monoid Any where
  mempty = Any False

instance Semigroup (Sum a) | + a & zero a where
  mappend (Sum x) (Sum y) = Sum (x + y)

instance Monoid (Sum a) | + a & zero a where
  mempty = Sum zero

instance Semigroup (Product a) | * a & one a where
  mappend (Product x) (Product y) = Product (x * y)

instance Monoid (Product a) | * a & one a where
  mempty = Product one

instance Semigroup (First a) where
  mappend r=:(First (Just _)) _ = r
  mappend (First Nothing) r = r

instance Monoid (First a) where
  mempty = First Nothing

instance Semigroup (Last a) where
  mappend _ r=:(Last (Just _)) = r
  mappend r (Last Nothing) = r

instance Monoid (Last a) where
  mempty = Last Nothing

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

