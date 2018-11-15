definition module Data.Monoid

from StdOverloaded import class +, class *, class zero, class one
from Data.Maybe import :: Maybe
import qualified StdList

class Semigroup a
where
	mappend :: !a a -> a

	(<++>) infixr 6 :: a a -> a | Semigroup a
	(<++>) ma mb :== mappend ma mb

class Monoid a | Semigroup a
where
	mempty :: a

	mconcat :: !.[a] -> a | Monoid a
	mconcat xs :== mconcat xs
	where
		mconcat []    = mempty
		mconcat [a:x] = mconcat a (mconcat x)

instance Semigroup ()
instance Monoid ()

:: Dual a = Dual a

:: Endo a = Endo (a -> a)

:: All = All Bool

:: Any = Any Bool

:: Sum a = Sum a

:: Product a = Product a

:: First a = First (Maybe a)

:: Last a = Last (Maybe a)

instance Semigroup (Dual a) | Semigroup a
instance Semigroup (Endo .a)
instance Semigroup All
instance Semigroup Any
instance Semigroup (Sum a) | + a & zero a
instance Semigroup (Product a) | * a & one a
instance Semigroup (First a)
instance Semigroup (Last a)

instance Monoid (Dual a) | Monoid a
instance Monoid (Endo .a)
instance Monoid All
instance Monoid Any
instance Monoid (Sum a) | + a & zero a
instance Monoid (Product a) | * a & one a
instance Monoid (First a)
instance Monoid (Last a)

getDual :: !(Dual .a) -> .a

appEndo :: !(Endo .a) -> (.a -> .a)

getAll :: !All -> Bool

getAny :: !Any -> Bool

getSum :: !(Sum a) -> a

getProduct :: !(Product a) -> a

getFirst :: !(First a) -> Maybe a

getLast :: !(Last a) -> Maybe a
