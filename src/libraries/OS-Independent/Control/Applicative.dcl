definition module Control.Applicative

from Control.Monad import class Monad, class MonadPlus
from Data.Functor  import class Functor
from Data.Maybe    import :: Maybe
from Data.Monoid   import class Monoid, class Semigroup

:: Const a b = Const a
:: WrappedMonad m a = WrapMonad (m a)

unwrapMonad :: !(WrappedMonad m a) -> m a

getConst :: !(Const a b) -> a

class Applicative f | Functor f
where
	pure           :: a -> f a
	(<*>) infixl 4 :: !(f (a -> b)) (f a) -> f b

	(<*) infixl 4  :: !(f a) (f b) -> f a
	(<*) fa fb = pure (\x _->x) <*> fa <*> fb

	(*>) infixl 4  :: !(f a) (f b) -> f b
	(*>) fa fb = pure (\_ x->x) <*> fa <*> fb

class Alternative f | Applicative f
where
	empty          :: f a
	(<|>) infixl 3 :: !(f a) (f a) -> f a

instance Functor (Const m)
instance Functor (WrappedMonad m) | Monad m
instance Applicative (Const m) | Monoid m
instance Applicative (WrappedMonad m) | Monad m
instance Monad (WrappedMonad m) | Monad m

instance Alternative (WrappedMonad m) | MonadPlus m

instance Semigroup (Const a b) | Semigroup a
instance Monoid (Const a b) | Monoid a

some :: (f a) -> f [a] | Alternative f

many :: (f a) -> f [a] | Alternative f

(<**>) infixl 4 :: (f a) (f (a -> b)) -> f b | Applicative f

lift :: a -> f a | Applicative f

liftA :: (a -> b) (f a) -> f b | Applicative f

liftA2 :: (a b -> c) (f a) (f b) -> f c | Applicative f

liftA3 :: (a b c -> d) (f a) (f b) (f c) -> f d | Applicative f

optional :: (f a) -> f (Maybe a) | Alternative f

/**
 * Conditional execution of Applicative expressions. For example,
 *
 *     when debug (putStrLn "Debugging")
 *
 * will output the string Debugging if the Boolean value debug is True, and otherwise do nothing.
 *
 * @type Bool (f ()) -> f () | Applicative f
 */
when p s :== if p s (pure ())

/**
 * The reverse of `when`
 * @type Bool (f ()) -> f () | Applicative f
 */
unless p s :== if p (pure ()) s
