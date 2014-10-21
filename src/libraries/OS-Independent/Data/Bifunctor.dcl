definition module Data.Bifunctor

from Data.Either import :: Either

class Bifunctor p where
  bifmap :: (a -> b) (c -> d) (p a c) -> p b d
  first :: (a -> b) (p a c) -> p b c
  second :: (b -> c) (p a b) -> p a c

instance Bifunctor (,)

instance Bifunctor ((,,) x)

instance Bifunctor ((,,,) x y)

instance Bifunctor ((,,,,) x y z)

instance Bifunctor Either
