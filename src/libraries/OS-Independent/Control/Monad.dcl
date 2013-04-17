definition module Control.Monad

from Data.Maybe   import :: Maybe
from Data.Void import :: Void

:: IO a = IO (World -> (a, World))

class Monad m where
  return :: a -> m a
  (>>=) infixl 1 :: (m a) (a -> m b) -> (m b)

instance Monad IO

instance Monad ((->) r)

instance Monad []

instance Monad Maybe

class MonadPlus m | Monad m where
   mzero :: m a
   mplus :: (m a) (m a) -> m a

instance MonadPlus []

instance MonadPlus Maybe

(>>) infixr 1     :: (a b) (a c) -> a c | Monad a
(=<<) infixr 1    :: (a -> b c) (b a) -> b c | Monad b
sequence          :: .[a b] -> a [b] | Monad a
sequence_         :: .[a b] -> a Void | Monad a
mapM              :: (.a -> b c) [.a] -> b [c] | Monad b
mapM_             :: (.a -> b c) [.a] -> b Void | Monad b
forM              :: u:([v:a] -> w:((v:a -> b c) -> b [c])) | Monad b, [w <= u,w <= v]
forM_             :: u:([v:a] -> w:((v:a -> b c) -> b Void)) | Monad b, [w <= u,w <= v]
forever           :: (a b) -> a c | Monad a
join              :: (a (a b)) -> a b | Monad a
zipWithM          :: (.a -> .(.b -> c d)) [.a] [.b] -> c [d] | Monad c
foldM             :: (a -> .(b -> c a)) a [b] -> c a | Monad c
replicateM        :: .Int (a b) -> a [b] | Monad a
(>=>) infixr 1    :: u:(.a -> b c) (c -> b d) -> v:(.a -> b d) | Monad b, [v <= u]
(<=<) infixr 1    :: u:((a -> b c) -> v:(w:(.d -> b a) -> x:(.d -> b c))) | Monad b, [v <= u,x <= w]
liftM             :: (a -> b) (c a) -> c b | Monad c
liftM2            :: (a -> .(b -> c)) (d a) (d b) -> d c | Monad d
liftM3            :: (a -> .(b -> .(c -> d))) (e a) (e b) (e c) -> e d | Monad e
liftM4            :: (a -> .(b -> .(c -> .(d -> e)))) (f a) (f b) (f c) (f d) -> f e | Monad f
liftM5            :: (a -> .(b -> .(c -> .(d -> .(e -> f))))) (g a) (g b) (g c) (g d) (g e) -> g f | Monad g
ap                :: u:((a (b -> c)) -> v:((a b) -> a c)) | Monad a, [v <= u]

