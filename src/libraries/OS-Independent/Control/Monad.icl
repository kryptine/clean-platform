implementation module Monad

from List    import map, zipWith, replicate
from Maybe   import :: Maybe, Nothing, Just
from StdList import foldr, ++
from StdFunc import flip, id, o, const
from StdMisc import undef
from StdInt  import class +, instance + Int

:: U1 = U1 // TODO: Move to other module?


:: IO a = IO (World -> (a, World))

class Monad m where
    return :: a -> m a
    (>>=) infixl 1 :: (m a) (a -> m b) -> (m b)

instance Monad IO where
  return x         = IO (\s -> (x, s))
  (>>=) ioa a2iob  = IO iob
    where iob st = let (IO fb) = a2iob a
                   in  fb nst
            where (a, nst) = let (IO fa) = ioa
                             in  fa st

instance Monad ((->) r) where
  return x       = const x
  (>>=) ma a2mb  = \r -> a2mb (ma r) r

instance Monad [] where
  return x   = [x]
  (>>=) m k  = foldr ((++) o k) [] m

instance Monad Maybe where
  return x          = Just x
  (>>=) (Just x) k  = k x
  (>>=) Nothing  _  = Nothing

class MonadPlus m | Monad m where
   mzero :: m a
   mplus :: (m a) (m a) -> m a

instance MonadPlus [] where
   mzero        = []
   mplus xs ys  = xs ++ ys

instance MonadPlus Maybe where
   mzero = Nothing

   mplus Nothing ys  = ys
   mplus xs      _   = xs

(>>) infixr 1 :: (a b) (a c) -> a c | Monad a
(>>) ma mb = ma >>= \_ -> mb

(=<<) infixr 1 :: (a -> b c) (b a) -> b c | Monad b
(=<<) f x         = x >>= f

sequence :: .[a b] -> a [b] | Monad a
sequence ms = foldr k (return []) ms
  where
    k m m` = m >>= \x -> m` >>= \xs -> return [x:xs]

sequence_ :: .[a b] -> a U1 | Monad a
sequence_ ms     =  foldr (>>) (return U1) ms

mapM :: (.a -> b c) [.a] -> b [c] | Monad b
mapM f as       =  sequence (map f as)

mapM_ :: (.a -> b c) [.a] -> b U1 | Monad b
mapM_ f as      =  sequence_ (map f as)

forM :: u:([v:a] -> w:((v:a -> b c) -> b [c])) | Monad b, [w <= u,w <= v]
forM            = flip mapM

forM_ :: u:([v:a] -> w:((v:a -> b c) -> b U1)) | Monad b, [w <= u,w <= v]
forM_           = flip mapM_

forever :: (a b) -> a c | Monad a
forever a   = let a` = a >> a` in a`

join :: (a (a b)) -> a b | Monad a
join x            =  x >>= id

zipWithM :: (.a -> .(.b -> c d)) [.a] [.b] -> c [d] | Monad c
zipWithM f xs ys  =  sequence (zipWith f xs ys)

foldM :: (a -> .(b -> c a)) a [b] -> c a | Monad c
foldM _ a []      =  return a
foldM f a [x:xs]  =  f a x >>= \fax -> foldM f fax xs

replicateM :: .Int (a b) -> a [b] | Monad a
replicateM n x    = sequence (replicate n x)

(>=>) infixr 1 :: u:(.a -> b c) (c -> b d) -> v:(.a -> b d) | Monad b, [v <= u]
(>=>) f g     = \x -> f x >>= g

(<=<) infixr 1 :: u:((a -> b c) -> v:(w:(.d -> b a) -> x:(.d -> b c))) | Monad b, [v <= u,x <= w]
(<=<)       = flip (>=>)

liftM :: (a b) (a c) -> a b | Monad a
liftM f m1               = m1 >>= \x1 -> return f x1

liftM2 :: (a -> b c) (b d) (b a) -> b c | Monad b
liftM2 f m1 m2           = m1 >>= \x1 -> m2 >>= \x2 -> return f x1 x2

liftM3 :: (a -> .(b -> c d)) (c e) (c a) (c b) -> c d | Monad c
liftM3 f m1 m2 m3        = m1 >>= \x1 -> m2 >>= \x2 -> m3 >>= \x3 -> return f x1 x2 x3

liftM4 :: (a -> .(b -> .(c -> d e))) (d f) (d a) (d b) (d c) -> d e | Monad d
liftM4 f m1 m2 m3 m4     = m1 >>= \x1 -> m2 >>= \x2 -> m3 >>= \x3 -> m4 >>= \x4 -> return f x1 x2 x3 x4

liftM5 :: (a -> .(b -> .(c -> .(d -> e f)))) (e g) (e a) (e b) (e c) (e d) -> e f | Monad e
liftM5 f m1 m2 m3 m4 m5  = m1 >>= \x1 -> m2 >>= \x2 -> m3 >>= \x3 -> m4 >>= \x4 -> m5 >>= \x5 -> return f x1 x2 x3 x4 x5

ap :: u:((a b) -> v:((a (a c)) -> a c)) | Monad a, [v <= u]
ap                =  liftM2 id
