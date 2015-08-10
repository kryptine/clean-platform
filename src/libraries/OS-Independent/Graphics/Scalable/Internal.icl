implementation module Graphics.Scalable.Internal

import Graphics.Scalable.Internal
from StdMisc import abort
from StdFunc import flip
from StdTuple import fst, snd
from StdOrdList import minList, maxList
from StdOverloaded import class toReal
import Data.List
import Data.Maybe
from Data.Set import :: Set, instance == (Set a), instance < (Set a)
from StdBool import &&
import qualified Data.Set as DS
import Text.HTML
from Data.Functor import class Functor (..)
import GenLexOrd

strictTRMapRev :: !(.a -> .b) ![.a] -> [.b]
strictTRMapRev f xs = strictTRMapAcc f xs []

strictTRMapAcc :: !(u:a -> v:b) !w:[u:a] !x:[v:b] -> y:[v:b], [w <= u,y <= v,x <= y]
strictTRMapAcc f []     acc = acc
strictTRMapAcc f [x:xs] acc = strictTRMapAcc f xs [f x : acc]

strictTRMap :: !(.a -> .b) ![.a] -> [.b]
strictTRMap f xs = reverseTR (strictTRMapAcc f xs [])

reverseTR :: ![.a] -> [.a]
reverseTR xs = rev` xs []
  where
  rev` :: !u:[v:a] !w:[v:a] -> x:[v:a], [x u <= v,w <= x]
  rev` [] acc = acc
  rev` [x:xs] acc = rev` xs [x:acc]

instance / Span where
  / (PxSpan 0.0)           _             = PxSpan 0.0
  / _                      (PxSpan 0.0)  = PxSpan 0.0 // Division by zero should be undefined, but that would be impractical
  / l                      (PxSpan 1.0)  = l // Identity
  / (PxSpan l)             (PxSpan r)    = PxSpan (l / r)
  / (MulSpan a (PxSpan l)) (PxSpan r)    = MulSpan a (PxSpan (l / r))
  / (DivSpan a (PxSpan l)) (PxSpan r)    = DivSpan a (PxSpan (l * r))
  / (MaxSpan xs)           r=:(PxSpan _) = MaxSpan (strictTRMap (\x -> x / r) xs)
  / (MinSpan xs)           r=:(PxSpan _) = MinSpan (strictTRMap (\x -> x / r) xs)
  / l=:(PxSpan _)          (MaxSpan xs)  = MaxSpan (strictTRMap (\x -> l / x) xs)
  / l=:(PxSpan _)          (MinSpan xs)  = MinSpan (strictTRMap (\x -> l / x) xs)
  / l                      r             = DivSpan l r

instance * Span where
  * (PxSpan 0.0)           _                      = PxSpan 0.0
  * _                      (PxSpan 0.0)           = PxSpan 0.0
  * (PxSpan 1.0)           r                      = r // Identity
  * l                      (PxSpan 1.0)           = l // Identity
  * (PxSpan a)             (PxSpan b)             = PxSpan (a * b)
  * (PxSpan a)             (MulSpan (PxSpan b) c) = MulSpan (PxSpan (a * b)) c // Associativity
  * (PxSpan a)             (MulSpan b (PxSpan c)) = MulSpan (PxSpan (a * c)) b // Associativity + commutativity
  * (MulSpan a (PxSpan b)) (PxSpan c)             = MulSpan a (PxSpan (b * c)) // Associativity
  * (MulSpan (PxSpan a) b) (PxSpan c)             = MulSpan b (PxSpan (a * c)) // Associativity + commutativity
  * (DivSpan (PxSpan a) b) (PxSpan c)             = DivSpan (PxSpan (a * c)) b
  * (DivSpan a (PxSpan b)) (PxSpan c)             = MulSpan a (PxSpan (c / b))
  * (PxSpan c)             (DivSpan (PxSpan a) b) = DivSpan (PxSpan (a * c)) b
  * (PxSpan c)             (DivSpan a (PxSpan b)) = MulSpan a (PxSpan (c / b))
  * (DivSpan a b)          (DivSpan c d)          = DivSpan (a * c) (b * d)
  * (MaxSpan xs)           r=:(PxSpan _)          = MaxSpan (strictTRMap (\x -> x * r) xs)
  * (MinSpan xs)           r=:(PxSpan _)          = MinSpan (strictTRMap (\x -> x * r) xs)
  * l=:(PxSpan _)          (MaxSpan xs)           = MaxSpan (strictTRMap (\x -> x * l) xs)
  * l=:(PxSpan _)          (MinSpan xs)           = MinSpan (strictTRMap (\x -> x * l) xs)
  * l                      r                      = MulSpan l r

instance zero Span where
  zero = PxSpan zero

instance abs Span where
  abs (PxSpan  x)  = PxSpan (abs x)
  abs (AbsSpan x)  = AbsSpan x
  abs (MaxSpan xs) = MaxSpan (strictTRMap abs xs)
  abs (MinSpan xs) = MinSpan (strictTRMap abs xs)
  abs span         = AbsSpan span

instance ~ Span where
  ~ s = zero - s
instance + Span where
  + (PxSpan 0.0)              b                         = b // Identity
  + a                         (PxSpan 0.0)              = a // Identity
  + (PxSpan a)                (PxSpan b)                = PxSpan (a + b)
  + (PxSpan a)                (AddSpan (PxSpan b) c)    = AddSpan (PxSpan (a + b)) c // Associativity
  + (PxSpan a)                (AddSpan b (PxSpan c))    = AddSpan (PxSpan (a + c)) b // Associativity + commutativity
  + (AddSpan a (PxSpan b))    (PxSpan c)                = AddSpan a (PxSpan (b + c)) // Associativity
  + (AddSpan (PxSpan a) b)    (PxSpan c)                = AddSpan b (PxSpan (a + c)) // Associativity + commutativity
  + (SubSpan a b=:(PxSpan _)) c=:(PxSpan _)             = SubSpan (a + c) b
  + (SubSpan (PxSpan a) b)    (PxSpan c)                = SubSpan (PxSpan (a + c)) b
  + a=:(PxSpan _)             (SubSpan b c=:(PxSpan _)) = SubSpan (a + b) c
  + (PxSpan a)                (SubSpan (PxSpan b) c)    = SubSpan (PxSpan (a + b)) c
  + (DivSpan a l=:(PxSpan b)) (DivSpan c r=:(PxSpan d))
     | b == d    = DivSpan (a + c) (PxSpan b)
     | otherwise = DivSpan ((l * c) + (r * a)) (PxSpan (b * d))
  + (MulSpan (PxSpan a) b)    (MulSpan (PxSpan c) d)
     | a == c = MulSpan (PxSpan a) (b + d)
  + (MulSpan a (PxSpan b))    (MulSpan (PxSpan c) d)
     | b == c = MulSpan (PxSpan b) (a + d)
  + (MulSpan (PxSpan a) b)    (MulSpan c (PxSpan d))
     | a == d = MulSpan (PxSpan a) (b + c)
  + (MulSpan a (PxSpan b))    (MulSpan c (PxSpan d))
     | b == d = MulSpan (PxSpan b) (a + c)
  + l=:(PxSpan _)             (MaxSpan xs)              = MaxSpan (strictTRMap (\x -> x + l) xs)
  + (MaxSpan xs)              r=:(PxSpan _)             = MaxSpan (strictTRMap (\x -> x + r) xs)
  + s                         t                         = AddSpan s t

instance - Span where
  - a                      (PxSpan 0.0)           = a // Identity
  - (PxSpan a)             (PxSpan b)             = PxSpan (a - b)
  - (AddSpan a (PxSpan b)) (PxSpan c)             = AddSpan a (PxSpan (b - c))
  - (AddSpan (PxSpan a) b) (PxSpan c)             = AddSpan (PxSpan (a - c)) b
  - (PxSpan c)             (AddSpan a (PxSpan b)) = SubSpan (PxSpan (c - b)) a
  - (PxSpan c)             (AddSpan (PxSpan a) b) = SubSpan (PxSpan (c - a)) b
  - (DivSpan a (PxSpan b)) (DivSpan c (PxSpan d))
     | b == d = DivSpan (a - c) (PxSpan b)
  - (MaxSpan xs)           r=:(PxSpan _)          = MaxSpan (strictTRMap (\x -> x - r) xs)
  - l=:(PxSpan _)          (MaxSpan xs)           = MaxSpan (strictTRMap (\x -> l - x) xs)
  - s                      t                      = SubSpan s t

instance *. Int  where
  *. l r = PxSpan (toReal l * toReal r)

instance *. Real where
  *. l r = PxSpan (l * toReal r)

instance *. Span where
  *. (PxSpan  a)             k = PxSpan    (a * toReal k)
  *. (MulSpan (PxSpan k1) a) k = MulSpan a (PxSpan (toReal k * k1))
  *. (MulSpan a (PxSpan k1)) k = MulSpan a (PxSpan (toReal k * k1))
  *. (DivSpan a (PxSpan k1)) k = MulSpan a (PxSpan (toReal k / k1))
  *. (MaxSpan xs)            k = MaxSpan (strictTRMap (\x -> x *. k) xs)
  *. (MinSpan xs)            k = MinSpan (strictTRMap (\x -> x *. k) xs)
  *. s                       k = MulSpan s (PxSpan (toReal k))

instance /. Int  where
  /. l r = PxSpan (toReal l / toReal r)

instance /. Real where
  /. l r = PxSpan (l / toReal r)

instance /. Span where
  /. (PxSpan  a)             k = PxSpan (a / toReal k)
  /. (MulSpan a (PxSpan k1)) k = MulSpan a (PxSpan (k1 / toReal k))
  /. (DivSpan a (PxSpan k1)) k = DivSpan a (PxSpan (k1 * toReal k))
  /. (MaxSpan xs)            k = MaxSpan (strictTRMap (\x -> x /. k) xs)
  /. (MinSpan xs)            k = MinSpan (strictTRMap (\x -> x /. k) xs)
  /. s                       k = DivSpan s (PxSpan (toReal k))
