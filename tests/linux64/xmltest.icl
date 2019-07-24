module xmltest

import StdEnv

import Gast
import Gast.CommandLine

import Text.GenXML

import Control.GenBimap
import Data.Func
import Data.GenEq
import Data.Error

instance == (MaybeError a e) | == a & == e
where
	== (Ok x) (Ok y) = x == y
	== (Error x) (Error y) = x == y
	== _ _ = False
instance == () where == _ _ = True
instance == T where == a b = a === b
instance == T2 where == a b = a === b
instance == R where == a b = a === b
instance == R2 where == a b = a === b
instance == T3 where == a b = a === b


toAndFro :: !a -> Property | XMLEncode{|*|}, XMLDecode{|*|}, Gast, == a
toAndFro a
	# s = toXMLString a
	= fromXMLString s =.= Ok a

derive XMLEncode (), T, T2, R, R2, T3
derive XMLDecode (), T, T2, R, R2, T3
derive gEq T, T2, R, R2, T3
derive class Gast MaybeError, T, T2, R, R2, T3

/*
Start w = exposeProperties [] [] 
	[ EP $ cast 42 toAndFro
	, EP $ cast True toAndFro
//	, EP $ cast 0.0 toAndFro
//	, EP $ cast "" toAndFro
//	, EP $ (toAndFro For map toChar [0..255])
	, EP $ cast () toAndFro
	, EP $ cast (A 42 42) toAndFro
	, EP $ cast C toAndFro
	, EP $ cast {a=42,b=37} toAndFro
//	, EP $ cast {c=A 42 42,d={a=42,b=37}} toAndFro
	, EP $ cast (T3 (A 0 0) D) toAndFro
	] w
where
	cast :: !a -> (a -> Property) -> (a -> Property)
	cast _ = id
*/

:: T = A Int Int | B Int Int
:: T2 = C | D
:: T3 = T3 T T2

:: R = {a :: Int, b :: Int}
:: R2 = {c :: T, d :: R}

Start = toXML (T3 (A 0 0) D)
