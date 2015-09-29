definition module Data.Eq

from StdOverloaded import class ==(..)
from StdClass import class Eq, <>
from StdBool import not

from StdBool import instance == Bool
from StdChar import instance == Char
from StdInt import instance == Int
from StdList import instance == [a]
from StdReal import instance == Real
from StdString import instance == {#Char}
from StdTuple import instance == (a,b)
from StdTuple import instance == (a,b,c)

// Make /= a synonym to <>
(/=) infix  4 //:: !a !a -> Bool
(/=) x y :== (<>) x y

