definition module Data.Ord

from StdOverloaded import class <(..)
from StdClass import class Ord, >, <=, >=, min, max // EXPLICIT because these are MACROS!
from StdBool import not // USED in macros from StdClass

from StdChar import instance < Char
from StdInt import instance < Int
from StdList import instance < [a]
from StdReal import instance < Real
from StdString import instance < {#Char}
from StdTuple import instance < (a,b)
from StdTuple import instance < (a,b,c)
