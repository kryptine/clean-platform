implementation module Data.Word8

import StdArray
import StdClass
import StdOverloaded
from StdInt import instance + Int, instance - Int, instance one Int, instance < Int, instance rem Int
from StdChar import instance toChar Int, instance fromChar Int

:: Word8 :== Char

instance fromChar Word8
where
	fromChar c = c

instance fromInt Word8
where
	fromInt i = toChar (i rem 0xFF)

instance toInt Word8
where
	toInt w8 = fromChar w8	

instance == Word8
where
	(==) a b = a == b
	
instance < Word8
where
	(<) a b = a < b

stringToBytes :: !String -> [Word8]
stringToBytes str = [c \\ c <-: str]

bytesToString :: ![Word8] -> String
bytesToString str = {c \\ c <- str}
