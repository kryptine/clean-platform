implementation module Data.Unicode.Encodings.JS

import StdString, StdArray, StdInt, StdChar, StdBool, StdFunc
import Data.List

import Data.Unicode.UString
from Data.Unicode.UChar import instance fromInt UChar, instance toInt UChar
import qualified Data.Unicode.UChar

toHex i = ['\\u'] ++ ['0' \\ a <- [1..4-length letters]] ++ reverse (toHex` i)
where
	letters = reverse (toHex` i)
	
	toHex` 0 = []
	toHex` i = [hex.[i rem 16]:toHex` (i / 16)] 
	where
		hex = "0123456789ABCDEF" 

encodeString :: UString -> String
encodeString us = {c \\ c <- concatMap (convert o toInt) us}
where
	convert cc 
		| cc == fromChar '\n' = ['\\n']
		| cc == fromChar '\r' = ['\\r']	
		| cc == fromChar '\t' = ['\\t']
		| cc == fromChar '\f' = ['\\f']
		| cc == fromChar '\v' = ['\\v']
		| cc == fromChar '\b' = ['\\b']
		| cc == fromChar '\'' = ['\\\'']	
		| cc == fromChar '"'  = ['\\"']	
		| cc == fromChar '\\' = ['\\\\']	
		| not ('Data.Unicode.UChar'.isControl c) && 'Data.Unicode.UChar'.isAscii c = [fromInt cc]
				= toHex cc
	where
		c :: UChar
		c = fromInt cc 



