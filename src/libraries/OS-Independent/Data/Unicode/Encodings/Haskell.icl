implementation module Data.Unicode.Encodings.Haskell

import StdString, StdArray, StdInt, StdChar, StdBool, StdFunc
import Data.List

import Data.Unicode.UString
from Data.Unicode.UChar import instance fromInt UChar, instance toInt UChar
import qualified Data.UnicodeUChar

// Everything is the same as in JS but in toHex \u is \x
// and no need for trailing zeros, but is should be closed with \&
// http://www.haskell.org/onlinereport/lexemes.html

toHex i = ['\\x'] ++ reverse (toHex` i) ++ ['\\&'] // the last one shouldn't be here all the time, but it's safe this way...
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



