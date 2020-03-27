definition module Data.Integer.GenJSON

from StdMaybe import :: Maybe
from Data.Integer import :: Integer
from Text.GenJSON import generic JSONEncode, generic JSONDecode, :: JSONNode

derive JSONEncode Integer
derive JSONDecode Integer
