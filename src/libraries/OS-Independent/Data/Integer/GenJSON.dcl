definition module Data.Integer.GenJSON

from Data.Maybe   import :: Maybe
from Text.GenJSON import generic JSONEncode, generic JSONDecode, :: JSONNode
from Data.Integer import :: Integer

derive JSONEncode Integer
derive JSONDecode Integer
