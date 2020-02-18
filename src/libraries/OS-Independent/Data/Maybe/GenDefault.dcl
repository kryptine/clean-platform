definition module Data.Maybe.GenDefault

from Data.Maybe      import :: Maybe
from Data.GenDefault import generic gDefault

derive gDefault Maybe
