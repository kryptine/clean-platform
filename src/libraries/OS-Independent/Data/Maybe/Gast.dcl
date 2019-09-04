definition module Data.Maybe.Gast

from Data.Maybe import :: Maybe
from Gast       import generic ggen, generic genShow, :: GenState

derive genShow Maybe
derive ggen    Maybe
