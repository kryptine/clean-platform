definition module Data.Generics.GenDefault

import StdGeneric

generic gDefault a ::  a 

derive gDefault Int, Bool, Real, Char, String, UNIT, PAIR, EITHER, CONS, FIELD, OBJECT, RECORD

derive gDefault (), [], (,), (,,),  (,,,), (,,,,), (,,,,,), (,,,,,,), (,,,,,,,)
