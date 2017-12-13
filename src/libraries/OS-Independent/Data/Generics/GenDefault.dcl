definition module Data.Generics.GenDefault

import StdGeneric

generic gDefault a ::  a 

derive gDefault Int, Real, String, PAIR, EITHER, CONS, FIELD, OBJECT 

derive gDefault [], (,), (,,),  (,,,), (,,,,), (,,,,,), (,,,,,,), (,,,,,,,)

