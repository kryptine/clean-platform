definition module Data.Generics.Map

import StdGeneric

generic gMap a b :: .a -> .b
derive gMap c, UNIT, PAIR, EITHER, CONS, FIELD, OBJECT, {}, {!} 

derive gMap [], (,), (,,),  (,,,), (,,,,), (,,,,,), (,,,,,,), (,,,,,,,)
