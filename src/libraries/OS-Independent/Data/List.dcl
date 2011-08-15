definition module List

import Functor, GenEq

unzip3			:: ![(.a,.b,.c)]			-> ([.a],[.b],[.c])
replaceInList	:: !(a a -> Bool) !a ![a]	-> [a]
splitWith		:: !(a -> Bool) ![a]		-> (![a],![a])
sortByIndex		:: ![(!Int,!a)]				-> [a]
intersperse		:: !a ![a]					-> [a]
getItems		:: ![a] ![Int]				-> [a]
isMemberGen		:: !a !.[a]					-> Bool | gEq{|*|} a

instance Functor []