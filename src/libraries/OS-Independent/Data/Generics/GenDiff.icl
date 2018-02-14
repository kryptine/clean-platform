implementation module Data.Generics.GenDiff

import StdBool
from StdFunc import o
import StdGeneric
import StdInt
import StdList
import StdString

import Data.List
import Data.Maybe
from Text import class Text(concat), instance Text String

instance zero Diff where zero = {status=Common, value="", children=[]}

setStatus :: DiffStatus Diff -> Diff
setStatus s d = {d & status=s, children=map (setStatus s) d.children}

generic gDiff a :: a a -> [Diff]
gDiff{|Int|} x y
| x == y    = [{status=Common, value=toString x, children=[]}]
| otherwise = [{status=Removed, value=toString x, children=[]}, {status=Added, value=toString y, children=[]}]
gDiff{|UNIT|} UNIT UNIT = []
gDiff{|PAIR|} fx fy (PAIR x1 y1) (PAIR x2 y2) = fx x1 x2 ++ fy y1 y2
gDiff{|OBJECT|} fx (OBJECT x) (OBJECT y)  = fx x y
gDiff{|CONS of d|} fx (CONS x) (CONS y)   = [{status=Common, value=d.gcd_name, children=fx x y}]
gDiff{|EITHER|} fl fr (LEFT x)  (LEFT y)  = fl x y
gDiff{|EITHER|} fl fr (RIGHT x) (RIGHT y) = fr x y
gDiff{|EITHER|} fl fr (LEFT x)  (RIGHT y) = map (setStatus Removed) (fl x x) ++ map (setStatus Added) (fr y y)
gDiff{|EITHER|} fl fr (RIGHT x) (LEFT y)  = map (setStatus Removed) (fr x x) ++ map (setStatus Added) (fl y y)

derive gDiff []

diffToConsole :: ([Diff] -> String)
diffToConsole = concat o intersperse " " o map (display False)
where
	display :: Bool Diff -> String
	display p d =
		color +++
		if p` "(" "" +++
		d.value +++
		reset +++
		concat (map ((+++) " " o display True) d.children) +++
		color +++
		if p` ")" "" +++
		reset
	where
		color = case d.status of
			Common  -> reset
			Added   -> "\033[0;32m"
			Removed -> "\033[0;31m"
		reset = "\033[0m"
		p` = p && not (isEmpty d.children)
