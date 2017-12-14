implementation module System.Terminal.VT100

from StdFunc import o
from Data.Func import $
from Text import class Text(split,join), instance Text String

import StdList, StdBool
import StdDebug

import Data.Tuple
import Control.Applicative

import Text.HTML

vt100render :: Int Int Int -> (String -> HtmlTag)
vt100render w h t = TtTag [StyleAttr $ style 0] o rvt 0 0 o fromString
	where
		rvt :: Int Int [Char] -> [HtmlTag]
		rvt x y [] = []
		rvt x y [c:cs]
			| x == w = [BrTag []:rvt 0 (y + 1) [c:cs]]
			| c == ' ' = [Html "&nbsp;":rvt x y cs]
			| c == '\t' = rvt x y (repeatn (t - x rem t) ' ' ++ cs)
			| c == '\n' = rvt w y cs
			| c <> '\x1B' = [Text $ toString c:rvt (x+1) y cs]
			=  case cs of
				['7':cs] = trace_n "sc not supported" $ rvt x y cs
				['8':cs] = trace_n "rc not supported" $ rvt x y cs
				['[':cs] = case cs of
					['H':cs] = trace_n "home not supported" $ rvt x y cs
					['?','2','5','l':cs] = trace_n "civis not supported" $ rvt x y cs
					['?','2','5','h':cs] = trace_n "cvvis not supported" $ rvt x y cs
					['K':cs] = trace_n "el not supported" $ rvt x y cs
					['0':'K':cs] = trace_n "el not supported" $ rvt x y cs
					['1':'K':cs] = trace_n "el1 not supported" $ rvt x y cs
					['2':'K':cs] = trace_n "e12 not supported" $ rvt x y cs
					cs = case uptom ['m','H','f','h'] cs of
						('m', codes, cs) = [SpanTag
								[StyleAttr $ join " " $ map (style o toInt)
									$ split ";" $ toString codes] $ rvt x y cs]
						('H', codes, cs) = trace_n "Curser movement not supported" $ rvt x y cs
						('f', codes, cs) = trace_n "Curser movement not supported" $ rvt x y cs
						(c, _, cs) = trace_n ("Escape: " +++ toString c) $ rvt x y cs
				_ = trace_n "Unsupported escape" $ rvt x y cs

		uptom :: [Char] [Char] -> (Char, [Char], [Char])
		uptom m [c:cs]
			| isMember c m = (c, [], cs)
			| isDigit c || c == ';' = appSnd3 (\cc->[c:cc]) $ uptom m cs
			= ('-', [], cs)

		style :: Int -> String
		style a = case a of
			0 = "text-decoration:none;color:black;background-color:white"
			4 = "text-decoration:underline;"
			30 = "color:black"
			31 = "color:red"
			32 = "color:green"
			33 = "color:yellow"
			34 = "color:blue"
			35 = "color:magenta"
			36 = "color:cyan"
			37 = "color:white"
			39 = "color:black"
			40 = "background-color:black"
			41 = "background-color:red"
			42 = "background-color:green"
			43 = "background-color:yellow"
			44 = "background-color:blue"
			45 = "background-color:magenta"
			46 = "background-color:cyan"
			47 = "background-color:white"
			49 = "background-color:white"
			_ = ""
