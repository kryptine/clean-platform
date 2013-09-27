implementation module Data.Unicode.UString

import StdArray, StdInt, StdList
from Data.Unicode.UChar import :: UChar, instance fromChar UChar

instance fromString UString
where
	fromString str = [fromChar c \\ c <-: str] 
	
instance % UString
where
	(%) str (f,t) = take (t-f+1) (drop f str )

instance +++ UString
where
	(+++) str1 str2 = str1 ++ str2


