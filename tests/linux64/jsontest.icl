module jsontest

import StdEnv
import Text.GenJSON

import Gast
import Gast.CommandLine

testReal :: Real -> Property
testReal a =
	toString (toReal (toString a)) == toString a
	==> case fromJSON (fromString (toString (toJSON a))) of
		Nothing = prop False
		Just b
			| isNaN a = prop (isNaN b)
			= rtos a =.= rtos b
where
	rtos :: (Real -> String)
	rtos = toString 

testA :: a a -> Bool | Eq, JSONEncode{|*|}, JSONDecode{|*|} a
testA _ a = maybe False ((==)a) (fromJSON (fromString (toString (toJSON a))))

Start w = exposeProperties [Quiet] []
	[ EP (testReal)
	, EP (testA 0)
	, EP (testA False)
	, EP (testA ' ' For [toChar c\\c<-[0..255]])
	, EP (testA " " For diagonal [ggen{|*|} genState, [toString (toChar c)\\c<-[0..255]]])
	] w


