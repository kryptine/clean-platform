implementation module StdMaybe

import StdFunc
import Data.Maybe

accMaybe :: .(St .x .a) !u:(Maybe .x) -> (!Maybe .a,!u:Maybe .x)
accMaybe f (Just x)
	# (a,x) = f x
	= (Just a,Just x)
accMaybe _ nothing
	= (Nothing,nothing)
