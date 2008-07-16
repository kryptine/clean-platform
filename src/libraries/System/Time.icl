implementation module Time

import StdString, StdArray, StdClass, StdOverloaded, StdInt
import code from "_TimeC.o"

instance toString Tm
	where
	toString tm 
		# string	= createArray 24 '\0'
		# ok  		= toStringTmC {tm.sec,tm.min,tm.hour,tm.mday,tm.mon,tm.year,tm.wday,tm.yday,if tm.isdst 1 0} string
		= if (ok == 0) string ""
		where
		toStringTmC :: !{#Int} !{#Char} -> Int
		toStringTmC a s = code {
			ccall toStringTmC "AS:I"
		}

instance toString Time
	where
	toString (Time t)
		# string	= createArray 24 '\0'
		# ok		= toStringTimeC t string
		= if (ok == 0) string ""
		where
		toStringTimeC :: !Int !{#Char} -> Int
		toStringTimeC t s = code {
			ccall toStringTimeC "IS:I"
		}

instance toString Clock
	where
	toString (Clock c) = toString c

clock :: !*World -> (!Clock, !*World)
clock world
	# (c, world) = clockC world
	= (Clock c, world)
	where
	clockC :: !*World -> (!Int, !*World)
	clockC world = code {
		ccall clockC ":I:A"
	}

time :: !*World -> (!Time, !*World)
time world
	# (t, world)	= timeC world
	= (Time t, world)
	where
	timeC :: !*World -> (!Int,!*World)
	timeC world = code {
		ccall timeC ":I:A"
	}

gmTime :: !*World -> (!Tm, !*World)
gmTime world
	# tm	= createArray 9 0
	# world	= gmTimeC tm world
	= ({ sec = tm.[0], min = tm.[1], hour = tm.[2]
	   , mday = tm.[3], mon = tm.[4], year = tm.[5]
	   , wday = tm.[6], yday = tm.[7], isdst = tm.[8] <> 0
	   }, world)
	where
	gmTimeC :: !{#Int} !*World -> *World
	gmTimeC tm world = code {
    	ccall gmTimeC "A:V:A"
		fill_a 0 1
		pop_a 1
	}

localTime :: !*World -> (!Tm, !*World)
localTime world
	# tm	= createArray 9 0
	# world	= localTimeC tm world
	= ({ sec = tm.[0], min = tm.[1], hour = tm.[2]
	   , mday = tm.[3], mon = tm.[4], year = tm.[5]
	   , wday = tm.[6], yday = tm.[7], isdst = tm.[8] <> 0
	   }, world)
	where
	localTimeC :: !{#Int} !*World -> *World
	localTimeC tm world = code {
    	ccall localTimeC "A:V:A"
		fill_a 0 1
		pop_a 1
	}
		
mkTime :: !Tm -> Time
mkTime tm 
	# t = mkTimeC {tm.sec,tm.min,tm.hour,tm.mday,tm.mon,tm.year,tm.wday,tm.yday,if tm.isdst 1 0}
	= Time t
	where
	mkTimeC :: !{#Int} -> Int
	mkTimeC tm = code {
		ccall mkTimeC "A:I"
	}

diffTime :: !Time !Time -> Int
diffTime (Time t1) (Time t2) = t1 - t2

strfTime :: !String !Tm -> String
strfTime format tm 
	# format	= format +++ "\0"
	# (len,ptr) = strfTimeC format {tm.sec,tm.min,tm.hour,tm.mday,tm.mon,tm.year,tm.wday,tm.yday,if tm.isdst 1 0}
	# string	= createArray len '\0'
	# ok		= copyStringC len ptr string
	= if (ok == 0) string ""
	where
	strfTimeC :: !String !{#Int} -> (!Int,!Int)
	strfTimeC format tm = code {
		ccall strfTimeC "SA:VII"
	}
	copyStringC :: !Int !Int !String -> Int
	copyStringC len ptr s = code { 
		ccall copyStringC "IIS:I"
	}
