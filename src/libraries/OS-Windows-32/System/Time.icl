implementation module Time

import StdString, StdArray, StdClass, StdOverloaded, StdInt
import _Pointer

import code from library "msvcrt.txt"

//String buffer size
MAXBUF :== 256

instance == Timestamp
where
	(==) (Timestamp t1) (Timestamp t2) = t1 == t2
	
instance < Timestamp
where
	(<) (Timestamp t1) (Timestamp t2) = t1 < t2 



instance toString Tm
where
	toString tm = derefString (toStringTmC (packTm tm))
	where
		toStringTmC :: !{#Int} -> Pointer
		toStringTmC a0 = code {
			ccall asctime@4 "A:I"
		}
instance toString Timestamp
where
	toString (Timestamp t) = derefString (toStringTimeC (packInt t))
	where	
		toStringTimeC :: !{#Int} -> Pointer
		toStringTimeC a0 = code {
			ccall ctime@4 "A:I"
		}
instance toString Clock
where
	toString (Clock c) = toString c
instance toInt Timestamp
where
	toInt (Timestamp i) = i

clock :: !*World -> (!Clock, !*World)
clock world
	# (c, world) = clockC world
	= (Clock c, world)
	where
	clockC :: !*World -> (!Int, !*World)
	clockC world = code {
		ccall clock@0 ":I:A"
	}

time :: !*World -> (!Timestamp, !*World)
time world
	# (t, world)	= timeC 0 world
	= (Timestamp t, world)
	where
	timeC :: !Int !*World -> (!Int,!*World)
	timeC a0 world = code {
		ccall time@4 "I:I:A"
	}

gmTime :: !*World -> (!Tm, !*World)
gmTime world
	# ((Timestamp t),world)	= time world
	# (tm, world)		= gmTimeC (packInt t) world
	= (derefTm tm, world)
	where
	gmTimeC :: !{#Int} !*World -> (!Int, !*World)
	gmTimeC tm world = code {
    	ccall gmtime@4 "A:I:A"
	}

localTime :: !*World -> (!Tm, !*World)
localTime world
	# ((Timestamp t),world)	= time world
	# (tm,world)		= localTimeC (packInt t) world
	= (derefTm tm, world)
	where
	localTimeC :: !{#Int} !*World -> (!Int, !*World)
	localTimeC tm world = code {
    	ccall localtime@4 "A:I:A"
	}

mkTime :: !Tm -> Timestamp
mkTime tm 
	# t = mkTimeC (packTm tm)
	= Timestamp t
	where
	mkTimeC :: !{#Int} -> Int
	mkTimeC tm = code {
		ccall mktime@4 "A:I"
	}

diffTime :: !Timestamp !Timestamp -> Int
diffTime (Timestamp t1) (Timestamp t2) = t1 - t2

strfTime :: !String !Tm -> String
strfTime format tm 
	# buf		= createArray MAXBUF 'X'
	# (len,buf)	= strfTimeC buf MAXBUF (packString format) (packTm tm) buf
	= buf % (0, len - 1)
	where
		strfTimeC :: !{#Char} !Int !{#Char} !{#Int} !{#Char} -> (!Int,!{#Char})
		strfTimeC a0 a1 a2 a3 a4 = code {
			ccall strftime@16 "sIsA:I:A"
		}

//Custom deref and pack for the Tm structure
derefTm :: !Int -> Tm
derefTm tm =	{ sec = readInt tm 0
				, min = readInt tm 4
				, hour = readInt tm 8 
				, mday = readInt tm 12 
				, mon = readInt tm 16
				, year = readInt tm 20
				, wday = readInt tm 24
				, yday = readInt tm 28 
				, isdst = readInt tm 32 <> 0
				}
packTm :: !Tm -> {#Int}
packTm tm = 	{ tm.sec
				, tm.min
				, tm.hour
				, tm.mday
				, tm.mon
				, tm.year
				, tm.wday
				, tm.yday
				, if tm.isdst 1 0
				}