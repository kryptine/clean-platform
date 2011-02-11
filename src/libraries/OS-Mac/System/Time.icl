implementation module Time

import StdString, StdArray, StdClass, StdOverloaded, StdInt
import _Pointer

//String buffer size
MAXBUF :== 256

instance toString Tm
where
	toString tm = derefString (toStringTmC (packTm tm))
	where
		toStringTmC :: !{#Int} -> Pointer
		toStringTmC a0 = code {
			ccall asctime "A:p"
		}
instance toString Time
where
	toString (Time t) = derefString (toStringTimeC (packInt t))
	where	
		toStringTimeC :: !{#Int} -> Pointer
		toStringTimeC a0 = code {
			ccall ctime "A:p"
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
		ccall clock ":I:p"
	}

time :: !*World -> (!Time, !*World)
time world
	# (t, world)	= timeC 0 world
	= (Time t, world)
	where
	timeC :: !Int !*World -> (!Int,!*World)
	timeC a0 world = code {
		ccall time "I:I:p"
	}

gmTime :: !*World -> (!Tm, !*World)
gmTime world
	# ((Time t),world)	= time world
	# (tm, world)		= gmTimeC (packInt t) world
	= (derefTm tm, world)
	where
	gmTimeC :: !{#Int} !*World -> (!Int, !*World)
	gmTimeC tm world = code {
    	ccall gmtime "A:p:p"
	}

localTime :: !*World -> (!Tm, !*World)
localTime world
	# ((Time t),world)	= time world
	# (tm,world)		= localTimeC (packInt t) world
	= (derefTm tm, world)
	where
	localTimeC :: !{#Int} !*World -> (!Int, !*World)
	localTimeC tm world = code {
    	ccall localtime "A:p:p"
	}

mkTime :: !Tm -> Time
mkTime tm 
	# t = mkTimeC (packTm tm)
	= Time t
	where
	mkTimeC :: !{#Int} -> Int
	mkTimeC tm = code {
		ccall mktime "A:I"
	}

diffTime :: !Time !Time -> Int
diffTime (Time t1) (Time t2) = t1 - t2

strfTime :: !String !Tm -> String
strfTime format tm 
	# buf		= createArray MAXBUF 'X'
	# (len,buf)	= strfTimeC buf MAXBUF (packString format) (packTm tm) buf
	= buf % (0, len - 1)
	where
		strfTimeC :: !{#Char} !Int !{#Char} !{#Int} !{#Char} -> (!Int,!{#Char})
		strfTimeC a0 a1 a2 a3 a4 = code {
			ccall strftime "sIsA:I:A"
		}

//Custom deref and pack for the Tm structure
derefTm :: !Int -> Tm
derefTm tm =	{ sec = readInt4Z tm 0
				, min = readInt4Z tm 4
				, hour = readInt4Z tm 8
				, mday = readInt4Z tm 12
				, mon = readInt4Z tm 16
				, year = readInt4Z tm 20 
				, wday = readInt4Z tm 24
				, yday = readInt4Z tm 28 
				, isdst = readInt4Z tm 32 <> 0
				}

packTm :: !Tm -> {#Int}
packTm tm = 	{ ((tm.min << 32) bitor (tm.sec bitand 0xFFFFFFFF))
				, ((tm.mday << 32) bitor (tm.hour bitand 0xFFFFFFFF))
				, ((tm.year << 32) bitor (tm.mon bitand 0xFFFFFFFF))
				, ((tm.yday << 32) bitor (tm.wday bitand 0xFFFFFFFF))
				, (if tm.isdst 1 0) bitand 0xFFFFFFFF
				}
