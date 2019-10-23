implementation module System.Time

import StdEnv
import System._Pointer
import System._WinBase
import Data.Integer
import Data.List
import Data.GenEq
from Data.Func import $
import Text

import code from library "msvcrt.txt"

//String buffer size
MAXBUF :== 256

derive gEq Timestamp

instance == Timestamp
where
	(==) (Timestamp t1) (Timestamp t2) = t1 == t2
	
instance < Timestamp
where
	(<) (Timestamp t1) (Timestamp t2) = t1 < t2 

instance toString Tm
where
	toString tm = trim (derefString (toStringTmC (packTm tm)))
	where
		toStringTmC :: !{#Int} -> Pointer
		toStringTmC a0 = code {
			ccall asctime "A:I"
		}
instance toString Timestamp
where
	toString (Timestamp t) 
	| t < 0 = abort "System.Time: Timestamp cannot be negative" 
	= trim (derefString (toStringTimeC (packInt t)))
	where	
		toStringTimeC :: !{#Int} -> Pointer
		toStringTimeC a0 = code {
			ccall ctime "A:I"
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
		ccall clock ":I:I"
	}

time :: !*World -> (!Timestamp, !*World)
time world
	# (t, world)	= timeC 0 world
	= (Timestamp t, world)
	where
	timeC :: !Int !*World -> (!Int,!*World)
	timeC a0 world = code {
		ccall time "I:I:I"
	}

gmTime :: !*World -> (!Tm, !*World)
gmTime world
	# ((Timestamp t),world)	= time world
	# tm					= gmTimeC (packInt t)
	= (derefTm tm, world)

localTime :: !*World -> (!Tm, !*World)
localTime world
	# ((Timestamp t),world)	= time world
	# (tm,world)			= localTimeC (packInt t) world
	= (derefTm tm, world)

mkTime :: !Tm !*World-> (!Timestamp, !*World)
mkTime tm world
	# (t, world) = mkTimeC (packTm tm) world
	= (Timestamp t, world)
where
	mkTimeC :: !{#Int} !*World -> (!Int, !*World)
	mkTimeC tm world = code {
		ccall mktime "A:I:I"
	}

timeGm :: !Tm -> Timestamp
timeGm tm = Timestamp (timegmC (packTm tm))
where
	timegmC :: !{#Int} -> Int
	timegmC tm = code {
		ccall _mkgmtime "A:I"
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
			ccall strftime "sIsA:I:A"
		}
		
toLocalTime :: !Timestamp !*World -> (!Tm,!*World)
toLocalTime (Timestamp t) world
	# (tm,world) = localTimeC (packInt t) world
	= (derefTm tm, world)

toGmTime :: !Timestamp -> Tm
toGmTime (Timestamp t) = derefTm (gmTimeC (packInt t))

gmTimeC :: !{#Int} -> Int
gmTimeC tm = code {
	ccall gmtime "A:I"
}

localTimeC :: !{#Int} !*World -> (!Int, !*World)
localTimeC tm world = code {
	ccall localtime "A:I:I"
}

//Custom deref and pack for the Tm structure
derefTm :: !Int -> Tm
derefTm tm =	{ sec	= readInt4S tm 0
				, min	= readInt4S tm 4
				, hour	= readInt4S tm 8 
				, mday	= readInt4S tm 12 
				, mon	= readInt4S tm 16
				, year	= readInt4S tm 20
				, wday	= readInt4S tm 24
				, yday	= readInt4S tm 28 
				, isdst	= readInt4S tm 32
				}

packTm :: !Tm -> {#Int}
packTm tm = (IF_INT_64_OR_32 packTm64 packTm32) tm

packTm64 :: !Tm -> {#Int}
packTm64 tm = 	{ tm.sec  + tm.min  << 32
				, tm.hour + tm.mday << 32
				, tm.mon  + tm.year << 32
				, tm.wday + tm.yday << 32
				, tm.isdst
				}
				
packTm32 :: !Tm -> {#Int}
packTm32 tm = 	{ tm.sec
				, tm.min
				, tm.hour
				, tm.mday
				, tm.mon
				, tm.year
				, tm.wday
				, tm.yday
				, tm.isdst
				}


//Number of 100ns ticks difference between the windows and linux epoch 
TICKSDIFF32   =: toInteger "11644473600" * TICKSPERSEC32
TICKSDIFF64   =: 11644473600 * TICKSPERSEC64

//Number of ticks per second (100 ns ticks)
TICKSPERSEC32 =: toInteger 10000000
TICKSPERSEC64 =: 10000000
/*
 * On windows GetSystemTimeAsFileTime returns a struct containing 2 32bit unsigned integers.
 * On 64 bit we therefore use an array of length 1, on 32 bit of length two.
 * On 64 bit we can use native integers, on 32 bit we use bigints.
 */
nsTime :: !*World -> (!Timespec, !*World)
nsTime w = IF_INT_64_OR_32 nsTime64 nsTime32 w
where
	nsTime64 w
		# (is, w) = GetSystemTimeAsFileTime {0} w
		= ({tv_sec=(is.[0] - TICKSDIFF64) / TICKSPERSEC64, tv_nsec=(is.[0] rem TICKSPERSEC64) * 100}, w)
	nsTime32 w
		# (is, w) = GetSystemTimeAsFileTime {0,0} w
		# ticks = uintToInt is.[0] + uintToInt is.[1] * (toInteger 2 ^ toInteger 32) - TICKSDIFF32
		= ({tv_sec=toInt (ticks / TICKSPERSEC32), tv_nsec=toInt (ticks rem TICKSPERSEC32) * 100}, w)

uintToInt :: Int -> Integer
uintToInt i
| i < 0 = toInteger i + {integer_s=0,integer_a={0,1}}
= toInteger i

timespecToStamp :: !Timespec -> Timestamp
timespecToStamp t = Timestamp t.tv_sec

timestampToSpec :: !Timestamp -> Timespec
timestampToSpec (Timestamp t) = {tv_sec=t,tv_nsec=0}

instance < Timespec
where
	(<) t1 t2
		| t1.tv_sec == t2.tv_sec = t1.tv_nsec < t2.tv_nsec
		= t1.tv_sec < t2.tv_sec

instance + Timespec
where
	(+) t1 t2 = let tv_nsec = t1.tv_nsec + t2.tv_nsec in
		{ tv_sec  = t1.tv_sec + t2.tv_sec + tv_nsec / 1000000000
		, tv_nsec = tv_nsec rem 1000000000
		}

instance - Timespec
where
	(-) t1 t2
		# tv_nsec = t1.tv_nsec - t2.tv_nsec
		| tv_nsec < 0
			= {tv_sec = t1.tv_sec - t2.tv_sec - 1, tv_nsec = 1000000000 + tv_nsec}
			= {tv_sec = t1.tv_sec - t2.tv_sec,     tv_nsec = tv_nsec}

instance zero Timespec
where zero = {tv_sec=0, tv_nsec=0}

instance == Timespec
where
	(==) t1 t2 = t1.tv_sec == t2.tv_sec && t1.tv_nsec == t2.tv_nsec
