implementation module System._Posix

import System._Pointer, System.Time
import StdInt

errno :: !*w -> (!Int,!*w)
errno world = (getErrno,world)
where
	getErrno :: Int
	getErrno = readInt4S errnoAddr 0
	
	errnoAddr :: Pointer
	errnoAddr = code {
		ccall __error ":p"
	}

strerr :: !Int -> Pointer
strerr world = code {
	ccall strerror "I:p"
}

stat :: !{#Char} !{#Char} !*w -> (!Int,!*w)
stat path buf world = code {
	ccall stat$INODE64 "ss:I:A"
}

unlink :: !{#Char} !*w -> (!Int,!*w)
unlink path world = code {
	ccall unlink "s:I:A"
}
fork :: !*w -> (!Int,!*w)
fork world = code {
	ccall fork ":I:A"
}
execvp :: !{#Char} !{#Pointer} !*w -> (!Int,!*w)
execvp name argv world = code {
	ccall execvp "sA:I:A"
}
waitpid :: !Int !{#Int} !Int !*w -> (!Int,!*w)
waitpid pid status_p options world = code {
    ccall waitpid "IAI:I:A"
}
exit :: !Int !*w -> (!.a,!*w)
exit num world = code {
	ccall exit "I:p:A"
}
getcwd :: !{#Char} !Int !*w -> (!Pointer,!*w)
getcwd buf size_t world = code {
	ccall getcwd "sI:p:A"
}
chdir :: !{#Char} !*w -> (!Int,!*w)
chdir name world = code {
	ccall chdir "s:I:A"
}
mkdir :: !{#Char} !Int !*w -> (!Int,!*w)
mkdir name mode world = code {
	ccall mkdir "sI:I:A"
}
rmdir :: !{#Char} !*w -> (!Int,!*w)
rmdir name world = code {
	ccall rmdir "s:I:A"
}
rename :: !{#Char} !{#Char} !*w -> (!Int,!*w)
rename old new world = code {
	ccall rename "ss:I:A"
}
opendir	:: !{#Char} !*w -> (!Pointer,!*w)
opendir path world = code {
	ccall opendir "s:p:A"
}
closedir :: !Pointer !*w -> (!Int,!*w)
closedir dir world = code {
	ccall closedir "p:I:A"
}
readdir	:: !Pointer !*w -> (!Pointer,!*w)
readdir dir world = code {
	ccall readdir "p:p:A"
}
pipe :: !Pointer !*w -> (!Int, !*w)
pipe arr world = code {
    ccall pipe "p:I:A"
}
posix_openpt :: !Int !*w -> (!Int, !*w)
posix_openpt flags w = code {
	ccall posix_openpt "I:I:A"
}
grantpt     :: !Int !*w -> (!Int, !*w)
grantpt fp w = code {
	ccall grantpt "I:I:A"
}
unlockpt    :: !Int !*w -> (!Int, !*w)
unlockpt fp w = code {
	ccall unlockpt "I:I:A"
}
ptsname     :: !Int !*w -> (!Pointer, !*w)
ptsname fp w = code {
	ccall ptsname "I:p:A"
}
open        :: !Pointer !Int !*w -> (!Int, !*w)
open p flags w = code {
	ccall open "pI:I:A"
}
tcgetattr   :: !Int !Pointer !*w -> (!Int, !*w)
tcgetattr fp f w = code {
	ccall tcgetattr "Ip:I:A"
}
cfmakeraw   :: !Pointer !*w -> *w
cfmakeraw p w = code {
	ccall cfmakeraw "p:V:A"
}
tcsetattr   :: !Int !Int !Pointer !*w -> (!Int, !*w)
tcsetattr fp strategy p w = code {
	ccall tcsetattr "IIp:I:A"
}
setsid :: !*w -> *w
setsid w = code {
	ccall setsid ":V:A"
}
dup2 :: !Int !Int !*w -> (!Int, !*w)
dup2 old new world = code {
    ccall dup2 "II:I:A"
}

close :: !Int !*w -> (!Int, !*w)
close fd world = code {
    ccall close "I:I:A"
}

ioctl :: !Int !Int !Pointer !*w -> (!Int, !*w)
ioctl fd op ptr world = code {
    ccall ioctl "IIp:I:A"
}

fcntlArg :: !Int !Int !Int !*w -> (!Int, !*w)
fcntlArg fd op arg world = code {
    ccall fcntl "III:I:A"
}

read :: !Int !Pointer !Int !*w -> (!Int, !*w)
read fd buffer nBuffer world = code {
    ccall read "IpI:I:A"
}

write :: !Int !{#Char} !Int !*w -> (!Int, !*w)
write fd buffer nBuffer world = code {
    ccall write "IsI:I:A"
}

select_ :: !Int !Pointer !Pointer !Pointer !Pointer !*w -> (!Int, !*w)
select_ nfds readfds writefds exceptfds timeout world = code {
    ccall select "Ipppp:I:A"
}

kill :: !Int !Int !*w -> (!Int, !*w)
kill pid sig world = code {
    ccall kill "II:I:A"
}

timegm :: !{#Int} -> Int
timegm tm = code {
	ccall timegm "A:I"
}

malloc :: !Int -> Pointer
malloc num = code {
	ccall malloc "I:p"
}
mallocSt	:: !Int !*w -> (!Pointer, !*w)
mallocSt num w = code {
	ccall malloc "I:p:A"
}
free :: !Pointer -> Int 
free ptr = code {
	ccall free "p:I"
}
freeSt :: !Pointer !*w -> *w
freeSt ptr world = code {
   ccall free "p:V:A"
}
memcpy_string_to_pointer :: !Pointer !{#Char} !Int -> Pointer
memcpy_string_to_pointer p s n = code {
    ccall memcpy "psp:p"
}
clock_gettime :: !Int !Pointer !*w -> (!Int, !*w)
clock_gettime _ _ _ = code {
	ccall clock_gettime "Ip:I:A"
}

//Mapping to/from byte arrays
unpackStat :: !{#Char} -> Stat
unpackStat s =
	{ st_dev			= unpackInt4S s 0
	, st_ino			= unpackInt8  s 8
	, st_mode			= unpackInt2S s 4
	, st_nlink			= unpackInt2S s 6
	, st_uid			= unpackInt4S s 16
	, st_gid			= unpackInt4S s 20
	, st_rdev			= unpackInt8  s 24 
	, st_atimespec  	= {tv_sec=unpackInt8 s 32, tv_nsec=unpackInt8 s 40}
	, st_mtimespec  	= {tv_sec=unpackInt8 s 48, tv_nsec=unpackInt8 s 56}
	, st_ctimespec  	= {tv_sec=unpackInt8 s 64, tv_nsec=unpackInt8 s 72}
	, st_birthtimespec	= {tv_sec=unpackInt8 s 80, tv_nsec=unpackInt8 s 88}
	, st_size       	= unpackInt8  s 96
	, st_blocks    		= unpackInt8  s 104
	, st_blksize    	= unpackInt4S s 112
	, st_flags      	= unpackInt4S s 116
	, st_gen        	= unpackInt4S s 120
	}

sizeOfStat :: Int
sizeOfStat = 144
