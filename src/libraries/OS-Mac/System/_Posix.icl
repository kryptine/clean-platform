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

malloc :: !Int -> Pointer
malloc num = code {
	ccall malloc "p:p"
}
free :: !Pointer -> Int 
free ptr = code {
	ccall free "p:I"
}
memcpy_string_to_pointer :: !Pointer !{#Char} !Int -> Pointer
memcpy_string_to_pointer p s n = code {
    ccall memcpy "psp:p"
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
    , st_atimespec  	= unpackInt8  s 32
    , st_mtimespec  	= unpackInt8  s 48
    , st_ctimespec  	= unpackInt8  s 64 
	, st_birthtimespec	= unpackInt8  s 80
    , st_size       	= unpackInt8  s 96 
    , st_blocks    		= unpackInt8  s 104 
    , st_blksize    	= unpackInt4S s 112
    , st_flags      	= unpackInt4S s 116
    , st_gen        	= unpackInt4S s 120
    }

sizeOfStat :: Int
sizeOfStat = 144
