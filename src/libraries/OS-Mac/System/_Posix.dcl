definition module System._Posix

from System._Pointer import :: Pointer
from System.Time import :: Tm

WNOHANG		:==	0x00000001  
WUNTRACED	:== 0x00000002
MAXPATHLEN	:== 1024

DIRENT_D_NAME_OFFSET	:== 8

S_IFMT		:== 0170000
S_IFIFO		:== 0010000
S_IFCHR		:== 0020000
S_IFDIR		:== 0040000
S_IFBLK		:== 0060000
S_IFREG		:== 0100000
S_IFLNK		:== 0120000
S_IFSOCK	:== 0140000
S_IFWHT		:== 0160000

STDIN_FILENO  :== 0
STDOUT_FILENO :== 1
STDERR_FILENO :== 2

//Posix API calls
errno		:: !*w -> (!Int,!*w)
strerr		:: !Int -> Pointer
stat		:: !{#Char} !{#Char} !*w -> (!Int,!*w)
unlink		:: !{#Char} !*w -> (!Int,!*w)
fork		:: !*w -> (!Int,!*w)
execvp		:: !{#Char} !{#Pointer} !*w -> (!Int,!*w)
waitpid		:: !Int !{#Int} !Int !*w -> (!Int,!*w)
exit		:: !Int !*w -> (!.a,!*w) 
getcwd		:: !{#Char} !Int !*w -> (!Pointer,!*w)
chdir		:: !{#Char} !*w -> (!Int,!*w)
mkdir		:: !{#Char} !Int !*w -> (!Int,!*w)
rmdir		:: !{#Char} !*w -> (!Int,!*w)
rename		:: !{#Char} !{#Char} !*w -> (!Int,!*w)
opendir		:: !{#Char} !*w -> (!Pointer,!*w)
closedir	:: !Pointer !*w -> (!Int,!*w)
readdir		:: !Pointer !*w -> (!Pointer,!*w)
pipe        :: !Pointer !*w -> (!Int, !*w)
dup2        :: !Int !Int !*w -> (!Int, !*w)
close       :: !Int !*w -> (!Int, !*w)

//Memory (impure)
malloc	:: !Int -> Pointer
free	:: !Pointer -> Int
memcpy_string_to_pointer :: !Pointer !{#Char} !Int -> Pointer

//Posix datastructures
:: Stat =
	{ st_dev			:: !Int
	, st_ino			:: !Int
	, st_mode			:: !Int
	, st_nlink			:: !Int
	, st_uid			:: !Int
	, st_gid			:: !Int
	, st_rdev			:: !Int
	, st_atimespec		:: !Int
	, st_mtimespec		:: !Int
	, st_ctimespec		:: !Int
	, st_birthtimespec	:: !Int
	, st_size			:: !Int
	, st_blocks			:: !Int
	, st_blksize		:: !Int
	, st_flags			:: !Int
	, st_gen			:: !Int
	}
//Mapping to/from byte arrays
unpackStat	:: !{#Char} -> Stat
sizeOfStat	:: Int
