definition module System._Posix

from System._Pointer import :: Pointer
from System.Time import :: Tm
from StdFile import class FileSystem

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

//Posix API calls
errno		:: !*w -> (!Int,!*w) | FileSystem w
strerr		:: !Int -> Pointer
stat		:: !{#Char} !{#Char} !*w -> (!Int,!*w) | FileSystem w
unlink		:: !{#Char} !*w -> (!Int,!*w) | FileSystem w
fork		:: !*w -> (!Int,!*w) | FileSystem w
execvp		:: !{#Char} !{#Pointer} !*w -> (!Int,!*w) | FileSystem w
waitpid		:: !Int !{#Int} !Int !*w -> (!Int,!*w) | FileSystem w
exit		:: !Int !*w -> (!.a,!*w) | FileSystem w
getcwd		:: !{#Char} !Int !*w -> (!Pointer,!*w) | FileSystem w
chdir		:: !{#Char} !*w -> (!Int,!*w) | FileSystem w
mkdir		:: !{#Char} !Int !*w -> (!Int,!*w) | FileSystem w
rmdir		:: !{#Char} !*w -> (!Int,!*w) | FileSystem w
rename		:: !{#Char} !{#Char} !*w -> (!Int,!*w) | FileSystem w
opendir		:: !{#Char} !*w -> (!Pointer,!*w) | FileSystem w
closedir	:: !Pointer !*w -> (!Int,!*w) | FileSystem w
readdir		:: !Pointer !*w -> (!Pointer,!*w) | FileSystem w

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
