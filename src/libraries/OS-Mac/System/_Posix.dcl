definition module _Posix

from _Pointer import :: Pointer

WNOHANG		:==	0x00000001  
WUNTRACED	:== 0x00000002
MAXPATHLEN	:== 1024

DIRENT_D_NAME_OFFSET	:== 8

//Posix API calls
errno		:: !*World -> (!Int,!*World)
strerr		:: !Int -> Pointer
stat		:: !{#Char} !{#Char} !*World -> (!Int,!*World)
unlink		:: !{#Char} !*World -> (!Int,!*World)
fork		:: !*World -> (!Int,!*World)
execvp		:: !{#Char} !{#Pointer} !*World -> (!Int,!*World)
waitpid		:: !Int !{#Int} !Int !*World -> (!Int,!*World)
exit		:: !Int !*World -> (!.a,!*World) 
getcwd		:: !{#Char} !Int !*World -> (!Pointer,!*World)
chdir		:: !{#Char} !*World -> (!Int,!*World)
mkdir		:: !{#Char} !Int !*World -> (!Int,!*World)
rmdir		:: !{#Char} !*World -> (!Int,!*World)
rename		:: !{#Char} !{#Char} !*World -> (!Int,!*World)
opendir		:: !{#Char} !*World -> (!Pointer,!*World)
closedir	:: !Pointer !*World -> (!Int,!*World)
readdir		:: !Pointer !*World -> (!Pointer,!*World)

//Memory (impure)
malloc	:: !Int -> Pointer
free	:: !Pointer -> Int
memcpy_string_to_pointer :: !Pointer !{#Char} !Int -> Pointer
