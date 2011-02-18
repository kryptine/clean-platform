definition module _Posix

from _Pointer import :: Pointer

WNOHANG		:==	0x00000001  
WUNTRACED	:== 0x00000002

//Posix API calls
errno	:: !*World -> (!Int,!*World)
strerr	:: !Int -> Pointer
stat	:: !{#Char} !{#Char} !*World -> (!Int,!*World)
unlink	:: !{#Char} !*World -> (!Int,!*World)
fork	:: !*World -> (!Int,!*World)
execvp	:: !{#Char} !{#Pointer} !*World -> (!Int,!*World)
waitpid	:: !Int !{#Int} !Int !*World -> (!Int,!*World)
exit	:: !Int !*World -> (!.a,!*World) 

//Memory (impure)
malloc	:: !Int -> Pointer
free	:: !Pointer -> Int
memcpy_string_to_pointer :: !Pointer !{#Char} !Int -> Pointer
