definition module _Posix

from _Pointer import :: Pointer

//Posix API calls

errno	:: !*World -> (!Int,!*World)
strerr	:: !Int -> Pointer
stat	:: !{#Char} !{#Char} -> Int
unlink	:: !{#Char} -> Int
