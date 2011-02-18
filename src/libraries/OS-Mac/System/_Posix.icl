implementation module _Posix

import _Pointer

errno :: !*World -> (!Int,!*World)
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

stat :: !{#Char} !{#Char} !*World -> (!Int,!*World)
stat path buf world = code {
	ccall stat "ss:I:A"
}

unlink :: !{#Char} !*World -> (!Int,!*World)
unlink path world = code {
	ccall unlink "s:I:A"
}
fork :: !*World -> (!Int,!*World)
fork world = code {
	ccall fork ":I:A"
}
execvp :: !{#Char} !{#Pointer} !*World -> (!Int,!*World)
execvp name argv world = code {
	ccall execvp "sA:I:A"
}
waitpid :: !Int !{#Int} !Int !*World -> (!Int,!*World)
waitpid pid status_p options world = code {
    ccall waitpid "IAI:I:A"
}
exit :: !Int !*World -> (!.a,!*World)
exit num world = code {
	ccall exit "I:p:A"
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
