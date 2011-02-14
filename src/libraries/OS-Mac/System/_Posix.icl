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

stat :: !{#Char} !{#Char} -> !Int
stat path buf = code {
	ccall stat "ss:I"
}

unlink :: !{#Char} -> !Int
unlink path = code {
	ccall unlink "s:I"
}
