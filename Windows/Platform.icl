implementation module Platform

getDevices :: !*World -> !*([String], !*World)
getDevices w
	# (ph, w) = getProcessheap w
	# (ptr, w) = heapAlloc ph 0 4096 w
	# (ret, w) = realQDD 0 ptr 4096 w
	| ret == 0 = abort "error in QueryDosDevice"
	# res = derefString ptr
	# (ok, w) = heapFree ph 0 ptr w
	= split "\0" res

realQDD :: !Pointer !Pointer !Int !*env -> !*(Int, !*env)
realQDD _ _ _ _ =
	= code {
		ccall QueryDosDevice "ppI:I:A"
	}
