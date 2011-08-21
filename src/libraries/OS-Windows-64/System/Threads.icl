implementation module Threads

import _WinBase, _Pointer, StdArray, StdInt, StdClass, dynamic_string, _Unsafe
foreign export threadFunc

fork :: !(*World -> *World) !*World -> (!ThreadId, !*World)
fork threadF world
	# threadFStr = copy_to_string threadF
	# (heap, world) = getProcessHeap world
	# s = size threadFStr
	# (ptr, world) = heapAlloc heap 0 (17 + s) world
	# ptr = store_int thread_func_address 0 ptr
	# ptr = store_int s 8 ptr
	# ptr = writeCharArray (ptr + 16) (packString threadFStr)
	# ptr = ptr - 16
	# (handle, _, world) = CreateThread 0 0 clean_new_thread_address ptr 0 world
	= (handle, world)

waitForThread :: !ThreadId !*World -> *World
waitForThread tid world
	# (_, world) = waitForSingleObject tid INFINITE world
	= world

threadFunc :: !LPVOID -> DWORD
threadFunc ptr
	# (s, ptr)		= load_int 8 ptr
	# threadFStr	= derefCharArray (ptr + 16) s
	# (threadF,_)	= copy_from_string {s` \\ s` <-: threadFStr}
	= appUnsafe (threadFunc` ptr threadF) 0
where
	threadFunc` :: !LPVOID !(*World -> *World) !*World -> *World
	threadFunc` ptr threadF world
		# (heap, world) = getProcessHeap world
		# (ok, world) = heapFree heap 0 ptr world
		// check ok
		# world = threadF world
		= world

clean_new_thread_address :: Int
clean_new_thread_address = code {
	pushL clean_new_thread
}

thread_func_address :: Int
thread_func_address = code {
	pushL threadFunc
}

store_int :: !Int !Int !Int -> Int;
store_int v o p = code {
	push_b 1
	pushI -8
	addI
	update_b 0 2
	pop_b 1

	push_b 2
	push_b 2
	addI
	push_b_a 0
	pop_b 1
	fill1_r _ 0 1 0 01
	push_a_b 0
	pop_a 1

	push_b 1
	push_b 1
	subI
	updatepop_b 0 3
}

load_int :: !Int !Int -> (!Int,!Int);
load_int o p = code {
	push_b 1
	push_b 1
	addI
	load_i 0
	updatepop_b 0 1
}