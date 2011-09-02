implementation module SharedMemory

import _WinBase, _Pointer, StdInt, StdTuple, StdString, StdArray, StdBool, StdFunc, FilePath, SharedDataSource, dynamic_string
import StdMisc

/**
* |---------------------------------|
* | size | value ptr | waiters list |
* |---------------------------------|
*         |           |               |-----------------|
*         |            -------------> | waiter 0 | next |
*         V                           |-----------------|
*        |----------------------|                 |
*        | dynamic value string |                  --> ...
*        |----------------------|                 
*        <-------- size -------->
*/
sharedMemory :: !a !*World -> (!Shared a World, !*World)
sharedMemory v world
	# (heap, world)	= getProcessHeap world
	# initStr		= copy_to_string v
	# sStr			= size initStr
	# (mutx, world)	= createMutexA NULL False NULL world
	// check ok
	# (iptr, world)	= heapAlloc heap 0 (INT_SIZE * 3) world
	# (vptr, world)	= heapAlloc heap 0 sStr world
	# vptr			= writeCharArray vptr initStr
	# iptr			= writeInt iptr 0 sStr
	# iptr			= writeInt iptr INT_SIZE vptr
	# iptr			= writeInt iptr (INT_SIZE * 2) NULL
	= (createBasicDataSource "sharedMemory" (toString iptr) (mkOps heap mutx iptr) get putback, world)
where
	get str = fst (copy_from_string {c \\ c <-: str})
	putback v _ = copy_to_string v
		
	mkOps heap mutx ptr world =
		({ read			= read
		, write			= write
		, getVersion	= getVersion
		, lock			= lock
		, lockExcl		= lockExcl
		, unlock		= unlock
		, close			= close
		, addWaiter		= addWaiter
		}, world)
	where
		read world
			# sStr		= readInt ptr 0
			# vptr		= readInt ptr INT_SIZE
			# str		= derefCharArray vptr sStr
			= (str, 0, world)
			
		write b world
			# vptr			= readInt ptr INT_SIZE
			# (ok, world)	= heapFree heap 0 vptr world
			# sStr			= size b
			# (vptr, world)	= heapAlloc heap 0 sStr world
			| vptr == NULL = abort "writing to shared memory: error allocating memory"
			# vptr			= writeCharArray vptr b
			# ptr			= writeInt ptr 0 sStr
			# ptr			= writeInt ptr INT_SIZE vptr
			# wptr			= readInt ptr (INT_SIZE * 2)
			# world			= notifyWaiters wptr world
			# ptr			= writeInt ptr (INT_SIZE * 2) NULL
			= forceEval ptr world
		where
			notifyWaiters wptr world
				| wptr == NULL = world
				# waiter 		= readInt wptr 0
				# (_, world)	= setEvent waiter world
				# next 			= readInt wptr INT_SIZE
				# world			= notifyWaiters next world
				# (ok, world)	= heapFree heap 0 wptr world
				| not ok = abort "notifyWaiters: error freeing heap"
				= world

		getVersion world
			= (0, world)
		
		addWaiter waiter world
			# (nptr, world)	= heapAlloc heap 0 (INT_SIZE * 2) world
			# optr			= readInt ptr (INT_SIZE * 2)
			# nptr			= writeInt nptr 0 waiter
			# nptr			= writeInt nptr INT_SIZE optr
			# ptr			= writeInt ptr (INT_SIZE * 2) nptr
			= forceEval ptr world
			
		lock = lock`
		lockExcl = lock`
		lock` world
			# (r, world) = waitForSingleObject mutx INFINITE world
			// check r
			= world
		
		unlock world
			# (ok, world) = releaseMutex mutx world
			// check ok
			= world
			
		close world = world
