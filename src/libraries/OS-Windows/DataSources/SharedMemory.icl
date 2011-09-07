implementation module SharedMemory

import _WinBase, _Pointer, StdInt, StdTuple, StdString, StdArray, StdBool, StdFunc, FilePath, SharedDataSource, dynamic_string
import StdMisc

/**
* |--------------------------------------------|
* | size | value ptr | version | observer list |
* |--------------------------------------------|
*         |                     |               |-------------------|
*         |                      -------------> | observer 0 | next |
*         V                                     |-------------------|
*        |----------------------|                             |
*        | dynamic value string |                              --> ...
*        |----------------------|                 
*        <-------- size -------->
*/
sharedMemory :: !a !*World -> (!Shared a World, !*World) | TC a
sharedMemory v world
	# (heap, world)	= getProcessHeap world
	# initStr		= copy_to_string v
	# sStr			= size initStr
	# (mutx, world)	= createMutexA NULL False NULL world
	// check ok
	# (iptr, world)	= heapAlloc heap 0 (INT_SIZE * 4) world
	# (vptr, world)	= heapAlloc heap 0 sStr world
	# vptr			= writeCharArray vptr initStr
	# iptr			= writeInt iptr 0 sStr				// init size of dynamic string
	# iptr			= writeInt iptr INT_SIZE vptr		// init pointer to dynamic string
	# iptr			= writeInt iptr (INT_SIZE * 2) 0	// init version number
	# iptr			= writeInt iptr (INT_SIZE * 3) NULL	// init linked list of observers
	= (createBasicDataSource "sharedMemory" (toString iptr) (mkOps heap mutx iptr) id const, world)
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
		, addObserver	= addObserver
		}, world)
	where
		read world
			# sStr		= readInt ptr 0
			# vptr		= readInt ptr INT_SIZE
			# ver		= readInt ptr (INT_SIZE * 2)
			# str		= derefCharArray vptr sStr
			= (fst (copy_from_string {c \\ c <-: str}), ver, world)
			
		write b world
			# dstr			= copy_to_string b
			# vptr			= readInt ptr INT_SIZE
			# (ok, world)	= heapFree heap 0 vptr world
			# sStr			= size dstr
			# (vptr, world)	= heapAlloc heap 0 sStr world
			| vptr == NULL = abort "writing to shared memory: error allocating memory"
			# vptr			= writeCharArray vptr dstr
			# ptr			= writeInt ptr 0 sStr
			# ptr			= writeInt ptr INT_SIZE vptr
			// increase version number
			# ver			= readInt ptr (INT_SIZE * 2)
			# ptr			= writeInt ptr (INT_SIZE * 2) (inc ver)
			// notify observers and empty list
			# wptr			= readInt ptr (INT_SIZE * 3)
			# world			= notifyObservers wptr world
			# ptr			= writeInt ptr (INT_SIZE * 3) NULL
			= forceEval ptr world
		where
			notifyObservers wptr world
				| wptr == NULL = world
				# obs 			= readInt wptr 0
				# (_, world)	= setEvent obs world
				# next 			= readInt wptr INT_SIZE
				# world			= notifyObservers next world
				# (ok, world)	= heapFree heap 0 wptr world
				| not ok = abort "notifyWaiters: error freeing heap"
				= world

		getVersion world
			= (readInt ptr (INT_SIZE * 2), world)
		
		addObserver observer world
			# (nptr, world)	= heapAlloc heap 0 (INT_SIZE * 2) world
			# optr			= readInt ptr (INT_SIZE * 3)
			# nptr			= writeInt nptr 0 observer
			# nptr			= writeInt nptr INT_SIZE optr
			# ptr			= writeInt ptr (INT_SIZE * 3) nptr
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
