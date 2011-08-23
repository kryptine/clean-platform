implementation module SharedMemory

import _WinBase, _Pointer, StdInt, StdTuple, StdString, StdArray, StdBool, StdFunc, FilePath, SharedDataSource, dynamic_string
import StdMisc

INT_SIZE :== IF_INT_64_OR_32 8 4

sharedMemory :: !a !*World -> (!Shared a World, !*World)
sharedMemory v world
	# (heap, world)	= getProcessHeap world
	# initStr		= copy_to_string v
	# sStr			= size initStr
	# (csec, world)	= heapAlloc heap 0 CRITICAL_SECTION_SIZE_BYTES world
	//# world			= initializeCriticalSection csec world
	# (iptr, world)	= heapAlloc heap 0 (INT_SIZE * 2) world
	# (vptr, world)	= heapAlloc heap 0 sStr world
	# vptr			= writeCharArray vptr initStr
	# iptr			= writeInt iptr 0 sStr
	# iptr			= writeInt iptr INT_SIZE vptr
	= (createBasicDataSource "sharedMemory" (toString iptr) (mkOps heap csec iptr) get putback, world)
where
	get str = fst (copy_from_string {c \\ c <-: str})
	putback v _ = copy_to_string v
		
	mkOps heap csec ptr world =
		({ read			= read
		, write			= write
		, getVersion	= getVersion
		, lock			= lock
		, lockExcl		= lockExcl
		, unlock		= unlock
		, close			= close
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
			# vptr			= writeCharArray vptr b
			#! ptr			= writeInt ptr INT_SIZE vptr
			= const world ptr // force eval of ptr
			
		getVersion world
			= (0, world)
			
		lock = lock`
		lockExcl = lock`
			
		lock` world
			= world
		
		unlock world
			= world
			
		close world
			= world
