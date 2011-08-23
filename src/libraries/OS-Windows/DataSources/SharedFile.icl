implementation module SharedFile

import _WinBase, _Pointer, StdInt, StdArray, StdBool, StdFunc, FilePath, SharedDataSource
import StdMisc

sharedFile :: !FilePath -> Shared String *World
sharedFile path = createBasicDataSource "sharedFile" path mkOps id const
where
	mkOps world
		# (heap, world)	= getProcessHeap world
		// check NULL
		# (handle, world)	= createFile
								(packString path)
								(GENERIC_READ + GENERIC_WRITE)
								(FILE_SHARE_READ + FILE_SHARE_WRITE)
								NULL
								OPEN_ALWAYS
								FILE_ATTRIBUTE_NORMAL
								NULL
								world
		| handle == INVALID_HANDLE_VALUE = abort "create: invalid handle"
		= (ops heap handle, world)
		
	ops heap handle =
		{ read			= read
		, write			= write
		, getVersion	= getVersion
		, lock			= lock
		, lockExcl		= lockExcl
		, unlock		= unlock
		, close			= close
		}
	where
		read world
			# (len, world)		= getFileSize handle (packInt 0) world
			// check INVALID_FILE_SIZE
			# (pBuffer, world)	= heapAlloc heap 0 len world
			// check NULL
			# (ok, world)	= readFile handle pBuffer len (packInt 0) NULL world
			| not ok = abort "read error"
			#! b = derefCharArray pBuffer len
			# (ok, world)	= heapFree heap 0 pBuffer world
			// check ok
			# (ver, world) = getVersion world
			= (b, ver, world)
			
		write b world
			# len	= size b
			# (pBuffer, world)	= heapAlloc heap 0 len world
			// check NULL
			# pBuffer	= writeCharArray pBuffer b
			# (overlapped, world)	= heapAlloc heap HEAP_ZERO_MEMORY OVERLAPPED_SIZE_BYTES world
			// check NULL
			# (ok, world)	= writeFile handle pBuffer len (packInt 0) overlapped world
			| not ok = abort "write error"
			# (ok, world)	= heapFree heap 0 overlapped world
			// check ok
			# (ok, world)	= heapFree heap 0 pBuffer world
			// check ok
			# (ok, world)	= setEndOfFile handle world
			| not ok = abort "set EOF error"
			= world
			
		getVersion world
			# (len, world)		= getFileSize handle (packInt 0) world
			// check INVALID_FILE_SIZE
			= (len, world)
			
		lock = lock` 0
		lockExcl = lock` LOCKFILE_EXCLUSIVE_LOCK
			
		lock` flags	world
			# (overlapped, world)	= heapAlloc heap HEAP_ZERO_MEMORY OVERLAPPED_SIZE_BYTES world
			// check NULL
			# (ok, world)		= lockFileEx
									handle
									flags
									NULL
									0
									0xffff0000
									overlapped
									world
			| not ok = abort "lock file error"
			# (ok, world)	= heapFree heap 0 overlapped world
			// check ok
			= world
		
		unlock world
			# (ok, world)	= unlockFile handle 0 0 0 0xffff0000 world
			| not ok = abort "unlock error"
			= world
			
		close world
			# (ok, world)		= closeHandle handle world
			| not ok = abort "close error"
			= world
