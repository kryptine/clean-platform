implementation module Directory

import StdArray, StdBool, StdClass, StdInt, StdChar, StdString

import Void
import FilePath
import OSError

import _Windows
import _Pointer

createDirectory :: !FilePath !*World -> (!MaybeOSError Void, !*World)
createDirectory path world
	# (ok,world)	= createDirectoryA (packString path) NULL world
	| ok
		= (Ok Void, world)
	| otherwise
		= getLastOSError world

removeDirectory :: !FilePath !*World -> (!MaybeOSError Void, !*World)
removeDirectory path world
	# (ok,world)	= removeDirectoryA (packString path) world
	| ok
		= (Ok Void, world)
	| otherwise
		= getLastOSError world

renameDirectory :: !FilePath !FilePath !*World -> (!MaybeOSError Void, !*World)
renameDirectory oldpath newpath world
	# (ok,world)	= moveFileA (packString oldpath) (packString newpath) world
	| ok
		= (Ok Void, world)
	| otherwise
		= getLastOSError world

readDirectory :: !FilePath !*World -> (!MaybeOSError [FilePath], !*World)
readDirectory path world
	# win32FindData = createArray WIN32_FIND_DATA_size_bytes '\0'
	# (handle, world) = findFirstFileA (packString (path </> "*.*")) win32FindData world
	| handle == INVALID_HANDLE_VALUE = getLastOSError world
	# (entry, world)	= readEntry win32FindData world
	# (entries,world)	= readEntries handle win32FindData world
	# (ok,world) = findClose handle world
	| not ok = getLastOSError world
	= (Ok [entry:entries], world)
where
	readEntries :: !HANDLE !LPWIN32_FIND_DATA !*World -> (![String],!*World)
	readEntries handle win32FindData world
		# (ok,world)	= findNextFileA handle win32FindData world
		| not ok
			= ([],world)
		# (entry,world)		= readEntry win32FindData world
		# (entries,world)	= readEntries handle win32FindData world
		= ([entry:entries],world)
	
	readEntry :: !LPWIN32_FIND_DATA !*World -> (!String,!*World) 
	readEntry win32FindData world
		= (unpackString (win32FindData % (WIN32_FIND_DATA_cFileName_bytes_offset, MAX_PATH)), world)

getCurrentDirectory :: !*World -> (!MaybeOSError FilePath, !*World)
getCurrentDirectory world
	# buf			= createArray MAX_PATH '\0'
	# (res,world)	= getCurrentDirectoryA MAX_PATH buf world
	| res == 0
		= getLastOSError world
	| otherwise
		= (Ok {c \\ c <-: buf | c <> '\0'},world)

setCurrentDirectory :: !FilePath !*World -> (!MaybeOSError Void, !*World)
setCurrentDirectory path world 
	# (ok,world)	= setCurrentDirectoryA (packString path) world
	| ok
		= (Ok Void, world)
	| otherwise
		= getLastOSError world
