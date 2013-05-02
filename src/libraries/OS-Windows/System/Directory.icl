implementation module System.Directory

import StdArray, StdBool, StdClass, StdInt, StdChar, StdString

import Data.Void
import System.File
import System.FilePath
import System.OSError

import qualified System._Windows
import System._Pointer

createDirectory :: !FilePath !*World -> (!MaybeOSError Void, !*World)
createDirectory path world
	# (ok,world)	= 'System._Windows'.createDirectoryA (packString path) 'System._Windows'.NULL world
	| ok
		= (Ok Void, world)
	| otherwise
		= getLastOSError world

removeDirectory :: !FilePath !*World -> (!MaybeOSError Void, !*World)
removeDirectory path world
	# (ok,world)	= 'System._Windows'.removeDirectoryA (packString path) world
	| ok
		= (Ok Void, world)
	| otherwise
		= getLastOSError world

readDirectory :: !FilePath !*World -> (!MaybeOSError [FilePath], !*World)
readDirectory path world
	# win32FindData = createArray 'System._Windows'.WIN32_FIND_DATA_size_bytes '\0'
	# (handle, world) = 'System._Windows'.findFirstFileA (packString (path </> "*.*")) win32FindData world
	| handle == 'System._Windows'.INVALID_HANDLE_VALUE = getLastOSError world
	# (entry, world)	= readEntry win32FindData world
	# (entries,world)	= readEntries handle win32FindData world
	# (ok,world) = 'System._Windows'.findClose handle world
	| not ok = getLastOSError world
	= (Ok [entry:entries], world)
where
	readEntries :: !'System._Windows'.HANDLE !'System._Windows'.LPWIN32_FIND_DATA !*World -> (![String],!*World)
	readEntries handle win32FindData world
		# (ok,world)	= 'System._Windows'.findNextFileA handle win32FindData world
		| not ok
			= ([],world)
		# (entry,world)		= readEntry win32FindData world
		# (entries,world)	= readEntries handle win32FindData world
		= ([entry:entries],world)
	
	readEntry :: !'System._Windows'.LPWIN32_FIND_DATA !*World -> (!String,!*World) 
	readEntry win32FindData world 
		= (unpackString (win32FindData % ('System._Windows'.WIN32_FIND_DATA_cFileName_bytes_offset, 'System._Windows'.WIN32_FIND_DATA_cFileName_bytes_offset + 'System._Windows'.MAX_PATH - 1)), world)

getCurrentDirectory :: !*World -> (!MaybeOSError FilePath, !*World)
getCurrentDirectory world
	# buf			= createArray 'System._Windows'.MAX_PATH '\0'
	# (res,world)	= 'System._Windows'.getCurrentDirectoryA 'System._Windows'.MAX_PATH buf world
	| res == 0
		= getLastOSError world
	| otherwise
		= (Ok (unpackString buf),world)

setCurrentDirectory :: !FilePath !*World -> (!MaybeOSError Void, !*World)
setCurrentDirectory path world 
	# (ok,world)	= 'System._Windows'.setCurrentDirectoryA (packString path) world
	| ok
		= (Ok Void, world)
	| otherwise
		= getLastOSError world
