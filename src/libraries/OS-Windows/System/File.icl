implementation module System.File

//StdEnv
import StdArray
import StdBool
import StdFile
import StdList
import StdMisc
import StdString

import Data.Func
import Data.Error
import System.OSError
import System._Pointer
import System.Time
import Text

CHUNK_SIZE :== 1024
 
from System._Windows import 
	::HANDLE, 
	:: LPWIN32_FIND_DATA, 
	WIN32_FIND_DATA_size_bytes, 
	INVALID_HANDLE_VALUE,
	deleteFileA, 
	findClose,
	findFirstFileA,
	:: DWORD,
	SYSTEMTIME_wYear_offset,
	SYSTEMTIME_wMinute_offset,
	SYSTEMTIME_wSecond_offset,
	SYSTEMTIME_wHour_offset,
	SYSTEMTIME_wMonth_offset,
	SYSTEMTIME_wDay_offset,
	SYSTEMTIME_wDayOfWeek_offset,
	SYSTEMTIME_size_bytes,
	WIN32_FIND_DATA_ftCreationTime_bytes_offset,
	WIN32_FIND_DATA_ftLastWriteTime_bytes_offset,
	WIN32_FIND_DATA_ftLastAccessTime_bytes_offset,
	FILETIME_size_bytes,
	FILE_ATTRIBUTE_DIRECTORY,
	moveFileA,
	:: FILETIME,
	fileTimeToSystemTime,
	fileTimeToTimeSpec,
	:: LPSYSTEMTIME
	
instance toString FileError
where
	toString CannotOpen = "Cannot open"
	toString CannotClose = "Cannot close"
	toString IOError = "I/O error"

readFile :: !String !*env -> (!MaybeError FileError String, !*env) | FileSystem env
readFile filename env = withFile filename FReadData readAll env

readAll :: !*File -> (!MaybeError FileError String, !*File)
readAll file
	# (ok,file)  = fseek file 0 FSeekEnd
	| not ok     = (Error IOError,file)
	# (pos,file) = fposition file
	# (err,file) = ferror file
	| err        = (Error IOError,file)
	# (ok,file)  = fseek file 0 FSeekSet
	| not ok     = (Error IOError,file)
	# (str,file) = freads file pos
	# (err,file) = ferror file
	| err        = (Error IOError,file)
	| otherwise  = (Ok str,file)

readFileLines :: !String !*env -> (!MaybeError FileError [String], !*env) | FileSystem env
readFileLines filename env = withFile filename FReadData readAllLines env

readAllLines :: !*File -> (!MaybeError FileError [String], !*File)
readAllLines file
# (result, file) = rec file []
= case result of
	Error e	   = (Error e, file)
	Ok lines = (Ok $ reverse lines, file)
where	
	rec :: *File [String] -> (!MaybeError FileError [String], *File)
	rec file acc 
		# (string, file) = freadline file
		# (err,file)	 = ferror file
		| err			 = (Error IOError,file)			
		| string == ""   = (Ok acc, file)
		| otherwise      = rec file [string:acc]

writeFile :: !String !String !*env -> (!MaybeError FileError (), !*env) | FileSystem env
writeFile filename contents env = 
	withFile filename FWriteData (\file -> (Ok (), fwrites contents file)) env

withFile :: !String Int (*File -> (!MaybeError FileError a,!*File)) !*env 
			-> (!MaybeError FileError a, !*env) | FileSystem env
withFile filename filemode operation env
# (ok,file,env)	= fopen filename filemode env
| not ok			= (Error CannotOpen, env)
# (result,file)		= operation file
| isError result 	= (result, env)
# (ok,env)	 		= fclose file env
| not ok			= (Error CannotClose, env)
= (Ok (fromOk result), env)

fileExists ::  !String !*World -> (!Bool, !*World)
fileExists filename world
# win32FindData = createArray WIN32_FIND_DATA_size_bytes '\0'
# (handle, world) = findFirstFileA (packString filename) win32FindData world
| handle == INVALID_HANDLE_VALUE = (False, world)
# (_,world) = findClose handle world
= (True, world)

deleteFile :: !String !*World -> (!MaybeOSError (), !*World)
deleteFile filename world 
	# (ok, world) = deleteFileA (packString filename) world
	| ok == 0 = getLastOSError world
	= (Ok (), world)

getFileInfo :: !String !*World -> (!MaybeOSError FileInfo, !*World)
getFileInfo filename world
	# win32FindData = createArray WIN32_FIND_DATA_size_bytes '\0'
	# (handle, world) = findFirstFileA (packString filename) win32FindData world
	| handle == INVALID_HANDLE_VALUE = getLastOSError world
	# creationTime     = fileTimeToTimeSpec (toFileTimeArray win32FindData WIN32_FIND_DATA_ftCreationTime_bytes_offset)
	# lastModifiedTime = fileTimeToTimeSpec (toFileTimeArray win32FindData WIN32_FIND_DATA_ftLastWriteTime_bytes_offset)
	# lastAccessedTime = fileTimeToTimeSpec (toFileTimeArray win32FindData WIN32_FIND_DATA_ftLastAccessTime_bytes_offset)
	# info = creationTime
	# info =	{ directory			= toDWORD win32FindData bitand FILE_ATTRIBUTE_DIRECTORY > 0
				, creationTime		= creationTime
				, lastModifiedTime	= lastModifiedTime
				, lastAccessedTime	= lastAccessedTime
				, sizeHigh			= 0
				, sizeLow			= size (win32FindData % (WIN32_FIND_DATA_ftCreationTime_bytes_offset, WIN32_FIND_DATA_ftCreationTime_bytes_offset + FILETIME_size_bytes))
				}
	= (Ok info, world)
where
	toFileTimeArray :: !{#Char} !Int -> {#Int}
	toFileTimeArray a o = IF_INT_64_OR_32 {unpackInt8 a o} {unpackInt4S a o,unpackInt4S a (o+4)}

	toDWORD :: !{#Char} -> DWORD
	toDWORD s = toInt s.[3] << 24 bitor toInt s.[2] << 16 bitor toInt s.[1] << 8 bitor toInt s.[0] //little-endian

moveFile :: !String !String !*World -> (!MaybeOSError (), !*World)
moveFile oldpath newpath world
	# (ok,world)	= moveFileA (packString oldpath) (packString newpath) world
	| ok
		= (Ok (), world)
	| otherwise
		= getLastOSError world
