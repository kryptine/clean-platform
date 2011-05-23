implementation module File

//StdEnv
import StdArray
import StdBool
import StdFile
import StdList
import StdMisc
import StdString

import Error
import Void
import OSError
import _Pointer
import Time
import _Windows

CHUNK_SIZE :== 1024

from _Windows import 
	::HANDLE, 
	:: LPWIN32_FIND_DATA, 
	WIN32_FIND_DATA_size_bytes, 
	INVALID_HANDLE_VALUE,
	deleteFileA, 
	findClose,
	findFirstFileA
	
instance toString FileError
where
	toString CannotOpen = "Cannot open"
	toString CannotClose = "Cannot close"
	toString IOError = "I/O error"

readFile :: !String *env -> (MaybeError FileError String, *env) | FileSystem env
readFile filename env = withFile filename FReadData readAll env

readAll :: *File -> (MaybeError FileError String, *File)
readAll file
# (result, file) = readAcc file []
= case result of
	Error e	   = (Error e, file)
	Ok contents = (Ok ((foldr (+++) "" (reverse contents))), file)
where
	readAcc :: *File [String] -> (MaybeError FileError [String], *File)
	readAcc file acc
		# (str,file)	= freads file CHUNK_SIZE
		# (err,file)	= ferror file
		| err			= (Error IOError,file)
		# (eof,file)	= fend file
		| eof			= (Ok [str:acc],file)
		| otherwise		= readAcc file [str:acc]			

writeFile :: !String !String *env -> (MaybeError FileError Void, *env) | FileSystem env
writeFile filename contents env = 
	withFile filename FWriteData (\file -> (Ok Void, fwrites contents file)) env

withFile :: !String Int (*File -> (MaybeError FileError a,*File)) *env 
			-> (MaybeError FileError a, *env) | FileSystem env
withFile filename filemode operation env
# (ok,file,env)	= fopen filename filemode env
| not ok			= (Error CannotOpen, env)
# (result,file)		= operation file
| isError result 	= (result, env)
# (ok,env)	 		= fclose file env
| not ok			= (Error CannotClose, env)
= (Ok (fromOk result), env)

fileExists ::  !String *World -> (Bool, *World)
fileExists filename world
# win32FindData = createArray WIN32_FIND_DATA_size_bytes '\0'
# (handle, world) = findFirstFileA (packString filename) win32FindData world
| handle == INVALID_HANDLE_VALUE = (False, world)
# (_,world) = findClose handle world
= (True, world)

deleteFile :: !String *World -> (MaybeOSError Void, *World)
deleteFile filename world 
	# (ok, world) = deleteFileA (packString filename) world
	| ok == 0 = getLastOSError world
	= (Ok Void, world)

getFileInfo :: !String *World -> (MaybeOSError FileInfo, *World)
getFileInfo filename world
	# win32FindData = createArray WIN32_FIND_DATA_size_bytes '\0'
	# (handle, world) = findFirstFileA (packString filename) win32FindData world
	| handle == INVALID_HANDLE_VALUE = getLastOSError world
	# (res, world) = filetimeToTm (win32FindData % (WIN32_FIND_DATA_ftCreationTime_bytes_offset, WIN32_FIND_DATA_ftCreationTime_bytes_offset + FILETIME_size_bytes)) world
	| isError res = (liftError res, world)
	# creationTime = fromOk res
	# (res, world) = filetimeToTm (win32FindData % (WIN32_FIND_DATA_ftLastWriteTime_bytes_offset, WIN32_FIND_DATA_ftLastWriteTime_bytes_offset + FILETIME_size_bytes)) world
	| isError res = (liftError res, world)
	# lastModifiedTime = fromOk res
	# (res, world) = filetimeToTm (win32FindData % (WIN32_FIND_DATA_ftLastAccessTime_bytes_offset, WIN32_FIND_DATA_ftLastAccessTime_bytes_offset + FILETIME_size_bytes)) world
	| isError res = (liftError res, world)
	# lastAccessedTime = fromOk res
	# info = creationTime
	# info =	{ directory			= WIN32_FIND_DATA_dwFileAttributes_bytes_offset bitand FILE_ATTRIBUTE_DIRECTORY > 0
				, creationTime		= creationTime
				, lastModifiedTime	= lastModifiedTime
				, lastAccessedTime	= lastAccessedTime
				, sizeHigh			= 0
				, sizeLow			= size (win32FindData % (WIN32_FIND_DATA_ftCreationTime_bytes_offset, WIN32_FIND_DATA_ftCreationTime_bytes_offset + FILETIME_size_bytes))
				}
	= (Ok info, world)

filetimeToTm :: !FILETIME *World -> (MaybeOSError Tm, *World)
filetimeToTm filetime world
	# systemtime = createArray SYSTEMTIME_size_bytes '\0'
	# (ok, world) = fileTimeToSystemTime filetime systemtime world
	| ok <> ok = undef
	| not ok = getLastOSError world
	# tm=	{ sec	= toInt systemtime.[SYSTEMTIME_wSecond_offset]		+ (toInt systemtime.[SYSTEMTIME_wSecond_offset + 1]		<< 8)
			, min	= toInt systemtime.[SYSTEMTIME_wMinute_offset]		+ (toInt systemtime.[SYSTEMTIME_wMinute_offset + 1]		<< 8)
			, hour	= toInt systemtime.[SYSTEMTIME_wHour_offset]		+ (toInt systemtime.[SYSTEMTIME_wHour_offset + 1]		<< 8)
			, mday	= toInt systemtime.[SYSTEMTIME_wDay_offset]			+ (toInt systemtime.[SYSTEMTIME_wDay_offset + 1]		<< 8)
			, mon	= toInt systemtime.[SYSTEMTIME_wMonth_offset]		+ (toInt systemtime.[SYSTEMTIME_wMonth_offset + 1]		<< 8) - 1
			, year	= toInt systemtime.[SYSTEMTIME_wYear_offset]		+ (toInt systemtime.[SYSTEMTIME_wYear_offset + 1]		<< 8) - 1900
			, wday	= toInt systemtime.[SYSTEMTIME_wDayOfWeek_offset]	+ (toInt systemtime.[SYSTEMTIME_wDayOfWeek_offset + 1]	<< 8)
			, yday	= -1	//Not implemented
			, isdst	= False //Not implemented
			}
	= (Ok tm, world)

moveFile :: !String !String !*World -> (!MaybeOSError Void, !*World)
moveFile oldpath newpath world
	# (ok,world)	= moveFileA (packString oldpath) (packString newpath) world
	| ok
		= (Ok Void, world)
	| otherwise
		= getLastOSError world
	
