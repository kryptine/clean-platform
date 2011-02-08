implementation module File

//StdEnv
import StdArray
import StdFile
import StdList
import StdString

import Error
import Void
import OSError
import Pointer

CHUNK_SIZE :== 1024

from _Windows import 
	::HANDLE, 
	:: LPWIN32_FIND_DATA, 
	WIN32_FIND_DATA_size_int, 
	findFirstFileA, 
	INVALID_HANDLE_VALUE,
	findClose
	
from _Windows import qualified deleteFile

instance toString FileError
where
	toString CannotOpen = "Cannot open"
	toString CannotClose = "Cannot close"
	toString IOError = "I/O error"

readFile :: !String *env -> (MaybeError String FileError, *env) | FileSystem env
readFile filename env = withFile filename FReadData readAll env

readAll :: *File -> (MaybeError String FileError, *File)
readAll file
# (result, file) = readAcc file []
= case result of
	Error e	   = (Error e, file)
	Ok contents = (Ok ((foldr (+++) "" (reverse contents))), file)
where
	readAcc :: *File [String] -> (MaybeError [String] FileError, *File)
	readAcc file acc
		# (str,file)	= freads file CHUNK_SIZE
		# (err,file)	= ferror file
		| err			= (Error IOError,file)
		# (eof,file)	= fend file
		| eof			= (Ok [str:acc],file)
		| otherwise		= readAcc file [str:acc]			

writeFile :: !String !String *env -> (MaybeError Void FileError, *env) | FileSystem env
writeFile filename contents env = 
	withFile filename FWriteData (\file -> (Ok Void, fwrites contents file)) env

withFile :: !String Int (*File -> (MaybeError a FileError,*File)) *env 
			-> (MaybeError a FileError, *env) | FileSystem env
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
# win32FindData = createArray WIN32_FIND_DATA_size_int 0
# (handle, world) = findFirstFileA (packString filename) win32FindData world
| handle == INVALID_HANDLE_VALUE = (False, world)
# (_,world) = findClose handle world
= (True, world)

deleteFile :: !String *World -> (MaybeOSError Void, *World)
deleteFile path world 
	# (ok, world) = '_Windows'.deleteFile path world
	| ok == 0 = getLastOSError world
	= (Ok Void, world)
