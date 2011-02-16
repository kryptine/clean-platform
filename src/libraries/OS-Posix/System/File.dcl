definition module File

from StdFile import class FileSystem
from StdClass import class toString

import Error
import Void
import OSError

:: FileError = CannotOpen | CannotClose | IOError

instance toString FileError

/**
* Given a filename, reads the contents of the file to a String
* @param Path to the file to read
* @return contents of the file
*/
readFile :: !String *env -> (MaybeError FileError Void, *env) | FileSystem env

/**
* Read all contents of a *File to a String. 
* @precondition The file must be opened in read mode
* @param Path to the file to read
* @return contents of the file
*/
readAll :: *File -> (MaybeError FileError String, *File)

/**
* writes a string to a file
* @param Path to the file to read
* @param contents of the file
*/
writeFile :: !String !String *env -> (MaybeError FileError Void, *env) | FileSystem env

/**
* Performs a file operation on a given filename.
* The file is opened and closed by the withFile function.
* @param Path to the file 
* @param file operation function
* @return file operation result
*/
withFile :: !String Int (*File -> (MaybeError FileError a,*File)) *env 
			-> (MaybeError FileError a, *env) | FileSystem env

/**
* checks if a file exists
* @param Path to the file 
* @return file exists
*/
fileExists ::  !String *World -> (Bool, *World)

/**
* deletes a file from disk
* @param Path to the file 
* @return delete succeeded
*/
deleteFile :: !String *World -> (MaybeOSError Void, *World)

