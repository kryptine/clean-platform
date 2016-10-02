definition module System.File.Experimental

from Data.String import class toString
from Data.Result import :: Result, :: Usually, :: ErrorWitness
from System.FilePath import :: FilePath

from StdFile import class FileSystem(stdio), stderr

/// # Files

// :: File
// BUILTIN

/// # IOModes

:: IOMode (:== Int)

ReadMode :: IOMode
WriteMode :: IOMode
AppendMode :: IOMode

:: SeekMode (:== Int)

AbsoluteSeek :: SeekMode
RelativeSeek :: SeekMode
SeekFromEnd :: SeekMode

:: FileError
	= FileDoesNotExist
	| FileAlreadyExists
	| PermissionDenied
	| FileIsAtEnd
	| FileIsFull
	| IllegalFileOperation
	| UserFileError String

instance toString FileError

/// # Opening and Closing

withFile :: !FilePath !IOMode (*File -> *(Usually a, *File)) !*World -> *(Usually a, !*World)
openFile :: !FilePath !IOMode !*World -> *(Usually *File, *World)
openBinaryFile :: !FilePath !IOMode !*World -> *(Usually *File, *World)
closeFile :: !*File !*World -> *(Usually (), *World)

/*
/// ## Reading and Writing

readFile :: !FilePath !*World -> *(Usually String, *World)
writeFile :: !FilePath !String !*World -> *(Usually (), *World)
appendFile :: !FilePath !String !*World -> *(Usually (), *World)
*/
