implementation module System.File.Experimental

import Data.Result
import Data.Bool

from System.FilePath import :: FilePath

from StdEnv import class FileSystem(fopen, fclose), instance FileSystem World

:: IOMode :== Int

ReadMode :: IOMode
ReadMode   = 0

WriteMode :: IOMode
WriteMode  = 1

AppendMode :: IOMode
AppendMode = 2

:: SeekMode :== Int

AbsoluteSeek :: SeekMode
AbsoluteSeek = 0

RelativeSeek :: SeekMode
RelativeSeek = 1

SeekFromEnd :: SeekMode
SeekFromEnd  = 2

/// # Opening and Closing

BINARY_MODE_OFFSET :== 3
instance + IOMode where
    (+) x y = x + y

withFile :: !FilePath !IOMode (*File -> *(Usually a, *File)) !*World -> *(Usually a, !*World)
withFile name mode operation world
    # (opened, world) = openFile name mode world
    | isErr opened
        = (rethrow opened, world)
    # file = unwrap opened

    # (result, file) = operation file
    | isErr result
        = (rethrow result, world)

    # (closed, world) = closeFile file world
    | isErr closed
        = (rethrow closed, world)

    | otherwise
        = (result, world)

openFile :: !FilePath !IOMode !*World -> *(Usually *File, *World)
openFile name mode world
    # (ok, file, world) = fopen name mode world
    | not ok
        = (throw FileDoesNotExist, world)
    | otherwise
        = (return file, world)

openBinaryFile :: !FilePath !IOMode !*World -> *(Usually *File, *World)
openBinaryFile name mode world
    = openFile name (mode + BINARY_MODE_OFFSET) world

closeFile :: !*File !*World -> *(Usually (), *World)
closeFile file world
    # (ok, world) = fclose file world
    | not ok
        = (throw FileIsFull, world)
    | otherwise
        = (return (), world)

/*
/// ## Reading and Writing

readFile :: !FilePath !*World -> *(Usually String, *World)
readFile name world = withFile name ReadMode readAll world

readFileLines :: !FilePath !*World -> (Usually [String], *World)
readFileLines name world = withFile name ReadMode readAllLines world

writeFile :: !FilePath !String !*World -> *(Usually (), *World)
writeFile name contents world = withFile name WriteMode (writeString contents) world

appendFile :: !FilePath !String !*World -> *(Usually (), *World)
appendFile name contents world = withFile name AppendMode (appendString contents) world

readAll :: !*File -> (Usually String, *File)
readAll file
    # (result, file) = readAcc file []
    = case result of
    	Err e	   = (Error e, file)
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


readAllLines

writeString

appendString
*/
