implementation module System.FilePath.Find

//! This module is lightly based on Haskell's System.FilePath.Find by Bryan O'Sullivan <bos@serpentine.com>.

import Data.Eq
import Data.Func
import Data.Functor
import Data.Foldable
import Data.Traversable
import Data.Error
import Data.String
from Data.List import map, filter, partition, ++, instance Functor []

from Control.Applicative import class Applicative 
from Control.Monad import class Monad

import System.FilePath
import System.File
import System.Directory
import System.Time
import System.OSError

//FIXME should be named FileInfo
:: FileInformation =
  { path :: FilePath
  , status :: FileInfo
  }

:: FindPredicate :== FileInformation -> Bool

//FIXME crashes on symlinks
find :: !FindPredicate !FilePath !*World -> (!MaybeOSError [FilePath], !*World)
find predicate path world
    // paths <- getDirectoryContents path
	# (pathsResult,world) = readDirectory path world
    | isError pathsResult = (pathsResult, world)
    # paths = fromOk pathsResult

    # paths = map ((</>) path) o filter (\p -> p % (0,0) /= ".") $ paths

    // infos <- traverse getFileStatus paths
    # (infoResults,world) = mapSt getFileInformation paths world
    # infosResult = sequence infoResults
    | isError infosResult = (convert infosResult, world)
    # infos = fromOk infosResult

    # (dirInfos,fileInfos) = partition isDirectory infos
    # matchedFileInfos = filter predicate fileInfos

    // matched <- concat <$> traverse (find predicate . getPath) dirs
    # (pathResults,world) = mapSt (find predicate o getPath) dirInfos world
    # pathsResult = concat <$> sequence pathResults
    | isError pathsResult = (pathsResult, world)
    # matchedPaths = fromOk pathsResult

    # paths = map getPath matchedFileInfos ++ matchedPaths
    = (Ok paths, world)

    where
      convert :: (MaybeOSError [FileInformation]) -> MaybeOSError [FilePath]
      convert (Error e) = Error e
      convert (Ok infos) = Ok (map getPath infos)

getFileInformation :: !FilePath !*World -> *(!MaybeOSError FileInformation, !*World)
getFileInformation path world
  # (result,world) = getFileInfo path world
  = case result of
    Error err = (Error err, world)
    Ok status = (Ok {path = path, status = status}, world)

isDirectory :: FileInformation -> Bool
isDirectory info = info.status.directory

getPath :: FileInformation -> FilePath
getPath info = info.path

getStatus :: FileInformation -> FileInfo
getStatus info = info.status

