definition module System.Directory

from Data.Void import :: Void
from System.FilePath import :: FilePath
from Data.Error import :: MaybeError
from System.OSError import :: MaybeOSError, :: OSError, :: OSErrorMessage, :: OSErrorCode
from StdFile import class FileSystem

createDirectory :: !FilePath !*w -> (!MaybeOSError Void, !*w) | FileSystem w

removeDirectory :: !FilePath !*w -> (!MaybeOSError Void, !*w) | FileSystem w

readDirectory :: !FilePath !*w -> (!MaybeOSError [FilePath], !*w) | FileSystem w

getCurrentDirectory :: !*w -> (!MaybeOSError FilePath, !*w) | FileSystem w

setCurrentDirectory :: !FilePath !*w -> (!MaybeOSError Void, !*w) | FileSystem w
