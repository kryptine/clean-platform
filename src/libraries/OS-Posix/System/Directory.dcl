definition module System.Directory

from Data.Void import :: Void
from System.FilePath import :: FilePath
from Data.Error import :: MaybeError
from System.OSError import :: MaybeOSError, :: OSError, :: OSErrorMessage, :: OSErrorCode

createDirectory :: !FilePath !*w -> (!MaybeOSError Void, !*w)

removeDirectory :: !FilePath !*w -> (!MaybeOSError Void, !*w)

readDirectory :: !FilePath !*w -> (!MaybeOSError [FilePath], !*w)

getCurrentDirectory :: !*w -> (!MaybeOSError FilePath, !*w)

setCurrentDirectory :: !FilePath !*w -> (!MaybeOSError Void, !*w)
