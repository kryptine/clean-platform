definition module System.Directory

from Data.Void import :: Void
from System.FilePath import :: FilePath
from Data.Error import :: MaybeError
from System.OSError import :: MaybeOSError, :: OSError, :: OSErrorMessage, :: OSErrorCode

createDirectory :: !FilePath !*World -> (!MaybeOSError Void, !*World)

removeDirectory :: !FilePath !*World -> (!MaybeOSError Void, !*World)

readDirectory :: !FilePath !*World -> (!MaybeOSError [FilePath], !*World)

getCurrentDirectory :: !*World -> (!MaybeOSError FilePath, !*World)

setCurrentDirectory :: !FilePath !*World -> (!MaybeOSError Void, !*World)
