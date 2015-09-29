definition module System.FilePath.Find

from Data.Error import ::MaybeError

from System.File import ::FileInfo
from System.FilePath import ::FilePath
from System.OSError import ::MaybeOSError, ::OSError, ::OSErrorMessage, ::OSErrorCode

:: FileInformation =
  { path :: FilePath
  , status :: FileInfo
  }

:: FindPredicate :== FileInformation -> Bool

find ::
	!FindPredicate                          // Predicate to match files
	!FilePath                               // Starting directory
	!*World                                 // The World
	-> (!MaybeOSError [FilePath], !*World)  // List of matched files and the new World

