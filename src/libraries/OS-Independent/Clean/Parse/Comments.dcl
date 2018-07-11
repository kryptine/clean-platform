definition module Clean.Parse.Comments

/**
 * This module can combine the AST of the Clean compiler (which can be parsed
 * using {{`Clean.Parse`}} with comments scanned by {{`Clean.ScanComments`}}.
 */

from StdFile import class FileSystem

from Data.Error import :: MaybeError
from Data.Maybe import :: Maybe
from System.File import :: FileError
from System.FilePath import :: FilePath

from syntax import :: Ident, :: Module, :: ParsedDefinition, :: ParsedModule

:: CleanComment =
	{ line      :: !Int
	, column    :: !Int
	, level     :: !Maybe Int
	, content   :: !String
	, multiline :: !Bool
	}

scanComments :: !FilePath !*env -> *(!MaybeError FileError [CleanComment], !*env) | FileSystem env
scanCommentsFile :: !*File -> *(!MaybeError FileError [CleanComment], !*File)

:: CollectedComments

emptyCollectedComments :: CollectedComments
getComment :: !Ident !CollectedComments -> Maybe String
collectComments :: ![CleanComment] !ParsedModule -> CollectedComments
