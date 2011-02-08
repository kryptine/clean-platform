definition module FilePath
/**
	Module for manipulation of file and directory paths
*/

:: FilePath :== String

pathSeparator :: Char

pathSeparators :: [Char]

extSeparator :: Char

(</>) infixr 5 :: !FilePath !FilePath -> FilePath

splitExtension :: !FilePath -> (String, String)

takeExtension :: !FilePath -> String

dropExtension :: !FilePath -> String

addExtension :: !FilePath !String -> FilePath

replaceExtension :: !FilePath !String -> FilePath

takeDirectory :: !FilePath -> FilePath