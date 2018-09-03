implementation module System.Directory

import StdBool
import StdClass
import StdString

import Data.Error
from Data.Func import $
import System._Directory
import System.File
import System.FilePath

scanDirectory :: !(FilePath FileInfo .st *World -> *(.st, *World)) !.st !FilePath !*World -> *(![OSError], !.st, !*World)
scanDirectory upd st dir w = scan dir [] st w
where
	scan dir errs st w
	# (fi,w) = getFileInfo dir w
	# (errs,st,w) = case fi of
		Error e -> ([e:errs], st, w)
		Ok fi   -> (\(st,w) -> (errs,st,w)) $ upd dir fi st w
	| isError fi = (errs, st, w)
	# fi = fromOk fi
	| not fi.directory = (errs, st, w)
	# (contents,w) = readDirectory dir w
	| isError contents = ([fromError contents:errs], st, w)
	# contents = [dir </> fp \\ fp <- fromOk contents | fp <> "." && fp <> ".."]
	= seqSt3 scan contents errs st w

	seqSt3 f [] s1 s2 s3 = (s1, s2, s3)
	seqSt3 f [x:xs] s1 s2 s3
	# (s1,s2,s3) = f x s1 s2 s3
	= seqSt3 f xs s1 s2 s3
