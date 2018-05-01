implementation module Platform

import Data.Error
import StdMisc, StdOverloaded, StdString
import System.Directory

getDevices :: !*World -> *([String], !*World)
getDevices w = case readDirectory "/dev" w of
	(Error (errcode, errmsg), w) = abort errmsg
	(Ok entries, w) = (entries, w)
