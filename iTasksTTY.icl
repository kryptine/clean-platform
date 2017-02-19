implementation module iTasksTTY

import StdList
import StdFunc
import StdMisc
import StdString

import System.Directory
import Data.Error
from Text import class Text(startsWith), instance Text String

getTTYDevices :: !*env -> *(![String], !*env)
getTTYDevices w = case readDirectory "/dev" w of
	(Error (errcode, errmsg), w) = abort errmsg
	(Ok entries, w) = (map ((+++) "/dev/") (filter isTTY entries), w)
	where
		isTTY s = not (isEmpty (filter (flip startsWith s) prefixes))
		prefixes = ["ttyS", "ttyACM", "ttyUSB", "tty.usbserial"]
