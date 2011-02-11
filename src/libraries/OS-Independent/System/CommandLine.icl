implementation module CommandLine

import StdInt, StdList, StdEnum
import _Pointer

getCommandLine :: *World -> ([String],*World)
getCommandLine world 
	# argc = readInt4Z global_argc 0
	# argv = derefInt global_argv
	= ([derefString (readInt argv (i << (IF_INT_64_OR_32 3 2)) ) \\ i <- [0..argc - 1]], world)
where
	//Global argc pointer
	global_argc :: Pointer
	global_argc = code {
		pushLc global_argc
	}

	//Global argv pointer
	global_argv :: Pointer
	global_argv = code {
		pushLc global_argv
	}
