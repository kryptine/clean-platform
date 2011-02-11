implementation module CommandLine

import StdInt, StdList, StdEnum
import Pointer

import StdEnv

getCommandLine :: *World -> ([String],*World)
getCommandLine world 
	# argc = derefInt global_argc
	# argv = derefInt global_argv
	# arg1 = derefInt argv
	= ([toString argc,toString argv,toString arg1],world)
/*
	= ([derefString (readInt argv (i << (IF_INT_64_OR_32 3 2)) ) \\ i <- [0..argc - 1]], world)
*/
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
