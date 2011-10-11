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
	global_argc = IF_INT_64_OR_32 global_argc64 global_argc32
	
	global_argc32 = code {
		pushLc global_argc
	}
	
	global_argc64 = code {
		pushL global_argc
	}

	//Global argv pointer
	global_argv :: Pointer
	global_argv = IF_INT_64_OR_32 global_argv64 global_argv32
	
	global_argv32 = code {
		pushLc global_argv
	}
	
	global_argv64 = code {
		pushL global_argv
	}
