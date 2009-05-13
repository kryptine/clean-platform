module IPLookup

import StdEnv
import IP, Maybe, CommandLine

Start :: *World -> (String, *World)
Start world
	# (cmd,world) = getCommandLine world
	| length cmd <> 2 = ("Usage: " +++ cmd !! 0 +++ " hostname",world)
	= case lookupIPAddress (cmd !! 1) world of
		(Just addr, world)	= (toString addr, world)
		(Nothing, world)	= ("Could not lookup IP address", world)
