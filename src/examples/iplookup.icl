module iplookup

import IP, Maybe

Start :: *World -> (String, *World)
Start world = case lookupIPAddress "clean.cs.ru.nl" world of
	(Just addr, world)	= (toString addr, world)
	(Nothing, world)	= ("Could not lookup IP address", world)