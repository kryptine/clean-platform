implementation module _SharedDataSourceOsDependent

import SharedDataSource, _SharedDataSourceTypes, _WinBase, _Unsafe, Func
		
waitOsDependent :: ![OBSERVER *st -> *st] !(*st -> *st) !*st -> *st
waitOsDependent addObserverFs close st = accUnsafe (waitOsDependent` st)
where
	waitOsDependent` st world
		# (ev, world)	= createEventA NULL True False NULL world
		# st			= seqSt (\addObserver st -> addObserver ev st) addObserverFs st
		#! st			= close st
		# (r, world)	= waitForSingleObject ev INFINITE world
		//# (_, world)	= closeHandle ev world
		= (st, world)