implementation module _SharedDataSourceOsDependent

import SharedDataSource, _SharedDataSourceTypes, _WinBase, _Unsafe

waitOsDependent :: !(SharedOps r w *st) !*st -> *st
waitOsDependent ops st = accUnsafe (waitOsDependent` ops st)
where
	waitOsDependent` :: !(SharedOps r w *st) !*st !*World -> (!*st, !*World)
	waitOsDependent` ops st world
		# (ev, world)		= createEventA NULL True False NULL world
		# addWaiter = case ops of
			BasicSourceOps _ _ {BasicSourceOps|addWaiter}	= addWaiter
			ComposedSourceOps {ComposedSourceOps|addWaiter}	= addWaiter
		# st			= addWaiter ev st
		#! st			= close ops st
		# (r, world)	= waitForSingleObject ev INFINITE world
		//# (_, world)	= closeHandle ev world
		= (st, world)