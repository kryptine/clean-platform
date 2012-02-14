implementation module _SharedDataSourceOsDependent

import SharedDataSource, _SharedDataSourceTypes, StdMisc
		
waitOsDependent :: ![OBSERVER *st -> *st] !(*st -> *st) !*st -> *st
waitOsDependent addObserverFs close st = abort "waitOsDependent POSIX: not implemented"