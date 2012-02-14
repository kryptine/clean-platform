definition module _SharedDataSourceOsDependent

import _SharedDataSourceTypes

:: OBSERVER :== Int

waitOsDependent :: ![OBSERVER *st -> *st] !(*st -> *st) !*st -> *st