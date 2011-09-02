definition module _SharedDataSourceOsDependent

import _SharedDataSourceTypes

:: WAITER :== Int

waitOsDependent :: !(SharedOps r w *st) !*st -> *st