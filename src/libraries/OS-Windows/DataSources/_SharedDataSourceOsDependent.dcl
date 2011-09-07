definition module _SharedDataSourceOsDependent

import _SharedDataSourceTypes

:: OBSERVER :== Int

waitOsDependent :: !(SharedOps r w *st) !*st -> *st