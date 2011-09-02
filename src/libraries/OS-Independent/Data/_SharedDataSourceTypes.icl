implementation module _SharedDataSourceTypes

from SharedDataSource				import :: BasicSourceOps{..}, :: SharedVer
from _SharedDataSourceOsDependent	import :: WAITER

close :: !(SharedOps r w *st) !*st -> *st
close (BasicSourceOps _ _ {BasicSourceOps|unlock, close}) st
	= close (unlock st)
close (ComposedSourceOps {opsX, opsY}) st
	# st	= close opsX st
	# st	= close opsY st
	= st