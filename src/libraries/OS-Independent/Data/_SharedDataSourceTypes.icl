implementation module _SharedDataSourceTypes

import Maybe
from SharedDataSource				import :: BasicSourceOps{..}, :: Version
from _SharedDataSourceOsDependent	import :: OBSERVER

close :: !(SharedOps r w *st) !*st -> *st
close (BasicSourceOps _ _ {BasicSourceOps|unlock, close}) st
	= close (unlock st)
close (ComposedSourceOps {opsX, opsY}) st
	# st	= close opsX st
	# st	= close opsY st
	= st