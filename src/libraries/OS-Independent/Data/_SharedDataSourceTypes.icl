implementation module _SharedDataSourceTypes

import Maybe, Error, Void
from SharedDataSource				import :: BasicSourceOps{..}, :: Hash
from _SharedDataSourceOsDependent	import :: OBSERVER

close :: !(SharedOps r w *env) !*env -> *env
close (BasicSourceOps _ _ {BasicSourceOps|unlock, close}) st
	= close (unlock st)
close (ComposedSourceOps {opsX, opsY}) st
	# st	= close opsX st
	# st	= close opsY st
	= st