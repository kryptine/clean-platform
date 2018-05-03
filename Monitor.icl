module Monitor

import iTasks
import iTasksTTY
import TTY
import Data.Either
import StdTuple
from Data.Func import $

Start w = startEngine manage w

manage = parallel
	[(Embedded, \stl->tune (Title "New device") $ forever $
		enterInformation "TTY Settings" []
		>>! \ts->appendTask Embedded (\_->tune (Title ts.devicePath) $ monitor ts @! ()) stl @! ())
	]
	[]
	<<@ ArrangeWithTabs True
	>>* [OnAction (Action "Shutdown") (always (shutDown 0))]

monitor ts = catchAll (
		withShared ([], [], False) \channels->
			syncSerialChannel ts id (\s->(Right [s], "")) channels
		||- viewSharedInformation "Incoming messages" [ViewAs (take 20 o fst3)] channels
		||- forever (
			enterInformation "Send line of text" []
			>>= \line->upd (\(r,w,s)->(r,w++[line+++"\n"],s)) channels
		) @? const NoValue
	) (\e->viewInformation "Exception occured" [] e)
	>>* [OnAction (Action "Close") (always (treturn ""))]
