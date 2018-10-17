module Monitor

import StdEnv

import Data.Either
import Data.Func
import System.Time

import iTasks

import TTY, iTasksTTY


Start w = startEngine manage w

manage = parallel
	[(Embedded, \stl->tune (Title "New device") $ forever $
		accWorld getTTYDevices
		>>= \ds->enterChoice "Choose path" [] ["Other":ds]
		>>= \path->updateInformation "TTY Settings" [] {zero & devicePath=path}
		>>! \ts->appendTask Embedded (\_->tune (Title ts.devicePath) $ monitor ts @! ()) stl @! ())
	]
	[]
	<<@ ArrangeWithTabs True
	>>* [OnAction (Action "Shutdown") (always (shutDown 0))]

monitor ts = catchAll (
		withShared ([], [], False) \channels->
			syncSerialChannel {tv_sec=0,tv_nsec=100*1000000} ts id (\s->(Right [s], "")) channels
		||- viewSharedInformation "Incoming messages" [ViewAs (take 20 o fst3)] channels
		||- forever (
			enterInformation "Send line of text" []
			>>= \line->upd (\(r,w,s)->(r,w++[line+++"\n"],s)) channels
		) @? const NoValue
	) (\e->viewInformation "Exception occured" [] e)
	>>* [OnAction (Action "Close") (always (treturn ""))]
