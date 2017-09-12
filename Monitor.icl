module Monitor

import iTasks
import iTasksTTY
import StdTuple

Start w = startEngine monitor w

monitor :: Task ()
monitor = enterTTYSettings <<@ ApplyLayout frameCompact
	>>! \ts->withShared ([], [], False) \channels->
			syncSerialChannel ts id id channels
		||- viewSharedInformation "Incoming messages" [ViewAs (take 20 o fst3)] channels
		||- forever (
			enterInformation "Send line of text" []
			>>= \line->upd (\(r,w,s)->(r,w++[line+++"\n"],s)) channels
		)
	@! ()
