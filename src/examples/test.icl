module test

import StdEnv

import TTY

TTYerrorclose :: !*World -> *World
TTYerrorclose w
# (err, w) = TTYerror w
= cwrite err w

cwrite :: String !*World -> *World
cwrite s w
# (io, w) = stdio w
= snd (fclose (io <<< s <<< "\n") w)

Start :: *World -> *World
Start w
# w = cwrite "open" w
# (ok, tty, w) = TTYopen {zero & sleepTime=2, devicePath="COM3"} w
| not ok = TTYerrorclose w
#! (l, tty) = TTYreadline tty
= w
/*
# io = io <<< "close\n"
# (ok, w) = TTYclose tty w
# io = io <<< "ok: " <<< toString ok <<< "\n"
| not ok = TTYerrorclose io w
= snd (fclose io w)
*/
/*
#! tty = TTYwrite "echo123\n" tty
#! (av, e, tty) = TTYavailable tty
# io = io <<< ("Bytes available: " +++ toString av +++ "\n")
#! (l, tty) = TTYreadline tty
# io = io <<< ("Line read: " +++ l)
#! (ok, w) = TTYclose tty w
| not ok = TTYerrorclose io w
= snd (fclose io w)*/

