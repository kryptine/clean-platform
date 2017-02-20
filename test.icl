module test

import StdEnv

import TTY

TTYerrorclose :: !*File !*World -> *World
TTYerrorclose f w
# (err, w) = TTYerror w
# (ok, w) = fclose (f <<< err <<< "\n") w
| not ok = abort "Couldn't close file"
= w

Start :: *World -> *World
Start w
# (io, w) = stdio w
# (ok, tty, w) = TTYopen {zero & devicePath="/dev/ttyUSB0"} w
| not ok = TTYerrorclose io w
#! tty = TTYwrite "echo123\n" tty
#! (av, tty) = TTYavailable tty
# io = io <<< ("Bytes available: " +++ toString av +++ "\n")
#! (l, tty) = TTYreadline tty
# io = io <<< ("Line read: " +++ l)
#! (ok, w) = TTYclose tty w
| not ok = TTYerrorclose io w
= snd (fclose io w)
