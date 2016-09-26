module test

import StdEnv

import TTY

Start :: *World -> (!String, *World)
Start w
#! (ok, tty, w) = TTYopen "/dev/ttyUSB0" zero w
| not ok = TTYerror w
#! tty = TTYwrite tty "echo123\n"
#! (c, tty) = TTYreadline tty
#! (ok, w) = TTYclose tty w
| not ok = TTYerror w
#! (s, w) = TTYerror w
= ("Read: " +++ c, w)
