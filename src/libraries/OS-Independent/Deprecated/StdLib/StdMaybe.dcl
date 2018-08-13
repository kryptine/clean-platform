definition module StdMaybe

from StdFunc import :: St
import Data.Maybe

u_isJust :== isJustU
u_isNothing :== isNothingU

accMaybe :: .(St .x .a) !u:(Maybe .x) -> (!Maybe .a,!u:Maybe .x)
