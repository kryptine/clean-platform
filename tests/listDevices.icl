module listDevices

import TTY
import Text
import Data.Tuple

Start w = appFst (join "\n") (getTTYDevices w)
