definition module OS

OS_NAME :== "Windows (32-bit)"
OS_PATH_SEPARATOR :== '\\'
OS_NEWLINE :== "\r\n"

IF_POSIX_OR_WINDOWS posix windows   :== windows

IF_WINDOWS win other				:== win
IF_WINDOWS32 win other				:== win
IF_WINDOWS64 win other				:== other
IF_POSIX posix other				:== other
IF_LINUX linux other				:== other
IF_LINUX32 linux other				:== other
IF_LINUX64 linux other				:== other
IF_MAC mac other					:== other
