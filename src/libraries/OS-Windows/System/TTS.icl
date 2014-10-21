implementation module System.TTS

import StdOverloaded, StdString, StdList
import Data.Maybe
import System.Process

tts :: String *World -> *World
tts str world
  #! (_, world) = echo ("Wscript.CreateObject(\"SAPI.spVoice\").speak \"" +++ str +++ "\" > \"" +++ tmpFileNm +++ "\"") world
  #! (_, world) = start tmpFileNm world
  #! (_, world) = pause world
  #! (_, world) = del tmpFileNm world
  = world
  where
    tmpFileNm       = "temp.vbs"
	exec cmd world  = runProcess "C:\\Windows\\System32\\cmd.exe" ["/c " +++ cmd] Nothing world
    echo str world  = exec ("echo " +++ str) world
    start str world = exec ("start " +++ str) world
    pause world     = exec "pause" world
    del file world  = exec ("del " +++ file) world