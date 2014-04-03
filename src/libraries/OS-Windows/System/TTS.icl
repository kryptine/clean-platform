implementation module System.TTS

import Data.Maybe
import System.Process

tts :: String *World -> *World
tts str world = say [str] world

say :: [String] *World -> *World
say str world
  # (_, world) = echo "' > \"" +++ tmpFileNm +++ "\""
  # (_, world) = echo "set speech = Wscript.CreateObject(\"SAPI.spVoice\") >> \"" +++ tmpFileNm +++ "\""
  # (_, world) = echo "speech.speak \"" +++ str +++ "\" >> \"" +++ tmpFileNm +++ "\""
  # (_, world) = start "tmpFileNm"
  # (_, world) = pause
  # (_, world) = del tmpFileNm
  = snd (runProcess "/usr/bin/say" args Nothing world)
  where
    tmpFileNm       = "temp%num%.vbs"
    echo str world  = runProcess "echo" [str] Nothing world
    start str world = runProcess "start" [str] Nothing world
    pause world     = runProcess "pause" [] Nothing world
    del file world  = runProcess "del" [file] Nothing world
