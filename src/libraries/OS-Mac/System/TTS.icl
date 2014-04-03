implementation module System.TTS

import StdTuple, StdOverloaded
import Data.Maybe
import System.Process

tts :: String *World -> *World
tts str world = say [str] world

ttsWithVoice :: Voice String *World -> *World
ttsWithVoice voice str world = say ["-v", toString voice, str] world

say :: [String] *World -> *World
say args world = snd (runProcess "/usr/bin/say" args Nothing world)

instance toString Voice where
  toString Agnes      =  "Agnes"
  toString Albert     =  "Albert"
  toString Alex       =  "Alex"
  toString BadNews    =  "Bad News"
  toString Bahh       =  "Bahh"
  toString Bells      =  "Bells"
  toString Boing      =  "Boing"
  toString Bruce      =  "Bruce"
  toString Bubbles    =  "Bubbles"
  toString Cellos     =  "Cellos"
  toString Deranged   =  "Deranged"
  toString Fred       =  "Fred"
  toString GoodNews   =  "Good News"
  toString Hysterical =  "Hysterical"
  toString Junior     =  "Junior"
  toString Kathy      =  "Kathy"
  toString PipeOrgan  =  "Pipe Organ"
  toString Princess   =  "Princess"
  toString Ralph      =  "Ralph"
  toString Trinoids   =  "Trinoids"
  toString Vicki      =  "Vicki"
  toString Victoria   =  "Victoria"
  toString Whisper    =  "Whisper"
  toString Zarvox     =  "Zarvox"
