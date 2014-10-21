definition module System.TTS

from StdOverloaded import class toString

:: Voice
  = Agnes
  | Albert
  | Alex
  | BadNews
  | Bahh
  | Bells
  | Boing
  | Bruce
  | Bubbles
  | Cellos
  | Deranged
  | Fred
  | GoodNews
  | Hysterical
  | Junior
  | Kathy
  | PipeOrgan
  | Princess
  | Ralph
  | Trinoids
  | Vicki
  | Victoria
  | Whisper
  | Zarvox

instance toString Voice

tts :: String *World -> *World

ttsToFile :: String String *World -> *World

ttsWithVoice :: Voice String *World -> *World

ttsWithVoiceToFile :: Voice String String *World -> *World
