module ProcessDemo

import StdEnv

import Environment
import FilePath
import File
import Process

Start world
# (mTemp, world) = getEnvironmentVariable "TEMP" world
| isNothing mTemp = ("TEMP environment variable not found", world)
# textfile = fromJust mTemp </> "hello.txt"
# (res, world) = writeFile textfile "Hello, World!" world
| isError res = ("Failed to write file: " +++ toString (fromError res), world)

# (mWindir, world) = getEnvironmentVariable "windir" world
| isNothing mWindir = ("windir environment variable not found", world)
# editor = fromJust mWindir </> "notepad.exe"
# (res, world) = callProcess editor [textfile] Nothing world
| isError res = ("Failed to run process: " +++ snd (fromError res), world)

# (res, world) = readFile textfile world
| isError res = ("Failed to read file: " +++ toString (fromError res), world)
# contents = fromOk res

# (res, world) = deleteFile textfile world
| isError res = ("Failed to delete file: " +++ snd (fromError res), world)
= (contents, world)

