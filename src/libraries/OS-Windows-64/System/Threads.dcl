definition module Threads

:: ThreadId :== Int

fork :: !(*World -> *World) !*World -> (!ThreadId, !*World)
waitForThread :: !ThreadId !*World -> *World

from _WinDef import :: LPVOID, :: DWORD

threadFunc :: !LPVOID -> DWORD