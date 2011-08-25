definition module Threads

:: ThreadId

fork :: !(*World -> *World) !*World -> (!ThreadId, !*World)
waitForThread :: !ThreadId !*World -> *World

from _WinDef import :: LPVOID, :: DWORD

threadFunc :: !LPVOID -> DWORD