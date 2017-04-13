implementation module System._WinBase

import System._WinDef
import code from library "_WinBase_library"

closeHandle :: !HANDLE !*w -> (!Bool,!*w)
closeHandle handle world
	= code {
		ccall CloseHandle@4 "PI:I:I"
	}

setHandleInformation :: !HANDLE !DWORD !DWORD !*w -> (!Bool, !*w)
setHandleInformation hObject dwMask dwFlags world = code {
	ccall SetHandleInformation@12 "PIII:I:I"
}

createDirectoryA :: !String !LPSECURITY_ATTRIBUTES !*w -> (!Bool, !*w)
createDirectoryA lpFileName lpSecurityAttributes world
	= code {
		ccall CreateDirectoryA@8 "PsI:I:I"
	}
	
createFileA :: !String !DWORD !DWORD !LPSECURITY_ATTRIBUTES !DWORD !DWORD !HANDLE !*w -> (!HANDLE, !*w)
createFileA lpFileName dwDesiredAccess dwShareMode lpSecurityAttributes 
	dwCreationDisposition dwFlagsAndAttributes hTemplateFile world
	= code {
		ccall CreateFileA@28 "PsIIIIII:I:I"
	}
	
readFile :: !HANDLE !LPVOID !DWORD !LPDWORD !LPOVERLAPPED !*w -> (!Bool, !*w)
readFile hFile lpBuffer nNumberOfBytesToRead lpNumberOfBytesRead lpOverlapped world
	= code {
		ccall ReadFile@20 "PIpIpp:I:I"
	}
	
writeFile :: !HANDLE !String !DWORD !LPDWORD !LPOVERLAPPED !*w -> (!Bool, !*w)
writeFile hFile lpBuffer nNumberOfBytesToWrite lpNumberOfBytesWritten lpOverlapped world
	= code {
		ccall WriteFile@20 "PIsIpp:I:I"
	}
	
setEndOfFile :: !HANDLE !*w -> (!Bool, !*w)
setEndOfFile hFile world
	= code {
		ccall SetEndOfFile@4 "PI:I:I"
	}

lockFileEx :: !HANDLE !DWORD !DWORD !DWORD !DWORD !LPOVERLAPPED !*w -> (!Bool, !*w)
lockFileEx hFile dwFlags dwReserved nNumberOfBytesToLockLow nNumberOfBytesToLockHigh lpOverlapped world
	= code {
		ccall LockFileEx@24 "PIIIIIp:I:I"
	}
	
unlockFile :: !HANDLE !DWORD !DWORD !DWORD !DWORD !*w -> (!Bool, !*w)
unlockFile hFile dwFileOffsetLow dwFileOffsetHigh nNumberOfBytesToUnlockLow nNumberOfBytesToUnlockHigh world
	= code {
		ccall UnlockFile@20 "PIIIII:I:I"
	}
	
getFileSize :: !HANDLE !LPDWORD !*w -> (!DWORD, !*w)
getFileSize hFile lpFileSizeHigh world
	= code {
		ccall GetFileSize@8 "PIA:I:I"
	}

createProcessA :: !String !String !LPSECURITY_ATTRIBUTES !LPSECURITY_ATTRIBUTES !Bool !Int !LPVOID
					!LPCTSTR !LPSTARTUPINFO !LPPROCESS_INFORMATION !*w -> (!Bool,!*w)
createProcessA lpApplicationName commandLine lpProcessAttributes lpThreadAttributes inheritHandles creationFlags lpEnvironment
					currentDirectory lpStartupInfo lpProcessInformation os
	= code {
		ccall CreateProcessA@40 "PssIIIIIIAA:I:I"
	}
	
createProcessA_dir :: !String !String !LPSECURITY_ATTRIBUTES !LPSECURITY_ATTRIBUTES !Bool !Int !LPVOID
					!String !LPSTARTUPINFO !LPPROCESS_INFORMATION !*w -> (!Bool,!*w)
createProcessA_dir lpApplicationName commandLine lpProcessAttributes lpThreadAttributes inheritHandles creationFlags lpEnvironment
					currentDirectory lpStartupInfo lpProcessInformation os
	= code {
		ccall CreateProcessA@40 "PssIIIIIsAA:I:I"
	}

terminateProcess :: !HANDLE !Int !*w -> (!Bool, !*w)
terminateProcess hProc exitCode world = code inline {
	ccall TerminateProcess@8 "PII:I:I"
}

deleteFileA :: !String !*w -> (!Int, !*w)
deleteFileA path world = code inline {
	ccall DeleteFileA@4 "Ps:I:I"
}

fileTimeToSystemTime :: !FILETIME !LPSYSTEMTIME !*w -> (!Bool, *w)
fileTimeToSystemTime lpFileTime lpSystemTime world
	= code {
		ccall FileTimeToSystemTime@8 "Pss:I:I"
	}

findClose :: !HANDLE !*w -> (!Bool, !*w)
findClose handle world
	= code {
		ccall FindClose@4 "PI:I:I"
	}

findFirstFileA :: !String !LPWIN32_FIND_DATA !*w -> (!HANDLE, !*w)
findFirstFileA filename win32FindData world
	= code {
		ccall FindFirstFileA@8 "Pss:I:I"
	}

findNextFileA :: !HANDLE !LPWIN32_FIND_DATA !*w -> (!Bool, !*w)
findNextFileA hFindFile lpFindFileData world
	= code {
		ccall FindNextFileA@8 "PIs:I:I"
	}

formatMessageA :: !DWORD !LPCVOID !DWORD !DWORD !{#LPTSTR} !DWORD !Int -> DWORD
formatMessageA dwFlags lpSource dwMessageId dwLanguageId lpBuffer nSize args
	= code {
		ccall FormatMessageA@28 "PIIIIAII:I"
	}
	
getCurrentDirectoryA :: !DWORD !{#Char} !*w -> (!DWORD, *w)
getCurrentDirectoryA nBufferLength lpBuffer world
	= code {
		ccall GetCurrentDirectoryA@8 "PIs:I:I"
	}

getExitCodeProcess :: !HANDLE !*w -> (!Bool,!Int,!*w);
getExitCodeProcess handle world
	= code {
		ccall GetExitCodeProcess@8 "PI:II:I"
	}

getLastError :: !*w -> (!Int, !*w)
getLastError world
	= code {
		ccall GetLastError@0 "P:I:A"
	}

localFree :: !HLOCAL -> HLOCAL
localFree hMem
	= code {
		ccall LocalFree@4 "PI:I"
	}

moveFileA :: !String !String !*w -> (!Bool, !*w)
moveFileA lpExistingFileName lpNewFileName world
	= code {
		ccall MoveFileA@8 "Pss:I:I"
	}
	
removeDirectoryA :: !String !*w -> (!Bool, !*w)
removeDirectoryA lpFileName world
	= code {
		ccall RemoveDirectoryA@4 "Ps:I:I"
	}

setCurrentDirectoryA :: !String !*w -> (!Bool, !*w)
setCurrentDirectoryA lpPathName world
	= code {
		ccall SetCurrentDirectoryA@4 "Ps:I:I"
	}

waitForSingleObject :: !HANDLE !Int !*env -> (!Int,!*env)
waitForSingleObject handle timeout world
	= code {
		ccall WaitForSingleObject@8 "PpI:I:I"
	}

getProcessHeap :: !*env -> (!HANDLE, !*env)
getProcessHeap world
	= code {
		ccall GetProcessHeap@0 "P:I:I"
	}
	
heapAlloc :: !HANDLE !DWORD !SIZE_T !*env -> (!LPVOID, !*env)
heapAlloc hHeap dwFlags dwBytes world
	= code {
		ccall HeapAlloc@12 "PIII:p:I"
	}
	
heapFree :: !HANDLE !DWORD !LPVOID !*env -> (!Bool, !*env)
heapFree hHeap dwFlags lpMem world
	= code {
		ccall HeapFree@12 "PIIp:I:I"
	}

heapCreate :: !DWORD !SIZE_T !SIZE_T !*w -> (!HANDLE, !*w)
heapCreate flOptions dwInitialSize dwMaximumSize world = code {
	ccall HeapCreate@12 "PIII:I:I"
}
	
CreateThread :: !LPSECURITY_ATTRIBUTES !SIZE_T !LPTHREAD_START_ROUTINE !LPVOID !DWORD !*w -> (!HANDLE,!DWORD,!*w)
CreateThread threadAttributes stackSize startAddress parameter creationFlags world = code {
	ccall CreateThread "pIppI:II:I"
}

ResumeThread :: !HANDLE !*w -> (!DWORD, *w)
ResumeThread threadHandle world = code {
	ccall ResumeThread@4 "PI:I:I"
}

initializeCriticalSection :: !LPCRITICAL_SECTION !*w -> *w
initializeCriticalSection lpCriticalSection world = code {
	fill_a 0 1
	pop_a 1
	ccall InitializeCriticalSection@4 "PI:V:A"
}

WinGetThreadId :: !HANDLE !*w -> (!DWORD, !*w);
WinGetThreadId handle world = code {
	ccall GetThreadId@4 "Pp:I:I"
}

WinGetCurrentThreadId :: !*w -> (!DWORD, !*w)
WinGetCurrentThreadId world = code {
	ccall GetCurrentThreadId@0 "P:I:I"
}

WinOpenThread :: !DWORD !Bool !DWORD *w -> (!DWORD, !*w)
WinOpenThread dwDesiredAccess bInheritHandle dwThreadId world
 = code {
	ccall OpenThread@12 "PIII:I:I"
}

initializeCriticalSectionAndSpinCount :: !LPCRITICAL_SECTION !DWORD !*w -> (!Bool, !*w)
initializeCriticalSectionAndSpinCount lpCriticalSection dwSpinCount world = code {
	ccall InitializeCriticalSectionAndSpinCount@8 "PpI:I:I"
}

enterCriticalSection :: !LPCRITICAL_SECTION !*w -> *w
enterCriticalSection lpCriticalSection world = code {
	fill_a 0 1
	pop_a 1
	ccall EnterCriticalSection@4 "Pp:V:I"
}
	
leaveCriticalSection :: !LPCRITICAL_SECTION !*w -> *w
leaveCriticalSection lpCriticalSection world = code {
	fill_a 0 1
	pop_a 1
	ccall LeaveCriticalSection@4 "Pp:V:I"
}

createMutexA :: !LPSECURITY_ATTRIBUTES !Bool !LPCTSTR !*env -> (!HANDLE, !*env)
createMutexA lpMutexAttributes bInitialOwner lpName world = code {
	ccall CreateMutexA@12 "PpIp:I:I"
}

releaseMutex :: !HANDLE !*env -> (!Bool, !*env)
releaseMutex hMutex world = code {
	ccall ReleaseMutex@4 "PI:I:I"
}

createEventA :: !LPSECURITY_ATTRIBUTES !Bool !Bool !LPCTSTR !*w -> (!HANDLE, !*w)
createEventA lpEventAttributes bManualReset bInitialState lpName world = code {
	ccall CreateEventA@16 "PpIIp:I:I"
}

setEvent :: !HANDLE !*env -> (!Bool, !*env)
setEvent hEvent world = code {
	ccall SetEvent@4 "PI:I:I"
}

sleep :: !DWORD !*w -> *w
sleep dwMilliseconds world = code {
	fill_a 0 1
	pop_a 1
	ccall Sleep@4 "PI:V:I"
}

createPipe :: !PHANDLE !PHANDLE !SECURITY_ATTRIBUTES !DWORD !*w -> (!Bool, !*w)
createPipe hReadPipe hWritePipe lpPipeAttributes nSize world = code {
	ccall CreatePipe@16 "PppAI:I:I"
}

peekNamedPipe :: !HANDLE !LPVOID !DWORD !LPDWORD !LPDWORD !LPDWORD !*w -> (!Bool, !*w)
peekNamedPipe hNamedPipe lpBuffer nBufferSize lpBytesRead lpTotalBytesAvail lpBytesLeftThisMessage world = code {
	ccall PeekNamedPipe@24 "PIpIppp:I:I"
}
