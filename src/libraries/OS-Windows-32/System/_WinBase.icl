implementation module _WinBase

import _WinDef

import code from library "_WinBase_library";

closeHandle :: !HANDLE !*World -> (!Bool,!*World)
closeHandle handle world
	= code {
		ccall CloseHandle@4 "PI:I:I"
	}
	
createDirectoryA :: !String !LPSECURITY_ATTRIBUTES !*World -> (!Bool, !*World)
createDirectoryA lpFileName lpSecurityAttributes world
	= code {
		ccall CreateDirectoryA@8 "PsI:I:I"
	}
	
createFile :: !String !DWORD !DWORD !LPSECURITY_ATTRIBUTES !DWORD !DWORD !HANDLE !*World -> (!HANDLE, !*World)
createFile lpFileName dwDesiredAccess dwShareMode lpSecurityAttributes 
	dwCreationDisposition dwFlagsAndAttributes hTemplateFile world
	= code {
		ccall CreateFileA@28 "PsIIIIII:I:I"
	}
	
readFile :: !HANDLE !LPVOID !DWORD !LPDWORD !LPOVERLAPPED !*World -> (!Bool, !*World)
readFile hFile lpBuffer nNumberOfBytesToRead lpNumberOfBytesRead lpOverlapped world
	= code {
		ccall ReadFile@20 "PIpIAp:I:I"
	}
	
writeFile :: !HANDLE !LPVOID !DWORD !LPDWORD !LPOVERLAPPED !*World -> (!Bool, !*World)
writeFile hFile lpBuffer nNumberOfBytesToWrite lpNumberOfBytesWritten lpOverlapped world
	= code {
		ccall WriteFile@20 "PIpIAp:I:I"
	}
	
setEndOfFile :: !HANDLE !*World -> (!Bool, !*World)
setEndOfFile hFile world
	= code {
		ccall SetEndOfFile@4 "PI:I:I"
	}

lockFileEx :: !HANDLE !DWORD !DWORD !DWORD !DWORD !LPOVERLAPPED !*World -> (!Bool, !*World)
lockFileEx hFile dwFlags dwReserved nNumberOfBytesToLockLow nNumberOfBytesToLockHigh lpOverlapped world
	= code {
		ccall LockFileEx@24 "PIIIIIp:I:I"
	}
	
unlockFile :: !HANDLE !DWORD !DWORD !DWORD !DWORD !*World -> (!Bool, !*World)
unlockFile hFile dwFileOffsetLow dwFileOffsetHigh nNumberOfBytesToUnlockLow nNumberOfBytesToUnlockHigh world
	= code {
		ccall UnlockFile@20 "PIIIII:I:I"
	}
	
getFileSize :: !HANDLE !LPDWORD !*World -> (!DWORD, !*World)
getFileSize hFile lpFileSizeHigh world
	= code {
		ccall GetFileSize@8 "PIA:I:I"
	}

createProcessA :: !String !String !LPSECURITY_ATTRIBUTES !LPSECURITY_ATTRIBUTES !Bool !Int !LPVOID
					!LPCTSTR !LPSTARTUPINFO !LPPROCESS_INFORMATION !*World -> (!Bool,!*World)
createProcessA lpApplicationName commandLine lpProcessAttributes lpThreadAttributes inheritHandles creationFlags lpEnvironment
					currentDirectory lpStartupInfo lpProcessInformation os
	= code {
		ccall CreateProcessA@40 "PssIIIIIIAA:I:I"
	}
	
createProcessA_dir :: !String !String !LPSECURITY_ATTRIBUTES !LPSECURITY_ATTRIBUTES !Bool !Int !LPVOID
					!String !LPSTARTUPINFO !LPPROCESS_INFORMATION !*World -> (!Bool,!*World)
createProcessA_dir lpApplicationName commandLine lpProcessAttributes lpThreadAttributes inheritHandles creationFlags lpEnvironment
					currentDirectory lpStartupInfo lpProcessInformation os
	= code {
		ccall CreateProcessA@40 "PssIIIIIsAA:I:I"
	}
	
deleteFileA :: !String !*World -> (!Int, !*World)
deleteFileA path world = code inline {
	ccall DeleteFileA@4 "Ps:I:I"
}

fileTimeToSystemTime :: !FILETIME !LPSYSTEMTIME !*World -> (!Bool, *World)
fileTimeToSystemTime lpFileTime lpSystemTime world
	= code {
		ccall FileTimeToSystemTime@8 "Pss:I:I"
	}

findClose :: !HANDLE !*World -> (!Bool, !*World)
findClose handle world
	= code {
		ccall FindClose@4 "PI:I:I"
	}

findFirstFileA :: !String !LPWIN32_FIND_DATA !*World -> (!HANDLE, !*World)
findFirstFileA filename win32FindData world
	= code {
		ccall FindFirstFileA@8 "Pss:I:I"
	}

findNextFileA :: !HANDLE !LPWIN32_FIND_DATA !*World -> (!Bool, !*World)
findNextFileA hFindFile lpFindFileData world
	= code {
		ccall FindNextFileA@8 "PIs:I:I"
	}

formatMessage :: !DWORD !LPCVOID !DWORD !DWORD !{#LPTSTR} !DWORD !Int -> DWORD
formatMessage dwFlags lpSource dwMessageId dwLanguageId lpBuffer nSize args
	= code {
		ccall FormatMessageA@28 "PIIIIAII:I"
	}
	
getCurrentDirectoryA :: !DWORD !{#Char} !*World -> (!DWORD, *World)
getCurrentDirectoryA nBufferLength lpBuffer world
	= code {
		ccall GetCurrentDirectoryA@8 "PIA:I:I"
	}

getExitCodeProcess :: !HANDLE !*World -> (!Bool,!Int,!*World);
getExitCodeProcess handle world
	= code {
		ccall GetExitCodeProcess@8 "PI:II:I"
	}

getLastError :: !*World -> (!Int, !*World)
getLastError world
	= code {
		ccall GetLastError@0 "P:I:A"
	}

localFree :: !HLOCAL -> HLOCAL
localFree hMem
	= code {
		ccall LocalFree@4 "PI:I"
	}

moveFileA :: !String !String !*World -> (!Bool, !*World)
moveFileA lpExistingFileName lpNewFileName world
	= code {
		ccall MoveFileA@8 "Pss:I:I"
	}
	
removeDirectoryA :: !String !*World -> (!Bool, !*World)
removeDirectoryA lpFileName world
	= code {
		ccall RemoveDirectoryA@4 "Ps:I:I"
	}

setCurrentDirectoryA :: !String !*World -> (!Bool, !*World)
setCurrentDirectoryA lpPathName world
	= code {
		ccall SetCurrentDirectoryA@4 "Ps:I:I"
	}

waitForSingleObject :: !HANDLE !Int !*World -> (!Int,!*World);
waitForSingleObject handle timeout world
	= code {
		ccall WaitForSingleObject@8 "PpI:I:I"
	}

getProcessHeap :: !*World -> (!HANDLE, !*World)
getProcessHeap world
	= code {
		ccall GetProcessHeap@0 "P:I:I"
	}
	
heapAlloc :: !HANDLE !DWORD !SIZE_T !*World -> (!LPVOID, !*World)
heapAlloc hHeap dwFlags dwBytes world
	= code {
		ccall HeapAlloc@12 "PIII:p:I"
	}
	
heapFree :: !HANDLE !DWORD !LPVOID !*World -> (!Bool, !*World)
heapFree hHeap dwFlags lpMem world
	= code {
		ccall HeapFree@12 "PIIp:I:I"
	}
	
CreateThread :: !LPSECURITY_ATTRIBUTES !SIZE_T !LPTHREAD_START_ROUTINE !LPVOID !DWORD !*World -> (!HANDLE,!DWORD,!*World)
CreateThread threadAttributes stackSize startAddress parameter creationFlags world = code {
	ccall CreateThread "pIppI:II:I"
}

initializeCriticalSection :: !LPCRITICAL_SECTION !*World -> *World
initializeCriticalSection lpCriticalSection world = code {
	ccall InitializeCriticalSection@4 "p:V:I"
}