implementation module _WinBase

import _WinDef

import code from library "_WinBase_library";

closeHandle :: !HANDLE !*World -> (!Bool,!*World)
closeHandle handle world
	= code {
		ccall CloseHandle@4 "PI:I:I"
	}

createFileA :: !LPCTSTR !DWORD !DWORD !LPSECURITY_ATTRIBUTES !DWORD !DWORD !HANDLE !*World -> (!Bool, !*World)
createFileA lpFileName dwDesiredAccess dwShareMode lpSecurityAttributes 
	dwCreationDisposition dwFlagsAndAttributes hTemplateFile world
	= code {
		ccall CreateFile@28 "PsIIAIII:I:I"
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
	
deleteFile :: !String !*World -> (!Int, !*World)
deleteFile path world = code inline {
	ccall DeleteFileA@4 "Ps:I:I"
}

findClose :: !HANDLE !*World -> (!Bool, !*World)
findClose handle world
	= code {
		ccall FindClose@4 "PI:I:I"
	}

findFirstFileA :: !String !LPWIN32_FIND_DATA !*World -> (!HANDLE, !*World)
findFirstFileA filename win32FindData world
	= code {
		ccall FindFirstFileA@8 "PsA:I:I"
	}

formatMessage :: !DWORD !LPCVOID !DWORD !DWORD !{#LPTSTR} !DWORD !Int -> DWORD
formatMessage dwFlags lpSource dwMessageId dwLanguageId lpBuffer nSize args =
	code {
		ccall FormatMessageA@28 "PIIIIAII:I"
	}
	
getExitCodeProcess :: !HANDLE !*World -> (!Bool,!Int,!*World);
getExitCodeProcess handle world
	= code {
		ccall GetExitCodeProcess@8 "PI:II:I"
	}

getLastError :: !*World -> (!Int, !*World)
getLastError world = 
	code {
		ccall GetLastError@0 "P:I:A"
	}

localFree :: !HLOCAL -> HLOCAL
localFree hMem =
	code {
		ccall LocalFree@4 "PI:I"
	}

waitForSingleObject :: !HANDLE !Int !*World -> (!Int,!*World);
waitForSingleObject handle timeout world
	= code {
		ccall WaitForSingleObject@8 "PpI:I:I"
	}
