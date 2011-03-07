definition module _WinBase

import _WinDef

/*
 * Record definitions, size and field offsets
 */

:: LPSECURITY_ATTRIBUTES :== Int

:: LPSTARTUPINFO :== {#Int}
STARTUPINFO_size_bytes :== 68
STARTUPINFO_size_int :== 17
STARTUPINFO_cb_int_offset :== 0
STARTUPINFO_dwFlags_int_offset :== 11
STARTUPINFO_hStdError_int_offset :== 16

:: LPWIN32_FIND_DATA :== {#Char}
WIN32_FIND_DATA_size_bytes :== 320
WIN32_FIND_DATA_cFileName_int_offset :== 11
WIN32_FIND_DATA_cFileName_bytes_offset :== 48

:: LPPROCESS_INFORMATION :== {#Int}
PROCESS_INFORMATION_size_bytes :== 32
PROCESS_INFORMATION_size_int :== 4
PROCESS_INFORMATION_hProcess_int_offset :== 0
PROCESS_INFORMATION_hThread_int_offset :== 1

/*
 * Macros
 */

DETACHED_PROCESS :== 8
FORMAT_MESSAGE_ALLOCATE_BUFFER :== 0x00000100
FORMAT_MESSAGE_FROM_SYSTEM :== 0x00001000
FORMAT_MESSAGE_IGNORE_INSERTS :== 0x00000200
INFINITE :== 0xFFFFFFFF
LANGUAGE_NEUTRAL_SUBLANG_DEFAULT :== 0x400
STARTF_USESTDHANDLES :== 0x00000100
STATUS_PENDING :== 0x00000103
STILL_ACTIVE :== STATUS_PENDING
WAIT_ABANDONED_0 :== 0x80
WAIT_FAILED :== 0xFFFFFFFF
WAIT_OBJECT_0 :== 0
WAIT_TIMEOUT :== 258

/*
 * Windows API calls 
 */

closeHandle :: !HANDLE !*World -> (!Bool,!*World)

createFileA :: !LPCTSTR !DWORD !DWORD !LPSECURITY_ATTRIBUTES 
	!DWORD !DWORD !HANDLE !*World -> (!Bool, !*World)

createDirectoryA :: !String !LPSECURITY_ATTRIBUTES !*World -> (!Bool, !*World)

createProcessA :: !String !String !LPSECURITY_ATTRIBUTES !LPSECURITY_ATTRIBUTES !Bool !Int !LPVOID
					!LPCTSTR !LPSTARTUPINFO !LPPROCESS_INFORMATION !*World -> (!Bool,!*World)

createProcessA_dir :: !String !String !LPSECURITY_ATTRIBUTES !LPSECURITY_ATTRIBUTES !Bool !Int !LPVOID
					!String !LPSTARTUPINFO !LPPROCESS_INFORMATION !*World -> (!Bool,!*World)

deleteFileA :: !String !*World -> (!Int, !*World)

findClose :: !HANDLE !*World -> (!Bool, !*World)

findFirstFileA :: !String !LPWIN32_FIND_DATA !*World -> (!HANDLE, !*World)

findNextFileA :: !HANDLE !LPWIN32_FIND_DATA !*World -> (!Bool, !*World)

formatMessage :: !DWORD !LPCVOID !DWORD !DWORD !{#LPTSTR} !DWORD !Int -> DWORD

getCurrentDirectoryA :: !DWORD !{#Char} !*World -> (!DWORD, *World)

getExitCodeProcess :: !HANDLE !*World -> (!Bool,!Int,!*World);


getLastError :: !*World -> (!Int, !*World)

localFree :: !HLOCAL -> HLOCAL

moveFileA :: !String !String !*World -> (!Bool, !*World)

removeDirectoryA :: !String !*World -> (!Bool, !*World)

setCurrentDirectoryA :: !String !*World -> (!Bool, !*World)

waitForSingleObject :: !HANDLE !Int !*World -> (!Int,!*World);
