definition module _WinBase

import _WinDef

/*
 * Record definitions, size and field offsets
 */

:: FILETIME :== {#Char}
FILETIME_size_bytes :== 8
FILETIME_size_int :== 2

:: LPSYSTEMTIME :== {#Char}
SYSTEMTIME_size_bytes :== 16
SYSTEMTIME_wYear_offset :== 0
SYSTEMTIME_wMonth_offset :== 2
SYSTEMTIME_wDayOfWeek_offset :== 4
SYSTEMTIME_wDay_offset :== 6
SYSTEMTIME_wHour_offset :== 8
SYSTEMTIME_wMinute_offset :== 10
SYSTEMTIME_wSecond_offset :== 12
SYSTEMTIME_wMilliseconds_offset :== 14

:: LPSECURITY_ATTRIBUTES :== Int

:: LPOVERLAPPED :== Int

:: LPSTARTUPINFO :== {#Int}
STARTUPINFO_size_bytes :== 68
STARTUPINFO_size_int :== 17
STARTUPINFO_cb_int_offset :== 0
STARTUPINFO_dwFlags_int_offset :== 11
STARTUPINFO_hStdError_int_offset :== 16

:: LPWIN32_FIND_DATA :== {#Char}
WIN32_FIND_DATA_size_bytes :== 320
WIN32_FIND_DATA_dwFileAttributes_bytes_offset :== 0
WIN32_FIND_DATA_ftCreationTime_bytes_offset :== 4
WIN32_FIND_DATA_ftLastAccessTime_bytes_offset :== 12
WIN32_FIND_DATA_ftLastWriteTime_bytes_offset :== 20
WIN32_FIND_DATA_nFileSizeHigh_bytes_offset :==28 
WIN32_FIND_DATA_nFileSizeLow_bytes_offset :== 32
WIN32_FIND_DATA_cFileName_bytes_offset :== 44

FILE_ATTRIBUTE_DIRECTORY :== 16

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

GENERIC_READ :== 0x80000000
GENERIC_WRITE :== 0x40000000
FILE_SHARE_READ :== 0x00000001
FILE_SHARE_WRITE :== 0x00000002

CREATE_ALWAYS		:== 2
CREATE_NEW			:== 1
OPEN_ALWAYS			:== 4
OPEN_EXISTING		:== 3
TRUNCATE_EXISTING	:== 5

FILE_ATTRIBUTE_NORMAL :== 128
LOCKFILE_EXCLUSIVE_LOCK :== 0x00000002

HEAP_ZERO_MEMORY :== 0x00000008

/*
 * Windows API calls 
 */

closeHandle :: !HANDLE !*World -> (!Bool,!*World)
	
createFile :: !String !DWORD !DWORD !LPSECURITY_ATTRIBUTES 
	!DWORD !DWORD !HANDLE !*World -> (!HANDLE, !*World)
	
readFile :: !HANDLE !LPVOID !DWORD !LPDWORD !LPOVERLAPPED !*World -> (!Bool, !*World)

writeFile :: !HANDLE !LPVOID !DWORD !LPDWORD !LPOVERLAPPED !*World -> (!Bool, !*World)

setEndOfFile :: !HANDLE !*World -> (!Bool, !*World)

lockFileEx :: !HANDLE !DWORD !DWORD !DWORD !DWORD !LPOVERLAPPED !*World -> (!Bool, !*World)

unlockFile :: !HANDLE !DWORD !DWORD !DWORD !DWORD !*World -> (!Bool, !*World)

getFileSize :: !HANDLE !LPDWORD !*World -> (!DWORD, !*World)

createDirectoryA :: !String !LPSECURITY_ATTRIBUTES !*World -> (!Bool, !*World)

createProcessA :: !String !String !LPSECURITY_ATTRIBUTES !LPSECURITY_ATTRIBUTES !Bool !Int !LPVOID
					!LPCTSTR !LPSTARTUPINFO !LPPROCESS_INFORMATION !*World -> (!Bool,!*World)

createProcessA_dir :: !String !String !LPSECURITY_ATTRIBUTES !LPSECURITY_ATTRIBUTES !Bool !Int !LPVOID
					!String !LPSTARTUPINFO !LPPROCESS_INFORMATION !*World -> (!Bool,!*World)

deleteFileA :: !String !*World -> (!Int, !*World)

fileTimeToSystemTime :: !FILETIME !LPSYSTEMTIME !*World -> (!Bool, *World)

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

getProcessHeap :: !*World -> (!HANDLE, !*World)

heapAlloc :: !HANDLE !DWORD !SIZE_T !*World -> (!LPVOID, !*World)

heapFree :: !HANDLE !DWORD !LPVOID !*World -> (!Bool, !*World)