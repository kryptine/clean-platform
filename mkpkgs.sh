#!/bin/bash
set -e
trap "rm -rf CleanSerial" EXIT

mkpkg() {
	rm -rf CleanSerial
	mkdir -p CleanSerial
	make -B $1 -C src clean all
	cp -Rv src/"Clean System Files" CleanSerial
	cp -v src/{TTY,iTasksTTY}.[id]cl src/$2/Platform.[id]cl CleanSerial
	file CleanSerial/*/*.o
	$3 CleanSerial-$4 CleanSerial
}

#linux
mkpkg "" POSIX "tar -czvf" linux64.tar.gz
#windows64
mkpkg "DETECTED_OS=Windows CC=x86_64-w64-mingw32-gcc CFLAGS=-m64" Windows "zip -rv" win64.zip
#windows32
mkpkg "DETECTED_OS=Windows CC=x86_64-w64-mingw32-gcc-win32 CFLAGS=-m32" Windows "zip -rv" win32.zip
