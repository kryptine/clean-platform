#!/bin/sh

#linux
mkdir CleanSerial-linux64
rm -rf "./Clean System Files/*"
make -B 'Clean System Files/ctty.o'
mv 'Clean System Files' CleanSerial-linux64
cp TTY.[id]cl iTasksTTY.[id]cl POSIX/Platform.[id]cl CleanSerial-linux64
tar -czvf CleanSerial-linux64.tar.gz CleanSerial-linux64
rm -r CleanSerial-linux64

#windows64
mkdir CleanSerial-win64
rm -rf "./Clean System Files/*"
DETECTED_OS=Windows CC=x86_64-w64-mingw32-gcc make -B 'Clean System Files/ctty.o'
mv 'Clean System Files' CleanSerial-win64
cp TTY.[id]cl iTasksTTY.[id]cl Windows/Platform.[id]cl CleanSerial-win64
zip -rv CleanSerial-win64.zip CleanSerial-win64
rm -r CleanSerial-win64

#windows32
mkdir CleanSerial-win32
rm -rf "./Clean System Files/*"
DETECTED_OS=Windows CC=x86_64-w64-mingw32-gcc-win32 make -B 'Clean System Files/ctty.o'
mv 'Clean System Files' CleanSerial-win32
cp TTY.[id]cl iTasksTTY.[id]cl Windows/Platform.[id]cl CleanSerial-win32
zip -rv CleanSerial-win32.zip CleanSerial-win32
rm -r CleanSerial-win32
