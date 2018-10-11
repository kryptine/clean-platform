#!/bin/sh

#linux
mkdir CleanSerial
rm -rf "./Clean System Files/*"
make -B 'Clean System Files/ctty.o'
mv 'Clean System Files' CleanSerial
cp TTY.[id]cl iTasksTTY.[id]cl POSIX/Platform.[id]cl CleanSerial
tar -czvf CleanSerial-linux64.tar.gz CleanSerial
rm -r CleanSerial

#windows64
mkdir CleanSerial
rm -rf "./Clean System Files/*"
DETECTED_OS=Windows CC=x86_64-w64-mingw32-gcc make -B 'Clean System Files/ctty.o'
mv 'Clean System Files' CleanSerial
cp TTY.[id]cl iTasksTTY.[id]cl Windows/Platform.[id]cl CleanSerial
cp Windows/CleanSerial_library CleanSerial/Clean\ System\ Files
zip -rv CleanSerial-win64.zip CleanSerial
rm -r CleanSerial

#windows32
mkdir CleanSerial
rm -rf "./Clean System Files/*"
DETECTED_OS=Windows CC=x86_64-w64-mingw32-gcc-win32 make -B 'Clean System Files/ctty.o'
mv 'Clean System Files' CleanSerial
cp TTY.[id]cl iTasksTTY.[id]cl Windows/Platform.[id]cl CleanSerial
cp Windows/CleanSerial_library CleanSerial/Clean\ System\ Files
zip -rv CleanSerial-win32.zip CleanSerial
rm -r CleanSerial
