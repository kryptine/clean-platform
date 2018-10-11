# CleanSerial

Serial port library for [clean](http://clean.cs.ru.nl). Build with `make`.

Linux and mac are fully supported. Windows support is only available at the
moment as a stub.  The functions are not implemented so it will crash.

## How to use
TODO

## Builds
Builds are available for Windows and Linux [here]([cpm](https://gitlab.science.ru.nl/mlubbers/CleanSerial/builds/artifacts/master/browse?job=clients)).
Just unzip it in `$CLEAN_HOME/lib` and add the path to your search paths in the
IDE/`cpm` or `clm`.

## Build it yourself
### Linux and mac
Run `make install`.

### Windows
Not sure, if I compile the object file with mingw it doesn't work. Cross
compiling works...

### Build the windows version on another OS
Install `gcc-mingw-w64` and run `DETECTED_OS=Windows CC=x86_64-w64-mingw32-gcc make`
