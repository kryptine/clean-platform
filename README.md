# CleanSerial

Serial port library for [clean](http://clean.cs.ru.nl). Build with `make`.

Linux x64, mac x64 and windows x86 and x64 supported.
Autobuilds are available for [linux x64][linux64], windows [x86][windows86] and [x64][windows64].
The autobuilds can be extracted in the lib folder on linux or the Libraries
folder on windows

## Build it yourself
### Linux and mac
Run `make`.

### Windows
Run `make` in a mingw msys2 shell.

### Build the windows version on another OS
See `.gitlab-ci.yml`

[linux64]: https://gitlab.science.ru.nl/mlubbers/CleanSerial/builds/artifacts/master/file/CleanSerial-linux-x64.tar.gz?job=linux
[windows64]: https://gitlab.science.ru.nl/mlubbers/CleanSerial/builds/artifacts/master/file/CleanSerial-windows-x64.tar.gz?job=windows-x64
[windows86]: https://gitlab.science.ru.nl/mlubbers/CleanSerial/builds/artifacts/master/file/CleanSerial-windows-x86.tar.gz?job=windows-x86
