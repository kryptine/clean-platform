CLEAN_HOME?=/opt/clean
CFLAGS?=-Wall -Wextra -DDEBUG

ifeq ($(OS), Windows_NT)
DETECTED_OS?=Windows
LIBFOLDER?=Libraries
CC=gcc
else
DETECTED_OS?=POSIX
LIBFOLDER?=lib
endif

all: Clean\ System\ Files/ctty.o

test: test.icl TTY.icl TTY.dcl Clean\ System\ Files/ctty.o
	clm -I $(DETECTED_OS) -IL Platform $(basename $<) -o $@

Clean\ System\ Files/ctty.o: tty.c
	mkdir -p Clean\ System\ Files
	$(CC) $(CFLAGS) -c "$<" -o "$@"

Monitor.prj:
	cpm project $(basename $@) create
	cpm project $@ target iTasks
	cpm project $@ set -h 2000m -s 20m -dynamics
	cpm project $@ path add "$$PWD/POSIX"

install: Clean\ System\ Files/ctty.o $(LIBFILE)
	mkdir $(CLEAN_HOME)/$(LIBFOLDER)/CleanSerial
	cp -R TTY.[id]cl iTasksTTY.[id]cl $(DETECTED_OS)/Platform.[id]cl "Clean System Files" $(CLEAN_HOME)/$(LIBFOLDER)/CleanSerial
	cp -f $(DETECTED_OS)/*_library "$(CLEAN_HOME)/$(LIBFOLDER)/CleanSerial"

clean:
	$(RM) -r $(DETECTED_OS)/Clean\ System\ Files/* Clean\ System\ Files/* test
