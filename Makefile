CLEAN_HOME?=/opt/clean

ifeq ($(OS), Windows_NT)
DETECTED_OS=Windows
else
DETECTED_OS=POSIX
endif

test: test.icl TTY.icl TTY.dcl Clean\ System\ Files/ctty.o
	clm -dynamics -I $(DETECTED_OS) -I $(CLEAN_HOME)/lib/Platform -I $(CLEAN_HOME)/lib/Dynamics $(basename $<) -o $@

Clean\ System\ Files/ctty.o: $(DETECTED_OS)/tty.c
	mkdir -p Clean\ System\ Files
	gcc -c "$<" -o "$@"

Monitor.prj:
	cpm project $(basename $@) create
	cpm project $@ target iTasks
	cpm project $@ set -h 2000m -s 20m -dynamics
	cpm project $@ path add "$$PWD/POSIX"

clean:
	$(RM) -r $(DETECTED_OS)/Clean\ System\ Files/* Clean\ System\ Files/* test
