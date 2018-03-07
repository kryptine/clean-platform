CLEAN_HOME?=/opt/clean

ifeq ($(OS), Windows_NT)
DETECTED_OS=Windows
else
DETECTED_OS=POSIX
endif

test: test.icl TTY.icl TTY.dcl Clean\ System\ Files/ctty.o
	clm -dynamics -I $(DETECTED_OS) -I $(CLEAN_HOME)/lib/Dynamics $(basename $<) -o $@

Clean\ System\ Files/ctty.o: $(DETECTED_OS)/tty.c
	mkdir -p Clean\ System\ Files
	gcc -c "$<" -o "$@"

clean:
	$(RM) -r $(DETECTED_OS)/Clean\ System\ Files/* Clean\ System\ Files/* test
