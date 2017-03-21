CLEAN_HOME?=/opt/clean
GCCVERSIONGTEQ6:=$(shell expr `gcc -dumpversion | cut -f1 -d.` \>= 6)

test: test.icl TTY.icl TTY.dcl Clean\ System\ Files/ctty.o
ifeq "$(GCCVERSIONGTEQ6)" "1"
	clm -dynamics -l -no-pie -I $(CLEAN_HOME)/lib/Dynamics $(basename $<) -o $@
else
	clm -dynamics -I $(CLEAN_HOME)/lib/Dynamics $(basename $<) -o $@
endif
	

Clean\ System\ Files/ctty.o: tty.c
	mkdir -p Clean\ System\ Files
ifeq "$(GCCVERSIONGTEQ6)" "1"
		gcc-5 -c $< -o "$@"
else
		gcc -c $< -o "$@"
endif

clean:
	$(RM) -r Clean\ System\ Files/* test
