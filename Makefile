CLEAN_HOME?=/opt/clean

test: test.icl TTY.icl TTY.dcl Clean\ System\ Files/ctty.o
	clm -dynamics -I $(CLEAN_HOME)/lib/Dynamics $(basename $<) -o $@

ifeq ($(OS), Windows_NT)
Clean\ System\ Files/ctty.o: tty_win.c
else
Clean\ System\ Files/ctty.o: tty_posix.c
endif
	mkdir -p Clean\ System\ Files
	gcc -c $< -o "$@"

clean:
	$(RM) -r Clean\ System\ Files/* test
