TTY: TTY.prj TTY.icl TTY.dcl Clean\ System\ Files/tty.o
	cpm make

Clean\ System\ Files/tty.o: tty.c
	gcc -g -c $< -o "$@"

clean:
	$(RM) -r Clean\ System\ Files/* TTY sapl
