TTY: TTY.prj TTY.icl TTY.dcl Clean\ System\ Files/tty.o
	clm -desc -exl -tst -ns -nt TTY -o TTY

Clean\ System\ Files/tty.o: tty.c
	gcc -g -c $< -o "$@"

clean:
	$(RM) -r Clean\ System\ Files/* TTY sapl
