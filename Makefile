test: test.icl TTY.icl TTY.dcl Clean\ System\ Files/tty.o
	clm -desc -exl -tst -ns -nt $(basename $<) -o $@

Clean\ System\ Files/tty.o: tty.c
	gcc -g -c $< -o "$@"

clean:
	$(RM) -r Clean\ System\ Files/* test sapl
