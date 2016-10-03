test: test.icl TTY.icl TTY.dcl Clean\ System\ Files/tty.o
	clm -desc -exl -tst -ns -nt -dynamics -I /opt/clean/lib/Dynamics $(basename $<) -o $@

Clean\ System\ Files/tty.o: tty.c
	gcc -g -DDEBUG -c $< -o "$@"

clean:
	$(RM) -r Clean\ System\ Files/* test
