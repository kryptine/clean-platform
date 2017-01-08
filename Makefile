test: test.icl TTY.icl TTY.dcl Clean\ System\ Files/ctty.o
	clm $(basename $<) -o $@

Clean\ System\ Files/ctty.o: tty.c
	gcc -c $< -o "$@"

clean:
	$(RM) -r Clean\ System\ Files/* test
