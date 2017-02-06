test: test.icl TTY.icl TTY.dcl Clean\ System\ Files/ctty.o
	clm -dynamics -l -no-pie -I /opt/clean/lib/Dynamics $(basename $<) -o $@

Clean\ System\ Files/ctty.o: tty.c
	if hash gcc-5;\
	then gcc-5 -DDEBUG -c $< -o "$@"; \
	else gcc -DDEBUG -c $< -o "$@"; \
	fi

clean:
	$(RM) -r Clean\ System\ Files/* test
