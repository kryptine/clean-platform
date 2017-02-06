CLEAN_HOME?=/opt/clean
test: test.icl TTY.icl TTY.dcl Clean\ System\ Files/ctty.o
	if hash gcc-5 2>/dev/null;\
	then clm -dynamics -l -no-pie -I $(CLEAN_HOME)/lib/Dynamics $(basename $<) -o $@; \
	else clm -dynamics -I $(CLEAN_HOME)/lib/Dynamics $(basename $<) -o $@; \
	fi
	

Clean\ System\ Files/ctty.o: tty.c
	if hash gcc-5 2>/dev/null;\
	then gcc-5 -DDEBUG -c $< -o "$@"; \
	else gcc -DDEBUG -c $< -o "$@"; \
	fi

clean:
	$(RM) -r Clean\ System\ Files/* test
