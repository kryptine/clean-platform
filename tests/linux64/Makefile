CLEAN_HOME?=/opt/clean
CLM:=clm
override CLMFLAGS+=-dynamics -l -no-pie
CLMLIBS:=\
	-I ../../src/libraries/OS-Independent\
	-I ../../src/libraries/OS-Independent/Deprecated/StdLib\
	-I ../../src/libraries/OS-Posix\
	-I ../../src/libraries/OS-Linux\
	-I ../../src/libraries/OS-Linux-64\
	-I $(CLEAN_HOME)/lib/StdEnv\
	-I $(CLEAN_HOME)/lib/Generics\
	-I $(CLEAN_HOME)/lib/Dynamics\
	-I $(CLEAN_HOME)/lib/TCPIP\

BINARIES:=test

all: $(BINARIES)

%: %.icl $(wildcard *.[id]cl)
	$(CLM) $(CLMLIBS) $(CLMFLAGS) $(basename $<) -o $@

clean:
	$(RM) -r $(BINARIES) Clean\ System\ Files
