CLEAN_HOME?=/opt/clean
CLM:=clm
override CLMFLAGS+=-dynamics
CLMLIBS:=\
	-I ../../src/libraries/OS-Independent\
	-I ../../src/libraries/OS-Independent/Deprecated/ArgEnv\
	-I ../../src/libraries/OS-Independent/Deprecated/Generics\
	-I ../../src/libraries/OS-Independent/Deprecated/MersenneTwister\
	-I ../../src/libraries/OS-Independent/Deprecated/StdLib\
	-I ../../src/libraries/OS-Posix\
	-I ../../src/libraries/OS-Linux\
	-I ../../src/libraries/OS-Linux-64\
	-I ../../src/libraries/Platform-x86\
	-IL StdEnv\
	-IL Dynamics\
	-IL GraphCopy\
	-IL TCPIP

GCCVERSIONGTEQ6:=$(shell expr `gcc -dumpversion | cut -f1 -d.` \>= 6)
ifeq "$(GCCVERSIONGTEQ6)" "1"
	override CLMFLAGS+=-l -no-pie
endif

BINARIES:=test gentest snappytest

all: $(BINARIES)

%: %.icl .FORCE
	$(CLM) $(CLMLIBS) $(CLMFLAGS) $@ -o $@
	./$@

snappytest: %: %.icl .FORCE
	$(CLM) $(CLMLIBS) -l -lsnappy $(CLMFLAGS) -nr $@ -o $@
	./$@

clean:
	$(RM) -r $(BINARIES) Clean\ System\ Files

.FORCE:
