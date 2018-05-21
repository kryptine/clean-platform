CLEAN_HOME?=/opt/clean
CLM:=clm
override CLMFLAGS+=-dynamics
CLMLIBS:=\
	-I ../../src/libraries/OS-Independent\
	-I ../../src/libraries/OS-Independent/Deprecated/Generics\
	-I ../../src/libraries/OS-Independent/Deprecated/ArgEnv\
	-I ../../src/libraries/OS-Independent/Deprecated/MersenneTwister\
	-I ../../src/libraries/OS-Independent/Deprecated/StdLib\
	-I ../../src/libraries/OS-Posix\
	-I ../../src/libraries/OS-Linux\
	-I ../../src/libraries/OS-Linux-64\
	-I ../../src/libraries/Platform-x86\
	-IL Dynamics\
	-IL TCPIP

BINARIES:=test gentest
RUN_BINARIES:=$(addprefix run_,$(BINARIES))

all: $(BINARIES)

$(BINARIES): .FORCE
	$(CLM) $(CLMLIBS) $(CLMFLAGS) $@ -o $@

$(RUN_BINARIES): run_%: %
	./$<

run: all
	@FAILED=0; for t in $(BINARIES); do echo "$$t"; ./$$t || FAILED=1; done; if [ $$FAILED -gt 0 ]; then exit 1; fi

clean:
	$(RM) -r $(BINARIES) Clean\ System\ Files

.FORCE:
.PHONY: all run clean .FORCE
