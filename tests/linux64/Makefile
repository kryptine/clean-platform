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
	-IL StdEnv\
	-IL Dynamics\
	-IL TCPIP\
	-I ../../../gast/Libraries\
	-IL Gast

BINARIES:=test gentest

# Recursive wildcard; https://stackoverflow.com/a/12959694/1544337
rwildcard=$(wildcard $1$2) $(foreach d,$(wildcard $1*),$(call rwildcard,$d/,$2))

ALL_TESTS_ICLS:=$(call rwildcard,_Tests/,*.icl)
ALL_TESTS:=$(subst /,.,$(patsubst %.icl,%,$(ALL_TESTS_ICLS)))

all: $(BINARIES) $(ALL_TESTS)

$(BINARIES): .FORCE
	$(CLM) $(CLMLIBS) $(CLMFLAGS) $@ -o $@

$(ALL_TESTS): .FORCE
	$(CLM) -nr -nt -nortsopts -h 100m $(CLMFLAGS) $(CLMLIBS) $@ -o $@

run: all
	@FAILED=0; for t in $(BINARIES) $(ALL_TESTS); do echo "$$t"; ./$$t || FAILED=1; done; if [ $$FAILED -gt 0 ]; then exit 1; fi

clean:
	$(RM) -r $(BINARIES) $(ALL_TESTS) Clean\ System\ Files

.FORCE:
.PHONY: all run clean .FORCE
