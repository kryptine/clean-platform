CLEAN_HOME?=/opt/clean
CLM:=clm
override CLMFLAGS+=-dynamics -aC,-h,200m,-s,2m -funcmayfail -lset
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
	-IL Dynamics\
	-IL GraphCopy\
	-IL TCPIP\
	-IL Gast\
	-l -lmariadb\
	-l -lsqlite3\
	-l -lsnappy

COCLPATH?=../../../compiler
COCLLIBS:=\
	-IL ArgEnv\
	-I $(COCLPATH)/frontend\
	-I $(COCLPATH)/main\
	-I $(COCLPATH)/main/Unix

BINARIES:=checktest test commentstest gentest snappytest tartest
RUN_BINARIES:=$(addprefix run_,$(BINARIES))

all: $(RUN_BINARIES)

$(filter-out checktest commentstest snappytest tartest,$(BINARIES)): .FORCE
	$(CLM) $(CLMLIBS) -PABC StdEnv
	$(CLM) $(CLMLIBS) -PABC StdMaybe
	$(CLM) $(CLMLIBS) -PABC -dynamics _SystemDynamic
	$(CLM) $(CLMLIBS) -PABC TCPIP
	$(CLM) $(CLMLIBS) -PABC Gast
	find ../../src/libraries -name '*.abc' -exec rm {} \;
	$(CLM) $(CLMLIBS) $(CLMFLAGS) $@ -o $@

$(filter-out run_tartest,$(RUN_BINARIES)): run_%: %
	./$<

run: all
	@FAILED=0; for t in $(BINARIES); do echo "$$t"; ./$$t || FAILED=1; done; if [ $$FAILED -gt 0 ]; then exit 1; fi

snappytest: %: %.icl .FORCE
	$(CLM) $(CLMLIBS) $(CLMFLAGS) -nr $@ -o $@

tartest: .FORCE
	$(CLM) $(CLMLIBS) $(CLMFLAGS) -nr -nt $@ -o $@

run_tartest: tartest
	$(RM) -r _test-old _test _tartest.tar
	mkdir -p _tartest/subdir/subdir2
	echo hello > _tartest/subdir/hello
	echo test > _tartest/test
	tar cf _tartest.tar _tartest
	mv _tartest _tartest-old
	./tartest
	diff -r _tartest-old _tartest
	$(RM) -r _tartest-old _tartest _tartest.tar

commentstest: .FORCE
	$(CLM) $(CLMLIBS) $(COCLLIBS) -nr -nt $@ -o $@

clean:
	$(RM) -r $(filter-out checktest,$(BINARIES)) _tartest* Clean\ System\ Files

.FORCE:
.PHONY: all run clean .FORCE
