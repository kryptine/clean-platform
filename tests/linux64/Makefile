CLEAN_HOME?=/opt/clean
CLM:=clm
override CLMFLAGS+=-dynamics -funcmayfail
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

BINARIES:=checktest test gentest snappytest tartest

all: $(BINARIES)

%: %.icl .FORCE
	$(CLM) $(CLMLIBS) -PABC StdEnv
	$(CLM) $(CLMLIBS) -PABC StdMaybe
	$(CLM) $(CLMLIBS) -PABC -dynamics _SystemDynamic
	$(CLM) $(CLMLIBS) -PABC TCPIP
	$(CLM) $(CLMLIBS) $(CLMFLAGS) $@ -o $@
	./$@

checktest: .FORCE
	./checktest.sh

snappytest: %: %.icl .FORCE
	$(CLM) $(CLMLIBS) -l -lsnappy $(CLMFLAGS) -nr $@ -o $@
	./$@

tartest: .FORCE
	$(RM) -r _test-old _test _$@.tar
	$(CLM) $(CLMLIBS) $(CLMFLAGS) -nr -nt $@ -o $@
	mkdir -p _$@/subdir/subdir2
	echo hello > _$@/subdir/hello
	echo test > _$@/test
	tar cf _$@.tar _$@
	mv _$@ _$@-old
	./$@
	diff -r _$@-old _$@
	$(RM) -r _$@-old _$@ _$@.tar

clean:
	$(RM) -r $(BINARIES) Clean\ System\ Files

.FORCE:
