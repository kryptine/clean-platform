ifeq ($(OS), Windows_NT)
DETECTED_OS=Windows
else
DETECTED_OS=POSIX
endif

all: listDevices

.SECONDARY: listDevices.prj

%: %.prj %.icl 
	cpm $<

%.prj:
	cpm project $(basename $@) create
	cpm project $@ target iTasks
	cpm project $@ set -h 2000m -s 20m -dynamics
	cpm project $@ root ..
	cpm project $@ path add "$$PWD/../$(DETECTED_OS)"
	cpm project $@ path add "$$PWD/.."
