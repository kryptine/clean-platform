%.prj:
	cpm project $(basename $@) create
	cpm project $@ target iTasks
	cpm project $@ set -h 2000m -s 20m -dynamics
	cpm project $@ root ..
	cpm project $@ path add "$$PWD/../POSIX"
	cpm project $@ path add "$$PWD/.."
