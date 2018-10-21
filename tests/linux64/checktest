#!/bin/bash

cd "$(dirname "${BASH_SOURCE[0]}")"

COLLECTIONS="OS-Independent OS-Linux OS-Linux-64 OS-Posix Platform-x86"
FILE="test.icl"

SUCCESS=1

for coll in $COLLECTIONS
do
	MODULES="$(find "../../src/libraries/$coll" -name '*.dcl' \
		| sed \
			-e 's:.*libraries/[^/]*/::' \
			-e 's:Deprecated/[^/]*/::' \
			-e 's:.dcl::' \
			-e 's:/:\.:g')"
	for mod in $MODULES
	do
		if ! grep "import.* $mod\\($\\|[^[:alnum:].]\\)" "$FILE" >/dev/null
		then
			echo "Not used in $FILE: $mod"
			export SUCCESS=0
		fi
	done
done

if [ $SUCCESS != 1 ]
then
	exit -1
else
	echo "All modules from $COLLECTIONS are included in $FILE."
fi
