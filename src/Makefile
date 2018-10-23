CFLAGS?=-Wall -Wextra

ifeq ($(OS), Windows_NT)
DETECTED_OS?=Windows
CC=gcc
else
DETECTED_OS?=POSIX
endif

LIBFILES=$(wildcard $(DETECTED_OS)/*library)

VPATH = src:../csource

all: $(addprefix Clean\ System\ Files/,ctty.o $(LIBFILES))

Clean\ System\ Files/%: %
	mkdir -p "Clean System Files"
	cp "$<" "Clean System Files"

ctty.o: tty.c
	$(COMPILE.c) $(OUTPUT_OPTION) $<

clean:
	$(RM) -rf *.o Clean\ System\ Files
