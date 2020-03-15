.PHONY: all clean

LYS_TTF=1
LYS_BACKEND=opencl
PROG_FUT_DEPS:=$(shell find fut -name \*.fut; find lib -name \*.fut)

all: futspace

ifeq ($(shell test futhark.pkg -nt lib; echo $$?),0)
futspace:
	futhark pkg sync
	@make # The sync might have resulted in a new Makefile.
else
FUT_SOURCE=fut/interactive_entrypoints.fut
CFLAGS_FUTSPACE=
LDFLAGS_FUTSPACE=-lfreeimage
include lib/github.com/diku-dk/lys/setup_flags.mk
futspace: libfutspace.o lib/github.com/diku-dk/lys/liblys.c lib/github.com/diku-dk/lys/liblys.h lib/github.com/diku-dk/lys/context_setup.c lib/github.com/diku-dk/lys/context_setup.h c/interactive.c
	gcc lib/github.com/diku-dk/lys/liblys.c lib/github.com/diku-dk/lys/context_setup.c c/interactive.c -I. -DPROGHEADER='"libfutspace.h"' libfutspace.o -o $@ $(CFLAGS) $(LDFLAGS) $(LDFLAGS_FUTSPACE) $(CFLAGS_FUTSPACE)
endif
libfutspace.o: libfutspace.c
	gcc -o $@ -c $< $(NOWARN_CFLAGS)

libfutspace.c: $(PROG_FUT_DEPS)
	futhark $(LYS_BACKEND) -o libfutspace --library $(FUT_SOURCE)

clean:
	rm -f futspace libfutspace.o libfutspace.c libfutspace.h