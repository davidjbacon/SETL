## Process this file with automake to generate Makefile.in
#
# $Id: Makefile.am,v 1.20 2022/12/11 17:09:27 setlorg Exp $
#
# Free software (c) dB - see file COPYING for license (GPL).
#
# Makefile.in -> Makefile via "configure" or "config.status" script.


# Suppress warnings caused by our use of GNU Make extensions:
AUTOMAKE_OPTIONS = -Wno-portability

GENNED_GOALS =  \
 custom.setl    \
 custom.lexicon \
 custom.sysrots \
 custom.h       \
 custom.c       \
 custom.dispatch

EXTRA_DIST = \
 whatami     \
 $(addprefix w-,$(GENNED_GOALS))

CLEANFILES = $(GENNED_GOALS)

all-local:  $(GENNED_GOALS)

custom.%:  w-custom.%
	cp -p $< $@

## Makefile.am ends here
