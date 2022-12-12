## Process this file with automake to generate Makefile.in
#
# $Id: Makefile.am,v 1.33 2022/11/17 14:16:07 setlorg Exp $
#
# Free software (c) dB - see file COPYING for license (GPL).
#
# Makefile.in -> Makefile via "configure" or "config.status" script.


# Our main build target:
bin_PROGRAMS = setlcpp

setlcpp_SOURCES = \
  config.h cpplib.h cpphash.h obstack.h \
  cppmain.c cpplib.c cpphash.c cppalloc.c cpperror.c cppexp.c \
  version.c obstack.c

# Some predefinitions for everyone:
AM_CPPFLAGS = \
  -DUSE_PROTOTYPES=1 \
  -DDOLLARS_IN_IDENTIFIERS=0 \
  -DGCC_INCLUDE_DIR=\"\" \
  -DINCLUDE_DEFAULTS='{{0,0,0}}'

# Make sure "common.h" and "config.h" can be found by setlcpp sources:
setlcpp_CPPFLAGS = $(AM_CPPFLAGS) -I$(top_srcdir) -I$(top_builddir)

# For GCC versions prior to the advent of #pragma GCC diagnostic,
# blanket-suppress several warnings that are normally only suppressed
# in limited ranges within the code using GCC_DIAG_OFF and GCC_DIAG_ON
# macro calls (which effectively expand to such #pragmas):
if COND_GCC
if !COND_PRAGMA_GCC_DIAGNOSTIC
  AM_CFLAGS += \
    -Wno-strict-prototypes \
    -Wno-missing-prototypes \
    -Wno-missing-declarations \
    -Wno-missing-field-initializers
endif
endif

# Rebuild the object files if Makefile changes:
$(setlcpp_OBJECTS):  Makefile

# Buildable sources for the old "monolithic" version of the
# C preprocessor (cccp) are included for historical reference.
# They are not part of setlcpp and are not built by default.
# Use "make cccp" to build the old thing.
#
# You will see lots of warnings if you "make cccp" with a recent
# enough version of GCC, unless you pass lots of extra flags
# ( make CFLAGS='-Wno-c++-compat -Wno-...' cccp ) or suppress all
# warnings using -w ( make CFLAGS=-w cccp ).

EXTRA_DIST = pcp.h cccp.c cexp.y
CLEANFILES = cccp
DISTCLEANFILES = cexp.c

cccp:  cccp.o cexp.o version.o obstack.o
	$(LINK) $^

cccp.o:  cccp.c config.h ../config.h pcp.h

cexp.o:  cexp.c config.h ../config.h

cexp.c:  cexp.y
	$(YACC) $<
	mv y.tab.c $@


## Makefile.am ends here
