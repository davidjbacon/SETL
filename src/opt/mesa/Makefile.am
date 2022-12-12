## Process this file with automake to generate Makefile.in
#
# $Id: Makefile.am,v 1.51 2022/03/04 00:32:25 setlorg Exp $
#
# Free software (c) dB - see file COPYING for license (GPL).
#
# Makefile.in -> Makefile via "configure" or "config.status" script.


# Suppress warnings caused by our use of GNU Make extensions:
AUTOMAKE_OPTIONS = -Wno-portability

AM_CPPFLAGS = -I$(srcdir) -I$(top_srcdir)/../run/freeglut-setl/include $(X_CFLAGS)

# The interface we build:
GENNED_GOALS = \
 custom.setl      \
 custom.lexicon   \
 custom.sysrots   \
 custom.h         \
 custom.c         \
 custom.dispatch

# Intermediate files:
TUPLES = \
 include.tuples   \
 const.tuples     \
 struct.tuples    \
 enum.tuples      \
 typedef.tuples   \
 routine.tuples

# More intermediate files:
GLUE = \
 stubprog.c       \
 preprocessed     \
 prototypes       \
 pulled.defs      \
 rawfunk          \
 pre.tuples       \
 typedefs         \
 tuples

# COPYING and README and Makefile.{am,in} are implicitly distributed,
# but these files have to be named explicitly in order to be distributed:
EXTRA_DIST = \
 auxi.c               \
 auxi.h               \
 auxi.setl            \
 defines.awk          \
 extern.setl          \
 for-s                \
 gears.setl           \
 gen.setl             \
 glut-patch           \
 glutaconst-gen.setl  \
 glutafunc-gen.setl   \
 glutSETL-gen.setl    \
 magic.setl           \
 massage              \
 megahack.setl        \
 one-line.setl        \
 sniff-params.setl    \
 t.setl               \
 tokens.setl          \
 ttt.setl             \
 tuplize.setl         \
 typedef.setl         \
 whatami

# These files normally have a very transient existence:
MOSTLYCLEANFILES = \
 tmp.c             \
 tmp.o

# Other files to be removed on a "make clean":
CLEANFILES = \
 $(GENNED_GOALS) \
 $(TUPLES)       \
 $(GLUE)

# On the subject of cleaning, see also the "scrub" rule below.


# This is to trigger prereq genning for user-facing goals such as all,
# clean, and install:
all-local:  $(GENNED_GOALS)

$(CLEANFILES):  Makefile.am

$(GENNED_GOALS):  tuples gen.setl auxi.h auxi.c
	$(SETL) $(srcdir)/gen.setl $(srcdir)/ <tuples

tuples:  $(TUPLES)
	cat $(TUPLES) >$@

include.tuples:  stubprog.c for-s
	$(GREP) '#include' <stubprog.c |\
	 $(srcdir)/for-s 'print([split(s)]);' >$@

struct.tuples:
	: >$@

enum.tuples:
	: >$@

typedef.tuples:  typedefs ttt.setl tokens.setl
	$(SETL) -I$(srcdir) $(srcdir)/ttt.setl <typedefs >$@

routine.tuples:  pre.tuples megahack.setl tokens.setl
	$(SETL) -I$(srcdir) $(srcdir)/megahack.setl <pre.tuples >$@

pre.tuples:  prototypes tuplize.setl
	$(SETL) $(srcdir)/tuplize.setl <prototypes >$@

prototypes:  rawfunk massage magic.setl
# The functions that take callback functions as parameters are
# supported separately and specially; glBlendEquationSeparateATI
# and gl.*MESA are declared in headers but not present in the
# precompiled libraries in at least one system (Ubuntu 9.04);
# glutInit is replaced by glutaInit:
	cat rawfunk |\
	 $(GREP) -v '(\* callback)' |\
	 $(GREP) -v ' _GLUfuncptr ' |\
	 $(GREP) -v ' GLprogramcallbackMESA ' |\
	 $(GREP) -v ' glBlendEquationSeparateATI(' |\
	 $(GREP) -v ' gl.*MESA' |\
	 $(GREP) -v ' glutInit(' |\
	 srcdir=$(srcdir) $(srcdir)/massage |\
	 $(SETL) $(srcdir)/magic.setl |\
	 $(SORT) | $(UNIQ) >$@

rawfunk:  preprocessed extern.setl
	$(SETL) $(srcdir)/extern.setl <preprocessed >$@

preprocessed:  stubprog.c auxi.h one-line.setl
	$(CPP) $(AM_CPPFLAGS) $(CPPFLAGS) stubprog.c |\
	 $(GREP) -v '^ *$$' |\
	 $(GREP) -v '^# ' |\
	 $(SED) -e 's/string/str/g' |\
	 $(SETL) $(srcdir)/one-line.setl >$@

typedefs:  preprocessed typedef.setl
	$(SETL) $(srcdir)/typedef.setl <preprocessed >$@

const.tuples:  pulled.defs defines.awk
	$(EGREP) '^#define (GL_|GLU_|GLUT_)' <pulled.defs |\
	 $(AWK) -f $(srcdir)/defines.awk |\
	 $(SORT) | $(UNIQ) >$@

pulled.defs:  stubprog.c auxi.h
	$(CPP_DEFINES) $(AM_CPPFLAGS) $(CPPFLAGS) stubprog.c >$@

stubprog.c:
	echo '#include <GL/freeglut.h>' >$@
	echo '#include "auxi.h"' >>$@

# This just checks to see if the contents of 'prototypes' will compile:
check-proto:  stubprog.c auxi.h prototypes
	cat stubprog.c prototypes >tmp.c
	$(CC) -c -Dtuple=void $(AM_CPPFLAGS) $(CPPFLAGS) $(AM_CFLAGS) $(CFLAGS) tmp.c
	-rm tmp.o tmp.c

# Convenience target that allows you to do what "make clean" does
# but without losing the Makefile:
scrub:
	-rm -f $(CLEANFILES)


# "make gears-demo" runs the GNU SETL version of the venerable one;
# $(GEARS_ARGS) (empty by default) is passed unquoted to gears.setl,
# so (for this strange little demo) you could go:
#
#  GEARS_ARGS='-geometry XxY+W+H' make gears-demo
#
# with approp substs of X,Y,W,H to pre-place and pre-scale the window:
gears-demo:  gears.setl custom.setl
	top_builddir=$(top_builddir)/.. $(top_srcdir)/../run-setl -I. $(srcdir)/gears.setl $(GEARS_ARGS)


.PHONY:  check-proto scrub gears-demo

# We don't want partly-written files left over after such failures as
# not yet having a $(SETL) command:
.DELETE_ON_ERROR:

## Makefile.am ends here
