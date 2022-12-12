## Process this file with automake to generate Makefile.in
#
# $Id: Makefile.am,v 1.74 2022/12/11 17:09:27 setlorg Exp $
#
# Free software (c) dB - see file COPYING for license (GPL).
#
# Makefile.in -> Makefile via "configure" or "config.status" script.


# Suppress warnings caused by use of GNU Make extensions:
AUTOMAKE_OPTIONS = -Wno-portability

# Support for "W" graphics:
W = w

# Support for FreeGLUT (which may or may not really ride on Mesa):
MESA = freeglut-setl

# Where the GNU MP (GMP) lib is built if needed.  Usually this default
# is fine, though it can be overridden, as in the distcheck-src recipe
# in ../../main.mk:
GMP = $(top_builddir)/../gmp-build

# Build the W lib only if customization demands it:
if COND_W
  OPT_W = $(W)
  OPT_W_LIB = $(W)/libXW.la
endif

# Build the FreeGLUT lib only if customization demands it:
if COND_MESA
  OPT_MESA = $(MESA)
  OPT_MESA_LIB = $(MESA)/src/.libs/libglut.la
endif

# Use the bundled GNU MP lib only if indicated:
if USE_BUNDLED_GMP
  OPT_GMP = $(GMP)
  OPT_GMP_LIB = $(GMP)/libgmp.la
endif

# $(OPT_GMP) isn't mentioned here because we don't manage the building
# of GMP from here, we just use it:
SUBDIRS = $(OPT_W) $(OPT_MESA)

# Our main build target:
bin_PROGRAMS = setl

setl_SOURCES = \
 setlrun.h arena.h patrie.h patricia.h fd_bitset.h \
 main.c run.c init.c go.c assemble.c execute.c errage.c spewcode.c \
 sys.c lib.c heap.c arena.c patty.c rex.c os.c util.c \
 callskel.c fd_bitset.c \
 nopnds.awk opndpfxs.awk opndtypes.awk sysdatdef.awk sysdatinit.awk \
 sysrotdef.awk sysrotinit.awk oprotsigs.awk sysrotsigs.awk
nodist_setl_SOURCES = custom.c

# Included by patty.c but not compiled directly into .o files:
EXTRA_DIST = patrie.c patricia.c

# See also ../tran/Makefile.am for BUILT_SOURCES discussion:
BUILT_SOURCES = \
 opcode_defns.h sysrot_defns.h sysdat_defns.h \
 opcode_inits.c sysrot_inits.c sysdat_inits.c \
 sysrotsigs.c \
 opndtypes.c opndpfxs.c nopnds.c \
 custom.h custom.c custom.dispatch

CLEANFILES = $(BUILT_SOURCES)

# The ...FLAG things in the following would have been ...FLAGS, but
# then they would look too much like target-specific specs with missing
# targets.  For example, OPT_W_CPPFLAGS would provoke a warning about
# there being no canonical name OPT_W.

if COND_W
  OPT_W_CPPFLAG = -I$(srcdir)/$(W)
  # X_LIBS, despite its name, is flags such as -L..., not actual libs.
  OPT_W_LDFLAG = $(X_LIBS)
  OPT_W_LIBS = $(OPT_W_LIB) $(X_PRE_LIBS) -lX11 $(X_EXTRA_LIBS)
endif

if COND_MESA
  OPT_MESA_CPPFLAG = -I$(srcdir)/$(MESA)/include $(X_CFLAGS) -DGL_ELEMENT_ARRAY_BUFFER=0x8893 -DGL_FOG_COORDINATE_ARRAY_POINTER=0x8456 -DGL_MATRIX_INDEX_ARRAY_POINTER_ARB=0x8849 -DGL_SECONDARY_COLOR_ARRAY_POINTER=0x845D -DGL_VARIANT_ARRAY_POINTER_EXT=0x87E9 -DGL_VERTEX_ATTRIB_ARRAY_POINTER=0x8645
  OPT_MESA_LDFLAG = $(X_LIBS)
  # The tail of this, after $(OPT_MESA_LIB), matches the setting of
  # libglut_la_LIBADD in freeglut-setl/src/Makefile.am:
  OPT_MESA_LIBS = $(OPT_MESA_LIB) -lGLU -lGL $(X_PRE_LIBS) -lXext -lXt -lX11 $(LIBXXF86VM) $(X_EXTRA_LIBS)
endif

if USE_BUNDLED_GMP
  OPT_GMP_CPPFLAG = -I$(GMP)
  OPT_GMP_LDFLAG =
  OPT_GMP_LIBS = $(OPT_GMP_LIB)
endif

# Make sure "common.h" and "config.h" can always be found:
setl_CPPFLAGS = $(AM_CPPFLAGS) -I$(top_srcdir) -I$(top_builddir)

# Make sure the extra -I and -D options for customization packages are
# used when building the setl executable:
setl_CPPFLAGS += $(OPT_W_CPPFLAG) $(OPT_MESA_CPPFLAG) $(OPT_GMP_CPPFLAG)

# Link with static versions of local libtool-built libraries:
setl_LDFLAGS = $(AM_LDFLAGS) -static

# Extra link-time flags associated with customization packages and GMP:
setl_LDFLAGS += $(OPT_W_LDFLAG) $(OPT_MESA_LDFLAG) $(OPT_GMP_LDFLAG)

# Libraries associated with customization packages and GMP:
setl_LDADD = $(OPT_W_LIBS) $(OPT_MESA_LIBS) $(OPT_GMP_LIBS)

# Used in extending the GNU SETL translator and interpreter:
custom_dirs = $(addprefix $(top_builddir)/opt/,$(CUSTOM_PAX))
custom_h_files        = $(addsuffix /custom.h,       $(custom_dirs))
custom_c_files        = $(addsuffix /custom.c,       $(custom_dirs))
custom_dispatch_files = $(addsuffix /custom.dispatch,$(custom_dirs))

# Rebuild the generated sources if Makefile.am changes:
$(BUILT_SOURCES):  Makefile.am

# Rebuild the object files if Makefile changes:
$(setl_OBJECTS):  Makefile

# See comments in ../Makefile.am for the rationale behind including
# Makefile as a prerequisite in these next 3 rules:

custom.h:  $(custom_h_files) Makefile
	echo '/* generated header for SETL "library" extensions */' >$@
	echo '#ifndef _custom_h' >>$@
	echo '#define _custom_h' >>$@
	cat $(custom_h_files) /dev/null >>$@
	echo '#endif /* _custom_h */' >>$@

custom.c:  $(custom_c_files) Makefile
	echo '/* generated impl code for SETL "library" extensions */' >$@
	cat $(custom_c_files) /dev/null >>$@

custom.dispatch:  $(custom_dispatch_files) Makefile
	echo '/* generated calls to custom routines */' >$@
	cat $(custom_dispatch_files) /dev/null >>$@

opcode_defns.h:  $(top_srcdir)/opcodes
	echo '/* generated from "$<" */' >$@
	$(SED) -e 's,#.*$$,,' <$< | \
	 $(AWK) 'NF > 0 {printf "#define op_%s %d\n", $$1, n++}' >>$@

opndtypes.c:  $(top_srcdir)/opcodes opndtypes.awk
	echo '  /* generated from "$<" by "opndtypes.awk" */' >$@
	$(SED) -e 's,#.*$$,,' <$< | \
	 $(AWK) -f $(srcdir)/opndtypes.awk >>$@

opndpfxs.c:  $(top_srcdir)/opcodes opndpfxs.awk
	echo '  /* generated from "$<" by "opndpfxs.awk" */' >$@
	$(SED) -e 's,#.*$$,,' <$< | \
	 $(AWK) -f $(srcdir)/opndpfxs.awk >>$@

nopnds.c:  $(top_srcdir)/opcodes nopnds.awk
	echo '  /* generated from "$<" by "nopnds.awk" */' >$@
	$(SED) -e 's,#.*$$,,' <$< | \
	 $(AWK) -f $(srcdir)/nopnds.awk >>$@

sysrot_defns.h:  $(top_srcdir)/lexicon \
                 $(top_builddir)/custom.lexicon \
                 sysrotdef.awk
	echo '/* generated by "sysrotdef.awk" */' >$@
	cat $(top_srcdir)/lexicon $(top_builddir)/custom.lexicon | \
	 $(AWK) -f $(srcdir)/sysrotdef.awk >>$@

sysdat_defns.h:  $(top_srcdir)/lexicon \
                 $(top_builddir)/custom.lexicon \
                 sysdatdef.awk
	echo '/* generated by "sysdatdef.awk" */' >$@
	cat $(top_srcdir)/lexicon $(top_builddir)/custom.lexicon | \
	 $(AWK) -f $(srcdir)/sysdatdef.awk >>$@

opcode_inits.c:  $(top_srcdir)/opcodes
	echo '  /* generated from "$<" */' >$@
	$(SED) -e 's,#.*$$,,' <$< | \
	 $(AWK) 'NF > 0 {printf "  optab_init(\"%s\",op_%s);\n", $$1, $$1}' >>$@

sysrot_inits.c:  $(top_srcdir)/lexicon \
                 $(top_builddir)/custom.lexicon \
                 sysrotinit.awk
	echo '  /* generated by "sysrotinit.awk" */' >$@
	cat $(top_srcdir)/lexicon $(top_builddir)/custom.lexicon | \
	 $(AWK) -f $(srcdir)/sysrotinit.awk >>$@

sysdat_inits.c:  $(top_srcdir)/lexicon \
                 $(top_builddir)/custom.lexicon \
                 sysdatinit.awk
	echo '  /* generated by "sysdatinit.awk" */' >$@
	cat $(top_srcdir)/lexicon $(top_builddir)/custom.lexicon | \
	 $(AWK) -f $(srcdir)/sysdatinit.awk >>$@

sysrotsigs.c:  $(top_srcdir)/lexicon \
               $(top_builddir)/custom.lexicon \
               $(top_srcdir)/sysrots \
               $(top_builddir)/custom.sysrots \
               oprotsigs.awk \
               sysrotsigs.awk
	echo '  /* generated by "oprotsigs.awk and sysrotsigs.awk" */' >$@
	cat $(top_srcdir)/lexicon $(top_builddir)/custom.lexicon | \
	 $(AWK) -f $(srcdir)/oprotsigs.awk >>$@
	cat $(top_srcdir)/sysrots $(top_builddir)/custom.sysrots | \
	 $(AWK) -f $(srcdir)/sysrotsigs.awk >>$@


## Makefile.am ends here
