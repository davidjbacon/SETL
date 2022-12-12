## Process this file with automake to generate Makefile.in
#
# $Id: Makefile.am,v 1.41 2021/10/05 23:01:50 setlorg Exp $
#
# Free software (c) dB - see file COPYING for license (GPL).
#
# Makefile.in -> Makefile via "configure" or "config.status" script.


# Our main build target:
bin_PROGRAMS = setltran

setltran_SOURCES = \
 setltran.h patrie.h patricia.h symstack.h \
 main.c tran.c tokenize.c toksym.c sysrot.c scopes.c parse.c \
 analyze.c flatten.c identify.c canonize.c gencode.c optimize.c \
 spewobj.c symtab.c util.c mem.c patty.c debug.c \
 include.awk mawk.awk nodes.awk sysrots.awk tinit.awk yytokens.awk
nodist_setltran_SOURCES = y.c

# These files are #include'd (by patty.c), not directly compiled:
EXTRA_DIST = patrie.c patricia.c

# These files need to be generated before the files that refer to
# them get compiled; note that, per the Automake documentation on
# BUILT_SOURCES, in a fresh distribution this will only happen on a
# make "all", "check", or "install", but after that, the automatic
# dependency tracking as reflected in .deps/ makes it possible to do
# (for example) a "make setltran":
BUILT_SOURCES = \
 y.y y.tab.h y.tab.c y.c \
 Tokens Tinits \
 Nodebuilders Nodetypes \
 Optypes Sysrot.inits String.opcodes Obj.header \
 Debug.tcodes Debug.nodetypes

MOSTLYCLEANFILES = y.output

CLEANFILES = $(BUILT_SOURCES)

# Make sure "common.h" and "config.h" can always be found:
setltran_CPPFLAGS = $(AM_CPPFLAGS) -I$(top_srcdir) -I$(top_builddir)

# Flags for yacc/bison:
setltran_YFLAGS = $(AM_YFLAGS) -d -t

# Versions of Darwin even through late 2015 don't grok POSIX syntax
# for the 'tail' command, and GNU now only groks the ancient and
# venerable "+N" option when coaxed to with this _POSIX2_VERSION
# envt var setting:
TAIL = _POSIX2_VERSION=199209 tail
# Here is a clumsier workaround that could be resurrected if nec.:
#TAILPLUS := $(shell if tail +2 </dev/null 2>/dev/null; \
#                    then echo 'tail +'; \
#                    else echo 'tail --lines=+'; \
#                    fi)
# You would invoke that using (for example) "$(TAILPLUS)2 ...".

# Rebuild the generated sources if Makefile.am changes:
$(BUILT_SOURCES):  Makefile.am

# Rebuild the object files if Makefile changes:
$(setltran_OBJECTS):  Makefile

y.tab.h y.tab.c:  y.y
	@echo '	Expect 146 shift/reduce conflicts...'
	$(YACC) $(setltran_YFLAGS) $(YFLAGS) $< 2>&1

y.y:  $(top_srcdir)/grammar Tokens Nodebuilders include.awk mawk.awk
	echo '/*  Generated from $(top_srcdir)/grammar  */' >$@
	echo '' >>$@
	$(AWK) -f $(srcdir)/include.awk <$(top_srcdir)/grammar \
	 | $(AWK) -f $(srcdir)/mawk.awk \
	 | $(UNIQ) >>$@

y.c:  y.tab.c
	echo '/*  Generated from y.tab.c  */' >$@
	echo '' >>$@
	$(SED) -e 's/YYSTYPE yyval;/YYSTYPE yyval = null;/' <$< >>$@

Tokens:  $(top_srcdir)/lexicon $(top_builddir)/custom.lexicon yytokens.awk
	echo '/*  Generated from $(top_srcdir)/lexicon and $(top_builddir)/custom.lexicon  */' >$@
	$(TAIL) +2 $(top_srcdir)/lexicon | \
	 cat - $(top_builddir)/custom.lexicon | \
	 $(AWK) -f $(srcdir)/yytokens.awk | \
	 $(SORT) | $(UNIQ) >>$@

Tinits:  $(top_srcdir)/lexicon $(top_builddir)/custom.lexicon tinit.awk
	echo '  /* Generated from $(top_srcdir)/lexicon and $(top_builddir)/custom.lexicon */' >$@
	$(TAIL) +2 $(top_srcdir)/lexicon | \
	 cat - $(top_builddir)/custom.lexicon | \
	 $(AWK) -f $(srcdir)/tinit.awk >>$@

Nodebuilders:  $(top_srcdir)/grammar nodes.awk
	echo '/*  Generated from $(top_srcdir)/grammar  */' >$@
	$(GREP) '\$$\$$' $< | \
	 $(GREP) ';' | \
	 $(AWK) '{print $$4}' | \
	 $(SED) -e 's,;,,' | \
	 $(GREP) '(\$$1' | \
	 $(AWK) -f $(srcdir)/nodes.awk | \
	 $(SORT) | $(UNIQ) >>$@

Nodetypes:  $(top_srcdir)/grammar
	echo '  /* Generated from $(top_srcdir)/grammar */' >$@
	$(GREP) '\$$\$$' $< | \
	 $(GREP) ';' | \
	 $(AWK) '{print $$4}' | \
	 $(GREP) '(\$$1' | \
	 $(SED) -e 's/(.*$$/,/;s/^/  N_/' | \
	 $(SORT) | $(UNIQ) >>$@

Optypes:  $(top_srcdir)/opcodes
	echo '  /* Generated from $(top_srcdir)/opcodes */' >$@
	$(SED) -e 's,#.*$$,,' <$< | \
	 $(AWK) 'NF > 0 {print "  " $$1 "_op,"}' >>$@

Sysrot.inits:  $(top_srcdir)/sysrots $(top_builddir)/custom.sysrots sysrots.awk
	echo '  /* Generated from $(top_srcdir)/sysrots and $(top_builddir)/custom.sysrots */' >$@
	cat $(top_srcdir)/sysrots $(top_builddir)/custom.sysrots | \
	 $(AWK) -f $(srcdir)/sysrots.awk >>$@

String.opcodes:  $(top_srcdir)/opcodes
	echo '  /* Generated from $(top_srcdir)/opcodes */' >$@
	$(SED) -e 's,#.*$$,,' <$< | \
	 $(AWK) 'NF > 0 {print "  \"" $$1 "\","}' >>$@

Obj.header:  $(top_srcdir)/header
	echo '  /* Generated from $(top_srcdir)/header */' >$@
	$(SED) -e 's/\"/\\"/g' <$< | \
	 $(AWK) '{print "  \"" $$0 "\","}' >>$@

Debug.tcodes:  y.tab.h
	echo '/*  Generated from y.tab.h  */' >$@
	$(AWK) '/^#define / {printf "\"%s\",\n", $$2}' <$< >>$@

Debug.nodetypes:  Nodetypes
	echo '  /* Generated from Nodetypes */' >$@
	$(TAIL) +3 $< | \
	 $(SED) -e 's/N_/"N_/;s/,$$/",/' >>$@


## Makefile.am ends here
