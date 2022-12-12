## Process this file with automake to generate Makefile.in
#
# $Id: Makefile.am,v 1.82 2021/01/03 20:18:56 setlorg Exp $
#
# Free software (c) dB  -  See file COPYING for license (GPL).
#
# Makefile.in -> Makefile via "configure" or "config.status" script.


# Suppress warnings caused by the GNU Make extensions used in the
# custom_* definitions below:
AUTOMAKE_OPTIONS = -Wno-portability

# Our job here is mainly to drive builds in these subdirs:
SUBDIRS = cpp tran run tests

# These are the subdirs that are included in GNU SETL distributions:
DIST_SUBDIRS = $(SUBDIRS) opt

# Make sure 'aclocal' will see the macros in the m4/ subdir:
ACLOCAL_AMFLAGS = -I m4

# Definitions used by programs in subdirs:
dist_noinst_HEADERS = common.h

# Misc. local scripts:
dist_noinst_SCRIPTS = pax-avail run-setl

EXTRA_DIST = lexicon grammar sysrots header opcodes

# Files that need to be built before their client sources are used:
BUILT_SOURCES = custom.lexicon \
                custom.sysrots \
                custom.setl

# These and things like .o files will be deleted on a "make clean":
CLEANFILES = $(BUILT_SOURCES)

# To be deleted (along with many other files) on a "make distclean";
# the .tar.gz file can be left over by an interrupted "make dist",
# and stamp-config-src is created by ../main.mk to witness the
# creation of Makefiles in the build src/ tree:
DISTCLEANFILES = stamp-config-src \
                 install.log \
                 $(PACKAGE_TARNAME)-$(PACKAGE_VERSION).tar.gz

# CUSTOM_PAX is normally defined by a "configure" script.
custom_dirs = $(addprefix $(top_builddir)/opt/,$(CUSTOM_PAX))

custom_lexicon_files = $(addsuffix /custom.lexicon,$(custom_dirs))
custom_sysrots_files = $(addsuffix /custom.sysrots,$(custom_dirs))
custom_setl_files    = $(addsuffix /custom.setl,   $(custom_dirs))

# Makefile changes may reflect CUSTOM_PAX changes and thus changes to
# the contents of the $(custom_*_files) variables, each of which lists
# some subset of available files.  Thus we track changes to the lists
# themselves as well as changes to the files they name in these rules:

custom.lexicon:  $(custom_lexicon_files) Makefile
	cat $(custom_lexicon_files) /dev/null >$@

custom.sysrots:  $(custom_sysrots_files) Makefile
	cat $(custom_sysrots_files) /dev/null >$@

custom.setl:  $(custom_setl_files) Makefile
	echo '-- customization package prosthetics --' >$@
	cat $(custom_setl_files) /dev/null >>$@

# The logging in the local install and uninstall rule extensions below
# only applies to "make install" and "make uninstall" issued at the
# dir level of this Makefile and above.

destbin = $(DESTDIR)$(bindir)

install-exec-local:
	@echo "`date` - GNU SETL executables installed in $(destbin)" \
	 >>install.log

uninstall-local:
	@echo "`date` - GNU SETL executables removed from $(destbin)" \
	 >>install.log


## Makefile.am ends here
