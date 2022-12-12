## Process this file with automake to generate Makefile.in
#
# $Id: Makefile.am,v 1.21 2017/09/28 03:32:33 setlorg Exp $
#
# Free software (c) dB  -  see file COPYING for license (GPL).
#
# Makefile.in -> Makefile via "configure" or "config.status" script.


# Please list here every subdir that generates custom.* files, in an
# order that (under concatenation of like-named custom.* files) puts
# all dependent definitions after their prerequisites, so that (for
# example) constants can be initialized in terms of other constants.
#
# Please start this line strictly with "SUBDIRS =" in the first column
# and keep it on one line, for primitive greppers to pick up:
#
SUBDIRS = mesa w

ACLOCAL_AMFLAGS = -I ../m4

DISTCLEANFILES = stamp-opt-config \
                 $(PACKAGE_TARNAME)-$(PACKAGE_VERSION).tar.gz


## Makefile.am ends here
