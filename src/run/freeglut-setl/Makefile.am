## Process this file with automake to produce Makefile.in

# $Id: Makefile.am,v 1.8 2017/09/28 03:32:33 setlorg Exp $

SUBDIRS = src progs doc

AUTOMAKE_OPTIONS = gnu

ACLOCAL_AMFLAGS = -I ../../m4

EXTRA_DIST = \
 README.SETL               \
 include/GL/freeglut.h     \
 include/GL/freeglut_ext.h \
 include/GL/glut.h

DISTCLEANFILES = @freeglut_srclinks@ \
                 $(PACKAGE_TARNAME)-$(PACKAGE_VERSION).tar.gz

## Makefile.am ends here
