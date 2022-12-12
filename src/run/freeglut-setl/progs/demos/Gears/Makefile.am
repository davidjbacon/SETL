## Process this file with automake to produce Makefile.in

# $Id: Makefile.am,v 1.2 2017/10/01 20:59:06 setlorg Exp $

noinst_PROGRAMS = gears
gears_SOURCES = gears.c
gears_CPPFLAGS = $(AM_CPPFLAGS) -I$(top_srcdir)/include
gears_CFLAGS = $(AM_CFLAGS) $(X_CFLAGS) $(GCC_W_FLAGS)
gears_LDFLAGS = $(AM_LDFLAGS) $(X_LIBS) -export-dynamic
gears_LDADD = $(top_builddir)/src/libglut.la

## Makefile.am ends here
