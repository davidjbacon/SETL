## Process this file with automake to produce Makefile.in

# $Id: Makefile.am,v 1.11 2017/12/08 03:19:16 setlorg Exp $

EXTRA_DIST = fractals.dat

noinst_PROGRAMS = fractals
fractals_SOURCES = fractals.c
fractals_CPPFLAGS = $(AM_CPPFLAGS) -I$(top_srcdir)/include
fractals_CFLAGS = $(AM_CFLAGS) $(X_CFLAGS) $(GCC_W_FLAGS)
fractals_LDFLAGS = $(AM_LDFLAGS) $(X_LIBS) -export-dynamic
fractals_LDADD = $(top_builddir)/src/libglut.la

## Makefile.am ends here
