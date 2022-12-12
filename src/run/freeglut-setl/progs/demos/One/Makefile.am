## Process this file with automake to produce Makefile.in

# $Id: Makefile.am,v 1.9 2017/12/08 03:19:16 setlorg Exp $

noinst_PROGRAMS = one
one_SOURCES = one.c
one_CPPFLAGS = $(AM_CPPFLAGS) -I$(top_srcdir)/include
one_CFLAGS = $(AM_CFLAGS) $(X_CFLAGS)
one_LDFLAGS = $(AM_LDFLAGS) $(X_LIBS) -export-dynamic
one_LDADD = $(top_builddir)/src/libglut.la

## Makefile.am ends here
