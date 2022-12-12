## Process this file with automake to produce Makefile.in

# $Id: Makefile.am,v 1.9 2017/12/08 03:19:16 setlorg Exp $

noinst_PROGRAMS = lorenz
lorenz_SOURCES = lorenz.c
lorenz_CPPFLAGS = $(AM_CPPFLAGS) -I$(top_srcdir)/include
lorenz_CFLAGS = $(AM_CFLAGS) $(X_CFLAGS) $(GCC_W_FLAGS)
lorenz_LDFLAGS = $(AM_LDFLAGS) $(X_LIBS) -export-dynamic
lorenz_LDADD = $(top_builddir)/src/libglut.la

## Makefile.am ends here
