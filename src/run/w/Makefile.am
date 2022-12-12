## Process this file with automake to generate Makefile.in
#
# $Id: Makefile.am,v 1.14 2022/12/11 17:09:27 setlorg Exp $
#
# Free software (c) dB  -  see file COPYING for license (GPL).
#
# Makefile.in -> Makefile via "configure" or "config.status" script.


# What we build here is the XW library, libXW.a.
#
# The _LTLIBRARIES suffix says use libtool for the build.  The use of
# libtool is why we point at the libtool control file here, libXW.la,
# rather than at the lib file (libXW.a).  The noinst_ prefix implies
# building just a static "convenience" (.a) lib, not a shared lib too.
#
noinst_LTLIBRARIES = libXW.la

libXW_la_SOURCES = W.h XW.c

libXW_la_CPPFLAGS = $(AM_CPPFLAGS) $(X_CFLAGS)
# Some X headers sometimes have identifiers named "class" or "new":
libXW_la_CFLAGS = $(AM_CFLAGS) -Wno-c++-compat
libXW_la_LDFLAGS = $(AM_LDFLAGS) $(X_LIBS)
libXW_la_LIBADD = $(X_PRE_LIBS) -lX11 $(X_EXTRA_LIBS)

## end of Makefile.am
