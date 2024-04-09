dnl This file is processed by autoconf to produce a configure script.
dnl
dnl This "subproject" for GNU SETL documentation can be adequately
dnl managed by the top-level Makefile for casual builders.  Hard-core
dnl maintainers will also like the Makefile in the build dir for
dnl quick rebuilds.
dnl
AC_REVISION([$Id: configure.ac,v 1.17 2024/04/08 18:57:22 setlorg Exp $])
AC_COPYRIGHT([[Free software (c) dB - see file COPYING for license (GPL).]])

AC_PREREQ([2.69])

dnl Get VERSION_NUMBER defined.
m4_include([version.m4])

dnl The 4th arg must agree with the setting of pkgname in ../main.mk:
AC_INIT([GNU SETL doc],
        [VERSION_NUMBER],
        [David.Bacon@nyu.edu],
        [setl],
        [https://setl.org])

AM_INIT_AUTOMAKE([1.15 gnu --warnings=all])

AC_CONFIG_FILES([Makefile])
AC_OUTPUT
