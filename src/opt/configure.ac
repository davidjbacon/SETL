dnl This file is processed by autoconf to produce a configure script.
dnl
dnl This "subproject" for maintaining optional customization packages
dnl is generally managed/driven by the very top-level Makefile.
dnl
AC_REVISION([$Id: configure.ac,v 1.26 2021/07/12 20:19:58 setlorg Exp $])
AC_COPYRIGHT([[Free software (c) dB - see file COPYING for license (GPL).]])

AC_PREREQ([2.69])

# Get VERSION_NUMBER defined.
m4_include([../version.m4])

dnl The 4th arg (TARNAME) to AC_INIT is effectively a placeholder
dnl (nothing is meant to be distributed or installed directly from this
dnl subproject, though "make dist" gives setl-opt-*.tar.gz):
AC_INIT([GNU SETL opt pax],
        VERSION_NUMBER,
        [David.Bacon@nyu.edu],
        [setl-opt],
        [https://setl.org])

AC_CONFIG_MACRO_DIR([../m4])

AM_INIT_AUTOMAKE([1.15 foreign no-define --warnings=all])

AC_CANONICAL_HOST

# Set CC on QNX 6 to the appropriate compiler invocation string.
CC_QNX6

AC_PROG_CC
AC_PROG_CPP
AC_PROG_EGREP

# Default commands.
MY_CMD([GREP],[grep])
MY_CMD([SED],[sed])
MY_CMD([SORT],[sort])
MY_CMD([UNIQ],[uniq])
MY_CMD([SETL],[setl])

# Command to get the C preprocessor to emit #define lines for all the
# macros in a given program, recursively including those brought in
# by #include.  The form "-Wp,-dM" works for both gcc and QNX's qcc,
# whereas "-dM" only works for gcc.
# TODO:  generalize this to non-GCC-based preprocessors.
#CPP_DEFINES="$CPP -dM"
CPP_DEFINES="$CPP -Wp,-dM"
AC_SUBST([CPP_DEFINES])

# Make libtool available, though no one in this subtree is currently
# using it.
LT_INIT

# Get X_CFLAGS defined for the sake of mesa.
AC_PATH_XTRA

# Add -lm to LIBS if that's where the math functions seem to live.
AC_SEARCH_LIBS([atan2],[m])

# Emit the Makefiles.
AC_CONFIG_FILES([Makefile
                 mesa/Makefile
                 w/Makefile])
AC_OUTPUT
