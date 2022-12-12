dnl This file is expanded by autoconf into a 'configure' script.
dnl
dnl Several programs may read this file, most notably aclocal, autoconf,
dnl automake, autoheader (which produces config.h.in), and libtoolize,
dnl all typically via autoreconf.
dnl
dnl This "subproject" for maintaining the GNU SETL executables is
dnl generally managed/driven by the top-level Makefile (one dir up).
dnl
AC_REVISION([$Id: configure.ac,v 1.117 2022/11/17 14:16:07 setlorg Exp $])
AC_COPYRIGHT([[Free software (c) dB - see file COPYING for license (GPL).]])

AC_PREREQ([2.69])

dnl Get VERSION_NUMBER defined.
m4_include([version.m4])

dnl The 4th arg must agree with the setting of pkgname in ../main.mk:
AC_INIT([GNU SETL],
        VERSION_NUMBER,
        [David.Bacon@nyu.edu],
        [setl],
        [https://setl.org])

# Get $host and components $host_cpu, $host_vendor, $host_os by having
# config.sub canonicalize $host_alias (which came from --host).
dnl We don't currently need $build et al., so we're not also invoking
dnl AC_CANONICAL_BUILD here, but there is no compelling reason not to.
AC_CANONICAL_HOST

dnl Sanity check that srcdir (e.g., . or --srcdir) contains this file:
AC_CONFIG_SRCDIR([common.h])

dnl The configure scripts in these subdirs are run by ../main.mk
dnl individually, not as part of a recursive call.  This
dnl AC_CONFIG_SUBDIRS, however, gives you the alternative option of
dnl driving the sub-configures from here.  And when autoreconf --install
dnl is done here, this causes it to be done in these subdirs too:
AC_CONFIG_SUBDIRS([run/freeglut-setl opt])

dnl Plan for future versions of the auto-tools:
AC_CONFIG_MACRO_DIR([m4])

dnl The no-define here is because we don't want a #define for PACKAGE
dnl (the parser already uses that as the name of a certain SETL token):
AM_INIT_AUTOMAKE([1.15 gnu std-options no-define --warnings=all])

dnl Emit substitutable PACKAGE_... symbol definitions:
MY_FABULOUS_PACKAGE

dnl Autoheader creates config.h.in from which config.status will create
dnl config.h:
AC_CONFIG_HEADERS([config.h])

# Help configure-generated test programs find "common.h" even when
# configure is invoked by a make distcheck.  Note that $ac_compile,
# which is used for test compilations in the configure script,
# incorporates $CPPFLAGS but not $AM_CPPFLAGS.
export CPPFLAGS="-I$srcdir $CPPFLAGS"
AC_MSG_NOTICE(["CPPFLAGS = $CPPFLAGS"])

dnl Make use of such OS extensions as are available:
AC_USE_SYSTEM_EXTENSIONS

# Set CC on QNX 6 to the appropriate compiler invocation string.
CC_QNX6

dnl Enable large-file support (64-bit file offsets) if available.
dnl This should also make off_t be 64 bits and lseek() mean lseek64():
AC_SYS_LARGEFILE

dnl Checks for programs:
AC_PROG_LN_S
AC_PROG_INSTALL
AC_PROG_CC
AC_PROG_CPP
AC_PROG_YACC
AC_PROG_AWK
AC_PROG_EGREP

dnl Support "unusual" archivers, as suggested by a warning once upon a time:
AM_PROG_AR

dnl Default commands:
MY_CMD([GREP],[grep])
MY_CMD([SED],[sed])
MY_CMD([SORT],[sort])
MY_CMD([UNIQ],[uniq])

dnl Enable libtool support, disabling shared library generation by default:
LT_INIT([disable-shared])

dnl Test for GCC:
AM_CONDITIONAL([COND_GCC],
               [test x"$GCC" = xyes])

if test x"$GCC" = xyes; then
  # Init "AM_CFLAGS = <warning flags>" in all Makefiles we generate:
  case $host in
  *-*-nto-qnx6.0.* | *-*-nto-qnx6.1.* | *-*-nto-qnx6.2.* | *-*-nto-qnx6.3.* | *-*-nto-qnx6.4.* | *-*-nto-qnx6.5.0)
    AC_SUBST([AM_CFLAGS], ['-Wall -Wcast-qual -Wshadow -Wpointer-arith -Wbad-function-cast -Wcast-align -Wwrite-strings -Wstrict-prototypes -Wmissing-prototypes -Wmissing-declarations -Wmissing-format-attribute -Wno-unused-parameter'])
    ;;
  *)
    AC_SUBST([AM_CFLAGS], ['-Wall -Wextra -Wcast-qual -Wc++-compat -Wshadow -Wpointer-arith -Wbad-function-cast -Wcast-align -Wwrite-strings -Wstrict-prototypes -Wmissing-prototypes -Wmissing-declarations -Wmissing-format-attribute -Wno-unused-parameter'])
    ;;
  esac
fi

# Prepare for "#pragma GCC diagnostic..." tests below.
saved_CFLAGS=$CFLAGS
CFLAGS="$CFLAGS -Wall -Werror"

dnl Check for "#pragma GCC diagnostic" support:
AH_TEMPLATE([HAVE_PRAGMA_GCC_DIAGNOSTIC],
            [Define to 1 if "#pragma GCC diagnostic" is supported.])
AC_MSG_CHECKING([[for "#pragma GCC diagnostic" support]])
AC_COMPILE_IFELSE([AC_LANG_SOURCE(
  [[#pragma GCC diagnostic warning "-Wsign-compare"]])],
  [AC_DEFINE([HAVE_PRAGMA_GCC_DIAGNOSTIC], [1])
   have_pragma_gcc_diag=yes],
  [have_pragma_gcc_diag=no])
AC_MSG_RESULT([$have_pragma_gcc_diag])
AM_CONDITIONAL([COND_PRAGMA_GCC_DIAGNOSTIC],
               [test "$have_pragma_gcc_diag" = yes])

dnl Check for "#pragma GCC diagnostic push/pop" support:
AH_TEMPLATE([HAVE_PRAGMA_GCC_DIAGNOSTIC_PUSHPOP],
            [Define to 1 if "#pragma GCC diagnostic {push|pop}" is supported.])
AC_MSG_CHECKING([[for "#pragma GCC diagnostic {push|pop}" support]])
AC_COMPILE_IFELSE([AC_LANG_SOURCE(
  [[#pragma GCC diagnostic push]])],
  [AC_DEFINE([HAVE_PRAGMA_GCC_DIAGNOSTIC_PUSHPOP], [1])
   have_pragma_gcc_diag_pushpop=yes],
  [have_pragma_gcc_diag_pushpop=no])
AC_MSG_RESULT([$have_pragma_gcc_diag_pushpop])
AM_CONDITIONAL([COND_PRAGMA_GCC_DIAGNOSTIC_PUSHPOP],
               [test "$have_pragma_gcc_diag_pushpop" = yes])

# Restore CFLAGS after "#pragma GCC diagnostic..." tests above.
CFLAGS=$saved_CFLAGS

dnl Check for __attribute__ ((noreturn)) support:
AH_TEMPLATE([HAVE_ATTRIBUTE_NORETURN],
     [Define to 1 if __attribute__ ((noreturn)) is supported.])
AC_MSG_CHECKING([[for __attribute__ ((noreturn)) support]])
AC_COMPILE_IFELSE([AC_LANG_SOURCE(
  [[void f(void) __attribute__ ((noreturn));]])],
  [AC_DEFINE([HAVE_ATTRIBUTE_NORETURN], [1])
   have_attribute_noreturn=yes],
  [have_attribute_noreturn=no])
AC_MSG_RESULT([$have_attribute_noreturn])

dnl Check for __builtin_expect support:
AH_TEMPLATE([HAVE_BUILTIN_EXPECT],
     [Define to 1 if __builtin_expect is supported.])
AC_MSG_CHECKING([[for __builtin_expect support]])
AC_COMPILE_IFELSE([AC_LANG_SOURCE(
  [[void f(void) {__builtin_expect(0,0);}]])],
  [AC_DEFINE([HAVE_BUILTIN_EXPECT], [1])
   have_builtin_expect=yes],
  [have_builtin_expect=no])
AC_MSG_RESULT([$have_builtin_expect])

dnl Check for __builtin_bswap{16,32,64} support:
AH_TEMPLATE([HAVE_BUILTIN_BSWAP],
     [Define to 1 if __builtin_bswap{16,32,64} are supported.])
AC_MSG_CHECKING([[for __builtin_bswap{16,32,64} support]])
AC_COMPILE_IFELSE([AC_LANG_SOURCE(
  [[void f(void) {__builtin_bswap16(0);
                  __builtin_bswap32(0);
                  __builtin_bswap64(0);}]])],
  [AC_DEFINE([HAVE_BUILTIN_BSWAP], [1])
   have_builtin_bswap=yes],
  [have_builtin_bswap=no])
AC_MSG_RESULT([$have_builtin_bswap])

dnl Check for __attribute__ ((format (printf, M, N))) support:
AH_TEMPLATE([HAVE_ATTRIBUTE_FORMAT_PRINTF],
     [Define to 1 if __attribute__ ((format (printf, M, N))) is supported.])
AC_MSG_CHECKING([[for __attribute__ ((format (printf, M, N))) support]])
AC_COMPILE_IFELSE([AC_LANG_SOURCE(
  [[void f(const char *s, ...) __attribute__ ((format (printf, 1, 2)));]])],
  [AC_DEFINE([HAVE_ATTRIBUTE_FORMAT_PRINTF], [1])
   have_attribute_format_printf=yes],
  [have_attribute_format_printf=no])
AC_MSG_RESULT([$have_attribute_format_printf])

dnl Check for __attribute__ ((aligned (N))) support:
AH_TEMPLATE([HAVE_ATTRIBUTE_ALIGNED],
     [Define to 1 if __attribute__ ((aligned (N))) is supported.])
AC_MSG_CHECKING([[for __attribute__ ((aligned (N))) support]])
AC_COMPILE_IFELSE([AC_LANG_SOURCE(
  [[struct t {int i;} __attribute__ ((aligned (8)));]])],
  [AC_DEFINE([HAVE_ATTRIBUTE_ALIGNED], [1])
   have_attribute_aligned=yes],
  [have_attribute_aligned=no])
AC_MSG_RESULT([$have_attribute_aligned])

dnl Check for --with-bundled-gmp arg.  Reporting is later, when
dnl $with_bundled_gmp is decided yes or no:
AC_ARG_WITH([bundled-gmp],
            [AS_HELP_STRING([--with-bundled-gmp],
                            [use the GNU MP bundled with GNU SETL])],
            [],
            [with_bundled_gmp=unknown])

dnl This doesn't work, as the 1st arg to AC_ARG_WITH gets turned into
dnl refs to "${with_$pak}" then.  So read on...
dnl CUSTOM_PAX=
dnl for pak in `"$srcdir/pax-avail" "$srcdir/opt"`; do
dnl   whatami=`cat "$srcdir/opt/$pak/whatami"`
dnl   AC_MSG_CHECKING([for $pak - $whatami])
dnl   AC_ARG_WITH([$pak], [AS_HELP_STRING([--with-$pak], ["$whatami"])],
dnl               [AS_IF([test x"$withval" != xno],
dnl                      [CUSTOM_PAX="$CUSTOM_PAX $pak"])],
dnl               [withval=no])
dnl   AC_MSG_RESULT([$withval])
dnl done

dnl Macro to check for --with-{pak} passed to the configure script.
dnl It also adds a line to the --help output describing --with-{pak},
dnl getting the right-hand side of that description from the "whatami"
dnl file associated with {pak}:
AC_DEFUN([WITH_PAK],
[dnl $1 is {pak}
m4_pushdef([whatami], m4_esyscmd_s([cat "opt/$1/whatami"]))dnl
AC_MSG_CHECKING([for $1 - ]whatami)
AC_ARG_WITH([$1],
            [AS_HELP_STRING([--with-$1], whatami)],
            [AS_IF([test x"$withval" != xno],
                   [CUSTOM_PAX="$CUSTOM_PAX $1"])],
            [withval=no])
AC_MSG_RESULT([$withval])
m4_popdef([whatami])dnl
])dnl

dnl Checks for SETL extension ("customization") package selections:
CUSTOM_PAX=
dnl This "foreach" loop is elaborated at autoconf time, and looks for a
dnl variable named with_{pak} for each {pak} in the output of pax-avail;
dnl that variable will have been set if --with-{pak} was passed to
dnl configure.  So there is technically an untracked dependency of the
dnl configure script on pax-avail's input, Makefile.am.  It only matters
dnl if the SUBDIRS line in Makefile.am changes, and in that case there
dnl will almost certainly be changes here too.  It were perhaps mildly
dnl preferable to have this check at configure time, in a shell loop
dnl like the one we wanted above, say through the offices of some minor
dnl eval contortions in a variant of AC_ARG_WITH:
m4_foreach_w([pak], m4_esyscmd_s([./pax-avail ./opt]),
             [WITH_PAK(m4_defn([pak]))])
dnl Summarize the selections, a bit redundantly:
AC_MSG_CHECKING([SETL extension (customization) package selections])
AS_IF([test x"$CUSTOM_PAX" != x],
      [selected_custom_pax=`echo $CUSTOM_PAX`],
      [selected_custom_pax='(none)'])
AC_MSG_RESULT([$selected_custom_pax])
AC_SUBST([CUSTOM_PAX])

dnl Need one of these for each customization package that requires
dnl conditional building of something else, such as a library that
dnl it depends on:
AM_CONDITIONAL([COND_W],
               [test x"$with_w" != x && test x"$with_w" != xno])
AM_CONDITIONAL([COND_MESA],
               [test x"$with_mesa" != x && test x"$with_mesa" != xno])

dnl Checks for header files:
dnl This macro is now said to be obsolete; in any case it was just
dnl checking for the most basic Unix headers, and given the coverage
dnl below (and somewhere above, which is triggering checks for stdio.h
dnl and all that crowd anyway), why bother:
dnl AC_HEADER_STDC
AC_HEADER_SYS_WAIT
AC_HEADER_STAT
AC_HEADER_STDBOOL
dnl This arg to AC_CHECK_HEADERS has to be literal rather than come from
dnl a variable, so that autoheader can pick up the list of headers:
AC_CHECK_HEADERS([arpa/inet.h assert.h bsd/sgtty.h errno.h fcntl.h \
 float.h glob.h grp.h inttypes.h limits.h locale.h malloc.h memory.h \
 netdb.h netinet/in.h netinet/in_systm.h netinet/tcp.h regex.h \
 signal.h stdarg.h stddef.h stdint.h stdlib.h string.h strings.h \
 stropts.h termio.h termios.h sys/ioctl.h sys/param.h sys/ptem.h \
 sys/resource.h sys/rusage.h /usr/ucbinclude/sys/rusage.h \
 sys/select.h sys/signal.h sys/socket.h sys/time.h sys/timeb.h \
 sys/uio.h linux/uio.h sys/un.h sys/utsname.h sys/neutrino.h \
 unistd.h endian.h byteswap.h gulliver.h libkern/OSByteOrder.h])

AS_IF([test x"$with_bundled_gmp" != xyes],
  [AC_CHECK_HEADERS([gmp.h],
    [with_bundled_gmp=no],
    [AS_IF([test x"$with_bundled_gmp" = xunknown],
      [with_bundled_gmp=yes],
      [AC_MSG_ERROR([bundled GNU MP declined and <gmp.h> not found])])])],
  [])
AC_MSG_CHECKING([whether to use the GNU MP library bundled with GNU SETL])
AC_MSG_RESULT([$with_bundled_gmp])

AM_CONDITIONAL([USE_BUNDLED_GMP],
               [test x"$with_bundled_gmp" = xyes])

dnl Add -I options to X_CFLAGS, and linker options such as -L to X_LIBS.
dnl Also set X_PRE_LIBS and X_EXTRA_LIBS as appropriate.  This is in
dnl case we need to compile with X headers and link with X libs for
dnl customization, and because run/w/, whose Makefile we generate,
dnl uses in its Makefile.am the X_* definitions established by this:
AC_PATH_XTRA

dnl Checks for libraries, in some partial order of least to most dependent.
dnl The current set probably has no such intradependencies.  Other lib deps
dnl may occur due to customization, and are nicely tracked in .la files by
dnl libtool with the help of our *_LIBADD settings in Makefile.am files:
AC_SEARCH_LIBS([atan2],[m])  dnl not [cos], 'cos cos chokes on SunOS 5.4
AC_SEARCH_LIBS([timer_settime],[rt])
AC_SEARCH_LIBS([socket],[socket])
AS_IF([test x"$with_bundled_gmp" != xyes],
  [AC_SEARCH_LIBS([__gmpz_init],[gmp],[], dnl add -lgmp to LIBS
    [AC_MSG_ERROR([cannot find installed GNU MP library (libgmp)])])],
  [])

dnl Checks for typedefs, structures, and other C platform characteristics:
AC_C_RESTRICT
AC_C_INLINE
AC_C_TYPEOF
AC_C_FLEXIBLE_ARRAY_MEMBER
AC_C_BIGENDIAN
AC_TYPE_MODE_T
AC_TYPE_OFF_T
AC_TYPE_PID_T
AC_TYPE_SIZE_T
AC_TYPE_UID_T
AC_TYPE_INT64_T
AC_TYPE_UINT64_T
dnl AC_HEADER_TIME
AC_STRUCT_TM
AC_CHECK_TYPES([ptrdiff_t])
AC_CHECK_TYPES([socklen_t], [], [], [[#include <sys/socket.h>]])
AC_TYPE_LONG_LONG_INT
AC_TYPE_UNSIGNED_LONG_LONG_INT

dnl Checks for library functions:
AC_FUNC_ALLOCA
AC_FUNC_CHOWN
AC_FUNC_ERROR_AT_LINE
AC_FUNC_FORK
AC_FUNC_GETGROUPS
AC_FUNC_LSTAT
AC_FUNC_LSTAT_FOLLOWS_SLASHED_SYMLINK
dnl Don't want these 2 checkers, as we never call malloc() or realloc()
dnl with a 0 arg anyway, and Autoconf makes pessimistic assumptions for
dnl cross-compilation, resulting in unwanted references to rpl_malloc()
dnl and rpl_realloc() respectively:
dnl AC_FUNC_MALLOC
dnl AC_FUNC_REALLOC
AC_FUNC_MEMCMP
AC_FUNC_MMAP
AC_FUNC_OBSTACK
AC_FUNC_SELECT_ARGTYPES
AC_FUNC_STAT
AC_FUNC_STRFTIME
AC_FUNC_STRTOD
AC_FUNC_VPRINTF
dnl Literal arg needed here as for AC_CHECK_HEADERS:
AC_CHECK_FUNCS([clock_gettime ftime \
                timer_gettime timer_settime timer_create timer_delete \
                getitimer setitimer \
                sysconf getrlimit getdtablesize getrusage gettimeofday \
                posix_openpt grantpt ptsname unlockpt \
                freeaddrinfo getaddrinfo getnameinfo \
                memcpy memmove memset \
                pselect recvmsg sendmsg socketpair \
                strchr strcasecmp strnlen strerror \
                setenv unsetenv])

dnl Checks for finding some other structs in or via specific headers:

AH_TEMPLATE([STRUCT_RUSAGE_IN_SYS_RESOURCE_H],
            [Define if <sys/resource.h> declares 'struct rusage'
             (otherwise we need to get it from some exotic BSD
             compatibility directory).])
MY_STRUCT_IN_HEADER([rusage], [sys/resource.h])

AH_TEMPLATE([STRUCT_TIMEVAL_IN_TIME_H],
            [Define if <time.h> declares 'struct timeval'.])
MY_STRUCT_IN_HEADER([timeval], [time.h])

AH_TEMPLATE([STRUCT_TIMEVAL_IN_SYS_TIME_H],
            [Define if <sys/time.h> declares 'struct timeval'.])
MY_STRUCT_IN_HEADER([timeval], [sys/time.h])

AH_TEMPLATE([STRUCT_TIMEVAL_IN_LINUX_TIME_H],
            [Define if <linux/time.h> declares 'struct timeval'.])
MY_STRUCT_IN_HEADER([timeval], [linux/time.h])

dnl Checks for declarations of library functions.  These are after
dnl the checks for headers, since we need "confdefs.h" to have all
dnl the appropriate HAVE_headername_H definitions already:
AC_CHECK_DECLS([sysconf, getrlimit, getdtablesize,
                posix_openpt, grantpt, ptsname, unlockpt,
                pselect,
                strcasecmp, strnlen], , ,
               [#include "common.h"])

dnl Checks for structure members (fields):
AC_CHECK_MEMBERS([struct cmsghdr.cmsg_len,
                  struct msghdr.msg_accrights],,,
                 [#include <sys/socket.h>])

dnl Checks for the sizes of certain types:
AC_CHECK_SIZEOF([void *])
AC_CHECK_SIZEOF([char *])
AC_CHECK_SIZEOF([int])
AC_CHECK_SIZEOF([long])
AC_CHECK_SIZEOF([long long])
AC_CHECK_SIZEOF([double])
AC_CHECK_SIZEOF([off_t])
AC_CHECK_SIZEOF([size_t])
AC_CHECK_SIZEOF([clock_t])

dnl Checks for the alignment requirements (or at least preferences)
dnl of certain types:
AC_CHECK_ALIGNOF([double])
AC_CHECK_ALIGNOF([void *])
AC_CHECK_ALIGNOF([long long])
AC_CHECK_ALIGNOF([long])
AC_CHECK_ALIGNOF([int])

dnl Have config.status create or rewrite these Makefiles:
AC_CONFIG_FILES([Makefile
             cpp/Makefile
            tran/Makefile
             run/Makefile
           run/w/Makefile
           tests/Makefile])

dnl Generate config.status and run it:
AC_OUTPUT
