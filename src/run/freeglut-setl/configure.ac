dnl Process this file with autoconf to produce a configure script.
dnl
AC_REVISION([$Id: configure.ac,v 1.20 2021/02/07 02:40:37 setlorg Exp $])
AC_COPYRIGHT([[Free software - see file COPYING for license (GPL).]])

AC_PREREQ([2.69])

dnl The 4th arg (TARNAME) to AC_INIT is effectively a placeholder
dnl (nothing is meant to be distributed or installed directly from this
dnl subproject, though "make dist" gives setl-freeglut-2.0.1.tar.gz):
AC_INIT([GNU SETL freeglut],
        [2.0.1],
        [David.Bacon@nyu.edu],
        [setl-freeglut],
        [https://setl.org])

AC_CANONICAL_BUILD
AC_CANONICAL_HOST

AC_CONFIG_HEADERS([config.h])
AC_CONFIG_MACRO_DIR([../../m4])

AM_INIT_AUTOMAKE([1.15 gnu --warnings=all])

dnl Set CC on QNX 6 to the appropriate compiler invocation string:
CC_QNX6

dnl The addition of AM_PROG_AR here was prompted by this warning
dnl on some system or other:
dnl
dnl  'libglut.la': linking libtool libraries using a non-POSIX
dnl  archiver requires 'AM_PROG_AR' in 'configure.ac'
dnl  ... while processing Libtool library 'libglut.la'.
dnl
AM_PROG_AR
AC_PROG_CC
AC_PROG_LN_S

AC_C_CONST
AC_EXEEXT

dnl Enable the use of libtool.
dnl
dnl The original freeglut-2.0.1 configure.in had AC_LIBTOOL_DLOPEN
dnl before its AM_PROG_LIBTOOL (the old-fashioned pre-LT_INIT way),
dnl but I do not see the need to check for dlopen support, as dlopen
dnl is nowhere used by the package; otherwise the following should be
dnl LT_INIT([disable-shared dlopen]) or LT_INIT([dlopen]).
dnl
dnl I should note however that there is definitely some late binding
dnl going on when it comes to the loading (mapping) of libs like
dnl /usr/X11R7/lib/modules/dri/swrast_dri.so on NetBSD, so there may
dnl very well be dlopen()ing going on in the system libs.
dnl
dnl Anyway, since I don't really know why AC_LIBTOOL_DLOPEN was being
dnl used, and don't see any need to retract functionality that the
dnl original freeglut offers to some clients I don't know about (I
dnl note also that all the Makefile.am files in progs/demos/*/ take
dnl the trouble to specify the -export-dynamic link option), I'm going
dnl to let this mysterious ability in the package remain:
dnl
dnl  'dlopen'
dnl       Enable checking for dlopen support.  This option should be
dnl       used if the package makes use of the '-dlopen' and
dnl       '-dlpreopen' libtool flags, otherwise libtool will assume that
dnl       the system does not support dlopening.
dnl
dnl Furthermore, I don't see why the demos shouldn't link to a
dnl shared-object freeglut-setl lib, and thus have small executables.
dnl They get built whether you want them or not, and don't need to be
dnl freestanding.  It is preferable that the GNU SETL interpreter itself
dnl link with the static freeglut-setl lib, though.
dnl
dnl If building or using the shared lib causes any problems for you,
dnl feel free to add disable-shared to this LT_INIT call (or simply put
dnl --disable-shared on the ./configure command line if you are working
dnl locally), as the demos should build fine with the static lib too.
dnl Or even just go into the progs/demos/ build dir and use
dnl "LDFLAGS=-static" on the make command line.
dnl
LT_INIT([dlopen])

dnl Debian (including its popular derivative Ubuntu) patches libtool to
dnl set this variable to no (this happens in the code emitted by the
dnl LT_INIT call above), contrary to the libtool documentation (even
dnl that on Debian/Ubuntu).  That has the consequence that when the
dnl demo programs are linked by libtool, the dependency_libs line in
dnl libglut.la is ignored when the programs are linked with the dynamic
dnl version of the libglut library, causing the link to fail.
dnl
dnl This overrides that, and applies to all executables built in the
dnl freeglut-setl subtree (which is currently just the demos).  There
dnl are fussier, less "blanket" ways of achieving the same thing, but
dnl this is fine in this limited context:
dnl
link_all_deplibs=yes

dnl Set X_CFLAGS (with -I args despite the name) and X_LIBS (with -L args
dnl depite the name):
AC_PATH_XTRA

dnl Checks for libraries, in some partial order of least to most dependent.
dnl
dnl Add -lm to LIBS if appropriate:
AC_SEARCH_LIBS([atan2],[m])
dnl
dnl Even though freeglut does not use any pthreads calls directly,
dnl it can happen (as on NetBSD) that some libs it is linked with do
dnl (the latter lacking info to suggest the need to look for a pthread
dnl lib, I suppose, or I presumably wouldn't have bumped into that),
dnl so we add -lpthread to LIBS if appropriate:
AC_SEARCH_LIBS([pthread_create],[pthread])
dnl
dnl This -lsocket may be needed for the Xxf86vm lib detection to work:
AC_SEARCH_LIBS([socket],[socket])
dnl
dnl The original freeglut-2.0.1 configure.in had this around here:
dnl
dnl AC_CHECK_LIB(Xxf86vm,XF86VidModeSwitchToMode,
dnl 	LIBXXF86VM=-lXxf86vm,LIBXXF86VM=,$X_LIBS -lXext -lX11)
dnl AC_SUBST(LIBXXF86VM)
dnl
dnl We continue to use AC_CHECK_LIB rather than AC_SEARCH_LIBS for this
dnl case, because we don't want it to prepend anything to LIBS when it
dnl succeeds, but do want it to define LIBXXF86VM so that we can put
dnl $(LIBXXF86VM) in just the right place in the definition of
dnl libglut_la_LIBADD in src/Makefile.am.
dnl
dnl Since X_LIBS, despite the name, just has loader flags in it, we
dnl add it to LDFLAGS temporarily rather than stuffing it into the last
dnl arg to AC_CHECK_LIB:
saved_LDFLAGS=$LDFLAGS
LDFLAGS="$LDFLAGS $X_LIBS"
AC_CHECK_LIB([Xxf86vm],[XF86VidModeSwitchToMode],
             [LIBXXF86VM=-lXxf86vm],
             [LIBXXF86VM=],
             [-lXext -lX11])
AC_SUBST([LIBXXF86VM])
LDFLAGS=$saved_LDFLAGS

dnl Check for standard system header files:
AC_HEADER_STDC

dnl Check for a couple of X video mode extension headers:
saved_CPPFLAGS=$CPPFLAGS
CPPFLAGS="$CPPFLAGS $X_CFLAGS"
AC_CHECK_HEADERS([X11/extensions/xf86vmode.h \
                  X11/extensions/xf86vmproto.h],,,[#include <X11/Xlib.h>])
CPPFLAGS=$saved_CPPFLAGS

# GCC_W_FLAGS is used in some of the demo Makefile.am files to
# reduce the number of warnings from GCC, particularly on fgets()
# calls where the function has attribute warn_unused_result.
AC_MSG_CHECKING([[for -Wno-unused-result compilation flag]])
GCC_W_FLAGS=
saved_CFLAGS=$CFLAGS
CFLAGS="$CFLAGS -Wno-unused-result"
AC_COMPILE_IFELSE([AC_LANG_SOURCE([])],
  [wf_no_unused_result=yes
   GCC_W_FLAGS="$GCC_W_FLAGS -Wno-unused-result"],
  [wf_no_unused_result=no])
AC_MSG_RESULT([$wf_no_unused_result])
AC_SUBST(GCC_W_FLAGS)
CFLAGS=$saved_CFLAGS

# In VPATH mode ($srcdir not .), have config.status make links for
# data files that certain demos expect to find in the same dir as the
# executable.  Also set freeglut_srclinks to the list of created links,
# so that @freeglut_srclinks@ can be in DISTCLEANFILES in Makefile.am
# and thus deleted on a "make distclean".
freeglut_srclinks=
if test . != "$srcdir"; then
  for f in progs/demos/Fractals/fractals.dat \
           progs/demos/Fractals_random/fractals.dat; do
    AC_CONFIG_LINKS([$f:$f])
    freeglut_srclinks="$freeglut_srclinks $f"
  done
fi
AC_SUBST([freeglut_srclinks])

dnl Have config.status create or rewrite these Makefiles:
AC_CONFIG_FILES([Makefile \
             doc/Makefile \
             src/Makefile \
           progs/Makefile \
           progs/demos/Makefile \
           progs/demos/Fractals/Makefile \
           progs/demos/Fractals_random/Makefile \
           progs/demos/Lorenz/Makefile \
           progs/demos/One/Makefile \
           progs/demos/Gears/Makefile])

dnl Generate config.status and run it:
AC_OUTPUT
