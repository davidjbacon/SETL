dnl  Local macro definitions

dnl  $Id: my.m4,v 1.13 2017/09/20 20:27:01 setlorg Exp $

dnl  Free software (c) dB - see file COPYING for license (GPL).

dnl Look for a particular struct $1 in or via a specific header $2:
AC_DEFUN([MY_STRUCT_IN_HEADER],
[ac_safe=`echo "$2" | sed 'y%./+-%__p_%'`
AC_MSG_CHECKING([for struct $1 via $2])
AC_CACHE_VAL(ac_cv_struct_$1_in_$ac_safe,
[AC_EGREP_HEADER(dnl
changequote(<<,>>)dnl
<<struct[ 	]*$1[^a-zA-Z_0-9]>>dnl
changequote([,]),
$2,
eval "ac_cv_struct_$1_in_$ac_safe=yes",
eval "ac_cv_struct_$1_in_$ac_safe=no")])
if eval "test \"`echo '$ac_cv_struct_'$1'_in_'$ac_safe`\" = yes"; then
  ac_struct_in_hdr=STRUCT_`echo $1 | sed 'y%abcdefghijklmnopqrstuvwxyz%ABCDEFGHIJKLMNOPQRSTUVWXYZ%'`_IN_`echo $ac_safe | sed 'y%abcdefghijklmnopqrstuvwxyz%ABCDEFGHIJKLMNOPQRSTUVWXYZ%'`
  AC_DEFINE_UNQUOTED([$ac_struct_in_hdr],[1])
  AC_MSG_RESULT(yes)
else
  AC_MSG_RESULT(no)
fi
])

dnl Default var $1 to $2 if the "$2" command is found.
dnl Also make $1 precious and set its description to "$2 command".
AC_DEFUN([MY_CMD],
  [AC_CHECK_PROG([$1],[$2],[$2])
   AC_ARG_VAR([$1],["$2" command])
  ]
)

dnl Some boilerplate for getting PACKAGE_* symbols defined conditionally.
AC_DEFUN([MY_FABULOUS_PACKAGE],[
dnl
dnl We provide these templates ourselves so that the PACKAGE_* symbols
dnl are only defaults.  This allows subprojects to have things like
dnl -DPACKAGE_NAME=name take precedence over any config.h definition.
dnl
dnl The #undef lines either become #define or get commented out in the
dnl config.h file that configure creates from the template config.h.in,
dnl in the usual manner (see autoconf/autoheader doc).
dnl
AH_VERBATIM([PACKAGE_BUGREPORT],
[/* Define to the e-mail address of the grateful bug report victim. */
#ifndef PACKAGE_BUGREPORT
#undef PACKAGE_BUGREPORT
#endif])
AH_VERBATIM([PACKAGE_NAME],
[/* Define to the full name of this package. */
#ifndef PACKAGE_NAME
#undef PACKAGE_NAME
#endif])
AH_VERBATIM([PACKAGE_STRING],
[/* Define to the full name and version of this package. */
#ifndef PACKAGE_STRING
#undef PACKAGE_STRING
#endif])
AH_VERBATIM([PACKAGE_TARNAME],
[/* Define to the short one-symbol archive name of this package. */
#ifndef PACKAGE_TARNAME
#undef PACKAGE_TARNAME
#endif])
AH_VERBATIM([PACKAGE_VERSION],
[/* Define to the version (x.y.z) of this package. */
#ifndef PACKAGE_VERSION
#undef PACKAGE_VERSION
#endif])
AH_VERBATIM([PACKAGE_URL],
[/* Define to the web home of this package. */
#ifndef PACKAGE_URL
#undef PACKAGE_URL
#endif])]
)

AC_DEFUN([CC_QNX6],
[if test -z "$CC"; then
  case $host_os in
  nto-qnx6* | qnx6* )  # a.k.a. Neutrino
    # QNX 6 development (Momentics) systems usually have both qcc and
    # gcc, both of which are GCC compilers.  We want CC to be qcc,
    # and on 6.3.2 it should be the better of the two versions that
    # are normally available (2.95.3 and 3.3.5), or at least a version
    # that supports the warning (-W...) flags we require.
    case $host_cpu in
    armbe )
      nto_arch=armbe
      : ${ac_cv_c_bigendian=yes}
      ;;
    armle | arm )
      nto_arch=armle
      : ${ac_cv_c_bigendian=no}
      ;;
    armv7 )
      nto_arch=armv7le
      : ${ac_cv_c_bigendian=no}
      ;;
    mipsbe )
      nto_arch=mipsbe
      : ${ac_cv_c_bigendian=yes}
      ;;
    mipsle )
      nto_arch=mipsle
      : ${ac_cv_c_bigendian=no}
      ;;
    powerpc )  # config.sub transforms ppcbe and ppc to powerpc
      # This is an abuse of notation occasioned by the fact that
      # as of this writing, config.sub scripts all over the world
      # do not recognize ppcbespe- (or ppcspe-) as a valid prefix
      # of a host alias.  But they generally do accept ppcbe-spe-,
      # where the spe part is posing as a "vendor" id.  So we
      # support configure args such as --host=ppcbe-spe-qnx6.5.0
      # to get the qcc arg -Vgcc_ntoppcbespe for such a host
      # (which by the way is called a "target" in QNX terms).
      case $host_vendor in
      spe )
        nto_arch=ppcbespe
        ;;
      * )
        nto_arch=ppcbe
        ;;
      esac
      : ${ac_cv_c_bigendian=yes}
      ;;
    shle | sh )
      nto_arch=shle
      : ${ac_cv_c_bigendian=no}
      ;;
    x86 | i*86 )
      nto_arch=x86
      : ${ac_cv_c_bigendian=no}
      ;;
    * )
      AC_MSG_ERROR([CPU type in $host_alias unrecognized for QNX 6])
      ;;
    esac
    case $host_os in
    *qnx6.3.2 )
      export CC="qcc -V3.3.5,gcc_nto$nto_arch"
      ;;
    * )
      export CC="qcc -Vgcc_nto$nto_arch"
      ;;
    esac
    ;;
  esac
fi])
