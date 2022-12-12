# $Id: oprotsigs.awk,v 1.4 2022/12/11 17:09:27 setlorg Exp $

# Free software (c) dB - see file COPYING for license (GPL).

# Feeds on lexicon and works with sysrotsigs.awk to produce sysrotsigs.c
# (see the Makefile.am).  Could be more helpfully named.

$1 != "$" && $2 ~ /^Uop/     { unary($1) }
$1 != "$" && ($2 ~ /^Bop/ ||
              $2 == "'*'" ||
              $2 == "'/'" ||
              $2 == "'='" ||
              $2 == "IN")    { binary($1) }
$1 != "$" && $2 ~ /^FROM/    { from_op($1) }
$1 != "$" && ($2 == "'+'" ||
              $2 == "'-'")   { unary($1 "_1"); binary($1 "_2") }

function unary(name) {
  print "  sysrotsig_init (\"S_" name "\", 4);"
  print "  sysrotsig_parm (\"S_" name "\", 1, \"sproc\");"
  print "  sysrotsig_parm (\"S_" name "\", 2, \"S_" name "\");"
  print "  sysrotsig_parm (\"S_" name "\", 3, \"<.\");"
  print "  sysrotsig_parm (\"S_" name "\", 4, \">.\");"
}

function binary(name) {
  print "  sysrotsig_init (\"S_" name "\", 5);"
  print "  sysrotsig_parm (\"S_" name "\", 1, \"sproc\");"
  print "  sysrotsig_parm (\"S_" name "\", 2, \"S_" name "\");"
  print "  sysrotsig_parm (\"S_" name "\", 3, \"<.\");"
  print "  sysrotsig_parm (\"S_" name "\", 4, \"<.\");"
  print "  sysrotsig_parm (\"S_" name "\", 5, \">.\");"
}

function from_op(name) {
  print "  sysrotsig_init (\"S_" name "\", 4);"
  print "  sysrotsig_parm (\"S_" name "\", 1, \"sproc\");"
  print "  sysrotsig_parm (\"S_" name "\", 2, \"S_" name "\");"
  print "  sysrotsig_parm (\"S_" name "\", 3, \">.\");"
  print "  sysrotsig_parm (\"S_" name "\", 4, \"<>.\");"
}
