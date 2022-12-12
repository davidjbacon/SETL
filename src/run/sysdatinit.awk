# $Id: sysdatinit.awk,v 1.3 2022/12/11 17:09:27 setlorg Exp $

# Free software (c) dB - see file COPYING for license (GPL).

function def(a,b) {
  printf "  sysdat_init(\"%s\",S_%s);\n", a, b
}

BEGIN {
  def("-", "dev_null")
  def("STATUS", "status")
  def("MACHINE", "machine")
  def("{}", "empty_set")
  def("[]", "empty_tuple")
}

$1~/^[A-Z]/ && $2=="Syscon" {
  def("S_" $1, $1)
}
