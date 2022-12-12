# $Id: sysdatdef.awk,v 1.3 2022/12/11 17:09:27 setlorg Exp $

# Free software (c) dB - see file COPYING for license (GPL).

function def(a) {
  printf "#define S_%s %d\n", a, ++n
}

BEGIN {
  def("dev_null")
  def("status")
  def("machine")
  def("empty_set")
  def("empty_tuple")
}

$1~/^[A-Z]/ && $2=="Syscon" {
  def($1)
}
