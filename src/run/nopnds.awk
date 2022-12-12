# $Id: nopnds.awk,v 1.5 2022/12/11 17:09:27 setlorg Exp $

# Free software (c) dB - see file COPYING for license (GPL).

NF > 0 {
  opcode = $1
  nopnd = split($2,opnds,",")
  vargs = 0
  print "  case op_" opcode ":"
  for (iopnd=1; iopnd<=nopnd && vargs == 0; iopnd++) {
    if (opnds[iopnd] == "*") {
      vargs = iopnd
    }
  }
  if (vargs == 0) {
    print "    numfields_check(fields," (nopnd+1) ");  break;"
  } else {
    # The * matches 0 or more args, so this works whether or not any
    # args are required after the optional ones signified by the *:
    print "    varfields_check(fields," nopnd ");  break;"
  }
}
