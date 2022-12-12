# $Id: opndpfxs.awk,v 1.6 2022/12/11 17:09:27 setlorg Exp $

# Free software (c) dB - see file COPYING for license (GPL).

NF > 0 {
  opcode = $1
  nopnd = split($2,opnds,",")
  if (nopnd > 0) {
    vargs = 0
    print ""
    print "  case op_" opcode ":"
    print "    switch (iopnd) {"
    for (iopnd = 1; iopnd <= nopnd && vargs == 0; iopnd++) {
      if (opnds[iopnd] == "*") {
        vargs = iopnd
      } else {
        print "    case " iopnd ":"
        if        (opnds[iopnd] == "<") {
          print "      in_prefix_check(fields," iopnd ");  break;"
        } else if (opnds[iopnd] == ">") {
          print "      out_prefix_check(fields," iopnd ");  break;"
        } else if (opnds[iopnd] == "<>") {
          print "      inout_prefix_check(fields," iopnd ");  break;"
        } else {
          print "      null_prefix_check(fields," iopnd ");  break;"
        }
      }
    }
    print "    default:"
    if (vargs != 0) {
      for (iopnd = vargs+1; iopnd <= nopnd; iopnd++) {
        print "      if (iopnd == nopnd - " (nopnd-iopnd) ") {"
        if        (opnds[iopnd] == "<") {
          print "        in_prefix_check(fields,iopnd);  break;"
        } else if (opnds[iopnd] == ">") {
          print "        out_prefix_check(fields,iopnd);  break;"
        } else if (opnds[iopnd] == "<>") {
          print "        inout_prefix_check(fields,iopnd);  break;"
        } else {
          print "        null_prefix_check(fields,iopnd);  break;"
        }
        print "      }"
      }
      # That just leaves operands corresponding to the *.  Their
      # prefixes can't be checked by what's in the opcodes file.
      print "      if (iopnd >= " vargs " &&"
      print "          iopnd <= nopnd - " (nopnd-vargs) ") {"
      print "        break;"
      print "      }"
    }
    print "      unexpected(iopnd);"
    print "    }"
    print "    break;"
  }
}
