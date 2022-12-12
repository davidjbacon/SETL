# $Id: opndtypes.awk,v 1.7 2022/12/11 17:09:27 setlorg Exp $

# Free software (c) dB - see file COPYING for license (GPL).

BEGIN {
  print ""
  print "#define proc_case(fieldname,opndtype)    \\"
  print "  case proc_fixed_opnd_index(fieldname): \\"
  print "    return opndtype;"
  print ""
}

NF > 0 {
  opcode = $1
  nopnd = split($2,opnds,",")
  if (nopnd > 0) {
    vargs = 0
    print ""
    print "  case op_" opcode ":"
    print "    switch (iopnd) {"
    if (opcode == "proc" ||
        opcode == "mainproc" ||
        opcode == "vproc") {
      # These correspond to the operands in proc_struct, which do not
      # have that one-to-one correspondence with args in the textual
      # form that the other SETL VM instrs do, but are derived from
      # the textual form and supplemented by other information:
      print "    proc_case(procnum,long_opnd)"
      print "    proc_case(level,  long_opnd)"
      print "    proc_case(nloc,   long_opnd)"
      print "    proc_case(nformal,long_opnd)"
      print "    proc_case(i_in,   long_opnd)"
      print "    proc_case(i_out,  long_opnd)"
      print "    proc_case(i_back, long_opnd)"
      print "    default:"
      print "      if (iopnd <= nopnd) {"
      # This means find_opndtype() shouldn't be used on the backtracks
      # starting at &bow[i_back], which are of fixed opndtype data_opnd:
      print "        return parm_opnd;  /* but backtracks are data_opnd */"
      print "      }"
      print "      unexpected(iopnd);"
    } else {
      for (iopnd = 1; iopnd <= nopnd && vargs == 0; iopnd++) {
        if (opnds[iopnd] == "*") {
          vargs = iopnd
        } else {
          print "    case " iopnd ":"
          if        (opnds[iopnd] == "r" ||
                     opnds[iopnd] == "l") {
            print "      return pc_opnd;"
          } else if (opnds[iopnd] == "p") {
            print "      return pc_opnd;"
          } else if (opnds[iopnd] == "s") {
            print "      return sysrot_opnd;"
          } else if (opnds[iopnd] == "i") {
            print "      return int_opnd;"
          } else if (opnds[iopnd] == "k") {
            print "      return long_opnd;"
          } else {
            print "      return data_opnd;"
          }
        }
      }
      print "    default:"
      if (vargs != 0) {
        # Should really loop from vargs+1 up to nopnd to check for all
        # possibilities in operands after the * (see for example
        # opndpfxs.awk), but we know it's all data from the * to the
        # end of the instruction.
        print "      if (" vargs " <= iopnd && iopnd <= nopnd) {"
        print "        return data_opnd;"
        print "      }"
      }
      print "      unexpected(iopnd);"
    }
    print "    }"
  }
}
