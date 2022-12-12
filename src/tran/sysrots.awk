# $Id: sysrots.awk,v 1.3 2022/12/11 17:09:27 setlorg Exp $

# Free software (c) dB - see file COPYING for license (GPL).

BEGIN {
  t["RD"] = "rd";
  t["WR"] = "wr";
  t["RW"] = "rw";
}

{
  name=$2;
  parms=$3;
  s = parms;
  gsub(/[\(\)\*]/,"",s);
  n = split(s,a,",");
  print "  y = put_sysrot(\"" $2 "\"," n ");";
  for (i=1;i<n;i++) {
    print "  y->parms[" (i-1) "] = " t[a[i]] "able;";
  }
  if (n > 0) {
    if (match(parms,/\*/) == 0) {
      print "  y->parms[" (n-1) "] = " t[a[n]] "able;";
    } else {
      print "  y->parms[" (n-1) "] = " t[a[n]] "able+starred;";
    }
  }
}
