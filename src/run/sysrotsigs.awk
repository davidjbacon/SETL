# $Id: sysrotsigs.awk,v 1.3 2022/12/11 17:09:27 setlorg Exp $

# Free software (c) dB - see file COPYING for license (GPL).

BEGIN {
  t["RD"] = "<";
  t["WR"] = ">";
  t["RW"] = "<>";
}

{
  name=$2;
  parms=$3;
  s = parms;
  variadic = match(parms,/\*/);
  gsub(/[\(\)\*]/,"",s);  # s := parms stripped of parens and stars
  n = split(s,a,",");
  print "  sysrotsig_init (\"S_" name "\", " (n+3) ");";
  if (variadic) {
    print "  sysrotsig_parm (\"S_" name "\", 1, \"vsproc\");";
  } else {
    print "  sysrotsig_parm (\"S_" name "\", 1, \"sproc\");";
  }
  print "  sysrotsig_parm (\"S_" name "\", 2, \"S_" name "\");";
  for (i=1;i<=n;i++) {
    p = ".";
    if (i==n && variadic) p = "*";
    print "  sysrotsig_parm (\"S_" name "\", " (i+2) ", \"" t[a[i]] p "\");";
  }
  print "  sysrotsig_parm (\"S_" name "\", " (n+3) ", \">.\");";  # for ret
}
