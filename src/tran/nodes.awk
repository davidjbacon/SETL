# $Id: nodes.awk,v 1.5 2022/12/11 17:09:27 setlorg Exp $

# Free software (c) dB - see file COPYING for license (GPL).

BEGIN {
  letters[1] = "a"
  letters[2] = "b"
  letters[3] = "c"
  letters[4] = "d"
  letters[5] = "e"
  letters[6] = "f"
  letters[7] = "g"
}

{
  n = split($0,a,"[,()]")
  s = "#define " a[1] "(t"
  for(i=3; i<n; i++) s = s "," letters[i-2]
  s = s ")"
  printf "%-35s node%d(N_%s,t", s, n-3, a[1]
  for(i=3; i<n; i++) printf ",%s",letters[i-2]
  printf ")\n"
}
