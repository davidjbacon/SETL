# $Id: tinit.awk,v 1.4 2022/12/11 17:09:27 setlorg Exp $

# Free software (c) dB - see file COPYING for license (GPL).

NF>1 && $1!="$" && $3=="no" { printf "  put_tcode(\"%s\",%s);\n",$1,$2 }
NF>1 && $1!="$" && $3!="no" { printf "  if (!strict) put_tcode(\"%s\",%s);\n",$1,$2 }
