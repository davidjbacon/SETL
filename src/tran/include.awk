# $Id: include.awk,v 1.4 2022/12/11 17:09:27 setlorg Exp $

# Free software (c) dB - see file COPYING for license (GPL).

$1 ~ /^%include/  { infile = $2
                    gsub("\"","",infile)
                    while (getline < infile) print
                  }
$1 !~ /^%include/ { print }
