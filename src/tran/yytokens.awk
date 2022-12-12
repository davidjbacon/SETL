# $Id: yytokens.awk,v 1.4 2022/12/11 17:09:27 setlorg Exp $

# Free software (c) dB - see file COPYING for license (GPL).

NF>1 { print "%token", $2 }
