# $Id: sysrotinit.awk,v 1.5 2022/12/11 17:09:27 setlorg Exp $

# Free software (c) dB - see file COPYING for license (GPL).

function def(a) {
  printf "  sysrot_init(\"S_%s\",S_%s);\n", $1, a
}

function def2(a) {
  printf "  sysrot_init(\"S_%s_1\",S_%s_1);\n", $1, a
  printf "  sysrot_init(\"S_%s_2\",S_%s_2);\n", $1, a
}

$1=="#" {def("card")}
$1=="*" {def("star")}
$1=="**" {def("power")}
$1=="+" {def2("plus")}
$1=="-" {def2("minus")}
$1=="/" {def("slash")}
$1=="/=" {def("ne")}
$1=="<" {def("lt")}
$1=="<=" {def("le")}
$1=="=" {def("eq")}
$1==">" {def("gt")}
$1==">=" {def("ge")}
$1=="?" {def("query")}
$1=="FROM" {def("FROM")}
$1=="FROMB" {def("FROMB")}
$1=="FROME" {def("FROME")}
$1=="IN" {def("IN")}
$1~/^[A-Z]/ && ($2~/^Uop/ || $2~/^Bop/ || $2=="Sysproc" || $2=="Sysval" || $2=="Sysvar") {def($1)}
