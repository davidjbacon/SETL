$3 ~ /^[1-9][0-9]*$/    {print "[[const " $2 "] '" $3 "']"}
$3 ~ /^0[0-7]+$/        {print "[[const " $2 "] '8#" substr($3,2) "#']"}
$3 ~ /^0x[0-9a-fA-F]+$/ {print "[[const " $2 "] '16#" substr($3,3) "#']"}
$3 == "0"               {print "[[const " $2 "] '0']"}
$5 ~ /glut/             {print "[[const " $2 "] 'gluta" substr($5,6,length($5)-6) "()']"}
