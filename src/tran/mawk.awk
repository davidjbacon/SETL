# $Id: mawk.awk,v 1.4 2022/12/11 17:09:27 setlorg Exp $

# Free software (c) dB - see file COPYING for license (GPL).

/^#if 0/   { ++x }
           { if (x == 0) print }
/^#endif/  { x-- }
