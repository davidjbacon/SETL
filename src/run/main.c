/*  ===  SETL 'main' program - just calls 'run'  ===================  */

/*  $Id: main.c,v 1.6 2010/12/04 07:18:15 bacon Exp $  */

/*  Free software (c) dB - see file COPYING for license (GPL).  */

#include "setlrun.h"

int main (int argc, char *const argv[]) {
  int status = run ((unsigned)argc, argv);
  return status;
}
