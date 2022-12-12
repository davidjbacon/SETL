/*  ===  C "main" stub, just calls tran()  =========================  */

/*  $Id: main.c,v 1.6 2015/11/30 03:47:40 setlorg Exp $  */

/*  Free software (c) dB - see file COPYING for license (GPL).  */

/*
 *  This program translates SETL source text into code for the
 *  SETL Virtual Machine.
 *
 *  Usually it is run by the "setl" command, with its output read
 *  through a pipe, but it can be run directly from the command line
 *  if desired, as an alternative to "setl -c -nocpp ...".
 *
 *  It is designed to be serially reusable, so that it can handle
 *  any number of %END-separated programs in sequence, and thus be
 *  used as a pumping "server".  See 'tran' in "tran.c".
 */

#include "setltran.h"

extern int main(int argc, char *const argv[]);

int main(int argc, char *const argv[]) {
  tran((unsigned)argc,argv);
}
