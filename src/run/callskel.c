/*  ===  Placeholder for SETL2 callout dispatcher  =================  */

/*  $Id: callskel.c,v 1.8 2017/09/05 02:32:48 setlorg Exp $  */

/*  Free software (c) dB - see file COPYING for license (GPL).  */

/*
 *  See the SETL2 distribution for a sample dispatcher, basically a
 *  switch statement on the 'service' code parameter.  As of this
 *  writing (early 90s?), it was in samples/callout/callskel.c in
 *  the SETL2 binary release.
 */

#include <stdio.h>
#include <errno.h>

extern char *setl2_callout(int service, unsigned argc, char *const argv[]);

char *setl2_callout(int service, unsigned argc, char *const argv[]) {
  unsigned argi;
  int saved_errno = errno;
  /* No os_fprintf treatment here, just "best effort" fprintf.  */
  fprintf(stderr, "setl2_callout called with service code %d and the"
                  " following %u arguments:\n", service, argc);
  for (argi=0; argi!=argc; argi++) {
    fprintf(stderr, "\"%s\"\n", argv[argi]);
  }
  errno = saved_errno;
  return NULL;
}
