/*  ===  Initialize symbol table of "system" procedures  ===========  */

/*  $Id: sysrot.c,v 1.5 2010/12/04 07:18:23 bacon Exp $  */

/*  Free software (c) dB - see file COPYING for license (GPL).  */


/* ------------------------------------------------------------------ */

#include "setltran.h"

/* Local routines */
static symbol *put_sysrot(const char *s, int nparms);


/* ------------------------------------------------------------------ */

/* Make the table */
void sysrotinit(void)
{
  symbol *y;

  sysrottab = new_symtab();

  /* All the calls to put_sysrot are here: */

#include "Sysrot.inits"

}

/* .................................................................. */

/* Enter one Sysproc, Sysval, or Sysvar symbol */
static symbol *put_sysrot(s,nparms)
const char *s;
int nparms;
{
  const char *t;
  symbol *y;
  if (keycase == lowercase) t = strmakelower(s);
  else                      t = strmakeupper(s);
  y = syminsert(sysrottab,t,sysrotsym,-1,(symbol *)NULL,nparms,nosym);
  return(y);
}
