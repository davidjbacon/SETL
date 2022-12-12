/*  ===  Semantic analysis  ========================================  */

/*  $Id: analyze.c,v 1.8 2020/12/18 23:37:36 setlorg Exp $  */

/*  Free software (c) dB - see file COPYING for license (GPL).  */

/*
 *  This is the top level of the semantic analysis pass.
 *
 *  It goes flatten(), identify(), canonize() on the parse tree.
 */


/* ------------------------------------------------------------------ */

#include "setltran.h"

/* Local routines */
static void silly_dump(node *p, const char *t, const char *s);


/* ------------------------------------------------------------------ */

void analyze(void)
{
  /*
   *  I'm risking modifying the parse tree in place, so as
   *  to avoid copying the whole thing (easily done as a
   *  dumptree-style depth-first walk if I change my mind):
   */
  silly_dump(root,"parse tree","parse.tree");
  if (root) flatten(root);      /* rationalize lists */
  if (verbose) fprintf(stderr,"%s:  done parse tree flattening (%d ms)\n",
                               tranprog, millisec());
  silly_dump(root,"flattened parse tree","flattened.tree");
  identify(root);       /* put explicit dcls in symtabs */
  if (verbose) fprintf(stderr,"%s:  done explicit symbols (%d ms)\n",
                               tranprog, millisec());
  if (nerrors > 0) quit();
  /*
   *  Another little pass in here could turn stuff like lhs and rw into
   *  expr, but I guess it turned out 'canonize' didn't really need that.
   */
  canonize(root);       /* make the parse tree beautiful */
  if (verbose) fprintf(stderr,"%s:  done implicit symbols (%d ms)\n",
                               tranprog, millisec());
  if (debug) {
    fprintf(stderr,"Symbol tables:\n");
    fprintf(stderr,"==== SYSROTS ====\n");
    symtabdump(stderr,sysrottab,0);
    fprintf(stderr,"==== PUBLIC ====\n");
    symtabdump(stderr,publictab,0);
    fprintf(stderr,"==== PRIVATE ====\n");
    symtabdump(stderr,privatetab,0);
    fprintf(stderr,"==== USETABS ====\n");
    symtabdump(stderr,usetabs,0);
    fprintf(stderr,"==== PAXINUSE ====\n");
    symtabdump(stderr,paxinuse,0);
    fprintf(stderr,"==== UNITTABS ====\n");
    symtabdump(stderr,unittabs,0);
  }
  if (nerrors > 0) quit();
} /* end analyze */

/* .................................................................. */

static void silly_dump(p,t,s)
node *p;
const char *t, *s;
{
  FILE *dumpfile;
  if (debug) {
    fprintf(stderr,"%s:  writing %s to \"%s\"\n",
                    tranprog,    t,       s);
    dumpfile = fopen(s,"w");
    dumptree(dumpfile,p,0,0);
    fclose(dumpfile);
    fprintf(stderr,"%s:  done writing %s\n",
                    tranprog,         t);
  }
} /* end silly_dump */
