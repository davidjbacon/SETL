/*  ===  A small debugging addendum to the parser  =================  */

/*  $Id: debug.c,v 1.5 2020/11/21 02:15:49 bacon Exp $  */

/*  Free software (c) dB - see file COPYING for license (GPL).  */

#include "setltran.h"

#define YACC_TOKEN_BASE 257

/* .................................................................. */

static const char *debug_tcodes[] = {
"YACC_TOKEN_BASE",
#include "Debug.tcodes"
""};
static const char *debug_nodetypes[] = {
"null",
"N_Token",
#include "Debug.nodetypes"
""};

/* .................................................................. */

/* Dump parse tree in readable format */
void dumptree(stream,nod,lev,isub)
FILE *stream;
node *nod;
int lev,isub;
{
  int i;
  token *t;
  for (i=0; i<lev; i++) fprintf(stream,".");
  fprintf(stream,"%d) ",isub);
  if (nod == (node *)NULL) {
    fprintf(stream,"null\n");
    return;
  }
  if (nod->type == N_Token) {
    t = (token *)nod;
    fprintf(stream,"N_Token tn=%ld %ld-%ld \"%s\" code=%d (",
                t->tn,t->srcloc,t->srcend,t->graphic,t->code);
    if (t->code < YACC_TOKEN_BASE) {
      fprintf(stream,"\'%c\'",t->code);
    } else {
      fprintf(stream,"%s",debug_tcodes[t->code-YACC_TOKEN_BASE]);
    }
    fprintf(stream,")\n");
  } else {
    fprintf(stream,"%s tn=%ld nsub=%d\n",
      debug_nodetypes[(int)(nod->type)],nod->tn,nod->nsub);
    for (i=0; i<(nod->nsub); i++) {
      dumptree(stream,nod->sub[i],lev+1,i);
    }
  }
}
