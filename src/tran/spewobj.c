/*  ===  Final code emission  ======================================  */

/*  $Id: spewobj.c,v 1.8 2021/01/09 01:44:32 setlorg Exp $  */

/*  Free software (c) dB - see file COPYING for license (GPL).  */


/* ------------------------------------------------------------------ */

#include "setltran.h"

/* Local routines */
static void spewheader(FILE *stream);
static void spewsrc(FILE *stream);
/*static void spewtokens(FILE *stream); */
static void spewcode(FILE *stream);
static void spew_source_upto(FILE *stream, int tn);

/* Local data */
static const char *object_header[] = {
#include "Obj.header"
""};

static int prev_srcend, prev_line;
static const char *string_opcodes[] = {
#include "String.opcodes"
""};


/* ------------------------------------------------------------------ */

void spewobj(void)
{
  spewheader(stdout);
  spewsrc(stdout);
/*spewtokens(stdout); */
  spewcode(stdout);
  fputs("%EXECUTE\n",stdout);
  fflush(stdout);
}

/* .................................................................. */

static void spewheader(stream)
FILE *stream;
{
  const char **p;
  for (p=object_header; strlen(*p)>0; p++) {
    fputs(*p,stream);  putc('\n',stream);
  }
}

/* .................................................................. */

static void spewsrc(stream)
FILE *stream;
{
  long isrc,iline;
  char c;
  fputs("%SOURCE\n",stream);
  fputs(source,stream);  putc('\n',stream);
  /*
   *  In some systems, it might have been necessary to have stored
   *  the line numbers with the source text, but in Unix, "lines"
   *  are just defined by where the newline characters are.
   */
  iline=1;
  for (isrc=0; isrc<srclen; iline++) {
    fprintf(stream,"%6ld\t",iline);
    while ((c=src[isrc++])!='\n') putc(c,stream);
    putc('\n',stream);
  }
}

#if 0
/* .................................................................. */

static void spewtokens(stream)
FILE *stream;
{
  int i;
  fputs("%TOKENS\n",stream);
  for (i=0; i<ntokens; i++) fprintf(stream,"%ld\n",tokens[i].srcloc);
}

#endif
/* .................................................................. */

static void spewcode(stream)
FILE *stream;
{
  csect *r;
  block *b;
  label *l;
  instr *i;
  opera *o;
  int tn = 0;
  prev_srcend = 0;
  prev_line = 0;
  fputs("%CODE\n",stream);
  for (r=virtcode; r!=(csect *)NULL; r=r->next) {
    for (b=r->blocks; b!=(block *)NULL; b=b->next) {
      for (l=b->labels; l!=(label *)NULL; l=l->next) {
        tn = l->tn > tn ? l->tn : tn;
        if (tn >= ntokens) tn = ntokens-1;
        spew_source_upto(stream,tn);
        fprintf(stream,"%6ld\t",tn >= 0 ? tokens[tn].srcloc : srclen);
        fputs(l->lab,stream);
        fputs(":\n",stream);
      }
      for (i=b->instrs; i!=(instr *)NULL; i=i->next) {
        tn = i->tn > tn ? i->tn : tn;
        if (tn >= ntokens) tn = ntokens-1;
        spew_source_upto(stream,tn);
        fprintf(stream,"%6ld\t\t",tn >= 0 ? tokens[tn].srcloc : srclen);
        fputs(string_opcodes[(int)(i->opcode)],stream);
        for (o=i->operas; o!=(opera *)NULL; o=o->next) {
          putc('\t',stream);
          if (o->acc & rd) putc('<',stream);
          if (o->acc & wr) putc('>',stream);
          fputs(o->libretto,stream);
        }
        putc('\n',stream);
      }
    }
  }
  spew_source_upto(stream,ntokens);
}

static void spew_source_upto(stream,tn)
FILE *stream;
int tn;
{
  int rtn = tn;
  int srclim;
  if (rtn < 0) rtn = 0;
  if (rtn < ntokens) srclim = tokens[rtn].srcend;
  else               srclim = srclen;
  while (prev_srcend < srclim) {
    prev_line++;
    putc('#',stream);  putc('\t',stream);
    do putc(src[prev_srcend],stream);
    while (src[prev_srcend++] != '\n');
  }
}
