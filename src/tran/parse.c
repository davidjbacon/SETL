/*  ===  A peartridge in a ... ...  ================================  */

/*  $Id: parse.c,v 1.9 2021/02/26 01:20:10 setlorg Exp $  */

/*  Free software (c) dB - see file COPYING for license (GPL).  */


/*
 *  This file essentially just provides an interface between the
 *  top-level (driver) routine of the translator and the LALR(1)
 *  parser 'yyparse' in y.c, which is obtained by preprocessing
 *  "grammar" to get y.y and then running yacc or bison on y.y.
 */


/* ------------------------------------------------------------------ */

#include "setltran.h"
#include "y.tab.h"


/* ------------------------------------------------------------------ */

/* Public data */

bool pre_parsing;               /* the lexer needs to know */


/* Local data */

static bool autosemi;           /* semicolon may be inferred */


/* ------------------------------------------------------------------ */

/* This subroutine is called by the driver */
void parse(void)
{
  pre_parsing = false;
  yylval = (node *)NULL;  /* tell yylex to start at the beginning */
  yydebug = debug;       /* control tracing of yacc-style parsing */
  /*
   *  The idea here was that a compiler option would turn on semicolon
   *  inference, resulting in a call to ok_autosemi(), but this does not
   *  appear to be an easy feature to implement properly in the context
   *  of LALR(1) parsing:
   */
  no_autosemi();        /* ';' will not be interpolated at '\n' */
  if (yyparse() != 0) quit();   /* invoke the yacc-based parser */
}

/* .................................................................. */

/*
 *  Everything past here is called only by yyparse, the yacc-based
 *  parser.
 */

/* .................................................................. */

/* Report an error */
void yyerror(s)
const char *s;
{
  /*
   *  I don't know exactly what circumstances can cause yylval
   *  to be NULL when this routine is entered, but I know that
   *  my most common SETL syntax error (missing END) can do it;
   *  for want of thinking of anything better to do, and perhaps
   *  not entirely inappropriately, I am just flagging the very
   *  end of the program in such cases:
   */
  if (yylval) tokerr(yylval->tn,s);
  else tokerr(ntokens,s);
}

/* .................................................................. */

static int curtoken;  /* shared by yylex and pop_ctrlscope */

/* .................................................................. */

/* Fetch a token */
int yylex(void)
{
  if (yylval==(node *)NULL) {
    curtoken = -1;
  }
  curtoken++;
  if (curtoken>=ntokens) {
    yylval = (node *)NULL;  /* all is peaceful again */
    return(0);
  }
  yylval = (node *)(&tokens[curtoken]);
  return tokens[curtoken].code;
} /* end yylex */


/* .................................................................. */

void ok_autosemi(void) {
  autosemi = true;
}

void no_autosemi(void) {
  autosemi = false;
}

/* .................................................................. */

/*
 *  Following is a "control scope manager."  Its sole purpose is to
 *  check tokens between END and ';' for agreement with those at the
 *  beginning of the current END-terminating control statement.
 */

struct ctrlscope {
  int tokenum;
  struct ctrlscope *prev;
};
static struct ctrlscope *ctrlscopestack;

/* .................................................................. */

void init_ctrlscope(x)
int x;
{
  ctrlscopestack = (struct ctrlscope *)NULL;
}

/* .................................................................. */

void push_ctrlscope(nod)
node *nod;
{
  struct ctrlscope *s;
  getmem(s,1,struct ctrlscope);
  s->tokenum = nod->tn;
  s->prev = ctrlscopestack;
  ctrlscopestack = s;
}

/* .................................................................. */

void pop_ctrlscope(nod)
node *nod;
{
  int i,j,k;
  struct ctrlscope *s;
  assert (nod != (node *)NULL);
  assert (tokens[nod->tn].code == END);
  s = ctrlscopestack;
  if (s == (struct ctrlscope *)NULL) {
    tokerr(nod->tn,"nothing for this END to terminate");
  }
  i = s->tokenum;
  j = nod->tn+1;
  /*
   *  I allow "(FOR..." to be terminated by "END (FOR..." or "END FOR..."
   *  and likewise for "(WHILE..." etc.:
   */
  if (tokens[i].code=='(' && tokens[j].code!='(') i++;
  k = 0;
  while (j<ntokens && tokens[j].code!=';' &&
         leq(tokens[j].graphic,tokens[i].graphic)) {
    i++; j++; k++;
  }
  if (j >= ntokens) {
    tokerr(j,"unexpected end of SETL text");
  }
  if (tokens[j].code==';') goto succeed;
  /*
   *  For SETL2 compatibility, allow loops of all types to be ended
   *  simply by 'END LOOP'.  Also allow things like PROGRAM <name>
   *  and PROC(EDURE) <name> to be ended by 'END <name>'.
   */
  if (tokens[s->tokenum].code==FOR   ||
      tokens[s->tokenum].code==WHILE ||
      tokens[s->tokenum].code==UNTIL) {
    if (tokens[j].code==LOOP && tokens[j+1].code==';') {
      /*
       *  This allows "END {some matching tokens} LOOP;" too;
       *  e.g., "END WHILE LOOP", "END FOR i LOOP" etc.
       */
      k++;
      goto succeed;
    }
    goto fail;
  }
  if ((tokens[s->tokenum].code==PACKAGE ||
       tokens[s->tokenum].code==CLASS)   &&
       tokens[s->tokenum+1].code==BODY) {
    if (leq(tokens[nod->tn+1].graphic,tokens[s->tokenum+2].graphic) &&
        tokens[nod->tn+2].code==';') {
      k = 1;
      goto succeed;
    }
    goto fail;
  }
  if (tokens[s->tokenum].code==PROGRAM   ||
      tokens[s->tokenum].code==PROC      ||
      tokens[s->tokenum].code==OP        ||
      tokens[s->tokenum].code==COMMAND   ||
      tokens[s->tokenum].code==DIRECTORY ||
      tokens[s->tokenum].code==MODULE    ||
      tokens[s->tokenum].code==LIBRARY   ||
      tokens[s->tokenum].code==PACKAGE   ||
      tokens[s->tokenum].code==CLASS) {
    if (leq(tokens[nod->tn+1].graphic,tokens[s->tokenum+1].graphic) &&
        tokens[nod->tn+2].code==';') {
      k = 1;
      goto succeed;
    }
    goto fail;
  }
fail:
  tokerr(j,"tokens after END do not match previous tokens");
succeed:
  curtoken += k;
  ctrlscopestack = s->prev;
  release(s);  /* a no-op given that we are in "ephemeral mode" */
}

/* .................................................................. */

void check_ctrlscope(i)
int i;
{
  int n;
  struct ctrlscope *s;
  s = ctrlscopestack;
  for (n=0; s != (struct ctrlscope *)NULL; n++) s = s->prev;
  assert (n == i);  /* else we haven't counted ENDs properly! */
}

/* .................................................................. */

/*
 *  The following is another control scope manager, for loops.
 *  It allows us to match loop header tokens with the tokens on
 *  QUIT and CONTINUE statements easily.
 */

struct loopscope {
  int tokenum;
  struct loopscope *prev;
};
static struct loopscope *loopscopestack;

/* .................................................................. */

void init_loopscope(x)
int x;
{
  loopscopestack = (struct loopscope *)NULL;
}

/* .................................................................. */

void push_loopscope(nod)
node *nod;
{
  struct loopscope *s;
  getmem(s,1,struct loopscope);
  s->tokenum = nod->tn;
  s->prev = loopscopestack;
  loopscopestack = s;
}

/* .................................................................. */

void pop_loopscope(void)
{
  struct loopscope *s;
  s = loopscopestack;
  assert (s != (struct loopscope *)NULL);
  loopscopestack = s->prev;
  release(s);  /* a no-op given that we are in "ephemeral mode" */
}

/* .................................................................. */

node *match_loop(nod)
node *nod;
{
  int i,j,k;
  struct loopscope *s;
  s = loopscopestack;
  if (s == (struct loopscope *)NULL) {
    tokerr(nod->tn,"no enclosing loop");
  }
  while (s != (struct loopscope *)NULL) {
    i = s->tokenum;
    j = nod->tn+1;
    if (tokens[i].code=='(' && tokens[j].code!='(') i++;
    k = 0;
    while (j<ntokens && tokens[j].code!=';' &&
           leq(tokens[j].graphic,tokens[i].graphic)) {
      i++; j++; k++;
    }
    if (j >= ntokens) {
      tokerr(j,"unexpected end of SETL text");
    }
    if (tokens[j].code==';') goto succeed;
    /*
     *  Allow loops of all SETL2 types to be exited or continued simply
     *  by 'QUIT LOOP' or 'CONTINUE LOOP' (SETL2 itself only allows
     *  you to quit or continue the innermost enclosing loop in this
     *  way, and I believe my rules work out to the same thing even
     *  though there is no explicit attempt here to avoid searching up
     *  the loopscope stack; in fact, I am putting in a rule that
     *  "(" as a loop-starter will always match LOOP in a QUIT or
     *  CONTINUE statement too):
     */
    if (tokens[s->tokenum].code==FOR   ||
        tokens[s->tokenum].code==WHILE ||
        tokens[s->tokenum].code==UNTIL ||
        tokens[s->tokenum].code=='(') {
      if (tokens[j].code==LOOP &&
          (tokens[j+1].code==';')) {
        /*
         *  This allows "QUIT {some matching tokens} LOOP;" too;
         *  e.g., "CONTINUE WHILE LOOP", "QUIT FOR i LOOP" etc.
         */
        k++;
        goto succeed;
      }
    }
    s = s->prev;
  } /* end while */
  tokerr(j,"tokens after QUIT or CONTINUE do not match previous tokens");
succeed:
  curtoken += k;
  return((node *)(&tokens[s->tokenum]));
}

/* .................................................................. */

void check_cmd_arg(nod)
node *nod;
{
  int i = nod->tn;
  int k = tokens[i].srcloc - 1;
  assert (k >= 0);
  if (!isspace((unsigned char)(src[k]))) {
    tokerr(i,"command statement tokens must be separated by whitespace");
  }
}

/* .................................................................. */

/* Routines to create and fill in parse tree nodes... */

node *nullnode(void)
{
  node *p;
  if (pre_parsing) return (node *)NULL;
  allocnode(p,0);
  p->type = null;
  p->tn = -1;
  p->nsub = 0;
  return(p);
}

/* .................................................................. */

node *node0(type,t)
nodetype type;
node *t;
{
  node *p;
  if (pre_parsing) return (node *)NULL;
  allocnode(p,0);
  p->type = type;
  p->tn = (t != (node *)NULL) ? t->tn : -1;
  p->nsub = 0;
  return(p);
}

/* .................................................................. */

node *node1(type,t,a)
nodetype type;
node *t;
node *a;
{
  node *p;
  if (pre_parsing) return (node *)NULL;
  allocnode(p,1);
  p->type = type;
  p->tn = (t != (node *)NULL) ? t->tn : -1;
  p->nsub = 1;
  p->sub[0] = a;
  return(p);
}

/* .................................................................. */

node *node2(type,t,a,b)
nodetype type;
node *t;
node *a,*b;
{
  node *p;
  if (pre_parsing) return (node *)NULL;
  allocnode(p,2);
  p->type = type;
  p->tn = (t != (node *)NULL) ? t->tn : -1;
  p->nsub = 2;
  p->sub[0] = a;
  p->sub[1] = b;
  return(p);
}

/* .................................................................. */

node *node3(type,t,a,b,c)
nodetype type;
node *t;
node *a,*b,*c;
{
  node *p;
  if (pre_parsing) return (node *)NULL;
  allocnode(p,3);
  p->type = type;
  p->tn = (t != (node *)NULL) ? t->tn : -1;
  p->nsub = 3;
  p->sub[0] = a;
  p->sub[1] = b;
  p->sub[2] = c;
  return(p);
}

/* .................................................................. */

node *node4(type,t,a,b,c,d)
nodetype type;
node *t;
node *a,*b,*c,*d;
{
  node *p;
  if (pre_parsing) return (node *)NULL;
  allocnode(p,4);
  p->type = type;
  p->tn = (t != (node *)NULL) ? t->tn : -1;
  p->nsub = 4;
  p->sub[0] = a;
  p->sub[1] = b;
  p->sub[2] = c;
  p->sub[3] = d;
  return(p);
}

/* .................................................................. */

node *node5(type,t,a,b,c,d,e)
nodetype type;
node *t;
node *a,*b,*c,*d,*e;
{
  node *p;
  if (pre_parsing) return (node *)NULL;
  allocnode(p,5);
  p->type = type;
  p->tn = (t != (node *)NULL) ? t->tn : -1;
  p->nsub = 5;
  p->sub[0] = a;
  p->sub[1] = b;
  p->sub[2] = c;
  p->sub[3] = d;
  p->sub[4] = e;
  return(p);
}

/* .................................................................. */

node *node6(type,t,a,b,c,d,e,f)
nodetype type;
node *t;
node *a,*b,*c,*d,*e,*f;
{
  node *p;
  if (pre_parsing) return (node *)NULL;
  allocnode(p,6);
  p->type = type;
  p->tn = (t != (node *)NULL) ? t->tn : -1;
  p->nsub = 6;
  p->sub[0] = a;
  p->sub[1] = b;
  p->sub[2] = c;
  p->sub[3] = d;
  p->sub[4] = e;
  p->sub[5] = f;
  return(p);
}

/* .................................................................. */

node *noden(type,t,n)
nodetype type;
node *t;
int n;
{
  node *p;
  if (pre_parsing) return (node *)NULL;
  allocnode(p,n);
  p->type = type;
  p->tn = (t != (node *)NULL) ? t->tn : -1;
  p->nsub = n;
  /* Caller fills in its own subnode fields */
  return(p);
}
