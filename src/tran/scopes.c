/*  ===  Control scope manager  ====================================  */

/*  $Id: scopes.c,v 1.3 2009/02/11 00:47:51 setlorg Exp $  */

/*  Free software (c) dB - see file COPYING for license (GPL).  */

/*
 *  This is a simple manager for a general-purpose stack of
 *  "control" scopes (not to be confused with the more specialized
 *  control scope managers private to parse.c).
 *
 *  It has nothing to do with "lexical" scopes, but is primarily
 *  used for checking the legality of jumps and labels.
 */


/* ------------------------------------------------------------------ */

#include "setltran.h"


/* ------------------------------------------------------------------ */

void initscope(type)
scopetype type;
{
  scopectr = 0;
  curscope = (scopestack *)NULL;
  pushscope(type);
}

/* .................................................................. */

void pushscope(type)
scopetype type;
{
  scopestack *t;
  getmem(t,1,scopestack);
  scopectr++;
  t->type = type;
  t->scopenum = scopectr;
  t->subscope = 0;  /* only 'gencode' cares about this field */
  t->ptr = curscope;
  curscope = t;
}

/* .................................................................. */

void popscope(type)
scopetype type;
{
  scopestack *t;
  assert (curscope->type == type);
  t = curscope;
  curscope = curscope->ptr;
  release(t);
}

/* .................................................................. */

void endscope(type)
scopetype type;
{
  popscope(type);
  assert (curscope == (scopestack *)NULL);
}
