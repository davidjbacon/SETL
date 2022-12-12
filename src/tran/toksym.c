/*  ===  Token code manager  =======================================  */

/*  $Id: toksym.c,v 1.9 2021/02/26 21:18:39 setlorg Exp $  */

/*  Free software (c) dB - see file COPYING for license (GPL).  */

/*
 *  Token codes are stored in a local table 'tcodes' whose keys
 *  are the graphical "string" forms of the tokens.  "System tags"
 *  (which are "reserved words" in the default stropping regime)
 *  are appropriately case-mapped according to the stropping regime
 *  ('keycase') by 'put_tcode' as they are entered into the table
 *  during initialization.  Similarly, "user tags" are case-mapped
 *  according to 'tagcase' when 'toksymname' is used to fill in the
 *  'code' field and update the 'graphic' field of a 'token' structure
 *  during lexical analysis.
 */


/* ------------------------------------------------------------------ */

#include "setltran.h"
#include "y.tab.h"

/* Local routines */
static void put_tcode(const char *s, int i);


/* ------------------------------------------------------------------ */

/* Local data */

/* SETL token code table */
static table tcodes;


/* ------------------------------------------------------------------ */

/* Initialize table */
void toksyminit(void)
{
  patcre(&tcodes);
#include "Tinits"
}

/* .................................................................. */

/* Store a system token code in the local table */
static void put_tcode(s,i)
const char *s;
int i;
{
  const char *t;
  DATA *ii;
  if (keycase == lowercase) t = strmakelower(s);
  else                      t = strmakeupper(s);
  keysub(tcodes,t,&ii);
  ii->code = i;
}

/* .................................................................. */

/*
 *  Fetch a system token code, or 0 if s does not contain the
 *  graphic of a system token.
 */
int get_tcode(s)
const char *s;
{
  DATA *ii;
  if (lookup(tcodes,s,&ii)) return(ii->code);
  else return(0);
}

/* .................................................................. */

/*
 *  Fill in the 'code' field of the token t, and update the case of
 *  its 'graphic' field according as it represents a keyword
 *  ("system" tag) or an identifier ("user" tag) in the current
 *  stropping regime as reflected in 'keycase' and 'tagcase'.
 */
void toksymname(t)
token *t;
{
  const char *s;
  int code;
  if (keycase == anycase) s = strmakeupper(t->graphic);
                     else s = t->graphic;
  if ((code=get_tcode(s)) != 0) {
    if (keycase == anycase) t->graphic = s;
    t->code = code;
  } else {
    toksymid(t);
  }
} /* end toksymname */

/* .................................................................. */

/*
 *  Do what it says above to treat the token as an identifier.  At the
 *  early stage where this is called, that includes anything that is
 *  not found to be a keyword, and anything after a dot that looks
 *  like an identifier in the reigning tagcase.
 */
void toksymid(t)
token *t;
{
  switch (tagcase) {
  case lowercase:
    if (strislower(t->graphic)) break;
    tranerr(t->srcloc,"neither keyword nor (lowercase) identifier");
  case uppercase:
    if (strisupper(t->graphic)) break;
    tranerr(t->srcloc,"neither keyword nor (uppercase) identifier");
  case anycase:
    t->graphic = strmakelower(t->graphic);
    break;
  case mixedcase:
    break;
  default:
    unexpected(tagcase);
  }
  t->code = Name;  /* "identifier" */
} /* end toksymid */

/* .................................................................. */


token *name_token(node *a) {
  token *r = (token *)a;
  assert (r->type == N_Token);
  assert (r->code == Name);
  return r;
}

token *cmdname_token(node *a) {
  token *r = (token *)a;
  assert (r->type == N_Token);
  assert (r->code == Command_name);
  return r;
}

token *bopname_token(node *a) {
  token *r = (token *)a;
  assert (r->type == N_Token);
  assert (r->code == Bop_name);
  return r;
}

token *uopname_token(node *a) {
  token *r = (token *)a;
  assert (r->type == N_Token);
  assert (r->code == Uop_name);
  return r;
}

token *refname_token(node *a) {
  token *r = (token *)a;
  assert (r->type == N_Token);
  assert (r->code == Ref_name);
  return r;
}

token *syscon_token(node *a) {
  token *r = (token *)a;
  assert (r->type == N_Token);
  assert (r->code == Syscon);
  return r;
}

token *sysval_token(node *a) {
  token *r = (token *)a;
  assert (r->type == N_Token);
  assert (r->code == Sysval);
  return r;
}

token *sysvar_token(node *a) {
  token *r = (token *)a;
  assert (r->type == N_Token);
  assert (r->code == Sysvar);
  return r;
}

token *sysproc_token(node *a) {
  token *r = (token *)a;
  assert (r->type == N_Token);
  assert (r->code == Sysproc);
  return r;
}

token *machine_token(node *a) {
  token *r = (token *)a;
  assert (r->type == N_Token);
  assert (r->code == Machine);
  return r;
}

token *c_code_token(node *a) {
  token *r = (token *)a;
  assert (r->type == N_Token);
  assert (r->code == C_code);
  return r;
}
