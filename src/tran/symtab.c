/*  ===  Compile-time symbol table manager  ========================  */

/*  $Id: symtab.c,v 1.12 2021/01/06 05:52:17 setlorg Exp $  */

/*  Free software (c) dB - see file COPYING for license (GPL).  */


/* ------------------------------------------------------------------ */

#include "setltran.h"
#include "y.tab.h"

/* Local routines */
static symbol *symstackpred(symstack *p, const char *tag, symbol *y);


/* ------------------------------------------------------------------ */

/* Create and initialize a symbol table */
symtab new_symtab(void)
{
  symtab s;
  patcre(&s);
  return(s);
}

/* .................................................................. */

/* Copy a symbol table */
symtab symtabcopy(s)
symtab s;
{
  int i,n;
  symtab r;
  if (s==(symtab)NULL) return(s);
  r = new_symtab();
  n = size(s);
  for (i=1; i<=n; i++) {
    (void)symcopy(r,symi(s,i));
  }
  return(r);
} /* end symtabcopy */

/* .................................................................. */

/* Create and initialize a new symbol record */
symbol *new_symbol(const char *tag, symtype type, long tn, symbol *owner, int nparms) {
  symbol *y;
  allocsym(y,nparms);           /* allocate space */
  y->type = type;               /* symbol type */
  y->tn = tn;                   /* token number */
  y->tag = tag;                 /* normally same as symtab key */
  y->owner = owner;             /* parent */
  y->refinement_calls = 0;      /* no "calls" to this refinement yet */
  y->scopenum = 0;              /* later replaced if a label symbol */
  y->ability = 0;               /* caller may "or" in other abilities */
  y->stab = (symtab)NULL;       /* caller may fill in this subtable */
  y->nparms = nparms;           /* #formals if a routine symbol */
  return y;
}

/* .................................................................. */

/* Create a symbol to serve merely as a holder for a symtab */
symbol *new_symtabholder(symtab s) {
  symbol *y = new_symbol(NULL, symtabsym, -1, NULL, 0);
  y->stab = s;
  return y;
}

/* .................................................................. */

/*
 *  Copy a symbol into a symbol table.  Return a pointer to the
 *  inserted symbol record.
 */
symbol *symcopy(s,y)
symtab s;
symbol *y;
{
  int j;
  symbol *z;
  z = syminsert(s,y->tag,y->type,y->tn,y->owner,y->nparms,nosym);
  z->scopenum = y->scopenum;
  z->ability = y->ability;
  z->stab = symtabcopy(y->stab);
  for (j=0; j<y->nparms; j++) {
    z->parms[j] = y->parms[j];
  }
  return(z);
} /* end symcopy */

/* .................................................................. */

/*
 *  Insert a new symbol into a symbol table, or if the symbol is already
 *  there, make sure it has the type given by 'prev'.  Return a pointer
 *  to the created or discovered symbol record.
 */
symbol *syminsert(s,tag,type,tn,owner,nparms,prev)
symtab s;         /* symbol table */
const char *tag;  /* tag */
symtype type;     /* type of symbol */
long tn;          /* token number */
symbol *owner;    /* link to parent */
int nparms;       /* number of "parms" in the symbol (for routines) */
symtype prev;     /* allowable type of already present symbol */
{
  datum *d;
  symbol *y;
  if (lookup(s,tag,&d)) {
    y = d->symptr;
    if (prev != y->type) {
      tokerr(tn,"redeclaration");
    }
  } else {
    check (!keysub(s,tag,&d));  /* insert key */
    y = new_symbol(tag,type,tn,owner,nparms);  /* new symbol record */
    d->symptr = y;  /* point at it */
  }
  return(y);
} /* end syminsert */

/* .................................................................. */

/*
 *  An abbreviated form of 'syminsert' which handles the most common
 *  cases:
 */
symbol *symdef(symbol *holder, token *t, symtype type, int nparms) {
  assert (t->type == N_Token);
  return syminsert (holder->stab, t->graphic, type, t->tn, holder, nparms, nosym);
}

/* .................................................................. */

/*
 *  Find symbol in symbol table based on string key.
 *  Return pointer to the symbol record or NULL.
 */
symbol *symfind(s,tag)
symtab s;  /* symbol table */
const char *tag;  /* key */
{
  datum *d;
  if (lookup(s,tag,&d)) return(d->symptr);
  return((symbol *)NULL);
}

/* .................................................................. */

/*
 *  Find symbol in symbol table based on position.
 *  Return pointer to the symbol record or NULL.
 */
symbol *symi(s,i)
symtab s;  /* symbol table */
int i;  /* position */
{
  datum *d;
  if (ordsub(s,i,&d)) return(d->symptr);
  return((symbol *)NULL);
}

/* .................................................................. */

/*
 *  Find symbol in symbol table stack based on string key.
 *  Return pointer to the symbol record or NULL.
 */
symbol *symstackfind(p,tag)
symstack *p;  /* pointer to top of symtab stack */
const char *tag;  /* key */
{
  symbol *f;
  while (p != (symstack *)NULL) {
    if ((f = symfind(p->holder->stab,tag)) != NULL) return f;
    p = p->ptr;
  }
  return((symbol *)NULL);
}

/* .................................................................. */

/*
 *  A local, modified version of symstackfind which, given a symbol
 *  pointer as an extra parameter, insists on finding the given name
 *  one level closer to the stack top relative to where the extra
 *  symbol is found, or if it can't be so found, returns NULL.
 */
static symbol *symstackpred(symstack *p, const char *tag, symbol *y) {
  symbol *f,*e;
  while (p != NULL) {
    if ( ((f = symfind(p->holder->stab,tag)) != NULL)         &&
         (p->ptr != NULL)                                     &&
         ((e = symfind(p->ptr->holder->stab,y->tag)) != NULL) &&
         (e == y) ) return f;
    p = p->ptr;
  }
  return NULL;
}

/* .................................................................. */

/*
 *  This routine breaks abstraction by reaching out to
 *  application-specific spam such as the global 'paxinuse' and a
 *  particular parse tree node type ('node' is bad enough; N_nameseq
 *  is even worse).
 *
 *  It attempts to resolve the dotted name (an N_nameseq node) in q
 *  to a specific symbol using the symstack p, and return that symbol.
 *  It also fills in (to *k) the number of names it used from the
 *  name seq, starting from the left, in making the identification.
 */
symbol *symresolve(symstack *p, node *q, int *k) {
  int i;
  token *t,*u;
  symbol *x,*y,*z,*w;
  assert (q->type == N_nameseq);
  assert (q->nsub > 0);
  i = 0;
  t = name_token(subi(q,i));
  x = symstackfind(p,t->graphic);
  if (x == (symbol *)NULL) goto done;
  assert (leq(x->tag,t->graphic));
  if (x->type == selsym) {
    tokerr(t->tn,"compound name cannot start with a selector");
  }
  i++;
  if (x->type == packsym && paxinuse != (symtab)NULL) {
    y = symfind(paxinuse,x->tag);
    if (y != (symbol *)NULL) {
      assert (y->type == packsym);
      assert (leq(y->tag,x->tag));
      z = symfind(publictab,y->tag);
      assert (z->type == packsym);
      assert (leq(z->tag,y->tag));
      /* Currently we can only see "one level" in the USEd package. */
      if (i < q->nsub) {
        u = name_token(subi(q,i));
        w = symfind(z->stab,u->graphic);
        if (w == (symbol *)NULL) {
          tokerr(u->tn,"name not publicized by given package");
        }
        if (w->type == selsym) goto done;
        x = w;
        i++;
      }
      goto done;
    }
  }
  /* Here if the leading name is not a package name in paxinuse. */
  while (x->type == packsym ||
         x->type == progsym ||
         x->type == procsym) {
    if (i < q->nsub) {
      u = name_token(subi(q,i));
      w = symstackpred(p,u->graphic,x);
      if (w == (symbol *)NULL) goto done;
      if (w->type == selsym) goto done;
      x = w;
      i++;
    } else {
      goto done;
    }
  }
done:
  *k = i;  /* 0 <= *k <= q->nsub is guaranteed */
  return x;
}

/* .................................................................. */

/*
 *  Push a symtab onto a symtab stack.
 */
void symstackpush(symstack **p, symbol *holder) {
  symstack *q;
  getmem (q, 1, symstack);
  q->holder = holder;  /* symbol that holds the actual symtab */
  q->ptr = *p;
  *p = q;
}

/* .................................................................. */

/*
 *  Pop a symtab from a symtab stack.
 */
void symstackpop(symstack **p) {
  symstack *q;
  q = (*p)->ptr;
  release(*p);  /* a no-op given that we are in "ephemeral mode" */
  *p = q;
}

/* .................................................................. */

void symtabdump(stream,s,lev)
FILE *stream;
symtab s;
int lev;
{
  int i,j,n;
  symbol *y;
  if (!s) {
    fprintf(stream,"<null>\n");
    return;
  }
  n = size(s);
  for (i=1; i<=n; i++) {
    y = symi(s,i);
    for (j=0; j<lev; j++) fprintf(stream,".");
    if (y->owner != (symbol *)NULL) {
      fprintf(stream,"%s type=%d tn=%ld owner=%s scopenum=%d ability=%d",
       y->tag, y->type, y->tn, y->owner->tag, y->scopenum, y->ability);
    } else {
      fprintf(stream,"%s type=%d tn=%ld scopenum=%d ability=%d",
       y->tag, y->type, y->tn, y->scopenum, y->ability);
    }
    if (y->nparms > 0) {
      fprintf(stream," parms=");
      for (j=0; j<y->nparms; j++) fprintf(stream,"%d ",y->parms[j]);
    }
    fprintf(stream,"\n");
    if (y->stab != (symtab)NULL) {
      symtabdump(stream,y->stab,lev+1);
    }
  }
}
