/*  ===  The essence of PATRICIA (SETL run-time version)  ==========  */

/*  $Id: patricia.c,v 1.11 2018/07/03 14:06:41 setlorg Exp $  */

/*  Free software (c) dB - see file COPYING for license (GPL).  */

/*
 *  For historical reasons, Patricia trees are called "subnodes"
 *  and yet are entirely made of subnodes.
 */

#include "patricia.h"

/*
 *  It might be worth making the types of these arrays configurable
 *  so that machines which are much better at word addressing than
 *  byte addressing have a fair chance:
 */
#if CHAR_BIT == 8
static unsigned char bitmasks[] =
 {0x80, 0x40, 0x20, 0x10, 0x08, 0x04, 0x02, 0x01};
static unsigned char first_bit[] = {
 8,7,6,6,5,5,5,5,4,4,4,4,4,4,4,4,
 3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,
 2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,
 2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,
 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
#else
#error Please define bitmasks and first_bit for CHAR_BIT-wide chars.
#endif

#define test_bit(a,i) ((a)[(i)/CHAR_BIT] & bitmasks[(i)&(CHAR_BIT-1)])

/* Number of equal leading bits */
KEYBITS COMPARE_KEYS (
KEYSTR b1, KEYSTR b2,  /* input bitstrings */
KEYBITS nb)  /* length of input bitstrings, in bits */
{
  KEYBITS n,i;
  /* CLEANME:  No TARGET_CPU_FAMILY* symbol is currently defined, and
   * the i686 is now just an old 32-bit machine, so this can all be
   * reduced to the bit in the #else part...  */
  /*
   *  This loop could possibly be faster if we compared a word at a
   *  time to start (alignment permitting), or could use a machine
   *  instruction like the /370's CLCL or the x86's repe cmpsb.
   *
   *  A standard library routine that worked like memcmp() but gave
   *  you the number of leading chars that agree would do.  Such a
   *  library routine would be optimizable on an x86 to be an inlined
   *  repe cmpsb, as is memcmp().
   *
   *  Meanwhile, for the gcc/x86 case, the following is the unimportant
   *  optimization that occasioned a little exploration of the in-line
   *  assembler (asm) of gcc.
   *
   *  The repe cmpsb instruction replaces the loop in the generic case
   *  (which is 6 instructions on an i686), but stores its results in
   *  a countdown variable n and a boolean saying whether the strings
   *  were equal.  n is initially the number of bytes in the strings
   *  being compared and is identified with ecx, the predecrement
   *  countdown register in the repe cmpsb.
   *
   *  If the strings are equal, the function immediately returns their
   *  common length nb, but otherwise, the byte offset i of the first
   *  disagreeing byte is derived from the countdown.
   */
#if TARGET_CPU_FAMILY_x86 && __GNUC__==4 && __GNUC_MINOR__==5
  KEYSTR t1, t2;  /* dummy output variables for asm, optimized away */
  if (nb==0) return 0;  /* empty strings have no agreeing bits */
  assert (nb%CHAR_BIT == 0);  /* % => mask op for unsigned */
  n = nb/CHAR_BIT;            /* / => shift op for unsigned */
  /*
   *  This is fragile, because there is no guarantee that the condition
   *  code won't be modified again by instructions inserted by gcc
   *  between the asm and the asm goto, but this is the best I can see
   *  to do until asm goto starts supporting outputs (which may never
   *  happen).  In that happy day, all the code will get to go into one
   *  big happy asm goto.  The assembly/machine code generated in the
   *  current (apparently spill-free) context passes inspection, anyway.
   */
  asm (
    "cld\n\t"         /* do comparisons through ascending addresses */
    "repe cmpsb\n"    /* loop comparing strings until inequality found */
    : "=&S"(t1), "=&D"(t2), "=&c"(n)  /* outputs */
    :   "0"(b1),   "1"(b2),   "2"(n)  /* inputs */
    : "cc"  /* cond. code is clobbered (for us, it's really an output) */
  );
  asm goto (
    "jne %l0\n" : : : : unequal  /* jump ahead if inequality found */
  );
  return nb;          /* else return the full bitlength immediately */
unequal:
  i = nb/CHAR_BIT - n - 1;  /* byte index of first inequality */
#elif TARGET_CPU_FAMILY_x86 && defined(__GNUC__)
  KEYSTR t1, t2;  /* dummy output variables for asm, optimized away */
  unsigned char equal;  /* state of ZF (zero flag) after cmpsb */
  if (nb==0) return 0;  /* empty strings have no agreeing bits */
  assert (nb%CHAR_BIT == 0);
  n = nb/CHAR_BIT;
  asm (
    "cld\n\t"         /* do comparisons through ascending addresses */
    "repe cmpsb\n\t"  /* loop comparing strings until inequality found */
    "sete %0\n"       /* set 'equal' if the strings are entirely equal */
    : "=&g"(equal), "=&S"(t1), "=&D"(t2), "=&c"(n)  /* outputs */
    :                 "1"(b1),   "2"(b2),   "3"(n)  /* inputs */
    : "cc"                    /* condition code is clobbered */
  );
  /*
   *  It is a pity we had to introduce the variable 'equal' and do
   *  the 'sete' instruction on it, only to test that here.  Really
   *  we just wanted the condition code tested (no sete), and if the
   *  zero flag is set, jump to the 'return nb;'.  Anyway, at -O2 it
   *  only comes to two (very cheap) extra instructions:  the sete on
   *  a byte register such as %dl, and the test as in test %dl,%dl.
   *
   *  For an amusing or tedious attempt to overcome even that in the
   *  pre-gcc-4.5 case, see the "minor_historical_interest" code below.
   */
  if (equal) return nb;
  i = nb/CHAR_BIT - n - 1;  /* byte index of first inequality */
#elif minor_historical_interest /* not! */
  /*
   *  The following works as a replacement for the above, and avoids
   *  storing the cc, but gives inferior code and is fragile.  On a
   *  test, ecx got spilled (unlike in the above), and the first
   *  attempt, patterned after the "hacked_foo" example in
   *  http://gcc.gnu.org/bugzilla/show_bug.cgi?id=40124,
   *  which has an unconditional jmp right after the conditional,
   *  failed because ecx didn't get restored.  The following in fact
   *  only works because on the 'je' path (to 'return_nb'), ecx is no
   *  longer needed.  The indirect jump ("computed goto") via labels[0]
   *  is of course horrible.
   *
   *  Jumps from asm bits to surrounding C code are to be supported
   *  better in gcc 4.5 (you get to document where the asm code might
   *  jump to, in yet another colon-led section of a construct that
   *  looks like asm but is called 'asm goto'), but even that won't
   *  do it for us until it can be used with asms that have outputs,
   *  which as of this 2010 writing may happen in the indefinite future.
   */
  KEYSTR t1, t2;  /* dummy output variables for asm, optimized away */
  static void* volatile labels[] = {&&compute_i};
  if (nb==0) return 0;  /* empty strings have no agreeing bits */
  assert (nb%CHAR_BIT == 0);
  n = nb/CHAR_BIT;
  asm volatile (
    "cld\n\t"         /* do comparisons through ascending addresses */
    "repe cmpsb\n\t"  /* loop comparing strings until inequality found */
    "je %l[ret_nb]\n"
    : "=&S"(t1), "=&D"(t2), "=&c"(n)  /* outputs */
    :   "0"(b1),   "1"(b2),   "2"(n),
       [ret_nb]"i"(&&return_nb), [comp_i]"i"(&&compute_i)  /* inputs */
    : "cc"                    /* condition code is clobbered */
  );
  goto *labels[0];
return_nb:
  return nb;
compute_i:
  i = nb/CHAR_BIT - n - 1;  /* byte index of first inequality */
#else  /* generic CPU or compiler */
  assert (nb%CHAR_BIT == 0);  /* nb must be a multiple of CHAR_BIT */
  n = nb/CHAR_BIT;
  i = 0;
  while (i<n && b1[i]==b2[i]) i++;
  /* i is now byte offset (index) of first disagreeing byte */
  if (i==n) return nb;  /* all bits agree */
#endif
  /* CHAR_BIT*i + offset of first disagreeing bit at byte i */
  return CHAR_BIT*i + first_bit[(unsigned char)b1[i]^
                                (unsigned char)b2[i]];
}

/* Lookup by key */
void SEARCH_SUBNODE (
SUBNODE **p, SUBNODE **q,  /* q always links to p; updated */
KEYSTR b,             /* search key (subscripted; not modified) */
KEYBITS nb,        /* key length in bits */
ORD *i,            /* returned as ordinal position of key */
KEYBITS *j,        /* returned as a bit position (see Knuth) */
TAGFLAG *tag)      /* tag on the link from q to p (updated) */
{
  SUBNODE *P, *Q; KEYSTR B; ORD I; KEYBITS J; TAGFLAG Tag;
  P = *p; Q = *q; B = b; I = 1; J = 0; Tag = *tag;
  while (!Tag && ((J += P->skip) < nb)) {
    Q = P;
    if (!test_bit (B, J)) {
      P = Q->llink; Tag = Q->ltag;
    } else {
      if (Q->ltag) I += 1;
              else I += Q->llink->size+1;
      P = Q->rlink; Tag = Q->rtag;
    }
  }
  *p = P; *q = Q; *i = I; *j = J; *tag = Tag;
}

/* Slightly faster lookup by key (ordinal position not returned) */
void LOOKUP_SUBNODE (
SUBNODE **p, SUBNODE **q,  /* q always links to p; updated */
KEYSTR b,             /* search key (subscripted; not modified) */
KEYBITS nb,        /* key length in bits */
KEYBITS *j,        /* returned as a bit position (see Knuth) */
TAGFLAG *tag)      /* tag on the link from q to p (updated) */
{
  SUBNODE *P, *Q; KEYSTR B; KEYBITS J; TAGFLAG Tag;
  P = *p; Q = *q; B = b; J = 0; Tag = *tag;
  while (!Tag && ((J += P->skip) < nb)) {
    Q = P;
    if (!test_bit (B, J)) {
      P = Q->llink; Tag = Q->ltag;
    } else {
      P = Q->rlink; Tag = Q->rtag;
    }
  }
  *p = P; *q = Q; *j = J; *tag = Tag;
}

/* Slightly faster version yet (j not returned either) */
void FIND_SUBNODE (
SUBNODE **p, SUBNODE **q,  /* q always links to p; updated */
KEYSTR b,             /* search key (subscripted; not modified) */
KEYBITS nb,        /* key length in bits */
TAGFLAG *tag)      /* tag on the link from q to p (updated) */
{
  SUBNODE *P, *Q; KEYSTR B; KEYBITS J; TAGFLAG Tag;
  P = *p; Q = *q; B = b; J = 0; Tag = *tag;
  while (!Tag && ((J += P->skip) < nb)) {
    Q = P;
    if (!test_bit (B, J)) {
      P = Q->llink; Tag = Q->ltag;
    } else {
      P = Q->rlink; Tag = Q->rtag;
    }
  }
  *p = P; *q = Q; *tag = Tag;
}

/*
 *  In practice, could use an even faster version of the above which
 *  does not return the tag value, and operates with just one subnode
 *  pointer parameter.
 */

/* Lookup by ordinal position */
void INDEX_SUBNODE (
SUBNODE **p, SUBNODE **q,   /* initialize as for search_subnode; updated */
ORD rank,           /* index to use as selector (lowest is 1) */
TAGFLAG *tag)       /* as for search_subnode */
{
  SUBNODE *P, *Q; TAGFLAG Tag;
  ORD i, left_size;
  P = *p; Q = *q; Tag = *tag;
  assert (rank >= 1);
  assert (rank <= Q->size);
  i = 1;
  while (!Tag) {
    Q = P;
    if (P->ltag) left_size = i; else left_size = i + P->llink->size;
    if (rank <= left_size) {
      P = Q->llink; Tag = Q->ltag;
    }
    else {
      i = left_size + 1;
      P = Q->rlink; Tag = Q->rtag;
    }
  }
  assert (i == rank);
  *p = P; *q = Q; *tag = Tag;
}

/*
 *  I think a somewhat more efficient version of the above would be
 *  useful too.  I'm not sure that it needs to mess around with
 *  updating a Tag variable the way it does either; surely you just
 *  have to inspect flags and keep track of the size to the left?
 *  Of course, some callers may want the value of the tag from q to p
 *  too.
 */

/* Insert subnode */
void INSERT_SUBNODE (
SUBNODE *p, SUBNODE *q,   /* as made up by search_subnode on partial key */
SUBNODE *r,       /* the subnode to insert */
KEYBITS j, KEYBITS l)     /* j from search_subnode; l offset of differing bit */
{
  SUBNODE *n;
  TAGFLAG t;
  r->parent = q;
  if (q->size == 0) {
    assert (p == q);
    assert (p == q->llink);
    assert (p->ltag == 1);
    assert (p->parent == NULL);
    q->ltag = 0;
    q->llink = r;
    r->ltag = 1;
    r->llink = r;
    r->rtag = 1;
    r->rlink = p;
    r->skip = 0;
    q->skip = 0;
    r->size = 1;
    q->size = 1;
    return;
  }
  if (q == p->parent) p->parent = r;
  if (p == q->llink) {
    t = q->ltag;
    q->ltag = 0;
    q->llink = r;
  }
  else {
    t = q->rtag;
    q->rtag = 0;
    q->rlink = r;
  }
  if (!test_bit(r->k->bstring,l)) {
    r->ltag = 1;
    r->llink = r;
    r->rtag = t;
    r->rlink = p;
  }
  else {
    r->rtag = 1;
    r->rlink = r;
    r->ltag = t;
    r->llink = p;
  }
  if (t) {
    r->skip = l-j;
    r->size = 1;
  }
  else {
    r->skip = l-j + p->skip;
    p->skip = j-l;
    r->size = 1 + p->size;
  }
  for (n = q; n; n = n->parent) n->size++;
}

/* Delete subnode */
void DELETE_SUBNODE (
SUBNODE *p, SUBNODE *q)   /* (input) q tag-linked to p; (output) delete p */
{
  SUBNODE *pp, *s, *f;
  TAGFLAG t;
  pp = q->parent;
  if (p == q->llink) {
    assert (q->ltag);
    s = q->rlink;
    t = q->rtag;
  }
  else {
    assert (q->rtag);
    s = q->llink;
    t = q->ltag;
  }
  if (q == pp->llink) {
    pp->llink = s;
    pp->ltag = t;
  }
  else {
    pp->rlink = s;
    pp->rtag = t;
  }
  if (!t) {
    s->parent = pp;
    s->skip += q->skip;
  }
  for (pp = q->parent; pp; pp = pp->parent) pp->size--;
  if (p != q) {
    f = p->parent;
    if (p == f->llink) f->llink = q;
    else               f->rlink = q;
    q->llink = p->llink;
    q->ltag  = p->ltag;
    q->rlink = p->rlink;
    q->rtag  = p->rtag;
    q->parent = f;
    if      (q == q->llink) q->ltag = 1;
    else if (q == q->rlink) q->rtag = 1;
    if (!q->ltag) (q->llink)->parent = q;
    if (!q->rtag) (q->rlink)->parent = q;
    q->skip = p->skip;
    q->size = p->size;
  }
}
