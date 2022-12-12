/*  ===  The essence of PATRICIA (SETL translator version)  ========  */

/*  $Id: patricia.c,v 1.5 2010/10/02 15:01:10 bacon Exp $  */

/*  Free software (c) dB - see file COPYING for license (GPL).  */

/*
 *  For historical reasons, Patricia trees are called "subnodes"
 *  and yet are entirely made of subnodes.
 */

/*
 *  TODO:  replace this and ../run/patricia.c with a lightly
 *  parameterized, unified version, and try generally to make the
 *  interface a little less funky.
 */

#include "patricia.h"
 
/*
 *  It might be worth making the types of these arrays configurable
 *  so that machines which are much better at "word" addressing than
 *  "byte" addressing have a fair chance:
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

/* Bit offset of first disagreeing bit (0 means none agree) */
KEYBITS compare_keys (b1, b2, nb)
KEY b1, b2;
KEYBITS nb;
{
  KEYBITS n,i;
  n = nb/CHAR_BIT;
  i = 0;
  while (i<n && b1[i]==b2[i]) i++;
  /* i is now byte offset (index) of first disagreeing byte */
  if (i==n) return nb;
  /* CHAR_BIT*i + offset of first disagreeing bit in b1[i] vs. b2[i] */
  return CHAR_BIT*i + first_bit[b1[i]^b2[i]];
}

/*
 *  The following comments are just here to remind me not to try to
 *  get rid of the dummy "header" subnode, as I seem to keep doing:
 * 
 *  To use the following routines, you have to coddle them a bit.
 *  The deal is, in Knuth-style Patricia, which I am going "back" to,
 *  the top subnode is special, and empty Patricia trees no longer
 *  have a representation here.  So please use init_subnode only
 *  when you're actually ready to put in the first key, and don't
 *  then call insert_subnode in that case.
 * 
 *  In all cases of insertion, you have to fill in the key yourself.
 *  For insert_subnode, this should be done before the call is made,
 *  so that I can use the key in the subnode to figure out where to
 *  put the subnode in the tree.
 * 
 *  For deletion, if there is only one subnode left, please delete
 *  it yourself (easy)--don't call delete_subnode; it will get
 *  confused (try to chase a null pointer).
 * 
 *  NO! - that won't work.  If you delete the root, the caller's
 *  header record will have to be changed.  That's not the end
 *  of the world, but slightly messy...I think I'm going to leave it
 *  alone.  I still can't help feeling sometimes that there's a more
 *  elegant deletion algorithm.
 * 
 *  Anyway, I think it would be best to clean up the insertion
 *  interface slightly nonetheless.  If the key just comes in as part
 *  of the subnode, I can simply have insert_subnode ignore it when
 *  it's the first one going into the initially empty tree.
 * 
 *  Okay, so insert_subnode now inserts properly into an initially
 *  empty tree as a special case.
 */

/* Initialize a Patricia tree (this is a very necessary "dummy" subnode) */
void init_subnode (p)
SUBNODE *p;
{
  p->llink = p;
  p->ltag = 1;
  p->parent = NULL;
  p->size = 0;
}
 
/* Lookup by key */
void search_subnode (p, q, b, nb, i, j, tag)
SUBNODE **p, **q;  /* q always links to p; updated */
KEY b;             /* search key (subscripted; not modified) */
KEYBITS nb;        /* key length in bits */
ORD *i;            /* returned as ordinal position of key */
KEYBITS *j;        /* returned as a bit position (see Knuth) */
TAGFLAG *tag;      /* tag on the link from q to p (updated) */
{
  SUBNODE *P, *Q; KEY B; ORD I; KEYBITS J; TAGFLAG Tag;
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
void lookup_subnode (p, q, b, nb, j, tag)
SUBNODE **p, **q;  /* q always links to p; updated */
KEY b;             /* search key (subscripted; not modified) */
KEYBITS nb;        /* key length in bits */
KEYBITS *j;        /* returned as a bit position (see Knuth) */
TAGFLAG *tag;      /* tag on the link from q to p (updated) */
{
  SUBNODE *P, *Q; KEY B; KEYBITS J; TAGFLAG Tag;
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
void find_subnode (p, q, b, nb, tag)
SUBNODE **p, **q;  /* q always links to p; updated */
KEY b;             /* search key (subscripted; not modified) */
KEYBITS nb;        /* key length in bits */
TAGFLAG *tag;      /* tag on the link from q to p (updated) */
{
  SUBNODE *P, *Q; KEY B; KEYBITS J; TAGFLAG Tag;
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
void index_subnode (p, q, rank, tag)
SUBNODE **p, **q;   /* initialize as for search_subnode; updated */
ORD rank;           /* index to use as selector (lowest is 1) */
TAGFLAG *tag;       /* as for search_subnode */
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
void insert_subnode (p, q, r, j, l)
SUBNODE *p, *q;   /* as made up by search_subnode on partial key */
SUBNODE *r;       /* the subnode to insert */
KEYBITS j, l;     /* j from search_subnode; l offset of differing bit */
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
  if (!test_bit(r->key,l)) {
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
void delete_subnode (p, q)
SUBNODE *p, *q;   /* (input) q tag-linked to p; (output) delete p */
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
