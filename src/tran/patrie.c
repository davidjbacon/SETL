/*  ===  Convenient PATRICIA (SETL translator version)  ============  */

/*  $Id: patrie.c,v 1.8 2020/12/18 23:53:42 setlorg Exp $  */

/*  Free software (c) dB - see file COPYING for license (GPL).  */

/*
 *  This file is not intended to be compiled directly, but rather
 *  to be #included by another file (see for example "patty.c")
 *  that sets up certain definitions before doing the #include of
 *  "patrie.h" and then of this file.  It would have been better,
 *  in retrospect, to have insisted on some "patuser.h" file that
 *  would be #included here just before an #include of "patrie.h".
 *  The present mechanism ain't broke, just weird.
 */

bool CREATE (t)
PATRIE *t;
{
  SUBNODE *h;
  if (!(h = (SUBNODE *)MALLOC(sizeof(SUBNODE)))) return false;
  init_subnode(h);
  *t = (PATRIE)h;
  return true;
}

bool DELETE (t)
PATRIE *t;
{
  /*
   *  Should improve this to free up ALL the subnodes in a
   *  non-empty patrie.  On the other hand, the client may want
   *  to traverse the whole tree to get rid of the data first
   *  anyway.  I really should provide some efficient
   *  tree-traverser, where you communicate through some kind
   *  of subnode pointer handle.  None of this matters for the
   *  SETL translator, however (see "mem.c"), nor for clients
   *  who don't mind the overhead of repeatedly deleting the
   *  first element (say) of the tree until it's empty before
   *  deleting the tree itself.
   */
  SUBNODE *h;
  h = (SUBNODE *)*t;
  FREE(h);
  *t = (PATRIE)NULL;
  return true;
}

bool DELKEY (t, key)
PATRIE t;
const char *key;
{
  SUBNODE *h,*p,*q;
  TAGFLAG tag;
  KEYBITS l,n;
  h = (SUBNODE *)t;
  q = h->llink;
  p = q->llink;
  tag = q->ltag;
  n = (strlen(key)+1)*CHAR_BIT;
  find_subnode (&p,&q,(KEY)key,n,&tag);
  if (p == h) return false;
  if (p->keylen != n) return false;
  l = compare_keys ((KEY)key, p->key, n);
  if (l != n) return false;
  delete_subnode (p,q);
  return true;
  /* Space is not freed here.  */
}

bool DELORD (t, ord)
PATRIE t;
ORD ord;
{
  SUBNODE *h,*p,*q;
  TAGFLAG tag;
  h = (SUBNODE *)t;
  if (ord >= 1 && ord <= h->size) {
    q = h->llink;
    p = q->llink;
    tag = q->ltag;
    index_subnode (&p,&q,ord,&tag);
    delete_subnode (p,q);
    return true;
  } else {
    return false;
  }
  /* Space is not freed here.  */
}

bool LOOKUP (t, key, data)
PATRIE t;
const char *key;
DATA **data;
{
  SUBNODE *h,*p,*q;
  TAGFLAG tag;
  KEYBITS l,n;
  h = (SUBNODE *)t;
  q = h->llink;
  p = q->llink;
  tag = q->ltag;
  n = (strlen(key)+1)*CHAR_BIT;
  find_subnode (&p,&q,(KEY)key,n,&tag);
  if (p == h) return false;
  if (p->keylen != n) return false;
  l = compare_keys ((KEY)key, p->key, n);
  if (l != n) return false;
  *data = &p->data;
  return true;
}

bool KEYSUB (t, key, data)
PATRIE t;
const char *key;
DATA **data;
{
  SUBNODE *h,*p,*q,*r;
  TAGFLAG tag;
  KEYBITS j,l,n;
  unsigned long keybytes;
  void *k;
  h = (SUBNODE *)t;
  q = h->llink;
  p = q->llink;
  tag = q->ltag;
  keybytes = strlen(key)+1;
  n = keybytes*CHAR_BIT;
  lookup_subnode (&p,&q,(KEY)key,n,&j,&tag);
  if (p == h) {
    assert (p == q);
    assert (p->size == 0);
    assert (p->ltag);
    assert (j == 0);
    l = 0;
    goto insert;
  } else {
    l = compare_keys((KEY)key,p->key,(n < p->keylen ? n : p->keylen));
    if (l == n) {
      *data = &p->data;
      return true;
    }
    q = h->llink;
    p = q->llink;
    tag = q->ltag;
    lookup_subnode (&p,&q,(KEY)key,l,&j,&tag);
  }
insert:
  r = (SUBNODE *)MALLOC(sizeof(SUBNODE));
  k = MALLOC(keybytes);
  memcpy(k,key,keybytes);
  r->key = (KEY)k;
  r->keylen = n;
  insert_subnode(p,q,r,j,l);
  *data = &r->data;
  return false;
}

bool ORDSUB (t, ord, data)
PATRIE t;
ORD ord;
DATA **data;
{
  SUBNODE *h,*p,*q;
  TAGFLAG tag;
  h = (SUBNODE *)t;
  if (ord >= 1 && ord <= h->size) {
    q = h->llink;
    p = q->llink;
    tag = q->ltag;
    index_subnode (&p,&q,ord,&tag);
    *data = &p->data;
    return true;
  } else {
    return false;
  }
}

bool KEYORD (t, key, ord)
PATRIE t;
const char *key;
ORD *ord;
{
  SUBNODE *h,*p,*q;
  TAGFLAG tag;
  KEYBITS j,l,n;
  ORD i;
  h = (SUBNODE *)t;
  q = h->llink;
  p = q->llink;
  tag = q->ltag;
  n = (strlen(key)+1)*CHAR_BIT;
  search_subnode (&p,&q,(KEY)key,n,&i,&j,&tag);
  if (p == h) return false;
  if (p->keylen != n) return false;
  l = compare_keys ((KEY)key, p->key, n);
  if (l != n) return false;
  *ord = i;
  return true;
}

bool ORDKEY (t, ord, key)
PATRIE t;
ORD ord;
const char **key;
{
  SUBNODE *h,*p,*q;
  TAGFLAG tag;
  h = (SUBNODE *)t;
  if (ord >= 1 && ord <= h->size) {
    q = h->llink;
    p = q->llink;
    tag = q->ltag;
    index_subnode (&p,&q,ord,&tag);
    *key = (const char *)p->key;
    return true;
  } else {
    return false;
  }
}

ORD SIZE (t)
PATRIE t;
{
  SUBNODE *h;
  h = (SUBNODE *)t;
  return h->size;
}
