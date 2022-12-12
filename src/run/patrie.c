/*  ===  Convenient PATRICIA (SETL run-time version)  ==============  */

/*  $Id: patrie.c,v 1.8 2010/10/14 18:34:43 bacon Exp $  */

/*  Free software (c) dB - see file COPYING for license (GPL).  */

/*
 *  This file is not intended to be compiled directly, but rather
 *  to be #included by another file (see for example "patty.c")
 *  that sets up certain definitions before doing the #include of
 *  "patrie.h" and then of this file.  It would have been better,
 *  in retrospect, to have insisted on some "patuser.h" file that
 *  would be #included here just before an #include of "patrie.h".
 *  The present mechanism works okay, it's just a little weird.
 */

/*
 *  This implementation supports relocating garbage collectors by
 *  conditionally including code to declare, attach, and release
 *  "handles" on memory block pointers using the conventions of
 *  the SETL interpreter.  You #define USE_HANDLES before the
 *  #include of this file to get that code.
 */

static SUBNODE *new_subnode(void);
static SUBNODE *copy_subtree (SUBNODE *p, SUBNODE *parent);
static int subtree_cmp (SUBNODE *p, SUBNODE *q);

/*
 *  The top-level subnode in a PATRIE trie is labelled with PATRIE_TYPE,
 *  and the rest are labelled with SUBNODE_TYPE (if TYPE is #defined).
 *
 *  The only meaningful fields in the top-level subnode are 'llink',
 *  'ltag', and 'size', but the rest should be set to NULL or 0 as
 *  appropriate.  All fields are meaningful in other subnodes.  An
 *  empty trie (size 0) consists of just a top-level subnode,
 *  "tag-linked to" itself (see PATCRE).
 */

static SUBNODE *new_subnode(void) {
  SUBNODE *r = ALLOC_SUBNODE();
#ifdef TYPE
  /* Local client fills in r->type as PATRIE_TYPE or SUNODE_TYPE */
#endif
#ifdef DATA
  r->d = 0;
#endif
  r->k = NULL;
  r->llink = NULL;
  r->rlink = NULL;
  r->parent = NULL;
  r->skip = 0;
  r->size = 0;
  r->ltag = 0;
  r->rtag = 0;
  return r;
}

bool PATCRE (PATRIE *tp) {
  PATRIE t = new_subnode();
#ifdef TYPE
  t->type = PATRIE_TYPE;
#endif
  t->llink = t;  /* self-link */
  t->ltag = 1;  /* "tagged" */
  *tp = t;
  return true;
}

/*
 *  By rights, I should provide a general traverser here, particularly
 *  for clients who want to do finalization on node deallocation or
 *  whatever.  But until such clients actually appear...
 */

bool PATCPY (PATRIE *tp, PATRIE t) {
#ifdef USE_HANDLES
  HANDLE ht;
#endif
  PATRIE r;
#ifdef TYPE
  assert (t->type == PATRIE_TYPE);
#endif
  assert (t->k == NULL);
  assert (t->parent == NULL);
#ifdef USE_HANDLES
  ht = ref(t);
#endif
  r = new_subnode();
#ifdef TYPE
  r->type = PATRIE_TYPE;
#endif
  if (t->size == 0) {
    assert (t == t->llink);
    assert (t->ltag);
    r->llink = r;
    r->ltag = 1;
    r->size = 0;
  } else {
#ifdef USE_HANDLES
    HANDLE hr;
#endif
    SUBNODE *p;
    assert (t != t->llink);
    assert (!t->ltag);
#ifdef USE_HANDLES
    hr = ref(r);
#endif
    p = copy_subtree(t->llink, r);
    r->llink = p;
    r->ltag = 0;
    r->size = t->size;
#ifdef USE_HANDLES
    retire(hr);
#endif
  }
#ifdef USE_HANDLES
  retire(ht);
#endif
  *tp = r;
  return true;
}

static SUBNODE *copy_subtree (SUBNODE *p, SUBNODE *parent) {
#ifdef USE_HANDLES
  HANDLE hp;
  HANDLE ha;
  HANDLE hr;
#endif
  SUBNODE *r;
  KEY *k;
#ifdef DATA
  DATA d;
#endif
#ifdef TYPE
  assert (p->type == SUBNODE_TYPE);
#endif
  assert (p->k);
  assert (p->llink);
  assert (p->rlink);
#ifdef USE_HANDLES
  hp = ref(p);
  ha = ref(parent);
#endif
  r = new_subnode();
#ifdef TYPE
  r->type = SUBNODE_TYPE;
#endif
#ifdef USE_HANDLES
  hr = ref(r);
#endif
  k = COPY_KEY(p->k);  /* retain simple lhs here please! */
  r->k = k;
#ifdef DATA
  d = COPY_DATA(p->d);  /* retain simple lhs here please! */
  r->d = d;
#endif
  r->parent = parent;  /* need parent links "early" */
  if (!p->ltag) {
    SUBNODE *t = copy_subtree(p->llink, r);
    r->llink = t;
  } else {
    /*
     *  This while-loop may appear to suggest that this way of copying
     *  a subtree is not linear in the size of the subtree, but note
     *  that all nodes in a subtree must be tag-linked to from some
     *  descendant in that subtree, and in fact the subtree will only
     *  have one tagged link available to point upwards and out of the
     *  subtree.  So although I have not carried through the analysis
     *  formally, there is a strong hint of amortizing out to perfectly
     *  linear in this pattern!  (It may be a fairly straightforward
     *  structural induction, since you can take the subtrees as small
     *  as you want; I just haven't followed this up at all.)
     */
    SUBNODE *pp = p;
    SUBNODE *rp = r;
    while (pp != p->llink) {  /* find the tag-linked-to ancestor of p */
      pp = pp->parent;
      rp = rp->parent;
    }
    r->llink = rp;  /* give r the corresponding up-link */
  }
  if (!p->rtag) {
    SUBNODE *t = copy_subtree(p->rlink, r);
    r->rlink = t;
  } else {
    SUBNODE *pp = p;
    SUBNODE *rp = r;
    while (pp != p->rlink) {  /* find the tag-linked-to ancestor of p */
      pp = pp->parent;
      rp = rp->parent;
    }
    r->rlink = rp;  /* give r the corresponding up-link */
  }
  r->skip = p->skip;
  r->size = p->size;
  r->ltag = p->ltag;
  r->rtag = p->rtag;
#ifdef USE_HANDLES
  retire(hr);
  retire(ha);
  retire(hp);
#endif
  return r;
}

/*
 *  The sense of this is backwards from "equal" in that 0 is returned
 *  if the patries ARE equal in value (like the C library routines
 *  with "cmp" at the ends of their names):
 */
int PATCMP (PATRIE t, PATRIE u) {
#ifdef TYPE
  assert (t->type == PATRIE_TYPE);
  assert (u->type == PATRIE_TYPE);
#endif
  if (t->size != u->size) return 1;  /* not equal */
  if (t->size == 0) {
    assert (t->ltag);
    assert (t == t->llink);
    assert (u->ltag);
    assert (u == u->llink);
    return 0;  /* equal */
  }
  return subtree_cmp (t->llink, u->llink);
}

static int subtree_cmp (SUBNODE *p, SUBNODE *q) {
#ifdef USE_HANDLES
  HANDLE hp;
  HANDLE hq;
#endif
  int r;
#ifdef TYPE
  assert (p->type == SUBNODE_TYPE);
  assert (q->type == SUBNODE_TYPE);
#endif
  if (p->size != q->size) return 1;  /* not equal */
  if (p->ltag != q->ltag) return 1;  /* not equal */
  if (p->rtag != q->rtag) return 1;  /* not equal */
#ifdef USE_HANDLES
  hp = ref(p);
  hq = ref(q);
#endif
  r = 0;  /* equal until proven otherwise */
  if (p->ltag) {
    if (!EQUAL_KEY (p->llink->k, q->llink->k) ||
        !EQUAL_DATA(p->llink->d, q->llink->d)) r = 1;  /* not equal */
  } else {  /* !p->ltag */
    if (subtree_cmp(p->llink, q->llink) != 0) r = 1;  /* not equal */
  }
  if (r == 0) {
    if (p->rtag) {
      if (!EQUAL_KEY (p->rlink->k, q->rlink->k) ||
          !EQUAL_DATA(p->rlink->d, q->rlink->d)) r = 1;  /* not equal */
    } else {  /* !p->rtag */
      if (subtree_cmp(p->rlink, q->rlink) != 0) r = 1;  /* not equal */
    }
  }
#ifdef USE_HANDLES
  retire(hq);
  retire(hp);
#endif
  return r;
}

bool STRSEE (PATRIE t, KEYSTR s, SUBNODE **d) {
  SUBNODE *p,*q;
  TAGFLAG tag;
  KEYBITS l,n;
#ifdef TYPE
  assert (t->type == PATRIE_TYPE);
#endif
  q = t->llink;
  p = q->llink;
  tag = q->ltag;
  n = (strlen(s)+1) * CHAR_BIT;
  FIND_SUBNODE (&p,&q,s,n,&tag);
  if (p == t) return false;
  if (p->k->length != n) return false;
  l = COMPARE_KEYS (s, p->k->bstring, n);
  if (l != n) return false;
#ifdef TYPE
  assert (p->type == SUBNODE_TYPE);
#endif
  *d = p;
  return true;
}

bool KEYSEE (PATRIE t, KEY *k, SUBNODE **d) {
  SUBNODE *p,*q;
  TAGFLAG tag;
  KEYBITS l,n;
#ifdef TYPE
  assert (t->type == PATRIE_TYPE);
#endif
#ifdef TYPE
  assert (k->type == KEY_TYPE);
#endif
  q = t->llink;
  p = q->llink;
  tag = q->ltag;
  n = k->length;
  FIND_SUBNODE (&p,&q,k->bstring,n,&tag);
  if (p == t) return false;
  if (p->k->length != n) return false;
  l = COMPARE_KEYS (k->bstring, p->k->bstring, n);
  if (l != n) return false;
#ifdef TYPE
  assert (p->type == SUBNODE_TYPE);
#endif
  *d = p;
  return true;
}

bool ORDSEE (PATRIE t, ORD rank, SUBNODE **d) {
  SUBNODE *p,*q;
  TAGFLAG tag;
#ifdef TYPE
  assert (t->type == PATRIE_TYPE);
#endif
  if (rank >= 1 && rank <= t->size) {
    q = t->llink;
    p = q->llink;
    tag = q->ltag;
    INDEX_SUBNODE (&p,&q,rank,&tag);
#ifdef TYPE
    assert (p->type == SUBNODE_TYPE);
#endif
    *d = p;
    return true;
  } else {
    return false;
  }
}

bool STRSUB (PATRIE t, KEYSTR s, SUBNODE **d) {
  SUBNODE *p,*q,*r;
  KEY *k;
#ifdef USE_HANDLES
  HANDLE hp,hq,hr;
#endif
  TAGFLAG tag;
  KEYBITS j,l,n,m;
  long nbytes;
#ifdef TYPE
  assert (t->type == PATRIE_TYPE);
#endif
  q = t->llink;
  p = q->llink;
  tag = q->ltag;
  nbytes = strlen(s)+1;
  n = nbytes * CHAR_BIT;
  LOOKUP_SUBNODE (&p,&q,s,n,&j,&tag);
  if (p == t) {
    assert (p == q);
    assert (p->size == 0);
    assert (p->ltag);
    assert (j == 0);
    l = 0;
  } else {
    m = p->k->length;
    l = COMPARE_KEYS(s, p->k->bstring, n<m ? n : m);
    if (l == n) {
#ifdef TYPE
      assert (p->type == SUBNODE_TYPE);
#endif
      *d = p;
      return true;
    }
    q = t->llink;
    p = q->llink;
    tag = q->ltag;
    LOOKUP_SUBNODE (&p,&q,s,l,&j,&tag);
  }
#ifdef USE_HANDLES
  hp = ref(p);
  hq = ref(q);
#endif
  r = new_subnode();
#ifdef TYPE
  r->type = SUBNODE_TYPE;
#endif
#ifdef USE_HANDLES
  hr = ref(r);
#endif
  k = ALLOC_KEY(nbytes);
  assert (k);
#ifdef TYPE
  k->type = KEY_TYPE;
#endif
  k->length = n;
  memcpy(k->bstring,s,nbytes);
  r->k = k;
  INSERT_SUBNODE(p,q,r,j,l);
#ifdef USE_HANDLES
  retire(hr);
  retire(hq);
  retire(hp);
#endif
  *d = r;
  return false;
}

bool KEYSUB (PATRIE t, KEY *k, SUBNODE **d) {
  SUBNODE *p,*q,*r;
#ifdef USE_HANDLES
  HANDLE hk,hp,hq;
#endif
  TAGFLAG tag;
  KEYBITS j,l,n,m;
#ifdef TYPE
  assert (t->type == PATRIE_TYPE);
#endif
#ifdef TYPE
  assert (k->type == KEY_TYPE);
#endif
  q = t->llink;
  p = q->llink;
  tag = q->ltag;
  n = k->length;
  LOOKUP_SUBNODE (&p,&q,k->bstring,n,&j,&tag);
  if (p == t) {
    assert (p == q);
    assert (p->size == 0);
    assert (p->ltag);
    assert (j == 0);
    l = 0;
  } else {
    m = p->k->length;
    l = COMPARE_KEYS(k->bstring, p->k->bstring, n<m ? n : m);
    if (l == n) {
#ifdef TYPE
      assert (p->type == SUBNODE_TYPE);
#endif
      *d = p;
      return true;
    }
    q = t->llink;
    p = q->llink;
    tag = q->ltag;
    LOOKUP_SUBNODE (&p,&q,k->bstring,l,&j,&tag);
  }
#ifdef USE_HANDLES
  hk = ref(k);
  hp = ref(p);
  hq = ref(q);
#endif
  r = new_subnode();
#ifdef TYPE
  r->type = SUBNODE_TYPE;
#endif
  r->k = k;
  INSERT_SUBNODE(p,q,r,j,l);
#ifdef USE_HANDLES
  retire(hq);
  retire(hp);
  retire(hk);
#endif
  *d = r;
  return false;
}

/*
 *  I free the key automatically in this routine, since I probably
 *  allocated it originally:
 */
bool STRDEL (PATRIE t, KEYSTR s) {
  SUBNODE *p,*q;
  TAGFLAG tag;
  KEYBITS l,n;
#ifdef TYPE
  assert (t->type == PATRIE_TYPE);
#endif
  q = t->llink;
  p = q->llink;
  tag = q->ltag;
  n = (strlen(s)+1) * CHAR_BIT;
  FIND_SUBNODE (&p,&q,s,n,&tag);
  if (p == t) return false;
  if (p->k->length != n) return false;
  l = COMPARE_KEYS (s, p->k->bstring, n);
  if (l != n) return false;
  DELETE_SUBNODE (p,q);
  FREE_KEY(p->k);
  FREE_SUBNODE(p);
  return true;
}

/*
 *  Since I probably did not allocate the key for you in the following
 *  case, I don't free it either.  Caller beware:  don't free the key
 *  before calling this routine.
 */
bool KEYDEL (PATRIE t, KEY *k) {
  SUBNODE *p,*q;
  TAGFLAG tag;
  KEYBITS l,n;
#ifdef TYPE
  assert (t->type == PATRIE_TYPE);
#endif
#ifdef TYPE
  assert (k->type == KEY_TYPE);
#endif
  q = t->llink;
  p = q->llink;
  tag = q->ltag;
  n = k->length;
  FIND_SUBNODE (&p,&q,k->bstring,n,&tag);
  if (p == t) return false;
  if (p->k->length != n) return false;
  l = COMPARE_KEYS (k->bstring, p->k->bstring, n);
  if (l != n) return false;
  DELETE_SUBNODE (p,q);
  FREE_SUBNODE(p);
  return true;
}

bool ORDDEL (PATRIE t, ORD rank) {
  SUBNODE *p,*q;
  TAGFLAG tag;
#ifdef TYPE
  assert (t->type == PATRIE_TYPE);
#endif
  if (rank >= 1 && rank <= t->size) {
    q = t->llink;
    p = q->llink;
    tag = q->ltag;
    INDEX_SUBNODE (&p,&q,rank,&tag);
    DELETE_SUBNODE (p,q);
    FREE_KEY(p->k);
    FREE_SUBNODE(p);
    return true;
  } else {
    return false;
  }
}

bool STRORD (PATRIE t, KEYSTR s, ORD *rank) {
  SUBNODE *p,*q;
  TAGFLAG tag;
  KEYBITS j,l,n;
  ORD i;
#ifdef TYPE
  assert (t->type == PATRIE_TYPE);
#endif
  q = t->llink;
  p = q->llink;
  tag = q->ltag;
  n = (strlen(s)+1) * CHAR_BIT;
  SEARCH_SUBNODE (&p,&q,s,n,&i,&j,&tag);
  if (p == t) return false;
  if (p->k->length != n) return false;
  l = COMPARE_KEYS (s, p->k->bstring, n);
  if (l != n) return false;
  *rank = i;
  return true;
}

bool KEYORD (PATRIE t, KEY *k, ORD *rank) {
  SUBNODE *p,*q;
  TAGFLAG tag;
  KEYBITS j,l,n;
  ORD i;
#ifdef TYPE
  assert (t->type == PATRIE_TYPE);
#endif
#ifdef TYPE
  assert (k->type == KEY_TYPE);
#endif
  q = t->llink;
  p = q->llink;
  tag = q->ltag;
  n = k->length;
  SEARCH_SUBNODE (&p,&q,k->bstring,n,&i,&j,&tag);
  if (p == t) return false;
  if (p->k->length != n) return false;
  l = COMPARE_KEYS (k->bstring, p->k->bstring, n);
  if (l != n) return false;
  *rank = i;
  return true;
}

ORD PATSIZ (const PATRIE t) {
#ifdef TYPE
  assert (t->type == PATRIE_TYPE);
#endif
  return t->size;
}
