/*  --  Low-level PATRICIA interface (SETL translator version)  --  */

/*  $Id: patricia.h,v 1.5 2010/12/04 07:18:23 bacon Exp $  */

/*  Free software (c) dB - see file COPYING for license (GPL).  */

/*
 *  Define DATA as a typename before doing the inclusion if you
 *  want the "payload" to be something other than an int.
 *
 *  Likewise TYPE if you want to define your type codes using
 *  something other than int (like an enumeration type).
 */

#ifndef PATRICIA_DEFS
#define PATRICIA_DEFS

#ifndef TYPE
#define TYPE int /* warning - not a typedef! */
#endif
#ifndef DATA
#define DATA int /* warning - not a typedef! */
#endif

typedef const unsigned char *KEY;    /* please coerce keys to type KEY */
typedef unsigned long      KEYBITS;  /* measure of key lengths, offsets */
typedef unsigned long      ORD;      /* ordinal index of key, 1 origin */
typedef unsigned char      TAGFLAG;  /* boolean */
#define SUBNODE struct subnode      /* what trees are made of */

/* Internal structure of a Patricia tree node */
SUBNODE {
  TYPE     type;        /* for those who like self-identifying data */
  DATA     data;
  KEY      key;
  KEYBITS  keylen;
  SUBNODE  *llink, *rlink, *parent;
  KEYBITS  skip;
  ORD      size;
  TAGFLAG  ltag, rtag;
};

/* Function prototypes:  */

KEYBITS compare_keys (KEY b1, KEY b2, KEYBITS bmax);

extern void init_subnode (SUBNODE *p);

extern void search_subnode (SUBNODE **p, SUBNODE **q, KEY b, KEYBITS bmax,
                             ORD *i, KEYBITS *j, TAGFLAG *tag);

extern void lookup_subnode (SUBNODE **p, SUBNODE **q, KEY b, KEYBITS bmax,
                                     KEYBITS *j, TAGFLAG *tag);

extern void find_subnode (SUBNODE **p, SUBNODE **q, KEY b, KEYBITS bmax,
                                                 TAGFLAG *tag);

extern void index_subnode (SUBNODE **p, SUBNODE **q, ORD rank,
                                                 TAGFLAG *tag);

extern void insert_subnode (SUBNODE *p, SUBNODE *q, SUBNODE *r,
                             KEYBITS j, KEYBITS l);

extern void delete_subnode (SUBNODE *p, SUBNODE *q);

#ifndef NULL
#define NULL 0
#endif

#endif
