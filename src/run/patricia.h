/*  --  Low-level PATRICIA interface (SETL run-time version)  --  */

/*  $Id: patricia.h,v 1.7 2010/10/02 15:01:08 bacon Exp $  */

/*  Free software (c) dB - see file COPYING for license (GPL).  */

/*
 *  You can pre-#define TYPE and DATA for carriage in Patricia
 *  "subnodes" (what a Patricia tree or trie is made of).
 * 
 *  In all cases, it is highly recommended that you also have
 *  declarations like:
 * 
 *      typedef TYPE your_type_type;
 *      typedef DATA your_data_type;
 * 
 *  (This is just because it is usually best to make declarations
 *  using typedef'd types if possible, especially for pointer types.)
 * 
 *  The default type designation for the structure which represents
 *  a Patricia subnode is given by:
 * 
 *      #define SUBNODE struct subnode_struct
 * 
 *  You can pre-#define this too (only useful if you really want
 *  multiple "versions" of Patricia to coexist in the same program,
 *  in which case you have to introduce all your own versions of
 *  the routine names by #defines).
 * 
 *  Once again, it is recommended that you also have a declaration
 *  like
 * 
 *      typedef SUBNODE your_subnode_type;
 * 
 *  (especially if you want pointers to tries).
 *  
 *  You can also pre-#define KEY.  Note that the KEY type is assumed
 *  to have a field called 'bstring' that can be subscripted to give
 *  byte (signed or unsigned char) references.
 */

/* Types */
#ifndef KEYSTR
#define KEYSTR  simple_string_key_pointer_type
typedef char    *simple_string_key_pointer_type;
#endif
#ifndef KEY
#define KEY     struct key_struct
KEY {
#ifdef TYPE
  TYPE type;
#endif
  char bstring[1];  /* this is actually "flexible" */
};
#endif
#ifndef KEYBITS
#define KEYBITS unsigned long
#endif
#ifndef ORD
#define ORD     unsigned long
#endif
#ifndef TAGFLAG
#define TAGFLAG unsigned char
#endif
#ifndef SUBNODE
#define SUBNODE struct subnode_struct
SUBNODE {
#ifdef TYPE
  TYPE     type;
#endif
#ifdef DATA
  DATA     d;
#endif
  KEY      *k;
  SUBNODE  *llink, *rlink, *parent;
  KEYBITS  skip;
  ORD      size;
  TAGFLAG  ltag, rtag;
};
#endif

/* Routines */
#ifndef COMPARE_KEYS
#define COMPARE_KEYS    compare_keys
#endif
#ifndef SEARCH_SUBNODE
#define SEARCH_SUBNODE  search_subnode
#endif
#ifndef LOOKUP_SUBNODE
#define LOOKUP_SUBNODE  lookup_subnode
#endif
#ifndef FIND_SUBNODE
#define FIND_SUBNODE    find_subnode
#endif
#ifndef INDEX_SUBNODE
#define INDEX_SUBNODE   index_subnode
#endif
#ifndef INSERT_SUBNODE
#define INSERT_SUBNODE  insert_subnode
#endif
#ifndef DELETE_SUBNODE
#define DELETE_SUBNODE  delete_subnode
#endif


KEYBITS COMPARE_KEYS (KEYSTR b1, KEYSTR b2, KEYBITS nb);
void SEARCH_SUBNODE (SUBNODE **p, SUBNODE **q, KEYSTR b, KEYBITS nb,
                 ORD *i, KEYBITS *j, TAGFLAG *tag);
void LOOKUP_SUBNODE (SUBNODE **p, SUBNODE **q, KEYSTR b, KEYBITS nb,
                         KEYBITS *j, TAGFLAG *tag);
void FIND_SUBNODE (SUBNODE **p, SUBNODE **q, KEYSTR b, KEYBITS nb,
                                     TAGFLAG *tag);
void INDEX_SUBNODE (SUBNODE **p, SUBNODE **q, ORD rank, TAGFLAG *tag);
void INSERT_SUBNODE (SUBNODE *p, SUBNODE *q, SUBNODE *r,
                 KEYBITS j, KEYBITS l);
void DELETE_SUBNODE (SUBNODE *p, SUBNODE *q);


#ifndef NULL
#define NULL 0
#endif
