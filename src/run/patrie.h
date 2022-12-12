/*  --  Convenient PATRICIA interface (SETL run-time version)  --  */

/*  $Id: patrie.h,v 1.12 2010/10/14 18:34:43 bacon Exp $  */

/*  Free software (c) dB - see file COPYING for license (GPL).  */

/*
 *  The routines in this set all manipulate a PATRIE, which stands for
 *  "Patricia trie".  This type has a default which you can override.
 * 
 *  If you want to change the definition of TYPE or DATA, you have to
 *  do what it says in "patricia.h" plus re-#define all the routine
 *  names that are here.
 * 
 *  It is highly recommended that you have:
 * 
 *    typedef PATRIE yourtype;
 * 
 *  (e.g., typedef PATRIE table;).
 * 
 *  If you want your DATA to be able to be, say, a union that
 *  includes PATRIE, you will probably do something like this
 *  instead of the above, and put it before the #include:
 * 
 *    typedef struct subnode_struct *yourtype;
 * 
 *  Another alternative is to use
 * 
 *    typedef struct subnode_struct subnode;
 * 
 *  and then always refer to subnodes ("patries") as 'subnode *'.
 * 
 * 
 *  The routines with KEY in their name use full-fledged keys;
 *  those with KEYSTR use null-terminated strings (KEYSTRs for now).
 * 
 *  I allocate memory and copy your KEYSTR arguments, but for KEY
 *  things, you're supposed to be more responsible:  hand me a
 *  pointer to your key that I can stuff directly into the subnode
 *  block.
 * 
 * 
 *  You can #define KEY_TYPE, SUBNODE_TYPE, and PATRIE_TYPE as
 *  "type codes" that get inserted into the appropriate records
 *  and later checked.
 * 
 * 
 *  In general, this interface is not quite as opaque as one might
 *  hope.  You may need to know the names of the appropriate fields in
 *  the struct that a SUBNODE is, those being d, of type DATA, and k,
 *  of type "KEY *".  Some clients, such as garbage collectors, need
 *  to know things like the names of the link fields too.  For a KEY,
 *  you need to know about the 'length' and 'bstring' fields, and
 *  of course the 'type' field in all records if TYPE is #defined.
 */

#ifndef DATA
#define DATA int
#endif
#ifndef KEYBITS
#define KEYBITS unsigned long
#endif
#ifndef KEY
#define KEY struct key_struct
KEY {
#ifdef TYPE
  TYPE type;  /* set to KEY_TYPE */
#endif
  KEYBITS length;
  char bstring[1];  /* this is actually "flexible" */
};
#define KEY_STUBSIZE offsetof(KEY, bstring)
#endif

#include "patricia.h"

#ifndef PATRIE
#define PATRIE SUBNODE *
#endif

#ifndef PATCRE
#define PATCRE patcre
#endif
#ifndef PATCPY
#define PATCPY patcpy
#endif
#ifndef PATCMP
#define PATCMP patcmp
#endif
#ifndef STRSEE
#define STRSEE strsee
#endif
#ifndef KEYSEE
#define KEYSEE keysee
#endif
#ifndef ORDSEE
#define ORDSEE ordsee
#endif
#ifndef STRSUB
#define STRSUB strsub
#endif
#ifndef KEYSUB
#define KEYSUB keysub
#endif
#ifndef STRDEL
#define STRDEL strdel
#endif
#ifndef KEYDEL
#define KEYDEL keydel
#endif
#ifndef ORDDEL
#define ORDDEL orddel
#endif
#ifndef STRORD
#define STRORD strord
#endif
#ifndef KEYORD
#define KEYORD keyord
#endif
#ifndef PATSIZ
#define PATSIZ patsiz
#endif

#ifndef MALLOC
#define MALLOC(n) malloc(n)
#endif
#ifndef FREE
#define FREE(p) free(p)
#endif
#ifndef ALLOC_SUBNODE
#define ALLOC_SUBNODE() \
        ((SUBNODE *) MALLOC(sizeof(SUBNODE)))
#endif
#ifndef FREE_SUBNODE
#define FREE_SUBNODE(p) (FREE(p))
#endif
#ifndef ALLOC_KEY
#define ALLOC_KEY(nbytes) \
        ((KEY *) MALLOC(KEY_STUBSIZE + nbytes))
#endif
#ifndef FREE_KEY
#define FREE_KEY(p) (FREE(p))
#endif
#ifndef COPY_KEY
#define COPY_KEY(k) (copy_key(k))  /* client defines 'copy_key' */
#endif
#ifndef COPY_DATA
#define COPY_DATA(d) (copy_data(d))  /* client defines 'copy_data' */
#endif
#ifndef EQUAL_KEY
#define EQUAL_KEY(k1,k2) (equal_key(k1,k2))  /*  "  "  'equal_key' */
#endif
#ifndef EQUAL_DATA
#define EQUAL_DATA(d1,d2) (equal_data(d1,d2))  /*  "  "  'equal_data' */
#endif

/* Function prototypes */
bool PATCRE (PATRIE *tp);                       /* create a patrie */
bool PATCPY (PATRIE *tp, PATRIE t);             /* copy a patrie */
int  PATCMP (PATRIE t, PATRIE u);               /* compare patries */
bool STRSEE (PATRIE t, KEYSTR s, SUBNODE **d);  /* lookup by str */
bool KEYSEE (PATRIE t, KEY *k, SUBNODE **d);    /* lookup by key */
bool ORDSEE (PATRIE t, ORD o, SUBNODE **d);     /* lookup by position */
bool STRSUB (PATRIE t, KEYSTR s, SUBNODE **d);  /* lookup/insert by str */
bool KEYSUB (PATRIE t, KEY *k, SUBNODE **d);    /* lookup/insert by key */
bool STRDEL (PATRIE t, KEYSTR s);               /* delete by str */
bool KEYDEL (PATRIE t, KEY *k);                 /* delete by key */
bool ORDDEL (PATRIE t, ORD o);                  /* delete by position */
bool STRORD (PATRIE t, KEYSTR s, ORD *o);       /* position for str */
bool KEYORD (PATRIE t, KEY *k, ORD *o);         /* position for key */
ORD  PATSIZ (const PATRIE t);                   /* size of patrie */

/*
 *  Boolean return codes for the above:
 *   true  -  successful creation/deletion, key/str found, ord in range
 *   false  -  key/str did not exist, ord out of range
 */
