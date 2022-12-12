/*  --  Convenient PATRICIA interface (SETL translator version)  --  */

/*  $Id: patrie.h,v 1.6 2020/12/18 23:53:42 setlorg Exp $  */

/*  Free software (c) dB - see file COPYING for license (GPL).  */

/*
 *  The routines in this set all refer to a PATRIE, which is defined
 *  as a "struct subnode *".  Patricia tries are both rooted at and
 *  composed of such records.
 *
 *  See also "patricia.h" (you will probably need to #define DATA etc.).
 */

#ifndef PATRIE_H
#define PATRIE_H

#include "patricia.h"

typedef SUBNODE *PATRIE;

/* Default names for the functions (silly feature) */
#ifndef CREATE
#define CREATE patcre
#endif
#ifndef DELETE
#define DELETE patdel
#endif
#ifndef DELKEY
#define DELKEY delkey
#endif
#ifndef DELORD
#define DELORD delord
#endif
#ifndef LOOKUP
#define LOOKUP lookup
#endif
#ifndef KEYSUB
#define KEYSUB keysub
#endif
#ifndef ORDSUB
#define ORDSUB ordsub
#endif
#ifndef KEYORD
#define KEYORD keyord
#endif
#ifndef ORDKEY
#define ORDKEY ordkey
#endif
#ifndef SIZE
#define SIZE size
#endif

/* Default names for the memory allocator routines */
#ifndef MALLOC
#define MALLOC malloc
#endif
#ifndef FREE
#define FREE free
#endif

/* Function prototypes */
extern bool CREATE (PATRIE *t);
extern bool DELETE (PATRIE *t);
extern bool DELKEY (PATRIE t, const char *key);
extern bool DELORD (PATRIE t, ORD ord);
extern bool LOOKUP (PATRIE t, const char *key, DATA **data);
extern bool KEYSUB (PATRIE t, const char *key, DATA **data);
extern bool ORDSUB (PATRIE t, ORD ord, DATA **data);
extern bool KEYORD (PATRIE t, const char *key, ORD *ord);
extern bool ORDKEY (PATRIE t, ORD ord, const char **key);
extern ORD SIZE (PATRIE t);

/*
 *  Return codes for the boolean functions above:
 *   true  -  successful creation/deletion, key found, ord in range
 *   false  -  key did not exist, ord out of range
 */

#endif /* PATRIE_H */
