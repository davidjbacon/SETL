/*  ===  Translator memory management  =============================  */

/*  $Id: mem.c,v 1.14 2022/12/10 23:35:26 setlorg Exp $  */

/*  Free software (c) dB - see file COPYING for license (GPL).  */

/*
 *  All the memory used by the translator after it has initialized is
 *  "ephemeral", meaning that all the memory after each translation
 *  cycle can be given back to the system.
 *
 *  To save the SETL maintainer the trouble of keeping track of all
 *  those blocks just to give them back (you'll always leak some if
 *  you try to work that way), all the blocks mem_alloc'd or
 *  mem_realloc'd during "ephemeral mode" are noted so that they can
 *  be given back in one fell swoop automatically upon 'mem_reset'.
 */

/* ------------------------------------------------------------------ */

#include "setltran.h"

/* How we keep track of ephemeral blocks */
typedef struct notes_struct  notes;
struct notes_struct {   /* chunk of notes (ptrs to ephemeral blocks) */
  notes *next;            /* next chunk of notes */
  long n_used;            /* how many blocks noted in this chunk */
  long n_blocks;          /* total #elements in the following array */
  void *blocks[FLEX];     /* array of block pointers */
};
#define notes_size(k)  (offsetof(notes, blocks) + \
                  (k) * elmtsize(notes, blocks))

/* Local routines */
static void mem_newchunk(void);
static void mem_note(void *p);
static void mem_remove(void *p);

/* Local data */
static bool ephemeral;          /* true means take notes */
static long ptrs_per_chunk;     /* current # of pointers per chunk */
static notes *notes_head;       /* noted blocks will be auto-cleared */

#define STARTING_PTRS_PER_CHUNK 1000


/* ------------------------------------------------------------------ */

void mem_init(void) {
  ephemeral = false;
  ptrs_per_chunk = STARTING_PTRS_PER_CHUNK;
  notes_head = NULL;
}

/* .................................................................. */

bool mem_ephemeral(bool flag) {
  bool r = ephemeral;
  ephemeral = flag;
  return r;
}

/* .................................................................. */

/*
 *  For extra efficiency (and as a guard against fragmentation-prone
 *  'malloc' implementations), we could allocate out of our own chain
 *  of "arenas", and then just give back all the arenas on a mem_reset.
 *
 *  Really we'd want two chains of arenas, one of "big" blocks and one
 *  of bunches of "little" blocks, so as not to waste gobs of space at
 *  the end of some arenas.  (Perhaps big >= 4K, arena size = 1Meg.)
 *  The chain would be doubly linked so that mem_realloc of a big block
 *  could properly retire the "old" pointer.
 */

void *mem_alloc(size_t nbytes) {
  void *r = malloc(nbytes);
  if (!r) {
    long nb = nbytes;  /* the size of size_t is system-dependent */
    fprintf(stderr,"%s:  out of memory on malloc request for %ld bytes!\n",
                    tranprog,                                nb);
    exit(2);
  }
  if (ephemeral) mem_note(r);
  return r;
}

/* .................................................................. */

void *mem_realloc(void *p, size_t nbytes) {
  void *r = realloc(p, nbytes);
  if (!r) {
    long nb = nbytes;  /* the size of size_t is system-dependent */
    fprintf(stderr,"%s:  out of memory on realloc request for %ld bytes!\n",
                    tranprog,                                 nb);
    exit(2);
  }
  if (ephemeral && (r != p)) {
    mem_remove(p);  /* remove the old pointer from our notes */
    mem_note(r);  /* and add the new one */
  }
  return r;
}

/* .................................................................. */

void mem_free(void *p) {
  if (!ephemeral) free(p);
  /* Ephemeral blocks will be cleared on mem_reset */
}

/* .................................................................. */

void mem_reset(void) {
  notes *n = notes_head;
  notes *m;
  while (n) {
    long k = n->n_used;
    while (k--) {
      void *p = n->blocks[k];
      if (p) free(p);
    }
    m = n->next;
    free(n);
    n = m;
  }
  ptrs_per_chunk = STARTING_PTRS_PER_CHUNK;
  notes_head = NULL;
}

/* .................................................................. */

static void mem_newchunk(void) {
  size_t nbytes = notes_size (ptrs_per_chunk);
  notes *n = (notes *) malloc(nbytes);
  if (!n) {
    long nb = nbytes;  /* the size of size_t is system-dependent */
    fprintf(stderr,"%s:  out of memory on malloc request for %ld bytes!\n",
                    tranprog,                                nb);
    exit(2);
  }
  n->next = notes_head;
  n->n_used = 0;
  n->n_blocks = ptrs_per_chunk;
  notes_head = n;
  ptrs_per_chunk *= 2;
}

/* .................................................................. */

static void mem_note(void *p) {
  notes *n = notes_head;
  if (!n || (n->n_used == n->n_blocks)) {
    mem_newchunk();
    n = notes_head;
  }
  n->blocks[n->n_used++] = p;
}

/* .................................................................. */

static void mem_remove(void *p) {
  /*
   *  If mem_realloc were to be used heavily, this could cause some
   *  noticeable inefficiency.  But it isn't (by me, anyway).  With
   *  arena management or any such large-shovel plan for client
   *  blocks, this routine would probably be a no-op.
   *
   *  Another solution would be to have mem_alloc and mem_realloc
   *  allocate an extra pair of list pointers in front of each block,
   *  for a doubly linked list that would allow the "old" block on a
   *  mem_realloc to be snipped out in a trice.  This is a degenerate
   *  form of the arena approach (we treat all blocks as "big").
   */
  notes *n = notes_head;
  while (n) {
    long k = n->n_used;
    while (k--) {
      void *q = n->blocks[k];
      if (q == p) {
        n->blocks[k] = NULL;
        return;
      }
    }
    n = n->next;
  }
}

#if 0
/* This can be used to help placate Autoconf-spewed configure scripts
 * when cross-compiling, if someone has misguidedly called
 * AC_FUNC_MALLOC and/or AC_FUNC_REALLOC in configure.ac (which would
 * be misguided because we avoid calling malloc() or realloc() with a
 * size arg of 0 in GNU SETL anyway):  */
#undef malloc
#undef realloc
void *rpl_malloc (size_t siz);  /* avoid warning of no prototype */
void *rpl_malloc (size_t siz) {
  extern void *malloc (size_t);
  return malloc (siz);
}
void *rpl_realloc (void *old, size_t siz);
void *rpl_realloc (void *old, size_t siz) {
  extern void *realloc (void *, size_t);
  return realloc (old, siz);
}
#endif
