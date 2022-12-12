/*  ===  SETL run-time memory allocation  ==========================  */

/*  $Id: heap.c,v 1.45 2022/11/17 14:16:07 setlorg Exp $  */

/*  Free software (c) dB - see file COPYING for license (GPL).  */

/*
 *  All memory blocks associated with SETL virtual machine instances
 *  are kept in the "heap" that is managed by the routines here.
 *
 *  The heap memory is a single contiguous arena, 'mem', that can grow
 *  and/or move as necessary, and is itself obtained using 'os_malloc'
 *  or 'os_realloc'.  (This arena should not be confused with the
 *  "arena allocator" implemented in arena.c, which is for managing
 *  short-term C objects that might not fit on the C stack.)
 *
 *  Blocks are allocated from the heap using 'alloc_mem', usually called
 *  indirectly via 'alloc_block', which itself is always called by the
 *  'fblock' or 'vblock' macro (see "setlrun.h").  Blocks are never
 *  explicitly released, but when an 'alloc_mem' request cannot be
 *  satisfied from the current arena, a compacting mark-and-sweep
 *  garbage collector is invoked.
 *
 *  The garbage collector decides, based on how much memory would be
 *  freed up by compaction alone, whether or not to increase the
 *  arena size before clumping all the active memory blocks together.
 *  See 'gc2' for details.
 *
 *  The fact that any or all heap memory blocks can be relocated on
 *  any 'alloc_mem' call has important implications for block pointer
 *  management.  This is loudly documented at the top of "setlrun.h",
 *  because it is crucial that you understand these implications
 *  thoroughly before attempting any nontrivial modifications to the
 *  SETL interpreter.
 *
 *  Because a single failure to follow any of the rules stated there
 *  can result in a bug that may go undetected indefinitely and then
 *  be extremely hard to track down once a garbage collection is
 *  provoked at just the wrong time, there are preprocessor flags
 *  DEBUG_HEAP and DEBUG_HANDLES which you can turn on when compiling
 *  this module.
 *
 *  DEBUG_HEAP forces a garbage collection on every 'alloc_mem' call,
 *  and furthermore makes sure that the arena moves on every garbage
 *  collection.  Needless to say, it makes execution very slow, but
 *  it can be very useful.
 *
 *  DEBUG_HANDLES just scans all existing handles every time you try
 *  to create a new handle, to make sure you haven't already put a
 *  handle on the block pointer whose address you are passing to
 *  'grab_handle'.  It is useful on its own, but you should always
 *  turn it on if you turn DEBUG_HEAP on, because it can provide a
 *  very quick and precise diagnosis of this kind of error.
 *
 *  The 'print_heap' routine may be called from anywhere at any time
 *  to give a full dump of the current heap contents (to stdout).
 */

#include "setlrun.h"


/*
 *  Macros
 */

/*
 *  If the amount of memory still in use in the arena after compaction
 *  would be more than this fraction of the total arena size, the arena
 *  itself will be made larger if possible:
 */
#ifndef MEMTHRESH
#define MEMTHRESH 0.5
#endif

/*
 *  If the amount of memory still in use in the arena after compaction
 *  would be more than this fraction of the total arena size, memory
 *  exhaustion will be reported (note that if this fraction were made
 *  too close to 1, excessive garbage collection would occur just
 *  before absolute memory exhaustion):
 */
#ifndef MEMLIMIT
#define MEMLIMIT 0.75
#endif

/*
 *  If the decision is made to make the arena larger, its new size
 *  will be this factor times the amount of memory that is still in
 *  use after compaction, or 'maxmem', whichever is less:
 */
#ifndef MEMFACTOR
#define MEMFACTOR 4.0
#endif

/*
 *  Round x up to the nearest multiple of MALIGN (usually 8) bytes:
 */
#define align_up(x) round_up(x,MALIGN)

/*
 *  Size of the prefix on each block that 'alloc_mem' returns:
 */
#define HEADER_SIZE  align_up(sizeof (header))

/*
 *  Number of bytes in block after rounding up for alignment:
 */
#define fphysbytes(type)    align_up(fbytes(type))
#define vphysbytes(type,n)  align_up(vbytes(type,n))

/*
 *  Number of "extra" bytes available in block:
 */
#define fsparebytes(type)    (fphysbytes(type)   - fbytes(type))
#define vsparebytes(type,n)  (vphysbytes(type,n) - vbytes(type,n))

/*
 *  Checks against corruption of the 'length' field in the header:
 */
#define fcheck(a,type)\
  assert ((a)->length == HEADER_SIZE + fphysbytes(type))
#define vcheck(a,type,n)\
  assert ((a)->length == HEADER_SIZE + vphysbytes(type,n))

/*
 *  Handles (see below) get their own memory, allocated this many
 *  at a time:
 */
#ifndef HANDLE_CHUNK
#define HANDLE_CHUNK 1000  /* about 1000 is good */
#endif


/*
 *  Typedefs
 */

typedef struct header_struct  header;
typedef struct handful_struct  handful;

#if SIZEOF_LONG < SIZEOF_VOID_P
#error C type 'long' is not wide enough to hold a 'void *' value.
#endif
#if SIZEOF_SIZE_T < SIZEOF_VOID_P
#error C type 'size_t' is not wide enough to hold a 'void *' value.
#endif
typedef unsigned long  address;
typedef long           address_diff;
typedef size_t         address_offset;  /* non-negative address offset */


/*
 *  Structs
 */

/*
 *  Every block managed by the heap allocator is prefixed by this header:
 */
struct header_struct {
  size_t          length;  /* bytes in block incl. header & alignment */
  address_offset  mark;    /* for use by the garbage collector */
# define unmarked  ((address_offset)-1)  /* default state of mark */
# define marked    ((address_offset)0)   /* mark, mark... */
};

/*
 *  The "handles" that must be attached and removed by users of heap
 *  block pointers are kept in the following so-called "handfuls".
 *
 *  Each handful can hold HANDLE_CHUNK pointers to block pointers, and
 *  (to accommodate the rare case of deep recursion in the SETL
 *  interpreter) if one handful is not enough, more handfuls will be
 *  allocated (by the system-level allocator) as necessary.  Handfuls,
 *  once allocated, are linked into a chain starting at 'master' and
 *  kept around for future use:
 */
struct handful_struct {
  long n;        /* current number of handles in this handful */
  handful *next,*prev;  /* list links */
  handle handles[HANDLE_CHUNK]; /* the baggage */
};


/*
 *  Local routine declarations
 */

static void          gbcol (size_t n);
static void          gc0 (void);
static void          gc1 (void);
static void          gcmark (block *start);
static address_diff  gc2 (size_t n);
static void          gc3 (address_diff warp);
static void          gc4 (void);
static header       *get_header (block *b);
static block        *get_block (header *a);
static void          print_blockptr (block *a);


/*
 *  Local data
 */

static handful         *master;   /* head of chain of handfuls */
static void            *mem;      /* heap contents */
static address_offset   front;    /* allocation high water mark */


/* ------------------------------------------------------------------ */

/*
 *  Allocate and initialize a handful of handles, and the arena itself
 */
void heap_init(void) {
  /*
   *  If any of these assertions fail, find wider types to represent
   *  'address' etc., and update also the static ("#if SIZEOF_...")
   *  checks on their underlying types:
   */
  assert (sizeof (address) >= sizeof (void *));
  assert (sizeof (address_diff) >= sizeof (void *));
  assert (sizeof (address_offset) >= sizeof (void *));
  assert (sizeof (size_t) >= sizeof (void *));
  master = (handful *) os_malloc(sizeof *master);
  master->n = 0;
  master->next = NULL;
  master->prev = NULL;
#ifdef DEBUG_HEAP
  memsize = HEADER_SIZE;  /* garbage collect on the first 'alloc_mem' */
#endif
  mem = os_malloc(memsize);
  front = 0;
} /* end heap_init */


/*
 *  Give back the arena and all allocated handfuls of handles
 */
void heap_fini(void) {
  os_free(mem);
  while (master) {
    handful *f = master->prev;
    os_free(master);
    master = f;
  }
} /* end heap_fini */


/* .................................................................. */

/*
 *  This routine is conventionally called via the 'ref' macro
 *  (see "setlrun.h"); it sequesters a reference to a block pointer.
 */
handle *grab_handle(block **reference) {

  handle *r;

  /*
   *  Make sure the block pointer we're supposed to keep updated does
   *  not lie in the heap (those pointers are handled separately):
   */
  assert ((address)reference <  (address)mem ||
          (address)reference >= (address)mem + front);
  /*
   *  Make sure the block it refers to does so lie, or that the
   *  pointer is null:
   */
  assert (*reference == NULL || (
          (address)*reference >= (address)mem &&
          (address)*reference <  (address)mem + front));

#ifdef DEBUG_HANDLES
  {
    handful *f;
    for (f = master; f; f = f->prev) {
      long i = f->n;
      while (i > 0) {
        handle *h = &f->handles[--i];
        assert (reference != h->reference);
      }
    }
  }
#endif

  if (master->n == HANDLE_CHUNK) {
    /* full handful */
    if (master->next) {
      /* pick next handful off list */
      master = master->next;
      assert (master->n == 0);
    } else {
      /* what, no more handfuls? so make one */
      handful *new_master = (handful *) os_malloc (sizeof *new_master);
      new_master->n = 0;
      new_master->next = NULL;
      new_master->prev = master;
      master->next = new_master;
      master = new_master;
    }
  }

  /*
   *  Here is the usual-case work of this routine:
   */
  r = &master->handles[master->n++];
  r->reference = reference;
  return r;

} /* end grab_handle */


/*
 *  This routine is conventionally called via the 'retire' macro
 *  (see "setlrun.h"); it releases the most recently grabbed handle
 */
void release_handle(handle *h) {

  if (master->n == 0) {
    master = master->prev;
    assert (master->n == HANDLE_CHUNK);
  }
  --master->n;

  assert (h == &master->handles[master->n]);

} /* end release_handle */


/* .................................................................. */

/*
 *  Allocators
 */

integer *alloc_integer(size_t nlimb) {
  integer *r = vblock(integer,nlimb);
  nlimb += vsparebytes(integer,nlimb) / integer_elmtsize;
  r->room = nlimb;
  r->size = 0;
  return r;
} /* end alloc_integer */

string *alloc_string(long nchar, long nbefore, long nafter) {
  size_t ntotal;
  string *r;
  assert (nchar >= 0);
  assert (nbefore >= 0);
  assert (nafter >= 1);  /* the minimum you must ask for */
  ntotal = nchar + nbefore + nafter;
  r = vblock(string,ntotal);
  r->nchar = nchar;
  r->nbefore = nbefore;
  /* this is the 'nafter' arg rounded up per the end of the block: */
  r->nafter = (vphysbytes(string,ntotal) -
                   vbytes(string,nbefore+nchar)) / string_elmtsize;
  return r;
} /* end alloc_string */

tuple *alloc_tuple(long nelt, long npre, long nsuf) {
  size_t ntotal;
  tuple *r;
  assert (nelt >= 0);
  assert (npre >= 0);
  assert (nsuf >= 0);
  ntotal = nelt + npre + nsuf;
  r = vblock(tuple,ntotal);
  nsuf += vsparebytes(tuple,ntotal) / tuple_elmtsize;
  r->nelt = nelt;
  r->npre = npre;
  r->nsuf = nsuf;  /* including free "spares" */
  if (nelt > 0) memset (&tupelt(r,1), 0, nelt*tuple_elmtsize);
  return r;
} /* end alloc_tuple */

/*
 *  Allocate a memory block and label it with a 'type'
 */
block *alloc_block(blocktype type, size_t size) {
  block *r;
  r = alloc_mem(size);
  assert (r);
  r->type = type;
  return r;
} /* end alloc_block */

/*
 *  Allocate a memory block of the requested size
 */
block *alloc_mem(size_t size) {
  size_t rs, n;
  header *a;
  block *b;
  if (size > (size_t) (LONG_MAX - HEADER_SIZE - MALIGN)) {
    runerr("Cannot allocate %zu bytes", size);
  }
  rs = align_up(size);
  n = HEADER_SIZE + rs;
#ifndef DEBUG_HEAP
  if (n > memsize-front) gbcol(n);
#else
  /*
   *  In DEBUG_HEAP mode, not only is a garbage collect forced on
   *  every 'alloc_mem' call, but the garbage collect itself insists on
   *  moving the arena in the hope that unhandled block pointers will
   *  be invalidated and caught as soon as possible (i.e., on the next
   *  'alloc_mem', by the checking the garbage collector does normally).
   */
  gbcol(n);
#endif
  a = (header *) ((address)mem + front);
  b = get_block(a);
  a->length = n;
  a->mark = unmarked;
  front += n;
  return b;
} /* end alloc_mem */

/* .................................................................. */

/*
 *  Passes of the garbage collector:
 *
 *   0.  Scan thru arena to check that all blocks are unmarked, i.e.,
 *       have 'unmarked' in the 'mark' field of the header preceding
 *       the block (see header_struct).
 *
 *   1.  Do a depth-first search starting from each block pointer
 *       referenced in a handle, and mark each visited block.  These
 *       reachable blocks are to be kept.  The rest are the garbage.
 *
 *   2.  Scan the arena, and for each marked (kept) block, change the
 *       'mark' field to the block's arena offset as it will be after
 *       compaction.  If the kept blocks and the block being allocated
 *       (the one that triggered the garbage collection) will occupy
 *       more than MEMTHRESH times the current arena size, reallocate
 *       the arena to be of size MEMFACTOR times the new occupancy.
 *
 *   3.  Update all block pointers within kept blocks with the addresses
 *       the pointed-to blocks will have after compaction.  Do the same
 *       to all the block pointers referenced by handles.
 *
 *   4.  Compact by moving all kept blocks to their new locations.
 *       Compress tuples and strings to their minimum size in doing so.
 *       Unmark all blocks.
 */


/*
 *  The garbage collector
 */
static void gbcol(size_t n) {
  address_diff warp;
  gc0();                /* check that blocks are still unmarked */
  gc1();                /* marking pass */
  warp = gc2(n);        /* determine new block locations */
  gc3(warp);            /* update block pointers */
  gc4();                /* move and unmark blocks */
} /* end gbcol */


/*
 *  Pass 0 (check that blocks are still unmarked)
 */
static void gc0(void) {
  address_offset offset = 0;
  while (offset < front) {
    header *a = (header *) ((address)mem + offset);
    assert (a->length != 0);
    assert (a->mark == unmarked);
    offset += a->length;
  }
} /* end gc0 */


/*
 *  Pass 1 (depth-first tree walk, mark what is reachable via handles)
 */
static void gc1(void) {
  handful *f;
  for (f = master; f; f = f->prev) {
    long i = f->n;
    while (i > 0) {
      handle *h = &f->handles[--i];
      gcmark(*h->reference);
    }
  }
} /* end gc1 */

#define gcmark_block(b) (gcmark((block *)(b)))

static void gcmark(block *b) {
  if (b) {
    header *a = get_header(b);  /* descriptor just before block */
    if (a->mark == unmarked) {
      a->mark = marked;
      switch (b->type) {
      case atom_type:
        fcheck(a,atom);
        break;
      case boolean_type:
        fcheck(a,boolean);
        break;
      case small_type:
        fcheck(a,small);
        break;
      case integer_type:
        {
          integer *t = (integer *)b;
          vcheck(a,integer,t->room);
        }
        break;
      case real_type:
        fcheck(a,real);
        break;
      case set_type:
        fcheck(a,set);
        gcmark_block(((set *)b)->tblval);
        break;
      case string_type:
        {
          string *t = (string *)b;
          vcheck(a,string,t->nchar+t->nbefore+t->nafter);
        }
        break;
      case tuple_type:
        {
          tuple *t = (tuple *)b;
          long i;
          vcheck(a,tuple,t->nelt+t->npre+t->nsuf);
          for (i=1; i<=t->nelt; i++) {
            gcmark_block(tupelt(t,i));
          }
        }
        break;
      case routine_type:
        fcheck(a,routine);
        break;
      case iterator_type:
        fcheck(a,iterator);
        gcmark_block(((iterator *)b)->constant);
        break;
      case table_type:
      case subnode_type:
        {
          subnode *t = (subnode *)b;
          fcheck(a,subnode);
          gcmark_block(t->d);
          gcmark_block(t->k);
          gcmark_block(t->llink);
          gcmark_block(t->rlink);
          /* Should not be necessary, but what the heck:  */
          gcmark_block(t->parent);
        }
        break;
      case key_type:
        {
          key *t = (key *)b;
          size_t nbytes = nbits_to_nbytes(t->length);
          vcheck(a,key,nbytes);
        }
        break;
      case source_type:
        {
          source *t = (source *)b;
          fcheck(a,source);
          gcmark_block(t->filename);
          gcmark_block(t->srctext);
        }
        break;
      case codeline_type:
        fcheck(a,codeline);
        gcmark_block(((codeline *)b)->text);
        break;
      case instr_type:
        {
          instr *t = (instr *)b;
          vcheck(a,instr,t->nopnd);
        }
        break;
      case parm_type:
        fcheck(a,parm);
        break;
      case frame_type:
        {
          frame *t = (frame *)b;
          /*
           *  We iterate instead of recursing, so that runaway
           *  recursion in the SETL program won't lead to C stack
           *  overflow in this garbage collector.  Otherwise, we
           *  could have this instead of the while loop below:
           */
#if 0 /* recursive version */
          long i;
          vcheck(a,frame,t->nloc);
          gcmark_block(t->link);
          gcmark_block(t->combiter);
          for (i=1; i<=t->nloc; i++) {
            gcmark_block(framelt(t,i));
          }
#endif
          bool looping = true;
          while (looping) {
            long i;
            vcheck(a,frame,t->nloc);
            gcmark_block(t->combiter);
            for (i=1; i<=t->nloc; i++) {
              gcmark_block(framelt(t,i));
            }
            t = t->link;
            if (t) {
              assert (is_frame(t));
              a = get_header((block *)t);
              if (a->mark == unmarked) {
                a->mark = marked;
              } else {
                looping = false;  /* frame already marked */
              }
            } else {
              looping = false;  /* end of frame list */
            }
          }
        }
        break;
      case symtab_type:
        {
          symtab *t = (symtab *)b;
          fcheck(a,symtab);
          gcmark_block(t->procnames);
          gcmark_block(t->procnums);
          gcmark_block(t->procaddrs);
          gcmark_block(t->globals);
          gcmark_block(t->locals);
          gcmark_block(t->labels);
          gcmark_block(t->gbl_backtrax);
          gcmark_block(t->lcl_backtrax);
          gcmark_block(t->vcode);
          gcmark_block(t->sources);
        }
        break;
      case machine_type:
        {
          machine *t = (machine *)b;
          long level;
          fcheck(a,machine);
          gcmark_block(t->code);
          gcmark_block(t->backs);
          gcmark_block(t->sym);
          gcmark_block(t->ready_maps);
          for (level=0; level<DISPLAY_LEVELS; level++) {
            gcmark_block(t->display[level]);
          }
        }
        break;
      default:
        unexpected (b->type);  /* no such blocktype */
      } /* end switch (b->type) */
    }
  }
} /* end gcmark */


/*
 *  Pass 2 (change all marks to new arena offsets)
 */
static address_diff gc2(size_t n) {

  double needed;
  address_offset old_offset, new_offset;
  size_t new_memsize;
  address_diff warp;
  void *new_mem;
  block *b;
  string *s;
  tuple *t;

  old_offset = 0;
  new_offset = 0;
  while (old_offset < front) {
    header *a = (header *) ((address)mem + old_offset);
    size_t length = a->length;
    if (a->mark == marked) {
      a->mark = new_offset;
      b = get_block(a);
      switch (b->type) {
      case string_type:
        s = (string *)b;
        new_offset += HEADER_SIZE + vphysbytes(string,s->nchar+1);
        break;
      case tuple_type:
        t = (tuple *)b;
        new_offset += HEADER_SIZE + vphysbytes(tuple,t->nelt);
        break;
      default:
        new_offset += length;
      }
    } else {
      assert (a->mark == unmarked);
    }
    old_offset += length;
  }

  assert (old_offset == front);

  needed = new_offset + (double)n;
  if (needed > maxmem*MEMLIMIT) {
    runerr("Memory exhausted");
  }

#ifndef DEBUG_HEAP

  if (needed > memsize*MEMTHRESH) {
    /*
     *  Grow the arena
     */
    double desired_memsize = needed*MEMFACTOR;
    new_memsize = (size_t) MIN (desired_memsize, maxmem);
    new_mem = os_realloc(mem, new_memsize);
    warp = (address)new_mem - (address)mem;
    mem = new_mem;
    memsize = new_memsize;
    return warp;
  } else {
    /*
     *  The arena is big enough (compaction alone will suffice)
     */
    return 0L;
  }

#else
  /*
   *  For DEBUG_HEAP debugging, grow the arena AND make sure it moves
   */
  new_memsize = needed;
  if (new_memsize < front) new_memsize = front;  /* else we overcopy */
  new_mem = os_malloc(new_memsize);
  warp = (address)new_mem - (address)mem;
  memcpy (new_mem, mem, front);
  memset (mem, 0xdB, memsize);  /* zap the old arena for good measure */
  os_free (mem);
  mem = new_mem;
  memsize = new_memsize;
  return warp;

#endif

} /* end gc2 */


/*
 *  Pass 3 (update block pointers)
 */
static void gc3(address_diff warp) {

  /*
   *  Macro to update a block pointer to its new value as given by
   *  the arena "base address" plus the offset that was stashed in
   *  the 'mark' field of the header prefixing the block.  Note that
   *  if the arena was reallocated, the block will have already moved
   *  by a distance of 'warp' from its old location by now:
   */
#if HAVE_TYPEOF
# define update(x) \
  if (x) {\
    header *w = (header *)((address)(x) + warp - HEADER_SIZE);\
    x = (typeof(x))((address)mem + w->mark + HEADER_SIZE);\
  }
#else
# define update(x) \
  if (x) {\
    header *w = (header *)((address)(x) + warp - HEADER_SIZE);\
    *(block **)&(x) = (block *)((address)mem + w->mark + HEADER_SIZE);\
  }
#endif

  { /* Update the block pointers referenced by the handles */
    handful *f;
    for (f=master; f; f=f->prev) {
      long i = f->n;
      while (i > 0) {
        handle *h = &f->handles[--i];
        update(*h->reference);
      }
    }
  }

  { /* Update the block pointers in the heap itself */
    address_offset offset = 0;
    while (offset < front) {
      header *a = (header *) ((address)mem + offset);
      size_t length = a->length;
      if (a->mark != unmarked) {
        block *b = get_block(a);
        switch (b->type) {
        case atom_type:
          break;
        case boolean_type:
          break;
        case small_type:
          break;
        case integer_type:
          break;
        case real_type:
          break;
        case set_type:
          update(((set *)b)->tblval);
          break;
        case string_type:
          break;
        case tuple_type:
          {
            tuple *t = (tuple *)b;
            long i;
            for (i=1; i<=t->nelt; i++) {
              update(tupelt(t,i));
            }
          }
          break;
        case routine_type:
          break;
        case iterator_type:
          update(((iterator *)b)->constant);
          break;
        case table_type:
        case subnode_type:
          {
            subnode *t = (subnode *)b;
            update(t->d);
            update(t->k);
            update(t->llink);
            update(t->rlink);
            update(t->parent);
          }
          break;
        case key_type:
          break;
        case source_type:
          {
            source *t = (source *)b;
            update(t->filename);
            update(t->srctext);
          }
          break;
        case codeline_type:
          update(((codeline *)b)->text);
          break;
        case instr_type:
          break;
        case parm_type:
          break;
        case frame_type:
          {
            frame *t = (frame *)b;
            long i;
            update(t->link);
            update(t->combiter);
            for (i=1; i<=t->nloc; i++) {
              update(framelt(t,i));
            }
          }
          break;
        case symtab_type:
          {
            symtab *t = (symtab *)b;
            update(t->procnames);
            update(t->procnums);
            update(t->procaddrs);
            update(t->globals);
            update(t->locals);
            update(t->labels);
            update(t->gbl_backtrax);
            update(t->lcl_backtrax);
            update(t->vcode);
            update(t->sources);
          }
          break;
        case machine_type:
          {
            machine *t = (machine *)b;
            long level;
            update(t->code);
            update(t->backs);
            update(t->sym);
            update(t->ready_maps);
            for (level=0; level<DISPLAY_LEVELS; level++) {
              update(t->display[level]);
            }
          }
          break;
        default:
          unexpected (b->type);  /* no such blocktype */
        } /* end switch (b->type) */
      }
      offset += length;
    }
  }

} /* end gc3 */


/*
 *  Pass 4 (move blocks for compaction, reset marks)
 */
static void gc4(void) {

  address_offset old_offset, new_offset;

  old_offset = 0;
  new_offset = 0;
  while (old_offset < front) {
    header *a = (header *) ((address)mem + old_offset);
    size_t length = a->length;
    if (a->mark != unmarked) {
      block *b = get_block(a);
      header *anew = (header *) ((address)mem + new_offset);
      switch (b->type) {
      case string_type:
        {
          string *s = (string *)b;
          long nchar = s->nchar;
          long nbefore = s->nbefore;
          string *snew = (string *)get_block(anew);
          size_t new_length = HEADER_SIZE + vphysbytes(string,nchar+1);
          new_offset += new_length;
          /*
           *  N.B.  s and snew could be aliases or nearly so, so we use
           *  the strval field directly here and delay filling in the
           *  other fields until after the payload has been copied:
           */
          if (snew != s || nbefore != 0) {
            if (nchar > 0) {
              memmove(&snew->strval[0],&strelt(s,1),nchar);
            }
          }
          anew->length = new_length;
          snew->type = string_type;
          snew->nchar = nchar;
          snew->nbefore = 0;
          /* Note that vsparebytes() does not quite apply here, because of
           * the internal trailing '\0'; cf. alloc_string(): */
          snew->nafter = (vphysbytes(string,nchar+1) -
                              vbytes(string,nchar)) / string_elmtsize;
          strelt(snew,nchar+1) = '\0';
        }
        break;
      case tuple_type:
        {
          tuple *t = (tuple *)b;
          long nelt = t->nelt;
          long npre = t->npre;
          tuple *tnew = (tuple *)get_block(anew);
          size_t new_length = HEADER_SIZE + vphysbytes(tuple,nelt);
          new_offset += new_length;
          /* Comments as for strings apply here too. */
          if (tnew != t || npre != 0) {
            if (nelt > 0) {
              memmove(&tnew->elts[0],&tupelt(t,1),nelt*sizeof(block *));
            }
          }
          anew->length = new_length;
          tnew->type = tuple_type;
          tnew->nelt = nelt;
          tnew->npre = 0;
          tnew->nsuf = vsparebytes(tuple,nelt) / tuple_elmtsize;
        }
        break;
      default:
        {
          if (new_offset != old_offset) memmove(anew,a,length);
          new_offset += length;
        }
      }
      anew->mark = unmarked;
    }
    old_offset += length;
  }
  assert (new_offset < memsize);
  front = new_offset;

} /* end gc4 */


static header *get_header(block *b) {
  assert ((address)b >= (address)mem);
  assert ((address)b <  (address)mem + front);
  return (header *)((address)b - HEADER_SIZE);
} /* end get_header */


static block *get_block(header *a) {
  return (block *)((address)a + HEADER_SIZE);
} /* end get_block */


/* .................................................................. */

/*
 *  Minor sleazery to direct print_heap() and print_blockptr()
 *  output to stderr
 */
#undef printf
#define printf  print_stderr
#undef putchar
#define putchar  putc_stderr

void print_heap(void) {

  address_offset offset;

  /*
   *  Run the first 2 passes of the gc so that the marks will
   *  reflect reachability
   */
  gc0();
  gc1();

  printf("mem at %p, size=%zu, front=%zu\n",
                mem,  memsize,     front);
  printf("offset length mark type details...\n");

  /*
   *  The main print loop, one line output per block
   */
  offset = 0;
  while (offset < front) {
    header *a = (header *) ((address)mem + offset);
    block *b = get_block(a);
    printf("%zu %zu %zu ",offset,a->length,a->mark);
    printf("%s ",TYPENAME(b));
    switch (b->type) {
    case atom_type:
      {
        atom *x = (atom *)b;
        printf("atmval=%ld",
             x->atmval);
      }
      break;
    case boolean_type:
      {
        boolean *x = (boolean *)b;
        printf("booval=%d",
             x->booval);
      }
      break;
    case small_type:
      {
        small *x = (small *)b;
        printf("weeval=%ld",
             x->weeval);
      }
      break;
    case integer_type:
      {
        integer *x = (integer *)b;
        ro_mpz(z, x)  /* const mpz_t z = alias of INTEGER x */
        printf("room=%zu size=%zd limbs=",
             x->room, x->size);
        mpz_out_str (stdout, 10, z);
      }
      break;
    case real_type:
      {
        real *x = (real *)b;
        printf("realval=%.*g", DOUBLE_REP_DIGS,
             x->realval);
      }
      break;
    case set_type:
      {
        set *x = (set *)b;
        printf("stype=%d card=%ld tblval=",
             x->stype, x->card);
        print_blockptr((block *)x->tblval);
      }
      break;
    case string_type:
      {
        string *x = (string *)b;
        long i;
        printf("nchar=%ld nbefore=%ld nafter=%ld '",
             x->nchar, x->nbefore, x->nafter);
        for (i=1; i<=x->nchar; i++) {
          char c = strelt(x,i);
          if (musttwin[(uchar)(c)]) {
            putchar(c);
            putchar(c);
          } else if (ctrlchar[(uchar)(c)]) {
            switch (c) {
            case '\a':  printf("\\a");  break;
            case '\b':  printf("\\b");  break;
            case '\f':  printf("\\f");  break;
            case '\n':  printf("\\n");  break;
            case '\r':  printf("\\r");  break;
            case '\t':  printf("\\t");  break;
            case '\v':  printf("\\v");  break;
            default:  unexpected(c);
            }
          } else if (strok[(uchar)(c)]) {
            putchar(c);
          } else {
            const char octdigs[] = "01234567";
            putchar('\\');
#if CHAR_BIT == 8
            putchar(octdigs[((uchar)(c)>>6)&3]);
            putchar(octdigs[((uchar)(c)>>3)&7]);
            putchar(octdigs[((uchar)(c)>>0)&7]);
#else
#error Please contribute to-octal conversion code for CHAR_BIT-wide chars.
#endif
          }
        }
        printf("'");
      }
      break;
    case tuple_type:
      {
        tuple *x = (tuple *)b;
        long i;
        printf("nelt=%ld npre=%ld nsuf=%ld",
             x->nelt, x->npre, x->nsuf);
        for (i=1; i<=x->nelt; i++) {
          printf(" ");
          print_blockptr(tupelt(x,i));
        }
      }
      break;
    case routine_type:
      {
        routine *x = (routine *)b;
        printf("proc_pc=%ld",
             x->proc_pc);
      }
      break;
    case iterator_type:
      {
        iterator *x = (iterator *)b;
        printf("itype=%d itstate=%ld constant=",
             x->itype,x->itstate);
        print_blockptr(x->constant);
        printf(" increment=%ld limit=%ld",
              x->increment, x->limit);
      }
      break;
    case table_type:
    case subnode_type:
      {
        subnode *x = (subnode *)b;
        printf("d=");
        print_blockptr(x->d);
        printf(" k=");
        print_blockptr((block *)x->k);
        printf(" llink=");
        print_blockptr((block *)x->llink);
        printf(" rlink=");
        print_blockptr((block *)x->rlink);
        printf(" parent=");
        print_blockptr((block *)x->parent);
        printf(" skip=%ld size=%ld ltag=%d rtag=%d",
              x->skip, x->size, x->ltag,x->rtag);
      }
      break;
    case key_type:
      {
        key *x = (key *)b;
        size_t nbytes = nbits_to_nbytes(x->length);
        size_t i;
        printf("length=%ld ",
             x->length);
        for (i=0; i<nbytes; i++) {
          printf("%02x",
           x->bstring[i]);
        }
      }
      break;
    case source_type:
      {
        source *x = (source *)b;
        printf("filename=");
        print_blockptr((block *)x->filename);
        printf(" srctext=");
        print_blockptr((block *)x->srctext);
      }
      break;
    case codeline_type:
      {
        codeline *x = (codeline *)b;
        printf("srcnum=%ld offset=%ld text=",
             x->srcnum, x->offset);
        print_blockptr((block *)x->text);
      }
      break;
    case instr_type:
      {
        instr *x = (instr *)b;
        long iopnd;
        printf("vindex=%ld op=%d nopnd=%ld",
             x->vindex, x->op,x->nopnd);
        for (iopnd=1; iopnd<=x->nopnd; iopnd++) {
          opndtype type = find_opndtype(x,iopnd);
          switch (type) {
          case data_opnd:
            printf(" (%ld %ld)", instrelt(x,iopnd).data.level,
                                 instrelt(x,iopnd).data.slot);
            break;
          case parm_opnd:
            /*
             *  This covers both parmins and parmouts; and because we
             *  haven't taken the trouble in opndtypes.awk -> opndtypes.c
             *  (the guts of find_opndtype()) to classify backtracks
             *  properly, it adventitiously covers them too:
             */
            printf(" (%ld %ld)", instrelt(x,iopnd).parmin.parmnum,
                                 instrelt(x,iopnd).parmin.slot);
            break;
         case pc_opnd:
            printf(" %ld", instrelt(x,iopnd).pc);
            break;
          case sysrot_opnd:
            printf(" %ld", instrelt(x,iopnd).sysrot);
            break;
          case int_opnd:
            printf(" %d", instrelt(x,iopnd).intval);
            break;
          case long_opnd:
            printf(" %ld", instrelt(x,iopnd).longval);
            break;
          default:
            unexpected (type);
          }
        }
      }
      break;
    case parm_type:
      {
        parm *x = (parm *)b;
        printf("parmnum=%ld slot=%ld",
             x->parmnum, x->slot);
      }
      break;
    case frame_type:
      {
        frame *x = (frame *)b;
        long i;
        printf("nloc=%ld proc_pc=%ld caller_pc=%ld "
               "caller_level=%ld nargs=%ld",
             x->nloc, x->proc_pc, x->caller_pc,
             x->caller_level, x->nargs);
        printf(" link=");
        print_blockptr((block *)x->link);
        printf(" combiter=");
        print_blockptr((block *)x->combiter);
        printf(" elements: ");
        for (i=1; i<=x->nloc; i++) {
          printf(" ");
          print_blockptr(framelt(x,i));
        }
      }
      break;
    case symtab_type:
      {
        symtab *x = (symtab *)b;
        printf("procnames=");
        print_blockptr((block *)x->procnames);
        printf(" procnums=");
        print_blockptr((block *)x->procnums);
        printf(" procaddrs=");
        print_blockptr((block *)x->procaddrs);
        printf(" globals=");
        print_blockptr((block *)x->globals);
        printf(" locals=");
        print_blockptr((block *)x->locals);
        printf(" labels=");
        print_blockptr((block *)x->labels);
        printf(" gbl_backtrax=");
        print_blockptr((block *)x->gbl_backtrax);
        printf(" lcl_backtrax=");
        print_blockptr((block *)x->lcl_backtrax);
        printf(" vcode=");
        print_blockptr((block *)x->vcode);
        printf(" sources=");
        print_blockptr((block *)x->sources);
      }
      break;
    case machine_type:
      {
        machine *x = (machine *)b;
        long level;
        printf("pc=%ld exit_status=%d raw_status=0x%x eof=%d",
             x->pc, x->exit_status,x->raw_status,  x->eof);
        printf(" magic=%d intslash=%d",
              x->magic,x->intslash);
        printf(" code=");
        print_blockptr((block *)x->code);
        printf(" backs=");
        print_blockptr((block *)x->backs);
        printf(" sym=");
        print_blockptr((block *)x->sym);
        printf(" ready_maps=");
        print_blockptr((block *)x->ready_maps);
        printf(" display: ");
        for (level=0; level<DISPLAY_LEVELS; level++) {
          printf(" ");
          print_blockptr((block *)x->display[level]);
        }
      }
      break;
    default:
      printf("(unknown type %d)", b->type);
    } /* end switch */
    printf("\n");
    offset += a->length;
  } /* end while */

  /*
   *  Reset all the marks to their default state
   */
  offset = 0;
  while (offset < front) {
    header *a = (header *) ((address)mem + offset);
    assert (a->length != 0);
    a->mark = unmarked;
    offset += a->length;
  }

} /* end print_heap */

static void print_blockptr(block *a) {
  if (is_om(a)) {
    printf("*");  /* print null pointer (OM) as a star */
  } else {
    address_offset offset = (address)a - (address)mem;
    printf("%zu", offset);  /* otherwise as mem offset */
  }
} /* end print_blockptr */
