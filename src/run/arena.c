/*  ===  A fast allocator for transient uses  ======================  */

/*  $Id: arena.c,v 1.6 2021/03/05 20:59:53 setlorg Exp $  */

/*  Free software (c) dB - see file COPYING for license (GPL).  */

/*
 *  The arena is implemented as a doubly-linked list of segments.
 *  Allocated memory blocks are within those segments.  Segments and
 *  their memory are allocated by os_malloc() as needed, and all
 *  released by os_free() when the arena is finalized.
 */

#include "setlrun.h"

typedef struct segment  segment_t;
struct segment {
  size_t      size;         /* how big this segment is */
  size_t      offset;       /* how much of it is in use */
  char       *space;        /* user memory as an array of chars */
  segment_t  *prev, *next;  /* arena is doubly-linked list of segments */
};

/*
 *  In C++, the arena would be a natural object, with these file-local
 *  variables among its private class members.
 */
static segment_t  *head_segment = NULL;   /* head of segment list */
static segment_t  *cur_segment;  /* current allocation segment */
static long        n_alloc = 0;  /* number of user blocks allocated */

#define ARENA_SEGMENT_GRANULARITY  4096  /* segment size multiple */
#define ARENA_INIT_SEGMENT_SIZE  round_up(4096, ARENA_SEGMENT_GRANULARITY)

/*
 *  Allocate and initialize a segment.
 */
static segment_t *get_segment(size_t size) {
  segment_t *seg = (segment_t *) os_malloc(sizeof *seg);
  seg->size = size;
  seg->offset = 0;
  seg->space = (char *) os_malloc(size);
  seg->prev = NULL;
  seg->next = NULL;
  return seg;
}

/*
 *  Finalize all the segments and return their memory to the system.
 */
static void free_segments(bool check_offset) {
  /*
   *  For philosophical purity, release the system-allocated memory in
   *  the reverse order of its acquisition.
   */
  while (cur_segment->next != NULL) {  /* seek to end of list */
    cur_segment = cur_segment->next;
  }
  while (cur_segment != NULL) {  /* free segments following prev ptrs */
    segment_t *prev_segment = cur_segment->prev;
    if (check_offset) {
      assert (cur_segment->offset == 0);
    }
    os_free(cur_segment->space);
    if (prev_segment == NULL) {
      assert (cur_segment == head_segment);  /* a penultimatecondition */
    }
    os_free(cur_segment);
    cur_segment = prev_segment;
  }
  head_segment = NULL;
}

/*
 *  Initialize the arena and give it a first segment.
 */
void arena_init(void) {
  assert (n_alloc == 0);
  assert (head_segment == NULL);
  head_segment = get_segment (ARENA_INIT_SEGMENT_SIZE);
  cur_segment = head_segment;
}

/*
 *  Finalize the arena, giving back all the segments.
 */
void arena_fini(void) {
  assert (n_alloc == 0);
  assert (head_segment != NULL);
  assert (head_segment == cur_segment);
  free_segments(true);
}

/*
 *  If you are not sure you have freed all blocks allocated since
 *  the last arena_init(), use this as the finalizer corresponding
 *  to arena_init() rather than the tighter arena_fini(), in order to
 *  bypass certain checks and still have all the segments automatically
 *  freed anyway.
 */
void arena_fini_alt(void) {
  n_alloc = 0;
  assert (head_segment != NULL);
  free_segments(false);
}

/*
 *  Begin allocation scope.
 */
void arena_begin(void) {
  assert (n_alloc == 0);
  assert (cur_segment == head_segment);
  assert (cur_segment->offset == 0);
}

/*
 *  If you believe you have given back everything allocated since the
 *  last arena_begin(), in strictly stackwise (LIFO) order, this checks
 *  that.
 */
void arena_end(void) {
  assert (n_alloc == 0);
  assert (cur_segment == head_segment);
  assert (cur_segment->offset == 0);
}

/*
 *  You can use this instead of arena_end() if you are not sure your
 *  allocation pattern has been strictly LIFO since the last
 *  arena_begin().
 */
void arena_end_alt(void) {
  assert (n_alloc == 0);  /* still require #allocs = #frees though! */
  while (cur_segment->prev != NULL) {
    cur_segment->offset = 0;
    cur_segment = cur_segment->prev;
  }
  assert (cur_segment == head_segment);
  cur_segment->offset = 0;
}

/*
 *  If you might have leaked too, this lets you end the allocation
 *  scope and leave things clean anyway.
 */
void arena_end_alt2(void) {
  n_alloc = 0;  /* forgive any missing frees */
  arena_end_alt();  /* clean up the data structures */
}

/*
 *  Get a block.
 */
void *arena_alloc(size_t size) {
  if (cur_segment->offset + size <= cur_segment->size) {
    char *space = &cur_segment->space[cur_segment->offset];
    void *mem = space;
    cur_segment->offset += size;
    ++n_alloc;
    return mem;
  } else if (cur_segment->next != NULL) {
    cur_segment = cur_segment->next;
    assert (cur_segment->offset == 0);
    return arena_alloc(size);  /* recurse to retry with next segment */
  } else {  /* must allocate new segment */
    const size_t segsize = round_up (2 * (cur_segment->offset + size),
                                     ARENA_SEGMENT_GRANULARITY);
    segment_t *new_segment = get_segment(segsize);
    /* Link new segment in at end of list, and advance cur_segment.  */
    cur_segment->next = new_segment;
    new_segment->prev = cur_segment;
    cur_segment = new_segment;
    return arena_alloc(size);  /* recurse with guarantee of success */
  }
}

/*
 *  Resize a block, relocating it if necessary.
 */
void *arena_realloc(void *mem, size_t oldsize, size_t newsize) {
  char *space;
  if (mem == NULL) {
    return arena_alloc(newsize);
  }
  assert (n_alloc > 0);
  space = (char *)mem;
  if (&space[oldsize] == &cur_segment->space[cur_segment->offset]) {
    /* Block is top of stack, within the current segment.  If it will
     * still fit in the segment after being resized, don't move it.  */
    size_t new_offset;
    assert (cur_segment->offset >= oldsize);
    new_offset = cur_segment->offset - oldsize + newsize;
    if (new_offset <= cur_segment->size) {
      /* It still fits.  Adjust offset up or down for the new size.  */
      cur_segment->offset = new_offset;
      /* It is possible for this offset to be 0 when newsize is 0.
       * Unlike arena_free(), we do not chase prev ptrs when this
       * happens, because an arena_free() is still required on the
       * now 0-sized block, and the ptr chase can happen then.  */
      return mem;
    } else {
      /* It doesn't fit.  But because it is the top block, we can
       * simulate freeing it even before allocating its new loc, and
       * thus effectively preserve a LIFO allocation pattern, by
       * pre-adjusting n_alloc and cur_segment->offset.  (It wouldn't
       * do to post-adjust the latter, as arena_alloc() will have
       * changed cur_segment by then.)  */
      void *new_mem;
      --n_alloc;
      cur_segment->offset -= oldsize;
      new_mem = arena_alloc(newsize);
      mvmem (new_mem, mem, oldsize);
      return new_mem;
    }
  } else {
    /* Block is not top of stack.  Allocate a new block, copy data,
     * free the old, and return the new.  Note that this case results
     * in non-LIFO allocation, as is true any time you arena_free() a
     * block other than the top one.  */
    const size_t minsize = MIN(oldsize, newsize);
    void *new_mem = arena_alloc(newsize);
    mvmem (new_mem, mem, minsize);
    arena_free(mem, oldsize);
    return new_mem;
  }
}

/*
 *  Release a block.
 */
void arena_free(void *mem, size_t oldsize) {
  char *space = (char *)mem;
  assert (n_alloc > 0);
  --n_alloc;
  if (&space[oldsize] == &cur_segment->space[cur_segment->offset]) {
    /* Block is top of stack.  Give it back to the segment immediately.
     * If that leaves the segment empty, set the current segment back
     * to the previous segment if any, and repeat while that segment is
     * empty too.  */
    assert (cur_segment->offset >= oldsize);
    cur_segment->offset -= oldsize;
    while (cur_segment->offset == 0 && cur_segment->prev != NULL) {
      cur_segment = cur_segment->prev;
    }
  } /* else abandon block.  It will be recovered later.  */
}
