/*  ===  A fast allocator for transient uses  ======================  */

/*  $Id: arena.h,v 1.3 2018/02/14 00:45:37 setlorg Exp $  */

/*  Free software (c) dB - see file COPYING for license (GPL).  */

#ifndef _arena_h
#define _arena_h

/*
 *  This "arena allocator" is for short-term allocation where you don't
 *  want to use the C stack.  It works best if your arena_alloc() and
 *  arena_free() requests are strictly stackwise ("LIFO") and balanced.
 *
 *  Overall initialization is done with arena_init(), and finalization
 *  with arena_fini().  If there is a chance of a leak (you didn't do
 *  one free for each alloc), you can use arena_fini_alt() to skip the
 *  normal checks and complete finalization anyway.
 *
 *  There is a concept of "allocation scopes", delimited by
 *  alloc_begin() and alloc_end() which serve as checkpoints where
 *  you think that you have no arena memory currently allocated and
 *  that you have issued alloc and free requests in strict LIFO order
 *  since the last checkpoint.  If you believe there is no chance
 *  of a leak but might have been non-LIFO requests, you can use
 *  arena_end_alt() instead of alloc_end() to check against leaks
 *  and clean things up as if the requests had been in LIFO order
 *  since the last alloc_begin().  If you're not even sure about
 *  leaks, you can resort to arena_end_alt2(), which forgives them
 *  unconditionally and then does like arena_end_alt().
 *
 *  Note that arena_realloc() works best when applied to the most
 *  recently allocated block, and plays well with LIFO calls to
 *  arena_alloc() and arena_free() then.
 */

/* For size_t:  */
#include <sys/types.h>

extern void  arena_init(void);
extern void  arena_fini(void);
extern void  arena_fini_alt(void);
extern void  arena_begin(void);
extern void  arena_end(void);
extern void  arena_end_alt(void);
extern void  arena_end_alt2(void);
extern void *arena_alloc(size_t size);
extern void *arena_realloc(void *mem, size_t oldsize, size_t newsize);
extern void  arena_free(void *mem, size_t oldsize);

#endif  /* _arena_h */
