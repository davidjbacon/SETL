/*  ===  A flexible alternative to fd_set  =========================  */

/*  $Id: fd_bitset.h,v 1.8 2022/11/17 14:16:07 setlorg Exp $  */

/*  Free software (c) dB - see file COPYING for license (GPL).  */

#ifndef _fd_bitset_h
#define _fd_bitset_h

/*
 *  The fd_set of select()/pselect() fame is a fixed-size struct
 *  capable of representing up to FD_SETSIZE file descriptors as a
 *  bit-set.  However, the actual maximum number of fds that a
 *  process can obtain is usually much higher.  In the shell, the
 *  "ulimit -n" command can be used to inspect and set both the
 *  soft and hard limit; and in C, those functions can be performed
 *  by getrlimit() and setrlimit() with the RLIMIT_NOFILE parameter
 *  (the soft limit can also be inspected by sysconf(_SC_OPEN_MAX)).
 *
 *  Since select() and pselect() take fd_set pointers, they should
 *  in theory be subject to the FD_SETSIZE limit.  But in practice,
 *  they are not so limited on any Unix-type system this author has
 *  ever encountered, but in fact happily handle larger bit-sets.
 *
 *  Moreover, there is general though unofficial agreement on the
 *  implementation of fd_set:  it never contains more than an array
 *  of integers, and within each of those "limbs", the low-order bit
 *  corresponds to the lowest-numbered file descriptor.  This can be
 *  checked by C code at 'configure' time, and overall correct
 *  functioning can be checked by SETL code in the test suite.
 *
 *  However, while select() and pselect() are happy to deal with
 *  "extended" bit-sets, the FD_* macros are another story.  The
 *  FD_ZERO macro clears a fixed amount of memory, and in newer
 *  versions of GCC that have _FORTIFY_SOURCE suitably enabled,
 *  macros such as FD_SET check the fd arg against a fixed upper
 *  bound.
 *
 *  The fd_bitset defined here is thus intended as an incompatible
 *  replacement for the standard fd_set that can represent a number
 *  of bits that is only known at run time.  The only use of fd_set
 *  is for dealing with select() and pselect(), so instances of it
 *  need not have more than a transient existence, and accordingly
 *  fd_bitsets are allocated using the arena allocator.  Each call
 *  to new_fd_bitset() must be balanced by a free_fd_bitset() call.
 *
 *  To get a fd_set pointer for passing to select() or pselect(),
 *  use the fd_set_ptr() macro.
 */

typedef struct {
  /*
   *  Would prefer 'long' here, but in practice, fd_set may have
   *  only an array of 'int' internally, so we can't make such bold
   *  assumptions about the alignment of fd_set.
   */
  int *fdset;
} fd_bitset;

/* Use of this macro depends on an externally provided def of fd_set.
 * This file is only intended as a header fragment, to be #included by
 * a client such as setlrun.h.  */
#define fd_set_ptr(a)  ((fd_set *) (a)->fdset)

extern void new_fd_bitset(fd_bitset *a, int nfd);
extern void null_fd_bitset(fd_bitset *a);
extern void free_fd_bitset(fd_bitset *a, int nfd);
extern void init_fd_bitset(fd_bitset *a, int nfd);
extern void zero_fd_bitset(fd_bitset *a, int nfd);
extern void copy_fd_bitset(fd_bitset *a, const fd_bitset *b, int nfd);
extern void merge_fd_bitset(fd_bitset *a, const fd_bitset *b, int nfd);
extern bool is_empty_fd_bitset(const fd_bitset *a, int nfd);
extern void set_fd_bit(fd_bitset *a, int fd, int nfd);
extern void clear_fd_bit(fd_bitset *a, int fd, int nfd);
extern bool is_set_fd_bit(const fd_bitset *a, int fd, int nfd);
extern int first_fd_in_bitset(const fd_bitset *a, int nfd);

#endif  /* _fd_bitset_h */
