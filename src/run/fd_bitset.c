/*  ===  A flexible alternative to fd_set  =========================  */

/*  $Id: fd_bitset.c,v 1.7 2022/11/17 14:16:07 setlorg Exp $  */

/*  Free software (c) dB - see file COPYING for license (GPL).  */

#include "setlrun.h"

#define ints_in_fd_set(nfd) \
  ((nfd)>0 ? ((nfd)+(INT_BIT-1))/INT_BIT : 1)
#define bytes_in_fd_set(nfd) \
  (ints_in_fd_set(nfd) * SIZEOF_INT)

void new_fd_bitset(fd_bitset *a, int nfd) {
  a->fdset = (int *) arena_alloc(bytes_in_fd_set(nfd));
}

void null_fd_bitset(fd_bitset *a) {
  a->fdset = NULL;
}

void free_fd_bitset(fd_bitset *a, int nfd) {
  if (a->fdset != NULL) {
    arena_free(a->fdset, bytes_in_fd_set(nfd));
    a->fdset = NULL;
  }
}

void init_fd_bitset(fd_bitset *a, int nfd) {
  new_fd_bitset(a, nfd);
  zero_fd_bitset(a, nfd);
}

void zero_fd_bitset(fd_bitset *a, int nfd) {
  if (a->fdset != NULL) {
    int i;
    for (i=0; i<ints_in_fd_set(nfd); i++) {
      a->fdset[i] = 0L;
    }
  }
}

void copy_fd_bitset(fd_bitset *a, const fd_bitset *b, int nfd) {  /* := */
  if (b->fdset != NULL) {
    int i;
    assert (a->fdset != NULL);
    for (i=0; i<ints_in_fd_set(nfd); i++) {
      a->fdset[i] = b->fdset[i];
    }
  } else {
    a->fdset = NULL;
  }
}

void merge_fd_bitset(fd_bitset *a, const fd_bitset *b, int nfd) {  /* +:= */
  if (b->fdset != NULL) {
    int i;
    assert (a->fdset != NULL);
    for (i=0; i<ints_in_fd_set(nfd); i++) {
      a->fdset[i] |= b->fdset[i];
    }
  }
}

bool is_empty_fd_bitset(const fd_bitset *a, int nfd) {
  if (a->fdset != NULL) {
    int i;
    for (i=0; i<ints_in_fd_set(nfd); i++) {
      if (a->fdset[i] != 0L) return false;
    }
  }
  return true;
}

#define FDSET_INDEX(fd) ((fd) / INT_BIT)
#define FDSET_LIMB(s,fd) (((int *)(s))[FDSET_INDEX(fd)])
#define FDSET_BIT(fd) ((fd) % INT_BIT)
#define FDSET_MASK(fd) (1UL << FDSET_BIT(fd))

#undef FD_SET
#define FD_SET(fd,s) ((void) (FDSET_LIMB(s,fd) |= FDSET_MASK(fd)))

void set_fd_bit(fd_bitset *a, int fd, int nfd) {
  assert (0 <= fd && fd < nfd);
  assert (a->fdset != NULL);
  FD_SET(fd, fd_set_ptr(a));
}

#undef FD_CLR
#define FD_CLR(fd,s) ((void) (FDSET_LIMB(s,fd) &= ~FDSET_MASK(fd)))

void clear_fd_bit(fd_bitset *a, int fd, int nfd) {
  assert (0 <= fd && fd < nfd);
  if (a->fdset != NULL) FD_CLR(fd, fd_set_ptr(a));
}

#undef FD_ISSET
#define FD_ISSET(fd,s) ((FDSET_LIMB(s,fd) & FDSET_MASK(fd)) != 0)

bool is_set_fd_bit(const fd_bitset *a, int fd, int nfd) {
  assert (0 <= fd && fd < nfd);
  return a->fdset != NULL && FD_ISSET(fd, fd_set_ptr(a));
}

int first_fd_in_bitset(const fd_bitset *a, int nfd) {
  int fd;
  for (fd=0; fd<nfd && !is_set_fd_bit(a, fd, nfd); fd++) ;
  return fd;  /* nfd (invalid) if bitset is empty */
}
