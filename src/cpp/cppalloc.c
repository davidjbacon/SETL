/* Part of CPP library.  (memory allocation - xmalloc etc)
   Copyright (C) 1986, 87, 89, 92, 93, 94, 1995 Free Software Foundation, Inc.
   Written by Per Bothner, 1994.
   Based on CCCP program by by Paul Rubin, June 1986
   Adapted to ANSI C, Richard Stallman, Jan 1987

This program is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

 In other words, you are welcome to use, share and improve this program.
 You are forbidden to forbid anyone else to use, share and improve
 what you give them.   Help stamp out software-hoarding!  */

#include "cpplib.h"

static void memory_full (void);
void *xmalloc (unsigned size);
void *xrealloc (void *old, unsigned size);
void *xcalloc (unsigned number, unsigned size);

static void
memory_full (void)
{
  fatal ("Memory exhausted.");
}

void *
xmalloc (size)
     unsigned size;
{
  void *ptr = malloc (size);
  if (ptr != 0) return (ptr);
  memory_full ();
  /*NOTREACHED*/
  return 0;
}

void *
xrealloc (old, size)
     void *old;
     unsigned size;
{
  void *ptr = realloc (old, size);
  if (ptr == 0)
    memory_full ();
  return ptr;
}

void *
xcalloc (number, size)
     unsigned number, size;
{
  void *ptr = calloc (number, size);
  if (ptr == 0)
    memory_full ();
  return ptr;
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
