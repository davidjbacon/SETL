/*  ===  Symbol table stack manager  ================================ */

/*  $Id: symstack.h,v 1.1 2009/02/15 09:37:02 setlorg Exp $  */

/*  Free software (c) dB - see file COPYING for license (GPL).  */

/*
 *  This is just a bunch of static declarations and macros that are
 *  used by identify.c, canonize.c, and gencode.c for managing a
 *  stack of symbol tables.  Also shoe-horned in is 'glowball', whose
 *  symbol table keeps track of "locals" created in global declarations
 *  such as the x in "CONST s = {x : x in ...}", where s is of scope
 *  global to the unit, but x is not.
 */

static symstack *inner;   /* current top of stack of symbol tables */

/* The symbol that contains the symtab at the top of the stack:  */
#define curholder      (inner->holder)

/* The symtab at the top of the stack:  */
#define cursymtab      (curholder->stab)

/* Push symtab-bearing symbol x onto the stack:  */
#define pushsymtab(x)  symstackpush (&inner, x)

/* Poppa!  */
#define popsymtab()    symstackpop  (&inner)

/* A special symbol whose symtab maps locals implicitly created by
 * the right-hand sides of complex global declarations:  */
static symbol *glowball;  /* shines in the dark */
