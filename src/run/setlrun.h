/*  ===  Declarations shared by the SETL run-time modules  =========  */

/*  $Id: setlrun.h,v 1.232 2024/04/08 18:57:22 setlorg Exp $  */

/*  Free software (c) dB - see file COPYING for license (GPL).  */

#ifndef _setlrun_h
#define _setlrun_h

/*
 * ###################################################################
 * #                                                                 #
 * #  Please read the following note carefully before attempting to  #
 * #  modify the SETL run-time program (VM interpreter) in any way!  #
 * #                                                                 #
 * ###################################################################
 *
 *  In order to keep this SETL implementation immune to memory
 *  fragmentation, all blocks on the dynamic "heap" are relocatable
 *  by the compacting garbage collector (see heap.c).
 *
 *  THEREFORE ALL BLOCK POINTERS MUST BE TRACKED so that the
 *  garbage collector can update them after its compacting phase.
 *
 *  Block pointers within the heap itself are handled automatically
 *  by the garbage collector, but those outside the heap, including
 *  all the transiently held pointers on the C stack ("locals"), must
 *  be made known to the garbage collector explicitly.  This is done
 *  by putting "handles" (pointers to the block pointers) on them,
 *  and linking the handles into a list that is accessible to the
 *  garbage collector.
 *
 *  Conventionally, handles are put on by the 'ref' macro, and
 *  removed by the 'retire' macro.  This is so critical, and at the
 *  same time such a burden, that a whole project called SETL/C++
 *  was inspired by the idea of using "smart pointers" to automate
 *  this (and the management of reference counts, which are not
 *  featured here) through constructors and destructors.  On what
 *  would now be called the RAII principle.
 *
 *  Anyway, in this SETL implementation in C, handles have to be put
 *  on and taken off manually as it were, by strict adherence to the
 *  following rules.  The meaning of "block pointer" in these rules
 *  is "pointer that points to a block that is inside the heap".
 *  These blocks are precisely those which conceptually derive from
 *  'block' (see the comments above 'block_struct' below).  There
 *  are no such blocks outside the heap, nor are there any other
 *  kinds of record inside the heap (except for the memory management
 *  header preceding each block).  Furthermore, if p is a block
 *  pointer, and q is a copy of p, then q is another, separate
 *  block pointer, just as much in need of updating upon garbage
 *  collection as p is.  (It is all too easy to think of q as being
 *  "the same" pointer.  But q is not the same pointer; it just
 *  happens to have the same value as p!)  So:
 *
 *   1.  Every block pointer must have a handle if there is any
 *       chance of a heap allocation occurring between the time it
 *       is created and the next time it is dereferenced.
 *
 *   2.  No block pointer may have more than one handle.  "Double
 *       updates" are just as disastrous as failures to update.
 *
 *   3.  Never put a handle on a block pointer that is in the heap
 *       (i.e., in a block).  In effect, those pointers already have
 *       handles; see rule 2.
 *
 *   4.  Every handle that is acquired by 'ref' must be released by
 *       'retire' before the referenced block pointer goes out of
 *       scope.
 *
 *   5.  Handles must be released in exactly the reverse order of
 *       their acquisition.
 *
 *   6.  Every block pointer, while it has a handle on it, must point
 *       to a block (which by definition is in the heap) or be NULL.
 *
 *  Note that by the rules of C, when a called routine ("callee") is
 *  passed a block pointer, it receives a copy of that block pointer,
 *  but when it is passed a pointer to a block pointer (such as when
 *  it is supposed to modify a block), the block pointer itself is
 *  not copied until the callee dereferences the pointer-to-pointer
 *  in an rvalue position.
 *
 *  In practice, the way this works out is that for "one star" block
 *  pointer parameters, the callee just puts on its own handle.  For
 *  a "two star" block pointer parameter, say "block **p", the callee
 *  makes a local copy of *p, say q, calls ref(q) if it needs to,
 *  and makes sure as one of its last acts to copy q back to *p.
 *
 *  Thus the caller does not in general put a handle on a block
 *  pointer if it does not itself chase the pointer but merely
 *  passes the address of the pointer to a callee.
 *
 *  Note that for a two star pointer p, the callee should not simply
 *  call ref(*p) directly and then refer to *p throughout the routine
 *  instead of using a local copy of *p, because it cannot in general
 *  know that the caller has not put a handle on *p.
 *
 *  See the comments at the top of "heap.c" for tools and hints for
 *  detecting and debugging heap damage.
 *
 *  See also the comments above the 'let' macro below regarding a
 *  subtlety which arises in connection with complicated lvalue
 *  expressions that refer to block pointers in C assignments.
 *
 *  Now for the pop quiz.  Is the following safe?
 *
 *    extern void f(integer *, integer *);
 *    integer *p = new_integer(55);
 *    HANDLE hp = ref(p);
 *    f(new_integer(47), p);
 *
 *  The answer is that it is not.  Now, does the following replacement
 *  for the last line fix it?
 *
 *    integer *temp = new_integer(47);
 *    f(temp, p);
 *
 *  The answer is that it does, which may seem surprising given that
 *  no new handle was introduced.  But what was introduced was a
 *  sequence point.  C does not specify an evaluation order among
 *  function args, so in the first case, a copy of the handled block
 *  pointer p might be pushed onto the C stack, then new_integer(47)
 *  called and its result pushed, and and then f() called.  If the
 *  call to new_integer(47) causes heap relocation, the copy of p on
 *  the stack becomes invalid.  This can actually happen on gcc/x86,
 *  where args are pushed in reverse order.  In the fixed version,
 *  there is no possibility of allocation during evaluation of the
 *  function args, so only valid pointers are passed to f().
 */


#include "common.h"

#if HAVE_GMP_H
#include <gmp.h>
#else  /* try for the local bundled one */
#include "gmp.h"
#endif

/*
 *  Fast transient stackwise memory allocation not on the C stack.
 */
#include "arena.h"

/*
 *  Generalization of the fd_set type used in select()/pselect() args.
 */
#include "fd_bitset.h"


#if HAVE_LLONG
/*
 *  Under some versions of GCC, these only get defined in C99 mode;
 *  but for now I am maintaining C89 compatibility too, or trying to.
 *
 *  Unlike the constants usually defined by <limits.h>, these versions
 *  don't have values that are known at preprocessing time, so cannot
 *  be used in arithmetic comparisons like "#if ULLONG_MAX > ULONG_MAX".
 *  Those tests would have to be done at config time.
 */
#ifndef ULLONG_MAX
#define ULLONG_MAX TYPE_MAXVAL(ullong)
#endif
#ifndef LLONG_MAX
#define LLONG_MAX  TYPE_MAXVAL(llong)
#endif
#ifndef LLONG_MIN
#define LLONG_MIN  TYPE_MINVAL(llong)
#endif
#endif /* HAVE_LLONG */

/*
 *  Max and min values for some POSIX types.
 */
#define OFF_T_MAX  TYPE_MAXVAL(off_t)
#define OFF_T_MIN  TYPE_MINVAL(off_t)
#define PID_T_MAX  TYPE_MAXVAL(pid_t)
#define PID_T_MIN  TYPE_MINVAL(pid_t)
#define UID_T_MAX  TYPE_MAXVAL(uid_t)
#define UID_T_MIN  TYPE_MINVAL(uid_t)
#define GID_T_MAX  TYPE_MAXVAL(gid_t)
#define GID_T_MIN  TYPE_MINVAL(gid_t)
#define MODE_T_MAX TYPE_MAXVAL(mode_t)
#define MODE_T_MIN TYPE_MINVAL(mode_t)

/*
 *  DOUBLE_REP_DIGS is the number of significant digits produced by
 *  default when converting a 'double' to string form.  It is chosen
 *  to give good (but not perfect) recovery of original values when
 *  they are "read" back in (e.g., by 'getval' or 'unstr'), while not
 *  reporting so many digits that (for example) numbers close to a
 *  round number of tenths come out with long strings of 0's or 9's
 *  followed by a junk digit at the far right end.  A reasonable rule
 *  of thumb might be to choose DOUBLE_REP_DIGS as 1 or 2 less than
 *  the number of digits that gives you perfect recovery down to the
 *  last bit.
 */
#ifndef DOUBLE_REP_DIGS
#define DOUBLE_REP_DIGS 15  /* a default suitable for IEEE 754 */
#endif

#ifndef INADDR_NONE
#define INADDR_NONE 0xffffffff
#endif


/*
 *  Choose POSIX real-time interval timers if available, or
 *  fall back to more traditional interval timers otherwise.
 *
 *  (The real-time ones have been defined by POSIX for a long time,
 *  but are not in Darwin even as of 2022.  The traditional ones
 *  have been marked obsolete as of POSIX.1-2008, but are still
 *  widely supported.)
 */
#if HAVE_TIMER_CREATE && HAVE_TIMER_DELETE && \
    HAVE_TIMER_GETTIME && HAVE_TIMER_SETTIME
#define USE_POSIX_RT 1
#elif HAVE_GETITIMER && HAVE_SETITIMER
#undef USE_POSIX_RT
#else
#error Need POSIX real-time interval timers or getitimer/setitimer
#endif

#ifdef _POSIX_VERSION
/*
 *  Use POSIX "extended" syntax for regular expressions (EREs).
 */
#ifndef USE_REGEX
#define USE_REGEX
#endif
#endif

/*
 *  Round x up to the nearest multiple of y.
 */
#define round_up(x,y)  ((x)+((y)-(x)%(y))%(y))

/*
 *  How many whole x's will fit in y.
 */
#define ceil_div(x,y)  (round_up(x,y)/(y))

/*
 *  The SETL value "OM" is represented as a NULL pointer in this
 *  implementation.
 */
#ifndef OM
#define OM 0
#endif

/*
 *  Definitions generated from tables.
 */
#include "opcode_defns.h"
#include "sysrot_defns.h"
#include "sysdat_defns.h"

/*
 *  Maximum number of distinct signals.
 */
#ifdef NSIG
#define N_SIGNAL NSIG
#else
#define N_SIGNAL 64
#endif

/*
 *  Signal disposition (SIG_DFL, SIG_IGN, or function pointer) type.
 */
typedef void (*sig_disp)(int);

/*
 *  Shortest interval supported by REAL-MS timer streams.
 */
#define MIN_REAL_MS 1

/*
 *  Proleptic run-time support for lexical scope nesting (see the
 *  'display' field of struct machine_struct).
 */
#define DISPLAY_LEVELS 2L          /* "global" and "local"            */
#define NO_LEVEL (-1)              /* use when "level" makes no sense */
#define GLOBAL_LEVEL 0             /* only one frame is at this level */
#define LOCAL_LEVEL (DISPLAY_LEVELS-1)  /* degenerate "current" level */

/*
 *  Enumeration types
 */

/* All the types of block that can go on the heap:  */
enum blocktype_enum {
  om_type,         /* a phony type code for OM                        */
  atom_type,
  boolean_type,
  small_type,
  integer_type,
  real_type,
  set_type,
  string_type,
  tuple_type,
  routine_type,
  iterator_type,
  table_type,
  subnode_type,
  key_type,
  source_type,
  codeline_type,
  instr_type,
  parm_type,
  frame_type,
  symtab_type,
  machine_type,
  invalid_type,    /* used only as a return value by 'denotype'       */
blocktypes_end};

/* Set types, in order from weakest to strongest (most structured):   */
enum settype_enum {
  plain_set,
  mmap,
  smap,
settypes_end};

/* Iterator types:  */
enum itertype_enum {
  counter,
  plain_iter,
  mmap_iter,
  smap_iter,
itertypes_end};

/* What can be a set or domain element (observe alphabetical order):  */
enum keytype_enum {
  atom_key,
  boolean_key,
  integer_key,
  om_key,          /* om_key only occurs within tuple keys            */
  real_key,
  routine_key,
  set_key,
  small_key,       /* currently supported but unused as a key         */
  string_key,
  tuple_key,
keytypes_end};

/* Kinds of "file" ("stream" in documentation) supported by GNU SETL: */
enum filetype_enum {
  no_file,         /* undefined file                                  */
  stream_file,     /* regular input or output file, possibly seekable */
  direct_file,     /* direct-access (seekable) file                   */
  twoway_file,     /* bidirectional stream                            */
  pipe_file,       /* connection to stdin and/or stdout of child      */
  tty_pipe_file,   /* like pipe_file but using master/slave pty/tty   */
  tcp_socket_file, /* TCP client socket, or peer socket from ACCEPT   */
  tcp_server_file, /* TCP server socket that can ACCEPT               */
  udp_client_file, /* UDP client socket (can SEND, RECV)              */
  udp_server_file, /* UDP server socket (can SENDTO, RECVFROM)        */
  unix_socket_file, /* Like tcp_socket_file but for Unix-domain       */
  unix_server_file, /* Like tcp_server_file but for Unix-domain       */
  unix_datagram_client_file, /* Unix-domain, can only SEND            */
  unix_datagram_server_file, /* Unix-domain, can SENDTO and RECVFROM  */
  signal_file,     /* a source of signals you can "read"              */
  sig_ign_file,    /* open >= 1 of these to SIG_IGN a signal          */
  sig_dfl_file,    /* open >= 1 of these to SIG_DFL a signal          */
  real_ms_file,    /* real-time interval alarm in ms (milliseconds)   */
filetypes_end};

/* Kinds of operand in instructions of the SETL virtual machine:      */
enum opndtype_enum {
  data_opnd,       /* operand is a lexical level and slot number      */
  parm_opnd,       /* operand describes a parmin or parmout           */
  pc_opnd,         /* operand is a code tuple index                   */
  sysrot_opnd,     /* operand is a system routine serial number       */
  int_opnd,        /* operand is an int                               */
  long_opnd,       /* operand is a long                               */
opndtypes_end};

/* Intentions with respect to 'getfd' (see sys.c):                    */
enum getfd_op_enum {
  getfd_check,     /* non-trapping check for fd open                  */
  getfd_info,      /* anything but one of the following               */
  getfd_getfile,   /* GETFILE (which returns OM on auto-open failure) */
  getfd_putfile,   /* PUTFILE (the only auto-closing outputter)       */
  getfd_read,      /* stream READ* and other GET* calls               */
  getfd_write,     /* stream WRITE*, PUT* and PRINT* calls            */
  getfd_seek,      /* SEEK and REWIND                                 */
  getfd_gets,      /* GETS                                            */
  getfd_puts,      /* PUTS                                            */
  getfd_flush,     /* FLUSH                                           */
  getfd_ftrunc,    /* FTRUNC                                          */
  getfd_accept,    /* ACCEPT (for stream server sockets)              */
  getfd_recv,      /* RECV (for UDP client sockets)                   */
  getfd_send,      /* SEND (for datagram client sockets)              */
  getfd_recvfrom,  /* RECVFROM (for datagram server sockets)          */
  getfd_sendto,    /* SENDTO (for datagram server sockets)            */
  getfd_recv_fd,   /* RECV_FD (receive a file descriptor)             */
  getfd_send_fd,   /* SEND_FD (send a file descriptor)                */
  getfd_sockaddr,  /* enquiries that use struct sockaddr              */
  getfd_sel_r,     /* SELECT for input (e.g. READ, ACCEPT, RECV)      */
  getfd_sel_w,     /* SELECT for output (e.g. WRITE, SEND)            */
  getfd_sel_e,     /* SELECT for exceptional condition noted          */
  getfd_shut_r,    /* shutdown of input side of socket                */
  getfd_shut_w,    /* shutdown of output side of socket               */
  getfd_shut_rw,   /* shutdown of both sides of socket                */
  getfd_close,     /* just want to close the file if open             */
getfd_ops_end};

/* Buffering policies:                                                */
enum buffering_enum {
  no_buffering,      /* no io_buffer even allocated                   */
  input_buffering,   /* read-only; no need for output buffer          */
  byte_buffering,    /* flush every byte immediately                  */
  line_buffering,    /* auto-flush after newlines or as necessary     */
  full_buffering,    /* auto-flush only when output buffer full       */
bufferings_end};

/* Ways the SETL programmer can OPEN things:                          */
/* This enum has to stay in sync with array open_modes[] in sys.c:    */
enum open_how_enum {
  open_read = 0,       /* seq and direct access input on existing     */
  open_write,          /* seq and direct access output, clobber old   */
  open_new,            /* like open_write, but create new file        */
  open_append,         /* sequential output at end of file            */
  open_direct,         /* direct access on existing, start rewound    */
  open_direct_empty,   /* direct access, empty file first             */
  open_direct_new,     /* direct access, new file                     */
  open_direct_append,  /* direct-access input; output at end only     */
  open_rw,             /* bidirectional sequential I/O                */
  open_pipe_from,      /* input from command                          */
  open_pipe_to,        /* output to command                           */
  open_pump,           /* input/output from/to command                */
  open_tty_pump,       /* input/output from/to pty-wrapped command    */
  open_tcp_client,     /* TCP client socket                           */
  open_tcp_server,     /* TCP server socket                           */
  open_tcp_peer,       /* already-open TCP connection                 */
  open_udp_client,     /* can SEND and RECV                           */
  open_udp_server,     /* can RECVFROM and SENDTO                     */
  open_unix_client,    /* Like open_tcp_client but for Unix-domain    */
  open_unix_server,    /* Like open_tcp_server but for Unix-domain    */
  open_unix_peer,      /* already-open Unix-domain connection         */
  open_unix_datagram_client,  /* can SEND                             */
  open_unix_datagram_server,  /* can RECVFROM and SENDTO              */
  open_signal,         /* get newline per caught signal               */
  open_ignore,         /* signal to be ignored                        */
  open_default,        /* signal to be set to SIG_DFL                 */
  open_real_ms,        /* get newline per timer expiry                */
open_hows_end};

/* CLOSE modes for pipe and pump streams:                             */
enum close_how_enum {
  close_await,     /* do a blocking waitpid() for the child           */
  close_autoreap,  /* wait away the child in our SIGCHLD handler      */
  close_zombie,    /* let it be a zombie (user can WAITPID it away)   */
  close_atfork,    /* a back door for atfork_child(), not for CLOSE   */
close_hows_end};

/* Character classification by UNPRETTY:                              */
enum backslash_chaser_enum {
  unescapable,  /* next character should not be preceded by backslash */
  repself,      /* next character stands for itself                   */
  control,      /* next character is "control", e.g. \n for newline   */
  hexlead,      /* \x... means hexadecimal "escape"                   */
  octlead,      /* \[0-3]... means octal "escape"                     */
backslash_chasers_end};


/*
 *  Typedefs.
 */

typedef enum blocktype_enum           blocktype;
typedef enum settype_enum             settype;
typedef enum itertype_enum            itertype;
typedef enum keytype_enum             keytype;
typedef enum filetype_enum            filetype;
typedef enum opndtype_enum            opndtype;
typedef enum getfd_op_enum            getfd_op;
typedef enum buffering_enum           buffering;
typedef enum open_how_enum            open_how;
typedef enum close_how_enum           close_how;
typedef enum backslash_chaser_enum    backslash_chaser;

typedef struct handle_struct            handle;
typedef struct block_struct    ALIGNED  block;
typedef struct atom_struct     ALIGNED  atom;
typedef struct boolean_struct  ALIGNED  boolean;
typedef struct small_struct    ALIGNED  small;
typedef struct integer_struct  ALIGNED  integer;
typedef struct real_struct     ALIGNED  real;
typedef struct set_struct      ALIGNED  set;
typedef struct string_struct   ALIGNED  string;
typedef struct tuple_struct    ALIGNED  tuple;
typedef struct routine_struct  ALIGNED  routine;
typedef struct iterator_struct ALIGNED  iterator;
typedef struct source_struct   ALIGNED  source;
typedef struct codeline_struct ALIGNED  codeline;
typedef union  operand_union            operand;
typedef struct instr_struct    ALIGNED  instr;
typedef struct call_struct     ALIGNED  call;
typedef struct proc_struct     ALIGNED  proc;
typedef struct parm_struct     ALIGNED  parm;
typedef struct frame_struct    ALIGNED  frame;
typedef struct symtab_struct   ALIGNED  symtab;
typedef struct machine_struct  ALIGNED  machine;
typedef struct key_struct      ALIGNED  key;
typedef struct subnode_struct  ALIGNED  subnode;
typedef struct subnode_struct  ALIGNED  table;

typedef struct allowed_struct         allowed;
typedef struct io_buffer_struct       io_buffer;
typedef struct open_mode_struct       open_mode;
typedef struct file_struct            file;
typedef struct sig_info_struct        sig_info;
typedef struct pid_list_struct        pid_list;

#ifndef uchar
typedef unsigned char                 uchar;
#define uchar uchar
#endif

#ifndef ushort
typedef unsigned short                ushort;
#define ushort ushort
#endif

#ifndef uint
typedef unsigned int                  uint;
#define uint uint
#endif

#ifndef ulong
typedef unsigned long                 ulong;
#define ulong ulong
#endif

#if HAVE_LLONG
#ifndef llong
typedef long long                     llong;
#define llong llong
#endif

#ifndef ullong
typedef unsigned long long            ullong;
#define ullong ullong
#endif
#endif /* HAVE_LLONG */

typedef int                           opcode;

typedef handle *                      HANDLE;


/*
 *  Record (struct) types, including representations of all SETL types.
 */

struct handle_struct {
  block **reference;    /* a ref (ptr) to a block ptr, to track it */
};

/*
 *  The 'block' type (struct block_struct) is the base type for all
 *  blocks that live on the heap.  Its derivatives can be recognized
 *  by the presence of
 *
 *    blocktype type;
 *
 *  at the beginning of the struct.
 *
 *  Each block whose size is variable has an array declared to be of
 *  size FLEX in its final field, followed by a couple of macros
 *  giving the size of the initial "stub" and the element size for
 *  the flexible array.
 */

struct block_struct {   /* generic block */
  blocktype type;       /* real_type, boolean_type, set_type, etc. */
};
#define is_om(p)      ((p) == OM)  /* NULL block pointer */
#define setl_type(p)  (is_om(p) ? om_type : (p)->type)
#define is_type(T,p)  (setl_type(p) == T##_type)
#define TYPENAME(p)   type_name((block *)(p))

struct atom_struct {
  blocktype type;       /* atom_type */
  long atmval;
};
#define is_atom(p)  is_type(atom,p)

struct boolean_struct {
  blocktype type;       /* boolean_type */
  bool booval;
};
#define is_boolean(p)  is_type(boolean,p)

struct small_struct {   /* for finite integers */
  blocktype type;       /* small_type */
  long weeval;
};
#define is_small(p)  is_type(small,p)

struct integer_struct {
  blocktype type;       /* integer_type */
  size_t     room;  /* #limbs in the 'limbs' field (allocated size) */
  ssize_t    size;  /* |size| is #limbs in use; if < 0, number is -ve */
  mp_limb_t  limbs[FLEX];  /* big digits */
};
#define integer_stubsize  offsetof(integer, limbs)
#define integer_elmtsize  elmtsize(integer, limbs)  /* per limb */
#define is_integer(p)     is_type(integer,p)

/*
 *  This should come out to be the same as integer_elmtsize (or as
 *  sizeof (p)->limbs[0] for any INTEGER p), but can have its numeric
 *  value evaluated at preprocessor time.
 */
#define SIZEOF_LIMB  (GMP_LIMB_BITS / CHAR_BIT)

/*
 *  Number of bytes occupied by the limbs of an INTEGER p.
 */
#define sizeof_limbs(p)  (ABS((p)->size) * SIZEOF_LIMB)

/*
 *  Code for a common case where you want to declare and initialize a
 *  GMP mpz_t z for read-only purposes from an INTEGER i.  Since only
 *  a pointer to i's limbs is copied into z, z effectively becomes an
 *  alias for i during its lifetime (not counting the "size" fields of
 *  either).
 *
 *  The z that is actually declared here is of type mpz_srcptr, which
 *  is perfect for passing to mpz_* functions that are documented as
 *  taking a const mpz_t arg.
 *
 *  In the special case that i is 0 (and thus has size 0), the returned
 *  ptr is to a static const mpz_t zero that has 1 limb allocated, as
 *  GMP reserves the right to refer to that first limb unconditionally
 *  (see the GMP doc Internals section).
 */
#define ro_mpz(z, i)                                          \
  mpz_t z##_buf;                                              \
  mpz_srcptr z = alias_mpz (z##_buf, (i)->limbs, (i)->size);

struct real_struct {
  blocktype type;       /* real_type */
  double realval;
};
#define is_real(p)  is_type(real,p)

#define is_numeric(p)  (is_integer(p) || is_real(p))

struct set_struct {
  blocktype type;       /* set_type */
  settype stype;        /* plain_set, mmap, or smap */
  long card;            /* size of set (!= patsiz(tblval) for mmaps) */
  table *tblval;
};
#define is_set(p)  is_type(set,p)

/*
 *  As a convenience for low-level debugging (e.g. via gdb),
 *  SETL strings are actually maintained with an extra NUL ('\0')
 *  character at the end internally.  This is done even though
 *  SETL strings can themselves contain NULs.  The addition of
 *  NUL does not count in 'nchar', but means that 'nafter' will
 *  always be at least 1:
 */
struct string_struct {
  blocktype type;       /* string_type */
  long nchar;           /* #chars in string */
  long nbefore,nafter;  /* chars before & after string proper */
  char strval[FLEX];    /* the chars themselves */
};
#define string_stubsize  offsetof(string, strval)
#define string_elmtsize  elmtsize(string, strval)  /* per char */
#define is_string(p)     is_type(string,p)

/*
 *  Tuples should only have trailing holes included in the 'nelt'
 *  count when they are in a transient "open" state of being built up
 *  (such as the impl of a tuple former might do, for example).
 *
 *  Only "closed" tuples are ever presented at the SETL level.
 *
 *  That state isn't explicitly tracked in this struct, but
 *  tup_truncate() makes sure a tuple is closed, typically after a
 *  sequence of other tup_*() operations (see sys.c).
 */
struct tuple_struct {
  blocktype type;       /* tuple_type */
  long nelt;            /* "size" (number of elts) */
  long npre,nsuf;       /* room before and after elts proper */
  block *elts[FLEX];    /* tuple components */
};
#define tuple_stubsize  offsetof(tuple, elts)
#define tuple_elmtsize  elmtsize(tuple, elts)  /* per block ptr */
#define is_tuple(p)     is_type(tuple,p)

/*
 *  Subroutine reference, as a value:
 */
struct routine_struct {
  blocktype type;       /* routine_type */
  long proc_pc;         /* code tuple index of op_proc or op_vproc */
};
#define is_routine(p)  is_type(routine,p)

/*
 *  There's no particular reason for lumping "counters" in with
 *  iterators, except for the conceptual link:
 */
struct iterator_struct {
  blocktype type;       /* iterator_type */
  itertype itype;       /* just for checking proper iterator use */
  long itstate;         /* index of last element produced */
  block *constant;      /* the thing being iterated through */
  long increment;       /* for itype = counter */
  long limit;           /* for itype = counter */
};
#define is_iterator(p)  is_type(iterator,p)

/*
 *  There is no distinct table_struct.  The 'table' typedef and the
 *  'table_type' blocktype go with a 'subnode_struct'; see below.
 */

struct source_struct {
  blocktype type;       /* source_type */
  string *filename;     /* where the presumed SETL source came from */
  string *srctext;      /* the text of the source */
};
#define is_source(p)  is_type(source,p)

struct codeline_struct {
  blocktype type;       /* codeline_type */
  long srcnum;          /* index into tuple of sources */
  long offset;          /* offset in original file */
  string *text;         /* text of line */
};
#define is_codeline(p)  is_type(codeline,p)

typedef struct {
  long level;           /* 'display' index (lexical level) */
  long slot;            /* frame element index */
} data_operand;

typedef struct {
  long parmnum;         /* parameter index */
  long slot;            /* local frame element index */
} parm_operand;

union operand_union {   /* things that can be an operand */
  long longval;         /* number that fits in a long */
  int intval;           /* number that fits in an int */
  data_operand data;    /* reference to a SETL variable */
  long pc;              /* code tuple index of routine or label */
  long sysrot;          /* system routine number */
  parm_operand parmin;  /* incoming value */
  parm_operand parmout; /* outgoing value */
};

struct instr_struct {   /* general layout of an instruction */
  blocktype type;       /* instr_type */
  opcode op;            /* operation code, e.g., op_assert */
  long nopnd;           /* number of operands in instruction */
  long vindex;          /* corresponding line of textual VM code */
  operand opnds[FLEX];  /* the operands themselves */
};
#define instr_stubsize  offsetof(instr, opnds)
#define instr_elmtsize  elmtsize(instr, opnds)  /* per operand */
#define is_instr(p)     is_type(instr,p)

/* Instruction element (operand) */
#define instrelt(instr_ptr,i) ((instr_ptr)->opnds[(i)-1])

/* For op_call etc., this overlays (refines) the instr structure:  */
struct call_struct {    /* layout of the "call" instructions */
  blocktype type;       /* instr_type */
  opcode op;            /* op_call, op_call_assigning, etc. */
  long nopnd;           /* as in instr_struct */
  long vindex;          /* corresponding line of textual VM code */
  operand callee;       /* whom ya gonna call (pc) */
  operand args[FLEX];   /* actual args and RET (function result) arg */
};
#define call_stubsize  offsetof(call, args)
#define call_elmtsize  elmtsize(call, args)  /* per operand */

/* Call instruction element (actual arg) */
#define callelt(call_ptr,i) ((call_ptr)->args[(i)-1])

/* Refinement (subclass) of instr for op_mainproc, op_proc, op_vproc:  */
/*
 *  The "proc" instructions are the most complex.  They are derived
 *  from the textual form, which uses prefixes <, >, or <> on formal
 *  parameters, corresponding to RD, WR, and RW at the SETL level.
 *
 *  The flexible array 'bow' that forms the tail of this struct is
 *  actually 3 flexible arrays in concatenation, referred to as
 *  parmins, parmouts, and backtracks even though the struct has
 *  no such fieldnames.  The indices i_in, i_out, and i_back tell
 *  where in 'bow' each of those arrays begin.
 *
 *  Each parmin and parmout is an operand that gives a pair of numbers:
 *  the parmnum (position in the list of formals), and
 *  the slot number (position of the local in the activation frame).
 *
 *  The parmins say which args to copy in to which formals on a call
 *  to this proc, and the parmouts say which formals to copy out to
 *  which args upon return.  A RW formal will thus appear in both the
 *  parmins and parmouts.
 */
struct proc_struct {    /* layout of the "proc" instructions */
  blocktype type;       /* instr_type */
  opcode op;            /* op_mainproc, op_proc, or op_vproc */
  long nopnd;           /* as in instr_struct */
  long vindex;          /* corresponding line of textual VM code */
  operand procnum;      /* routine serial number */
  operand level;        /* routine lexical level ('display' index) */
  operand nloc;         /* number of local variables */
  operand nformal;      /* number of formal parameters */
  operand i_in;         /* index into bow for parmins (always 0) */
  operand i_out;        /* index into bow for parmouts */
  operand i_back;       /* index into bow for backtracks */
  operand bow[FLEX];    /* parmins, parmouts, backtracks */
};     /* block of words, he pressed */
#define proc_stubsize  offsetof(proc, bow)
#define proc_elmtsize  elmtsize(proc, bow)  /* per operand */
#define proc_n_fixed_opnds  \
  ((proc_stubsize - instr_stubsize) / sizeof (operand))
#define proc_fixed_opnd_index(name)  \
  (1 + (offsetof(proc, name) - offsetof(proc, procnum)) / sizeof (operand))

/* sop the operand union a little... */
#define proc_procnum(p) ((p)->procnum.longval)
#define proc_level(p)   ((p)->level.longval)
#define proc_nloc(p)    ((p)->nloc.longval)
#define proc_nformal(p) ((p)->nformal.longval)
#define proc_i_in(p)    ((p)->i_in.longval)
#define proc_i_out(p)   ((p)->i_out.longval)
#define proc_i_back(p)  ((p)->i_back.longval)

/*
 *  Descriptor relating formal parameter index to local stack frame
 *  index, transiently used by assemble.c for the members of the
 *  'parmins' and 'parmouts' tuples:
 */
struct parm_struct {    /* descriptor that goes in parmins/parmouts */
  blocktype type;       /* parm_type */
  long parmnum;         /* parameter index */
  long slot;            /* index of variable in the local frame */
};
#define is_parm(p)  is_type(parm,p)

struct frame_struct {   /* run-time activation record */
  blocktype type;       /* frame_type */
  long nloc;            /* number of locals ('locs' field below) */
  long proc_pc;         /* pc of proc or vproc instr */
  long caller_pc;       /* caller's pc */
  long caller_level;    /* caller's display index (lexical scope) */
  long nargs;           /* number of actual args (NARGS) */
  frame *link;          /* link to previous frame at our level */
  iterator *combiter;   /* local iterator for "combining" invocations */
  block *locs[FLEX];    /* the "variables" in this frame */
};
#define frame_stubsize  offsetof(frame, locs)
#define frame_elmtsize  elmtsize(frame, locs)  /* per "variable" */
#define is_frame(p)     is_type(frame,p)

/* Stack frame element ("slot") */
#define framelt(frame_ptr,i) ((frame_ptr)->locs[(i)-1])

struct symtab_struct {  /* run-time symbol table for debugging */
  blocktype type;       /* symtab_type */
  tuple *procnames;     /* tuple of routine names */
  table *procnums;      /* map from routine name to 'procnames' index */
  table *procaddrs;     /* map from routine name to code index (pc) */
  table *globals;       /* top-level map from variable name to slot */
  table *locals;        /* per-routine variable name-to-slot maps */
  table *labels;        /* per-routine label name-to-slot maps */
  table *gbl_backtrax;  /* obsolescent */
  table *lcl_backtrax;  /* obsolescent */
  tuple *vcode;         /* SETL VM text lines as passed to assemble() */
  tuple *sources;       /* tuple of input sources */
};
#define is_symtab(p)  is_type(symtab,p)

/*
 *  SETL virtual machine "instance":
 */
struct machine_struct { /* code and state of a SETL virtual machine */
  blocktype type;       /* machine_type */
  long pc;              /* current instruction index ("pgm ctr") */
  long level;           /* current lexical scope level in 'display' */
  int exit_status;      /* status we will exit with (set by STOP) */
  int raw_status;       /* last synchronously received child status */
  /*
   *  This mnemonic for -1 is more portable than it might first appear;
   *  the POSIX system() function we no longer use returns -1 only
   *  when no child status can be obtained, so we reuse that for cases
   *  where raw_status should reflect "undefined" or "error".
   *
   *  Most case analysis on raw_status should check first for no_status:
   */
  #define no_status (-1) /* meaning STATUS = OM */
  bool eof;             /* end-of-file flag */
  bool magic;           /* regexp magic in effect */
  bool intslash;        /* if true, make INTEGER/INTEGER -> INTEGER */
                        /* (the wiser default mode produces REAL) */
  tuple *code;          /* "executable" code */
  tuple *backs;         /* which globals are backtrackable */
  symtab *sym;          /* everything needed for debugging */
  tuple *ready_maps;    /* 3 fd->stream maps for SELECT to dole out */
  /*
   *  Note that the following 'display' should be a trailing "flexible"
   *  array (like that in the tuple_struct, or the frame_struct) for
   *  full support of lexical routine nesting.  The calling convention
   *  for SETL routines that is implemented in 'execute' already has
   *  that generality, because I like having it even though it carries
   *  some extra overhead.
   *
   *  Forming a closure (inner routine reference, lambda expression)
   *  would include saving a slice of the display.  Routining a
   *  continuation would include saving the whole display.  (That
   *  sequestering of list-head frame pointers would preserve the
   *  frames from destruction by the garbage collector.)  Calling a
   *  closure would involve pushing the saved frame pointers into the
   *  display.  Calling a continuation would involve replacing the
   *  contents of the display with the saved heads.
   */
  frame *display[DISPLAY_LEVELS];  /* see almost any compilers book */
};
#define is_machine(p)  is_type(machine,p)

/*
 *  The struct key_struct, of type key_type, is defined in patrie.h
 *  (which is #included below).
 */

/*
 *  The struct subnode_struct, of type table_type or subnode_type,
 *  is defined in patricia.h (which is #included by patrie.h).
 */

/*
 *  N.B.  Because TYPE is defined to be 'blocktype' here, those
 *  records which have a "TYPE type;" field also define blocks that
 *  reside on the SETL heap.  See "patrie.h" and "patricia.h";
 *  currently this includes the key_struct and subnode_struct just
 *  mentioned.
 */

/* Interface to Convenient Patricia */
#define DATA            block *
#define TYPE            blocktype
#define PATRIE_TYPE     table_type
#define SUBNODE_TYPE    subnode_type
#define KEY_TYPE        key_type
/*
 *  The preprocessor symbol MALLOC is defined just below as
 *  alloc_mem, the wonderful SETL heap allocator.  This establishes
 *  MALLOC for the subsequent inclusion of "patrie.h", which would
 *  otherwise #define it as malloc.
 *
 *  For those few places in this interpreter where the OS-level
 *  allocator is called, however, the wrapper os_malloc defined
 *  in "os.c" is supposed to be used instead.
 */
#define MALLOC(n)   alloc_mem(n)
#define FREE(p)     ((void)(p))  /* automatic by garbage collection */
#define USE_HANDLES             /* make "patrie.c" use our HANDLEs */
#define COPY_KEY(k) copy_key(k)
#define COPY_DATA(d) copy_value(d)
#define EQUAL_KEY(k1,k2) \
               (((k1)==NULL && (k2)==NULL) || equal_key(k1,k2))
#define EQUAL_DATA(d1,d2) \
               (((d1)==NULL && (d2)==NULL) || equal_value(d1,d2))
/*
 *  We don't mess with KEY or SUBNODE, so they should get their
 *  default definitions as struct key_struct and struct subnode_struct
 *  from "patrie.h", which #includes "patricia.h":
 */
#include "patrie.h"
#define key_stubsize  offsetof(key, bstring)  /* equals KEY_STUBSIZE */
#define key_elmtsize  elmtsize(key, bstring)  /* per byte */
#define is_key(p)     is_type(key,p)
#define is_table(p)   is_type(table,p)
#define is_subnode(p) is_type(subnode,p)
#define tokey(p)      block_to_key((block *)(p))

/*
 *  Support for the --allow-* options.
 *
 *  This is a "fat" struct representing a thing that is allowed even
 *  in --restricted mode.
 */
struct allowed_struct {
  char *what;     /* NULL; or filename, command, host:service, etc. */
  int fd;         /* file descriptor, for --allow-fd-open options */
  open_how how;   /* OPEN mode, for --allow-*open options */
  allowed *next;  /* list ptr */
};

/*
 *  Sort of like a stripped down stdio FILE, but with a distinct input
 *  and output buffer internally.
 *
 *  SYS_BUF_SIZ is the size of each internal buffer.
 */
#define SYS_BUF_SIZ 8192
struct io_buffer_struct {
  int fd;               /* file descriptor */
  buffering policy;     /* line_buffering, full_buffering, etc. */
  int j_in, n_in, j_out, n_out;  /* buffer indices */
  bool eof;             /* end-of-file indicator for SETL level (EOF) */
  bool eof_pending;     /* end reached on underlying byte stream */
  bool seekable;        /* direct-access file */
  bool appending;       /* auto-seeks to end on every write */
  int pending_errno;    /* meaningful when eof_pending */
  off_t file_pos;       /* should match OS file pos for seekables */
  double dummy;         /* encourage good buffer alignment */
  /*
   *  Since output is flushed whenever input is initiated, and input
   *  is drained whenever output is initiated, these two buffers
   *  could be merged into one.  That flushing and draining was
   *  not done for all bidirectional streams in the past, however,
   *  and it is conceivable that a modal switch or some such horror
   *  could be provided in the future to suppress it.  So there should
   *  be no rush to do the rather trivial optimization of merging the
   *  buffers, even though providing the modal switch seems an unlikely
   *  feature to offer, given that independent input and output on a
   *  given channel can already be performed at the SETL level in a
   *  couple of different ways even in the presence of the automatic
   *  flushing and draining.  You might want that kind of full-duplex
   *  operation for high-performance bulk transfer.
   *
   *  One way to overlap input and output like that is not to use
   *  the buffering, but to use SYS_READ and SYS_WRITE to bypass it,
   *  with SELECT telling when more input is available or more output
   *  is possible.  Another is to spawn a reader and a writer process,
   *  an approach that facilitates immunity from unwanted blocking, as
   *  the reader need only wake up the parent (via SELECT) when a whole
   *  chunk of data has been read, and similarly the writer need only
   *  wake up the parent (by sending it a single newline character, for
   *  example) after it has finished writing a whole chunk.
   */
  uchar in[SYS_BUF_SIZ], out[SYS_BUF_SIZ];  /* the buffers */
};

/*
 *  Fetch and remove a byte from io_buffer *iobuf, resorting to
 *  fill_in() if the input buffer is empty.
 *
 *  If fill_in() cannot read at least one byte into the input buffer,
 *  fill_in() and thus get_char() returns EOF and sets
 *  iobuf->eof_pending to true.  As long as the input buffer remains
 *  empty and that eof_pending flag remains true, further calls to
 *  get_char() will continue to return EOF without a further input
 *  attempt by fill_in().
 */
#define get_char(iobuf) \
  ( is_empty(iobuf) ? fill_in(iobuf) : get_char_unchecked(iobuf) )

/*
 *  Given a nonempty input buffer, fetch and remove a byte.
 */
#define get_char_unchecked(iobuf) \
  ( (int)(iobuf)->in[(iobuf)->j_in++] )

/*
 *  N.B. unget_char() is unchecked.  One character of pushback is
 *  guaranteed if and only if get_char() has succeeded at least once
 *  since initialization or the last drain or unget_char().
 */
#define unget_char(c,iobuf) \
  ( (iobuf)->in[--(iobuf)->j_in] = (uchar)(c) )

/*
 *  All bytes in the input buffer have been consumed.
 */
#define is_empty(iobuf) ( (iobuf)->j_in >= (iobuf)->n_in )

/*
 *  Put c in iobuf.  Assume there is enough room for it.
 */
#define put_char_unchecked(c,iobuf) \
  ( (iobuf)->out[(iobuf)->j_out++] = (uchar)(c) )

/*
 *  For line_buffering and byte_buffering streams, n_out is always 0,
 *  so this will always be true.  For full_buffering, n_out is the
 *  size of the output buffer, so it will only be true when the
 *  buffer is full.
 */
#define may_need_flushing(iobuf) \
  ( (iobuf)->j_out >= (iobuf)->n_out )

/*
 *  Put one byte c into iobuf.  If iobuf might then need flushing,
 *  call maybe_flush() to have a closer look and possibly flush.
 */
#define put_char(c,iobuf) ( \
  put_char_unchecked(c,iobuf), \
  may_need_flushing(iobuf) ? maybe_flush(iobuf) /* true or errno set */ \
                           : true /* success without flushing */ \
)


/*
 *  Masks for the 'abilities' field of file_struct.
 */
#define can_ask      (1<<0)
#define can_read     (1<<1)
#define can_write    (1<<2)
#define can_seek     (1<<3)
#define can_accept   (1<<4)
#define can_recv     (1<<5)
#define can_send     (1<<6)
#define can_recvfrom (1<<7)
#define can_sendto   (1<<8)
/*
 *  I shall remain permissive regarding the can_recv_fd and
 *  can_send_fd that should logically appear here; the result I
 *  expect is that if the SETL programmer tries to use the wrong
 *  kind of stream to pass a file descriptor on, a somewhat
 *  lower-level error message than is strictly desirable ensues.
 *
 *  This is to plan ahead for when some of the current restrictions
 *  are removed, at which point fd passing suddenly starts working
 *  on some kinds of channel it didn't use to.
 */
#define can_sel_r    (1<<11)
#define can_sel_w    (1<<12)
#define can_sel_e    (1<<13)
#define can_persist  (1<<14)  /* stay open across SETL pgm executions */
#define can_os_close (1<<15)  /* no longer used, now reserved */
#define can_shutdown (1<<16)
#define can_append   (1<<17)  /* means O_APPEND is on the fd */
#define can_rw  (can_read | can_write)  /* rw mode, sockets, etc. */
#define can_rs  (can_read | can_seek)   /* r mode */
#define can_ws  (can_write | can_seek)  /* w,n modes */
#define can_wa  (can_write | can_append)  /* a mode */
#define can_rws (can_rw | can_seek)     /* r+,w+,n+ modes */
#define can_rwsa (can_rws | can_append)  /* a+ mode */
#define duplex  (can_rw | can_shutdown)  /* bidirectional but not seekable */

struct open_mode_struct {
  open_how how;         /* the desired way of OPENing a thing */
  filetype ftype;       /* as in file_struct, see below */
  unsigned abilities;   /* as in file_struct, see below */
  int oflag;            /* open(2)-style I/O mode (see list below) */
};
/* Some possible values of 'oflag' in struct open_mode:  */
#define R_MODE (O_RDONLY)                        /* r: read-only */
#define W_MODE (O_WRONLY | O_CREAT | O_TRUNC)    /* w: write, empty first */
#define N_MODE (O_WRONLY | O_CREAT | O_EXCL)     /* n: write new file */
#define A_MODE (O_WRONLY | O_CREAT | O_APPEND)   /* a: write at end */
#define RP_MODE (O_RDWR)                         /* r+: r/w existing file */
#define WP_MODE (O_RDWR | O_CREAT | O_TRUNC)     /* w+: r/w, empty first */
#define NP_MODE (O_RDWR | O_CREAT | O_EXCL)      /* n+: r/w new file */
#define AP_MODE (O_RDWR | O_CREAT | O_APPEND)    /* a+: seek/r; w at end */
#define RW_MODE (O_RDWR)                         /* rw: sequential r/w */

/*
 *  This is rather a "fat" record; each kind of file only uses a
 *  subset of the fields in it.
 */
struct file_struct {    /* per-file block (not on the SETL heap) */
  filetype ftype;       /* specific kind of "file" */
  unsigned abilities;   /* some "or" of can_read, can_write, etc. */
  char *filename;       /* name of file, command, etc., or NULL */
  char *nodename;       /* host part of [host,serv], or NULL */
  char *servname;       /* serv part of [host,serv], or NULL */
  io_buffer *buffer;    /* input and output buffer, or NULL */
  int fd;               /* our file descriptor, should == buffer->fd */
  int tie;              /* output fd to be flushed on our input ops */
  pid_t pid;            /* process id if this file is a pipe or pump */
  pid_t ppid;           /* process id of creator (and hence waiter) */
  struct timespec initial;  /* delay before first expiry of timer */
  struct timespec interval;  /* recurring timer expiry period */
  volatile struct timespec timestamp;  /* when evcount last bumped */
  volatile long evcount;  /* #events not yet read by client */
  file *next;           /* link in sig_info client list, or NULL */
  int sig;              /* e.g., SIGHUP, SIGTERM, SIGCHLD, ... */
  bool auto_close;      /* auto-closing upon eof is enabled */
  bool auto_close_output;  /* auto-closing after output is enabled */
  bool local_child;     /* created by PUMP or TTY_PUMP */
};

/*
 *  Per-signal record (not on the SETL heap).
 *
 *  Signal dispositions are altered by SETL programs ("clients")
 *  OPENing streams of 'signal', 'ignore', or 'default' mode.  SIGALRM
 *  is used to support mode 'real-ms', and cannot be opened directly
 *  in 'signal' mode, though it can in 'ignore' and 'default' mode.
 *
 *  The clients are chained together on lists whose heads are in
 *  the sig_info struct, as multiple clients of every mode for every
 *  valid signal are permitted.
 *
 *  A 'signal' or 'real-ms' stream always takes precedence over an
 *  'ignore' stream, and 'ignore' always beats 'default', for a given
 *  signal, in the sense that if both streams are open on that signal,
 *  the signal disposition follows the winner.
 *
 *  A stream that is only open for 'default' handling cannot be read
 *  or written, but causes the disposition to be SIG_DFL.  This may
 *  or may not be the same as what was inherited by the process,
 *  though POSIX specifies some standard defaults (see sig_num() in
 *  sys.c).
 *
 *  A stream that is open for 'ignore' handling (but nothing stronger)
 *  causes that signal's disposition to be SIG_IGN.  Like a 'default'
 *  stream, it cannot be read or written; the only purpose of the
 *  stream is to govern the signal disposition.
 *
 *  Finally, if there are any streams open in 'signal' or 'real-ms'
 *  mode for the signal, those streams are all readable and will
 *  deliver a newline for each event (received signal in the case
 *  of a 'signal' stream, and timer expiry in the case of 'real-ms').
 *
 *  Closing any of the foregoing can also cause a change in signal
 *  disposition if it is the last stream of that mode for the given
 *  signal.  When the last of all streams for that signal is closed,
 *  the signal action as inherited by the process is restored
 *  (except in the case of SIGCHLD, which is always caught by the
 *  SETL run-time in a way that allows it to be treated by the SETL
 *  program just like other signals, but isn't actually restored to
 *  the inherited action until the SETL program is about to exit).
 */
struct sig_info_struct {
  file *readers;         /* list of clients reading this signal */
  file *ignorers;        /* list of clients studiously ignoring it */
  file *defaulters;      /* list of clients studiously defaulting it */
#if USE_POSIX_RT
  timer_t timerid;       /* from os_timer_create() */
#endif
  struct sigaction old_action;  /* saved action, e.g. by os_signal() */
};

/*
 *  List element for list of process ids.  Not on the SETL heap.
 */
struct pid_list_struct {
  pid_t    pid;     /* pid of child process */
  pid_list *next;   /* next list element */
};


/*
 *  Macro-utilities.
 */

#ifndef USE_SYSTEM_ASSERT
#undef assert
#ifdef NDEBUG
#define assert(expr) \
  ((void)0)
#else
#define assert(expr) \
  ((void)((expr) || \
   (internal_assert_failure(#expr, FILEBASE, __LINE__), 0)))
#endif
#endif

/*
 *  Use this 'check' macro as you would 'assert', but when you want
 *  to make sure the side-effects of the expression argument get
 *  executed even if NDEBUG is turned on by some daring programmer:
 */
#ifdef NDEBUG
#define check(expr) \
  ((void)(expr))
#else
#define check(expr) \
  ((void)((expr) || \
   (internal_check_failure(#expr, FILEBASE, __LINE__), 0)))
#endif

/*
 *  All assertions are supposed to be tautologies, but some are so
 *  extremely unlikely to be violated that the only reason for not
 *  deleting them entirely (as opposed to the blanket approach of
 *  turning on NDEBUG) is that they have some documentary value.
 *
 *  So here is yet another alternative to 'assert', recommended for
 *  use when (1) the code is likely to be hit often enough that you
 *  don't want to incur even the relatively trivial expense of a
 *  real 'assert', (2) you are convinced that the code is correct
 *  AND well-tested already, and (3) you want to document local
 *  expectations.
 *
 *  Note that if you ever have some suspicion that you have been a
 *  tiny bit over-eager in changing an 'assert' to a 'taut', you
 *  can turn on the ASSERT_TAUTOLOGIES preprocessor symbol (by
 *  sticking a #define in this code or with a "-D..." option) to
 *  make 'taut' act like 'assert':
 */
/* #undef ASSERT_TAUTOLOGIES */  /* #define to 1 to assert tautologies */
#ifdef ASSERT_TAUTOLOGIES
#define taut(expr) assert(expr)
#else
#define taut(expr) ((void)0)
#endif

/*
 *  This 'unexpected' macro is for use in 'switch' statements where
 *  the case analysis is unexpectedly incomplete.  Actually, it can
 *  be used anywhere you are dealing with an int or enum, and want
 *  the value included in the error message.  Don't use this for
 *  bad user-generated input, however, since it reports an "internal
 *  error":
 */
#define unexpected(expr) \
  internal_case_failure(#expr, expr, FILEBASE, __LINE__)

/*
 *  Same idea, but for C character strings:
 */
#define unexpected_str(expr) \
  unexpected_string_value(#expr, expr, FILEBASE, __LINE__)

/*
 *  Number of bytes in fixed-length block:
 */
#define fbytes(type)  (sizeof (type))

/*
 *  Allocate a fixed-length block on the SETL heap:
 */
#define fblock(type) \
  ((type *) alloc_block (type##_type, fbytes(type)))

/*
 *  Number of bytes in variable-length block:
 */
#define vbytes(type,n)  (type##_stubsize + (n)*type##_elmtsize)

/*
 *  Allocate a variable-length block on the SETL heap:
 */
#define vblock(type,n) \
  ((type *) alloc_block (type##_type, vbytes(type,n)))

/*
 *  The following macros are access methods for elements of tuples
 *  and strings.
 *
 *  The macros assume that the first field is numbered 1, not 0, so
 *  they subtract 1 from the SETL index you supply to get the C index.
 *
 *  They can all be used on the left hand sides of assignments,
 *  and can have the "&" (address of) operator applied, so you can
 *  use them in transient references to subtuples, substrings, etc.,
 *  though of course you have to respect the limited lifetime of
 *  pointers you create in this way (per the diatribe at the
 *  beginning of this file):
 */

/*
 *  Tuple element reference:
 */
#ifdef NDEBUG
#define tupelt(t,i) \
  ((t)->elts[(t)->npre + ((i)-1)])
#else
/* The normal, subscript-checking version:  */
#define tupelt(t,i) \
  ((t)->elts[(t)->npre + \
    (1 <= (i) && (i) <= (t)->nelt ? (i)-1 : \
         tupelt_error(i,(t)->nelt,FILEBASE,__LINE__), (i)-1)])
#endif

/*
 *  String element (character) reference:
 */
#ifdef NDEBUG
#define strelt(s,i) \
  ((s)->strval[(s)->nbefore + ((i)-1)])
#else
/* The normal, subscript-checking version (one extra char is allowed
   for the nul automatically maintained after the user string);
   s->nchar is assumed to be of a long (at least) signed type:  */
#define strelt(s,i) \
  ((s)->strval[(s)->nbefore + \
       (1 <= (i) && (long)(i) <= (s)->nchar+1 ? (i)-1 : \
              strelt_error(i,(s)->nchar+1,FILEBASE,__LINE__), (i)-1)])
#endif
/* This is equivalent to strelt(s,(s)->nchar) but avoids the GCC
 * warning "assuming signed overflow does not occur when assuming
 * that (X + c) < X is always false" that strelt would elicit for
 * the comparison (s)->nchar <= (s)->nchar+1:  */
#define last_char(s) \
  ((s)->strval[(s)->nbefore + \
       (1 <= (s)->nchar ? (s)->nchar-1 : \
            bugerr("No last char in empty string at %s:%d", \
                                   FILEBASE,__LINE__), (s)->nchar-1)])

/* Some trivial conversions of minor documentary value */
#define nbits_to_nbytes(n) ((size_t)(n) / (unsigned)CHAR_BIT)
#define nbytes_to_nbits(n) ((size_t)(n) * (unsigned)CHAR_BIT)

/*
 *  Mixed-mode arithmetic is done by converting both arguments to the
 *  local C 'double' before operating.
 */
#define real_to_double(r) ((r)->realval)
/*      integer_to_double() is defined in sys.c */
#define integer_to_real(i) new_real(integer_to_double(i))

/* Obtain HANDLE for any type of block; note the ampersand. */
/* The intermediate (void *) cast is probably to avoid a gcc warning
 * about type-punned pointers; note that (block *) is effectively a
 * base for every type of pointer you are allowed to pass as b:  */
#define ref(b) (grab_handle((block **)(void *)&(b)))

/* Release HANDLE */
#define retire(h) (release_handle(h))

/*
 *  The following 'let' macro helps solve a problem with assignments
 *  involving block pointers (pointers to blocks on the SETL heap).
 *  It should be used in place of plain assignment whenever the lhs is
 *  an lvalue expression more complex than a simple variable reference,
 *  and the rhs could cause heap allocation.  For example,
 *
 *    tupelt(p,i) = hairy_rhs()
 *
 *  should be replaced by the macro call
 *
 *    let (tupelt(p,i), hairy_rhs())
 *
 *  because C is free to evaluate arguments of any binary operator,
 *  including "=", in either order.  Hence in the plain assignment
 *  statement above, it is possible for the lvalue produced by
 *  tupelt(p,i) to be invalid by the time hairy_rhs() has been
 *  evaluated, due to garbage collection in the rhs.  This is
 *  analogous to the indeterminate (but much simpler) example
 *
 *    array[i] = array[++i]
 *
 *  in C, where in fact the GNU C compiler may assign to a different
 *  array element depending on the level of optimization.
 *
 *  To make sure the rhs gets evaluated before the lhs, the 'let'
 *  macro puts the rhs result into a local temporary, and then
 *  executes an assignment of the form "lhs = local_temporary".
 *
 *  You, dear compiler writer, are still on your honour to make sure
 *  evaluation of the lhs cannot cause allocation.  Fortunately, you
 *  probably do that normally anyway; 'tupelt' calls and other block
 *  subclass field references are typical lhs expressions.
 *
 *  When the lhs is of the form *p such as for a (block **) p arg,
 *  a common alternative is to initialize a (block *) t = *p, use t
 *  throughout the scope (putting a HANDLE on t if need be), and
 *  then assign *p = t at the end, without using 'let'.
 */
#if HAVE_TYPEOF
/* Good, we don't have to lose the static type checking just because
 * 'let' is a macro.  */
#define let(lhs,rhs) do { \
  typeof(rhs) temp = rhs; \
  lhs = temp; \
} while (0)
#else
/* Bummer.  Also, this "no typeof" version only works for pointers. */
#define let(lhs,rhs) do { \
  void *temp = rhs; \
  *(void **)&(lhs) = temp; \
} while (0)
#endif

/*
 *  When a block ptr is to be copied and the source of it is no
 *  longer needed, this small but trivial macro can be used to
 *  remind readers of the roughly C++ rvalue reference semantics
 *  of the "move" of the ptr (from rhs to lhs) that it does.
 *
 *  Note that here in C, both lhs and rhs must be lvalues.
 */
#define move_blockptr(lhs,rhs) do {\
  /* we deliberately do _not_ use let() here */\
  (lhs) = (block *)(rhs);  /* transient alias */\
  (rhs) = NULL;  /* ptr ownership transferred */\
} while (0)


/*
 *  An alternative to strndup() that uses os_malloc() to get the space.
 */
#define strndup_malloc(s, n) \
  (strncpy_plus_nul ((char *)os_malloc((n)+1), s, n))

/*
 *  An alternative to strdup() that uses os_malloc() to get the space,
 *  and returns a NULL pointer for NULL input rather than freaking out.
 */
#define strdup_malloc(s) \
  ((s) ? strcpy ((char *)os_malloc(strlen(s)+1), s) : NULL)

/*
 *  r := s if it fits, otherwise r := a prefix of s followed by "...";
 *  unprintable chars are turned into '?' (question mark); r should be
 *  declared as a character array so that sizeof r gives its size;
 *  &r[0] is returned by finite_strcpy(), just as strcpy(3) does.
 */
#define finite_strcpy(r, s) \
  (do_finite_strcpy(r, s, sizeof (r)))

/*
 *  Size of tame()'s static buffer.
 */
#define TAME_SIZE  55

/*
 *  Never trust a C library function with a "length 0" argument.
 */
/* (That's probably no longer a concern, but you never know.) */
#define mvmem(dst, src, n) \
  ((n) > 0 ? memcpy(dst, src, n) : dst)

#if !HAVE_MEMMOVE
#error Need POSIX memmove().
#endif

/*
 *  A comparison result convention:  -1, 0, and 1 must not be changed,
 *  and additional result cases must be +ve.
 */
#define A_LT_B  (-1)  /* A < B */
#define A_EQ_B    0   /* A = B */
#define A_GT_B    1   /* A > B */
#define A_IS_NAN  2   /* A is NaN and B is unknown */
#define B_IS_NAN  3   /* B is NaN but A is not NaN */


/*
 *  C routine signatures (a.k.a. declarations, a.k.a. prototypes):
 */

/*
 *  Conceptually, wherever you see a SETL type name (such as 'string',
 *  'integer', 'tuple', etc.) with only one star after it in the
 *  signature of a routine (lib.c provides the richest and most
 *  uniform set of examples), this means that the routine is taking
 *  the parameter as input only.  Two stars means it may write or
 *  update the referenced item.  (An exception is the 'promote' and
 *  'demote' routines in sys.c, which have no "logical" effect but can
 *  change the _representation_ of a set dramatically.  Of course, they
 *  can't change your pointer, but they are allowed to interfere with
 *  the _contents_ of the 'set' block.)
 */

/* run.c */

extern int      run(unsigned argc, char *const argv[]);
extern string  *get_line(void);  /* 'getline' is a GNU libc extension */


/* init.c */

extern void     init(void);
extern void     fini(void);


/* go.c */

extern int      go(void);


/* assemble.c */

extern machine *assemble(tuple *vcode, tuple *sources);


/* execute.c */

extern void     execute(void);
extern long     newat(void);
extern bool     get_magic(void);
extern bool     set_magic(bool x);
extern bool     get_intslash(void);
extern bool     set_intslash(bool x);
extern bool     get_eof(void);
extern bool     set_eof(bool x);
extern int      get_raw_status(void);
extern int      set_raw_status(int new_status);


/* errage.c  */
extern void     bugerr(const char *fmt, ...) NOISY_QUITTER;
extern void     cmdlinerr(const char *fmt, ...) NOISY_QUITTER;
extern void     tranerr(const char *fmt, ...) NOISY_QUITTER;
extern void     linkerr(const char *fmt, ...) NOISY_QUITTER;
extern void     runerr(const char *fmt, ...) NOISY_QUITTER;
extern void     panic(const char *fmt, ...) NOISY_QUITTER;
extern void     internal_assert_failure(const char *assertion,
                                        const char *filename,
                                        long line) NO_RETURN;
extern void     internal_check_failure(const char *expr,
                                       const char *filename,
                                       long line) NO_RETURN;
extern void     internal_case_failure(const char *expr,
                                      long value,
                                      const char *filename,
                                      long line) NO_RETURN;
extern void     unexpected_string_value(const char *expr,
                                        const char *value,
                                        const char *filename,
                                        long line) NO_RETURN;
extern void     tupelt_error(long i, long n,
                             const char *filename,
                             long line) NO_RETURN;
extern void     strelt_error(long i, long n,
                             const char *filename,
                             long line) NO_RETURN;


/* spewcode.c */

extern void     spewcode(void);


/* heap.c */

extern void     heap_init(void);
extern void     heap_fini(void);
extern handle  *grab_handle(block **reference);
extern void     release_handle(handle *h);
extern integer *alloc_integer(size_t nlimb);
extern string  *alloc_string(long nchar, long nbefore, long nafter);
extern tuple   *alloc_tuple(long nelt, long npre, long nsuf);
extern block   *alloc_block(blocktype type, size_t size);
extern block   *alloc_mem(size_t size);
extern void     print_heap(void);


/* sys.c */

extern const char *type_name(const block *p);

extern atom    *new_atom(long k);

extern boolean *new_boolean(bool b);
extern bool     get_bool(boolean *b, const char *what);

extern small   *new_small(long i);

extern integer *new_integer(long i);
extern integer *ulong_integer(ulong i);
#if HAVE_LLONG
extern integer *llong_integer(llong i);
extern integer *ullong_integer(ullong i);
#endif
extern integer *double_to_integer(double d);
extern integer *charstr_to_integer(const char *s, int radix);

extern long     get_long_in(integer *i, long lwb, long upb, const char *what);
extern long     get_long(integer *i, const char *what);
extern long     get_nat_long(integer *i, const char *what);
extern long     get_pos_long(integer *i, const char *what);
extern ulong    get_ulong(integer *i, const char *what);
extern int      get_int(integer *i, const char *what);
extern int      get_nat_int(integer *i, const char *what);
extern uint     get_uint(integer *i, const char *what);
extern short    get_short(integer *i, const char *what);
extern ushort   get_ushort(integer *i, const char *what);
#if HAVE_LLONG
extern llong    get_llong_in(integer *i, llong lwb, llong upb, const char *what);
extern llong    get_llong(integer *i, const char *what);
extern ullong   get_ullong(integer *i, const char *what);
#endif
extern off_t    get_off_t(integer *i, const char *what);
extern off_t    get_pos_off_t(integer *i, const char *what);
extern pid_t    get_pid_t(integer *i, const char *what);
extern pid_t    get_nat_pid_t(integer *i, const char *what);
extern uid_t    get_uid_t(integer *i, const char *what);
extern gid_t    get_gid_t(integer *i, const char *what);
extern mode_t   get_mode_t(integer *i, const char *what);

extern double   integer_to_double(integer *a);

extern int      integer_cmp(integer *a, integer *b);

extern integer *integer_add(integer *a, integer *b);
extern void     integer_inc(integer **host, integer *b);
extern integer *integer_sub(integer *a, integer *b);
extern void     integer_dec(integer **host, integer *b);
extern integer *integer_neg(integer *a);
extern integer *integer_mul(integer *a, integer *b);
extern integer *integer_div(integer *a, integer *b);
extern integer *integer_rem(integer *a, integer *b);
extern integer *integer_mod(integer *a, integer *b);
extern integer *integer_pow(integer *a, ulong e);
extern integer *integer_abs(integer *a);
extern integer *integer_random(integer *a);
extern integer *integer_and(integer *a, integer *b);
extern integer *integer_com(integer *a);
extern integer *integer_or(integer *a, integer *b);
extern integer *integer_xor(integer *a, integer *b);

extern integer *integer_timeval_to_ms(struct timeval t);
extern struct timeval integer_ms_to_timeval(integer *i, const char *what);
extern integer *integer_timespec_to_ms(struct timespec t);
extern struct timespec integer_ms_to_timespec(integer *i, const char *what);

extern real    *new_real(double d);
extern double   get_double(real *a, const char *what);

extern string  *null_string(void);
extern string  *new_string(const char *s);
extern string  *new_nstring(const char *s, long n);
extern string  *new_estring(long n);
extern string  *new_cstring(char c);
extern string  *str_lower(string *s);
extern string  *str_upper(string *s);
extern string  *copy_substring(string *s, long i, long j);
extern string  *str_lpad(string *s, long n, int c);
extern string  *str_rpad(string *s, long n, int c);
extern string  *str_dress(string *s);
extern string  *str_undress(string *s,
                             void (*err)(const char *fmt, ...) NOISY_QUITTER);
extern string  *str_join(string *a, string *b);
extern void     str_resize(string **host, long size);
extern void     str_tackon(string **host, char c);
extern void     str_append(string **host, const char *s);
extern void     str_concat(string **host, string *s);
extern void     str_insert(string **host, long i, long j, string *x);
extern void     str_concat_substring(string **host, string *s,
                                                     long i, long j);

extern set     *null_set(void);
extern set     *singleton(block *p);
extern bool     in_set(set *host, block *x);
extern void     set_insert(set **host, block *x);
extern void     set_delete(set **host, block *x);
extern block   *set_from(set **host);
extern bool     promote(set *s, settype strength);
extern void     demote(set *s, settype strength);
extern set     *set_fl(integer *first, integer *last);
extern set     *set_fnl(integer *first, integer *next, integer *last);
extern void     set_extend(set **host, set *b);
extern void     set_reduce(set **host, set *b);
extern set     *set_union(set *a, set *b);
extern set     *set_intersection(set *a, set *b);
extern set     *set_difference(set *a, set *b);

extern tuple   *null_tuple(void);
extern tuple   *new_tuple(long size);
extern tuple   *tup_join(tuple *a, tuple *b);
extern void     tup_resize(tuple **host, long size);
extern void     tup_tackon(tuple **host, block *x);
extern void     tup_concat(tuple **host, tuple *p);
extern void     tup_insert(tuple **host, long i, long j, tuple *x);
extern void     tup_truncate(tuple **host);
extern tuple   *tup_fl(integer *first, integer *last);
extern tuple   *tup_fnl(integer *first, integer *next, integer *last);

extern routine *new_routine(long proc_pc);

extern string  *block_plus_string(block *a, string *s);
extern string  *string_plus_block(string *s, block *b);
extern string  *tostrad(integer *a, int radix);
extern string  *tostr(block *a);
extern block   *getval(io_buffer *buffer, int *stopper, const char *context);
extern block   *unstr(string *a, long *i, int *stopper, const char *context);
extern blocktype denotype(string *a, long *i, int *stopper);
extern block   *val(string *a);

extern iterator *init_counter_fl(integer *first, integer *last);
extern iterator *init_counter_fnl(integer *first, integer *next,
                                                   integer *last);
extern iterator *init_iterator(block *p);
extern iterator *init_itersmap(block *p);
extern iterator *init_itermmap(set *p);
extern bool     step_counter(iterator **it, integer **e);
extern bool     step_iterator(iterator **it, block **e);
extern bool     step_itersmap(iterator **it, block **x, block **f);
extern bool     step_itermmap(iterator **it, block **x, set **f);

extern block   *smap_fetch(block *host, block *sel);
extern set     *mmap_fetch(set *host, block *sel);
extern block   *slice_fetch(block *host, block *sel1, block *sel2);

extern void     smap_insert(block **host, block *sel, block *x);
extern void     mmap_insert(set **host, block *sel, set *x);
extern void     slice_insert(block **host, block *sel1, block *sel2,
                                                          block *x);

extern block   *copy_value    (block    *p);
extern atom    *copy_atom     (atom     *p);
extern boolean *copy_boolean  (boolean  *p);
extern small   *copy_small    (small    *p);
extern integer *copy_integer  (integer  *p);
extern real    *copy_real     (real     *p);
extern set     *copy_set      (set      *p);
extern string  *copy_string   (string   *p);
extern tuple   *copy_tuple    (tuple    *p);
extern routine *copy_routine  (routine  *p);
extern iterator *copy_iterator(iterator *p);
extern table   *copy_table    (table    *p);
extern key     *copy_key      (key      *p);
extern source  *copy_source   (source   *p);
extern codeline *copy_codeline(codeline *p);
extern instr   *copy_instr    (instr    *p);
extern parm    *copy_parm     (parm     *p);
extern frame   *copy_frame    (frame    *p);
extern symtab  *copy_symtab   (symtab   *p);
extern machine *copy_machine  (machine  *p);

extern bool     equal_value   (block    *p, block    *q);
extern bool     equal_atom    (atom     *p, atom     *q);
extern bool     equal_boolean (boolean  *p, boolean  *q);
extern bool     equal_small   (small    *p, small    *q);
extern bool     equal_integer (integer  *p, integer  *q);
extern bool     equal_string  (string   *p, string   *q);
extern bool     equal_real    (real     *p, real     *q);
extern bool     equal_set     (set      *p, set      *q);
extern bool     equal_tuple   (tuple    *p, tuple    *q);
extern bool     equal_routine (routine  *p, routine  *q);
extern bool     equal_table   (table    *p, table    *q);
extern bool     equal_key     (key      *p, key      *q);
extern bool     equal_source  (source   *p, source   *q);

extern int      compare_values (block *a, block *b, const char *opsym);

extern key     *block_to_key(block *p);  /* usu. called via 'tokey' macro */
extern block   *unkey(key *k);

extern set     *ip_addresses(string *host);
extern set     *ip_names(string *host);
extern string  *sockaddr_name(const struct sockaddr *sa, socklen_t salen);
extern string  *sockaddr_address(const struct sockaddr *sa, socklen_t salen);
extern integer *sockaddr_portnum(const struct sockaddr *sa, socklen_t salen);
extern tuple   *sockaddr_tuple(const struct sockaddr *sa, socklen_t salen);
extern string  *sockaddr_pathname(const struct sockaddr *sa, socklen_t salen);

extern void     sig_handler(int sig);
extern void     alarm_handler(int sig);

extern pid_t    do_waitpid(pid_t p, int waitflags);

extern tuple   *sys_select(tuple *a, tuple *t);
extern tuple   *do_select(tuple *a, tuple *t);

extern io_buffer *new_io_buffer(int fd, unsigned abilities, buffering policy);
extern void     del_io_buffer(io_buffer *b);
extern bool     flush_io_buffer(io_buffer *b);
extern void     drain_io_buffer(io_buffer *b);
extern off_t    seek_io_buffer(io_buffer *b, off_t offset, int whence);
extern int      fill_in(io_buffer *b);
extern bool     maybe_flush(io_buffer *b);
extern void     clear_eof(io_buffer *b);
extern void     raise_eof(io_buffer *b);
extern bool     at_eof(void);

extern file    *new_file(int fd, filetype ftype, unsigned abilities,
                                                   buffering policy);
extern void     init_file(file *f, filetype ftype, unsigned abilities);
extern file    *find_file_unchecked(int fd);
extern file    *find_file(int fd);     /* for 0 <= fd < n_files */
extern file    *find_fd_file(int fd);  /* for fd_lo <= fd < fd_hi */
extern file    *find_sd_file(int fd);  /* for sd_lo <= fd < sd_hi */
extern void     del_file(file *f);
extern void     flush_file(file *f);
extern void     drain_file(file *f);
extern off_t    seek_file(file *f, off_t offset, int whence);
extern void     eof_file(file *f);
extern const open_mode *lookup_open_mode(const char *mode_name);
extern int      file_open(block *a, open_how how, bool check_please);
extern int      file_mkstemp(string **a);
extern int      file_accept(int fd);
extern void     file_close(int fd, close_how how);
extern void     file_shutdown(int fd, int how);
extern void     file_flush(int fd);
extern void     file_drain(int fd);
extern off_t    file_seek(int fd, off_t offset, int whence);
extern off_t    file_pos(int fd);
extern void     file_trunc(int fd, off_t length);
extern void     file_tie(int fd, int gd);
extern void     file_untie(int fd);
extern int      getfd(block *a, getfd_op intent);
extern void     stream_error(const char *msg, block *a) NO_RETURN;
extern void     insert_name_to_fd(int fd);
extern void     delete_name_to_fd(int fd);
extern block   *make_name(int fd);
extern void     flush_tie(int fd);
extern void     flush_all(void);
extern void     file_rites(void);
extern file    *check_not_open(int fd);

extern pid_t    do_fork(void);
extern int      do_system(const char *cmd);
extern string  *do_filter(string *cmd, string *input, int *statusp);
extern int      local_pump(unsigned abilities);
extern int      local_tty_pump(unsigned abilities);
extern void     kill_coprocs(void);


/* lib.c */

extern integer *l_card(block *a);
extern block   *l_star(block *a, block *b);
extern block   *l_power(block *a, block *b);
extern block   *l_uplus(block *a);
extern block   *l_bplus(block *a, block *b);
extern void     l_aplus(block **a, block *b);
extern block   *l_uminus(block *a);
extern block   *l_bminus(block *a, block *b);
extern void     l_aminus(block **a, block *b);
extern block   *l_slash(block *a, block *b);
extern boolean *l_ne(block *a, block *b);
extern boolean *l_lt(block *a, block *b);
extern boolean *l_le(block *a, block *b);
extern boolean *l_eq(block *a, block *b);
extern boolean *l_gt(block *a, block *b);
extern boolean *l_ge(block *a, block *b);
extern block   *l_query(block *a, block *b);
extern block   *l_abs(block *a);
extern real    *l_acos(real *a);
extern integer *l_accept(block *a);
extern boolean *l_and(boolean *a, boolean *b);
extern string  *l_any(string **a, string *b);
extern block   *l_arb(set *a);
extern real    *l_arg(block *a);
extern real    *l_asin(real *a);
extern real    *l_atan(real *a);
extern real    *l_atan2(real *a, real *b);
extern integer *l_bit_and(integer *a, integer *b);
extern integer *l_bit_not(integer *a);
extern integer *l_bit_or(integer *a, integer *b);
extern integer *l_bit_xor(integer *a, integer *b);
extern string  *l_break(string **a, string *b);
extern void     l_callf(block *a, block *b, block *c);
extern string  *l_callout(integer *a, block *b, tuple *c);
extern string  *l_callout2(integer *a, block *b, tuple *c);
extern integer *l_ceil(real *a);
extern string  *l_char(integer *a);
extern void     l_chdir(tuple *a, long nargs);
extern void     l_clear_error(void);
extern integer *l_clock(void);
extern void     l_close(block *a, tuple *b, long nargs);
extern tuple   *l_command_line(void);
extern string  *l_command_name(void);
extern block   *l_compile(block *a);
extern block   *l_complex(block *a);
extern real    *l_cos(real *a);
extern real    *l_cosh(real *a);
extern string  *l_date(void);
extern string  *l_denotype(string *a);
extern integer *l_div(integer *a, integer *b);
extern set     *l_domain(set *a);
extern real    *l_double(block *a);
extern integer *l_dup(integer *);
extern integer *l_dup2(integer *, integer *);
extern void     l_eject(tuple *a, long nargs);
extern boolean *l_eof(tuple *a, long nargs);
extern boolean *l_even(integer *a);
extern void     l_exec(string *a, tuple *b, long nargs) NO_RETURN;
extern block   *l_execute(block *a);
extern real    *l_exp(real *a);
extern string  *l_fdate(integer *ms, tuple *fmt, long nargs);
extern boolean *l_fexists(string *a);
extern block   *l_filename(block *a);
extern integer *l_fileno(block *a);
extern integer *l_filepos(block *a);
extern string  *l_filter(string *a, tuple *b, long nargs);
extern integer *l_fix(real *a);
extern string  *l_fixed(real *v, integer *w, integer *a);
extern real    *l_float(integer *a);
extern string  *l_floating(real *v, integer *w, integer *a);
extern integer *l_floor(real *a);
extern void     l_flush(block *a);
extern integer *l_fork(void);
extern void     l_from(block **a, set **b);
extern void     l_fromb(block **a, block **b);
extern void     l_frome(block **a, block **b);
extern integer *l_fsize(block *a);
extern void     l_ftrunc(block *a, integer *b);
extern void     l_get(tuple **a, long nargs);
extern void     l_geta(block *a, tuple **b, long nargs);
extern void     l_getb(block *a, tuple **b, long nargs);
extern string  *l_getc(block *a);
extern string  *l_getchar(void);
extern integer *l_getegid(void);
extern void     l_getem(block **a, block **b);
extern string  *l_getenv(string *a);
extern integer *l_geteuid(void);
extern void     l_getf(block *a, tuple **b, long nargs);
extern string  *l_getfile(block *a);
extern integer *l_getgid(void);
extern integer *l_getipp(block *a);
extern void     l_getk(tuple **a, long nargs);
extern string  *l_getline(block *a);
extern string  *l_getn(block *a, integer *b);
extern integer *l_getpgid(integer *a);
extern integer *l_getpgrp(void);
extern integer *l_getpid(void);
extern integer *l_getppid(void);
extern void     l_gets(block *a, integer *b, integer *c, string **d);
extern integer *l_getsid(tuple *a, long nargs);
extern string  *l_getspp(block *a);
extern integer *l_getuid(void);
extern string  *l_getwd(void);
extern tuple   *l_glob(string *a);
extern tuple   *l_gmark(string *a, block *b);
extern tuple   *l_gsub(string **a, block *b, tuple *c, long nargs);
extern string  *l_hex(string *a);
extern string  *l_host(tuple *a, long nargs);
extern string  *l_hostaddr(void);
extern string  *l_hostname(void);
extern integer *l_ichar(string *a);
extern real    *l_imag(block *a);
extern boolean *l_impl(boolean *a, boolean *b);
extern boolean *l_in(block *a, block *b);
extern boolean *l_incs(set *a, set *b);
extern integer *l_int(block *a);
extern boolean *l_intslash(void);
extern set     *l_ip_addresses(tuple *a, long nargs);
extern set     *l_ip_names(tuple *a, long nargs);
extern boolean *l_is_atom(block *a);
extern boolean *l_is_boolean(block *a);
extern boolean *l_is_complex(block *a);
extern boolean *l_is_double(block *a);
extern boolean *l_is_float(block *a);
extern boolean *l_is_integer(block *a);
extern boolean *l_is_map(block *a);
extern boolean *l_is_mmap(block *a);
extern boolean *l_is_numeric(block *a);
extern boolean *l_is_om(block *a);
extern boolean *l_is_open(block *a);
extern boolean *l_is_op(block *a);
extern boolean *l_is_proc(block *a);
extern boolean *l_is_real(block *a);
extern boolean *l_is_routine(block *a);
extern boolean *l_is_set(block *a);
extern boolean *l_is_smap(block *a);
extern boolean *l_is_string(block *a);
extern boolean *l_is_there(block *a);
extern boolean *l_is_tuple(block *a);
extern string  *l_join(tuple *a, string *b);
extern void     l_kill(integer *a, tuple *b, long nargs);
extern integer *l_last_errno(void);
extern string  *l_last_error(void);
extern string  *l_len(string **a, integer *b);
extern set     *l_less(set *a, block *b);
extern void     l_aless(set **a, block *b);
extern set     *l_lessf(set *a, block *b);
extern void     l_alessf(set **a, block *b);
extern boolean *l_lexists(string *a);
extern void     l_link(string *a, string *b);
extern real    *l_log(real *a);
extern string  *l_lpad(string *a, integer *b);
extern boolean *l_magic(void);
extern tuple   *l_mark(string *a, block *b);
extern string  *l_match(string **a, string *b);
extern block   *l_max(block *a, block *b);
extern integer *l_mem_alloc(integer *a);
extern void     l_mem_free(integer *a);
extern integer *l_mem_realloc(integer *a, integer *b);
extern void     l_mem_copy(integer *a, integer *b, integer *c);
extern string  *l_mem_fetch_string(integer *a, integer *b);
extern string  *l_mem_fetch_c_string(integer *a);
extern void     l_mem_store_string(string *a, integer *b);
extern void     l_mem_store_c_string(string *a, integer *b);
extern block   *l_min(block *a, block *b);
extern integer *l_mkstemp(string **a);
extern block   *l_mod(block *a, block *b);
extern atom    *l_newat(void);
extern boolean *l_not(boolean *a);
extern string  *l_notany(string **a, string *b);
extern boolean *l_notin(block *a, block *b);
extern set     *l_npow(block *a, block *b);
extern void     l_nprint(tuple *a, long nargs);
extern void     l_nprinta(block *a, tuple *b, long nargs);
extern boolean *l_odd(integer *a);
extern integer *l_open(block *a, string *b);
extern boolean *l_or(boolean *a, boolean *b);
extern string  *l_pack_short(integer *a);
extern string  *l_pack_unsigned_short(integer *a);
extern string  *l_pack_int(integer *a);
extern string  *l_pack_unsigned_int(integer *a);
extern string  *l_pack_long(integer *a);
extern string  *l_pack_unsigned_long(integer *a);
extern string  *l_pack_long_long(integer *a);
extern string  *l_pack_unsigned_long_long(integer *a);
extern string  *l_pack_integer(integer *a);
extern string  *l_pack_double(real *a);
extern string  *l_pack_float(real *a);
extern string  *l_pack_real(real *a);
extern string  *l_peekc(block *a);
extern string  *l_peekchar(void);
extern string  *l_peer_address(block *a);
extern string  *l_peer_name(block *a);
extern integer *l_peer_port(block *a);
extern tuple   *l_peer_sockaddr(block *a);
extern boolean *l_pexists(integer *a);
extern integer *l_pid(tuple *a, long nargs);
extern tuple   *l_pipe(void);
extern integer *l_pipe_from_child(void);
extern integer *l_pipe_to_child(void);
extern integer *l_port(block *a);
extern set     *l_pow(set *a);
extern string  *l_pretty(block *a);
extern void     l_print(tuple *a, long nargs);
extern void     l_printa(block *a, tuple *b, long nargs);
extern integer *l_pump(void);
extern void     l_put(tuple *a, long nargs);
extern void     l_puta(block *a, tuple *b, long nargs);
extern void     l_putb(block *a, tuple *b, long nargs);
extern void     l_putc(block *a, string *b);
extern void     l_putchar(string *a);
extern void     l_putenv(string *a);
extern void     l_putf(block *a, tuple *b, long nargs);
extern void     l_putfile(block *a, string *b);
extern void     l_putk(tuple **a, long nargs);
extern void     l_putline(block *a, tuple *b, long nargs);
extern void     l_puts(block *a, integer *b, string *c);
extern block   *l_random(block *a);
extern set     *l_range(set *a);
extern string  *l_rany(string **a, string *b);
extern string  *l_rbreak(string **a, string *b);
extern void     l_read(tuple **a, long nargs);
extern void     l_reada(block *a, tuple **b, long nargs);
extern string  *l_readlink(string *a);
extern void     l_reads(string *a, tuple **b, long nargs);
extern string  *l_recv(block *a);
extern tuple   *l_recvfrom(block *a);
extern integer *l_recv_fd(block *a);
extern integer *l_rem(integer *a, integer *b);
extern void     l_rename(string *a, string *b);
extern string  *l_reverse(string *a);
extern void     l_rewind(block *a);
extern string  *l_rlen(string **a, integer *b);
extern string  *l_rmatch(string **a, string *b);
extern string  *l_rnotany(string **a, string *b);
extern integer *l_round(real *a);
extern string  *l_rpad(string *a, integer *b);
extern string  *l_rspan(string **a, string *b);
extern integer *l_seek(block *a, integer *b, tuple *c, long nargs);
extern tuple   *l_select(tuple *a, tuple *t, long nargs);
extern void     l_send(block *a, string *b);
extern void     l_sendto(block *a, block *b, string *c);
extern void     l_send_fd(block *a, integer *b);
extern void     l_setctty(block *a);
extern void     l_setegid(integer *a);
extern void     l_setem(block *a, block *b);
extern void     l_setenv(string *a, tuple *b, long nargs);
extern void     l_seteuid(integer *a);
extern void     l_setgid(integer *a);
extern void     l_setpgid(integer *a, integer *b);
extern void     l_setpgrp(void);
extern void     l_setrandom(integer *a);
extern void     l_setsid(void);
extern void     l_setuid(integer *a);
extern boolean *l_set_intslash(boolean *a);
extern boolean *l_set_magic(boolean *a);
extern void     l_shutdown(block *a, integer *b);
extern integer *l_sign(block *a);
extern real    *l_sin(real *a);
extern real    *l_sinh(real *a);
extern tuple   *l_sockaddr(block *a);
extern tuple   *l_socketpair(void);
extern string  *l_span(string **a, string *b);
extern tuple   *l_split(string *a, tuple *b, long nargs);
extern real    *l_sqrt(real *a);
extern integer *l_status(void);
extern string  *l_str(block *a);
extern string  *l_strad(integer *a, integer *b);
extern string  *l_sub(string **a, block *b, tuple *c, long nargs);
extern boolean *l_subset(set *a, set *b);
extern void     l_symlink(string *a, string *b);
extern integer *l_system(string *a);
extern string  *l_sys_read(integer *a, integer *b);
extern integer *l_sys_write(integer *a, string *b);
extern real    *l_tan(real *a);
extern real    *l_tanh(real *a);
extern integer *l_tcgetpgrp(block *a);
extern void     l_tcsetpgrp(block *a, integer *b);
extern void     l_tie(block *a, block *b);
extern integer *l_time(void);
extern void     l_title(tuple *a, long nargs);
extern string  *l_tmpnam(void);
extern string  *l_to_lower(string *a);
extern string  *l_to_upper(string *a);
extern integer *l_tod(void);
extern integer *l_tty_pump(void);
extern string  *l_type(block *a);
extern integer *l_umask(tuple *a, long nargs);
extern void     l_ungetc(block *a, string *b);
extern void     l_ungetchar(string *a);
extern string  *l_unhex(string *a);
extern void     l_unlink(string *a);
extern integer *l_unpack_short(string *a);
extern integer *l_unpack_unsigned_short(string *a);
extern integer *l_unpack_int(string *a);
extern integer *l_unpack_unsigned_int(string *a);
extern integer *l_unpack_long(string *a);
extern integer *l_unpack_unsigned_long(string *a);
extern integer *l_unpack_long_long(string *a);
extern integer *l_unpack_unsigned_long_long(string *a);
extern integer *l_unpack_integer(string *a);
extern real    *l_unpack_double(string *a);
extern real    *l_unpack_float(string *a);
extern real    *l_unpack_real(string *a);
extern string  *l_unpretty(string *a);
extern void     l_unsetctty(block *a);
extern void     l_unsetenv(string *a);
extern block   *l_unstr(string *a);
extern void     l_untie(block *a, block *b);
extern block   *l_val(string *a);
extern integer *l_wait(tuple *a, long nargs);
extern integer *l_waitpid(integer *a, tuple *b, long nargs);
extern string  *l_whole(block *v, integer *w);
extern block   *l_with(block *a, block *b);
extern void     l_awith(block **a, block *b);
extern void     l_write(tuple *a, long nargs);
extern void     l_writea(block *a, tuple *b, long nargs);


/* rex.c */

extern string  *rex_fetch(string *s, string *p);
extern void     rex_store(string **host, string *p, string *x);
extern string  *rex_getslice(string *s, block *p, block *q);
extern void     rex_insertslice(string **host, block *p, block *q,
                                                            string *x);
extern tuple   *rex_mark(string *s, block *p);
extern string  *rex_sub(string **host, block *p, string *x);
extern tuple   *rex_gmark(string *s, block *p);
extern tuple   *rex_gsub(string **host, block *p, string *x);
extern tuple   *rex_split(string *s, string *p);


/* os.c */

extern void     os_error(const char *s) NO_RETURN;

extern void    *os_malloc(size_t nbytes);
extern void    *os_realloc(void *p, size_t nbytes);
extern void     os_free(void *p);

extern char    *os_getenv(const char *name);
extern void     os_setenv(const char *name, const char *value);
extern void     os_unsetenv(const char *name);
extern void     os_putenv(char *s);
extern mode_t   os_get_umask(void);
extern mode_t   os_set_umask(mode_t mask);
extern void     os_getcwd(char *path, int maxlen);
extern void     os_chdir(const char *path);
extern int      os_glob(const char *pattern, int flags,
                         int (*errfunc) (const char *epath, int eerrno),
                          glob_t *pglob);
extern void     os_globfree(glob_t *pglob);
extern int      os_stat(const char *path, struct stat *buf);
extern int      os_fstat(int fd, struct stat *buf);
extern int      os_lstat(const char *path, struct stat *buf);
extern int      os_link(const char *oldpath, const char *newpath);
extern int      os_symlink(const char *oldpath, const char *newpath);
extern int      os_rename(const char *oldpath, const char *newpath);
extern ssize_t  os_readlink(const char *path, char *buf, size_t bufsiz);
extern int      os_mkdir(const char *path, mode_t mode);
extern int      os_mkfifo(const char *path, mode_t mode);
extern int      os_mknod(const char *path, mode_t mode, dev_t dev);
extern int      os_chmod(const char *path, mode_t mode);
extern int      os_chown(const char *path, uid_t owner, gid_t group);
extern int      os_unlink(const char *path);
extern int      os_rmdir(const char *path);
extern int      os_remove(const char *path);
extern int      os_truncate(const char *path, off_t length);
extern int      os_ftruncate(int fd, off_t length);
extern int      os_fputc(int c, FILE *fp);
extern int      os_printf(const char *fmt, ...) LIKE_PRINTF;
extern int      os_fprintf(FILE *fp, const char *fmt, ...) LIKE_FPRINTF;
extern int      os_vprintf(const char *fmt, va_list ap) LIKE_VPRINTF;
extern int      os_vfprintf(FILE *fp, const char *fmt, va_list ap) LIKE_VFPRINTF;
extern int      os_max_open_files(void);
extern void     os_check_fd(int fd);
extern int      os_open(const char *path, int oflag);
extern int      os_pipe(int p[2]);
extern int      os_dup(int fd);
extern int      os_dup2(int fd1, int fd2);
extern int      os_mkstemp(char *templat);
extern const char *os_tmpnam(void);
extern int      os_fcntl(int fd, int cmd, int arg);
extern bool     os_is_fd(int fd);
extern int      os_ioctl_int(int fd, int cmd, int arg,
                                                   const char *what);
extern int      os_ioctl_addr(int fd, int cmd, void *arg,
                                                   const char *what);
extern bool     os_close(int fd);
extern void     os_shutdown(int fd, int how);
extern off_t    os_seek(int fd, off_t offset, int whence);
extern ssize_t  os_read(int fd, void *buf, size_t nbytes);
extern ssize_t  os_recv(int fd, void *buf, size_t nbytes,
                  int flags);
extern ssize_t  os_recvfrom(int fd, void *buf, size_t nbytes,
                  int flags, struct sockaddr *from, socklen_t *fromlen);
extern ssize_t  os_recvmsg(int fd, struct msghdr *msg, int flags);
extern int      os_recv_fd(int fd);
extern ssize_t  os_write(int fd, const void *buf, size_t nbytes);
extern ssize_t  os_send(int fd, const void *buf, size_t nbytes,
                  int flags);
extern ssize_t  os_sendto(int fd, const void *buf, size_t nbytes,
                  int flags, const struct sockaddr *to, socklen_t tolen);
extern ssize_t  os_sendmsg(int fd, const struct msghdr *msg, int flags);
extern int      os_send_fd(int fd, int fd_to_send);
extern int      os_pselect(int nfds, fd_set *readfds, fd_set *writefds,
                  fd_set *exceptfds, const struct timespec *timeout,
                  const sigset_t *sigmask);
extern int      os_accept(int fd, struct sockaddr *sa, socklen_t *salen);
extern int      os_bind(int fd, const struct sockaddr *sa, socklen_t salen);
extern int      os_connect(int fd, const struct sockaddr *sa, socklen_t salen);
extern int      os_listen(int fd, int backlog);
extern int      os_socket(int family, int type, int protocol);
extern void     os_setsockopt(int fd, int level, int optname,
                               const void *optval, socklen_t optlen);
extern int      os_getsocktype(int fd);
extern bool     os_getsockname(int fd, struct sockaddr *sa, socklen_t *salen);
extern bool     os_getpeername(int fd, struct sockaddr *sa, socklen_t *salen);
extern int      os_gethostname(char *name, size_t len);
extern int      os_getnameinfo(const struct sockaddr *sa, socklen_t salen,
                   char *nodename, size_t nodelen,
                   char *servname, size_t servlen, int flags);
extern int      os_getaddrinfo(const char *nodename, const char *servname,
                   const struct addrinfo *hints, struct addrinfo **res);
extern void     os_freeaddrinfo(struct addrinfo *list);

extern struct timespec os_tod(void);      /* time since 1970 */
extern struct timespec os_mono(void);     /* steadily advancing time */
extern void     os_set_time_base(void);   /* base for elapsed time */
extern struct timespec os_elapsed(void);  /* elapsed (clock) time */
extern struct timeval os_cputime(void);   /* consumption by this pgm */
extern struct tm *os_localtime(const time_t *t);
extern size_t   os_strftime(char *s, size_t n, const char *fmt,
                                                const struct tm *t);

#if USE_POSIX_RT
extern timer_t  os_timer_create(void);
extern void     os_timer_delete(timer_t t);
extern void     os_timer_delete_quietly(timer_t t);
extern void     os_timer_gettime(timer_t t, struct itimerspec *cur);
extern void     os_timer_settime(timer_t t, int flags,
                                      const struct itimerspec *new_spec,
                                            struct itimerspec *old_spec);
#else
extern void     os_get_timer(int which, struct itimerval *t);
extern void     os_set_timer(int which, const struct itimerval *t);
#endif

extern void     os_signal(int sig, const sig_disp handler,
                                            struct sigaction *oldact);
extern void     os_sigaction(int sig, const struct sigaction *act,
                                            struct sigaction *oldact);
extern void     os_sigrestore(int sig, const struct sigaction *oldact);
extern void     os_sigprocmask(int how, const sigset_t *mask,
                                              sigset_t *old_mask);
extern void     os_sigblock(int sig, sigset_t *old_mask);
extern void     os_sigsetmask(const sigset_t *mask);
extern void     os_sigsuspend(const sigset_t *mask);
extern void     os_sigemptyset(sigset_t *s);
extern void     os_sigaddset(sigset_t *s, int sig);
extern int      os_sigismember(sigset_t *s, int sig);
extern int      os_kill(pid_t pid, int sig);
extern pid_t    os_fork(void);
extern void     os_execvp(const char *path, char *const argv[]) NO_RETURN;
extern void     os_execve(const char *path, char *const argv[],
                                            char *const envp[]) NO_RETURN;
extern void     os_exec_sh(const char *cmd) NO_RETURN;
extern pid_t    os_waitpid(pid_t pid, int *statusp, int options);
extern pid_t    os_getpid(void);
extern pid_t    os_getppid(void);
extern pid_t    os_getpgid(pid_t pid);
extern pid_t    os_getpgrp(void);
extern void     os_setpgid(pid_t pid, pid_t pgid);
extern void     os_setpgrp(void);
extern pid_t    os_getsid(pid_t pid);
extern void     os_setsid(void);
extern pid_t    os_tcgetpgrp(int fd);
extern void     os_tcsetpgrp(int fd, pid_t pgid);
extern void     os_setctty(int fd);
extern void     os_unsetctty(int fd);

extern uid_t    os_getuid(void);
extern void     os_setuid(uid_t uid);
extern gid_t    os_getgid(void);
extern void     os_setgid(gid_t gid);
extern uid_t    os_geteuid(void);
extern void     os_seteuid(uid_t uid);
extern gid_t    os_getegid(void);
extern void     os_setegid(gid_t gid);

extern int      ptym_open(char *pts_name);
extern int      ptys_open(int mfd, const char *pts_name);
extern void     tty_rawmode(int fd);
extern bool     os_isatty(int fd);

extern const char *os_strerror(int errnum);


/* util.c */

extern opndtype  find_opndtype(instr *x, long iopnd);
extern bool     digital(const char *s);
#if !HAVE_STRCASECMP
extern bool     case_insensitively_equal(const char *s, const char *t);
#endif
extern char    *strncpy_plus_nul(char *r, const char *s, size_t n);
extern char    *do_finite_strcpy(char *r, const char *s, size_t n);
extern const char  *tame(const char *s);
extern const char  *qtame(char q, const char *s);
extern void     make_printable(char *p);
extern void     print_stderr(const char *fmt, ...) LIKE_PRINTF;
extern void     putc_stderr(int c);
extern void     zero_timeval (struct timeval *r);
extern bool     timeval_is_zero (const struct timeval *t);
extern void     assign_timeval (struct timeval *dst,
                          const struct timeval *src);
extern void     add_timevals(struct timeval *result,
                       const struct timeval *a,
                       const struct timeval *b);
extern void     subtract_timevals(struct timeval *result,
                            const struct timeval *a,
                            const struct timeval *b);
extern int      compare_timevals(const struct timeval *a,
                                 const struct timeval *b);
extern struct timespec  timespec_plus (struct timespec a,
                                       struct timespec b);
extern struct timespec  timespec_minus (struct timespec a,
                                        struct timespec b);
extern int      timespec_compare (struct timespec a,
                                  struct timespec b);
extern bool     timespec_is_zero (struct timespec a);
extern struct timespec  timeval_to_timespec(struct timeval t);
extern struct timeval  timespec_to_timeval(struct timespec t);
extern int      peek_char(io_buffer *b);
extern void     eat_white(io_buffer *b);
extern void     span_white(const string *a, long *i);
extern bool     put_chars(file *f, string *s);
extern mpz_srcptr  alias_mpz (mpz_t z,
                              const mp_limb_t *limbs, ssize_t size);
extern ulong    random_ulong (ulong n);
extern table   *empty_table(void);  /* new empty associative table */
extern key     *string_to_key (string *s);
extern string  *key_to_string (key *k);
extern key     *num_to_key (long i);
extern long     key_to_num (key *k);
extern key     *ith_key (table *t, long i);
extern block   *ith_block (table *t, long i);
extern long     ith_num (table *t, long i);
extern bool     has_key (table *t, key *k);
extern block   *lookup_block (table *t, key *k);
extern long     lookup_num (table *t, key *k);
extern table   *lookup_subtable (table *t, key *k);
extern tuple   *lookup_tuple (table *t, key *k);
extern bool     see_block (table *t, key *k, block **a);
extern bool     see_num (table *t, key *k, long *a);
extern bool     insert_block (table *t, key *k, block *a);
extern bool     insert_num (table *t, key *k, long a);
extern bool     insert_subtable (table *t, key *k, table *a);
extern bool     insert_tuple (table *t, key *k, tuple *a);
extern void     insert_block_new (table *t, key *k, block *a);
extern bool     cond_insert_block (table *t, key *k, block *a);
extern bool     cond_insert_num (table *t, key *k, long a);
extern bool     insert_or_get_block (table *t, key *k, block **a);
extern bool     insert_or_get_num (table *t, key *k, long *a);


/* callskel.c */

extern char    *setl2_callout(int service, unsigned argc, char *const argv[]);



/*
 *  Data that may be shared among files of this C program.
 */

extern bool exit_please;  /* set to stop reading SETL VM input */

/*
 *  Character recognition and translation tables:
 */
extern bool terminator[CHARSETSIZE];   /* line terminator */
extern bool separator [CHARSETSIZE];   /* input field separator */
extern bool prefix    [CHARSETSIZE];   /* input operand prefix */
extern bool alphabetic[CHARSETSIZE];   /* a-z and A-Z */
extern bool idtail    [CHARSETSIZE];   /* alpha+digit+'_' */
extern bool strok     [CHARSETSIZE];   /* unmodified by PRETTY */
extern bool musttwin  [CHARSETSIZE];   /* repeated by PRETTY */
extern bool ctrlchar  [CHARSETSIZE];   /* '\a\b\f\n\r\t\v' */
extern bool whitespace[CHARSETSIZE];   /* ' \f\n\r\t\v' */
extern bool blankortab[CHARSETSIZE];   /* ' \t' */
extern bool valsep    [CHARSETSIZE];   /* whitespace+',' */
extern bool nongarbage[CHARSETSIZE];   /* valsep+']}' */
extern bool exponent  [CHARSETSIZE];   /* 'eE' */
extern bool plusminus [CHARSETSIZE];   /* '+-' */
extern bool numeric   [CHARSETSIZE];   /* '0123456789+-.eE' */
extern bool decdigit  [CHARSETSIZE];   /* '0123456789' */
extern bool posdigit  [CHARSETSIZE];   /* '123456789' */
extern bool hexdigit  [CHARSETSIZE];   /* '0123456789abcdefABCDEF' */
extern bool octdigit  [CHARSETSIZE];   /* '01234567' */

extern char to_lower  [CHARSETSIZE];   /* upper- to lowercase */
extern char to_upper  [CHARSETSIZE];   /* lower- to uppercase */

extern int  hexval    [CHARSETSIZE];   /* base 16 */
extern int  octval    [CHARSETSIZE];   /* base 8 */
extern int  digval    [CHARSETSIZE];   /* base 10 */
extern int  setl2digval[CHARSETSIZE];  /* base 2 thru 36 */

extern backslash_chaser escapee[CHARSETSIZE];

#if HAVE_LLONG
extern mpz_t            const_mpz_llong_min;  /* LLONG_MIN as an mpz_t */
#endif
extern gmp_randstate_t  randstate;     /* GMP PRNG state */

/*
 *  Misc. utility maps:
 */
extern table  *optab;    /* map from string opcode to integer opcode */
extern HANDLE h_optab;

extern table  *sysdats;  /* map from system "variable" name to number */
extern HANDLE h_sysdats;

extern table  *sysrots;  /* map from system routine name to number */
extern HANDLE h_sysrots;

extern table  *sysrotsigs;  /* map from system routine name to signature */
extern HANDLE h_sysrotsigs;

/*
 *  This mmap allows you to use a name instead of the fd on I/O ops
 *  when it is an unambiguous (single fd) reference.  A name is a
 *  STRING (e.g. a filename) or a 2-TUPLE (e.g. [host, port]).
 */
extern set    *name_to_fd;  /* mmap from name to file descriptor */
extern HANDLE h_name_to_fd;

/*
 *  Interval timers and other signals:
 */
extern sig_info *sigfo;     /* info about signals */

/*
 *  Each pid on the 'reapable' list refers to a child process that was
 *  once associated with an open I/O stream that has now been closed in
 *  CLOSE_AUTOREAP mode.  That pid is removed from the list if and when
 *  the SIGCHLD handler waits away the child, and the list node is then
 *  moved to the 'reaped' list.  The 'reaped' list serves as a free list
 *  that is the first choice when new nodes are needed in response to
 *  further CLOSE(...,CLOSE_AUTOREAP) calls.
 */
extern pid_list *reapable;  /* CLOSE_AUTOREAP pids */
extern pid_list *reaped;    /* a free list */

/*
 *  "Normal" fds come from the host OS; pseudo-fds are what we use
 *  to label signal and timer "streams" (see file_init() in init.c).
 *
 *  Note that after init, 0 = fd_lo < fd_hi = sd_lo < sd_hi = n_files
 *  and fd_hi = os_max_open_files().
 */
extern int  fd_lo, fd_hi;  /* normal fd range, fd_lo <= fd < fd_hi */
extern int  sd_lo, sd_hi;  /* pseudo-fd range, sd_lo <= fd < sd_hi */
extern int  n_files;       /* total # of files in 'files' (= sd_hi) */
extern file *files;        /* indexed by fd */
#define is_normal_fd(fd) (fd_lo <= (fd) && (fd) < fd_hi)
#define is_pseudo_fd(sd) (sd_lo <= (sd) && (sd) < sd_hi)

/*
 *  Things governed by the command line (external program interface):
 */
extern const char *setlprog;  /* driver/run-time program basename */
extern bool  spew;            /* emit assembled code; don't 'execute' */

extern bool  verbose;         /* running commentary on execution */
extern bool  debug;           /* gory details of memory, etc. */

extern bool  abort_on_error;  /* abort() rather than exit() on error */

extern bool  restricted;      /* disallow various OS ops and OPEN */

extern size_t maxmem,memsize;  /* control the allocator */

extern allowed  *allowed_open_list;     /* for --allow-open options */
extern allowed  *allowed_fd_open_list;  /* for --allow-fd-open options */
extern allowed  *allowed_mkstemp_list;  /* for --allow-mkstemp options */
extern allowed  *allowed_filter_list;   /* for --allow-filter options */
extern allowed  *allowed_system_list;   /* for --allow-system options */

extern tuple   *command_line;  /* run-time (command-line) args */
extern HANDLE  h_command_line;

extern string  *command_name;  /* command that launched this SETL VM */
extern HANDLE  h_command_name;

extern integer *stdin_integer;   /* fd_stdin as an INTEGER */
extern HANDLE  h_stdin_integer;
extern integer *stdout_integer;  /* fd_stdout as an INTEGER */
extern HANDLE  h_stdout_integer;

/*
 *  In --restricted mode, UMASK cannot be used to clear bits that were
 *  set in the environmentally provided file creation mask (umask):
 */
extern mode_t  initial_umask;  /* umask as provided by the environment */

/*
 *  Misc. shared resources:
 */
extern long atom_counter;    /* for NEWAT calls */

/*
 *  Current SETL virtual machine instance:
 */
extern machine *setl_vm;  /* the route of all weevils */
extern HANDLE h_setl_vm;  /* even SETL VMs are garbage-collectible */


#endif  /* _setlrun_h */
