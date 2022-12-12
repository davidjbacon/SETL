/*  ===  SETL run-time fundamentals  ===============================  */

/*  $Id: sys.c,v 1.310 2022/12/10 23:35:26 setlorg Exp $  */

/*  Free software (c) dB - see file COPYING for license (GPL).  */

/*
 *  Roughly speaking, what is here is mostly intended to be the
 *  implementation level directly below the "library" (lib.c), for
 *  the objects representing SETL values.
 */

#include "setlrun.h"


/*
 *  When an integer is a key, it is stored big-endian.
 */
#if    GMP_LIMB_BITS == LONG_BIT
#define BE_LIMB(x)   BE_LONG(x)
#elif  GMP_LIMB_BITS == LLONG_BIT
#define BE_LIMB(x)   BE_LLONG(x)
#else
#error Cannot define BE_LIMB macro.
#endif


/*
 *  A local container for the dispositions of SIGINT and SIGQUIT
 */
typedef struct {
  struct sigaction  saved_sigint, saved_sigquit;
} sigsave_t;


static const struct timespec  timespec_zero = {0, 0};  /* s, ns */


/*
 *  Declarations of all our local functions...
 */

static integer *cvt_mpz_to_integer(const mpz_t z);
static bool fits_long(integer *i);
static bool fits_ulong(integer *i);
#if HAVE_LLONG
static bool fits_llong(integer *i);
static bool fits_ullong(integer *i);
static ullong integer_to_ullong_unchecked(integer *i);
#endif

/* Helpers for 'jetsam' and 'flotsam' macros resp.  */
static bool deno_ends_ok(io_buffer *buffer, int stopper);
static bool deno_tail_ok(string *a, long j, int stopper);

static int compare_longs (long a, long b);
static int compare_doubles (double a, double b);

static size_t nbytes_in_key(block *p);
static size_t cvt_to_key(key *k, block *p, size_t ik);
static block *cvt_from_key(key *k, size_t *pik);

/* Signal/timer-related utilities.  */
static int sig_num(const char *s);
static void read_event(int fd);
static bool note_signals(fd_bitset *ready, fd_bitset *wants, int nfd);
static bool is_event_client(file *f);
static bool is_event_pending(file *f);
static void jostle_timers(void);

static const open_mode *find_open_mode(open_how how);
static const char *canonical_mode_name(open_how how);

/* Helpers for 'file_open' and 'file_close'.  */
static void check_open_fd_allowed(int fd, open_how how);
static void check_open_allowed(const char *what, open_how how);
static int open_real_ms_timer (struct timespec initial,
                               struct timespec interval,
                               unsigned abilities);
static int open_socket (const char *nodename, const char *servname,
                        filetype ftype, unsigned abilities);
static int open_unix_socket (const char *pathname,
                        filetype ftype, unsigned abilities);
enum socket_role {client_role, server_role};
static int new_socket (const char *nodename, const char *servname,
                       int socktype, enum socket_role role);
static int new_unix_socket (const char *pathname,
                       int socktype, enum socket_role role);
static int new_pseudo_fd(void);
static file *new_pipe_file(int fd, filetype ftype, unsigned abilities,
                           pid_t pid);
static file *new_signal_file(int fd, filetype ftype, unsigned abilities,
                             buffering policy, int sig);
static void hello_sig_reader(int sig, const sig_disp handler);
static void hello_sig_ignorer(int sig);
static void hello_sig_defaulter(int sig);
static void bye_sig_client(int sig);
static void add_file(file **h, file *f);     /* insert f into h list */
static void excise_file(file **h, file *f);  /* unlink f from h list */

static int unique_fd(set *s);  /* get fd from a set of 1 */

/* Helper for 'insert_name_to_fd' and 'delete_name_to_fd'.  */
static void update_name_to_fd(int fd, void (*set_op)(set **, block *));

/* Helpers for variously connected subprocesses.  */
static void save_and_defend (sigsave_t *p);
static void restore_actions (const sigsave_t *p);
static int co_fork(int *pfd, unsigned abilities,
                    const sigsave_t *saved_sigs);
static int pty_fork(int *pfd, unsigned abilities,
                     const sigsave_t *saved_sigs);
static pid_t sys_fork(const sigsave_t *saved_sigs);
static void atfork_child(const sigsave_t *saved_sigs);
static void std_redir(int fd, unsigned abilities);


const char *type_name(const block *p) {
  switch (setl_type(p)) {
  case om_type:        return "OM";
  case atom_type:      return "ATOM";
  case boolean_type:   return "BOOLEAN";
  case small_type:     return "SMALL";
  case integer_type:   return "INTEGER";
  case real_type:      return "REAL";
  case set_type:       return "SET";
  case string_type:    return "STRING";
  case tuple_type:     return "TUPLE";
  case routine_type:   return "PROC_REF";
  case iterator_type:  return "ITERATOR";
  case table_type:     return "TABLE";
  case subnode_type:   return "SUBNODE";
  case key_type:       return "KEY";
  case source_type:    return "SOURCE";
  case codeline_type:  return "CODELINE";
  case instr_type:     return "INSTR";
  case parm_type:      return "PARM";
  case frame_type:     return "FRAME";
  case symtab_type:    return "SYMTAB";
  case machine_type:   return "MACHINE";
  default:
    {
      static char s[50];
      snprintf (s, sizeof s, "<INVALID TYPE CODE %d>", setl_type(p));
      return s;
    }
  }
} /* end type_name */


atom *new_atom(long k) {
  atom *r = fblock(atom);
  r->atmval = k;
  return r;
}


boolean *new_boolean(bool b) {
  boolean *r = fblock(boolean);
  r->booval = b;
  return r;
}

bool get_bool(boolean *b, const char *what) {
  if (!is_boolean(b)) {
    runerr("Type of %s must be BOOLEAN, not %s",
                  what,             TYPENAME(b));
  }
  return b->booval;
}


/*
 *  This type of integer is intended mainly for the early phases of
 *  the interpreter, such as the assembler, but could be an alternate
 *  form of integer in the user's program some day if you thought the
 *  optimization was really worth it.
 */
small *new_small(long i) {
  small *r = fblock(small);
  r->weeval = i;
  return r;
}


/*
 *  Pieces of code common to many sys.c functions that return INTEGER.
 *
 *  The "alias" referred to in the comments on ro_mpz() calls is to
 *  the array of limbs, as 'integer' uses the same representation for
 *  the limbs as the GMP's mpz_t does.  The expectation is that the
 *  integer won't be changed while the aliased mpz_t is in use:
 */

#define new_prep                                                \
  integer *r;     /* INTEGER to be returned */                  \
  mpz_t z;        /* precursor to r */

#define unary_prep                                              \
  integer *r;     /* INTEGER to be returned */                  \
  mpz_t z;        /* precursor to r */                          \
  ro_mpz(za, a)   /* const mpz_t za = alias of INTEGER a */

#define binary_prep                                             \
  integer *r;     /* INTEGER to be returned */                  \
  mpz_t z;        /* precursor to r */                          \
  ro_mpz(za, a)   /* const mpz_t za = alias of INTEGER a */     \
  ro_mpz(zb, b)   /* const mpz_t zb = alias of INTEGER b */

#define cvt_and_finish                                          \
  r = cvt_mpz_to_integer(z);                                    \
  mpz_clear(z);   /* free z's limb space */                     \
  return r;

static integer *cvt_mpz_to_integer(const mpz_t z) {
  const size_t nlimb = mpz_size(z);
  size_t count;
  integer *r = alloc_integer(nlimb);
  r->size = mpz_sgn(z) < 0 ? -nlimb : nlimb;
  mpz_export (r->limbs, &count, -1, SIZEOF_LIMB, 0, GMP_NAIL_BITS, z);
  assert (count == nlimb);
  return r;
}

integer *new_integer(long i) {
  new_prep  /* declare mpz_t z, begin arena allocation scope */
  mpz_init_set_si (z, i);  /* use GMP to make new z from i */
  cvt_and_finish  /* end alloc scope, return z as INTEGER */
}

integer *ulong_integer(ulong i) {
  new_prep  /* declare mpz_t z, begin arena allocation scope */
  mpz_init_set_ui (z, i);  /* use GMP to make new z from i */
  cvt_and_finish  /* end alloc scope, return z as INTEGER */
}

#if HAVE_LLONG
integer *llong_integer(llong i) {
  const ullong j = ABS(i);
  new_prep  /* declare mpz_t z, begin arena allocation scope */
  mpz_init (z);  /* there is no mpz_init_import() */
  mpz_import (z, 1, -1, sizeof j, 0, 0, &j);  /* make z from j */
  if (i < 0) mpz_neg (z, z);  /* negate z if i < 0 */
  cvt_and_finish  /* end alloc scope, return z as INTEGER */
}

integer *ullong_integer(ullong i) {
  new_prep  /* declare mpz_t z, begin arena allocation scope */
  mpz_init (z);  /* there is no mpz_init_import() */
  mpz_import (z, 1, -1, sizeof i, 0, 0, &i);  /* make z from i */
  cvt_and_finish  /* end alloc scope, return z as INTEGER */
}
#endif /* HAVE_LLONG */

integer *double_to_integer(double d) {
  if (isfinite(d)) {
    new_prep  /* declare mpz_t z, begin arena allocation scope */
    mpz_init_set_d (z, d);  /* use GMP to convert d to new z */
    cvt_and_finish  /* end alloc scope, return z as INTEGER */
  } else {
    return OM;  /* e.g., for Inf or NaN */
  }
}

integer *charstr_to_integer(const char *s, int radix) {
  new_prep  /* declare mpz_t z, begin arena allocation scope */
  mpz_init_set_str(z, s, radix);  /* use GMP to convert s to new z */
  cvt_and_finish  /* end alloc scope, return z as INTEGER */
}


static bool fits_long(integer *i) {
  ro_mpz(z, i)  /* const mpz_t z = alias of INTEGER i */
  return mpz_fits_slong_p(z) != 0;
}

static bool fits_ulong(integer *i) {
  if (i->size >= 0) {  /* integer is non-negative */
    ro_mpz(z, i)  /* const mpz_t z = alias of INTEGER i */
    return mpz_fits_ulong_p(z) != 0;
  } else {
    return false;  /* integer is negative */
  }
}

#if HAVE_LLONG
static bool fits_llong(integer *i) {
  ro_mpz(z, i)  /* const mpz_t z = alias of INTEGER i */
  return mpz_sizeinbase (z, 2) <= LLONG_BIT - 1 ||
         mpz_cmp (z, const_mpz_llong_min) == 0;
}

static bool fits_ullong(integer *i) {
  if (i->size >= 0) {  /* integer is non-negative */
    ro_mpz(z, i)  /* const mpz_t z = alias of INTEGER i */
    return mpz_sizeinbase (z, 2) <= LLONG_BIT;
  } else {
    return false;  /* integer is negative */
  }
}
#endif /* HAVE_LLONG */


/*
 *  Get bounded long int from INTEGER i.
 */
long get_long_in(integer *i, long lwb, long upb, const char *what) {
  if (!is_integer(i)) {
    runerr("Type of %s must be INTEGER, not %s",
                  what,             TYPENAME(i));
  }
  if (fits_long(i)) {
    ro_mpz(z, i)  /* const mpz_t z = alias of INTEGER i */
    const long r = mpz_get_si(z);  /* use GMP to get r from z */
    if (lwb <= r && r <= upb) {
      return r;
    } else {
      runerr("Value of %s (%ld) is out of range %ld..%ld",
                     what,   r,                 lwb, upb);
    }
  } else {
    runerr("Value of %s is out of range %ld..%ld",
                   what,                lwb, upb);
  }
} /* end get_long_in */

long get_long(integer *i, const char *what) {
  return get_long_in(i, LONG_MIN,LONG_MAX, what);
}

long get_nat_long(integer *i, const char *what) {
  return get_long_in(i, 0,LONG_MAX, what);
}

long get_pos_long(integer *i, const char *what) {
  return get_long_in(i, 1,LONG_MAX, what);
}

ulong get_ulong(integer *i, const char *what) {
  if (!is_integer(i)) {
    runerr("Type of %s must be INTEGER, not %s",
                  what,             TYPENAME(i));
  }
  if (fits_ulong(i)) {
    ro_mpz(z, i)  /* const mpz_t z = alias of INTEGER i */
    return mpz_get_ui(z);  /* use GMP to get ulong result from z */
  } else {
    runerr("Value of %s is out of range 0..%lu",
                   what,             ULONG_MAX);
  }
}

int get_int(integer *i, const char *what) {
  return get_long_in(i, INT_MIN,INT_MAX, what);
}

int get_nat_int(integer *i, const char *what) {
  return get_long_in(i, 0,INT_MAX, what);
}

uint get_uint(integer *i, const char *what) {
#if UINT_MAX < ULONG_MAX
  return get_long_in(i, 0,UINT_MAX, what);
#else
  return get_ulong(i, what);
#endif
}

short get_short(integer *i, const char *what) {
  return get_long_in(i, SHRT_MIN,SHRT_MAX, what);
}

ushort get_ushort(integer *i, const char *what) {
  return get_long_in(i, 0,USHRT_MAX, what);
}

#if HAVE_LLONG
/*
 *  For an INTEGER i already known to fit within an unsigned long long,
 *  get its absolute value as an ullong.
 */
static ullong integer_to_ullong_unchecked(integer *i) {
  ullong r;
  size_t count;
  ro_mpz(z, i)  /* const mpz_t z = alias of INTEGER i */
  mpz_export (&r, &count, -1, sizeof r, 0, 0, z);  /* get r from z */
  assert ((i->size == 0 && count == 0) ||
          (i->size != 0 && count == 1));
  return r;
}

/*
 *  Get bounded long long (llong) from INTEGER i.
 */
llong get_llong_in(integer *i, llong lwb, llong upb, const char *what) {
  if (!is_integer(i)) {
    runerr("Type of %s must be INTEGER, not %s",
                  what,             TYPENAME(i));
  }
  if (fits_llong(i)) {
    const ullong u = integer_to_ullong_unchecked(i);
    const llong r = i->size < 0 ? -u : u;  /* signed result */
    if (lwb <= r && r <= upb) {
      return r;
    } else {
      runerr("Value of %s (%lld) is out of range %lld..%lld",
                     what,    r,                  lwb,  upb);
    }
  } else {
    runerr("Value of %s is out of range %lld..%lld",
                   what,                 lwb,  upb);
  }
} /* end get_llong_in */

llong get_llong(integer *i, const char *what) {
  return get_llong_in(i, LLONG_MIN,LLONG_MAX, what);
}

ullong get_ullong(integer *i, const char *what) {
  if (!is_integer(i)) {
    runerr("Type of %s must be INTEGER, not %s",
                  what,             TYPENAME(i));
  }
  if (fits_ullong(i)) {
    return integer_to_ullong_unchecked(i);
  } else {
    runerr("Value of %s is out of range of 0..%llu",
                   what,                ULLONG_MAX);
  }
}
#endif /* HAVE_LLONG */

#if SIZEOF_OFF_T <= SIZEOF_LONG
/* off_t fits in a long */
#define GET_OFF_T  get_long_in
#elif SIZEOF_OFF_T <= SIZEOF_LLONG
/* off_t fits in a llong */
#define GET_OFF_T  get_llong_in
#else
#error Please contribute an #elif for your unusually large off_t.
#endif

off_t get_off_t(integer *i, const char *what) {
  return GET_OFF_T(i, OFF_T_MIN,OFF_T_MAX, what);
}

off_t get_pos_off_t(integer *i, const char *what) {
  return GET_OFF_T(i, 1,OFF_T_MAX, what);
}

pid_t get_pid_t(integer *i, const char *what) {
  return get_long_in(i, PID_T_MIN,PID_T_MAX, what);
}

pid_t get_nat_pid_t(integer *i, const char *what) {
  return get_long_in(i, 0,PID_T_MAX, what);
}

uid_t get_uid_t(integer *i, const char *what) {
  return get_long_in(i, UID_T_MIN,UID_T_MAX, what);
}

gid_t get_gid_t(integer *i, const char *what) {
  return get_long_in(i, GID_T_MIN,GID_T_MAX, what);
}

mode_t get_mode_t(integer *i, const char *what) {
  return get_long_in(i, MODE_T_MIN,MODE_T_MAX, what);
}


double integer_to_double(integer *i) {
  ro_mpz(z, i)  /* const mpz_t z = alias of INTEGER i */
  return mpz_get_d(z);  /* use GMP to convert z to a double */
}


int integer_cmp(integer *a, integer *b) {
  ro_mpz(za, a)  /* const mpz_t za = alias of INTEGER a */
  ro_mpz(zb, b)  /* const mpz_t zb = alias of INTEGER b */
  return mpz_cmp(za, zb);
}


integer *integer_add(integer *a, integer *b) {
  binary_prep  /* define za, zb; declare z; begin arena alloc scope */
  mpz_init (z);  /* there is no mpz_init_add() */
  mpz_add (z, za, zb);  /* z := a + b */
  cvt_and_finish  /* end alloc scope, return z as INTEGER */
}

void integer_inc(integer **host, integer *b) {
  integer *tmp = integer_add(*host, b);
  *host = tmp;
}

integer *integer_sub(integer *a, integer *b) {
  binary_prep  /* define za, zb; declare z; begin arena alloc scope */
  mpz_init (z);  /* there is no mpz_init_sub() */
  mpz_sub (z, za, zb);  /* z := a - b */
  cvt_and_finish  /* end alloc scope, return z as INTEGER */
}

void integer_dec(integer **host, integer *b) {
  integer *tmp = integer_sub(*host, b);
  *host = tmp;
}

integer *integer_neg(integer *a) {
  unary_prep  /* define za; declare z; begin arena alloc scope */
  mpz_init (z);  /* there is no mpz_init_neg() */
  mpz_neg (z, za);  /* z := -a */
  cvt_and_finish  /* end alloc scope, return z as INTEGER */
}

integer *integer_mul(integer *a, integer *b) {
  binary_prep  /* define za, zb; declare z; begin arena alloc scope */
  mpz_init (z);  /* there is no mpz_init_mul() */
  mpz_mul (z, za, zb);  /* z := a * b */
  cvt_and_finish  /* end alloc scope, return z as INTEGER */
}

integer *integer_div(integer *a, integer *b) {
  binary_prep  /* define za, zb; declare z; begin arena alloc scope */
  mpz_init (z);  /* there is no mpz_init_tdiv_q() */
  mpz_tdiv_q (z, za, zb);  /* z := a DIV b */
  cvt_and_finish  /* end alloc scope, return z as INTEGER */
}

integer *integer_rem(integer *a, integer *b) {
  binary_prep  /* define za, zb; declare z; begin arena alloc scope */
  mpz_init (z);  /* there is no mpz_init_tdiv_r() */
  mpz_tdiv_r (z, za, zb);  /* z := a REM b */
  cvt_and_finish  /* end alloc scope, return z as INTEGER */
}

integer *integer_mod(integer *a, integer *b) {
  binary_prep  /* define za, zb; declare z; begin arena alloc scope */
  mpz_init (z);  /* there is no mpz_init_mod() */
  mpz_mod (z, za, zb);  /* z := a MOD b */
  cvt_and_finish  /* end alloc scope, return z as INTEGER */
}

integer *integer_pow(integer *a, ulong e) {
  unary_prep  /* define za; declare z; begin arena alloc scope */
  mpz_init (z);  /* there is no mpz_init_pow_ui() */
  mpz_pow_ui (z, za, e);  /* z := a**e, with 0**0 defined as 1 */
  cvt_and_finish  /* end alloc scope, return z as INTEGER */
}

integer *integer_abs(integer *a) {
  unary_prep  /* define za; declare z; begin arena alloc scope */
  mpz_init (z);  /* there is no mpz_init_abs() */
  mpz_abs (z, za);  /* z := ABS a */
  cvt_and_finish  /* end alloc scope, return z as INTEGER */
}

integer *integer_random(integer *a) {
  /*
   *  It is easier to do the sensible thing here and return an
   *  integer in the range 0 to a-1 than to implement the silly
   *  SETL definition of RANDOM which gives 0 to a inclusive.
   *  Callers can make adjustments as required (e.g., in lib.c).
   *
   *  It happens that mpz_urandomm() uses only the absolute value
   *  of a, and treats a=0 as a divide-by-zero error; but neither of
   *  those facts are explicitly stated in the basic user doc, so
   *  you are strictly speaking on your own if you call this function
   *  with anything but a positive a.
   */
  unary_prep  /* define za; declare z; begin arena alloc scope */
  mpz_init (z);  /* there is no mpz_init_randomm() */
  mpz_urandomm (z, randstate, za);  /* z := something in 0..a-1 */
  cvt_and_finish  /* end alloc scope, return z as INTEGER */
}

integer *integer_and(integer *a, integer *b) {
  binary_prep  /* define za, zb; declare z; begin arena alloc scope */
  mpz_init (z);  /* there is no mpz_init_and() */
  mpz_and (z, za, zb);  /* z := a BIT_AND b */
  cvt_and_finish  /* end alloc scope, return z as INTEGER */
}

integer *integer_com(integer *a) {
  unary_prep  /* define za; declare z; begin arena alloc scope */
  mpz_init (z);  /* there is no mpz_init_com() */
  mpz_com (z, za);  /* z := BIT_NOT a (1's complement) */
  cvt_and_finish  /* end alloc scope, return z as INTEGER */
}

integer *integer_or(integer *a, integer *b) {
  binary_prep  /* define za, zb; declare z; begin arena alloc scope */
  mpz_init (z);  /* there is no mpz_init_or() */
  mpz_ior (z, za, zb);  /* z := a BIT_OR b */
  cvt_and_finish  /* end alloc scope, return z as INTEGER */
}

integer *integer_xor(integer *a, integer *b) {
  binary_prep  /* define za, zb; declare z; begin arena alloc scope */
  mpz_init (z);  /* there is no mpz_init_xor() */
  mpz_xor (z, za, zb);  /* z := a BIT_XOR b */
  cvt_and_finish  /* end alloc scope, return z as INTEGER */
}

/*
 *  Convert a sec+usec spec (struct timeval) to milliseconds
 */
integer *integer_timeval_to_ms(struct timeval t) {
  new_prep  /* declare mpz_t z, begin arena allocation scope */
  mpz_init_set_ui (z, t.tv_sec);
  mpz_mul_ui (z, z, 1000);
  mpz_add_ui (z, z, t.tv_usec/1000);
  cvt_and_finish  /* end alloc scope, return z as INTEGER */
}

/*
 *  Convert non-negative milliseconds to a struct timeval (sec + usec)
 */
struct timeval integer_ms_to_timeval(integer *i, const char *what) {
  struct timeval t;
  assert (is_integer(i));
  if (i->size >= 0) {  /* i is non-negative */
    mpz_t q;           /* q will be i DIV 1000 */
    ulong r;           /* r will be i REM 1000 */
    ro_mpz(z, i)       /* const mpz_t z = alias of INTEGER i */
    mpz_init(q);       /* there is no mpz_init_tdiv_q_ui() */
    r = mpz_tdiv_q_ui (q, z, 1000);  /* [q,r] := i [DIV,REM] 1000 */
    /* Conservatively assume that only a signed long (in this instance
     * known to be non-negative) will fit in the time_t tv_sec field:  */
    if (mpz_fits_slong_p(q)) {
      t.tv_sec = mpz_get_si(q);  /* i DIV 1000 is the seconds */
      t.tv_usec = r * 1000;      /* remainder*1000 is the microseconds */
    } else {
      runerr("Too many milliseconds (more than %lu999) in %s",
                                          LONG_MAX,     what);
    }
    mpz_clear(q);       /* free q's limb space */                 \
  } else {  /* i < 0 */
    runerr("Negative number of milliseconds in %s", what);
  }
  return t;
}

integer *integer_timespec_to_ms(struct timespec t) {
  return integer_timeval_to_ms(timespec_to_timeval(t));
}

struct timespec integer_ms_to_timespec(integer *i, const char *what) {
  return timeval_to_timespec(integer_ms_to_timeval(i, what));
}


real *new_real(double d) {
  real *r;
  r = fblock(real);
  r->realval = d;
  return r;
}

double get_double(real *a, const char *what) {
  switch (setl_type(a)) {
  case real_type:
    return real_to_double(a);
  case integer_type:
    return integer_to_double((integer *)a);
  default: ;
  }
  runerr("%s must be numeric, not %s",
        what,             TYPENAME(a));
}


string *null_string(void) {  /* new empty string */
  string *r = alloc_string(0,0,1);  /* nchar, nbefore, nafter */
  strelt(r,1) = '\0';
  return r;
}

/*
 *  NOTE  -  Be very careful not to call new_string() or new_nstring()
 *  on the "guts" of a string on the heap, because your &strelt(...)
 *  pointer may very well be invalid by the time the attempt to copy
 *  your char array is made!
 */

string *new_string(const char *s) {
  string *r = alloc_string(strlen(s),0,1);  /* nchar, nbefore, nafter */
  strcpy(&strelt(r,1),s);  /* includes trailing '\0' */
  return r;
}

string *new_nstring(const char *s, long n) {
  string *r = alloc_string(n,0,1);  /* nchar, nbefore, nafter */
  mvmem(&strelt(r,1), s, n);
  strelt(r,n+1) = '\0';
  return r;
}

string *new_estring(long n) {
  string *r = alloc_string(n,0,1);  /* nchar, nbefore, nafter */
  strelt(r,n+1) = '\0';
  return r;
}

string *new_cstring(char c) {
  string *r = alloc_string(1,0,1);  /* nchar, nbefore, nafter */
  strelt(r,1) = c;
  strelt(r,2) = '\0';
  return r;
}

string *str_lower(string *s) {
  string *r = copy_string(s);
  long i, n = r->nchar;
  char *t = &strelt(r,1);
  for (i=0; i<n; i++) t[i] = to_lower[(uchar)t[i]];
  return r;
}

string *str_upper(string *s) {
  string *r = copy_string(s);
  long i, n = r->nchar;
  char *t = &strelt(r,1);
  for (i=0; i<n; i++) t[i] = to_upper[(uchar)t[i]];
  return r;
}

string *copy_substring(string *s, long i, long j) {
  HANDLE h = ref(s);
  string *r;
  long n, m;
  assert (is_string(s));
  assert (i >= 1);
  assert (j >= i-1);
  n = s->nchar;
  if (i > n+1) i = n+1;
  if (j > n) j = n;
  m = (j-i)+1;
  r = new_estring(m);  /* includes \0 at offset m */
  mvmem (&strelt(r,1), &strelt(s,i), m);
  retire(h);
  return r;
}

string *str_lpad(string *s, long n, int c) {
  HANDLE hs = ref(s);
  long ns = s->nchar;
  long nr = MAX(n, ns);
  string *r = new_estring(nr);
  assert (n >= 0);
  n -= ns;
  if (n < 0) n = 0;
  else if (n > 0) memset (&strelt(r,1), c, n);
  mvmem (&strelt(r,1+n), &strelt(s,1), ns);
  retire(hs);
  return r;
}

string *str_rpad(string *s, long n, int c) {
  HANDLE hs = ref(s);
  long ns = s->nchar;
  long nr = MAX(n, ns);
  string *r = new_estring(nr);
  assert (n >= 0);
  mvmem (&strelt(r,1), &strelt(s,1), ns);
  n -= ns;
  if (n > 0) memset (&strelt(r,ns+1), c, n);
  retire(hs);
  return r;
}

/*
 *  Produce a "pretty" string (see comments above 'l_pretty')
 */
string *str_dress(string *a) {
  HANDLE h = ref(a);
  string *r = new_cstring('\'');
  long i;
  assert (is_string(a));
  for (i=1; i<=a->nchar; i++) {
    char c = strelt(a,i);
    if (musttwin[(uchar)(c)]) {
      str_tackon(&r,c);
      str_tackon(&r,c);
    } else if (ctrlchar[(uchar)(c)]) {
      switch (c) {
      case '\a':  str_append(&r,"\\a");  break;
      case '\b':  str_append(&r,"\\b");  break;
      case '\f':  str_append(&r,"\\f");  break;
      case '\n':  str_append(&r,"\\n");  break;
      case '\r':  str_append(&r,"\\r");  break;
      case '\t':  str_append(&r,"\\t");  break;
      case '\v':  str_append(&r,"\\v");  break;
      default:  unexpected(c);
      }
    } else if (strok[(uchar)(c)]) {
      str_tackon(&r,c);
    } else {
      const char *octdigs = "01234567";
      str_tackon(&r,'\\');
#if CHAR_BIT == 8
      str_tackon(&r,octdigs[((uchar)(c)>>6)&3]);
      str_tackon(&r,octdigs[((uchar)(c)>>3)&7]);
      str_tackon(&r,octdigs[((uchar)(c)>>0)&7]);
#else
#error Please contribute to-octal code for CHAR_BIT-wide chars.
#endif
    }
  }
  str_tackon(&r,'\'');
  retire(h);
  return r;
} /* end str_dress */

/*
 *  Convert a "pretty" string to unrestricted form (see comments
 *  above 'l_unpretty')
 *
 *  The 2nd arg is a pointer to a function that prints a message and
 *  terminates the program, like tranerr or runerr.
 */
string *str_undress(string *s,
                     void (*err)(const char *msg, ...) NOISY_QUITTER) {
  HANDLE h = ref(s);
  long i, n = s->nchar;
  string *r = null_string();
  char q;
  assert (is_string(s));
  q = strelt(s,1);
  if ((q != '\'' && q != '\"') || strelt(s,n) != q) {
    err("\"Pretty\" string (%s) not enclosed in single or double quotes",
                tame(&strelt(s,1)));
  }
  for (i=2; i<=n-1; i++) {
    char c = strelt(s,i), d, e;
    if (!strok[(uchar)c]) {
      err("Invalid character (hex %02x) in \"pretty\" string %s",
                              (uchar)c,          tame(&strelt(s,1)));
    }
    if (c == '\\') {
      i++;
      c = strelt(s,i);
      switch (escapee[(uchar)c]) {
      case unescapable:
        err("Invalid character (hex %02x) after backslash in"
            " \"pretty\" string %s", (uchar)c, tame(&strelt(s,1)));
        break;
      case repself:
        str_tackon(&r,c);
        break;
      case control:
        switch (c) {
        case 'a':  str_tackon(&r,'\a');  break;
        case 'b':  str_tackon(&r,'\b');  break;
        case 'f':  str_tackon(&r,'\f');  break;
        case 'n':  str_tackon(&r,'\n');  break;
        case 'r':  str_tackon(&r,'\r');  break;
        case 't':  str_tackon(&r,'\t');  break;
        case 'v':  str_tackon(&r,'\v');  break;
        default:  unexpected(c);
        }
        break;
      case hexlead:
        i++;
        c = strelt(s,i);
        if (!hexdigit[(uchar)c]) {
          err("Invalid hexadecimal escape in \"pretty\" string %s",
                                                   tame(&strelt(s,1)));
        }
        i++;
        d = strelt(s,i);
        if (!hexdigit[(uchar)d]) {
          i--;
          str_tackon(&r,(hexval[(uchar)c]));
        } else {
#if CHAR_BIT == 8
          str_tackon(&r,(hexval[(uchar)c]<<4)+
                        (hexval[(uchar)d]   ));
#else
#error Please provide from-hex conversion code for CHAR_BIT-wide chars.
#endif
        }
        break;
      case octlead:
        i++;
        d = strelt(s,i);
        if (!octdigit[(uchar)d]) {
          i--;
          str_tackon(&r,octval[(uchar)c]);
        } else {
          i++;
          e = strelt(s,i);
          if (!octdigit[(uchar)e]) {
            i--;
            str_tackon(&r,(octval[(uchar)c]<<3)+
                          (octval[(uchar)d]   ));
          } else {
#if CHAR_BIT == 8
            str_tackon(&r,(octval[(uchar)c]<<6)+
                          (octval[(uchar)d]<<3)+
                          (octval[(uchar)e]   ));
#else
#error Please supply from-octal conversion code for CHAR_BIT-wide chars.
#endif
          }
        }
        break;
      default:
        unexpected (escapee[(uchar)c]);
      }
    } else if (c == q) {
      i++;
      c = strelt(s,i);
      if (c == q && i <= n-1) {
        str_tackon(&r,q);
      } else {
        err("Stray quote in \"pretty\" string (%s)",
                                   tame(&strelt(s,1)));
      }
    } else {
      str_tackon(&r,c);
    }
  }
  retire(h);
  return r;
} /* end str_undress */

string *str_join(string *a, string *b) {
  string *r;
  HANDLE ha = ref(a);
  HANDLE hb = ref(b);
  long na,nb,n;
  assert (is_string(a));
  assert (is_string(b));
  na = a->nchar;
  nb = b->nchar;
  n = na + nb;
  r = alloc_string(n,0,1);
  mvmem (&strelt(r,1), &strelt(a,1), na);
  mvmem (&strelt(r,na+1), &strelt(b,1), nb);
  strelt(r,n+1) = '\0';
  retire(hb);
  retire(ha);
  return r;
} /* end str_join */

/*
 *  This routine exposes arbitrary data on expansion; its callers
 *  should be sure to fill in the exposed region immediately:
 */
void str_resize(string **host, long size) {
  string *t = *host;
  long oldsize;
  assert (is_string(t));
  assert (size >= 0);
  oldsize = t->nchar;
  if (size >= oldsize+t->nafter) {
    HANDLE h = ref(t);
    string *r = alloc_string(size,0,oldsize+10);  /* nchar, 0, nafter */
    mvmem (&strelt(r,1), &strelt(t,1), oldsize);
    strelt(r,size+1) = '\0';  /* leaving garbage from oldsize+1 to size */
    retire(h);
    *host = r;
  } else {
    t->nafter -= size-oldsize;
    t->nchar = size;
    assert (t->nafter > 0);
    strelt(t,size+1) = '\0';
  }
} /* end str_resize */

void str_tackon(string **host, char c) {
  string *t = *host;
  long newsize;
  assert (is_string(t));
  newsize = t->nchar + 1;
  str_resize(&t,newsize);
  strelt(t,newsize) = c;
  *host = t;
}

void str_append(string **host, const char *s) {
  string *t = *host;
  long oldsize, newsize;
  assert (is_string(t));
  oldsize = t->nchar;
  newsize = oldsize + strlen(s);
  str_resize(&t,newsize);
  strcpy(&strelt(t,oldsize+1),s);
  *host = t;
}

void str_concat(string **host, string *s) {
  string *t = *host;
  HANDLE hs;
  long tsize, ssize;
  assert (is_string(t));
  assert (is_string(s));
  tsize = t->nchar;
  ssize = s->nchar;
  hs = ref(s);
  /*
   *  Even if s is t (an alias) at this point, resizing it should
   *  do no harm, since in that case it is to be clobbered anyway:
   */
  str_resize(&t,tsize+ssize);
  retire(hs);
  mvmem (&strelt(t,tsize+1), &strelt(s,1), ssize);
  *host = t;
}

void str_concat_substring(string **host, string *s, long i, long j) {
  string *t = *host;
  HANDLE hs = ref(s);
  long tsize, ssize, subsize;
  assert (is_string(t));
  assert (is_string(s));
  assert (i >= 1);
  assert (j >= i-1);
  ssize = s->nchar;
  if (i > ssize+1) i = ssize+1;
  if (j > ssize) j = ssize;
  subsize = (j-i)+1;
  tsize = t->nchar;
  str_resize(&t,tsize+subsize);
  retire(hs);
  mvmem (&strelt(t,tsize+1), &strelt(s,i), subsize);
  *host = t;
}

/* See comments above tup_insert, on which this was originally based:  */

void str_insert(string **host, long i, long j, string *x) {
  string *b = *host;
  HANDLE hb,hx;
  long m,n,nx;
  assert (is_string(b));
  assert (is_string(x));
  assert (i >= 1);
  assert (j >= i-1);
  n = b->nchar;
  if (i > n) j = i-1;
  m = j-i+1;  /* length of substring to be replaced */
  nx = x->nchar;
  hb = ref(b);
  hx = ref(x);
  if (nx == m && j <= n) {
    /* Straight overlaying */
    mvmem (&strelt(b,i), &strelt(x,1), m);
  } else if (j > n) {
    /* Replace an imaginary region beyond the end of the string */
    str_resize(&b,i+nx-1);
    if (i > n+1) memset (&strelt(b,n+1), ' ', i-(n+1));
    mvmem (&strelt(b,i), &strelt(x,1), nx);
  } else if (b != x) {
    /* A length-changing slice; try to let the big end stay still */
    long growth = nx - m;  /* can be negative */
    if (i-1 >= n-j) {
      /* The left end is favoured to stay put */
      string *a = new_estring(n-j);
      mvmem (&strelt(a,1), &strelt(b,j+1), n-j);  /* copy of the tail */
      if (growth >= b->nafter) {
        /* Insufficient room in the right end; make new string */
        HANDLE h = ref(a);
        string *t = alloc_string (n+growth, b->nbefore, n+10);
        retire(h);
        mvmem (&strelt(t,1), &strelt(b,1), i-1);  /* move head part */
        b = t;
      } else {
        /* String has enough room even after substring replacement */
        b->nafter -= growth;
        b->nchar += growth;
      }
      mvmem (&strelt(b,i), &strelt(x,1), nx);  /* copy replacement in */
      mvmem (&strelt(b,i+nx), &strelt(a,1), n-j);  /* copy tail back in */
      strelt(b,b->nchar+1) = '\0';
    } else {
      /* The right end wants to remain stationary */
      string *a = new_estring(i-1);
      mvmem (&strelt(a,1), &strelt(b,1), i-1);  /* copy head part */
      if (growth > b->nbefore) {
        /* Insufficient room in the left end; make a new string */
        HANDLE h = ref(a);
        string *t = alloc_string (n+growth, n+10, b->nafter);
        retire(h);
        mvmem (&strelt(t,j+1+growth), &strelt(b,j+1), n-j);  /* mv tail */
        strelt(t,t->nchar+1) = '\0';
        b = t;
      } else {
        /* String has enough room even after substring replacement */
        b->nbefore -= growth;
        b->nchar += growth;
      }
      mvmem (&strelt(b,1), &strelt(a,1), i-1);  /* copy head back in */
      mvmem (&strelt(b,i), &strelt(x,1), nx);  /* copy replacement in */
    }
  } else {
    /* b is x, so build a new b with an embedded copy of its old self */
    string *t = new_estring(n+nx-m);  HANDLE h = ref(t);
    mvmem (&strelt(t,1), &strelt(b,1), i-1);  /* initial part */
    mvmem (&strelt(t,i), &strelt(x,1), nx);  /* x, or the old b */
    mvmem (&strelt(t,i+nx), &strelt(b,j+1), n-j);  /* tail part */
    retire(h);
    b = t;
  }
  retire(hx);
  retire(hb);
  *host = b;
} /* end str_insert */


set *null_set(void) {
  set *r;
  table *t;  HANDLE ht;
  patcre(&t);
  ht = ref(t);
  r = fblock(set);
  r->stype = smap;
  r->card = 0;
  r->tblval = t;
  retire(ht);
  return r;
}

set *singleton(block *p) {
  HANDLE h = ref(p);
  set *r = null_set();
  set_insert(&r,p);
  assert (r->card == 1);
  retire(h);
  return r;
}

bool in_set(set *host, block *x) {
  HANDLE h = ref(host);
  bool r;
  assert (is_set(host));
  assert (x != OM);
  switch (host->stype) {
  case plain_set:
    {
      key *k = tokey(x);
      r = has_key(host->tblval, k);
    }
    break;
  case mmap:
    if (is_tuple(x)) {
      tuple *t = (tuple *)x;
      if (t->nelt == 2 && tupelt(t,1) != OM) {
        HANDLE ht = ref(t);
        key *k = tokey(tupelt(t,1));
        subnode *b;
        if (keysee(host->tblval, k, &b)) {
          set *s = (set *)(b->d);
          assert (is_set(s));
          r = in_set(s, tupelt(t,2));
        } else {
          r = false;
        }
        retire(ht);
      } else {
        r = false;
      }
    } else {
      r = false;
    }
    break;
  case smap:
    if (is_tuple(x)) {
      tuple *t = (tuple *)x;
      if (t->nelt == 2 && tupelt(t,1) != OM) {
        HANDLE ht = ref(t);
        key *k = tokey(tupelt(t,1));
        subnode *b;
        if (keysee(host->tblval, k, &b)) {
          r = equal_value(b->d, tupelt(t,2));
        } else {
          r = false;
        }
        retire(ht);
      } else {
        r = false;
      }
    } else {
      r = false;
    }
    break;
  default:
    unexpected (host->stype);
  }
  retire(h);
  return r;
} /* end in_set */

void set_insert(set **host, block *x) {
  set *r;       HANDLE hr;
  if (x == OM) return;       /* formerly an error */
  r = *host;    hr = ref(r);
  assert (is_set(r));
  switch (r->stype) {
  case plain_set:
    {
      key *k = tokey(x);
      subnode *b;
      if (!keysub(r->tblval, k, &b)) {
        b->d = NULL;
        r->card++;
      }
    }
    break;
  case mmap:
    if (is_tuple(x)) {
      tuple *t = (tuple *)x;
      if (t->nelt == 2 && tupelt(t,1) != OM) {
        HANDLE ht = ref(t);
        key *k = tokey(tupelt(t,1));
        subnode *b = NULL;  HANDLE hb = ref(b);
        if (!keysub(r->tblval, k, &b)) {
          set *s = singleton(tupelt(t,2));
          r->card++;
          b->d = (block *)s;
        } else {
          set *s = (set *)(b->d);
          long n = s->card;
          set_insert(&s, tupelt(t,2));
          r->card += (s->card - n);     /* 1 or 0 */
          b->d = (block *)s;
        }
        retire(hb);
        retire(ht);
      } else {  /* x is a tuple, but not a valid map element */
        HANDLE hx = ref(x);
        demote(r, plain_set);
        set_insert(&r, x);
        retire(hx);
      }
    } else {  /* x is not even a tuple */
      HANDLE hx = ref(x);
      demote(r, plain_set);
      set_insert(&r, x);
      retire(hx);
    }
    break;
  case smap:
    if (is_tuple(x)) {
      tuple *t = (tuple *)x;
      if (t->nelt == 2 && tupelt(t,1) != OM) {
        HANDLE ht = ref(t);
        key *k = tokey(tupelt(t,1));
        subnode *b = NULL;  HANDLE hb = ref(b);
        if (!keysub(r->tblval, k, &b)) {
          block *s = copy_value(tupelt(t,2));
          r->card++;
          b->d = s;
        } else {
          if (!equal_value(tupelt(t,2), b->d)) {
            demote(r, mmap);
            set_insert(&r, (block *)t);
          }
        }
        retire(hb);
        retire(ht);
      } else {  /* x is a tuple, but not a valid map element */
        HANDLE hx = ref(x);
        demote(r, plain_set);
        set_insert(&r, x);
        retire(hx);
      }
    } else {  /* x is not even a tuple */
      HANDLE hx = ref(x);
      demote(r, plain_set);
      set_insert(&r, x);
      retire(hx);
    }
    break;
  default:
    unexpected (r->stype);
  }
  retire(hr);
  *host = r;
} /* end set_insert */

void set_delete(set **host, block *x) {
  set *r = *host;  HANDLE hr = ref(r);
  assert (is_set(r));
  assert (x != OM);
  switch (r->stype) {
  case plain_set:
    {
      key *k = tokey(x);
      if (keydel(r->tblval, k)) r->card--;
    }
    break;
  case mmap:
    if (is_tuple(x)) {
      tuple *t = (tuple *)x;
      if (t->nelt == 2 && tupelt(t,1) != OM) {
        HANDLE ht = ref(t);
        key *k = tokey(tupelt(t,1));  HANDLE hk = ref(k);
        subnode *b = NULL;  HANDLE hb = ref(b);
        if (keysee(r->tblval, k, &b)) {
          set *s = (set *)(b->d);
          long n = s->card;
          set_delete(&s, tupelt(t,2));
          r->card -= (n - s->card);     /* 1 or 0 */
          if (s->card == 0) {
            check (keydel(r->tblval, k));
          } else {
            b->d = (block *)s;
          }
        }
        retire(hb);
        retire(hk);
        retire(ht);
      }
    }
    break;
  case smap:
    if (is_tuple(x)) {
      tuple *t = (tuple *)x;
      if (t->nelt == 2 && tupelt(t,1) != OM) {
        HANDLE ht = ref(t);
        key *k = tokey(tupelt(t,1));  HANDLE hk = ref(k);
        subnode *b;
        if (keysee(r->tblval, k, &b)) {
          if (equal_value(tupelt(t,2), b->d)) {
            check (keydel(r->tblval, k));
            r->card--;
          }
        }
        retire(hk);
        retire(ht);
      }
    }
    break;
  default:
    unexpected (r->stype);
  }
  retire(hr);
  *host = r;
} /* end set_delete */

block *set_from(set **host) {
  set *s = *host;  HANDLE hs;
  block *r = NULL;  HANDLE hr;
  assert (is_set(s));
  if (s->card == 0) return OM;
  hs = ref(s);
  hr = ref(r);
  switch (s->stype) {
  case plain_set:
    {
      subnode *b;
      check (ordsee(s->tblval, 1, &b));
      r = unkey(b->k);
      check (orddel(s->tblval, 1));
      s->card--;
    }
    break;
  case mmap:
    {
      subnode *b = NULL;  HANDLE hc = ref(b);
      tuple *t = new_tuple(2);  HANDLE ht = ref(t);
      set *u;  HANDLE hu;
      check (ordsee(s->tblval, 1, &b));
      let (tupelt(t,1), unkey(b->k));
      u = (set *)b->d;  hu = ref(u);
      assert (is_set(u));
      let (tupelt(t,2), set_from(&u));
      s->card--;
      if (u->card == 0) {
        check (orddel(s->tblval, 1));
      } else {
        b->d = (block *)u;
      }
      r = (block *)t;
      retire(hu);
      retire(ht);
      retire(hc);
    }
    break;
  case smap:
    {
      subnode *b = NULL;  HANDLE hc = ref(b);
      tuple *t = new_tuple(2);  HANDLE ht = ref(t);
      check (ordsee(s->tblval, 1, &b));
      let (tupelt(t,1), unkey(b->k));
      let (tupelt(t,2), copy_value(b->d));
      check (orddel(s->tblval, 1));
      s->card--;
      r = (block *)t;
      retire(ht);
      retire(hc);
    }
    break;
  default:
    unexpected (s->stype);
  }
  retire(hr);
  retire(hs);
  *host = s;
  return r;
} /* end set_from */


/* Try to give a set sufficient strength */
bool promote(set *s, settype strength) {
  assert (is_set(s));
  if (s->stype >= strength) return true;
  {
    /*
     *  This could be more efficient than it is, especially for the
     *  mmap-to-smap case, but that case shouldn't be needed often.
     */
    HANDLE hs = ref(s);
    iterator *i = init_iterator((block *)s);  HANDLE hi = ref(i);
    set *t = null_set();        HANDLE ht = ref(t);
    block *b;
    while (step_iterator(&i,&b)) set_insert(&t,b);
    retire(ht);
    retire(hi);
    retire(hs);
    *s = *t;    /* copy the set header */
    return s->stype >= strength;
  }
} /* end promote */

/* Give a set sufficient weakness */
void demote(set *s, settype strength) {
  HANDLE hs;
  set *t;  HANDLE ht;
  set *x;  HANDLE hx;
  tuple *u;  HANDLE hu;
  subnode *b;  HANDLE hb;
  long i,j,n;
  /*
   *  The conversions to plain set here would be somewhat more
   *  efficient if instead of converting (key,assoc) pairs to
   *  2-tuples and then converting those back to keys, we just
   *  did something more like tokey does---convert the associate
   *  to a key and then, given the lengths of the both the key
   *  and the associate key, make a new key by concatenating them.
   *  This would speed things up especially for the mmap-to-plain_set
   *  case, because there you get associate keys rather immediately
   *  as a side-effect of demoting each range subset to a plain set.
   *
   *  The conversions to plain set here are also done a little
   *  dangerously---there's an implicit assumption that set_insert
   *  will not be clever and promote the empty set!
   */
  assert (is_set(s));
  if ((int)(s->stype) <= (int)strength) return;
  hs = ref(s);
  n = patsiz(s->tblval);
  switch (s->stype) {
  case mmap:
    assert (strength == plain_set);
    t = null_set();  ht = ref(t);
    t->stype = plain_set;
    u = new_tuple(2);  hu = ref(u);
    b = NULL;  hb = ref(b);
    x = NULL;  hx = ref(x);
    for (i=1; i<=n; i++) {
      check (ordsee(s->tblval, i, &b));
      let (tupelt(u,1), unkey(b->k));
      x = (set *)(b->d);
      assert (is_set(x));
      demote (x, plain_set);
      for (j=1; j<=x->card; j++) {
        check (ordsee(x->tblval, j, &b));
        let (tupelt(u,2), unkey(b->k));
        set_insert (&t, (block *)u);
      }
    }
    retire(hx);
    retire(hb);
    retire(hu);
    retire(ht);
    *s = *t;    /* copy set header */
    assert (s->stype == plain_set);
    break;
  case smap:
    assert (n == s->card);
    switch (strength) {
    case plain_set:
      t = null_set();  ht = ref(t);
      t->stype = plain_set;
      u = new_tuple(2);  hu = ref(u);
      b = NULL;  hb = ref(b);
      for (i=1; i<=n; i++) {
        check (ordsee(s->tblval, i, &b));
        let (tupelt(u,1), unkey(b->k));
        let (tupelt(u,2), b->d);  /* this transient alias is OK */
        set_insert (&t, (block *)u);
      }
      retire(hb);
      retire(hu);
      retire(ht);
      *s = *t;  /* copy set header */
      assert (s->stype == plain_set);
      break;
    case mmap:
      b = NULL;  hb = ref(b);
      for (i=1; i<=n; i++) {
        check (ordsee(s->tblval, i, &b));
        let (b->d, (block *)singleton(b->d));
      }
      s->stype = mmap;
      retire(hb);
      break;
    default:
      unexpected (strength);
    }
    break;
  default:
    unexpected (s->stype);
  }
  retire(hs);
} /* end demote */

/*
 *  The remaining set utilities can do everything they need using
 *  the low-level ones already defined---no more grubbing with fields
 *  of the set representation block past here, for example.
 */

/* P. 120 f. is the primary reference for the following:  */
set *set_fl(integer *first, integer *last) {
  long i,f,l;
  set *r;  HANDLE hr;
  f = get_long(first, "F in {F..L}");
  l = get_long(last, "L in {F..L}");
  r = null_set();  hr = ref(r);
  for (i=f; i<=l; i++) {
    integer *j = new_integer(i);
    set_insert(&r,(block *)j);
  }
  retire(hr);
  return r;
} /* end set_fl */

set *set_fnl(integer *first, integer *next, integer *last) {
  long i,f,n,l,step;
  set *r;  HANDLE hr;
  f = get_long(first, "F in {F,N..L}");
  n = get_long(next, "N in {F,N..L}");
  l = get_long(last, "L in {F,N..L}");
  r = null_set();  hr = ref(r);
  step = n-f;
  if (step > 0) {
    for (i=f; i<=l; i+=step) {
      integer *j = new_integer(i);
      set_insert(&r,(block *)j);
    }
  } else if (step == 0) {
    /* by convention, empty set is returned */
  } else { /* step < 0 */
    for (i=f; i>=l; i+=step) {
      integer *j = new_integer(i);
      set_insert(&r,(block *)j);
    }
  }
  retire(hr);
  return r;
} /* end set_fnl */

void set_extend(set **host, set *b) {
  set *s = *host;       HANDLE hs = ref(s);
  iterator *i = NULL;   HANDLE hi = ref(i);
  block *x;
  assert (is_set(s));
  assert (is_set(b));
  i = init_iterator((block *)b);
  while (step_iterator(&i,&x)) set_insert(&s,x);
  retire(hi);
  retire(hs);
  *host = s;
} /* end set_extend */

void set_reduce(set **host, set *b) {
  set *s = *host;       HANDLE hs = ref(s);
  iterator *i = NULL;   HANDLE hi = ref(i);
  block *x;
  assert (is_set(s));
  assert (is_set(b));
  i = init_iterator((block *)b);
  while (step_iterator(&i,&x)) set_delete(&s,x);
  retire(hi);
  retire(hs);
  *host = s;
} /* end set_reduce */

set *set_union(set *a, set *b) {
  HANDLE ha = ref(a);
  HANDLE hb = ref(b);
  set *r = null_set();  HANDLE hr = ref(r);
  iterator *i = NULL;  HANDLE hi = ref(i);
  block *x;
  assert (is_set(a));
  assert (is_set(b));
  i = init_iterator((block *)a);
  while (step_iterator(&i,&x)) set_insert(&r,x);
  i = init_iterator((block *)b);
  while (step_iterator(&i,&x)) set_insert(&r,x);
  retire(hi);
  retire(hr);
  retire(hb);
  retire(ha);
  return r;
} /* end set_union */

set *set_intersection(set *a, set *b) {
  HANDLE ha = ref(a);
  HANDLE hb = ref(b);
  set *r = null_set();  HANDLE hr = ref(r);
  iterator *i = NULL;  HANDLE hi = ref(i);
  block *x = OM;  HANDLE hx = ref(x);
  assert (is_set(a));
  assert (is_set(b));
  i = init_iterator((block *)a);
  while (step_iterator(&i,&x)) if (in_set(b,x)) set_insert(&r,x);
  retire(hx);
  retire(hi);
  retire(hr);
  retire(hb);
  retire(ha);
  return r;
} /* end set_intersection */

set *set_difference(set *a, set *b) {
  HANDLE ha = ref(a);
  HANDLE hb = ref(b);
  set *r = null_set();  HANDLE hr = ref(r);
  iterator *i = NULL;  HANDLE hi = ref(i);
  block *x = OM;  HANDLE hx = ref(x);
  assert (is_set(a));
  assert (is_set(b));
  i = init_iterator((block *)a);
  while (step_iterator(&i,&x)) if (!in_set(b,x)) set_insert(&r,x);
  retire(hx);
  retire(hi);
  retire(hr);
  retire(hb);
  retire(ha);
  return r;
} /* end set_difference */


tuple *null_tuple(void) {
  return alloc_tuple(0,0,0);  /* nelt, npre, nsuf */
}

tuple *new_tuple(long size) {
  return alloc_tuple(size,0,0);  /* nelt, npre, nsuf */
}

tuple *tup_join(tuple *a, tuple *b) {
  tuple *r;
  HANDLE ha = ref(a);
  HANDLE hb = ref(b);
  long na,nb,n;
  long i;
  assert (is_tuple(a));
  assert (is_tuple(b));
  na = a->nelt;
  nb = b->nelt;
  n = na + nb;
  r = alloc_tuple(n,0,0);
  for (i=1; i<=na; i++) tupelt(r,i) = tupelt(a,i);
  for (i=1; i<=nb; i++) tupelt(r,i+na) = tupelt(b,i);
  retire(hb);
  retire(ha);
  return r;
} /* end tup_join */

/*
 *  Note that the operations on tuples at this level do not and
 *  should not automatically "truncate" (clip the trailing OMs from)
 *  tuples.
 *
 *  For example, the 'extend_tuple' opcode uses 'tup_tackon' to
 *  build up (possibly holey) tuples.  The tuple should only be
 *  truncated after it is completed.
 */

void tup_resize(tuple **host, long size) {
  long oldsize, i;
  tuple *t = *host;
  assert (is_tuple(t));
  assert (size >= 0);
  oldsize = t->nelt;
  if (size > oldsize+t->nsuf) {
    HANDLE h = ref(t);
    tuple *r = alloc_tuple(size,0,oldsize+10);  /* nelt, npre, nsuf */
    for (i=1; i<=oldsize; i++) tupelt(r,i) = tupelt(t,i);  /* ptr cpy */
    retire(h);
    *host = r;
  } else {
    t->nsuf -= size-oldsize;
    t->nelt = size;
    /* Clear ptrs on expansion rather than on contraction:  */
    for (i=oldsize+1; i<=size; i++) tupelt(t,i) = OM;
  }
} /* end tup_resize */

void tup_tackon(tuple **host, block *x) {
  long newsize;
  HANDLE hx = ref(x);
  tuple *t = *host;  HANDLE ht = ref(t);
  assert (is_tuple(t));
  newsize = t->nelt + 1;
  if ((block *)t == x) {  /* tack on a copy of t itself */
    block *y = copy_value(x);  HANDLE hy = ref(y);
    tup_resize(&t,newsize);
    tupelt(t,newsize) = y;
    retire(hy);
  } else {
    tup_resize(&t,newsize);
    let (tupelt(t,newsize), copy_value(x));
  }
  retire(ht);
  retire(hx);
  *host = t;
}

void tup_concat(tuple **host, tuple *p) {
  long i,j,n;
  HANDLE hp = ref(p);
  tuple *t = *host;  HANDLE ht = ref(t);
  assert (is_tuple(t));
  assert (is_tuple(p));
  j = t->nelt;
  n = p->nelt;
  tup_resize(&t,j+n);
  /* If t and p are aliases, it's OK here, but "just barely"!  */
  for (i=1; i<=n; i++) {
    ++j;
    let (tupelt(t,j), copy_value(tupelt(p,i)));
  }
  retire(ht);
  retire(hp);
  *host = t;
}

/*
 *  Slice insertion is efficient, even if you are using it to build up
 *  your tuple at or near both ends, because even if the tuple needs to
 *  be grown expensively (which happens about log2 n times in n), I
 *  preserve at least the amount of padding you had on the other end.
 *
 *  The idea is not to stand in the way of using tuples for
 *  bidirectional queues.
 *
 *  This all goes for strings too.
 *
 *  Garbage collection removes all padding, so that this doesn't get
 *  too far out of hand.
 *
 *  Although the time for building up an n-element tuple or string on
 *  either or both ends is O(n) after you amortize over the O(log2 n)
 *  doublings, it is interesting to note that tup_tackon and tup_resize
 *  don't trouble to preserve front-end padding (unlike their smooth
 *  slicing counterparts), so if you have the customary implausible
 *  Bernoulli trials choosing whether to do "t with:= e" (append on
 *  right) or "t(1..0) := [e]" (insert slice on left), you should still
 *  get O(n) time but perhaps a factor of 2 slower than if it were
 *  "t(#t+1..) := [e]" versus that "t(1..0) := [e]" (i.e. doing it all
 *  with slices)!  Of course I haven't actually troubled to try this;
 *  other overheads would pollute the measured differences anyway.
 */

void tup_insert(tuple **host, long i, long j, tuple *x) {
  tuple *b = *host;
  HANDLE hb,hx;
  long m,n,nx,p;
  assert (is_tuple(b));
  assert (is_tuple(x));
  assert (i >= 1);
  assert (j >= i-1);
  n = b->nelt;
  if (i > n) j = i-1;
  m = j-i+1;
  nx = x->nelt;
  hb = ref(b);
  hx = ref(x);
  if (nx == m && j <= n) {
    /* Straight overlaying of appropriate elements */
    for (p=1; p<=m; p++,i++) let (tupelt(b,i), copy_value(tupelt(x,p)));
  } else if (j > n) {
    /* Replace an imaginary region beyond the end of the tuple */
    tup_resize(&b,i+nx-1);
    for (p=1; p<=nx; p++,i++) let (tupelt(b,i), copy_value(tupelt(x,p)));
  } else if (b != x) {
    /* An inner slice, let the big end stay still */
    long growth = nx - m;  /* can be negative */
    /*
     *  We avoid endian ambushes by copying the moving
     *  part to a temporary temporarily rather than trying
     *  to be clever about l-to-r or r-to-l copying order
     *  (although perhaps the rules of C make that unnecessary);
     *  note that we only shunt pointers around here.
     */
    if (i-1 >= n-j) {
      /* The left end is favoured to stay put */
      tuple *a = new_tuple(n-j);  HANDLE h = ref(a);
      for (p=1; p<=n-j; p++) tupelt(a,p) = tupelt(b,j+p);
      /* tup_resize(&b,n+growth);  -- the equivalent, more or less */
      if (growth > b->nsuf) {
        long nelt = n + growth;  /* i.e., b->nelt + growth */
        long npre = b->npre;
        long nsuf = n + 10;  /* i.e., b->nelt + 10 */
        tuple *t = alloc_tuple(nelt,npre,nsuf);
        for (p=1; p<i; p++) tupelt(t,p) = tupelt(b,p);
        b = t;
      } else {
        b->nsuf -= growth;
        b->nelt += growth;
        /* Clear ptrs on expansion rather than on contraction:  */
        for (p=n+1; p<=b->nelt; p++) tupelt(b,p) = OM;
      }
      for (p=1; p<=nx; p++,i++) let (tupelt(b,i), copy_value(tupelt(x,p)));
      for (p=1; p<=n-j; p++,i++) tupelt(b,i) = tupelt(a,p);
      retire(h);
    } else {
      /* The right end wants to remain stationary */
      tuple *a = new_tuple(i-1);  HANDLE h = ref(a);
      for (p=1; p<i; p++) tupelt(a,p) = tupelt(b,p);
      if (growth > b->npre) {
        long nelt = n + growth;  /* i.e., b->nelt + growth */
        long npre = n + 10;  /* i.e., b->nelt + 10 */
        long nsuf = b->nsuf;
        tuple *t = alloc_tuple(nelt,npre,nsuf);
        for (p=j+1; p<=n; p++) tupelt(t,p+growth) = tupelt(b,p);
        b = t;
      } else {
        b->npre -= growth;
        b->nelt += growth;
        /* Clear ptrs on expansion rather than on contraction:  */
        for (p=i; p<=growth; p++) tupelt(b,p) = OM;
      }
      for (p=1; p<i; p++) tupelt(b,p) = tupelt(a,p);
      for (p=1; p<=nx; p++,i++) let (tupelt(b,i), copy_value(tupelt(x,p)));
      retire(h);
    }
  } else {
    /* b is x, so build a new b with the old b (x) embedded in it */
    tuple *t = new_tuple(n+nx-m);  HANDLE h = ref(t);
    for (p=1; p<i; p++) tupelt(t,p) = tupelt(b,p);
    for (p=1; p<=nx; p++,i++) let (tupelt(t,i), copy_value(tupelt(x,p)));
    for (p=j+1; p<=n; p++,i++) tupelt(t,i) = tupelt(b,p);
    retire(h);
    b = t;
  }
  tup_truncate(&b);
  retire(hx);
  retire(hb);
  *host = b;
} /* end tup_insert */

void tup_truncate(tuple **host) {
  tuple *t = *host;
  assert (is_tuple(t));
  while (t->nelt > 0 && tupelt(t,t->nelt) == OM) {
    t->nelt--;
    t->nsuf++;
  }
}

/* P. 120 f. is the primary reference for the following:  */
tuple *tup_fl(integer *first, integer *last) {
  long f = get_long(first, "F in [F..L]");
  long l = get_long(last, "L in [F..L]");
  if (f <= l) {
    long nr = (l-f) + 1;
    long ir,i;
    tuple *r = new_tuple(nr);  HANDLE hr = ref(r);
    ir = 0;
    for (i=f; i<=l; i++) {
      ir++;
      let (tupelt(r,ir), (block *)new_integer(i));
    }
    assert (ir == nr);
    retire(hr);
    return r;
  } else {
    return null_tuple();
  }
} /* end tup_fl */

tuple *tup_fnl(integer *first, integer *next, integer *last) {
  long i,f,n,l,step;
  long ir,nr;
  tuple *r;  HANDLE hr;
  f = get_long(first, "F in [F,N..L]");
  n = get_long(next, "N in [F,N..L]");
  l = get_long(last, "L in [F,N..L]");
  step = n-f;
  if (step > 0) {
    if (f <= l) {
      nr = (l-f)/step + 1;
      r = new_tuple(nr);  hr = ref(r);
      ir = 0;
      for (i=f; i<=l; i+=step) {
        ir++;
        let (tupelt(r,ir), (block *)new_integer(i));
      }
      retire(hr);
    } else {
      r = null_tuple();
    }
  } else if (step == 0) {
    r = null_tuple();
  } else {  /* step < 0 */
    if (f >= l) {
      nr = (f-l)/(-step) + 1;
      r = new_tuple(nr);  hr = ref(r);
      ir = 0;
      for (i=f; i>=l; i+=step) {
        ir++;
        let (tupelt(r,ir), (block *)new_integer(i));
      }
      retire(hr);
    } else {
      r = null_tuple();
    }
  }
  return r;
} /* end tup_fnl */


routine *new_routine(long proc_pc) {
  routine *r = fblock(routine);
  r->proc_pc = proc_pc;
  return r;
} /* end new_routine */


string *block_plus_string(block *a, string *s) {
  HANDLE hs = ref(s);
  string *sa = tostr(a);
  string *r = str_join(sa, s);
  retire(hs);
  return r;
} /* end block_plus_string */

string *string_plus_block(string *s, block *b) {
  HANDLE hs = ref(s);
  string *sb = tostr(b);
  string *r = str_join(s, sb);
  retire(hs);
  return r;
} /* end string_plus_block */

const char *pfx_table[] = {NULL, NULL,
  "2#", "3#", "4#", "5#", "6#", "7#", "8#", "9#", "10#", "11#", "12#",
  "13#", "14#", "15#", "16#", "17#", "18#", "19#", "20#", "21#", "22#",
  "23#", "24#", "25#", "26#", "27#", "28#", "29#", "30#", "31#", "32#",
  "33#", "34#", "35#", "36#"
};

string *tostrad(integer *a, int radix) {
  assert (is_integer(a));
  assert (2 <= radix && radix <= 36);
  {
    HANDLE ha = ref(a);
    ro_mpz(z1, a)  /* const mpz_t z1 = alias of INTEGER a */
    const char *pfx = pfx_table[radix];
    const int pfxsize = strlen(pfx);
    /* This can be 1 more char than the digits actually require:  */
    const size_t ndig = mpz_sizeinbase (z1, radix);  /* #digits in a */
    string *r = new_estring (pfxsize + ndig + 2);
    /* Since new_estring() may have made 'a' move, form a new alias:  */
    ro_mpz(z2, a)  /* const mpz_t z2 = alias of INTEGER a */
    mpz_get_str (&strelt(r, pfxsize + 1), radix, z2);  /* convert */
    if (a->size >= 0) {  /* a is non-negative */
      memcpy (&strelt(r, 1), pfx, pfxsize);
    } else {  /* a is negative */
      strelt(r, 1) = '-';
      memcpy (&strelt(r, 2), pfx, pfxsize);  /* '#' clobbers other '-' */
    }
    str_resize (&r, strlen(&strelt(r,1)));
    retire(ha);
    return r;
  }
} /* end tostrad */

string *tostr(block *a) {
  switch (setl_type(a)) {
  case om_type:
    return new_string("*");
  case atom_type:
    {
      char buf[25];
      snprintf (buf, sizeof buf, "#%ld", ((atom *)a)->atmval);
      return new_string(buf);
    }
  case boolean_type:
    if (((boolean *)a)->booval) {
      return new_string("#T");
    } else {
      return new_string("#F");
    }
  case integer_type:
    {
      /* As in tostrad(), but without the 10# prefix... */
      integer *i = (integer *)a;
      HANDLE hi = ref(i);
      ro_mpz(z1, i)  /* const mpz_t z1 = alias of INTEGER i */
      /* Usually 1 more char than the digits actually require:  */
      const size_t ndig = mpz_sizeinbase (z1, 10);  /* #digits in i */
      string *r = new_estring (ndig + 2);
      ro_mpz(z2, i)  /* const mpz_t z2 = alias of INTEGER i */
      mpz_get_str (&strelt(r, 1), 10, z2);  /* convert */
      str_resize (&r, strlen(&strelt(r, 1)));
      retire(hi);
      return r;
    }
  case real_type:
    {
      char buf[DOUBLE_REP_DIGS + 10];
      snprintf(buf, sizeof buf, "%.*g",
                   DOUBLE_REP_DIGS, real_to_double((real *)a));
      return new_string(buf);
    }
  case set_type:
    {
      iterator *i = init_iterator(a);   HANDLE hi = ref(i);
      string *r = new_cstring('{');     HANDLE hr = ref(r);
      block *b = NULL;                  HANDLE hb = ref(b);
      bool flag = false;
      while (step_iterator(&i,&b)) {
        if (flag) str_tackon(&r,' ');
        flag = true;
        str_concat(&r,tostr(b));
      }
      str_tackon(&r,'}');
      retire(hb);
      retire(hr);
      retire(hi);
      return r;
    }
  case string_type:
    {
      string *s = (string *)a;
      bool flag = true;
      long i;
      if (s->nchar > 0) flag = alphabetic[(uchar)strelt(s,1)];
      else flag = false;
      for (i=2; flag && i <= s->nchar; i++)
       flag = idtail[(uchar)strelt(s,i)];
      if (flag) return copy_string(s);
      else {
        HANDLE h = ref(s);
        string *r = new_cstring('\'');
        long j;
        for (j=1; j<=s->nchar; j++) {
          if (strelt(s,j) == '\'') str_tackon(&r,'\'');  /* twin it */
          str_tackon(&r,strelt(s,j));
        }
        str_tackon(&r,'\'');
        retire(h);
        return r;
      }
    }
  case tuple_type:
    {
      tuple *t = (tuple *)a;    HANDLE ht = ref(t);
      string *r = new_string("[");      HANDLE hr = ref(r);
      long i;
      for (i=1; i<=t->nelt; i++) {
        if (i>1) str_tackon(&r,' ');
        str_concat(&r,tostr(tupelt(t,i)));
      }
      str_tackon(&r,']');
      retire(hr);
      retire(ht);
      return r;
    }
  case routine_type:
    return new_string("<ROUTINE>");
  default:
    runerr("Cannot convert %s to string", TYPENAME(a));
  }
} /* end tostr */

/*
 *  One slightly sleazy thing about 'getval' and 'unstr' is that
 *  they allow any number of spaces, commas, formfeeds, tabs, or
 *  newlines in a row to separate, precede, or follow values.
 *  For example, they ignore all the commas in ",,[,1,],", which
 *  is thus equivalent to "[1]".  They should be more strict
 *  about the use of commas, though for well-formed input they
 *  seem to be fine.
 *
 *  Also, the error diagnosis is poor.  For 'getval', the input line
 *  and column number should be reported, and perhaps some of the
 *  input data; for 'unstr', a column and some of the string.
 *
 *  And finally, the only extant documentation, at the time of this
 *  writing, on exactly what these routines expect and how they
 *  distinguish types (always by the first character, except that
 *  numeric types are not fully resolved to REAL or INTEGER that
 *  early), is the "library" documentation dealing with UNSTR.
 */

/*
 *  Gory semantics of 'getval' arguments:
 *
 *  The stopper argument points to EOF, ']', or '}'.  This determines
 *  how the input value is allowed to be ended.  If *stopper == EOF,
 *  the input must be terminated by EOF or by a normal value separator
 *  (comma or whitespace, as given in valsep); otherwise it can also
 *  be terminated by the appropriate punctuation symbol (']' or '}').
 *
 *  If *stopper is ']' or '}', and a corresponding ']' or '}' is read,
 *  getval assumes it has been called recursively, and sets *stopper
 *  to 0 to signify that the end of a tuple or set (respectively) has
 *  been reached.
 *
 *  The 'context' argument is just used for error reporting.  Callers
 *  set this argument to some description, such as "READA input".
 *
 *  Since 'getval' uses get_char(), the treatment of eof on input is
 *  governed by fill_in().  Manifestly incomplete values, such as
 *  unclosed sets, tuples, and strings, raise run-time errors, as do
 *  inputs that are not recognized as denotations (value tokens).
 *
 *  The caller is expected to have absorbed any leading whitespace as
 *  defined by 'valsep' (see eat_white()) before calling getval.
 */

/* This macro is only meaningful within 'getval': */
#define jetsam  (!deno_ends_ok(buffer,*stopper))

block *getval(io_buffer *buffer, int *stopper, const char *context) {
  block *r = OM;
  int c = get_char(buffer);
  switch (c) {
  case EOF:  /* eof on the stream */
    break;  /* r is OM */
  case '*':  /* denotation meaning OM */
    if (jetsam) runerr("Junk after * in %s", context);
    break;  /* r is OM */
  case '#':
    c = get_char(buffer);
    switch (c) {
    case 'T':  case 't':
      if (jetsam) runerr("Junk after #T or #t in %s", context);
      r = (block *)new_boolean(true);
      break;
    case 'F':  case 'f':
      if (jetsam) runerr("Junk after #F or #f in %s", context);
      r = (block *)new_boolean(false);
      break;
    case '0':  case '1':  case '2':  case '3':  case '4':
    case '5':  case '6':  case '7':  case '8':  case '9':
      runerr("Invalid attempt to create specific atom in %s", context);
    case EOF:
      runerr("End-of-file after initial '#' in %s", context);
    default:
      runerr("Junk after initial '#' in %s", context);
    }
    break;  /* r is a BOOLEAN */
  case '+':  case '-':  case '0':  case '1':  case '2':  case '3':
  case '4':  case '5':  case '6':  case '7':  case '8':  case '9':
  case '.':
    {
      const bool negative = (c == '-');
      bool integral = (c != '.');
      bool dotseen = (c == '.');
      bool eseen = false;
      string *s = new_cstring(c);  HANDLE hs = ref(s);
      const int start = (c != '+') ? 1 : 2;
      int ndig = decdigit[c] ? 1 : 0;
      int sharps = 0;
      long radix = 10;  /* default */
      bool flag = true;
      while (flag) {
        int prevchar = c;
        c = get_char(buffer);
        if (c == EOF) {
          /* Numeric deno (or prefix of one) ended by eof, leaving
           * eof_pending set in buffer.  */
          flag = false;  /* exit while-loop */
        } else if (sharps == 0 && decdigit[c]) {
          str_tackon(&s,c);
          ndig++;
        } else if (sharps > 0 && setl2digval[c] >= 0) {
          str_tackon(&s,c);
          if (setl2digval[c] >= radix) {
            runerr("Char '%c' in \"%s\" in %s is outside of base %ld",
                           c, tame(&strelt(s,1)), context, radix);
          }
          ndig++;
        } else if (sharps == 0 && numeric[c]) {
          /* This case is really just for any of .+-eE, because the
           * digits were already ruled out by tests above.  We don't
           * even look for .+-eE after a sharp, but treat .+- anywhere
           * past a sharp as junk.  */
          str_tackon(&s,c);
          integral = false;
          switch (c) {
          case '.':
            /* Only one dot is allowed */
            if (dotseen) {
              runerr("Misplaced dot in \"%s\" in %s",
                            tame(&strelt(s,1)), context);
            }
            dotseen = true;
            break;
          case '+':  case '-':
            /* Sign is only allowed right after 'e' or 'E' */
            if (integral || !exponent[prevchar]) {
              runerr("Misplaced '+' or '-' in \"%s\" in %s",
                                   tame(&strelt(s,1)), context);
            }
            break;
          case 'e':  case 'E':
            /* E/e is only allowed once, and only after there have
             * been digits since the last ndig reset */
            if (eseen || ndig == 0) {
              runerr("Misplaced 'e' or 'E' in \"%s\" in %s",
                                   tame(&strelt(s,1)), context);
            }
            ndig = 0;  /* to catch incomplete nums ending in e, e+, e- */
            eseen = true;
            dotseen = true;  /* no dots allowed after e or E */
            break;
          default:
            unexpected(c);
          }
        } else if (c == '#') {  /* radix indicator */
          if (integral) {
            switch (sharps) {
            case 0:  /* the s so far is the radix */
              if (ndig > 0) {
                int saved_errno = errno;
                errno = 0;
                radix = strtol(&strelt(s,start), NULL, 10);
                if (errno == 0 && 2 <= radix && radix <= 36) {
                  str_resize(&s, 0);  /* clear s for digits after '#' */
                  ndig = 0;
                  ++sharps;
                } else {
                  runerr("Radix in \"%s#\" in %s is out of range 2..36",
                          tame(&strelt(s,1)), context);
                }
                errno = saved_errno;
              } else {
                runerr("Need radix digit(s) between sign and '#' in %s",
                                                                context);
              }
              break;
            case 1:  /* this is a terminating '#' */
              if (jetsam) {
                runerr("Junk after trailing '#' of \"%ld#%s#\" in %s",
                                     radix, tame(&strelt(s,1)), context);
              }
              flag = false;  /* exit while-loop */
              break;
            default:
              unexpected (sharps);
            }
          } else {
            runerr("Non-integer radix in \"%s#\" in %s",
                              tame(&strelt(s,1)), context);
          }
        } else {  /* end of number */
          unget_char(c,buffer);  /* put back last char read */
          if (jetsam) {  /* number improperly terminated */
            runerr("Junk after \"%s\" in %s",
                    tame(&strelt(s,1)), context);
          }
          /* At this point, we know that the number is syntactically
           * correct as far as it goes, and is properly terminated by
           * a valsep or an appropriate ']' or '}', but we do not know
           * whether it is complete.  Just like if we had hit eof.
           * We check for completeness after the loop.  */
          flag = false;  /* exit while-loop */
        }
      } /* end while (flag) */
      /* Did we get a complete number ready to try out on the mp
       * functions or strtod?  */
      if (ndig == 0) {
        /* No, the digit counter is 0, meaning no digits were seen
         * since the last ndig reset, which occurs initially, after
         * a first sharp, and after an e or E.  */
        if (sharps == 0) {
          runerr("Missing digits after \"%s\" in %s",
                            tame(&strelt(s,1)), context);
        } else {
          /* In this case, s was cleared and nothing came after the
           * sharp, so cite the radix indicator rather than the empty
           * string.  */
          runerr("Missing digits after \"%ld#\" in %s",
                                        radix, context);
        }
      }
      if (integral) {
        /* For integers with no explicit radix (sharps == 0), we
         * convert the string directly including any leading '-' sign
         * (but not '+', to avoid confusing charstr_to_integer()).
         * For integers with an explicit radix (sharps > 0), we
         * convert the abs val and then negate if nec.  */
        integer *t = charstr_to_integer(&strelt(s,start), radix);
        if (negative && sharps > 0) {
          t->size = -t->size;
        }
        r = (block *)t;
      } else {
        double d;
        char *end;
        int saved_errno = errno;
        errno = 0;
        d = strtod(&strelt(s,start), &end);
        /* We permit underflow and overflow but not conversion errors: */
        if ((errno != 0 && errno != ERANGE) || *end != '\0') {
          /* Something slipped through all our oh-so-careful checks
           * but was rejected by strtod().  I do not currently know of
           * an input that can trigger this.  */
          runerr("Bad floating-point denotation \"%s\" in %s",
                                     tame(&strelt(s,1)), context);
        }
        errno = saved_errno;
        r = (block *)new_real(d);
      }
      retire(hs);
    }
    break;  /* r is an INTEGER or REAL */
  case '[':
    {
      tuple *t = null_tuple();  HANDLE ht = ref(t);
      while (true) {
        int tup_stopper = ']';
        block *e;
        eat_white(buffer);  /* absorb leading whitespace if any */
        e = getval(buffer, &tup_stopper, context);  /* member deno */
        if (tup_stopper == 0) {  /* tuple deno properly ended with ] */
          break;  /* exit while-loop */
        }
        if (buffer->eof_pending) {  /* on recursive call to getval() */
          /* As a minor point of interest, "[1" as the whole input
           * string would hit this runerr() before the 1 is appended to
           * t, whereas "[1 " (with trailing space) would append the 1
           * to t and then go around again, skipping the whitespace,
           * incidentally getting an OM from getval(), and hitting this
           * then because of the whitespace skippage.  */
          runerr("Missing ']' (unterminated tuple) in %s", context);
        }
        tup_tackon(&t,e);
      }
      tup_truncate(&t);
      if (jetsam) runerr("Junk after ']' in %s", context);
      retire(ht);
      r = (block *)t;
    }
    break;  /* r is a TUPLE */
  case ']':
    if (*stopper != ']') {
      runerr("Unexpected ']' character in %s", context);
    }
    *stopper = 0;  /* done tuple on recursive getval() call */
    break;  /* r (still OM) is irrelevant because we cleared *stopper */
  case '{':
    {
      set *s = null_set();  HANDLE hs = ref(s);
      while (true) {
        int set_stopper = '}';
        block *e;
        eat_white(buffer);  /* absorb leading whitespace if any */
        e = getval(buffer, &set_stopper, context);  /* member deno */
        if (set_stopper == 0) {  /* set deno properly ended with } */
          break;    /* leave while-loop */
        }
        if (buffer->eof_pending) {
          /* Similar observations here as for tuple case above.  */
          runerr("Missing '}' (unterminated set) in %s", context);
        }
        set_insert(&s,e);
      }
      if (jetsam) runerr("Junk after '}' in %s", context);
      retire(hs);
      r = (block *)s;
    }
    break;  /* r is a SET */
  case '}':
    if (*stopper != '}') {
      runerr("Unexpected '}' character in %s", context);
    }
    *stopper = 0;  /* done set on recursive getval() call */
    break;  /* r (still OM) is irrelevant because we cleared *stopper */
  case '\'':  case '\"':
    {
      int q = c;
      /* The handle on s here is overdefensive as of this writing, as
       * nothing in this scope currently causes SETL heap allocation
       * apart from the str_tackon(&s,char) calls:  */
      string *s = null_string();  HANDLE hs = ref(s);
      bool flag = true;
      while (flag) {
        c = get_char(buffer);
        if (c == EOF) {
          runerr("Unterminated quoted string %s in %s",
                          qtame(q,&strelt(s,1)), context);
        }
        if (c == q) {  /* quote matching the opening quote */
          c = get_char(buffer);
          if (c == EOF) {  /* eof met after the closing quote */
            flag = false;  /* exit string-forming while-loop */
          } else if (c == q) {  /* q repeated internally */
            str_tackon(&s,q);  /* means one q */
          } else {  /* something else after the closing quote */
            unget_char(c,buffer);  /* put it back to be re-read */
            if (jetsam) {
              runerr("Junk after quoted string %s%c in %s",
                           qtame(q,&strelt(s,1)), q, context);
            }
            flag = false;  /* exit string-forming while-loop */
          }
#if ABSORB_BACKSLASH_NEWLINE
        } else if (c == '\\') {
          c = get_char(buffer);
          if (c == EOF) {
            runerr("Unterminated quoted string %s in %s",
                            qtame(q,&strelt(s,1)), context);
          }
          if (c == '\n') {
            /* OK, string continued on next line */
          } else {
            str_tackon(&s,'\\');  /* backslash means itself */
            unget_char(c,buffer);  /* next char treated normally */
          }
#endif
        } else {
          str_tackon(&s,c);
        }
      } /* end while (flag) */
      retire(hs);
      r = (block *)s;
    }
    break;  /* r is a STRING */
  default:
    if (alphabetic[c]) {
      /* Look for an unquoted string in the form of a SETL identifier */
      string *s = new_cstring(c);  HANDLE hs = ref(s);
      while ((c=get_char(buffer))!=EOF && idtail[c]) {
        str_tackon(&s,c);
      }
      if (c != EOF) {
        unget_char(c,buffer);  /* push back the ending non-idtail char */
        if (jetsam) {
          runerr("Junk after unquoted string \"%s\" in %s",
                                  tame(&strelt(s,1)), context);
        }
      }
      retire(hs);
      r = (block *)s;
    } else {
      if (isprint(c)) {
        runerr("Unrecognized denotation starting with '%c' in %s",
                                                        c, context);
      } else {
        runerr("Bad denotation starting with unprintable character"
               " (hex %02x) in %s", c, context);
      }
    }
    break;  /* r is a STRING */
  } /* end switch (c) */
  return r;
} /* end getval */

#undef jetsam

/*
 *  The following helper function, 'deno_ends_ok', is used only by
 *  'getval' (via the 'jetsam' macro).  Its purpose is to peek at
 *  the first character, if any, beyond the denotation just scanned,
 *  and yield a boolean indication of whether it is a valid way of
 *  terminating the denotation according to 'stopper'.
 *
 *  Seeing eof is always a valid way of ending the denotation, as is
 *  a normal "value separator".  Passing 'stopper' as EOF makes those
 *  the only valid enders.  Passing 'stopper' as any other character
 *  additionally allows that character to end the denotation; the
 *  known use cases for that are ']' and '}'.
 *
 *  Seeing eof on the peek causes buffer->eof_pending to be set or
 *  left set.
 */
static bool deno_ends_ok(io_buffer *buffer, int stopper) {
  int c = peek_char(buffer);
  return c == EOF || valsep[c] || c == stopper;
} /* end deno_ends_ok */

/*
 *  The following routine, 'unstr', is patterned after 'getval' but
 *  reads from a string instead of from an input buffer.  It also takes
 *  an index parameter (i) to tell it where to start looking in the
 *  string, and updates this parameter to point to just after what it
 *  has scanned.  Other parameters have semantics as in 'getval'.
 *  Initial whitespace (as in 'valsep') is skipped over before the
 *  unstr call, generally by span_white().
 */

/* These macros are meaningful within 'unstr' and 'denotype': */
#define curchar   (j > a->nchar ? EOF : (uchar)strelt(a,j))
#define nextchar  (c = (++j > a->nchar ? EOF : (uchar)strelt(a,j)))
#define flotsam   (!deno_tail_ok(a,j,*stopper))

block *unstr(string *a, long *i, int *stopper, const char *context) {
  HANDLE ha = ref(a);
  block *r = OM;
  long j = *i;
  int c = curchar;  /* a(j) or EOF */
  switch (c) {
  case EOF:  /* nothing left in string */
    break;  /* r is OM */
  case '*':  /* denotation meaning OM */
    ++j;
    if (flotsam) runerr("Junk after * in %s", context);
    break;  /* r is OM */
  case '#':
    nextchar;
    switch (c) {
    case 'T':  case 't':
      ++j;
      if (flotsam) runerr("Junk after #T or #t in %s", context);
      r = (block *)new_boolean(true);
      break;
    case 'F':  case 'f':
      ++j;
      if (flotsam) runerr("Junk after #F or #f in %s", context);
      r = (block *)new_boolean(false);
      break;
    case '0':  case '1':  case '2':  case '3':  case '4':
    case '5':  case '6':  case '7':  case '8':  case '9':
      runerr("Invalid attempt to create specific atom in %s", context);
    case EOF:
      runerr("Nothing after initial '#' in %s", context);
    default:
      runerr("Junk after initial '#' in %s", context);
    }
    break;  /* r is a BOOLEAN */
  case '+':  case '-':  case '0':  case '1':  case '2':  case '3':
  case '4':  case '5':  case '6':  case '7':  case '8':  case '9':
  case '.':
    {
      const bool negative = (c == '-');
      bool integral = (c != '.');
      bool dotseen = (c == '.');
      bool eseen = false;
      string *s = new_cstring(c);  HANDLE hs = ref(s);
      const int start = (c != '+') ? 1 : 2;
      int ndig = decdigit[c] ? 1 : 0;
      int sharps = 0;
      long radix = 10;  /* default */
      bool flag = true;
      while (flag) {
        int prevchar = c;
        nextchar;
        if (c == EOF) {
          /* Numeric deno (or prefix of one) ended by end of string */
          flag = false;  /* exit while-loop */
        } else if (sharps == 0 && decdigit[c]) {
          str_tackon(&s,c);
          ndig++;
        } else if (sharps > 0 && setl2digval[c] >= 0) {
          str_tackon(&s,c);
          if (setl2digval[c] >= radix) {
            runerr("Char '%c' in \"%s\" in %s is outside of base %ld",
                           c, tame(&strelt(s,1)), context, radix);
          }
          ndig++;
        } else if (sharps == 0 && numeric[c]) {
          /* This case is really just for any of .+-eE, because the
           * digits were already ruled out by tests above.  We don't
           * even look for .+-eE after a sharp, but treat .+- anywhere
           * past a sharp as junk.  */
          str_tackon(&s,c);
          integral = false;
          switch (c) {
          case '.':
            /* Only one dot is allowed */
            if (dotseen) {
              runerr("Misplaced dot in \"%s\" in %s",
                            tame(&strelt(s,1)), context);
            }
            dotseen = true;
            break;
          case '+':  case '-':
            /* Sign is only allowed right after 'e' or 'E' */
            if (integral || !exponent[prevchar]) {
              runerr("Misplaced '+' or '-' in \"%s\" in %s",
                                   tame(&strelt(s,1)), context);
            }
            break;
          case 'e':  case 'E':
            /* E/e is only allowed once, and only after there have
             * been digits since the last ndig reset */
            if (eseen || ndig == 0) {
              runerr("Misplaced 'e' or 'E' in \"%s\" in %s",
                                   tame(&strelt(s,1)), context);
            }
            ndig = 0;  /* to catch incomplete nums ending in e, e+, e- */
            eseen = true;
            dotseen = true;  /* no dots allowed after e or E */
            break;
          default:
            unexpected(c);
          }
        } else if (c == '#') {  /* radix indicator */
          if (integral) {
            switch (sharps) {
            case 0:  /* the s so far is the radix */
              if (ndig > 0) {
                int saved_errno = errno;
                errno = 0;
                radix = strtol(&strelt(s,start), NULL, 10);
                if (errno == 0 && 2 <= radix && radix <= 36) {
                  str_resize(&s, 0);  /* clear s for digits after '#' */
                  ndig = 0;
                  ++sharps;
                } else {
                  runerr("Radix in \"%s#\" in %s is out of range 2..36",
                          tame(&strelt(s,1)), context);
                }
                errno = saved_errno;
              } else {
                runerr("Need radix digit(s) between sign and '#' in %s",
                                                                context);
              }
              break;
            case 1:  /* this is a terminating '#' */
              ++j;
              if (flotsam) {
                runerr("Junk after trailing '#' of \"%ld#%s#\" in %s",
                                     radix, tame(&strelt(s,1)), context);
              }
              flag = false;  /* exit while-loop */
              break;
            default:
              unexpected (sharps);
            }
          } else {
            runerr("Non-integer radix in \"%s#\" in %s",
                              tame(&strelt(s,1)), context);
          }
        } else {  /* end of number */
          if (flotsam) {  /* number improperly terminated */
            runerr("Junk after \"%s\" in %s",
                    tame(&strelt(s,1)), context);
          }
          /* At this point, we know that the number is syntactically
           * correct as far as it goes, and is properly terminated by
           * a valsep or an appropriate ']' or '}', but we do not know
           * whether it is complete.  Just like if we had hit eof.
           * We check for completeness after the loop.  */
          flag = false;  /* exit while-loop */
        }
      } /* end while (flag) */
      /* Did we get a complete number ready to try out on the mp
       * functions or strtod?  */
      if (ndig == 0) {
        /* No, the digit counter is 0, meaning no digits were seen
         * since the last ndig reset, which occurs initially, after
         * a first sharp, and after an e or E.  */
        if (sharps == 0) {
          runerr("Missing digits after \"%s\" in %s",
                            tame(&strelt(s,1)), context);
        } else {
          /* In this case, s was cleared and nothing came after the
           * sharp, so cite the radix indicator rather than the empty
           * string.  */
          runerr("Missing digits after \"%ld#\" in %s",
                                        radix, context);
        }
      }
      if (integral) {
        /* For integers with no explicit radix (sharps == 0), we
         * convert the string directly including any leading '-' sign
         * (but not '+', to avoid confusing charstr_to_integer()).
         * For integers with an explicit radix (sharps > 0), we
         * convert the abs val and then negate if nec.  */
        integer *t = charstr_to_integer(&strelt(s,start), radix);
        if (negative && sharps > 0) {
          t->size = -t->size;
        }
        r = (block *)t;
      } else {
        double d;
        char *end;
        int saved_errno = errno;
        errno = 0;
        d = strtod(&strelt(s,start), &end);
        /* We permit underflow and overflow but not conversion errors: */
        if ((errno != 0 && errno != ERANGE) || *end != '\0') {
          /* Something slipped through all our oh-so-careful checks
           * but was rejected by strtod().  I do not currently know of
           * an input that can trigger this.  */
          runerr("Bad floating-point denotation \"%s\" in %s",
                                     tame(&strelt(s,1)), context);
        }
        errno = saved_errno;
        r = (block *)new_real(d);
      }
      retire(hs);
    }
    break;  /* r is an INTEGER or REAL */
  case '[':
    {
      tuple *t = null_tuple();  HANDLE ht = ref(t);
      ++j;
      while (true) {
        int tup_stopper = ']';
        block *e;
        span_white(a, &j);  /* skip leading whitespace if any */
        e = unstr(a, &j, &tup_stopper, context);  /* member deno */
        if (tup_stopper == 0) {  /* tuple deno properly ended with ] */
          break;  /* exit while-loop */
        }
        if (j > a->nchar) {  /* end of string */
          /* As a minor point of interest, "[1" as the whole input
           * string would hit this runerr() before the 1 is appended to
           * t, whereas "[1 " (with trailing space) would append the 1
           * to t and then go around again, skipping the whitespace,
           * incidentally getting an OM from unstr(), and hitting this
           * then because of the whitespace skippage.  */
          runerr("Missing ']' (unterminated tuple) in %s", context);
        }
        tup_tackon(&t,e);
      }
      tup_truncate(&t);
      if (flotsam) runerr("Junk after ']' in %s", context);
      retire(ht);
      r = (block *)t;
    }
    break;  /* r is a TUPLE */
  case ']':
    if (*stopper != ']') {
      runerr("Unexpected ']' character in %s", context);
    }
    ++j;  /* point past the ']' */
    *stopper = 0;  /* done building tuple on recursive unstr() call */
    break;  /* r (still OM) is irrelevant because we cleared *stopper */
  case '{':
    {
      set *s = null_set();  HANDLE hs = ref(s);
      ++j;
      while (true) {
        int set_stopper = '}';
        block *e;
        span_white(a, &j);  /* skip leading whitespace if any */
        e = unstr(a, &j, &set_stopper, context);  /* member deno */
        if (set_stopper == 0) {  /* set deno properly ended with } */
          break;    /* leave while-loop */
        }
        if (j > a->nchar) {  /* end of string */
          /* Similar observations here as for tuple case above.  */
          runerr("Missing '}' (unterminated set) in %s", context);
        }
        set_insert(&s,e);
      }
      if (flotsam) runerr("Junk after '}' in %s", context);
      retire(hs);
      r = (block *)s;
    }
    break;  /* r is a SET */
  case '}':
    if (*stopper != '}') {
      runerr("Unexpected '}' character in %s", context);
    }
    ++j;  /* point past the '}' */
    *stopper = 0;  /* done building set on recursive unstr() call */
    break;  /* r (still OM) is irrelevant because we cleared *stopper */
  case '\'':  case '\"':
    {
      int q = c;
      string *s = null_string();
      bool flag = true;
      while (flag) {
        nextchar;
        if (c == EOF) {
          runerr("Unterminated quoted string %s in %s",
                          qtame(q,&strelt(s,1)), context);
        }
        if (c == q) {  /* quote matching the opening quote */
          nextchar;
          if (c == EOF) {  /* string ends after the closing quote */
            flag = false;  /* exit string-forming while-loop */
          } else if (c == q) {  /* q repeated internally */
            str_tackon(&s,q);  /* means one q */
          } else {  /* something else after the closing quote */
            if (flotsam) {
              runerr("Junk after quoted string %s%c in %s",
                           qtame(q,&strelt(s,1)), q, context);
            }
            flag = false;  /* exit string-forming while-loop */
          }
#if ABSORB_BACKSLASH_NEWLINE
        } else if (c == '\\') {
          nextchar;
          if (c == EOF) {
            runerr("Unterminated quoted string %s in %s",
                            qtame(q,&strelt(s,1)), context);
          }
          if (c == '\n') {
            /* OK, string continued on next "line" */
          } else {
            str_tackon(&s,'\\');  /* backslash means itself */
            j--;  /* next char treated normally */
          }
#endif
        } else {
          str_tackon(&s,c);
        }
      } /* end while (flag) */
      r = (block *)s;
    }
    break;  /* r is a STRING */
  default:
    if (alphabetic[c]) {
      /* Look for an unquoted string in the form of a SETL identifier */
      string *s;
      long k = j+1;
      while (k<=a->nchar && idtail[(uchar)strelt(a,k)]) ++k;
      s = new_estring(k-j);
      mvmem (&strelt(s,1), &strelt(a,j), k-j);
      j = k;
      if (flotsam) {
        runerr("Junk after unquoted string \"%s\" in %s",
                                tame(&strelt(s,1)), context);
      }
      r = (block *)s;
    } else {
      if (isprint(c)) {
        runerr("Unrecognized denotation starting with '%c' in %s",
                                                        c, context);
      } else {
        runerr("Bad denotation starting with unprintable character"
               " (hex %02x) in %s", c, context);
      }
    }
    break;  /* r is a STRING */
  } /* end switch (c) */
  *i = j;
  retire(ha);
  return r;
} /* end unstr */

/*
 *  The following routine, 'denotype', is patterned after 'unstr'
 *  but just checks the 'a' string instead of converting it, and
 *  returns the type it would have if converted by 'unstr'.
 *  In fact 'denotype' was derived from 'unstr', so wherever you
 *  see it returning the initial assumption of invalid_type, and
 *  don't understand what's wrong with the string, see the error
 *  message in the corresponding code in 'unstr' ('denotype'
 *  doesn't trigger run-time errors for bad denotations as 'unstr'
 *  does, but meekly returns invalid_type).
 */

#define return_from_denotype do { \
  *i = j; \
  retire(ha); \
  return r; \
} while (0)

blocktype denotype (string *a, long *i, int *stopper) {
  HANDLE ha = ref(a);
  blocktype r = invalid_type;
  long j = *i;
  int c = curchar;  /* a(j) or EOF */
  switch (c) {
  case EOF:  /* nothing left in string */
    break;  /* r is invalid_type */
  case '*':  /* denotation meaning OM */
    ++j;
    if (!flotsam) r = om_type;
    break;  /* r is om_type or invalid_type */
  case '#':
    nextchar;
    switch (c) {
    case 'T':  case 't':  case 'F':  case 'f':
      ++j;
      if (!flotsam) r = boolean_type;
    }
    break;  /* r is boolean_type or invalid_type */
  case '+':  case '-':  case '0':  case '1':  case '2':  case '3':
  case '4':  case '5':  case '6':  case '7':  case '8':  case '9':
  case '.':
    {
      bool integral = (c != '.');
      bool dotseen = (c == '.');
      bool eseen = false;
      string *s = new_cstring(c);
      const int start = (c != '+') ? 1 : 2;
      int ndig = decdigit[c] ? 1 : 0;
      int sharps = 0;
      long radix = 10;  /* default */
      bool flag = true;
      while (flag) {
        int prevchar = c;
        nextchar;
        if (c == EOF) {
          /* Numeric deno (or prefix of one) ended by end of string */
          flag = false;  /* exit while-loop */
        } else if (sharps == 0 && decdigit[c]) {
          str_tackon(&s,c);
          ndig++;
        } else if (sharps > 0  && setl2digval[c] >= 0) {
          str_tackon(&s,c);
          if (setl2digval[c] >= radix) {
            return_from_denotype;  /* r is invalid_type */
          }
          ndig++;
        } else if (sharps == 0 && numeric[c]) {
          /* This case is really just for any of .+-eE, because the
           * digits were already ruled out by tests above.  We don't
           * even look for .+-eE after a sharp, but treat .+- anywhere
           * past a sharp as junk.  */
          str_tackon(&s,c);
          integral = false;
          switch (c) {
          case '.':
            /* Only one dot is allowed */
            if (dotseen) {
              return_from_denotype;  /* r is invalid_type */
            }
            dotseen = true;
            break;
          case '+':  case '-':
            /* Sign is only allowed right after 'e' or 'E' */
            if (integral || !exponent[prevchar]) {
              return_from_denotype;  /* r is invalid_type */
            }
            break;
          case 'e':  case 'E':
            /* E/e is only allowed once, and only after there have
             * been digits since the last ndig reset */
            if (eseen || ndig == 0) {
              return_from_denotype;  /* r is invalid_type */
            }
            ndig = 0;  /* to catch incomplete nums ending in e, e+, e- */
            eseen = true;
            dotseen = true;  /* no dots allowed after e or E */
            break;
          default:
            unexpected(c);
          }
        } else if (c == '#') {  /* radix indicator */
          if (integral) {
            switch (sharps) {
            case 0:  /* the s so far is the radix */
              if (ndig > 0) {
                int saved_errno = errno;
                errno = 0;
                radix = strtol(&strelt(s,start), NULL, 10);
                if (errno == 0 && 2 <= radix && radix <= 36) {
                  str_resize(&s, 0);  /* clear s for digits after '#' */
                  ndig = 0;
                  ++sharps;
                } else {
                  errno = saved_errno;
                  return_from_denotype;  /* r is invalid_type */
                }
                errno = saved_errno;
              } else {
                return_from_denotype;  /* r is invalid_type */
              }
              break;
            case 1:  /* this is a terminating '#' */
              ++j;
              if (flotsam) {
                return_from_denotype;  /* r is invalid_type */
              }
              flag = false;  /* exit while-loop */
              break;
            default:
              unexpected (sharps);
            }
          } else {  /* non-integer before the '#' */
            return_from_denotype;  /* r is invalid_type */
          }
        } else {  /* end of number */
          if (flotsam) {
            return_from_denotype;  /* r is invalid_type */
          }
          flag = false;  /* exit while-loop */
        }
      } /* end while (flag) */
      /* Did we get a complete number ready to try out on the mp
       * functions or strtod?  */
      if (ndig == 0) {
        /* No, the digit counter is 0, meaning no digits were seen
         * since the last ndig reset, which occurs initially, after
         * a first sharp, and after an e or E.  */
        return_from_denotype;  /* r is invalid_type */
      }
      if (integral) {
        r = integer_type;
      } else {
        char *end;
        int saved_errno = errno;
        errno = 0;
        (void) strtod(&strelt(s,start), &end);
        /* We permit underflow and overflow but not conversion errors: */
        if ((errno == 0 || errno == ERANGE) && *end == '\0') {
          /* If we don't reach here, then the ostensible floating-point
           * denotation slipped through all our oh-so-careful checks
           * but was rejected by strtod().  I do not currently know of
           * an input that can trigger that.  */
          r = real_type;
        }
        errno = saved_errno;
      }
    }
    break;  /* r is integer_type, real_type, or invalid_type */
  case '[':
    ++j;
    while (true) {
      int tup_stopper = ']';
      blocktype e;
      span_white(a, &j);  /* skip leading whitespace if any */
      e = denotype(a, &j, &tup_stopper);  /* member denotype */
      if (tup_stopper == 0) break;  /* if ] hit, exit while-loop */
      if (e == invalid_type) return_from_denotype;
      if (j > a->nchar) return_from_denotype;  /* missing ] */
    }
    if (!flotsam) r = tuple_type;
    break;  /* r is tuple_type or invalid_type */
  case ']':
    if (*stopper != ']') {  /* unexpected ] */
      return_from_denotype;  /* r is invalid_type */
    }
    ++j;  /* point past the ']' */
    *stopper = 0;  /* done tuple on recursive denotype() call */
    break;  /* r is irrelevant as we cleared *stopper */
  case '{':
    ++j;
    while (true) {
      int set_stopper = '}';
      blocktype e;
      span_white(a, &j);  /* skip leading whitespace if any */
      e = denotype(a, &j, &set_stopper);  /* member denotype */
      if (set_stopper == 0) break;  /* if } hit, exit while-loop */
      if (e == invalid_type) return_from_denotype;
      if (j > a->nchar) return_from_denotype;  /* missing } */
    }
    if (!flotsam) r = set_type;
    break;  /* r is set_type or invalid_type */
  case '}':
    if (*stopper != '}') {  /* unexpected } */
      return_from_denotype;  /* r is invalid_type */
    }
    ++j;  /* point past the '}' */
    *stopper = 0;  /* done set on recursive denotype() call */
    break;  /* r is irrelevant as we cleared *stopper */
  case '\'':  case '\"':
    {
      int q = c;
      bool flag = true;
      while (flag) {
        nextchar;
        if (c == EOF) {  /* unterminated quoted string */
          return_from_denotype;  /* r is invalid_type */
        }
        if (c == q) {  /* quote matching the opening quote */
          nextchar;
          if (c == EOF) {  /* string ends after the closing quote */
            flag = false;  /* exit while-loop */
          } else if (c == q) {  /* q repeated internally */
            /* qool */
          } else {  /* something else after the closing quote */
            if (flotsam) {
              return_from_denotype;  /* r is invalid_type */
            }
            flag = false;  /* exit while-loop */
          }
#if ABSORB_BACKSLASH_NEWLINE
        } else if (c == '\\') {
          nextchar;
          if (c == EOF) {
            return_from_denotype;  /* r is invalid_type */
          }
          if (c == '\n') {
            /* OK, string continued on next "line" */
          } else {
            /* backslash means itself */
            j--;  /* next char treated normally */
          }
#endif
        } else {
          /* normal char */
        }
      } /* end while (flag) */
      r = string_type;
    }
    break;  /* r is string_type */
  default:
    if (alphabetic[c]) {
      /* Look for an unquoted string in the form of a SETL identifier */
      ++j;
      while (j<=a->nchar && idtail[(uchar)strelt(a,j)]) ++j;
      if (!flotsam) r = string_type;
    }
    break;  /* r is string_type or invalid_type */
  } /* end switch (c) */
  return_from_denotype;
} /* end denotype */

#undef return_from_denotype

/*
 *  This is used only by 'unstr' and 'denotype' (via the 'flotsam'
 *  macro), and is the analogue of the 'deno_ends_ok' function
 *  used by 'getval':
 */
static bool deno_tail_ok(string *a, long j, int stopper) {
  int c = curchar;
  return c == EOF || valsep[c] || c == stopper;
} /* end deno_tail_ok */

#undef flotsam
#undef nextchar
#undef curchar

block *val(string *a) {
  HANDLE ha = ref(a);
  block *r = OM;
  int stopper = EOF;    /* "top-level" */
  long i = 1;         /* string index */
  long j;
  blocktype t;
  assert (is_string(a));
  span_white(a, &i);  /* skip leading whitespace if any */
  j = i;  /* where the denotation (token) proper starts */
  t = denotype(a, &j, &stopper);
  if (t == integer_type ||
      t == real_type) {
    span_white(a, &j);  /* skip whitespace after the denotation */
    if (j <= a->nchar) {  /* that still left some junk in the string */
      runerr("Junk after initial denotation in \"%s\" in VAL operand",
                                     tame(&strelt(a,1)));
    }
    stopper = EOF;
    r = unstr(a, &i, &stopper, "ostensibly error-free re-scan of number");
  } else {
    /* r remaining OM will have to do */
  }
  retire(ha);
  return r;
} /* end val */


iterator *init_counter_fl(integer *first, integer *last) {
  long f,l;
  iterator *r;
  f = get_long(first, "F in F..L");
  l = get_long(last, "L in F..L");
  r = fblock(iterator);
  r->itype = counter;
  r->itstate = f;  /* for counters, this is the next value to deliver */
  r->constant = NULL;
  r->increment = 1;
  r->limit = MAX(f,l+1);  /* just beyond the last value to deliver */
  return r;
} /* end init_counter_fl */

iterator *init_counter_fnl(integer *first, integer *next, integer *last) {
  long f,n,l;
  iterator *r;
  f = get_long(first, "F in F,N..L");
  n = get_long(next, "N in F,N..L");
  l = get_long(last, "L in F,N..L");
  r = fblock(iterator);
  r->itype = counter;
  r->itstate = f;
  r->constant = NULL;
  r->increment = n - f;
  /*
   *  By choosing the limit properly, and having step_counter just test
   *  for equality with this limit before doing its yield and
   *  post-increment, we can avoid having a direction test in every
   *  step_counter call.  This means setting the limit always one
   *  increment beyond the farthest value allowed to be assumed by the
   *  itstate (the value of the counter):
   */
  if (r->increment > 0) {
    long a = r->increment;
    long s = l - f;
    if (s >= 0) {
      r->limit = l + (a - s % a);  /* lc ell + ... */
    } else {
      r->limit = f;  /* l < f with +ve increment means no iterations */
    }
  } else if (r->increment == 0) {
    r->limit = f;  /* a 0 increment is taken to mean no iterations */
  } else {
    long a = -r->increment;
    long s = f - l;
    if (s >= 0) {
      r->limit = l - (a - s % a);  /* lc ell - ... */
    } else {
      r->limit = f;  /* l > f with -ve increment means no iterations */
    }
  }
  return r;
} /* end init_counter_fnl */

iterator *init_iterator(block *p) {
  iterator *r;
  block *c;  HANDLE hc;
  switch (setl_type(p)) {
  case set_type:
  case string_type:
  case tuple_type:
    c = copy_value(p);  hc = ref(c);
    if (is_set(c)) demote((set *)c,plain_set);
    r = fblock(iterator);
    r->itype = plain_iter;
    r->itstate = 0;
    r->constant = c;
    retire(hc);
    return r;
  default: ;
  }
  runerr("Can only iterate over SET, STRING, or TUPLE, not %s",
                                                   TYPENAME(p));
} /* end init_iterator */

iterator *init_itersmap(block *p) {
  iterator *r;
  block *c;  HANDLE hc;
  switch (setl_type(p)) {
  case set_type:
  case string_type:
  case tuple_type:
    c = copy_value(p);  hc = ref(c);
    if (is_set(c)) {
      if (!promote((set *)c,smap)) {
        if (!promote((set *)c,mmap)) {
          runerr("Cannot smap-iterate over non-map");
        } else {
          runerr("Cannot smap-iterate over multivalued map");
        }
      }
    }
    r = fblock(iterator);
    r->itype = smap_iter;
    r->itstate = 0;
    r->constant = c;
    retire(hc);
    return r;
  default: ;
  }
  runerr("Can only smap-iterate over SET, STRING, or TUPLE, not %s",
                                                        TYPENAME(p));
} /* end init_itersmap */

iterator *init_itermmap(set *p) {
  iterator *r;
  set *c;  HANDLE hc;
  switch (setl_type(p)) {
  case set_type:
    c = copy_set(p);  hc = ref(c);
    if (!promote(c,mmap)) runerr("Cannot mmap-iterate over non-map");
    demote(c,mmap);
    r = fblock(iterator);
    r->itype = mmap_iter;
    r->itstate = 0;
    r->constant = (block *)c;
    retire(hc);
    return r;
  default: ;
  }
  runerr("Can only mmap-iterate over SET, not %s",
                                      TYPENAME(p));
} /* end init_itermmap */

bool step_counter(iterator **it, integer **e) {
  iterator *i = *it;
  bool r;
  assert (is_iterator(i));
  assert (i->itype == counter);
  if (i->itstate == i->limit) {
    *e = OM;
    i = OM;  /* iterator exhausted */
    r = false;
  } else {
    HANDLE hi = ref(i);
    integer *p = new_integer (i->itstate);
    retire(hi);
    i->itstate += i->increment;
    *e = p;
    r = true;
  }
  *it = i;
  return r;
} /* end step_counter */

bool step_iterator(iterator **it, block **e) {
  iterator *i = *it;  HANDLE hi = ref(i);
  block *p = OM;
  bool r = true;
  assert (is_iterator(i));
  assert (i->itype == plain_iter);
  i->itstate++;
  switch (setl_type(i->constant)) {
  case set_type:
    {
      subnode *b;
      set *s = (set *)(i->constant);
      assert (s->stype == plain_set);
      if (ordsee(s->tblval, i->itstate, &b)) p = unkey(b->k);
      else r = false;
    }
    break;
  case string_type:
    {
      string *s = (string *)(i->constant);
      if (i->itstate <= s->nchar)
       p = (block *)new_cstring(strelt(s,i->itstate));
      else r = false;
    }
    break;
  case tuple_type:
    {
      tuple *t = (tuple *)(i->constant);
      if (i->itstate <= t->nelt) p = copy_value(tupelt(t,i->itstate));
      else r = false;
    }
    break;
  default:
    unexpected (setl_type(i->constant));
  }
  *e = p;
  if (!r) i = OM;  /* iterator exhausted */
  retire(hi);
  *it = i;
  return r;
} /* end step_iterator */

bool step_itersmap(iterator **it, block **x, block **f) {
  iterator *i = *it;  HANDLE hi = ref(i);
  block *xloc = OM;
  block *floc = OM;
  HANDLE hx;
  bool r = true;
  assert (is_iterator(i));
  assert (i->itype == smap_iter);
  i->itstate++;
  switch (setl_type(i->constant)) {
  case set_type:
    {
      subnode *b;
      set *s = (set *)(i->constant);
      assert (s->stype == smap);
      if (ordsee(s->tblval, i->itstate, &b)) {
        HANDLE hb = ref(b);
        xloc = unkey(b->k);  hx = ref(xloc);
        floc = copy_value(b->d);
        retire(hx);
        retire(hb);
      } else {
        r = false;
      }
    }
    break;
  case string_type:
    {
      string *s = (string *)(i->constant);
      if (i->itstate <= s->nchar) {
        char c = strelt(s,i->itstate);
        xloc = (block *)new_integer(i->itstate);  hx = ref(xloc);
        floc = (block *)new_cstring(c);
        retire(hx);
      } else {
        r = false;
      }
    }
    break;
  case tuple_type:
    {
      tuple *t = (tuple *)(i->constant);
      if (i->itstate <= t->nelt) {
        HANDLE ht = ref(t);
        xloc = (block *)new_integer(i->itstate);  hx = ref(xloc);
        floc = copy_value(tupelt(t,i->itstate));
        retire(hx);
        retire(ht);
      } else {
        r = false;
      }
    }
    break;
  default:
    unexpected (setl_type(i->constant));
  }
  *f = floc;
  *x = xloc;
  if (!r) i = OM;  /* iterator exhausted */
  retire(hi);
  *it = i;
  return r;
} /* end step_itersmap */

bool step_itermmap(iterator **it, block **x, set **f) {
  iterator *i = *it;  HANDLE hi = ref(i);
  bool r = true;
  set *s;
  subnode *b;
  assert (is_iterator(i));
  assert (i->itype == mmap_iter);
  s = (set *)(i->constant);
  assert (is_set(s));
  assert (s->stype == mmap);
  i->itstate++;
  if (ordsee(s->tblval, i->itstate, &b)) {
    HANDLE hb = ref(b);
    block *xloc = unkey(b->k);    HANDLE hx = ref(xloc);
    set   *floc = copy_set((set *)(b->d));
    retire(hx);
    retire(hb);
    *f = floc;
    *x = xloc;
    r = true;
  } else {
    *f = OM;
    *x = OM;
    i = OM;  /* iterator exhausted */
    r = false;
  }
  retire(hi);
  *it = i;
  return r;
} /* end step_itermmap */


/*
 *  See pages 42, 70, 71, and 102 of SDDS for more about f(x) things.
 *
 *  Note that subscripting of strings by regular expression (as in the
 *  'rex_fetch' call here) is an extension, and the result may be OM
 *  or a string that is not necessarily 1 character long.
 */
block *smap_fetch(block *host, block *sel) {
  HANDLE h = ref(host);
  HANDLE hsel;
  key *k;
  subnode *b;
  block *r;
  long i;
  if (is_om(sel)) runerr("Subscript (selector) is OM");
  switch (setl_type(host)) {
  case set_type:
    hsel = ref(sel);
    if (!promote((set *)host,mmap)) {
      runerr("Cannot select from non-map set");
    }
    switch (((set *)host)->stype) {
    case plain_set:
      unexpected (((set *)host)->stype);
    case mmap:
      k = tokey(sel);
      if (keysee(((set *)host)->tblval, k, &b)) {
        set *s = (set *)(b->d);
        assert (is_set(s));
        if (s->card == 1) {
          HANDLE hs = ref(s);
          subnode *c;
          demote (s,plain_set);
          retire(hs);
          check (ordsee(s->tblval, 1, &c));
          r = unkey(c->k);
        } else r = OM;
      } else r = OM;
      break;
    case smap:
      k = tokey(sel);
      if (keysee(((set *)host)->tblval, k, &b)) {
        r = copy_value(b->d);
      } else {
        r = OM;
      }
      break;
    default:
      unexpected (((set *)host)->stype);
    }
    retire(hsel);
    break;
  case string_type:
    switch (setl_type(sel)) {
    case integer_type:
      i = get_pos_long((integer *)sel, "string subscript");
      if (i > ((string *)host)->nchar) {
        r = (block *)null_string();
      } else {
        r = (block *)new_cstring(strelt((string *)host,i));
      }
      break;
    case string_type:
      r = (block *)rex_fetch((string *)host,(string *)sel);
      break;
    case tuple_type:
      /* not yet implemented, drop thru */
    default:
      runerr("Subscript of STRING must be INTEGER or STRING, not %s",
                                                       TYPENAME(sel));
    }
    break;
  case tuple_type:
    i = get_pos_long((integer *)sel, "tuple subscript");
    if (i > ((tuple *)host)->nelt) {
      r = OM;
    } else {
      r = copy_value(tupelt((tuple *)host,i));
    }
    break;
  default:
    runerr("Selection like p(x) requires SET (map), STRING, or TUPLE p,"
           " not %s", TYPENAME(host));
  }
  retire(h);
  return r;
} /* end smap_fetch */

/*
 *  Map operations are covered on pp 75-83 SDDS.
 */
set *mmap_fetch(set *host, block *sel) {
  HANDLE h = ref(host);
  HANDLE hsel = ref(sel);
  key *k;
  subnode *b;
  set *r;
  if (!is_set(host)) {
    runerr("Selection like p{x} requires SET (map) p, not %s",
                                               TYPENAME(host));
  }
  if (!promote(host,mmap)) runerr("Cannot mmap-select from non-map");
  if (is_om(sel)) runerr("Cannot mmap-select at OM");
  k = tokey(sel);
  switch (host->stype) {
  case mmap:
    if (keysee(host->tblval, k, &b)) r = copy_set((set *)(b->d));
    else r = null_set();
    break;
  case smap:
    if (keysee(host->tblval, k, &b)) r = singleton(b->d);
    else r = null_set();
    break;
  default:
    unexpected (host->stype);
  }
  retire(hsel);
  retire(h);
  return r;
} /* end mmap_fetch */

/*
 *  The basic rules about string slicing are on pp 42-43 SDDS,
 *  and about tuple slicing on pp 70-72.
 *
 *  The use of regular expressions to specify string slice bounds
 *  (as in the call to 'get_rexslice' here) is an extension.
 */
block *slice_fetch(block *host, block *sel1, block *sel2) {
  block *r;
  switch (setl_type(host)) {
  case string_type:
    {
      string *s = (string *)host;
      if (!(is_integer(sel1) || is_string(sel1)) ||
          !(is_integer(sel2) || is_string(sel2))) {
        runerr("String slice bounds must be of type INTEGER or STRING");
      }
      r = (block *)rex_getslice(s,sel1,sel2);
    }
    break;
  case tuple_type:
    {
      tuple *t = (tuple *)host;  HANDLE ht = ref(t);
      tuple *s;  HANDLE hs;
      long i,j,m,n,p;
      i = get_pos_long((integer *)sel1, "tuple slice lower index");
      j = get_nat_long((integer *)sel2, "tuple slice upper index");
      if (j < i-1) j = i-1;  /* an empty tuple slice */
      n = t->nelt;
      if (i > n) i = n+1;
      if (j > n) j = n;
      m = (j-i)+1;
      s = new_tuple(m);  hs = ref(s);
      for (p=1; p<=m; p++,i++) let (tupelt(s,p), copy_value(tupelt(t,i)));
      tup_truncate(&s);
      retire(hs);
      retire(ht);
      r = (block *)s;
    }
    break;
  default:
    runerr("Can only take slice of STRING or TUPLE, not %s",
                                             TYPENAME(host));
  }
  return r;
} /* end slice_fetch */


/*
 *  Each of the following insertion operations has its corresponding
 *  fetching operation above.
 *
 *  Extensions beyond SDDS are that assignment to a character in a
 *  string can change the string's length (the character is just a
 *  slice of length 1), and that slice bounds can be specified with
 *  regular expressions, as in the calls to 'rex_store' and
 *  'rex_insertslice' below.
 */

void smap_insert(block **host, block *sel, block *x) {
  block *r = *host;
  HANDLE hsel = ref(sel);
  HANDLE hx = ref(x);
  if (is_om(sel)) runerr("Cannot smap-insert at OM");
  switch (setl_type(r)) {
  case set_type:
    {
      set *s = (set *)r;  HANDLE hs = ref(s);
      subnode *b = NULL;  HANDLE hb = ref(b);
      key *k;
      if (!promote(s,mmap)) runerr("Cannot insert into non-map set");
      k = tokey(sel);
      switch (s->stype) {
      case mmap:
        if (x == OM) {
          if (keysee(s->tblval, k, &b)) {
            set *t = (set *)(b->d);
            assert (is_set(t));
            s->card -= t->card;
            keydel(s->tblval, k);
          }
        } else {
          if (keysub(s->tblval, k, &b)) {
            set *t = (set *)(b->d);
            assert (is_set(t));
            s->card -= t->card;
          }
          let (b->d, (block *)singleton(x));
          s->card++;
        }
        break;
      case smap:
        if (x == OM) {
          if (keydel(s->tblval, k)) s->card--;
        } else {
          if (!keysub(s->tblval, k, &b)) s->card++;
          let (b->d, copy_value(x));
        }
        break;
      default:
        unexpected (s->stype);
      }
      retire(hb);
      retire(hs);
      r = (block *)s;
    }
    break;
  case string_type:
    {
      long i;
      switch (setl_type(sel)) {
      case integer_type:
        i = get_pos_long((integer *)sel, "string subscript");
        if (!is_string(x)) {
          runerr("Character replacement must be STRING, not %s",
                                                    TYPENAME(x));
        }
        str_insert((string **)(void *)&r,i,i,(string *)x);
        break;
      case string_type:
        if (!is_string(x)) {
          runerr("Substring replacement must be STRING, not %s",
                                                    TYPENAME(x));
        }
        rex_store((string **)(void *)&r,(string *)sel,(string *)x);
        break;
      default:
        runerr("Subscript of STRING must be INTEGER or pattern STRING,"
               " not %s", TYPENAME(sel));
      }
    }
    break;
  case tuple_type:
    {
      tuple *t = (tuple *)r;  HANDLE ht = ref(t);
      long i = get_pos_long((integer *)sel, "tuple subscript");
      if (x == OM) {
        if (i <= t->nelt) {
          tupelt(t,i) = OM;
          tup_truncate(&t);
        }
      } else if (i > t->nelt) {
        if (x == r) {  /* t aliased to x; copy x before extending t */
          block *y = copy_value(x);  HANDLE hy = ref(y);
          tup_resize(&t,i);
          tupelt(t,i) = y;
          retire(hy);
        } else {
          tup_resize(&t,i);
          let (tupelt(t,i), copy_value(x));
        }
      } else {  /* not extending */
        let (tupelt(t,i), copy_value(x));
      }
      retire(ht);
      r = (block *)t;
    }
    break;
  default:
    runerr("Selection like p(x) requires SET (map), STRING, or TUPLE p,"
           " not %s", TYPENAME(r));
  }
  retire(hx);
  retire(hsel);
  *host = r;
} /* end smap_insert */

void mmap_insert(set **host, block *sel, set *x) {
  set *s = *host;  HANDLE hs = ref(s);
  HANDLE hsel = ref(sel);
  HANDLE hx = ref(x);
  key *k;
  subnode *b;   HANDLE hb;
  if (!is_set(s)) {
    runerr("Selection like p{x} requires SET (map) p, not %s",
                                                  TYPENAME(s));
  }
  if (!promote(s,mmap)) runerr("Cannot mmap-insert into non-map");
  if (is_om(sel)) runerr("Cannot mmap-insert at OM");
  if (!is_set(x)) {
    runerr("Can only mmap-insert SET, not %s", TYPENAME(x));
  }
  switch (s->stype) {
  case mmap:
    k = tokey(sel);
    if (keysub(s->tblval, k, &b)) {
      assert (is_set(b->d));
      s->card -= ((set *)(b->d))->card;
    }
    if (x->card > 0) {
      hb = ref(b);
      let (b->d, (block *)copy_set(x));
      s->card += x->card;
      retire(hb);
    } else {
      keydel(s->tblval, k);
    }
    break;
  case smap:
    demote(s,mmap);
    mmap_insert(&s, sel, x);
    break;
  default:
    unexpected (s->stype);
  }
  retire(hx);
  retire(hsel);
  retire(hs);
  *host = s;
} /* end mmap_insert */

void slice_insert(block **host, block *sel1, block *sel2, block *x) {
  long i,j;
  switch (setl_type(*host)) {
  case string_type:
    if (!(is_integer(sel1) || is_string(sel1)) ||
        !(is_integer(sel2) || is_string(sel2))) {
      runerr("String slice bounds must be of type INTEGER or STRING");
    }
    if (!is_string(x)) {
      runerr("String slice replacement must be STRING, not %s",
                                                   TYPENAME(x));
    }
    rex_insertslice((string **)host,sel1,sel2,(string *)x);
    break;
  case tuple_type:
    i = get_pos_long((integer *)sel1, "tuple slice lower index");
    j = get_nat_long((integer *)sel2, "tuple slice upper index");
    if (!is_tuple(x)) {
      runerr("Tuple slice replacement must be TUPLE, not %s",
                                                 TYPENAME(x));
    }
    if (j < i-1) j = i-1;  /* the empty tuple slice just before i */
    tup_insert((tuple **)host,i,j,(tuple *)x);
    break;
  default:
    runerr("Can only slice STRING or TUPLE, not %s", TYPENAME(*host));
  }
} /* end slice_insert */


block *copy_value(block *p) {
  switch (setl_type(p)) {
  case om_type:        return OM;
  case atom_type:      return (block *)copy_atom    ((atom     *)p);
  case boolean_type:   return (block *)copy_boolean ((boolean  *)p);
  case small_type:     return (block *)copy_small   ((small    *)p);
  case integer_type:   return (block *)copy_integer ((integer  *)p);
  case real_type:      return (block *)copy_real    ((real     *)p);
  case set_type:       return (block *)copy_set     ((set      *)p);
  case string_type:    return (block *)copy_string  ((string   *)p);
  case tuple_type:     return (block *)copy_tuple   ((tuple    *)p);
  case routine_type:   return (block *)copy_routine ((routine  *)p);
  case iterator_type:  return (block *)copy_iterator((iterator *)p);
  case table_type:     return (block *)copy_table   ((table    *)p);
  case subnode_type:   unexpected (setl_type(p));  /* see copy_table */
  case key_type:       return (block *)copy_key     ((key      *)p);
  case source_type:    return (block *)copy_source  ((source   *)p);
  case codeline_type:  return (block *)copy_codeline((codeline *)p);
  case instr_type:     return (block *)copy_instr   ((instr    *)p);
  case parm_type:      return (block *)copy_parm    ((parm     *)p);
  case frame_type:     return (block *)copy_frame   ((frame    *)p);
  case symtab_type:    return (block *)copy_symtab  ((symtab   *)p);
  case machine_type:   return (block *)copy_machine ((machine  *)p);
  default:             unexpected (setl_type(p));  /* bad block type */
  }
} /* end copy_value */

atom *copy_atom(atom *p) {
  HANDLE hp;
  atom *r;
  assert (is_atom(p));
  hp = ref(p);
  r = fblock(atom);
  r->atmval = p->atmval;
  retire(hp);
  return r;
}

boolean *copy_boolean(boolean *p) {
  HANDLE hp;
  boolean *r;
  assert (is_boolean(p));
  hp = ref(p);
  r = fblock(boolean);
  r->booval = p->booval;
  retire(hp);
  return r;
}

small *copy_small(small *p) {
  HANDLE hp;
  small *r;
  assert (is_small(p));
  hp = ref(p);
  r = fblock(small);
  r->weeval = p->weeval;
  retire(hp);
  return r;
}

integer *copy_integer(integer *p) {
  HANDLE hp;
  integer *r;
  ssize_t size;
  size_t nlimb;
  assert (is_integer(p));
  hp = ref(p);
  size = p->size;
  nlimb = ABS(size);
  r = alloc_integer(nlimb);
  r->size = size;
  mvmem (r->limbs, p->limbs, nlimb * sizeof p->limbs[0]);
  retire(hp);
  return r;
}

real *copy_real(real *p) {
  HANDLE hp;
  real *r;
  assert (is_real(p));
  hp = ref(p);
  r = fblock(real);
  r->realval = p->realval;
  retire(hp);
  return r;
}

set *copy_set(set *p) {
  HANDLE hp;
  set *r;  HANDLE hr;
  assert (is_set(p));
  hp = ref(p);
  r = fblock(set);  hr = ref(r);
  r->card  = p->card;
  r->stype = p->stype;
  r->tblval = NULL;  /* in case copy_table garbage-collects */
  let (r->tblval, copy_table(p->tblval));
  retire(hr);
  retire(hp);
  return r;
}

string *copy_string(string *p) {
  HANDLE hp;
  string *r;
  long nchar;
  assert (is_string(p));
  hp = ref(p);
  nchar = p->nchar;
  r = alloc_string(nchar,0,1);  /* nchar, nbefore, nafter */
  mvmem (&strelt(r,1), &strelt(p,1), nchar);
  strelt(r,nchar+1) = '\0';
  retire(hp);
  return r;
}

tuple *copy_tuple(tuple *p) {
  HANDLE hp;
  tuple *r;  HANDLE hr;
  long nelt, i;
  assert (is_tuple(p));
  hp = ref(p);
  nelt = p->nelt;
  r = alloc_tuple(nelt,0,0);  /* nelt, npre, nsuf */
  hr = ref(r);
  for (i=1; i<=nelt; i++) {
    let (tupelt(r,i), copy_value(tupelt(p,i)));
  }
  retire(hr);
  retire(hp);
  return r;
}

routine *copy_routine(routine *p) {
  HANDLE hp;
  routine *r;
  assert (is_routine(p));
  hp = ref(p);
  r = fblock(routine);
  r->proc_pc = p->proc_pc;
  retire(hp);
  return r;
}

iterator *copy_iterator(iterator *p) {
  HANDLE hp;
  iterator *r;  HANDLE hr;
  assert (is_iterator(p));
  hp = ref(p);
  r = fblock(iterator);  hr = ref(r);
  r->itype     = p->itype;
  r->itstate   = p->itstate;
  r->constant = NULL;  /* in case the copy_value garbage-collects */
  let (r->constant, copy_value(p->constant));
  r->increment = p->increment;
  r->limit     = p->limit;
  retire(hr);
  retire(hp);
  return r;
}

table *copy_table(table *p) {
  table *r;
  assert (is_table(p));
  patcpy(&r,p);
  return r;
}

key *copy_key(key *p) {
  HANDLE hp;
  key *r;
  size_t nbytes;
  assert (is_key(p));
  hp = ref(p);
  nbytes = (size_t) nbits_to_nbytes(p->length);
  r = vblock(key, nbytes);
  r->length = p->length;
  mvmem (r->bstring, p->bstring, nbytes);
  retire(hp);
  return r;
}

source *copy_source(source *p) {
  HANDLE hp;
  source *r;  HANDLE hr;
  assert (is_source(p));
  hp = ref(p);
  r = fblock(source);  hr = ref(r);
  r->filename = NULL;  /* guard against g.c., as usual... */
  r->srctext = NULL;
  if (p->filename != NULL) let (r->filename, copy_string(p->filename));
  if (p->srctext  != NULL) let (r->srctext,  copy_string(p->srctext));
  retire(hr);
  retire(hp);
  return r;
}

codeline *copy_codeline(codeline *p) {
  HANDLE hp;
  codeline *r;  HANDLE hr;
  assert (is_codeline(p));
  hp = ref(p);
  r = fblock(codeline);  hr = ref(r);
  r->srcnum = p->srcnum;
  r->offset = p->offset;
  r->text = NULL;
  let (r->text, copy_string(p->text));
  retire(hr);
  retire(hp);
  return r;
}

instr *copy_instr(instr *p) {
  HANDLE hp;
  instr *r;
  long nopnd, i;
  assert (is_instr(p));
  hp = ref(p);
  nopnd = p->nopnd;
  r = vblock(instr, nopnd);
  r->op     = p->op;
  r->nopnd  = nopnd;
  r->vindex = p->vindex;
  for (i=1; i<=nopnd; i++) instrelt(r,i) = instrelt(p,i);
  retire(hp);
  return r;
}

parm *copy_parm(parm *p) {
  HANDLE hp;
  parm *r;
  assert (is_parm(p));
  hp = ref(p);
  r = fblock(parm);
  r->parmnum = p->parmnum;
  r->slot    = p->slot;
  retire(hp);
  return r;
}

frame *copy_frame(frame *p) {
  HANDLE hp;
  frame *r;  HANDLE hr;
  long nloc, i;
  assert (is_frame(p));
  hp = ref(p);
  nloc = p->nloc;
  r = vblock(frame, nloc);
  hr = ref(r);
  r->nloc         = nloc;
  r->proc_pc      = p->proc_pc;
  r->caller_pc    = p->caller_pc;
  r->caller_level = p->caller_level;
  r->nargs        = p->nargs;
  r->link         = NULL;
  r->combiter     = NULL;
  for (i=1; i<=nloc; i++) framelt(r,i) = NULL;
  if (p->link) let (r->link, copy_frame(p->link));
  if (p->combiter) let (r->combiter, copy_iterator(p->combiter));
  for (i=1; i<=nloc; i++) {
    let (framelt(r,i), copy_value(framelt(p,i)));
  }
  retire(hr);
  retire(hp);
  return r;
}

/*
 *  The symtab design is junky and obsolete, and the symtab itself is
 *  scarcely used, but in case someone wants to clone a machine, its
 *  symtab should be copied too...
 */
symtab *copy_symtab(symtab *p) {
  HANDLE hp;
  symtab *r;  HANDLE hr;
  assert (is_symtab(p));
  hp = ref(p);
  r = fblock(symtab);
  hr = ref(r);
  r->procnames    = NULL;
  r->procnums     = NULL;
  r->procaddrs    = NULL;
  r->globals      = NULL;
  r->locals       = NULL;
  r->labels       = NULL;
  r->gbl_backtrax = NULL;
  r->lcl_backtrax = NULL;
  r->vcode        = NULL;
  r->sources      = NULL;
  if (p->procnames)    let (r->procnames,    copy_tuple(p->procnames));
  if (p->procnums)     let (r->procnums,     copy_table(p->procnums));
  if (p->procaddrs)    let (r->procaddrs,    copy_table(p->procaddrs));
  if (p->globals)      let (r->globals,      copy_table(p->globals));
  if (p->locals)       let (r->locals,       copy_table(p->locals));
  if (p->labels)       let (r->labels,       copy_table(p->labels));
  if (p->gbl_backtrax) let (r->gbl_backtrax, copy_table(p->gbl_backtrax));
  if (p->lcl_backtrax) let (r->lcl_backtrax, copy_table(p->lcl_backtrax));
  if (p->vcode)        let (r->vcode,        copy_tuple(p->vcode));
  if (p->sources)      let (r->sources,      copy_tuple(p->sources));
  retire(hr);
  retire(hp);
  return r;
}

/*
 *  The ticklish bit in copying ("cloning") a SETL virtual machine is
 *  that in general, frames (activation records) can be reached in two
 *  ways, either by the link from a callee's frame to a caller's, or
 *  from the display (indexed by lexical scope level) in the instance
 *  (machine) record.  So the technique is to start with the "top"
 *  frame as indicated by the current lexical scope level, descend
 *  through the 'link' fields, and for each frame encountered, scan
 *  through the 'display' to see if any of its entries point to that
 *  frame.  If so, the new version of such an entry is set to point
 *  to the new frame.
 */
machine *copy_machine(machine *p) {
  HANDLE hp;
  machine *r;  HANDLE hr;
  long level;
  assert (is_machine(p));
  hp = ref(p);
  r = fblock(machine);  /* 'vblock' for unbounded routine nesting */
  hr = ref(r);
  r->pc          = p->pc;
  r->level       = p->level;
  r->exit_status = p->exit_status;
  r->raw_status  = p->raw_status;
  r->eof         = p->eof;
  r->magic       = p->magic;
  r->intslash    = p->intslash;
  r->code        = NULL;
  r->backs       = NULL;
  r->sym         = NULL;
  r->ready_maps  = NULL;
  for (level=0; level<DISPLAY_LEVELS; level++) r->display[level] = NULL;
  if (p->code)  let (r->code,  copy_tuple(p->code));
  if (p->backs) let (r->backs, copy_tuple(p->backs));
  if (p->sym)   let (r->sym,  copy_symtab(p->sym));
  if (p->ready_maps) let (r->ready_maps, copy_tuple(p->ready_maps));
  if (p->display[p->level] != NULL) {
    frame *g = copy_frame(p->display[p->level]);
    frame *f = p->display[p->level];
    while (f != NULL) {
      for (level=0; level<DISPLAY_LEVELS; level++) {
        if (f == p->display[level]) r->display[level] = g;
      }
      f = f->link;
      g = g->link;
    }
  }
  retire(hr);
  retire(hp);
  return r;
}

bool equal_value (block *p, block *q) {
  /* Please resist any temptation to "optimize" by returning true
   * immediately if p == q.  That would be incorrect if p and q are
   * both NaN (or -NaN), and in fact one of the few reasons to write
   * the expression x = x (compare x to itself) in a SETL program is
   * in order to check whether x is REAL but Not a Number.  */
  if (setl_type(p) != setl_type(q)) {
    if        (is_real(p) && is_integer(q)) {
      return real_to_double((real *)p) == integer_to_double((integer *)q);
    } else if (is_integer(p) && is_real(q)) {
      return integer_to_double((integer *)p) == real_to_double((real *)q);
    } else {
      return false;
    }
  }
  switch (setl_type(p)) {
  case om_type:        return true;
  case atom_type:      return equal_atom    ((atom     *)p,
                                             (atom     *)q);
  case boolean_type:   return equal_boolean ((boolean  *)p,
                                             (boolean  *)q);
  case small_type:     return equal_small   ((small    *)p,
                                             (small    *)q);
  case integer_type:   return equal_integer ((integer  *)p,
                                             (integer  *)q);
  case real_type:      return equal_real    ((real     *)p,
                                             (real     *)q);
  case set_type:       return equal_set     ((set      *)p,
                                             (set      *)q);
  case string_type:    return equal_string  ((string   *)p,
                                             (string   *)q);
  case tuple_type:     return equal_tuple   ((tuple    *)p,
                                             (tuple    *)q);
  case routine_type:   return equal_routine ((routine  *)p,
                                             (routine  *)q);
  case iterator_type:  /* caller, why are you comparing iterators? */
                       unexpected (setl_type(p));
  case table_type:     return equal_table   ((table    *)p,
                                             (table    *)q);
  case subnode_type:   /* should be subsumed within table comparisons */
                       unexpected (setl_type(p));
  case key_type:       return equal_key     ((key      *)p,
                                             (key      *)q);
  case source_type:    return equal_source  ((source   *)p,
                                             (source   *)q);
  case codeline_type:
  case instr_type:
  case parm_type:
  case frame_type:
  case symtab_type:
  case machine_type:
    unexpected (setl_type(p));  /* unsupported comparison operation */
  default:
    unexpected (setl_type(p));  /* unknown block type */
  }
} /* end equal_value */

bool equal_atom (atom *p, atom *q) {
  assert (is_atom(p));
  assert (is_atom(q));
  return p->atmval ==
         q->atmval;
}

bool equal_boolean (boolean *p, boolean *q) {
  assert (is_boolean(p));
  assert (is_boolean(q));
  return p->booval ==
         q->booval;
}

bool equal_small (small *p, small *q) {
  assert (is_small(p));
  assert (is_small(q));
  return p->weeval ==
         q->weeval;
}

bool equal_integer (integer *p, integer *q) {
  assert (is_integer(p));
  assert (is_integer(q));
  return integer_cmp(p, q) == 0;
}

bool equal_string (string *p, string *q) {
  long nchar;
  assert (is_string(p));
  assert (is_string(q));
  nchar = p->nchar;
  if (nchar != q->nchar) return false;
  return memcmp(&strelt(p,1),
                &strelt(q,1), nchar) == 0;
}

bool equal_real (real *p, real *q) {
  assert (is_real(p));
  assert (is_real(q));
  return real_to_double(p) ==
         real_to_double(q);
}

bool equal_set (set *p, set *q) {
  HANDLE hp, hq;
  settype least;
  bool r;
  assert (is_set(p));
  assert (is_set(q));
  if (p->card !=
      q->card) return false;
  least = MIN(p->stype,
              q->stype);
  hp = ref(p);
  hq = ref(q);
  demote(p, least);
  demote(q, least);
  r = equal_table(p->tblval,
                  q->tblval);
  retire(hq);
  retire(hp);
  return r;
}

bool equal_tuple (tuple *p, tuple *q) {
  /*
   *  Assume we are being called with truncated tuples (or at
   *  least that if one or the other isn't truncated, a mere
   *  length difference can be counted as a difference; in
   *  practice, the stronger assumption should actually be true
   *  but is not worth checking).
   */
  HANDLE hp, hq;
  long nelt, i;
  bool r;
  assert (is_tuple(p));
  assert (is_tuple(q));
  nelt = p->nelt;
  if (nelt != q->nelt) return false;
  hp = ref(p);
  hq = ref(q);
  r = true;
  for (i=1; r && i<=nelt; i++) {
    r = equal_value(tupelt(p,i),
                    tupelt(q,i));
  }
  retire(hq);
  retire(hp);
  return r;
}

bool equal_routine (routine *p, routine *q) {
  assert (is_routine(p));
  assert (is_routine(q));
  return p->proc_pc ==
         q->proc_pc;
}

bool equal_table (table *p, table *q) {
  assert (is_table(p));
  assert (is_table(q));
  return patcmp(p,q) == 0;
}

bool equal_key (key *p, key *q) {
  size_t nbytes;
  assert (is_key(p));
  assert (is_key(q));
  if (p->length !=
      q->length) return false;
  nbytes = nbits_to_nbytes(p->length);
  return memcmp(p->bstring,
                q->bstring, nbytes) == 0;
}

bool equal_source (source *p, source *q) {
  source *ps = (source *)p;
  source *qs = (source *)q;
  return equal_string(ps->filename,
                      qs->filename) &&
         equal_string(ps->srctext,
                      qs->srctext);
}


#define compare_scalars(a,b) \
  ((a)<(b) ? A_LT_B :        \
   (a)>(b) ? A_GT_B :        \
             A_EQ_B)

static int compare_longs (long a, long b) {
  return compare_scalars(a, b);
}

static int compare_doubles (double a, double b) {
  if (isnan(a)) return A_IS_NAN;
  if (isnan(b)) return B_IS_NAN;
  return compare_scalars(a, b);
}

int compare_values (block *a, block *b, const char *opsym) {
  if (is_integer(a) &&
      is_integer(b)) {
    int c = integer_cmp((integer *)a, (integer *)b);
    return SIGN(c);  /* A_LT_B, A_EQ_B, or A_GT_B */
  }
  if (is_real(a) &&
      is_real(b)) {
    double da = real_to_double((real *)a);
    double db = real_to_double((real *)b);
    return compare_doubles(da,db);
  }
  if (is_integer(a) &&
      is_real(b)) {
    double da = integer_to_double((integer *)a);
    double db =    real_to_double((real    *)b);
    return compare_doubles(da,db);
  }
  if (is_real(a) &&
      is_integer(b)) {
    double da =    real_to_double((real    *)a);
    double db = integer_to_double((integer *)b);
    return compare_doubles(da,db);
  }
  if (is_string(a) &&
      is_string(b)) {
    string *sa = (string *)a;
    string *sb = (string *)b;
    int k = memcmp(&strelt(sa,1),
                   &strelt(sb,1),MIN(sa->nchar,
                                     sb->nchar));
    return k != 0 ? SIGN(k) : compare_longs (sa->nchar, sb->nchar);
  }
  if (is_tuple(a) &&
      is_tuple(b)) {
    tuple *ta = (tuple *)a;
    tuple *tb = (tuple *)b;
    long n = MIN(ta->nelt,tb->nelt);
    long i;
    for (i=1; i<=n; i++) {
      int k = compare_values (tupelt(ta,i), tupelt(tb,i), opsym);
      if (k != A_EQ_B) return k;
    }
    return compare_longs (ta->nelt, tb->nelt);
  }
  runerr("\"%s %s %s\" is invalid", TYPENAME(a), opsym, TYPENAME(b));
} /* end compare_values */


key *block_to_key(block *p) {
  HANDLE h = ref(p);
  key *k;
  size_t nbytes, ik;
  if (is_om(p)) runerr("OM cannot be a key");
  nbytes = nbytes_in_key(p);
  if (nbytes > (size_t)(LONG_MAX / CHAR_BIT)) {
    runerr("Key too large for this implementation on this platform");
  }
  k = vblock(key,nbytes);
  k->length = nbytes_to_nbits(nbytes);
  ik = cvt_to_key(k,p,0);
  assert (ik == nbytes);
  retire(h);
  return k;
} /* end block_to_key */

static const keytype om_k      = om_key;
static const keytype atom_k    = atom_key;
static const keytype boolean_k = boolean_key;
static const keytype integer_k = integer_key;
static const keytype real_k    = real_key;
static const keytype routine_k = routine_key;
static const keytype set_k     = set_key;
static const keytype small_k   = small_key;
static const keytype string_k  = string_key;
static const keytype tuple_k   = tuple_key;

static size_t nbytes_in_key(block *p) {
  switch (setl_type(p)) {
  case om_type:
    return sizeof om_k;
  case atom_type:
    return sizeof atom_k +
           sizeof ((atom *)p)->atmval;
  case boolean_type:
    return sizeof boolean_k +
           1;
  case small_type:
    return sizeof small_k +
           sizeof ((small *)p)->weeval;
  case integer_type:
    {
      integer *i = (integer *)p;
      return sizeof integer_k +
             sizeof i->size +
             sizeof_limbs(i);
    }
  case real_type:
    return sizeof real_k +
           sizeof ((real *)p)->realval;
  case routine_type:
    return sizeof routine_k +
           sizeof ((routine *)p)->proc_pc;
  case set_type:
    {
      size_t nbytes;
      long i, n;
      set *a = (set *)p;
      HANDLE h = ref(a);
      demote(a,plain_set);
      nbytes = sizeof set_k + sizeof n;
      n = patsiz(a->tblval);
      assert (n == a->card);
      for (i=1; i<=n; i++) {
        subnode *b;
        check (ordsee(a->tblval, i, &b));
        assert (is_key(b->k));
        nbytes += nbits_to_nbytes(b->k->length);
      }
      retire(h);
      return nbytes;
    }
  case string_type:
    return sizeof string_k +
           (((string *)p)->nchar + 7) / 7 * 8;
  case tuple_type:
    {
      const tuple *t = (const tuple *)p;
      HANDLE h = ref(t);
      long i, n = t->nelt;
      size_t nbytes = sizeof tuple_k + sizeof n;
      for (i=1; i<=n; i++) nbytes += nbytes_in_key(tupelt(t,i));
      retire(h);
      return nbytes;
    }
  default:
    unexpected (setl_type(p));
  }
} /* end nbytes_in_key */

static size_t cvt_to_key(key *k, block *p, size_t ik) {

#define copy_vpart(kp,n) do {\
  mvmem(&k->bstring[ik], &(kp), n);\
  ik += (n);\
} while (0)
#define copy_keypart(kp)  copy_vpart(kp, sizeof (kp))

#define zero_vpart(n) do {\
  if ((n) > 0) {\
    memset(&k->bstring[ik], 0, n);\
    ik += (n);\
  }\
} while (0)

  switch (setl_type(p)) {
  case om_type:
    copy_keypart(om_k);
    break;
  case atom_type:
    copy_keypart(atom_k);
    {
      const atom *a = (const atom *)p;
      const long v = BE_LONG(a->atmval);
      copy_keypart(v);
    }
    break;
  case boolean_type:
    copy_keypart(boolean_k);
    k->bstring[ik++] = ((boolean *)p)->booval;
    break;
  case small_type:
    copy_keypart(small_k);
    {
      const small *a = (const small *)p;
      const long v = BE_LONG(a->weeval);
      copy_keypart(v);
    }
    break;
  case integer_type:
    copy_keypart(integer_k);
    {
      const integer *a = (const integer *)p;
      const ssize_t size = a->size;  /* includes sign */
      const size_t sz = BE_SIZE_T(size ^ SIZE_T_HIGHBIT);
      const size_t nlimb = ABS(size);
      const mp_limb_t mask = size < 0 ? GMP_NUMB_MASK : 0;
      size_t i;
      copy_keypart(sz);
      for (i=nlimb; i--; ) {
        const mp_limb_t limb = BE_LIMB(a->limbs[i] ^ mask);
        copy_keypart(limb);
      }
    }
    break;
  case real_type:
    copy_keypart(real_k);
    {
      const real *a = (const real *)p;
      union {int_double_holder i; double d;} v;
      v.d = a->realval;
      if (v.i < 0) {
        v.i = ~v.i;  /* complement all bits */
      } else {
        v.d = -v.d;  /* complement just the sign bit */
      }
      v.i = BE_DOUBLE(v.i);
      copy_keypart(v.i);
    }
    break;
  case routine_type:
    copy_keypart(routine_k);
    {
      const routine *a = (const routine *)p;
      const long v = BE_LONG(a->proc_pc);
      copy_keypart(v);
    }
    break;
  case set_type:
    copy_keypart(set_k);
    {
      long i, n, m;
      const set *a = (const set *)p;
      assert (a->stype == plain_set);
      n = patsiz(a->tblval);
      assert (n == a->card);
      m = BE_LONG(n);
      copy_keypart(m);
      for (i=1; i<=n; i++) {
        subnode *b;
        size_t nbytes;
        check (ordsee(a->tblval, i, &b));
        assert (is_key(b->k));
        nbytes = nbits_to_nbytes(b->k->length);
        copy_vpart(b->k->bstring, nbytes);
      }
    }
    break;
  case string_type:
    copy_keypart(string_k);
    {
      const string *a = (const string *)p;
      const long n = a->nchar + 1;
      const long m = (n - 1) % 7 + 1;
      long i;
      for (i=1; i<=n-7; i+=7) {
        copy_vpart(strelt(a,i), 7);
        k->bstring[ik++] = '\0';
      }
      assert (n-i+1 == m);
      copy_vpart(strelt(a,i), m);
      zero_vpart(7-m);
      k->bstring[ik++] = m;
    }
    break;
  case tuple_type:
    copy_keypart(tuple_k);
    {
      const tuple *a = (const tuple *)p;
      const long n = a->nelt;
      const long m = BE_LONG(n);
      long i;
      copy_keypart(m);
      for (i=1; i<=n; i++) ik = cvt_to_key(k,tupelt(a,i),ik);
    }
    break;
  default:
    unexpected (setl_type(p));
  } /* end switch */
  return ik;
#undef zero_vpart
#undef copy_vpart
#undef copy_keypart
} /* end cvt_to_key */

block *unkey(key *k) {
  block *b;
  size_t nbytes, ik = 0;
  assert (is_key(k));
  nbytes = nbits_to_nbytes(k->length);
  b = cvt_from_key(k,&ik);
  assert (ik == nbytes);
  return b;
}

static block *cvt_from_key(key *k, size_t *pik) {

#define copy_vpart(kp,n) do {\
  mvmem(&(kp), &k->bstring[ik], n);\
  ik += (n);\
} while (0)
#define copy_keypart(kp)  copy_vpart(kp, sizeof (kp))

  HANDLE h;
  block *b;
  size_t nbytes, ik;
  keytype ktype;
  assert (is_key(k));
  nbytes = nbits_to_nbytes(k->length);
  ik = *pik;
  assert (ik < nbytes);
  h = ref(k);
  copy_keypart(ktype);
  switch (ktype) {
  case om_key:
    b = OM;
    break;
  case atom_key:
    {
      atom *r = fblock(atom);
      long v;
      copy_keypart(v);
      r->atmval = BE_LONG(v);
      b = (block *)r;
    }
    break;
  case boolean_key:
    {
      boolean *r = fblock(boolean);
      r->booval = k->bstring[ik++];
      b = (block *)r;
    }
    break;
  case small_key:
    {
      small *r = fblock(small);
      long v;
      copy_keypart(v);
      r->weeval = BE_LONG(v);
      b = (block *)r;
    }
    break;
  case integer_key:
    {
      integer *r;
      size_t sz, nlimb, i;
      ssize_t size;
      mp_limb_t mask;
      copy_keypart(sz);
      size = (ssize_t)(BE_SIZE_T(sz) ^ SIZE_T_HIGHBIT);
      nlimb = ABS(size);
      mask = size < 0 ? GMP_NUMB_MASK : 0;
      r = alloc_integer(nlimb);
      r->size = size;
      for (i=nlimb; i--; ) {
        mp_limb_t limb;
        copy_keypart(limb);
        r->limbs[i] = BE_LIMB(limb) ^ mask;
      }
      b = (block *)r;
    }
    break;
  case real_key:
    {
      real *r = fblock(real);
      union {int_double_holder i; double d;} v;
      copy_keypart(v.i);
      v.i = BE_DOUBLE(v.i);
      if (v.i < 0) {
        v.d = -v.d;  /* complement just the sign bit */
      } else {
        v.i = ~v.i;  /* complement all bits */
      }
      r->realval = v.d;
      b = (block *)r;
    }
    break;
  case routine_key:
    {
      routine *r = fblock(routine);
      long v;
      copy_keypart(v);
      r->proc_pc = BE_LONG(v);
      b = (block *)r;
    }
    break;
  case set_key:
    {
      set *r = null_set();
      HANDLE hr = ref(r);
      long i, n;
      copy_keypart(n);
      n = BE_LONG(n);
      for (i=1; i<=n; i++) {
        block *t = cvt_from_key(k,&ik);
        set_insert(&r,t);
      }
      retire(hr);
      b = (block *)r;
    }
    break;
  case string_key:
    {
      string *r;
      long i, n = 0;
      long m;
      size_t jk = ik+7;
      while (k->bstring[jk] == '\0') {
        jk += 8;
        n += 7;
      }
      m = k->bstring[jk];
      n += m;
      r = new_estring(n-1);
      for (i=1; i<=n-7; i+=7) {
        copy_vpart(strelt(r,i), 7);
        ik++;
      }
      assert (n-i+1 == m);
      copy_vpart(strelt(r,i), m);  /* includes trailing NUL */
      ik += 8-m;
      assert (ik == jk+1);
      b = (block *)r;
    }
    break;
  case tuple_key:
    {
      tuple *r;
      HANDLE hr;
      long i, n;
      copy_keypart(n);
      n = BE_LONG(n);
      r = new_tuple(n);
      hr = ref(r);
      for (i=1; i<=n; i++) {
        block *t = cvt_from_key(k,&ik);
        tupelt(r,i) = t;
      }
      tup_truncate(&r);
      retire(hr);
      b = (block *)r;
    }
    break;
  default:
    unexpected (ktype);
  }
  retire(h);
  *pik = ik;
  return b;
#undef copy_vpart
#undef copy_keypart
} /* end cvt_from_key */


/*
 *  The essence of IP_ADDRESSES.
 *
 *  If errno is set, it is by gai_errno() when getaddrinfo() fails on
 *  the given host (name or addr).  The empty set is returned in that
 *  case.
 */
set *ip_addresses(string *host) {
  HANDLE h_host = ref(host);
  set *r = null_set();
  const char *name_or_addr = &strelt(host,1);
  struct addrinfo *head;
  /* No hints here.  For this case, we are being inclusive about what we
   * accept from getaddrinfo().  */
  if (os_getaddrinfo (name_or_addr, NULL, NULL, &head) == 0) {
    HANDLE hr = ref(r);
    const struct addrinfo *ai;
    for (ai=head; ai != NULL; ai=ai->ai_next) {
      if (ai->ai_family == AF_INET ||
          ai->ai_family == AF_INET6) {
        string *s = sockaddr_address(ai->ai_addr, ai->ai_addrlen);
        set_insert(&r, (block *)s);
      }
    }
    os_freeaddrinfo(head);  /* errno remains unchanged here */
    retire(hr);
  }
  retire(h_host);
  return r;
} /* end ip_addresses */

/*
 *  The essence of IP_NAMES.
 *
 *  If errno is set, it is by gai_errno() when getaddrinfo() fails on
 *  the given host (name or addr).  The empty set is returned in that
 *  case.
 */
set *ip_names(string *host) {
  HANDLE h_host = ref(host);
  set *r = null_set();
  const char *name_or_addr = &strelt(host,1);
  struct addrinfo *head;
  /* No hints here.  For this case, we are being inclusive about what we
   * accept from getaddrinfo().  */
  if (os_getaddrinfo (name_or_addr, NULL, NULL, &head) == 0) {
    int saved_errno = errno;  /* against sockaddr_name() failures */
    HANDLE hr = ref(r);
    const struct addrinfo *ai;
    for (ai=head; ai != NULL; ai=ai->ai_next) {
      if (ai->ai_family == AF_INET ||
          ai->ai_family == AF_INET6) {
        string *s = sockaddr_name(ai->ai_addr, ai->ai_addrlen);
        set_insert(&r, (block *)s);
      }
    }
    os_freeaddrinfo(head);
    retire(hr);
    errno = saved_errno;
  }
  retire(h_host);
  return r;
} /* end ip_names */

/*
 *  A hostname for the given Internet address.
 *
 *  If errno is set, it is by gai_errno() when getnameinfo() fails on
 *  the given address.  OM is returned in that case.
 */
string *sockaddr_name (const struct sockaddr *sa, socklen_t salen) {
  char hostname[NI_MAXHOST+1];
  hostname[0] = '\0';  /* in case getnameinfo() fails */
  if (os_getnameinfo (sa,salen, hostname,NI_MAXHOST, NULL,0,
                                         NI_NAMEREQD) == 0) {
    return new_string(hostname);  /* empty string not disallowed */
  } else {
    return OM;  /* with errno set by getnameinfo() */
  }
} /* end sockaddr_name */

/*
 *  Host address part of the given Internet address, as a string.
 */
string *sockaddr_address (const struct sockaddr *sa, socklen_t salen) {
  char addr[NI_MAXHOST+1];
  addr[0] = '\0';  /* paranoia */
  if (os_getnameinfo (sa,salen, addr,NI_MAXHOST, NULL,0,
                                     NI_NUMERICHOST) == 0) {
    return new_string(addr);
  } else {  /* errno was set by getnameinfo() */
    os_error("getnameinfo on NI_NUMERICHOST");
  }
#if 0  /* alternate implementation */
  switch (sa->sa_family) {
  case AF_INET:
    {
      const struct sockaddr_in *sin4 = (const struct sockaddr_in *)sa;
      char buf[INET_ADDRSTRLEN];
      assert (salen == sizeof *sin4);
      check (inet_ntop (AF_INET, &sin4->sin_addr, buf, sizeof buf));
      return new_string(buf);
    }
  case AF_INET6:
    {
      const struct sockaddr_in6 *sin6 = (const struct sockaddr_in6 *)sa;
      char buf[INET6_ADDRSTRLEN];
      assert (salen == sizeof *sin6);
      check (inet_ntop (AF_INET6, &sin6->sin6_addr, buf, sizeof buf));
      return new_string(buf);
    }
  default:
    unexpected (sa->sa_family);
  }
#endif
} /* end sockaddr_address */

/*
 *  Port number from the given Internet address, as an integer.
 */
integer *sockaddr_portnum (const struct sockaddr *sa, socklen_t salen) {
  char port[NI_MAXSERV+1];
  uint16_t portnum;
  int saved_errno = errno;
  errno = 0;
  port[0] = '\0';  /* against the impossible getnameinfo() failure */
  /* This fancy-looking call really just converts the 16-bit port
   * number in 'sa' to a string of digits in 'port', but in a portable
   * way (ha, ha) that parallels the other getnameinfo() calls:  */
  os_getnameinfo (sa,salen, NULL,0, port,NI_MAXSERV, NI_NUMERICSERV);
  portnum = strtoul(port, NULL, 10);
  assert (errno == 0);  /* else bogus port number in 'port' */
  errno = saved_errno;
  return ulong_integer(portnum);
#if 0  /* alternate implementation */
  uint16_t portnum;
  switch (sa->sa_family) {
  case AF_INET:
    {
      const struct sockaddr_in *sin4 = (const struct sockaddr_in *)sa;
      assert (salen == sizeof *sin4);
      portnum = ntohs (sin4->sin_port);
    }
    break;
  case AF_INET6:
    {
      const struct sockaddr_in6 *sin6 = (const struct sockaddr_in6 *)sa;
      assert (salen == sizeof *sin6);
      portnum = ntohs (sin6->sin6_port);
    }
    break;
  default:
    unexpected (sa->sa_family);
  }
  return ulong_integer(portnum);
#endif
} /* end sockaddr_portnum */

/*
 *  [Host address, port number] from the given Internet address.
 */
tuple *sockaddr_tuple (const struct sockaddr *sa, socklen_t salen) {
  tuple *r = new_tuple(2);  HANDLE hr = ref(r);
  let (tupelt(r,1), (block *) sockaddr_address(sa, salen));
  let (tupelt(r,2), (block *) sockaddr_portnum(sa, salen));
  retire(hr);
  return r;
} /* end sockaddr_tuple */

/*
 *  Pathname from Unix-domain sockaddr.
 */
string *sockaddr_pathname (const struct sockaddr *sa, socklen_t salen) {
  const struct sockaddr_un *sockname = (const struct sockaddr_un *)sa;
  const socklen_t minlen = offsetof(struct sockaddr_un, sun_path);
  return salen >= minlen ? new_nstring(sockname->sun_path, salen - minlen)
                         : null_string();
} /* end sockaddr_pathname */

/*
 *  The (almost) all-purpose signal handler.
 *
 *  SIGCHLD gets its own sigchld_handler() (see init.c), which calls
 *  ("hooks") this handler as its first act.
 *
 *  SIGALRM gets alarm_handler() (see below), but since the only
 *  SETL interface to that signal is via the 'REAL-MS' mode of OPEN
 *  (though SIGALRM is allowed to be OPENed in 'IGNORE' and 'DEFAULT'
 *  mode), alarm_handler() does not hook this handler.
 *
 *  Since the supported signals are not the so-called "real-time"
 *  POSIX signals, they are not required to be queuing (and in fact
 *  are not, on Linux), so there is the theoretical possibility of
 *  missed signal events due to an inability to keep up (from CPU
 *  starvation or periods of suspension, say).
 *
 *  There is also the theoretical possibility of the event counter
 *  (a long) wrapping around if the SETL program subscribes for a signal
 *  that is sent to it at high frequency but not read often enough.
 */
void sig_handler(int sig) {
  sig_info *p = &sigfo[sig];
  file *f;
  for (f = p->readers; f != NULL; f = f->next) {
    f->evcount++;  /* increment event counter */
  }
} /* end sig_handler */

/*
 *  The signal handler for SIGALRM.  The SETL client gets events from
 *  SIGALRM through timer streams only (OPEN mode 'REAL-MS').
 */
void alarm_handler(int sig) {
  jostle_timers();  /* gen client events; schedule next SIGALRM */
}

/*
 *  Call waitpid(), and set STATUS.  If it returned the status of a
 *  child process that exited or was terminated by a signal, remove
 *  that child's pid from the 'reapable' list (where it was put on a
 *  CLOSE_AUTOREAP, meaning that either a CLOSE of that kind or the
 *  WAITPID that caused the present do_waitpid() call was probably
 *  inappropriate!).
 *
 *  SIGCHLD is blocked during the list manip.
 */
pid_t do_waitpid(pid_t p, int waitflags) {
  pid_t pid;  /* as received from os_waitpid() */
  int raw_status;
  sigset_t old_mask;
  pid = os_waitpid(p, &raw_status, waitflags);  /* waitpid() may set errno */
  set_raw_status(pid > 0 ? raw_status : no_status);
  if (pid > 0 && (WIFEXITED(raw_status) || WIFSIGNALED(raw_status))) {
    /* This list manip is the same as in sigchld_handler() in init.c:  */
    pid_list *rover, *plover, *grover;  /* 'reapable' list ptrs */
    plover = NULL;  /* predecessor if any of rover in 'reapable' list */
    os_sigblock(SIGCHLD, &old_mask);  /* critsect-enter (block SIGCHLD) */
    for (rover = reapable; rover; rover = grover) {
      grover = rover->next;  /* preserve rover->next */
      if (rover->pid == pid) {  /* a foolish node in need of removal */
        if (plover) {  /* rover has a predecessor */
          plover->next = grover;  /* excise rover by bypassing it */
        } else {  /* rover must still be the head ('reapable') */
          reapable = grover;  /* excise rover by updating head */
        }
        rover->next = reaped;  /* link rover into 'reaped' list */
        reaped = rover;     /* make rover the new 'reaped' head */
      } else {
        plover = rover;  /* "previous" rover for next loop iter */
      }
    }
    os_sigsetmask(&old_mask);  /* critsect-exit (restore SIGCHLD) */
  }
  return pid;
} /* end do_waitpid */

/*
 *  Map signal name to signal number.
 *
 *  These are the names of the signals that can be passed to OPEN;
 *  see the discussion ahead of 'file_open' below for more details.
 *  (KILL has a much larger repertoire:  see 'l_kill' in lib.c.)
 */
static int sig_num(const char *s) {
  int sig;
  /* Please keep the table of signals in the comments ahead of the
   * 'file_open' function in one-to-one correspondence with this:  */
  if      (leq_ic(s,"HUP" ) || leq_ic(s,"SIGHUP" )) sig = SIGHUP;
  else if (leq_ic(s,"INT" ) || leq_ic(s,"SIGINT" )) sig = SIGINT;
  else if (leq_ic(s,"QUIT") || leq_ic(s,"SIGQUIT")) sig = SIGQUIT;
  else if (leq_ic(s,"USR1") || leq_ic(s,"SIGUSR1")) sig = SIGUSR1;
  else if (leq_ic(s,"USR2") || leq_ic(s,"SIGUSR2")) sig = SIGUSR2;
  else if (leq_ic(s,"PIPE") || leq_ic(s,"SIGPIPE")) sig = SIGPIPE;
  else if (leq_ic(s,"ALRM") || leq_ic(s,"SIGALRM")) sig = SIGALRM;
  else if (leq_ic(s,"TERM") || leq_ic(s,"SIGTERM")) sig = SIGTERM;
  else if (leq_ic(s,"CHLD") || leq_ic(s,"SIGCHLD")) sig = SIGCHLD;
  else if (leq_ic(s,"CONT") || leq_ic(s,"SIGCONT")) sig = SIGCONT;
  else if (leq_ic(s,"TSTP") || leq_ic(s,"SIGTSTP")) sig = SIGTSTP;
  else if (leq_ic(s,"TTIN") || leq_ic(s,"SIGTTIN")) sig = SIGTTIN;
  else if (leq_ic(s,"TTOU") || leq_ic(s,"SIGTTOU")) sig = SIGTTOU;
#ifdef SIGXCPU
  else if (leq_ic(s,"XCPU") || leq_ic(s,"SIGXCPU")) sig = SIGXCPU;
#endif
#ifdef SIGXFSZ
  else if (leq_ic(s,"XFSZ") || leq_ic(s,"SIGXFSZ")) sig = SIGXFSZ;
#endif
#ifdef SIGPWR
  else if (leq_ic(s,"PWR" ) || leq_ic(s,"SIGPWR" )) sig = SIGPWR;
#endif
#ifdef SIGWINCH
  else if (leq_ic(s,"WINCH")|| leq_ic(s,"SIGWINCH"))sig = SIGWINCH;
#endif
  else {
    runerr("Signal name \"%s\" not recognized",
                      tame(s));
  }
  return sig;
} /* end sig_num */

/*
 *  "Read" an event from the given signal or timer stream by waiting
 *  for the event count in the 'file' record to become non-zero, and
 *  then decrementing it once.
 */
static void read_event(int fd) {
  file *f = find_sd_file(fd);
  sigset_t old_mask;
  assert (is_event_client(f));
  os_sigblock(f->sig, &old_mask);  /* critsect-enter (block sig) */
  while (f->evcount == 0) {  /* while no event pending, wait for one */
    os_sigsuspend(&old_mask);  /* old_mask restored during suspension */
  }
  --f->evcount;
  os_sigsetmask(&old_mask);  /* critsect-exit (restore mask) */
}

/*
 *  This function expects the caller to have blocked all signals, as
 *  it shares access to signal counts with sig_handler().
 *
 *  The rw args 'ready' and 'wants' are bit-sets whose indices are
 *  sd_lo less than the pseudo-fds they represent.
 *
 *  For each client that has a bit set in the 'wants' arg, and has a
 *  signal or timer event pending on the associated stream, that bit
 *  is cleared and the corresponding bit in 'ready' is set.
 *
 *  If any bit in 'wants' is thereby cleared, note_signals() returns
 *  true, else false.
 */
static bool note_signals(fd_bitset *ready, fd_bitset *wants, int nfd) {
  int fd;
  bool r = false;
  assert (sd_lo+nfd <= sd_hi);
  for (fd=sd_lo; fd<sd_lo+nfd; fd++) {
    file *f = find_file_unchecked(fd);
    if (is_event_client(f) &&
        is_set_fd_bit(wants, fd-sd_lo, nfd) &&
        is_event_pending(f)) {
      clear_fd_bit(wants, fd-sd_lo, nfd);
      set_fd_bit(ready, fd-sd_lo, nfd);
      r = true;
    }
  }
  return r;
}

static bool is_event_client(file *f) {
  return f->ftype == signal_file ||
         f->ftype == real_ms_file;
}

/*
 *  Tell whether an event is pending on the given signal or timer
 *  stream.
 *
 *  This function expects to be called in a context where the
 *  associated signal is blocked.
 */
static bool is_event_pending(file *f) {
  return f->evcount != 0;
}

/*
 *  Update real-ms (interval timer) client event counts, and schedule
 *  the next system timer expiry.  The SETL-level timer-event readers
 *  all run off a single POSIX-level CLOCK_REALTIME interval timer.
 *
 *  This function should be called when a new real-ms client has just
 *  joined the 'readers' list of the SIGALRM sig_info record (as on
 *  an OPEN), when a client has just left that list (as on a CLOSE),
 *  and when the SIGALRM signal handler is called.
 *
 *  SIGALRM must be blocked when jostle_timers() is called.  (POSIX
 *  does so automatically when calling the signal handler.)
 *
 *  The 'file' record for each reader has an 'evcount' field giving
 *  the number of timer expiry events not yet read, and a 'timestamp'
 *  field giving the time represented by the most recent incrementing
 *  of evcount (initially the time when the reader was created).
 *
 *  This function advances evcount and timestamp in every such record
 *  according to the current time, and schedules the next system timer
 *  expiry for the soonest next desired expiry among all readers.
 *
 *  SIGALRM wakes up any SELECT that is waiting for timer events.
 */
static void jostle_timers(void) {
  const struct timespec now = os_mono();  /* time of this call */
  struct timespec min_next;  /* time till next SIGALRM is wanted */
  file *f;
  sig_info *p = &sigfo[SIGALRM];  /* info about SIGALRM */
  if (p->readers == NULL) {
    /*
     *  Not clear why we were called; perhaps some delayed signal?
     *  At any rate, ignore the call.  Normally when the list of
     *  readers becomes empty (as upon a CLOSE), the system timer
     *  (in the USE_POSIX_RT case) is destroyed.
     */
    return;
  }
  for (f = p->readers; f != NULL; f = f->next) {
    struct timespec diff;  /* time from last event delivery till now */
    struct timespec time_till_next;  /* time till next expiry wanted */
    assert(f->sig == SIGALRM);  /* a REAL-MS client */
    diff = timespec_minus(now, f->timestamp);
    /*
     *  Break that diff down into how many timer periods (intervals)
     *  it represents, and a remainder.
     *
     *  Although we could get the number of intervals as the diff div
     *  the interval size, we're not generally expecting to get far
     *  enough behind for that number to exceed 1 very often, so a
     *  simpler iterative approach (no div) will do nicely.
     *
     *  In fact, we don't even bother computing that number, but
     *  simply increment the client event count, reducing the diff by
     *  an interval and advancing the timestamp by an interval, as
     *  long as the diff is at least one interval.
     */
    while (timespec_compare(diff, f->interval) >= 0) {
      diff = timespec_minus(diff, f->interval);
      f->timestamp = timespec_plus(f->timestamp, f->interval);
      f->evcount++;  /* deliver expiry event to client record */
    }
    /*
     *  Set min_next to the minimum of the next expiry times wanted
     *  by clients.
     */
    time_till_next = timespec_minus(f->interval, diff);  /* > 0 */
    if (p->readers == f) {  /* first element in the list */
      min_next = time_till_next;  /* initial min */
    } else {  /* not the first element in the list */
      if (timespec_compare(min_next, time_till_next) > 0) {
        min_next = time_till_next;  /* new min */
      }
    }
  }
  {
    /*
     *  Set one-shot.  It will be set to a new one-shot the next time
     *  jostle_timers() is called in the presence of timer clients,
     *  which is most often from the SIGALRM handler but also when
     *  the arrival or departure of a client occasions an update.
     */
#if USE_POSIX_RT
    const struct itimerspec oneshot = {{0,0}, min_next};
    os_timer_settime(p->timerid, 0, &oneshot, NULL);
#else
    const struct itimerval oneshot = {{0,0}, {min_next.tv_sec,
                                              min_next.tv_nsec/1000}};
    os_set_timer(ITIMER_REAL, &oneshot);
#endif
  }
} /* end jostle_timers */


/*
 *  This acts as a front end to do_select(), which is what l_select()
 *  used to call directly.  Now it calls this.
 *
 *  This caches the result of do_select() in setl_vm->ready_maps, and
 *  then on subsequent calls pulls preferentially from that cache.
 *
 *  Note that closing a SETL stream also removes it from ready_maps.
 *  using unready_stream().
 */
tuple *sys_select(tuple *a, tuple *t) {
  /*
   *  If there are no streams in common between the args and
   *  ready_maps, call do_select() and merge the resulting
   *  streams into ready_maps.
   *
   *  Then dole out the first fd that is both ready (cached) and
   *  wanted (present in the corresponding arg set), strangely
   *  packaged so as to be compatible with the original signature
   *  of SELECT and of course also with the arg.
   */
  HANDLE ha = ref(a);  /* OM or tuple of sets, from SELECT arg */
  HANDLE ht = ref(t);  /* optional timeout arg */
  tuple *r = new_tuple(3);  HANDLE hr = ref(r);  /* to be returned */
  set *s = NULL;  HANDLE hs = ref(s);
  set *e = NULL;  HANDLE he = ref(e);
  key *k = NULL;  HANDLE hk = ref(k);
  block *b = NULL;  HANDLE hb = ref(b);
  iterator *it = NULL;  HANDLE hit = ref(it);
  table *m = NULL;  HANDLE hm = ref(m);
  table *f = NULL;  HANDLE hf = ref(f);
  int is;
  bool found = false;

  assert (setl_vm);  /* needed for its ready_maps member */
  if (!setl_vm->ready_maps) {  /* init on demand */
    let (setl_vm->ready_maps, new_tuple(3));
    for (is=0; is<3; is++) {
      let (tupelt(setl_vm->ready_maps, is+1), (block *)empty_table());
    }
  }
  assert (setl_vm->ready_maps->nelt == 3);

  for (is=0; is<3; is++) {
    long i, n;
    e = null_set();  /* to be an element of the return triple r */
    /* s is the set of readers, writers, or raisers of interest */
    s = a && is < a->nelt ? (set *)tupelt(a,is+1) : NULL;  /* arg set */
    /* m is a ready_maps map, taking fd to stream (block *) */
    m = (table *)tupelt(setl_vm->ready_maps, is+1);  /* map (cache) */
    n = patsiz(m);  /* number of entries in map */
    if (!found && n > 0 && s && s->card > 0) {
      /* if there is a stream in common between s and the range of m,
       * put the first one found into e for later return, and remove it
       * from m.
       * we could be tricky and edit m directly as we iterate through
       * it, but instead we just copy all but possibly that one entry
       * to a new f, one by one, and then move f back to m. */
      f = empty_table();  /* what we incrementally copy from m */
      for (i=1; i<=n; i++) {  /* loop over map */
        subnode *c;
        check (ordsee(m,i,&c));
        k = c->k;  /* the key (fd) */
        b = c->d;  /* the stream (block *) */
        c = NULL;  /* this unprot ptr is now dead */
        if (!found && in_set(s,b)) {  /* first stream found in common */
          set_insert(&e, b);  /* b is the stream to be returned */
          found = true;
        } else {  /* retain the map entry by copying it to f */
          insert_block_new(f, k, b);
        }
        b = NULL;
        k = NULL;
      }
      /* m, less 1 member if we just found something returnable: */
      tupelt(setl_vm->ready_maps, is+1) = (block *)f;
      f = NULL;
      s = NULL;
    } else {
      /* already found a stream in common, or no chance of a
       * non-empty intersection; leave e and ready_maps alone */
    }
    m = NULL;
    tupelt(r,is+1) = (block *)e;  /* empty set or singleton */
    e = NULL;
  }

  if (found) {
    /* r is ready to be returned, after handles are released */
  } else {
    /* nothing in the intersection of any arg set and a corresponding
     * ready_maps range.  so call do_select() and merge its result
     * into ready_maps with the aid of getfd(), except for the stream
     * if any being returned via e. */
    tuple *u = do_select(a,t);  HANDLE hu = ref(u);
    for (is=0; is<3; is++) {
      e = null_set();
      m = copy_table ((table *) tupelt(setl_vm->ready_maps, is+1));
      s = (set *) tupelt(u,is+1);  /* streams returned by do_select() */
      assert (is_set(s));
      it = init_iterator((block *)s);
      while (step_iterator(&it, &b)) {  /* for each stream b in s */
        if (!found) {  /* nothing yet selected for return */
          set_insert(&e, b);  /* b is the stream to be returned */
          found = true;
        } else {  /* another was selected for return */
          /* look it up in a way compatible with what do_select()
           * just did. */
          int fd = getfd(b, getfd_check);
          k = num_to_key(fd);  /* the fd is the ready_maps key */
          insert_block(m, k, b);  /* add entry to ready_maps (cache) */
          k = NULL;
        }
      }
      b = NULL;
      it = NULL;
      s = NULL;
      tupelt(setl_vm->ready_maps, is+1) = (block *)m;
      m = NULL;
      tupelt(r,is+1) = (block *)e;  /* empty set or singleton */
      e = NULL;
    }
    retire(hu);
  }

  retire(hf);
  retire(hm);
  retire(hit);
  retire(hb);
  retire(hk);
  retire(he);
  retire(hs);
  retire(hr);
  retire(ht);
  retire(ha);

  return r;

} /* end sys_select */

static const getfd_op select_intents[] = {getfd_sel_r,
                                          getfd_sel_w,
                                          getfd_sel_e};

/*
 *  This is the routine which does the "real" work for SELECT.
 */
tuple *do_select(tuple *a, tuple *t) {
  HANDLE ha = ref(a);
  HANDLE ht = ref(t);
  tuple *r = new_tuple(3);  HANDLE hr = ref(r);
  /* What the 3 elements mean:  */
  #define is_reader 0
  #define is_writer 1
  #define is_raiser 2
  fd_bitset fd_wants[3];  /* fds we need to check with os_pselect() */
  fd_bitset sd_wants[3];  /* pseudo-fds we need to check */
  fd_bitset fd_ready[3];  /* "ready" fds found so far */
  fd_bitset sd_ready[3];  /* "ready" pseudo-fds found so far */
  fd_bitset fd_tmp[3];
  int fd_maxes[3];  /* max requested fds */
  int sd_maxes[3];  /* max requested pseudo-fds */
  int is;  /* is_reader, is_writer, or is_raiser to index the above */
  int fd_max;  /* max among fd_maxes */
  int sd_max;  /* max among sd_maxes */
  int nfd;     /* number of significant bits in fd bitsets */
  int nsd;     /* number of significant bits in pseudo-fd bitsets */
  int saved_errno;
  struct timespec starting_time;
  struct timespec timeout_time;
  struct timespec remaining_time;
  bool timeout_requested;        /* whether a timeout arg is present */
  bool timed_out = false;        /* set if os_pselect() times out */
  bool fd_requested = false;     /* at least one fd in 'a' */
  bool sd_requested = false;     /* at least one pseudo-fd in 'a' */
  bool ready_fd_exists = false;  /* fd_ready is nonempty */
  bool ready_sd_exists = false;  /* sd_ready is nonempty */
  bool done = false;
  bool did_pselect = false;
  sigset_t mask, old_mask;  /* we block signals for brief intervals */
  set *e = NULL;        HANDLE he = ref(e);
  set *s = NULL;        HANDLE hs = ref(s);
  iterator *i = NULL;   HANDLE hi = ref(i);
  block *b = NULL;      HANDLE hb = ref(b);

  assert (is_om(a) || is_tuple(a));
  assert (is_tuple(t));

  /* Nothing we do is supposed to alter errno, but this function is
   * so huge that we defensively save and restore it anyway.  */
  saved_errno = errno;

  /* Find highest fd (fd_max) and pseudo-fd (sd_max) */
  for (is=0; is<3; is++) {  /* for readers, writers, raisers */
    const getfd_op intent = select_intents[is];
    fd_maxes[is] = fd_lo - 1;
    sd_maxes[is] = sd_lo - 1;
    if (a && is < a->nelt) {
      s = (set *)tupelt(a,is+1);  /* elmt of 1st arg tup */
      if (s) {  /* set of readers, writers, or raisers of interest */
        assert (is_set(s));
        i = init_iterator((block *)s);
        while (step_iterator(&i, &b)) {  /* for each element in s */
          int fd = getfd(b, intent);  /* find it */
          if (is_normal_fd(fd)) {
            fd_maxes[is] = MAX(fd, fd_maxes[is]);
          } else {
            assert (is_pseudo_fd(fd));
            assert (is == is_reader);  /* else getfd() betrayed us */
            sd_maxes[is] = MAX(fd, sd_maxes[is]);
          }
        }
      }
    }
  }

  fd_max = MAX3(fd_maxes[is_reader],
                fd_maxes[is_writer],
                fd_maxes[is_raiser]);
  sd_max = MAX3(sd_maxes[is_reader],
                sd_maxes[is_writer],
                sd_maxes[is_raiser]);

  nfd = fd_max + 1 - fd_lo;
  nsd = sd_max + 1 - sd_lo;

  /* Allocate file descriptor bitsets */
  for (is=0; is<3; is++) {  /* for readers, writers, raisers */
    if (fd_maxes[is] >= fd_lo) {
      new_fd_bitset(&fd_wants[is], nfd);
      new_fd_bitset(&fd_ready[is], nfd);
      new_fd_bitset(&fd_tmp[is], nfd);
    } else {
      null_fd_bitset(&fd_wants[is]);
      null_fd_bitset(&fd_ready[is]);
      null_fd_bitset(&fd_tmp[is]);
    }
    if (sd_maxes[is] >= sd_lo) {
      new_fd_bitset(&sd_wants[is], nsd);
      new_fd_bitset(&sd_ready[is], nsd);
    } else {
      null_fd_bitset(&sd_wants[is]);
      null_fd_bitset(&sd_ready[is]);
    }
  }

  /* Gather fd bitsets */
  for (is=0; is<3; is++) {  /* for readers, writers, raisers */
    const getfd_op intent = select_intents[is];
    zero_fd_bitset(&fd_wants[is], nfd);
    zero_fd_bitset(&fd_ready[is], nfd);
    zero_fd_bitset(&sd_wants[is], nsd);
    zero_fd_bitset(&sd_ready[is], nsd);
    /* Set appropriate bits... */
    if (a && is < a->nelt) {
      s = (set *)tupelt(a,is+1);
      if (s) {  /* set of readers, writers, or raisers of interest */
        assert (is_set(s));
        i = init_iterator((block *)s);
        while (step_iterator(&i, &b)) {  /* for each element in s */
          const int fd = getfd(b, intent);  /* find it */
          file *f = find_file(fd);
          io_buffer *p = f->buffer;  /* may be NULL for e.g. UDP */
          if (is_normal_fd(fd)) {
            if (is == is_reader && p != NULL && !is_empty(p)) {
              set_fd_bit(&fd_ready[is], fd-fd_lo, nfd);  /* ready already */
              ready_fd_exists = true;
            } else {
              /*
               *  One could immediately declare an output fd to be
               *  "ready" if there is room for more than one byte in
               *  the output buffer (analogous to how an input fd is
               *  declared to be ready already if there is at least
               *  one unread byte in the input buffer), but that is
               *  unlikely to be helpful.
               *
               *  What the user surely wants to know is whether the
               *  _system_ (OS) is ready for at least one byte, not
               *  whether the SETL buffering can handle another byte
               *  without flushing.
               *
               *  So membership in the ready output set is defined
               *  to mean that the fd is flushable, which is the same
               *  as saying a POSIX-level write() of a byte will not
               *  block.  If you also know that the output buffer is
               *  empty, then you can safely write and flush a byte
               *  at that point.  And of course in practice, once you
               *  know that the system is ready for one output byte, if
               *  the stream has socket-like semantics, it is actually
               *  ready for some system-defined larger number of bytes.
               *  (See the POSIX pages on write()/pwrite() for more
               *  on that.)
               *
               *  Hence we unconditionally set the bit in fd_wants for
               *  each fd in the output set, just as we do for each in
               *  the exceptional-conditions set.
               *
               *  Note that the above definition of output readiness is
               *  now part of the lib spec, not the impl whim.  It is
               *  also compatible with the use of SYS_WRITE in place of
               *  buffered output.
               */
              set_fd_bit(&fd_wants[is], fd-fd_lo, nfd);
            }
            fd_requested = true;
          } else {
            assert (is_pseudo_fd(fd));
            assert (is == is_reader);  /* else getfd() betrayed us */
            assert (is_event_client(f));  /* signal or timer */
            /*
             *  If there is an unread byte in the buffer (which can
             *  only happen via an UNGETC, by the way), set the bit
             *  in the 'ready' set.
             *
             *  Otherwise, since we don't want to look at the event
             *  counter until the signal is blocked, just set the bit
             *  in the 'wants' set; note_signals() will move it if
             *  appropriate.
             */
            if (p != NULL && !is_empty(p)) {
              set_fd_bit(&sd_ready[is], fd-sd_lo, nsd);
              ready_sd_exists = true;
            } else {
              set_fd_bit(&sd_wants[is], fd-sd_lo, nsd);
            }
            sd_requested = true;
          }
        }
      }
    }
  }

  /* Gather timeout_time and timeout_requested from the second arg */
  switch (t->nelt) {
  case 0:
    timeout_requested = false;  /* so timeout_time is not defined */
    break;
  case 1:
    timeout_time = integer_ms_to_timespec((integer *)tupelt(t,1),
                                      "timeout arg to SELECT");
    timeout_requested = true;
    break;
  default:
    unexpected (t->nelt);
  }

  if (timeout_requested) {
    starting_time = os_mono();  /* get time as a struct timespec */
    remaining_time = timeout_time;
  }

  /*
   *  Enter critical section with all signals blocked, saving
   *  existing signal mask in old_mask.
   */
  sigfillset(&mask);
  os_sigprocmask(SIG_BLOCK, &mask, &old_mask);  /* critsect-enter */

  done = false;
  while (!done) {
    if (sd_requested) {
      if (note_signals(&sd_ready[is_reader],
                       &sd_wants[is_reader], nsd)) {
        ready_sd_exists = true;
      }
    }
    if (!fd_requested &&
        !timeout_requested &&
        ready_sd_exists) {
      /*
       *  The only thing asked for was signals/timers, and at
       *  least 1 is ready, so return what we have.
       */
      done = true;
    } else if (did_pselect && (ready_fd_exists || ready_sd_exists)) {
      /*
       *  If we already have a non-null ready set to return, and have
       *  already done at least one os_pselect(), then we have done
       *  our best and can now return the catch.
       */
      done = true;
    } else if (timed_out) {
      /*
       *  Last os_pselect(), whether during a "polling" call or one
       *  that waited for fds to go ready, ran out of time.
       *
       *  As with the other tests for doneness above, we tested for
       *  the condition (in this case timed_out) up here rather than
       *  simply setting done true as soon as the condition became
       *  true below, to get the benefit of any subsequent arrivals
       *  in sd_ready due to the note_signals() call above.
       */
      done = true;
    } else {
      /*
       *  There is a "normal" fd in the SELECT arg, or a timeout spec,
       *  or an empty set of ready signal/timer streams.  Also, we
       *  know that either nothing has gone ready yet or os_pselect()
       *  has never been called (or both), and that os_pselect()
       *  didn't already time out.  It is appropriate in these
       *  circumstances to call os_pselect().
       *
       *  In the particular case where all the 'wants' sets are empty
       *  and no timeout arg is given, we expect os_pselect() to wait
       *  until interrupted by a signal.  That translates to a
       *  SETL-level "wait until program is killed" because even if
       *  signals are still being caught, causing os_pselect() to
       *  return with an EINTR, we will just keep calling it again
       *  in this loop.
       *
       *  Getting back to the more general case, if right here we have
       *  some ready normal fd by virtue of having seen a nonempty read
       *  buffer, or we have a ready pseudo-fd, the timeout passed
       *  to os_pselect() will be 0 to indicate a "poll".  This is
       *  so that what os_pselect() finds ready can be included in
       *  SELECT's result sets along with what was already found.
       *
       *  Otherwise (nothing ready yet), the timeout arg will be
       *  what was asked for---or what is left of what was asked for,
       *  if we're having to retry the os_pselect().
       */
      struct timespec tv, *tvp = NULL;  /* timeout value if tvp = &tv */
      if (ready_fd_exists || ready_sd_exists) {
        tv = timespec_zero;  /* just "poll"; something's ready already */
        tvp = &tv;
      } else {  /* nothing ready yet, so wait as instructed */
        if (!timeout_requested) {
          tvp = NULL;  /* want os_pselect() to wait indefinitely */
        } else {
          tv = remaining_time;
          tvp = &tv;  /* what remains of user-specified timeout */
        }
      }
      for (is=0; is<3; is++) {
        copy_fd_bitset(&fd_tmp[is], &fd_wants[is], nfd);
      }
      /*
       *  The main blocking point, awaiting I/O and other events:
       */
      if (os_pselect(nfd,
                     fd_set_ptr(&fd_tmp[is_reader]),
                     fd_set_ptr(&fd_tmp[is_writer]),
                     fd_set_ptr(&fd_tmp[is_raiser]),
                     tvp,
                     &old_mask) > 0) {
        ready_fd_exists = true;
        for (is=0; is<3; is++) {
          merge_fd_bitset(&fd_ready[is], &fd_tmp[is], nfd);
        }
      } else {  /* os_pselect() returned 0 */
        if (tvp != NULL) {  /* finite timeout period was specified */
          /*
           *  We work by comparing the elapsed time to the
           *  amount of time specified in the timeout parameter,
           *  because the latter is constrained only by the maximum
           *  representable time.  If we instead tried to compute
           *  a deadline against which we intended to check the
           *  current time, we could suffer an unrepresentable
           *  deadline := starting time + timeout.
           */
          const struct timespec elapsed_time =
                        timespec_minus(os_mono(), starting_time);
          if (timespec_compare(elapsed_time, timeout_time) >= 0) {
            timed_out = true;
          } else {
            /* This remaining_time will be the next timeout period */
            remaining_time = timespec_minus(timeout_time, elapsed_time);
          }
        }
      }
      did_pselect = true;
    }
  } /* end while (!done) */

  os_sigsetmask(&old_mask);  /* critsect-exit (restore signal mask) */

  for (is=0; is<3; is++) {
    const getfd_op intent = select_intents[is];
    e = null_set();
    if (a != NULL && is < a->nelt) {
      s = (set *)tupelt(a,is+1);
      if (s) {  /* set of readers, writers, or raisers of interest */
        assert (is_set(s));
        i = init_iterator((block *)s);
        /* We count on this iterating the same way both times:  */
        while (step_iterator(&i, &b)) {  /* for each element in s */
          int fd = getfd(b, intent);  /* find it */
          if (is_normal_fd(fd)) {
            if (is_set_fd_bit(&fd_ready[is], fd, nfd)) {
              set_insert(&e,b);  /* copy file id from arg set */
            }
          } else {
            assert (is_pseudo_fd(fd));
            assert (is == is_reader);  /* else getfd() betrayed us */
            if (is_set_fd_bit(&sd_ready[is], fd-sd_lo, nsd)) {
              set_insert(&e,b);  /* copy file id from arg set */
            }
          }
        }
      }
    }
    tupelt(r,is+1) = (block *)e;  /* r(is+1) := this set of file ids */
  }

  for (is = 3; is-- > 0; ) {
    free_fd_bitset(&sd_ready[is], nsd);
    free_fd_bitset(&sd_wants[is], nsd);
    free_fd_bitset(&fd_tmp[is], nfd);
    free_fd_bitset(&fd_ready[is], nfd);
    free_fd_bitset(&fd_wants[is], nfd);
  }

  retire(hb);
  retire(hi);
  retire(hs);
  retire(he);
  retire(hr);
  retire(ht);
  retire(ha);

  errno = saved_errno;

  return r;  /* result triple */

} /* end do_select */


/*
 *  Buffer support for SETL I/O...
 */

io_buffer *new_io_buffer(int fd, unsigned abilities, buffering policy) {
  const bool seekable = test_all(abilities, can_seek);
  const bool appending = test_all(abilities, can_append);
  io_buffer *b = (io_buffer *) os_malloc(sizeof *b);
  if (fd == fd_stderr &&
      policy == line_buffering /* e.g. per file_init() */ &&
      !os_getenv("SETL_LINEBUF_STDERR")) {
    policy = byte_buffering;  /* the default for STDERR */
  }
  b->fd = fd;
  b->policy = policy;
  /*
   *  The fact that j_in and n_in are set equal below means that the
   *  buffer is considered empty, triggering a fill_in() on the next
   *  get_char().
   *
   *  The fact that j_in is set to 0 means that no bytes of pushback
   *  are initially possible.  Pushback is possible after fill_in()
   *  sets j_in to 1.
   */
  b->j_in = b->n_in = 0;  /* initially empty input buffer */
  b->j_out = 0;           /* initially empty output buffer */
  switch (policy) {
  case input_buffering:
    b->n_out = 0;  /* for definiteness (output buffer is unused) */
    break;
  case byte_buffering:
  case line_buffering:
    b->n_out = 0;  /* call maybe_flush() for every output byte */
    break;
  case full_buffering:
    b->n_out = sizeof b->out;  /* let output buffer fill maximally */
    break;
  default:
    unexpected (policy);
  }
  b->eof = false;
  b->eof_pending = false;
  b->seekable = seekable;
  b->appending = appending;
  b->pending_errno = 0;
  b->file_pos = 0;  /* revised on read, write, seek */
  if (appending) {
    /*
     *  For definiteness in this corner case where POSIX does not
     *  define the initial position of a file opened for appending,
     *  side with Linux in defining it as the end of the file.
     *
     *  Ignore errors from lseek(), as the stream may not even be
     *  seekable (in which case 0 remains fine as an initial answer).
     */
    int saved_errno = errno;
    off_t init_pos = lseek (fd, 0, SEEK_END);
    if (init_pos != -1) {
      b->file_pos = init_pos;  /* revised on read, write, seek */
    }
    errno = saved_errno;  /* ignore lseek() errors */
  }
  return b;
} /* end new_io_buffer */

/*
 *  Flush and free the io_buffer
 */
void del_io_buffer(io_buffer *b) {
  if (b != NULL) {
    flush_io_buffer(b);  /* write() may set errno */
    os_free(b);
  }
} /* end del_io_buffer */

/*
 *  Flush the output buffer.  For the convenience of maybe_flush(),
 *  return a bool result saying whether the last write() operation
 *  succeeded.  If not, errno is presumed to have been set.  If
 *  write() is not called, because the buffer is already empty,
 *  the result is vacuously true.
 */
bool flush_io_buffer(io_buffer *b) {
  if (b != NULL && b->j_out > 0) {  /* non-empty output buffer */
    int j = 0, n = 0;
    while (j < b->j_out &&
           (n = os_write (b->fd, &b->out[j], b->j_out - j)) > 0) j += n;
    if (b->seekable && b->appending) {
      /*
       *  In this case (a+ mode), the OS will have moved the file pos
       *  to the end of the file before the latest write() occurred,
       *  and we don't actually track where that end is, so we lseek()
       *  (presumably cheap relative to write()) to get an updated
       *  file_pos.
       *
       *  Note that the actual end of the file may change due to the
       *  activity of other processes, so this is really just to give
       *  proper accounting for the sake of FILEPOS when we are the
       *  only process doing something to the file.  The doc should
       *  tell the SETL user to use SEEK for up to date info if the
       *  scarier case has to be handled.
       *
       *  For plain 'a' mode (stream not marked seekable), we should
       *  be able to trust our accounting (with that caveat regarding
       *  other processes), just as we do for non-appending modes.
       *
       *  We call lseek() rather than os_seek(), and fall back to
       *  treating a+ like 'a' on failure, as a user-level SEEK would
       *  have failed if the stream isn't seekable as the OS level,
       *  and in the absence of SEEK calls (including REWIND, GETS,
       *  and PUTS in that designation, of course), our normal
       *  accounting suffices.
       */
      int saved_errno = errno;
      off_t end_pos = lseek (b->fd, 0, SEEK_END);  /* same as SEEK_CUR here */
      if (end_pos != -1) {
        b->file_pos = end_pos;  /* update for OS-level state */
      } else {
        /* leave file_pos alone (see "normal accounting" comment above) */
      }
      errno = saved_errno;  /* ignore lseek() errors */
    } else {
      /*
       *  Even if the last os_write() failed (by returning -1 or 0),
       *  consider the flush to be complete.  For -1, errno will have
       *  been set by write().  A return of 0 from os_write() is rather
       *  anomalous, as we have asked to write at least 1 byte, and not
       *  in non-blocking mode.  So we choose to regard 0 as another
       *  error case, but one for which errno might not be set.  Really
       *  it should not occur at all, but we allow j to be 0 in case
       *  some OS does do that.
       *
       *  No access to the apparent number of unwritten bytes in error
       *  cases is provided at the SETL level, but that would be
       *  indeterminate on some old Unix systems which could allow
       *  write() to return -1 and set errno to EINTR even after
       *  succeeding in writing some of the requested bytes.
       *
       *  So if the write failed, we are falling back on the assumption
       *  that no bytes were written.
       */
      b->file_pos += j;
    }
    b->j_out = 0;  /* empty the output buffer */
    return n >= 0;
  } else {
    /*
     *  No-op.  Output buffer is empty.
     */
    return true;
  }
} /* end flush_io_buffer */

/*
 *  Discard unused input from a seekable stream (no-op for others).
 *
 *  The system file pos is moved back by the number of bytes buffered
 *  but not yet read at the SETL user level, and our file_pos updated
 *  to match (by seek_io_buffer()).
 */
void drain_io_buffer(io_buffer *b) {
  if (b->seekable) {
    off_t n_unconsumed = b->n_in - b->j_in;
    if (n_unconsumed > 0) {  /* #bytes buffered but not yet GETC'd */
      seek_io_buffer (b, -n_unconsumed, SEEK_CUR);
    }
  }
  /*
   *  Just as in new_io_buffer(), setting j_in and n_in equal makes
   *  the buffer empty, triggering a fill_in() on the next get_char().
   *
   *  Setting j_in to 0 means that no bytes of pushback are possible
   *  after a drain.  Pushback becomes possible when fill_in() sets
   *  j_in to 1.
   */
  b->j_in = b->n_in = 0;  /* drain the input buffer */
} /* end drain_io_buffer */

/*
 *  Call os_seek() and update the io_buffer's version of the
 *  system file position accordingly.
 *
 *  Note that os_seek() abends rather than setting errno, on the
 *  presumption that lseek() failures will generally reflect
 *  programming errors rather than legitimate failure modes.
 *
 *  The file_pos is updated in some other places too.
 */
off_t seek_io_buffer(io_buffer *b, off_t offset, int whence) {
  off_t pos = b->file_pos;
  assert (b->seekable);
  switch (whence) {
  case SEEK_SET:
  case SEEK_CUR:
  case SEEK_END:
    pos = os_seek (b->fd, offset, whence);
    break;
  default:
    unexpected (whence);
  }
  b->file_pos = pos;
  return b->file_pos;
} /* end seek_io_buffer */

/*
 *  Called by the get_char() macro when faced with an empty input
 *  buffer.
 *
 *  If the io_buffer's eof_pending flag is not set (initial state),
 *  fill_in() tries to fill the input buffer using read().  If it
 *  succeeds, it returns the first input character from that buffer
 *  as an unsigned char cast to an int.  If it cannot get even one
 *  character, it sets eof_pending to true and returns EOF (the
 *  constant defined in <stdio.h>).  If the read() indicates an
 *  error, errno is saved in the io_buffer's pending_errno for later
 *  presentation at the SETL level, not presented to the fill_in()
 *  caller (raise_eof() will later do that).
 *
 *  If the io_buffer's eof_pending flag is already set when fill_in()
 *  is called, it is left set, and fill_in() returns EOF without
 *  attempting a read().
 *
 *  The idea is that the ultimate caller of get_char(), when 0 "items"
 *  in the units of that caller are obtained (not merely fewer items
 *  than requested at the SETL level), will then call file_eof(),
 *  which in turn calls raise_eof(), which "moves" the pending
 *  indications of eof and any associated error to where the
 *  SETL-level primitives EOF and LAST_ERROR will see them.
 *
 *  EOF is only possible for normal file descriptors, not for
 *  pseudo-fds, as the latter will always wait for an event to
 *  satisfy the request if no input is immediately available.
 */
int fill_in(io_buffer *b) {
  int fd, r;
  assert (b);
  fd = b->fd;
  if (is_normal_fd(fd)) {
    if (!b->eof_pending) {
      int saved_errno = errno;
      b->pending_errno = 0;
      b->n_in = os_read(fd, b->in, sizeof b->in);  /* read() may set errno */
      if (b->n_in > 0) {  /* got at least 1 byte, errno not set */
        b->file_pos += b->n_in;  /* the presumed new file position */
        r = b->in[0];  /* suck first byte from buffer as our yield */
        b->j_in = 1;  /* which can now be ungetted */
      } else {  /* eof or error */
        r = EOF;
        b->eof_pending = true;
        if (b->n_in < 0) {  /* error from read() */
          b->pending_errno = errno;
          errno = saved_errno;  /* defer setting errno to raise_eof() */
        } else {
          /* read() returned 0, errno was not set */
        }
        b->j_in = 0;  /* there is now no unconsumed input */
      }
    } else {  /* eof is already pending */
      r = EOF;
      /* The eof remains pending, and so does any pending errno.  */
    }
  } else if (is_pseudo_fd(fd)) {  /* signal or timer stream */
    read_event(fd);  /* read one event, blocking if necessary */
    r = b->in[0] = '\n';  /* our yield is a newline */
    b->j_in = b->n_in = 1;  /* which can now be ungetted */
    b->file_pos += b->n_in;  /* new "file position" like after read() */
  } else {
    bugerr("Unexpected value (%d) of fd at %s:%d",
                              fd,   FILEBASE, __LINE__);
  } /* end if */
  return r;  /* either EOF or an unsigned char value */
} /* end fill_in */

/*
 *  For use by put_char() when the output buffer may need flushing
 *  after a byte has been added to it.
 *
 *  Note that errno may be set by flush_io_buffer(), in which case
 *  maybe_flush() returns false.
 */
bool maybe_flush(io_buffer *b) {
  assert (b != NULL);
  switch (b->policy) {
  case full_buffering:
  case byte_buffering:
    /* We assume in the full_buffering case that we have only been
     * called because the output buffer is full, and in the
     * byte_buffering case that it has one byte in it.  */
    return flush_io_buffer(b);
  case line_buffering:
    /* We assume here that the output buffer has at least one byte
     * in it.  */
    if (b->out[b->j_out-1] == '\n' || b->j_out == sizeof b->out) {
      return flush_io_buffer(b);
    }
    return true;  /* flushing not nec, so trivially successful */
  default:
    unexpected (b->policy);
  }
} /* end maybe_flush */

/*
 *  Clear the SETL-level EOF indicators but leave eof_pending alone.
 *  (If eof_pending was set, we want it to remain so until it is
 *  cleared by raise_eof().)
 *
 *  This is meant for library input routines to use just before a
 *  read attempt if any.
 */
void clear_eof(io_buffer *b) {
  b->eof = false;  /* EOF indicator for the particular buffer */
  set_eof(false);  /* EOF indicator for the SETL VM */
  /* (b->eof_pending and b->pending_errno are not touched here) */
}

/*
 *  Clear eof_pending and set the SETL-level EOF indicators (EOF(f)
 *  for stream f, and the parameterless EOF).
 *
 *  Since this is only meant to be called in the context of the
 *  eof_pending flag being set (though this is not enforced), it
 *  effectively "moves" a pending EOF to a SETL-visible one.
 *
 *  The clearing of eof_pending also clears the way for attempting
 *  another read on the underlying byte stream, which is important
 *  so that the SETL user can still meaningfully keep reading after
 *  an EOF, such as when the stream is attached to a terminal device
 *  that delivers an end-of-file condition when ctrl-D is entered
 *  but from which further lines can be read if the stream isn't
 *  closed.
 *
 *  In concert with the "move" of eof_pending, if the io_buffer's
 *  pending_errno is not 0, it is "moved" into errno, and cleared
 *  for good measure (it is cleared before any read() attempt via
 *  fill_in() anyway).  This is semantics beyond what is suggested
 *  by the name 'raise_eof', obviously.  My feeble excuse is that
 *  error cases are a subset of EOF cases.
 */
void raise_eof(io_buffer *b) {
  b->eof_pending = false;  /* EOF no longer pending, but being posted */
  b->eof = true;  /* EOF indicator for the particular buffer */
  set_eof(true);  /* EOF indicator for the SETL VM */
  if (b->pending_errno != 0) {
    errno = b->pending_errno;  /* from the failed read() */
    b->pending_errno = 0;  /* for good measure, to complete the "move" */
  }
}

bool at_eof(void) {
  return get_eof();  /* eof indicator from the SETL VM instance */
}


/*
 *  The 'file' data type embraces both streams and direct-access
 *  files, much as FILE does in the world of C stdio.  Unlike FILE,
 *  it also supports full-duplex streams.
 */

/*
 *  Look up and initialize a file block
 */
file *new_file(int fd, filetype ftype, unsigned abilities,
                                                  buffering policy) {
  file *f = find_file(fd);
  assert (f->ftype == no_file);
  init_file (f, ftype, abilities);
  if (policy != no_buffering) {
    f->buffer = new_io_buffer (fd, abilities, policy);
  }
  return f;
}

/*
 *  Initialize a file block
 */
void init_file(file *f, filetype ftype, unsigned abilities) {
  if (ftype != no_file) {
    abilities |= can_ask;
  }
  if (abilities & (can_accept | can_read | can_recv | can_recvfrom)) {
    abilities |= (can_sel_r | can_sel_e);
  }
  if (abilities & (can_send | can_sendto | can_write)) {
    abilities |= (can_sel_w | can_sel_e);
  }
  f->ftype = ftype;
  f->abilities = abilities;
  f->filename = NULL;
  f->nodename = NULL;
  f->servname = NULL;
  f->buffer = NULL;
  f->tie = -1;
  f->pid = -1;
  f->ppid = -1;
  f->interval = timespec_zero;
  f->timestamp = timespec_zero;
  f->evcount = 0;
  f->next = NULL;
  f->sig = 0;
  f->auto_close = false;
  f->auto_close_output = false;
  f->local_child = false;
}

/*
 *  A glorified array reference
 */
file *find_file_unchecked(int fd) {
  /* Not completely unchecked... */
  file *f = &files[fd];
  assert (f->fd == fd);  /* mainly a sanity check against corruption */
  return f;
}

/*
 *  The fd must be a valid index into 'files'
 */
file *find_file(int fd) {
  assert (0 <= fd && fd < n_files);
  return find_file_unchecked(fd);
}

/*
 *  The fd must be in the "normal" range
 */
file *find_fd_file(int fd) {
  assert (is_normal_fd(fd));
  return find_file_unchecked(fd);
}

/*
 *  The fd must be in the "pseudo-fd" range
 */
file *find_sd_file(int fd) {
  assert (is_pseudo_fd(fd));
  return find_file_unchecked(fd);
}

/*
 *  Finalize a file block, putting it back to a clean no_file state.
 *
 *  On the way by, flush and free the io_buffer if any, release the
 *  malloc()'d storage for filename-like items, and unless can_persist
 *  is set in the file's 'abilities' bits, close the OS-level fd.
 *
 *  Note that can_persist plays a different, though related, role in
 *  file_rites(), where it helps the std in and out streams retain
 *  their buffers across whole SETL program executions within the
 *  main go() loop.
 */
void del_file(file *f) {
  int fd;
  io_buffer *b = f->buffer;
  if (b != NULL) {
    assert (b->fd == f->fd);
    del_io_buffer(b);  /* flush and free buffer; may set errno */
  }
  os_free(f->servname);
  os_free(f->nodename);
  os_free(f->filename);
  /* The above f->buffer and os_free()'d pointers are set to NULL by
   * init_file() below.  */
  fd = f->fd;
  if (is_normal_fd(fd)) {
    if (!test_all(f->abilities, can_persist)) {
      os_close(fd);  /* may set errno */
    }
  } else {
    assert (is_pseudo_fd(fd));
  }
  init_file(f, no_file, 0);
}

void flush_file(file *f) {
  io_buffer *b = f->buffer;
  if (b != NULL) {
    assert (b->fd == f->fd);
    flush_io_buffer(b);  /* may set errno */
  }
}

void drain_file(file *f) {
  io_buffer *b = f->buffer;
  if (b != NULL) {
    assert (b->fd == f->fd);
    drain_io_buffer(b);
  }
}

/*
 *  Move the file pos.  The output buffer is assumed to be empty,
 *  usually as a result of having been flushed.
 *  
 *  The result of seek_file() is the new file pos, which is then the
 *  same at both the OS and SETL level.
 *
 *  Since the input buffer is always drained by this operation, SEEK
 *  provides an explicit way for the user to force a drain.  This will
 *  never be necessary if the current program is the only one operating
 *  on the file, but could be part of coordinating with another process.
 */
off_t seek_file(file *f, off_t offset, int whence) {
  io_buffer *b = f->buffer;
  assert (b->fd == f->fd);
  assert (b->seekable);
  assert (b->j_out == 0);  /* e.g. by flush_io_buffer() on getfd_seek */
  if (whence == SEEK_CUR) {
    /* Adjust the requested offset by how far ahead we have buffered,
     * effectively pre-accounting for what drain_io_buffer() would
     * have done had we called it here instead of optimizing it away
     * for the sake of doing only one lseek().  */
    off_t n_unconsumed = b->n_in - b->j_in;
    offset -= n_unconsumed;
  }
  /* Drain the input buffer, as drain_io_buffer() would have done.  */
  b->j_in = b->n_in = 0;
  /* Move the OS file pos if nec and update b->file_pos to match.  */
  seek_io_buffer (b, offset, whence);
  return b->file_pos;
}

/*
 *  For use when eof was hit during a call to a library input
 *  routine before any items were read.  It calls raise_eof()
 *  and then auto-closes f if it was auto-opened (in which case
 *  the only trace of the eof condition will be in the argless EOF,
 *  as EOF(f) will be invalid by then).
 */
void eof_file(file *f) {
  io_buffer *b = f->buffer;
  assert (b != NULL);
  raise_eof(b);  /* clear eof_pending and set SETL-level EOF indicators */
  if (f->auto_close) file_close (f->fd, close_await);  /* may set errno */
}


/* This array must stay in perfect sync (correspondence and order) with
 * the  open_how_enum  enumeration in "setlrun.h":  */
static const open_mode open_modes[] = {
/* how                 ftype        abilities   oflag                       */
  {open_read,          stream_file, can_rs,     R_MODE},  /* seq/dir input  */
  {open_write,         stream_file, can_ws,     W_MODE},  /* seq/dir output */
  {open_new,           stream_file, can_ws,     N_MODE},  /* w to new file  */
  {open_append,        stream_file, can_wa,     A_MODE},  /* w at end       */
  {open_direct,        direct_file, can_rws,   RP_MODE},  /* seq/dir i/o    */
  {open_direct_empty,  direct_file, can_rws,   WP_MODE},  /* + empty first  */
  {open_direct_new,    direct_file, can_rws,   NP_MODE},  /* + new file     */
  {open_direct_append, direct_file, can_rwsa,  AP_MODE},  /* dir i; o @ end */
  {open_rw,            twoway_file, duplex,    RW_MODE},  /* seq i and o    */
  {open_pipe_from,     pipe_file,   can_read,        0},  /* input from cmd */
  {open_pipe_to,       pipe_file,   can_write,       0},  /* output to cmd  */
  {open_pump,          pipe_file,   duplex,          0},  /* i/o on cmd     */
  {open_tty_pump,      tty_pipe_file, duplex,        0},  /* tty i/o on cmd */
  {open_tcp_client,    tcp_socket_file, duplex,      0},  /* TCP client     */
  {open_tcp_server,    tcp_server_file, can_accept,  0},  /* TCP server     */
  {open_tcp_peer,      tcp_socket_file, duplex,      0},  /* TCP connection */
  {open_udp_client,    udp_client_file, can_recv|can_send, 0},  /* UDP cli. */
  {open_udp_server,    udp_server_file, can_recvfrom|can_sendto, 0},
  {open_unix_client,   unix_socket_file, duplex,     0},  /* AF_UNIX client */
  {open_unix_server,   unix_server_file, can_accept, 0},  /* AF_UNIX server */
  {open_unix_peer,     unix_socket_file, duplex,     0},  /* AF_UNIX conn.  */
  {open_unix_datagram_client, unix_datagram_client_file, can_send, 0},
  {open_unix_datagram_server, unix_datagram_server_file,  /* Unix datagrams */
                                    can_recvfrom|can_sendto, 0},
  {open_signal,        signal_file, can_read,        0},  /* signal input   */
  {open_ignore,        sig_ign_file, 0,              0},  /* ignore signal  */
  {open_default,       sig_dfl_file, 0,              0},  /* SIG_DFL signal */
  {open_real_ms,       real_ms_file, can_read,       0}   /* elapsed time   */
};

/* To maintainers:  if open_mode_name_map below is changed, please
 * update setl-lib.texi accordingly.  */
/* The mode names in this table are case-insensitive in terms of what
 * the SETL user can specify, but are reported in uppercase:  */
static const struct {
  const char *mode_name; const open_how how;
} open_mode_name_map[] = {
  {"R",                  open_read},
  {"W",                  open_write},
  {"N",                  open_new},
  {"A",                  open_append},
  {"R+",                 open_direct},
  {"W+",                 open_direct_empty},
  {"N+",                 open_direct_new},
  {"A+",                 open_direct_append},
  {"INPUT",              open_read},
  {"OUTPUT",             open_write},
  {"NEW",                open_new},
  {"NEW-W",              open_new},
  {"APPEND",             open_append},
  {"OUTPUT-APPEND",      open_append},
  {"CODED",              open_read},
  {"CODED-IN",           open_read},
  {"CODED-OUT",          open_write},
  {"CODED-NEW",          open_new},
  {"NEW-CODED",          open_new},
  {"CODED-APPEND",       open_append},
  {"PRINT",              open_write},
  {"PRINT-APPEND",       open_append},
  /* "Text" mode is not distinguished from "binary".  See comments
   * about B or BINARY below:  */
  {"TEXT",               open_read},
  {"TEXT-IN",            open_read},
  {"TEXT-OUT",           open_write},
  {"TEXT-NEW",           open_new},
  {"NEW-TEXT",           open_new},
  {"TEXT-APPEND",        open_append},
  {"RANDOM",             open_direct},
  {"DIRECT",             open_direct},
  {"NEW+",               open_direct_new},
  {"NEW-R+",             open_direct_new},
  {"NEW-W+",             open_direct_new},
  {"NEW-RANDOM",         open_direct_new},
  {"NEW-DIRECT",         open_direct_new},
  {"RANDOM-NEW",         open_direct_new},
  {"DIRECT-NEW",         open_direct_new},
  {"RW",                 open_rw},
  {"TWO-WAY",            open_rw},
  {"TWOWAY",             open_rw},
  {"BIDIRECTIONAL",      open_rw},
  {"INPUT-OUTPUT",       open_rw},
  {"READ-WRITE",         open_rw},
  {"PIPE-FROM",          open_pipe_from},
  {"PIPE-IN",            open_pipe_from},
  {"PIPE-TO",            open_pipe_to},
  {"PIPE-OUT",           open_pipe_to},
  {"PUMP",               open_pump},
  {"TTY-PUMP",           open_tty_pump},
  {"LINE-PUMP",          open_tty_pump},
  {"TCP-CLIENT",         open_tcp_client},
  {"TCP-CLIENT-SOCKET",  open_tcp_client},
  {"SOCKET",             open_tcp_client},
  {"CLIENT-SOCKET",      open_tcp_client},
  {"SERVER-SOCKET",      open_tcp_server},
  {"TCP-SERVER",         open_tcp_server},
  {"TCP-SERVER-SOCKET",  open_tcp_server},
  {"TCP-PEER",           open_tcp_peer},
  {"TCP-PEER-SOCKET",    open_tcp_peer},
  {"UDP-CLIENT",         open_udp_client},
  {"UDP-CLIENT-SOCKET",  open_udp_client},
  {"UDP-SERVER",         open_udp_server},
  {"UDP-SERVER-SOCKET",  open_udp_server},
  {"UNIX-STREAM-CLIENT", open_unix_client},
  {"UNIX-CLIENT",        open_unix_client},
  {"UNIX-CLIENT-SOCKET", open_unix_client},
  {"UNIX-STREAM-SERVER", open_unix_server},
  {"UNIX-SERVER",        open_unix_server},
  {"UNIX-SERVER-SOCKET", open_unix_server},
  {"UNIX-PEER",          open_unix_peer},
  {"UNIX-PEER-SOCKET",   open_unix_peer},
  {"UNIX-DATAGRAM-CLIENT", open_unix_datagram_client},
  {"UNIX-DATAGRAM-SERVER", open_unix_datagram_server},
  {"SIGNAL",             open_signal},
  {"SIGNAL-IN",          open_signal},
  {"SIGNAL-IGNORE",      open_ignore},
  {"IGNORE",             open_ignore},
  {"IGNORE-SIGNAL",      open_ignore},
  {"SIGNAL-DEFAULT",     open_default},
  {"DEFAULT",            open_default},
  {"DEFAULT-SIGNAL",     open_default},
  {"REAL-MS",            open_real_ms},
  /* B or BINARY has no effect, as it is the only case supported...  */
  {"RB",                 open_read},
  {"WB",                 open_write},
  {"NB",                 open_new},
  {"AB",                 open_append},
  {"RB+",                open_direct},
  {"WB+",                open_direct_empty},
  {"NB+",                open_direct_new},
  {"AB+",                open_direct_append},
  {"R+B",                open_direct},
  {"W+B",                open_direct_empty},
  {"N+B",                open_direct_new},
  {"A+B",                open_direct_append},
  {"BINARY",             open_read},
  {"BINARY-IN",          open_read},
  {"BINARY-OUT",         open_write},
  {"BINARY-NEW",         open_new},
  {"NEW-BINARY",         open_new},
  {"BINARY-APPEND",      open_append},
  {"RANDOM-BINARY",      open_direct},
  {"DIRECT-BINARY",      open_direct},
  {"BINARY-RANDOM",      open_direct},
  {"BINARY-DIRECT",      open_direct},
  {"RANDOM-BINARY-NEW",  open_direct_new},
  {"DIRECT-BINARY-NEW",  open_direct_new},
  {"BINARY-RANDOM-NEW",  open_direct_new},
  {"BINARY-DIRECT-NEW",  open_direct_new},
  {"NEW-RANDOM-BINARY",  open_direct_new},
  {"NEW-DIRECT-BINARY",  open_direct_new},
  {"NEW-BINARY-RANDOM",  open_direct_new},
  {"NEW-BINARY-DIRECT",  open_direct_new}
};

const open_mode *lookup_open_mode(const char *mode_name) {
  size_t i;
  for (i = 0; i < numberof (open_mode_name_map); i++) {
    if (leq_ic (mode_name, open_mode_name_map[i].mode_name)) {
      return find_open_mode (open_mode_name_map[i].how);
    }
  }
  return NULL;
}

/* A (barely) glorified array reference:  */
static const open_mode *find_open_mode(open_how how) {
  const open_mode *mode = &open_modes[how];
  assert (mode->how == how);
  return mode;
}

/*
 *  This is "canonical" only in the sense that it gives the first
 *  matching entry from open_mode_name_map (above).
 *
 *  To maintainers:  please update the tables in doc/setl-lib.texi
 *  if you change or reorder entries in open_mode_name_map, so that
 *  the first table of modes reflects what this function can return,
 *  and the second table (of mode "synonyms") gives all the rest,
 *  sans any you want to remain unadvertised.
 */
static const char *canonical_mode_name(open_how how) {
  size_t i;
  for (i = 0; i < numberof (open_mode_name_map); i++) {
    if (how == open_mode_name_map[i].how) {
      return open_mode_name_map[i].mode_name;
    }
  }
  unexpected (how);
}

/*
 *  Description of 'file_open' (and hence OPEN):
 *  --------------------------------------------
 *
 *  The routine immediately following this comment tries to open
 *  something for operations such as input and/or output.  It returns
 *  an integer called a file descriptor (fd) on success, or -1 on
 *  non-catastrophic failure.
 *
 *  Apart from some preliminary checking (see 'l_open' in lib.c),
 *  this is the implementation of the SETL OPEN primitive.  Note that
 *  OPEN itself returns OM on non-catastrophic failure, not -1.
 *
 *  The returned fd acts as a handle for the stream that connects
 *  the SETL program to a file, device, subprocess, network service,
 *  etc.  Some streams support direct-access operations such as
 *  SEEK and GETS.  Signals and timers also present as streams at
 *  the SETL level.
 *
 *  The returned fd is a standard POSIX file descriptor, except that
 *  for a signal or timer stream, it is an integer above the range of
 *  normal fds.
 *
 *  The returned fd is used directly by the SETL programmer, with the
 *  buffer structure being implicitly associated with that fd.  This
 *  contrasts with how stdio is used in C, where buffered I/O is done
 *  through FILE objects that in turn use the fd for I/O operations
 *  at the OS level.
 *
 *  This OPEN directly supports SETL programs that use the OPEN
 *  described in Schwartz et. al, when they ignore the return value,
 *  because the GNU SETL I/O routines accept, as an alternative to
 *  the fd, the original first argument to OPEN, so long as that is
 *  an unambiguous handle (multiple streams can be opened on a given
 *  file, you see).
 *
 *  This OPEN also supports SETL2 usage in that the fd serves as a
 *  unique handle.
 *
 *  But this OPEN offers far more I/O modes, including network,
 *  inter-process, pipe, co-process (pump), signal and timer streams,
 *  than any previous version.  See 'open_mode_name_map' above for
 *  the full list of supported modes.  OPEN treats the mode parameter
 *  case-insensitively.
 *
 *  There are three predefined streams with the following aliases:
 *
 *   fd         aliases (case-insensitive)   meaning
 *   --------   --------------------------   ---------------
 *   0,STDIN    '','-','INPUT','STDIN'       standard input
 *   1,STDOUT   '','-','OUTPUT','STDOUT'     standard output
 *   2,STDERR   'ERROR','STDERR'             standard error
 *
 *  A file whose actual name is 'input', 'OUTPUT', 'StdErr', etc. may
 *  still be referred to by explicitly OPENing it before starting I/O
 *  on it.  When it is so OPENed, its name is treated case-sensitively,
 *  hiding one predefined stream alias (of which there are many, given
 *  all the case combinations) until it is closed.
 *
 *  The empty string ('') acts as STDIN or STDOUT depending on the
 *  direction of an input or output operation.  Likewise '-'.
 *
 *  Given the predefined integer constants STDIN, STDOUT, and STDERR,
 *  however, there is probably little reason to bother using the
 *  string handles for the predefined streams, in new programs.
 *
 *  You can close STDIN, STDOUT, or STDERR at any time, in which case
 *  they become candidates for being chosen next on a call such as
 *  DUP, PIPE, OPEN, ACCEPT, MKSTEMP, or RECV_FD on a POSIX system
 *  (which always gives the lowest available fd when asked to choose a
 *  new one).
 *
 *  Many input and output routines automatically open files,
 *  connections, etc. on first reference to an integer, string, or
 *  2-tuple that could have been passed to OPEN (see 'getfd').
 *
 *  Any file or socket that has been automatically opened for
 *  sequential input will be automatically closed when an input
 *  routine encounters an end of file.
 *
 *  The only output routine that auto-closes is PUTFILE, when it
 *  has auto-opened the stream.
 *
 *  FILENO returns the fd of any open stream, and FILENAME is the
 *  converse in that it gives whatever was originally passed to OPEN.
 *
 *  IS_OPEN can be used to test an integer, string, or 2-tuple for
 *  whether it currently refers to an open stream, without any
 *  side-effects such as auto-opening a file or crashing the program.
 *
 *
 *  Compatibility hints for the SETL programmer:
 *
 *   - For simplicity at the risk of ambiguity, code OPEN in
 *     statement form and use whatever you passed to it as a
 *     handle everywhere.
 *
 *   - For safety, SETL2 compatibility, and a bit of extra
 *     efficiency, use the result of OPEN as the file handle.
 *     This is arguably the most natural usage anyway.
 *
 *   - If OPEN yields OM, the value of LAST_ERROR (a string)
 *     should indicate why.
 *
 *   - A convenient idiom if you want all OPEN failures to be
 *     considered fatal (i.e., to terminate your program) is
 *
 *       fd := FILENO OPEN (...);
 *
 *
 *  A SETL stream may be opened over a fd that is only open at the
 *  POSIX level.  The mode should be chosen by the SETL pgmer to be
 *  compatible with how the fd was really opened.  For example 'r'
 *  or 'rw' or 'r+'.  More specific modes may be approp if you intend
 *  to do socket ops.  For example 'TCP-PEER' (see below).
 *
 *  Following are further details on the less obvious OPEN modes,
 *  which after checking in 'l_open' are mapped as the 'how' arg to
 *  'file_open'.  (There are also calls to 'file_open' in 'getfd'
 *  for auto-opening streams on certain first references to files
 *  etc.)
 *
 *  For the 'TCP-CLIENT' and 'UDP-CLIENT' (client socket) modes,
 *  the first OPEN argument should be an ordered pair (2-tuple)
 *  of the form [nodename,service], where nodename is an Internet
 *  host name or address (IPv4 dotted or IPv6 colon notation), and
 *  service is either a service name or a port number expressed as
 *  a decimal string or positive integer.  For back-compatibility,
 *  the arg may instead be a string of the form "nodename:service".
 *
 *  For example, here is a SETL program to fetch a raw document from
 *  an HTTP server and write it to standard output:
 *
 *    fd := OPEN (['setl.org','http'], 'tcp-client');  -- connect
 *    PRINTA (fd, 'GET /');
 *    PUTCHAR (GETFILE fd);
 *
 *  For the 'TCP-SERVER' and 'UDP-SERVER' (server socket) modes,
 *  the nodename can be specified as OM in order to listen on any
 *  available interface.  Also, if service is 0 or OM, the system
 *  chooses a port number.  Again, for back-compatibility, a string
 *  of the form "nodename:service" may be used instead of a 2-tuple,
 *  and if there is no colon in the string, the nodename is taken as
 *  OM (meaning listen on any interface).  TCP server example:
 *
 *    fd := OPEN ([], 'tcp-server');  -- [] equivalent to [OM,0]
 *    PRINT ('server port number is', PORT fd);
 *
 *  The only I/O (data-transferring) operations allowed on UDP
 *  client sockets are SEND and RECV, and the only ones allowed on
 *  UDP server sockets are SENDTO and RECVFROM.  Conversely,
 *  RECV can only be used on UDP client sockets.  SEND, however,
 *  can be used on both UDP and Unix-domain datagram client sockets,
 *  and RECVFROM and SENDTO can be used on both kinds of datagram
 *  server sockets.
 *
 *  There is currently no way to specify the use of IPv4 or IPv6
 *  explicitly when OPENing a socket using a [host, port] or
 *  "host:port" arg, but if an Internet address rather than a
 *  hostname is used to specify the host, then a dotted address is
 *  taken to mean IPv4 and an address in "colon" notation IPv6.
 *
 *  For example, for an OPEN in 'TCP-SERVER' mode, a first OPEN arg
 *  of ['::0',0] or '::0:0' means listen on all IPv6 interfaces, while
 *  ['0.0.0.0',0] and '0.0.0.0:0' indicate IPv4.  It is unspecified
 *  whether [OM,0], '*:0', and '0' select IPv4 or IPv6.
 *
 *  The connection between TCP client and server is symmetric once
 *  established, in terms of API and most observable behaviour.
 *  The mode 'TCP-PEER' is intended for the case where you want to
 *  open a stream over a fd that was created by accept(), or the
 *  less usual case where you only care that it is connected, not
 *  how it got that way.
 *
 *  The modes 'UNIX-CLIENT', 'UNIX-SERVER', and 'UNIX-PEER' are rather
 *  like 'TCP-CLIENT', 'TCP-SERVER', and 'TCP-PEER', but for
 *  Unix-domain sockets.  Similarly, 'UNIX-DATAGRAM-CLIENT' and
 *  'UDP-DATAGRAM-SERVER' resemble 'UDP-CLIENT' and 'UDP-SERVER'.
 *  For Unix-domain sockets, pathnames in filesystem space take the
 *  place of network addresses and port numbers.
 *
 *  The Unix-domain server modes, before binding a pathname to the
 *  socket, unlink the existing pathname if any.  The Unix-domain
 *  datagram client mode cannot RECV, as it does not bind a name
 *  and therefore cannot be sent to.  Hence in the datagram world,
 *  it is often best if everyone just be a server, so that sources
 *  can be identified and replied to.
 *
 *  For the 'PIPE-IN' ('PIPE-FROM'), 'PIPE-OUT' ('PIPE-TO'), 'PUMP'
 *  and 'TTY-PUMP' ('LINE-PUMP') modes, the first arg to OPEN is
 *  a command.  The difference between 'TTY-PUMP' ('LINE-PUMP')
 *  and ordinary 'PUMP' is that the ordinary pump is fully buffered,
 *  whereas the child process in the "tty pump" is given an environment
 *  in which its standard input and output are connected to the slave
 *  end of a pseudo-terminal (of which your SETL program gets the
 *  master end as a fd), so whatever buffering normally applies to
 *  interactive use for the command takes place.  This can be used
 *  to allow off-the-shelf programs like "sed" or "awk" to be used
 *  as pumps by causing them to flush after every output line, or to
 *  implement fancy drivers of programs that really are meant to be
 *  interactive, sending them commands and getting back results line
 *  by line, rather in the manner of the "expect" program that is
 *  available on many systems, but under control of the SETL program
 *  rather than of a dedicated external script.
 *
 *  For the 'SIGNAL', 'IGNORE' ('SIGNAL-IGNORE', 'IGNORE-SIGNAL'),
 *  and 'DEFAULT' ('SIGNAL-DEFAULT', 'DEFAULT-SIGNAL') modes, the
 *  signal names that can be in the first OPEN argument are as follows,
 *  with or without the "SIG" prefix, and case is not significant:
 *
 *   signal    default action    meaning
 *   ------    --------------    ------------------
 *   'HUP'     terminate         hangup
 *   'INT'     terminate         interrupt from keyboard
 *   'QUIT'    terminate+core    quit from keyboard
 *   'USR1'    terminate         user-defined signal 1
 *   'USR2'    terminate         user-defined signal 2
 *   'PIPE'    terminate         write to pipe with no readers
 *   'ALRM'    terminate         timer expiry (*)
 *   'TERM'    terminate         software termination
 *   'CHLD'    ignore            child terminated
 *   'CONT'    ignore            continue after stop
 *   'TSTP'    stop (suspend)    terminal stop signal
 *   'TTIN'    stop (suspend)    background process attempting read
 *   'TTOU'    stop (suspend)    background process attempting write
 *   'XCPU'    terminate+core    soft CPU limit exceeded
 *   'XFSZ'    terminate+core    soft filesize limit exceeded
 *   'PWR'     ignore            battery low or power fail imminent
 *   'WINCH'   ignore            terminal window size change
 *
 *  (*) SIGALRM can only be opened in 'IGNORE' or 'DEFAULT' mode, as
 *  outlined below.
 *
 *  Please keep the above table of signals in sync with the ones
 *  recognized by the 'sig_num' function (defined here in sys.c).
 *
 *  SIGPWR and SIGWINCH are not defined by POSIX, but are available on
 *  many Unix-like platforms.
 *
 *  Whenever a signal is received and there is at least one stream
 *  open in 'SIGNAL' mode, a newline is delivered to every such stream.
 *  Otherwise, if the signal has at least one stream open in 'IGNORE'
 *  mode, it is ignored.  Otherwise, if the signal has at least
 *  one stream open in 'DEFAULT' mode, its effect is as shown in
 *  the above table.  If the signal is not open in any mode, its
 *  disposition is determined by the environment in conjunction
 *  with the rules of POSIX.  A login shell will normally provide
 *  dispositions similar to those in the above table.
 *
 *  All stream input routines and SELECT can be used on signal streams
 *  just as they can on regular input streams.
 *
 *  SA_NOCLDSTOP is currently not included in the flags to sigaction(),
 *  so input from a SIGCHLD stream may reflect stop and continue events
 *  as well as termination events; see also STATUS.
 *
 *  Access to timers is provided by 'REAL-MS' mode, for which the
 *  first argument to OPEN must be either [initial,interval] or simply
 *  [interval] (which means [interval,interval]), where initial and
 *  interval are non-negative INTEGERs representing milliseconds.  An
 *  older form where the arg to OPEN can be a STRING containing a
 *  non-negative INTEGER interval denotation (that is acceptable to
 *  VAL) is also supported.  The interval is how long should elapse
 *  between newline deliveries, and initial is how long before
 *  the first one.  Any number of such input streams may be open.
 *  Just as for signal streams, all stream input routines and SELECT
 *  can be used on timer streams.
 *
 *  SIGALRM cannot be opened directly as a 'SIGNAL' stream, though
 *  it can be opened in 'IGNORE' and 'DEFAULT' modes, because timer
 *  streams use SIGALRM.  Having a timer stream open takes precedence
 *  over ignoring or explicitly defaulting SIGALRM, just as having a
 *  'SIGNAL' stream open on any other signal takes precedence over any
 *  'IGNORE' and 'DEFAULT' streams on that signal.
 *
 *  The modes 'N' and 'N+' ensure that the file named by the first
 *  OPEN arg doesn't already exist, and then create it, as a single
 *  atomic operation.  This allows files in the filesystem to be used
 *  as synchronization objects (see also LINK and SYMLINK), though
 *  this is not advised over older versions of NFS.
 *
 *  The check_please arg to 'file_open', if true, causes the OPEN
 *  request to be checked against either the --allow-open or the
 *  --allow-fd-open list (the latter if the first 'file_open' arg is
 *  an integer).  Current callers pass this flag as true in
 *  --restricted mode, with two exceptions:
 *
 *   - where 'file_open' is called initially by the SETL interpreter
 *     to read the input program), and
 *
 *   - mode 'REAL-MS', which is never restricted.
 */
int file_open(block *a, open_how how, bool check_please) {

  const open_mode *mode = find_open_mode(how);

  int fd = -1;

  const filetype ftype = mode->ftype;
  const unsigned abilities = mode->abilities;

  switch (setl_type(a)) {

  case integer_type:
    {
      /*
       *  In all cases, OPENing an INTEGER argument means attaching to
       *  a fd that is already open at the underlying system (OS) level
       *  but not yet at the SETL level.
       *
       *  Opportunities for checks are limited at this stage, so for the
       *  most part, optimistic assumptions are made, with inappropriate
       *  attachments usually being caught on the first I/O operation.
       */
      integer *t = (integer *)a;
      file *f;  /* stream descriptor */
      /*
       *  A check like this may already have been done.  If so, that's
       *  fine.  If not, this gives a consistent message with what it
       *  would have said:
       */
      fd = get_long_in(t, 0,n_files-1, "fd");
      /*
       *  Now a more specific diagnostic for our tighter needs here:
       */
      if (fd < fd_lo || fd >= fd_hi)
      {
        runerr("Existing fd to be opened (%d) must be in range %d..%d",
                                          fd,           fd_lo, fd_hi-1);
      }
      if (check_please) {
        /* Abend if fd,how is not in an --allow-fd-open arg.  */
        check_open_fd_allowed(fd, how);
      }
      f = find_fd_file(fd);
      if (f->ftype != no_file) {
        runerr("File descriptor (fd) %d already open", fd);
      }

      switch (ftype) {
      case stream_file:
      case direct_file:
      case twoway_file:
      case pipe_file:
      case tty_pipe_file:
      case tcp_socket_file:
      case tcp_server_file:
      case udp_client_file:
      case udp_server_file:
      case unix_socket_file:
      case unix_server_file:
      case unix_datagram_client_file:
      case unix_datagram_server_file:
        break;
      default:
        runerr("Cannot open fd %d in %s mode",
                            fd, canonical_mode_name(how));
      } /* end switch (ftype) */

      if (os_is_fd(fd)) {  /* fd is open at the POSIX level */
        /*
         *  Create a new file block and entry in our fd-indexed table.
         */
        switch (ftype) {
        case stream_file:
        case twoway_file:
          new_file (fd, ftype, abilities, os_isatty(fd) ? line_buffering
                                                        : full_buffering);
          break;
        case direct_file:
          new_file (fd, ftype, abilities, full_buffering);
          break;
        case pipe_file:
        case tty_pipe_file:
        case tcp_socket_file:
        case unix_socket_file:
          /*
           *  By using just 'new_file' rather than 'new_pipe_file' here
           *  even in the pipe_file and tty_pipe_file cases, we cause
           *  the ppid field of f to be left as -1 rather than being
           *  set to getpid() as 'new_pipe_file' would do, thus
           *  preventing us from attempting a waitpid() when the stream
           *  is later being closed, and likewise from sending a SIGTERM
           *  to the child of some ancestor if we abend.
           *
           *  The SETL-level use of a mode that leads to one of
           *  the ftypes under this case is taken to mean that the
           *  fd is not connected to a tty-like device.  Contrast
           *  that with the more generic modes such as 'RW' (the
           *  twoway_file case above), which entail an isatty().
           *
           *  The crafty SETL programmer could thus work around the
           *  lack of an interface for explicitly setting buffering
           *  policy by using a mode like 'PUMP' (or even 'TTY-PUMP',
           *  since the master side of a pty/tty pair is never meant
           *  to be a terminal-like device) when OPENing over a fd
           *  that is connected to a tty, in order to cause full
           *  buffering of output rather than the line-by-line
           *  buffering that would occur with the more honest 'RW'
           *  when the ttyness was sensed.
           */
          new_file (fd, ftype, abilities, full_buffering);
          break;
        case tcp_server_file:
        case udp_client_file:
        case udp_server_file:
        case unix_server_file:
        case unix_datagram_client_file:
        case unix_datagram_server_file:
          new_file (fd, ftype, abilities, no_buffering);
          break;
        default:
          unexpected (ftype);
        } /* end switch (ftype) */
      } else {
        fd = -1;  /* fd not open at system level, errno set (to EBADF) */
      }

    }
    break;  /* end case integer_type */

  case string_type:
    {
      string *t = (string *)a;
      const char *what = &strelt(t,1);  /* points into SETL heap (!) */

      switch (ftype) {

      case stream_file:
      case twoway_file:
        if (check_please) {
          check_open_allowed(what, how);  /* abend if not in --allow-open */
        }
        fd = os_open(what, mode->oflag);
        if (fd >= 0) {
          check_not_open(fd);  /* not open at SETL level */
          new_file (fd, ftype, abilities, os_isatty(fd) ? line_buffering
                                                        : full_buffering);
        }
        break;

      case direct_file:
        if (check_please) {
          check_open_allowed(what, how);  /* abend if not in --allow-open */
        }
        fd = os_open(what, mode->oflag);
        if (fd >= 0) {
          check_not_open(fd);  /* not open at SETL level */
          new_file (fd, ftype, abilities, full_buffering);
        }
        break;

      case pipe_file:
        if (check_please) {
          check_open_allowed(what, how);  /* abend if not in --allow-open */
        }
        {
          pid_t pid = co_fork(&fd, abilities, NULL);
          if (pid > 0) {  /* parent */
            new_pipe_file (fd, ftype, abilities, pid);
          } else if (pid == 0) {  /* child */
            os_exec_sh (what);  /* exec sh -c {OPEN arg} */
          } else {
            unexpected (pid);  /* co_fork() should not return on error */
          }
        }
        break;

      case tty_pipe_file:
        if (check_please) {
          check_open_allowed(what, how);  /* abend if not in --allow-open */
        }
        {
          pid_t pid = pty_fork(&fd, abilities, NULL);
          if (pid > 0) {  /* parent */
            new_pipe_file (fd, ftype, abilities, pid);
          } else if (pid == 0) {  /* child */
            os_exec_sh (what);  /* exec sh -c {OPEN arg} */
          } else {
            unexpected (pid);  /* pty_fork() should not return on error */
          }
        }
        break;

      case tcp_socket_file:
      case tcp_server_file:
      case udp_client_file:
      case udp_server_file:
        if (how != open_tcp_peer) {
          char *nodename_buf = NULL;
          const char *nodename = NULL;
          const char *servname = NULL;
          const char *colon = strrchr(what,':');  /* in SETL heap (!) */
          size_t n = 0;
          if (!colon) {
            /* For a server, a missing nodename means listen on all
             * interfaces.  For a client, it means localhost.  */
            servname = what;  /* take whole arg as svc name/num */
          } else {
            n = colon - what;  /* length of nodename */
            if (n != 0) {
              if (n >= NI_MAXHOST) {
                runerr("Host name or address in \"%s\" too long",
                                              tame(what));
              }
              /* nodename := what(1..n) */
              nodename_buf = (char *)arena_alloc(n + 1);
              strncpy_plus_nul (nodename_buf, what, n);
              nodename = nodename_buf;
            }
            servname = colon + 1;
          }
          if (check_please) {
            check_open_allowed(what, how);  /* abend if not in --allow-open */
          }
          fd = open_socket (nodename, servname, ftype, abilities);
          if (nodename_buf != NULL) {
            arena_free(nodename_buf, n + 1);
          }
        } else {
          runerr("TCP-PEER can only be opened over existing INTEGER fd");
        }
        break;

      case unix_socket_file:
      case unix_server_file:
      case unix_datagram_client_file:
      case unix_datagram_server_file:
        if (how != open_unix_peer) {
          if (check_please) {
            check_open_allowed(what, how);  /* abend if not in --allow-open */
          }
          fd = open_unix_socket (what, ftype, abilities);
        } else {
          runerr("UNIX-PEER can only be opened over existing INTEGER fd");
        }
        break;

      case signal_file:
        {
          int sig = sig_num(&strelt(t,1));
          sig_info *p;
          file *f;
          sigset_t old_mask;

          if (sig == SIGALRM) {
            runerr("Cannot open SIGALRM-catching stream.  Use REAL-MS" );
          }
          if (check_please) {
            check_open_allowed(what, how);  /* abend if not in --allow-open */
          }
          p = &sigfo[sig];  /* info about sig */
          fd = new_pseudo_fd();
          f = new_signal_file (fd, ftype, abilities, input_buffering, sig);
          f->evcount = 0;  /* for emphasis */

          os_sigblock(sig, &old_mask);  /* critsect-enter (block sig) */
          hello_sig_reader(sig, sig_handler);  /* update sig disp */
          add_file(&p->readers, f);  /* include f among readers */
          os_sigsetmask(&old_mask);  /* critsect-exit (restore mask) */
        }
        break;

      case sig_ign_file:
        {
          file *f;
          int sig = sig_num(&strelt(t,1));
          sig_info *p = &sigfo[sig];  /* info about sig */
          sigset_t old_mask;

          if (check_please) {
            check_open_allowed(what, how);  /* abend if not in --allow-open */
          }
          fd = new_pseudo_fd();
          /* Nothing can be read from or written to this stream:  */
          f = new_signal_file (fd, ftype, abilities, no_buffering, sig);

          os_sigblock(sig, &old_mask);  /* critsect-enter (block sig) */
          hello_sig_ignorer(sig);  /* update sig disp for new ignorer */
          add_file(&p->ignorers, f);  /* add f to the ignorers */
          os_sigsetmask(&old_mask);  /* critsect-exit (restore mask) */
        }
        break;

      case sig_dfl_file:
        {
          file *f;
          int sig = sig_num(&strelt(t,1));
          sig_info *p = &sigfo[sig];  /* info about sig */
          sigset_t old_mask;

          if (check_please) {
            check_open_allowed(what, how);  /* abend if not in --allow-open */
          }
          fd = new_pseudo_fd();
          /* Nothing can be read from or written to this stream:  */
          f = new_signal_file (fd, ftype, abilities, no_buffering, sig);

          os_sigblock(sig, &old_mask);  /* critsect-enter (block sig) */
          hello_sig_defaulter(sig);  /* update sig disp for new dfltr */
          add_file(&p->defaulters, f);  /* add f to the defaulters */
          os_sigsetmask(&old_mask);  /* critsect-exit (restore mask) */
        }
        break;

      case real_ms_file:
        {
          /* Note that this kills the validity of 't' and 'what':  */
          integer *i = (integer *)val(t);  /* possible g.c. here */
          if (i != OM) {
            const struct timespec interval = integer_ms_to_timespec (i,
                                                       "time interval");
            fd = open_real_ms_timer (interval, interval, abilities);
          } else {
            runerr("STRING in REAL-MS mode must denote an integer");
          }
        }
        break;

      default:
        unexpected (ftype);

      } /* end switch (ftype) */

      /*
       *  For stream types other than timers and network sockets, set
       *  the 'filename' field that will be used by FILENAME and in
       *  name-to-fd lookups.  For timers and network sockets, whose
       *  canonical "names" are actually TUPLEs, not STRINGs, leave
       *  the 'filename' field NULL.
       */
      switch (ftype) {
      case tcp_socket_file:
      case tcp_server_file:
      case udp_client_file:
      case udp_server_file:
      case real_ms_file:
        break;  /* leave f->filename NULL */
      default:
        if (fd >= 0) {
          file *f = find_file(fd);
          f->filename = strdup_malloc(what);
        }
      } /* end switch (ftype) */

    }
    break;  /* end case string_type */

  case tuple_type:
    {
      tuple *t = (tuple *)a;

      switch (ftype) {

      case tcp_socket_file:
      case tcp_server_file:
      case udp_client_file:
      case udp_server_file:
        if (how != open_tcp_peer) {
          const char *nodename, *servname;
          char serv[12];
          if (t->nelt > 2) {
            runerr("More than 2 elements in TUPLE passed to OPEN");
          }
          if (t->nelt == 0 || is_om(tupelt(t,1))) {
            nodename = NULL;  /* any (server) or loopback (client) address */
          } else if (is_string(tupelt(t,1))) {
            string *s = (string *)tupelt(t,1);
            if (s->nchar == 0) {
              nodename = NULL;
            } else {
              nodename = &strelt(s,1);  /* points into SETL heap (!) */
            }
          } else {
            runerr("Host name or address must be STRING or OM, not %s",
                                                TYPENAME(tupelt(t,1)));
          }
          if (t->nelt < 2) {
            servname = "0";  /* let system assign "ephemeral" port number */
          } else if (is_integer(tupelt(t,2))) {  /* port num */
            integer *i = (integer *)tupelt(t,2);
            ro_mpz(z, i)  /* const mpz_t z = alias of INTEGER i */
            if (mpz_sizeinbase(z, 10) > sizeof serv - 2) {  /* many digits */
              runerr("Port number too large");
            }
            /* No explicit check against negative port#, but
             * os_getaddrinfo() (see new_socket()) probably wouldn't like
             * a minus sign in serv:  */
            mpz_get_str (serv, 10, z);  /* serv := port num as string */
            servname = serv;  /* points to local array on stack */
          } else if (is_string(tupelt(t,2))) {  /* port name or num */
            /* If the service name is an empty string, or contains garbage,
             * we do not diagnose that here, but let getaddrinfo() and
             * downstream system calls do what is appropriate with it.  */
            string *s = (string *)tupelt(t,2);
            servname = &strelt(s,1);  /* points into SETL heap (!) */
          } else {
            /* a(2) = OM is taken as "0", as shown above.  */
            runerr("Service name or port number must be STRING or INTEGER,"
                   " not %s", TYPENAME(tupelt(t,2)));
          }

          /* Make sure the OPEN is allowed */
          if (check_please) {
            /* The checking is slightly stricter (more literal) than is
             * absolutely necessary here, but we can live with that.  */
            if (nodename != NULL) {
              /* what := nodename + ':' + servname */
              const size_t n = strlen(nodename) + 1 + strlen(servname);
              char *what = (char *)arena_alloc(n + 1);
              strcpy (what, nodename);
              strcat (what, ":");
              strcat (what, servname);
              check_open_allowed(what, how);
              arena_free(what, n + 1);
            } else {  /* only the servname is present */
              check_open_allowed(servname, how);
            }
          }

          fd = open_socket (nodename, servname, ftype, abilities);

        } else {
          runerr("TCP-PEER can only be opened over existing INTEGER fd");
        }
        break;  /* end network socket cases */

      case real_ms_file:
        {
          struct timespec initial, interval;
          switch (t->nelt) {
          case 1:
            if (is_integer(tupelt(t,1))) {
              integer *i = (integer *)tupelt(t,1);
              interval = integer_ms_to_timespec (i, "timer interval");
              initial = interval;
            } else {
              runerr("[interval] must be [INTEGER], not [%s]",
                                        TYPENAME(tupelt(t,1)));
            }
            break;
          case 2:
            if (is_integer(tupelt(t,1)) &&
                is_integer(tupelt(t,2))) {
              integer *i = (integer *)tupelt(t,1);
              integer *j = (integer *)tupelt(t,2);
              initial = integer_ms_to_timespec (i, "initial delay");
              interval = integer_ms_to_timespec (j, "timer interval");
            } else {
              runerr("[initial, interval] must be [INTEGER, INTEGER],"
                     " not [%s, %s]",
                     TYPENAME(tupelt(t,1)), TYPENAME(tupelt(t,2)));
            }
            break;
          default:
            runerr("TUPLE for REAL-MS mode must have 1 or 2 elements");
          }

          /* OPEN in REAL-MS mode is always allowed, so no check here.  */

          fd = open_real_ms_timer (initial, interval, abilities);
        }
        break;  /* end case real_ms_file */

      default:
        runerr("Cannot open a TUPLE in %s mode",
                                canonical_mode_name(how));

      }  /* end switch (ftype) */

    }
    break;  /* end case tuple_type */

  default:
    unexpected (setl_type(a));

  } /* end switch (setl_type(a)) */

  if (fd >= 0) {
    insert_name_to_fd(fd);  /* add new name if any to name_to_fd mmap */
  }

  return fd;  /* with errno set if -1 */

} /* end file_open */


static void check_open_fd_allowed(int fd, open_how how) {
  allowed *a;
  for (a = allowed_fd_open_list; a != NULL; a = a->next) {
    if (a->how == how && a->fd == fd) {
      return;
    }
  }
  runerr("Opening fd %d in mode %s is restricted",
                     fd, canonical_mode_name(how));
} /* end check_open_fd_allowed */

static void check_open_allowed(const char *what, open_how how) {
  allowed *a;
  switch (how) {
  case open_real_ms:
    return;  /* always allow timers */
  case open_signal:
  case open_ignore:
  case open_default:
    for (a = allowed_open_list; a != NULL; a = a->next) {
      /* Check for equivalent signal name */
      if (a->how == how && sig_num(a->what) == sig_num(what)) {
        return;
      }
    }
    break;  /* signal name 'what' is not on the --allow-open list */
  case open_tcp_client:
  case open_tcp_server:
  case open_udp_client:
  case open_udp_server:
    for (a = allowed_open_list; a != NULL; a = a->next) {
      /* Check for [host:]service match, case-insensitively */
      if (a->how == how && leq_ic(a->what, what)) {
        return;
      }
    }
    break;  /* [host:]service is not on the --allow-open list */
  default:
    /* Filename (e.g. how==open_read) or command (e.g. how==open_pump)
     * or any future thing that requires an exact "what"-part match */
    for (a = allowed_open_list; a != NULL; a = a->next) {
      /* Case-sensitive match.  For commands, this is only natural.
       * For filenames, since we are checking for an allowed security
       * exception, it is appropriately conservative even though the
       * filesystem underlying the open() that is being attempted may
       * turn out to treat filenames case-insensitively.  Thus the
       * caller of OPEN must use the same lettercases as appear on
       * the --allow-open command-line option, in restricted mode.  */
      if (a->how == how && leq(a->what, what)) {
        return;
      }
    }
    break;  /* thing to be OPENed is not on the --allow-open list */
  }
  runerr("Opening \"%s\" in mode %s is restricted",
              tame(what), canonical_mode_name(how));
} /* end check_open_allowed */

static int open_real_ms_timer (struct timespec initial,
                               struct timespec interval,
                               unsigned abilities) {
  const filetype ftype = real_ms_file;
  const int sig = SIGALRM;
  const int fd = new_pseudo_fd();
  sig_info *p = &sigfo[sig];
  sigset_t old_mask;

  if (interval.tv_sec == 0) {
    /*
     *  Approximate 0 as MIN_REAL_MS.
     *
     *  The alternative of making 0 illegal when it could have
     *  semi-innocently got that way from some unintended
     *  roundoff or an overzealous slider seems unhelpful.
     */
    interval.tv_nsec = MAX(interval.tv_nsec,
                           MIN_REAL_MS*(1000*1000));
  }
  file *f = new_signal_file (fd, ftype, abilities, input_buffering, sig);
  f->initial = initial;
  f->interval = interval;
  f->evcount = 0;          /* for emphasis */

  os_sigblock(sig, &old_mask);  /* critsect-enter (block sig) */
  if (p->readers == NULL) {  /* transition from 0 readers to 1 */
    hello_sig_reader(sig, alarm_handler);  /* update sig disp */
#if USE_POSIX_RT
    p->timerid = os_timer_create();  /* generate SIGALRMs when set */
#endif
  }
  /*
   *  Set the "last expiry" timestamp to now + initial - interval:
   */
  f->timestamp = os_mono();  /* now */
  f->timestamp = timespec_plus (f->timestamp, initial);
  f->timestamp = timespec_minus (f->timestamp, interval);
  add_file(&p->readers, f);  /* include f among the sig readers */
  jostle_timers();           /* accommodate f in the schedule */
  os_sigsetmask(&old_mask);  /* critsect-exit (restore signal mask) */

  return fd;

} /* end open_real_ms_timer */

static int open_socket (const char *nodename, const char *servname,
                        filetype ftype, unsigned abilities) {
  int fd;
  int socktype;
  enum socket_role role;
  buffering policy;
  switch (ftype) {
  case tcp_socket_file:
    socktype = SOCK_STREAM;
    role = client_role;
    policy = full_buffering;
    break;
  case tcp_server_file:
    socktype = SOCK_STREAM;
    role = server_role;
    policy = no_buffering;
    break;
  case udp_client_file:
    socktype = SOCK_DGRAM;
    role = client_role;
    policy = no_buffering;
    break;
  case udp_server_file:
    socktype = SOCK_DGRAM;
    role = server_role;
    policy = no_buffering;
    break;
  default:
    unexpected (ftype);
  }
  fd = new_socket (nodename, servname, socktype, role);
  if (fd >= 0) {
    file *f = new_file (fd, ftype, abilities, policy);
    f->nodename = strdup_malloc (nodename);
    f->servname = strdup_malloc (servname);
  }
  return fd;
} /* end open_socket */

static int open_unix_socket (const char *pathname,
                        filetype ftype, unsigned abilities) {
  int fd;
  int socktype;
  enum socket_role role;
  buffering policy;
  switch (ftype) {
  case unix_socket_file:
    socktype = SOCK_STREAM;
    role = client_role;
    policy = full_buffering;
    break;
  case unix_server_file:
    socktype = SOCK_STREAM;
    role = server_role;
    policy = no_buffering;
    break;
  case unix_datagram_client_file:
    socktype = SOCK_DGRAM;
    role = client_role;
    policy = no_buffering;
    break;
  case unix_datagram_server_file:
    socktype = SOCK_DGRAM;
    role = server_role;
    policy = no_buffering;
    break;
  default:
    unexpected (ftype);
  }
  fd = new_unix_socket (pathname, socktype, role);
  if (fd >= 0) {
    new_file (fd, ftype, abilities, policy);
  }
  return fd;
} /* end open_unix_socket */

static int new_socket (const char *nodename, const char *servname,
                       int socktype, enum socket_role role) {
  struct addrinfo hints;
  struct addrinfo *head;
  const struct addrinfo *ai;
  int fd = -1;  /* init to avoid gcc warning */
  int saved_errno = errno;
  memset (&hints, 0, sizeof hints);
  hints.ai_family = AF_UNSPEC;
  hints.ai_socktype = socktype;
  if (role == server_role) hints.ai_flags = AI_PASSIVE;
  if (os_getaddrinfo(nodename, servname, &hints, &head) != 0) return -1;
  for (ai = head; ai != NULL; ai = ai->ai_next) {
    assert (ai->ai_socktype == socktype);
    fd = os_socket(ai->ai_family, ai->ai_socktype, ai->ai_protocol);
    if (fd == -1) continue;  /* to next loop iter. */
    check_not_open(fd);  /* not open at SETL level */
    switch (role) {
    case client_role:
      if (os_connect(fd, ai->ai_addr, ai->ai_addrlen) == -1) {
        os_close(fd);
        continue;  /* to next loop iter. */
      }
      break;  /* from switch */
    case server_role:
      if (ai->ai_socktype == SOCK_STREAM) {
        int ok = 1;
        os_setsockopt(fd, SOL_SOCKET, SO_REUSEADDR, &ok, sizeof ok);
      }
      if (os_bind(fd, ai->ai_addr, ai->ai_addrlen) == -1) {
        os_close(fd);
        continue;  /* to next loop iter. */
      }
      if (ai->ai_socktype == SOCK_STREAM) {
        if (os_listen(fd, 5) == -1) {
          os_close(fd);
          continue;  /* to next loop iter. */
        }
      }
      break;  /* from switch */
    default:
      unexpected(role);
    } /* end switch */
    break;  /* exit loop with connected or bound fd */
  } /* end for (ai ...) */
  os_freeaddrinfo(head);  /* does not change errno */
  if (ai != NULL) {
    errno = saved_errno;
    return fd;  /* success */
  }
  /* The loop completed without success; errno should reflect the
   * failure of some os_*() function.  */
  return -1;
} /* end new_socket */

static int new_unix_socket (const char *pathname,
                       int socktype, enum socket_role role) {
  struct sockaddr_un sockname;
  int fd;
  int saved_errno;
  if (strlen(pathname) >= sizeof sockname.sun_path) {
    runerr("Unix-domain socket pathname \"%s\" too long", tame(pathname));
  }
  fd = socket(AF_UNIX, socktype, 0);
  if (fd == -1) {
    return -1;  /* with errno set by socket() */
  }
  check_not_open(fd);  /* not open at SETL level */
  memset(&sockname, 0, sizeof sockname);
  sockname.sun_family = AF_UNIX;
  strcpy(sockname.sun_path, pathname);
  switch (role) {
  case client_role:
    if (os_connect(fd, (const struct sockaddr *) &sockname,
                                           sizeof sockname) == 0) {
      return fd;  /* connected socket */
    }
    break;  /* with errno from os_connect() */
  case server_role:
    saved_errno = errno;
    errno = 0;
    if (os_unlink(pathname) == 0 || errno == ENOENT) {
      errno = saved_errno;
      /* not meddling with umask here */
      if (os_bind(fd, (const struct sockaddr *) &sockname,
                                          sizeof sockname) == 0 &&
          (socktype == SOCK_DGRAM || os_listen(fd, 5) == 0)) {
        return fd;  /* bound, possibly listening socket */
      }
    }
    break;  /* with errno from os_unlink(), os_bind(), or os_listen() */
  default:
    unexpected(role);
  }
  saved_errno = errno;  /* from os_connect() or os_unlink/bind/listen() */
  os_close(fd);  /* don't care about error in os_close() */
  errno = saved_errno;
  return -1;
} /* end new_unix_socket */

static int new_pseudo_fd(void) {
  int fd;
  for (fd=sd_lo; fd<sd_hi; fd++) {
    file *f = find_file_unchecked(fd);
    if (f->ftype == no_file) return fd;
  }
  runerr("Maximum number of signal and/or timer streams exceeded");
}

/*
 *  Call 'new_file' on a fd connected to a pipe/pump child process.
 *
 *  We do not set FD_CLOEXEC, so the fd will remain open across the
 *  next exec*().  Unless the SETL program EXECs itself away, it won't
 *  care about that.  But other child processes that inherit the fd
 *  might.  Absent an interface to set the FD_CLOEXEC flag, they
 *  should explicitly close the fd if they don't want it to remain
 *  open across the next exec*() and be subject to the whims of the
 *  launched program.  This is not so easy for the child processes
 *  created by OPEN, SYSTEM, or FILTER, however, as the fork+exec is
 *  done as a bundle in those cases.  Maybe enable with a FCNTL or
 *  OPEN mode modifiers or a modal flag someday.  Meanwhile, the
 *  child has to close any unwanted fds.
 */
static file *new_pipe_file(int fd, filetype ftype, unsigned abilities,
                           pid_t pid) {
  file *f = find_fd_file(fd);
  new_file (fd, ftype, abilities, full_buffering);
  /* If you wanted FD_CLOEXEC to reign, you could uncomment this line,
   * but note that that is not the POSIX default:  */
  /* os_fcntl (fd, F_SETFD, FD_CLOEXEC | os_fcntl (fd, F_GETFD)); */
  f->pid = pid;
  f->ppid = os_getpid();
  return f;
}

/*
 *  Call 'new_file' on a pseudo-fd connected to a signal-catching,
 *  signal-ignoring, signal-defaulting, or timer stream.
 */
static file *new_signal_file(int fd, filetype ftype, unsigned abilities,
                             buffering policy, int sig) {
  file *f = find_sd_file(fd);
  new_file (fd, ftype, abilities, policy);
  f->sig = sig;
  return f;
}


int file_mkstemp(string **a) {
  string *s = copy_string(*a);  /* template ending in XXXXXX */
  char *what = &strelt(s,1);
  int fd = os_mkstemp(what);
  if (fd >= 0) {
    file *f;
    check_not_open(fd);  /* not open at SETL level */
    *a = s;  /* update the RW arg string */
    f = new_file (fd, direct_file, can_rws, full_buffering);
    f->filename = strdup_malloc(what);
    insert_name_to_fd(fd);  /* just like file_open() would do */
  }
  return fd;
} /* end file_mkstemp */


int file_accept(int fd) {
  file *f = find_fd_file(fd);
  int gd;
  assert (f->ftype == tcp_server_file ||
          f->ftype == unix_server_file);
  gd = os_accept(fd, NULL, NULL);  /* accept() may set errno */
  if (gd < 0) return gd;
  check_not_open(gd);  /* not open at SETL level */
  new_file (gd, f->ftype == tcp_server_file ? tcp_socket_file
                                            : unix_socket_file,
             duplex, full_buffering);
  /* Nothing suitable for FILENAME to use, other than the fd, is
   * placed in the 'file' record here; all connection information
   * is available through other primitives such as PEER_SOCKADDR and
   * SOCKADDR.  Moreover, nothing is inserted into the name_to_fd
   * map here; there is no "name" other than the fd returned by
   * ACCEPT for the user to use to identify this stream.  */
  return gd;
} /* end file_accept */


/*
 *  Remove fd from the ready_maps cache
 */
static void unready_stream(int fd) {
  key *k;  HANDLE hk;
  int is;
  if (!setl_vm || !setl_vm->ready_maps) return;
  assert (setl_vm->ready_maps->nelt == 3);
  k = num_to_key(fd);  hk = ref(k);  /* fd is the map key */
  for (is=0; is<3; is++) {
    table *t = (table *) tupelt(setl_vm->ready_maps, is+1);
    if (t && (patsiz(t) > 0)) {
      keydel(t,k);  /* delete any entry with that key */
    }
  }
  retire(hk);
} /* end unready_stream */

/*
 *  The complement of 'file_open':
 *
 *   file_close(fd, how);  - implements CLOSE, auto-close, etc.
 *
 *  where how is close_await, close_autoreap, or close_zombie.
 *
 *  If fd has no associated SETL buffer, it is passed to os_close() if
 *  it is a normal fd, or ignored if it is a pseudo-fd; and that's the
 *  end of the story.
 *
 *  Otherwise, the buffer is flushed and destroyed.  Any stream
 *  associated with it by TIE is also flushed, and the association
 *  is dissolved.  Output failures in the flushing are ignored.
 *  The fd is removed from the ready_maps cache (see sys_select())
 *  and the filename's entry from the name_to_fd map.
 *
 *  For a Unix-domain server socket, the pathname to which it is
 *  bound is unlinked (it is not an error for it to have been unlinked
 *  already, say deliberately by the user).
 *
 *  The fd itself is then handed to os_close() if it is a normal fd,
 *  or used to adjust data structures and trigger calls to the likes
 *  of sigaction() and timer_settime() if it is a pseudo-fd.
 *
 *  For streams that also manage a child process (the categories
 *  pipe_file and tty_pipe_file), the 'how' arg is significant, as it
 *  governs how the child will be handled after the fd connected to it
 *  is closed:
 *
 *   close_await - do a blocking waitpid and set STATUS to the child's
 *     exit status (or to OM if someone already reaped it before the
 *     CLOSE, with WAIT or WAITPID).
 *
 *   close_autoreap - unless the child has already exited, its pid
 *     gets put on the 'reapable' list so that the later exit if any
 *     will be handled in the background by the SIGCHLD handler.
 *
 *   close_zombie - do nothing, but let the child become a zombie if
 *     it exits before the parent (caller of CLOSE) exits or does a
 *     successful WAIT or WAITPID on it.
 *
 *  Failure of close(), or of waitpid() in the close_await case (when
 *  STATUS gets set to OM), cause LAST_ERROR to be set.
 */
void file_close(int fd, close_how how) {

  file *f = find_file(fd);
  sig_info *p;
  sigset_t old_mask;
  pid_t pid, ppid, rpid;
  int sig;
  int raw_status;

  if (f->ftype == no_file) {  /* file not open at SETL level */
    if (is_normal_fd(fd)) {  /* but fd is in the valid range */
      /*
       *  CLOSE on an in-range fd that is not open at the SETL level
       *  but may be open at the system level is permitted.  If that
       *  fails, errno will tell why.
       *  
       *  This closes regardless of the can_persist bit in
       *  f->abilities, as that field is undefined when the ftype is
       *  no_file.
       */
      os_close(fd);  /* close() may set errno */
    }
    return;
  }

  assert (f->fd == fd);

  if (f->tie != -1) file_untie(f->tie);

  unready_stream(fd);  /* remove stream from ready_maps cache */
  delete_name_to_fd(fd);  /* remove name if any from name_to_fd mmap */

  switch (f->ftype) {

  case stream_file:
  case direct_file:
  case twoway_file:
  case tcp_socket_file:
  case tcp_server_file:
  case udp_client_file:
  case udp_server_file:
  case unix_socket_file:
  case unix_datagram_client_file:
    assert (is_normal_fd(fd));
    del_file(f);  /* remove any buffering; close fd unless can_persist */
    break;

  case unix_server_file:
  case unix_datagram_server_file:
    assert (is_normal_fd(fd));
    if (f->filename) {
      /* User may well have UNLINKed it early, so ignore unlink() errs */
      int saved_errno = errno;
      os_unlink(f->filename);
      errno = saved_errno;
    }
    del_file(f);  /* remove any buffering; close fd */
    break;

  case pipe_file:
  case tty_pipe_file:
    assert (is_normal_fd(fd));
    pid = f->pid;
    ppid = f->ppid;
    /*
     *  Doing the del_file() here rather than as a last act makes us,
     *  the parent, close our end of the connection unless there is an
     *  unclosed duplicate of the fd.  If the child is waiting on its
     *  stdin, it will then get an EOF, and hopefully exit as a result
     *  so that we don't wait forever for it even in the close_await
     *  case below.
     */
    del_file(f);  /* remove buffering; close fd */
    if (ppid == os_getpid()) {
      /*
       *  We are the direct parent (ppid) of this child (pid), and
       *  therefore responsible for reaping its status.
       */
      switch (how) {
      case close_await:
        /*
         *  We do not block SIGCHLD during the synchronous os_waitpid()
         *  here, for the same reason as in do_system() (not wanting to
         *  interfere with processing of the 'reapable' list by the
         *  SIGCHLD handler).
         */
        rpid = os_waitpid(pid, &raw_status, 0);
        if (rpid == pid) {
          set_raw_status(raw_status);
        } else {
          /*
           * Error.  Probably the pid does not exist, which probably
           * means that a WAIT or WAITPID call already reaped the
           * status.  The caller who cares should be able to detect
           * the probable errno==ECHILD via LAST_ERROR.  Since the
           * true status is now irrecoverable, we set STATUS to OM.
           */
          set_raw_status(no_status);
        }
        break;
      case close_autoreap:
        /*
         *  We do block SIGCHLD around this initial nonblocking attempt
         *  to reap the status of the child, and leave it blocked
         *  during the list manip that follows.  Blocking SIGCHLD before
         *  the os_waitpid() call avoids a race condition where
         *  os_waitpid() returns 0, the child exits and causes the
         *  handler to fire (it finds nothing), and the pid is then
         *  added to the 'reapable' (to be auto-reaped) list, a moment
         *  too late.  It could remain a zombie for a long time then.
         *  This timely SIGCHLD blockage avoids that.
         */
        os_sigblock(SIGCHLD, &old_mask);  /* exclude SIGCHLD handler */
        rpid = os_waitpid(pid, NULL, WNOHANG);
        if (rpid == pid) {
          /*
           * Good, we're done.
           */
        } else if (rpid == 0) {
          /*
           * Status is not yet available for this child process.
           * Probably it has not yet terminated.  Put its pid in the
           * reapable set (list) processed by our SIGCHLD handler
           * (sigchld_handler() in init.c):
           */
          pid_list *member;
          if (reaped) {
            /* Reuse a block from the 'reaped' free list */
            member = reaped;
            reaped = reaped->next;
          } else {
            /* Must grab a fresh block */
            member = (pid_list *) os_malloc (sizeof *member);
          }
          member->pid = pid;
          member->next = reapable;  /* old head */
          reapable = member;
        } else {
          /*
           * Non-fatal error as for the CLOSE_AWAIT case above;
           * errno will have been set.
           *
           * But here we don't set_raw_status(), as we're never
           * supposed to in this CLOSE_AUTOREAP case.
           */
        }
        os_sigsetmask(&old_mask);  /* unblock SIGCHLD */
        break;
      case close_zombie:
        /*
         *  Nothing to do.  User responsibility to do the WAITPID
         *  themself if they care.
         */
        break;
      default:
        unexpected (how);
      }
    } else {
      /* Not our child; no point in calling waitpid() on it. */
    }
    break;

  /*
   *  The signal_file, sig_ign_file, sig_dfl_file, and real_ms_file
   *  cases below reverse what is done by the corresponding cases in
   *  file_open().  Thus the del_file() call at the end of each case
   *  here undoes the new_pseudo_fd()+new_signal_file() pair of calls
   *  near the beginning of the case there, the early excise_file()
   *  here reverses the late add_file() there, and the hello_sig_*()
   *  and bye_sig_*() calls are inverses.
   */

  case signal_file:
    assert (is_pseudo_fd(fd));
    sig = f->sig;
    p = &sigfo[sig];  /* info about sig */
    os_sigblock(sig, &old_mask);  /* critsect-enter (block sig) */
    excise_file(&p->readers, f);  /* remove f from list of readers */
    bye_sig_client(sig);  /* update sig disp for loss of reader */
    os_sigsetmask(&old_mask);  /* critsect-exit (restore mask) */
    del_file(f);  /* remove buffering; make f and fd available again */
    break;

  case sig_ign_file:
    assert (is_pseudo_fd(fd));
    sig = f->sig;
    p = &sigfo[sig];  /* info about sig */
    os_sigblock(sig, &old_mask);  /* critsect-enter (block sig) */
    excise_file(&p->ignorers, f);  /* remove f from list of ignorers */
    bye_sig_client(sig);  /* update sig disp for loss of ignorer */
    os_sigsetmask(&old_mask);  /* critsect-exit (restore mask) */
    del_file(f);  /* make f and fd available again */
    break;

  case sig_dfl_file:
    assert (is_pseudo_fd(fd));
    sig = f->sig;
    p = &sigfo[sig];  /* info about sig */
    os_sigblock(sig, &old_mask);  /* critsect-enter (block sig) */
    excise_file(&p->defaulters, f);  /* remove f from list of defaulters */
    bye_sig_client(sig);  /* update sig disp for loss of defaulter */
    os_sigsetmask(&old_mask);  /* critsect-exit (restore mask) */
    del_file(f);  /* make f and fd available again */
    break;

  case real_ms_file:
    assert (is_pseudo_fd(fd));
    sig = f->sig;
    assert (sig == SIGALRM);
    p = &sigfo[sig];
    os_sigblock(sig, &old_mask);  /* critsect-enter (block sig) */
    excise_file(&p->readers, f);  /* remove f from list of readers */
    if (p->readers == NULL) {
      /*
       *  No more readers of this timer.  Delete the timer and
       *  use bye_sig_client() to update the SIGALRM disposition in
       *  accordance with any ignorers or defaulters that have been
       *  inherited by the current program.
       */
      if (how != close_atfork) {
        /* In a new child process, the timerid will not be valid. */
#if USE_POSIX_RT
        os_timer_delete(p->timerid);
#else
        const struct itimerval disarm = {{0,0}, {0,0}};
        os_set_timer(ITIMER_REAL, &disarm);
#endif
      }
      bye_sig_client(sig);  /* update sig disp for no readers */
    } else {            /* still have some readers of this timer */
      if (how != close_atfork) {
        jostle_timers();  /* accommodate loss of f in the schedule */
      }
    }
    os_sigsetmask(&old_mask);  /* critsect-exit (restore mask) */
    del_file(f);  /* remove buffering; make f and fd available again */
    break;

  default:
    unexpected (f->ftype);

  } /* end switch (f->ftype) */

} /* end file_close */


/*
 *  The following hello_sig_* and bye_sig_* functions need to have
 *  'sig' blocked before they are called.
 */

/*
 *  Adjust signal action for a new reader about to be added
 */
static void hello_sig_reader(int sig, const sig_disp handler) {
  if (sig != SIGCHLD) {  /* SIGCHLD has its own handler */
    sig_info *p = &sigfo[sig];  /* info about sig */
    if (p->readers == NULL) {  /* list of readers is empty */
      if (p->ignorers == NULL &&
          p->defaulters == NULL) {
        /*
         *  Transition from no clients of the signal to one reader.
         *  Save old signal action and establish new handler.
         */
        os_signal(sig, handler, &p->old_action);
      } else {
        /*
         *  Transition from no readers but at least one ignorer or
         *  defaulter to one reader.  Old signal action was already
         *  saved, so just establish new handler.
         */
        os_signal(sig, handler, NULL);
      }
    }
  }
} /* end hello_sig_reader */

/*
 *  Adjust signal action for a new ignorer about to be added
 */
static void hello_sig_ignorer(int sig) {
  if (sig != SIGCHLD) {  /* SIGCHLD has its own handler */
    sig_info *p = &sigfo[sig];  /* info about sig */
    if (p->readers == NULL &&
        p->ignorers == NULL) {
      if (p->defaulters == NULL) {
        /*
         *  Transition from no clients of the signal to one ignorer.
         *  Save old signal action and set new disposition to SIG_IGN.
         */
        os_signal(sig, SIG_IGN, &p->old_action);
      } else {
        /*
         * We let 'ignore' take precedence over 'default'; the old
         * signal action was already saved.
         */
        os_signal(sig, SIG_IGN, NULL);
      }
    } else {
      /*
       *  Adding an ignorer when there is already at least one reader
       *  or ignorer has no effect on the signal disposition.
       */
      ;
    }
  }
} /* end hello_sig_ignorer */

/*
 *  Adjust signal action for a new defaulter about to be added.
 */
static void hello_sig_defaulter(int sig) {
  if (sig != SIGCHLD) {  /* SIGCHLD has its own handler */
    sig_info *p = &sigfo[sig];  /* info about sig */
    if (p->readers == NULL &&
        p->ignorers == NULL &&
        p->defaulters == NULL) {
      /*
       *  Transition from no clients of the signal to one defaulter.
       *  Save old signal action and set new disposition to SIG_DFL.
       */
      os_signal(sig, SIG_DFL, &p->old_action);
    } else {
      /*
       *  Adding a defaulter when there is already at least one
       *  client has no effect on the signal disposition.
       */
      ;
    }
  }
} /* end hello_sig_defaulter */

/*
 *  Adjust signal action as needed for loss of a client.
 *
 *  A common treatment suffices here (several hello_sig_*()
 *  functions, but just this one bye_sig_*() function), because
 *  the new disposition is based on the new state of the readers,
 *  ignorers, and defaulters lists along with the presumption in
 *  the case where all 3 lists are empty that there was a client
 *  of some kind that is now being closed (and hence we can
 *  restore the "old" signal action in that case).
 */
static void bye_sig_client(int sig) {
  if (sig != SIGCHLD) {  /* SIGCHLD has its own handler */
    sig_info *p = &sigfo[sig];  /* info about sig */
    if (p->readers == NULL) {
      if (p->ignorers != NULL) {
        /* Down to 0 readers, but still have at least 1 ignorer */
        os_signal(sig, SIG_IGN, NULL);
      } else if (p->defaulters != NULL) {
        /* Down to 0 readers and ignorers, but still >=1 defaulter */
        os_signal(sig, SIG_DFL, NULL);
      } else {
        /* No more SETL clients; restore old signal action */
        os_sigaction(sig, &p->old_action, NULL);
      }
    }
  }
} /* end bye_sig_client */

/*
 *  Insert f into the list whose head ptr is pointed to by h
 */
static void add_file(file **h, file *f) {
  f->next = *h;  /* list head */
  *h = f;        /* push f onto list */
}

/*
 *  Unlink f from the list whose head ptr is pointed to by h
 */
static void excise_file(file **h, file *f) {
  while (*h != NULL) {
    if (*h == f) {    /* found f */
      *h = f->next;   /* bypass f in the list */
      return;         /* only remove the first occurrence of f */
    }
    h = &(*h)->next;  /* ptr to the ptr to update if next *h is f */
  }
#if 0
  /* This version would be better than the one above if you
   * thought you might ever want to remove the return statement
   * in order to remove all occurrences of f:  */
  file *cur = *h;  /* init cur to list head */
  while (cur != NULL) {
    if (cur == f) {
      *h = cur->next;  /* bypass f in the list */
      return;
    }
    h = &cur->next;  /* ptr to what to update if next cur is f */
    cur = *h;  /* advance cur to next */
  }
#endif
}


void file_shutdown(int fd, int how) {
  file *f = find_fd_file(fd);
  if (f->ftype == no_file) {
    os_shutdown(fd, how);  /* may set errno */
    return;
  }
  assert (f->fd == fd);
  /* Any needed flushing for the WR and RDWR cases needs to have been
   * done already.  See e.g. the getfd_shut_* cases near the end of
   * getfd().  */
  switch (how) {
  case SHUT_RD:
    assert (test_all(f->abilities, can_read | can_shutdown));
    f->abilities &= ~can_read;
    break;
  case SHUT_WR:
    assert (test_all(f->abilities, can_write | can_shutdown));
    f->abilities &= ~can_write;
    break;
  case SHUT_RDWR:
    assert (test_all(f->abilities, can_rw | can_shutdown));
    f->abilities &= ~can_rw;
    break;
  default:
    unexpected (how);
  }
  os_shutdown(fd, how);  /* may set errno */
  if (f->tie != -1) file_untie(f->tie);
} /* end file_shutdown */


void file_flush(int fd) {
  file *f = find_file(fd);
  flush_file(f);  /* may set errno */
}

void file_drain(int fd) {
  file *f = find_file(fd);
  drain_file(f);
}

off_t file_seek(int fd, off_t offset, int whence) {
  file *f = find_file(fd);
  return seek_file(f, offset, whence);
}

/*
 *  Whereas the file_pos field of the io_buffer record is always
 *  supposed to reflect the current underlying OS file position,
 *  this FILEPOS impl gives the position from the SETL point of view.
 *  (For SEEK, those end up the same, as SEEK flushes and drains the
 *  stream before returning the pos.)
 *
 *  In most cases, file_pos() doesn't do any system calls, but in a+
 *  mode when there is unflushed output in the buffer, it does a
 *  silent (error-ignoring) lseek() to find out the length of the
 *  file before that buffered output is written.
 */
off_t file_pos(int fd) {
  file *f = find_file(fd);
  off_t pos = 0;
  io_buffer *b = f->buffer;
  if (b) {
    assert (b->fd == fd);
    if (b->seekable && b->appending && b->j_out > 0) {
      /*
       *  In this corner case where the mode is a+ (seekable and
       *  appending), and there is buffered output that has not yet
       *  been written, we don't know if the OS-level file pos is at
       *  the end of the file or not, since we don't track that state
       *  (though we could, with some fussy little at_end flag that
       *  has to be cleared whenever an lseek() puts it in doubt),
       *  and we want the result of FILEPOS to reflect the pos that
       *  would obtain if the current buffer were now flushed.
       *
       *  So we make sure to update the file_pos field in the io_buffer
       *  to the end of the file at the OS level.  We do that by
       *  calling lseek() here, even though for a user who calls
       *  FILEPOS repeatedly before the next flush, this entails the
       *  overhead of redundant lseek() calls (and the negligible
       *  benefit of catching up more often with what other processes
       *  might be doing to the file).  When there is nothing in
       *  the output buffer, we don't do this, as the last operation
       *  may very well have been a seek (possibly followed by some
       *  reading) that leaves us not at the end of the file, whereas
       *  we know that cannot be the case if the buffer is non-empty,
       *  as input and seek ops trigger flushing.
       *
       *  Note the resemblance of this to the code in flush_io_buffer(),
       *  which takes care of updating file_pos after an os_write(), at
       *  which point the output buffer is emptied.
       */
      int saved_errno = errno;
      off_t end_pos = lseek (fd, 0, SEEK_END);
      if (end_pos != -1) {
        b->file_pos = end_pos;
      }
      errno = saved_errno;  /* ignore lseek() errors */
    }
    /*
     *  Since we don't flush or drain in this enquiry-only function,
     *  we have to adjust for unflushed output (add j_out) and
     *  unconsumed input (subtract n_in-j_in).
     */
    pos = b->file_pos + b->j_out - (b->n_in - b->j_in);
    assert (pos >= 0);
  }
  return pos;
} /* end file_pos */

void file_tie(int fd, int gd) {
  file *f = find_file(fd);
  file *g = find_file(gd);
  if (f->tie != -1) file_untie(f->tie);
  if (g->tie != -1) file_untie(g->tie);
  f->tie = gd;
  g->tie = fd;
}

void file_untie(int fd) {
  file *f = find_file(fd);
  if (f->tie != -1) {
    file *g = find_file(f->tie);
    g->tie = -1;
    f->tie = -1;
  }
}


/*
 *  This getfd() is a central routine in the I/O system, widely used to
 *  prepare for I/O operations.  It takes a putative stream reference
 *  (STRING name, TUPLE network or timer reference, or INTEGER fd) and
 *  returns a file descriptor for it, if it can do so unambiguously.
 *  It auto-opens the stream on the way by if nec and if it thinks it
 *  can guess the type of stream desired.
 *
 *  It also can return -1 in these cases of the 'intent' arg:
 *
 *   getfd_check - to indicate that the stream is not open;
 *
 *   getfd_getfile - to let GETFILE return OM on an auto-open failure,
 *                   with errno set, rather than abending which is
 *                   what happens with all other auto-open failures.
 *
 *  In all cases but getfd_getfile, getfd() abends on errors.
 *  On success, it preserves errno.
 */
int getfd(block *a, getfd_op intent) {

  HANDLE ha = ref(a);

  /* For when the stream "name" is a canonicalized ordered pair:  */
  tuple *name = OM;  HANDLE h_name = ref(name);

  int saved_errno = errno;

  int fd = -1;  /* return value */
  file *f;  /* scratch variable */

  /*  Setup phase.  Obtain a candidate fd.  */

  switch (setl_type(a)) {

  case integer_type:
    {
      integer *t = (integer *)a;
      fd = get_long_in(t, 0,n_files-1, "fd");
    }
    break;

  case string_type:
    {
      string *t = (string *)a;  HANDLE ht = ref(t);
      set *s = mmap_fetch (name_to_fd, (block *)t);

      switch (s->card) {

      case 0:  /* STRING a is not in the name_to_fd domain */
        {
          const char *what = &strelt(t,1);
          const long n = t->nchar;
          if (n > PATH_MAX) {
            runerr("Name with %ld chars (%s) is too long (> %d)",
                              n,    tame(what),     (int)PATH_MAX);
          }
          if (
            leq_ic(what,"STDIN") ||
            leq_ic(what,"INPUT"))  fd = fd_stdin;
          else if (
            leq_ic(what,"STDOUT") ||
            leq_ic(what,"OUTPUT")) fd = fd_stdout;
          else if (
            leq_ic(what,"STDERR") ||
            leq_ic(what,"ERROR"))  fd = fd_stderr;
          else if (what[0]=='\0' || leq(what,"-")) {
            switch (intent) {
            case getfd_read:     fd = fd_stdin;  break;
            case getfd_getfile:  fd = fd_stdin;  break;
            case getfd_write:    fd = fd_stdout; break;
            case getfd_putfile:  fd = fd_stdout; break;
            default:             fd = -1;        break;
            }
          } else {
            fd = -1;  /* possible candidate for auto-open */
          }
        }
        break;

      case 1:
        fd = unique_fd(s);
        break;

      default:
        stream_error("Ambiguity - more than one %@ is currently open", a);
      }

      retire(ht);
    }
    break;  /* end case string_type */

  case tuple_type:
    {
      tuple *t = (tuple *)a;  HANDLE ht = ref(t);
      set *s;
      if (t->nelt > 2) {
        runerr("More than 2 elements (%ld) in TUPLE to auto-open",
                                    t->nelt);
      }
      name = new_tuple(2);
      /* The aliases in name to members of t are thought to be OK:  */
      if (t->nelt == 0 || is_string(tupelt(t,1)) || is_om(tupelt(t,1))) {
        /* socket */
        if (t->nelt > 0 && is_string(tupelt(t,1))) {
          if (((string *)tupelt(t,1))->nchar > 0) {
            let (tupelt(name,1), tupelt(t,1));  /* node name or addr */
          }
        }
        if (t->nelt < 2) {
          let (tupelt(name,2), (block *)new_string("0"));
        } else if (is_string(tupelt(t,2))) {  /* service name or num */
          let (tupelt(name,2), tupelt(t,2));
        } else if (is_integer(tupelt(t,2))) {  /* INTEGER portnum */
          let (tupelt(name,2), (block *)tostr(tupelt(t,2)));
        } else {
          runerr("Invalid type in 2nd member of TUPLE to auto-open");
        }
      } else if (is_integer(tupelt(t,1))) {
        /* real-ms timer */
        let (tupelt(name,1), tupelt(t,1));
        if (t->nelt < 2) {
          let (tupelt(name,2), tupelt(t,1));  /* [i] becomes [i,i] */
        } else if (is_integer(tupelt(t,2))) {
          let (tupelt(name,2), tupelt(t,2));
        } else {
          runerr("Need INTEGER after INTEGER in TUPLE to auto-open");
        }
      } else {
        runerr("Invalid type in 1st member of TUPLE to auto-open");
      }
      s = mmap_fetch (name_to_fd, (block *)name);
      switch (s->card) {
      case 0:
        break;  /* not found; fd remains -1 */
      case 1:
        fd = unique_fd(s);
        break;
      default:
        stream_error("Ambiguity - more than one %@ is currently open",
                                       (block *)name);
      }
      retire(ht);
    }
    break;

  default:  /* getfd must be called with INTEGER or STRING or TUPLE */
    unexpected (setl_type(a));
  }

  /*  Checking and auto-opening phase.  */

  if (fd < 0 || (f = find_file(fd))->ftype == no_file) {
    /*
     *  File not currently open
     */
    switch (intent) {

    case getfd_check:
    case getfd_ftrunc:
      fd = -1;
      break;  /* yay, we have our (negative) result */

    case getfd_info:
    case getfd_sel_e:
    case getfd_flush:
      stream_error("Stream %@ is not open", a);

    case getfd_accept:
      switch (setl_type(a)) {
      case integer_type:
      case string_type:
      case tuple_type:
        fd = file_open(a, open_tcp_server, restricted);
        if (fd < 0) {
          stream_error("Cannot auto-open %@ as a TCP server socket", a);
        }
        break;  /* success */
      default:
        unexpected (setl_type(a));
      }
      /* NOT setting auto_close flag here (when to stop ACCEPTing?).  */
      break;

    case getfd_recv:
    case getfd_send:
      switch (setl_type(a)) {
      case integer_type:
      case string_type:
      case tuple_type:
        fd = file_open(a, open_udp_client, restricted);
        if (fd < 0) {
          stream_error("Cannot auto-open %@ as a UDP client socket", a);
        }
        break;  /* success */
      default:
        unexpected (setl_type(a));
      }
      /* NOT setting the auto_close flag here (no EOF in UDP).  */
      break;

    case getfd_recvfrom:
    case getfd_sendto:
      switch (setl_type(a)) {
      case integer_type:
      case string_type:
      case tuple_type:
        fd = file_open(a, open_udp_server, restricted);
        if (fd < 0) {
          stream_error("Cannot auto-open %@ as a UDP server socket", a);
        }
        break;  /* success */
      default:
        unexpected (setl_type(a));
      }
      /* NOT setting the auto_close flag here (no EOF in UDP).  */
      break;

    case getfd_getfile:
      switch (setl_type(a)) {
      case integer_type:
      case string_type:
        /* As with getfd_read, this silly bit of automation might seem
         * counterintuitive to those expecting something other than an
         * attempt to read a file when they go GETFILE 'localhost:13'
         * (they should say GETFILE ['localhost', 13] or even just
         * GETFILE [OM, 13]), but that's the price of progress I guess.  */
        fd = file_open(a, open_read, restricted);
        break;
      case tuple_type:
        /* 'name' is already canonicalized, so we don't need to check
         * for [] here.  */
        switch (setl_type(tupelt(name,1))) {
        case string_type:
        case om_type:
          fd = file_open(a, open_tcp_client, restricted);
          break;
        case integer_type:
          /* You would not want GETFILE [55] or GETFILE [55,55] to work,
           * as it would just sit there forever absorbing SIGALRMs and
           * building up an infinite string of newlines.  */
          stream_error("Will not auto-open %@ for GETFILE", a);
        default:
          unexpected (setl_type(tupelt(name,1)));
        }
        break;
      default:
        unexpected (setl_type(a));
      }
      if (fd >= 0) {
        f = find_fd_file(fd);
        f->auto_close = true;  /* auto-CLOSE on EOF */
      } else {  /* errno should say why file_open() failed */
        /* To let GETFILE return OM on auto-open failure rather than
         * abending, we continue here with fd = -1, and make the errno
         * set by file_open() stick.  */
        saved_errno = errno;  /* so this is what will be "restored" */
      }
      break;

    /*
     *  There was a time when auto-opened buffers over existing fds
     *  were made "persistent" so that the fd would not be closed on
     *  the subsequent close.  So you see extra CLOSEs in the thesis.
     *  Nowadays, we close the underlying fd even though we inherited
     *  it, as the use case for leaving it open seems slender to none.
     *  If you have one, you can effectively keep the fd open by using
     *
     *   gd := DUP(fd);  -- preserve file description
     *   CLOSE(fd);      -- close SETL stream and fd
     *   DUP2(gd, fd);   -- restore fd
     *   CLOSE(gd);      -- done with temporary
     */
    case getfd_read:
    case getfd_sel_r:
      switch (setl_type(a)) {
      case integer_type:
      case string_type:
        fd = file_open(a, open_read, restricted);
        if (fd < 0) {
          stream_error("Cannot auto-open %@ for input", a);
        }
        break;  /* success */
      case tuple_type:  /* canonical form of 'a' is in 'name' */
        switch (setl_type(tupelt(name,1))) {
        case string_type:
        case om_type:
          fd = file_open(a, open_tcp_client, restricted);
          if (fd < 0) {
            stream_error("Cannot auto-open %@ as a TCP client", a);
          }
          break;  /* success */
        case integer_type:
          fd = file_open(a, open_real_ms, false);
          if (fd < 0) {
            stream_error("Cannot auto-open %@ in REAL-MS mode", a);
          }
          break;  /* success */
        default:
          unexpected (setl_type(tupelt(name,1)));
        }
        break;
      default:
        unexpected (setl_type(a));
      }
      f = find_fd_file(fd);
      f->auto_close = true;  /* auto-CLOSE on EOF */
      break;

    case getfd_write:
    case getfd_sel_w:
    case getfd_putfile:
      switch (setl_type(a)) {
      case integer_type:
      case string_type:
        fd = file_open(a, open_write, restricted);
        if (fd < 0) {
          stream_error("Cannot auto-open %@ for output", a);
        }
        break;  /* success */
      case tuple_type:  /* canonical form of 'a' is in 'name' */
        switch (setl_type(tupelt(name,1))) {
        case string_type:
        case om_type:
          fd = file_open(a, open_tcp_client, restricted);
          if (fd < 0) {
            stream_error("Cannot auto-open %@ as a TCP client", a);
          }
          break;  /* success */
        case integer_type:
          stream_error("Will not auto-open %@ for output", a);
        default:
          unexpected (setl_type(tupelt(name,1)));
        }
        break;
      default:
        unexpected (setl_type(a));
      }
      f = find_fd_file(fd);
      f->auto_close = true;  /* auto-CLOSE after EOF */
      if (intent == getfd_putfile) {
        f->auto_close_output = true;  /* auto-CLOSE after PUTFILE */
      }
      break;

    case getfd_seek:
    case getfd_gets:
    case getfd_puts:
      switch (setl_type(a)) {
      case integer_type:
      case string_type:
        fd = file_open(a, open_direct, restricted);
        if (fd < 0) {
          stream_error("Cannot auto-open %@ in r+ mode", a);
        }
        break;  /* success */
      case tuple_type:
        stream_error("Will not auto-open %@ in r+ mode", a);
      default:
        unexpected (setl_type(a));
      }
      /* NOT setting the auto_close flag here.  */
      break;

    case getfd_recv_fd:
    case getfd_send_fd:
      if (is_normal_fd(fd)) {
        /* Allow use of normal-range fd not open at SETL level */
      } else {
        stream_error("Will not auto-open %@ for fd passing", a);
      }
      break;

    case getfd_sockaddr:
      if (is_normal_fd(fd)) {
        /* Allow use of normal-range fd not open at SETL level */
      } else {
        stream_error("Will not auto-open %@ for socket enquiry", a);
      }
      break;

    case getfd_shut_r:
    case getfd_shut_w:
    case getfd_shut_rw:
      if (is_normal_fd(fd)) {
        /* Allow use of normal-range fd not open at SETL level */
      } else {
        stream_error("Will not auto-open %@ for SHUTDOWN", a);
      }
      break;

    case getfd_close:
      if (fd < 0) {
        stream_error("Stream %@ is not open", a);  /* non-INTEGER */
      }
      /* Allow fd in the "sd" range to be CLOSEd without complaint too.  */
      break;  /* you can CLOSE a fd not open at SETL level */

    default:
      unexpected (intent);

    } /* end switch (intent) */

  } else {
    /*
     *  The fd exists
     */
    switch (intent) {
    case getfd_check:
    case getfd_info:
    case getfd_flush:
      if (test_all(f->abilities, can_ask)) break;
      bugerr("Missing can_ask flag on apparently open file");
    case getfd_read:
    case getfd_getfile:
      if (test_all(f->abilities, can_read)) break;
      stream_error("Stream %@ is not open for input", a);
    case getfd_write:
    case getfd_putfile:
      if (test_all(f->abilities, can_write)) break;
      stream_error("Stream %@ is not open for output", a);
    case getfd_seek:
      if (test_all(f->abilities, can_seek)) break;
      stream_error("Stream %@ is not open for direct access", a);
    case getfd_gets:
      if (test_all(f->abilities, can_rs)) break;
      stream_error("Stream %@ must be open for seeking and input", a);
    case getfd_puts:
      if (test_all(f->abilities, can_ws)) break;
      stream_error("Stream %@ must be open for seeking and output", a);
    case getfd_ftrunc:
      /* stream must be writable, and at least one of seekable and appending */
      if (test_all(f->abilities, can_ws)) break;
      if (test_all(f->abilities, can_wa)) break;
      stream_error("Cannot FTRUNC stream %@", a);
    case getfd_accept:
      if (test_all(f->abilities, can_accept)) break;
      stream_error("Stream %@ is not a server socket", a);
    case getfd_recv:
      if (test_all(f->abilities, can_recv)) break;
      stream_error("Stream %@ is not a UDP client socket", a);
    case getfd_send:
      if (test_all(f->abilities, can_send)) break;
      stream_error("Stream %@ is not a datagram client socket", a);
    case getfd_recvfrom:
      if (test_all(f->abilities, can_recvfrom)) break;
      stream_error("Stream %@ is not a datagram server socket", a);
    case getfd_sendto:
      if (test_all(f->abilities, can_sendto)) break;
      stream_error("Stream %@ is not a datagram server socket", a);
    case getfd_sel_r:
      if (test_all(f->abilities, can_sel_r)) break;
      stream_error("Cannot SELECT on %@ for input-type operation", a);
    case getfd_sel_w:
      if (test_all(f->abilities, can_sel_w)) break;
      stream_error("Cannot SELECT on %@ for output-type operation", a);
    case getfd_sel_e:
      if (test_all(f->abilities, can_sel_e)) break;
      stream_error("Cannot SELECT on %@ for I/O exception detection", a);
    case getfd_shut_r:
    case getfd_shut_w:
    case getfd_shut_rw:
      if (!test_all(f->abilities, can_shutdown)) {
        stream_error("SHUTDOWN on %@ is not allowed", a);
      }
      switch (intent) {
      case getfd_shut_r:
        if (test_all(f->abilities, can_read)) break;
        stream_error("Stream %@ is not open for input", a);
      case getfd_shut_w:
        if (test_all(f->abilities, can_write)) break;
        stream_error("Stream %@ is not open for output", a);
      case getfd_shut_rw:
        if (test_all(f->abilities, can_rw)) break;
        stream_error("Stream %@ is not open for bidirectional I/O", a);
      default:
        unexpected (intent);
      }
      break;
    case getfd_recv_fd:
      if (is_normal_fd(fd)) break;
      stream_error("Cannot RECV_FD on %@", a);
    case getfd_send_fd:
      if (is_normal_fd(fd)) break;
      stream_error("Cannot SEND_FD on %@", a);
    case getfd_sockaddr:
      if (is_normal_fd(fd)) break;
      stream_error("Cannot perform socket enquiry on %@", a);
    case getfd_close:
      break;  /* this request is always accepted */
    default:
      unexpected (intent);
    }
  }

  /*  Endphase.  */

  if (fd >= 0) {
    /* We ignore any errno set by flush_tie() or file_flush(), by
     * restoring errno just before return.  */
    switch (intent) {
    case getfd_read:
    case getfd_getfile:
    case getfd_recv_fd:
    case getfd_sel_r:
      /* Flush output for fd and any tied fd before doing input.  */
      flush_tie(fd);
      break;
    case getfd_write:
    case getfd_putfile:
      file_drain(fd);  /* drain input before doing output */
      break;
    case getfd_seek:
    case getfd_gets:
    case getfd_puts:
    case getfd_close:
    case getfd_send_fd:
    case getfd_shut_w:
    case getfd_shut_rw:
    case getfd_ftrunc:
      /* Flush output for fd before doing any of the ops in this set of
       * cases.  */
      file_flush(fd);  /* no-op if buffer is empty or no buffer */
      /* The caller also drains the input after this getfd() call in the
       * getfd_seek, getfd_gets, getfd_puts, and getfd_ftrunc cases.  */
      break;
    default:
      break;
    }
  }

  retire(h_name);
  retire(ha);

  errno = saved_errno;

  return fd;

} /* end getfd */

static int unique_fd(set *s) {
  subnode *b;
  integer *i;
  assert (s->card == 1);
  check (ordsee(s->tblval,1,&b));
  i = (integer *)unkey(b->k);
  return get_long_in(i, 0,n_files-1, "fd");
}

void stream_error(const char *msg, block *a) {
  const size_t atpos = strchr(msg,'@') - msg;
  char *loc_msg;  /* local copy of msg with a substitution for %@ */
  assert (atpos > 0);
  assert (msg[atpos-1] == '%');
  /* Room for msg with %@ changed to [%s,%s], plus some spare:  */
  loc_msg = (char *)arena_alloc(strlen(msg) + 9);
  strcpy (loc_msg, msg);
  switch (setl_type(a)) {
  case integer_type:
    {
      const int fd = get_long_in((integer *)a, 0,n_files-1, "fd");
      /* change %@ to fd %d */
      strcpy (&loc_msg[atpos-1], "fd %d");
      strcpy (&loc_msg[atpos+4], &msg[atpos+1]);
      runerr(loc_msg, fd);
    }
  case string_type:
    {
      const char *pat = "\"%s\"";  /* change %@ to "%s" */
      strcpy(&loc_msg[atpos-1], pat);
      strcpy(&loc_msg[atpos-1+strlen(pat)], &msg[atpos+1]);
      runerr(loc_msg, tame(&strelt((string *)a,1)));
    }
  case tuple_type:
    {
      /* tame()'s single static buf won't do:  */
      char buf1[TAME_SIZE], buf2[TAME_SIZE];
      const char *pat = "[%s,%s]";  /* change %@ to [%s,%s] */
      tuple *t = (tuple *)a;  HANDLE ht = ref(t);
      string *p = OM;  HANDLE hp = ref(p);
      string *q;
      if (t->nelt == 0 || is_string(tupelt(t,1)) || is_om(tupelt(t,1))) {
        /* socket */
        if (t->nelt == 0 || is_om(tupelt(t,1))) {
          p = new_string("OM");
        } else {
          p = (string *)tupelt(t,1);
        }
        if (t->nelt < 2) {
          q = new_string("0");  /* [] -> ["OM","0"]; [s] -> [s,"0"] */
        } else if (is_string(tupelt(t,2))) {  /* service name or num */
          q = (string *)tupelt(t,2);
        } else if (is_integer(tupelt(t,2))) {  /* INTEGER portnum */
          q = tostr(tupelt(t,2));  /* cvt to STRING */
        } else {
          unexpected (setl_type(tupelt(t,2)));
        }
      } else if (is_integer(tupelt(t,1))) {
        /* real-ms timer */
        p = tostr(tupelt(t,1));  /* cvt INTEGER to STRING */
        if (t->nelt < 2) {
          q = p;  /* [interval] -> [interval,interval] */
        } else if (is_integer(tupelt(t,2))) {
          q = tostr(tupelt(t,2));  /* cvt INTEGER to STRING */
        } else {
          unexpected (setl_type(tupelt(t,2)));
        }
      } else {
        unexpected (setl_type(tupelt(t,1)));
      }
      retire(hp);
      retire(ht);
      strcpy(&loc_msg[atpos-1], pat);
      strcpy(&loc_msg[atpos-1+strlen(pat)], &msg[atpos+1]);
      runerr(loc_msg, finite_strcpy(buf1, &strelt(p,1)),
                      finite_strcpy(buf2, &strelt(q,1)));
    }
  default:
    unexpected (setl_type(a));
  }
} /* end stream_error */

/*
 *  Update the name_to_fd mmap for a filename or (nodename, servname)
 *  newly added to the fd's 'file' record.
 */
void insert_name_to_fd(int fd) {
  update_name_to_fd(fd, set_insert);
}

/*
 *  Update the name_to_fd mmap for a filename or (nodename, servname)
 *  about to be removed from the fd's 'file' record.
 */
void delete_name_to_fd(int fd) {
  update_name_to_fd(fd, set_delete);
}

/*
 *  Helper for the two functions above.
 */
static void update_name_to_fd(int fd, void (*set_op)(set **, block *)) {
  block *x = make_name(fd);
  if (x != OM) {
    HANDLE hx = ref(x);
    integer *i = new_integer(fd);  HANDLE hi = ref(i);
    set *s = mmap_fetch (name_to_fd, x);
    set_op (&s, (block *)i);  /* insert/delete i into/from s */
    mmap_insert (&name_to_fd, x, s);  /* deletes x from mmap if #s = 0 */
    retire(hi);
    retire(hx);
  }
}

/*
 *  Yield a "key" suitable for doing a lookup in the name_to_fd mmap, if
 *  either the 'filename' or 'servname' field is non-NULL in the fd's
 *  'file' record.  If they are both NULL, the "key" is a pair of
 *  INTEGERs if the ftype is real_ms_file, otherwise OM.
 */
block *make_name(int fd) {
  file *f = find_file(fd);
  if (f->filename != NULL) {  /* 'filename' field is defined; use it */
    assert (f->servname == NULL);  /* mut. excl. case */
    assert (f->ftype != real_ms_file);  /* likewise */
    return (block *) new_string (f->filename);
  } else if (f->servname != NULL) {  /* socket */
    tuple *t = new_tuple(2);  HANDLE ht = ref(t);
    let (tupelt(t,1), f->nodename ?
                      (block *)new_string(f->nodename) : OM);
    let (tupelt(t,2), (block *)new_string(f->servname));
    retire(ht);
    return (block *)t;  /* [nodename, servname] or [OM, servname] key */
  } else if (f->ftype == real_ms_file) {  /* REAL-MS stream */
    tuple *t = new_tuple(2);  HANDLE ht = ref(t);
    let (tupelt(t,1), (block *)integer_timespec_to_ms(f->initial));
    let (tupelt(t,2), (block *)integer_timespec_to_ms(f->interval));
    retire(ht);
    return (block *)t;  /* [initial, interval] key */
  } else {
    return OM;  /* no meaningful key */
  }
}

/*
 *  Despite the mere "tie" in the name, this flushes both fd and any
 *  stream tied to fd by TIE.
 *
 *  Note that errno may be set as a side-effect of the flushing.
 */
void flush_tie(int fd) {
  file *f = find_file(fd);
  if (f->ftype != no_file) {
    file_flush(fd);  /* flush self (we might be bidirectional) */
    if (f->tie >= 0) {
      file_flush(f->tie);  /* flush partner (tied fd) */
    }
  }
} /* end flush_tie */


/*
 *  Flush all output buffers, ignoring failures.
 */
void flush_all(void) {
  int fd;
  int saved_errno = errno;
  for (fd = 0; fd < n_files; fd++) {
    file *f = find_file(fd);
    if (f->ftype != no_file && test_all(f->abilities, can_write)) {
      flush_file(f);
    }
  }
  errno = saved_errno;  /* restore errno in case flush_file() set it */
} /* end flush_all */


/*
 *  First, we call flush_all().
 *
 *  Then, for every stream open at the SETL level (i.e., having ftype
 *  other than no_file in its 'file' record), we call file_close() to
 *  release the buffer and close the OS-level fd unless the stream is
 *  marked with the can_persist ability.
 *
 *  The purpose of file_rites() is to clean up after an execute() in
 *  go().  Only std streams are supposed to be can_persist, and even
 *  they can lose that attribute by being reopened.  If they keep it,
 *  the buffers are retained:  stdin remains open and is not drained,
 *  as is appropriate for successive SETL program executions, and
 *  stdout is flushed after each, as also seems appropriate.  (In case
 *  we later decide not to flush_all() here, there is also a "backup"
 *  flush_all() at the beginning of file_fini() in init.c; we would
 *  still have to do something more delicate here in that eventuality.)
 *
 *  The reason for flushing everything in a separate first pass thru the
 *  file descriptors is that it seems less race-prone than just flushing
 *  before each individual close.  Splitting heads on a pin there, of
 *  course, though I have now made that part of the lib spec (@node close).
 *  The app should in any case assume that the order of processing fds is
 *  arbitrary in each of those two passes.
 */
void file_rites(void) {
  int fd;
  flush_all();
  for (fd=n_files; fd-->0; ) {  /* nice bogus hint of limit notation */
    file *f = find_file(fd);
    if (f->ftype != no_file) {
      if (!test_all(f->abilities, can_persist)) {
        /*
         *  The mode is close_autoreap here so that if we are closing a
         *  child stream, we don't get hung up on it but neither do we
         *  abandon it, which is approp at this level of finalization.
         */
        file_close(fd, close_autoreap);
      }
    }
  }
} /* end file_rites */


/*
 *  Make sure the fd we just got from some function like os_open() or
 *  co_fork() or pty_fork() isn't already open at the SETL level, and
 *  return a pointer to its 'file' record.
 */
file *check_not_open(int fd) {
  file *f = find_fd_file(fd);
  if (f->ftype != no_file) {
    /* Something got closed under our feet, so to speak, causing the
     * OS to come up with a fd we are managing at the SETL level:  */
    runerr("Newly created file descriptor %d was already in use!",
                                          fd);
  }
  return f;
} /* end check_not_open */


/*
 *  Implementation of FORK.
 *
 *  Apart from the fact this returns -1 on failure rather than abending,
 *  this behaves like sys_fork(NULL).
 */
pid_t do_fork(void) {
  pid_t pid;
  flush_all();  /* avoid late and duplicate SETL stream outputs */
  pid = os_fork();
  if (pid == 0) {  /* child process */
    atfork_child (NULL);  /* signal/timer housekeeping */
  }
  return pid;  /* + in parent, 0 in child, -1 on error with errno set */
}

/*
 *  SYSTEM is like POSIX system() but again with handling of SETL needs.
 *
 *  In SYSTEM and FILTER, the parent ignores SIGINT and SIGQUIT while
 *  waiting for the child process to terminate.  POSIX system() also
 *  does that.  Thus terminal-generated signals (typically ctrl-C for
 *  SIGINT and ctrl-\ for SIGQUIT) that go to the process group will
 *  be acted on by the child but not the parent.  Following the return
 *  from waitpid(), the SIGINT and SIGQUIT dispositions are restored.
 *  
 *  But unlike POSIX system(), SYSTEM and FILTER do not block SIGCHLD
 *  while waiting for the child to finish, and neither does CLOSE
 *  in CLOSE_AWAIT mode.  The SETL VM manages the SIGCHLD handler
 *  and can thus avoid waiting away the child (its pid is not on
 *  the autoreap list, after all), whereas system() lives in a much
 *  rougher environment of application-provided SIGCHLD handlers.
 *  This allows for safe "background" reaping by our sigchld_handler()
 *  during a protracted SYSTEM, FILTER, or CLOSE call.
 *
 *  WAITPID allows for both blocking and nonblocking waitpid()
 *  calls.  It does not interfere with any signals, except to block
 *  SIGCHLD while it checks the 'reapable' list for a pid matching
 *  one that just got reaped, which it removes from that list.
 *  (Either the CLOSE_AUTOREAP or the WAITPID by the app was
 *  inappropriate if that happens, but anyway the garbage can't be
 *  left around.)  See do_waitpid().
 *
 *  An error return from fork() causes a program abend.
 *
 *  If errno is set, it is by waitpid() on waiting for the child.
 */
int do_system (const char *cmd) {
  sigsave_t saver;
  pid_t pid;
  assert (cmd != NULL);
  save_and_defend (&saver);    /* ignore SIGINT and SIGQUIT */
  pid = sys_fork (&saver);     /* flush_all(), os_fork(), etc. */
  if (pid > 0) {               /* parent process */
    int raw_status;
    /* Wait for the child to terminate, and reap its exit status.
     * Since SIGCHLD is not blocked, and os_waitpid() reissues the
     * waitpid() on EINTR, this allows "background" processing of the
     * reapable list by the SIGCHLD handler while we wait for this
     * particular child to finish.  */
    pid_t rpid = os_waitpid (pid, &raw_status, 0);
    if (rpid != pid) {
      raw_status = no_status;  /* errno presumed set by os_waitpid() */
    }
    restore_actions (&saver);  /* restore SIGINT and SIGQUIT */
    return raw_status;
  } else if (pid == 0) {       /* child process */
    os_exec_sh (cmd);          /* exec sh -c {cmd} */
    /* NOTREACHED */
  } else {
    unexpected (pid);  /* sys_fork() should not return on error */
  }
} /* end do_system */

/*
 *  Implementation of FILTER.
 *
 *  Create a producer child to feed the string 'input' into the stdin
 *  of a child that executes 'cmd'.  Capture the latter child's stdout
 *  as the string that is returned by FILTER.
 *
 *  See comments above do_system() about SIGINT and SIGQUIT.
 *
 *  An error return from fork() causes a program abend.
 *
 *  FILTER can set errno in the case of a waitpid() failure on the child
 *  sh that executes 'cmd'.  It still yields whatever was collected from
 *  the child process stdout in that case, though.
 */
string *do_filter(string *cmd, string *input, int *statusp) {
  sigsave_t saver;
  pid_t prod_pid, sh_pid;
  int prod_fd, sh_fd;
  int saved_errno = errno;
  save_and_defend (&saver);  /* ignore SIGINT and SIGQUIT */
  /* We pass NULL rather than &saver in creating this "producer" child,
   * because it is logically a part of the parent that doles out the
   * 'input' string while the parent waits for the other child with
   * SIGINT and SIGQUIT ignored; so we don't want those dispositions
   * restored in this child:  */
  prod_pid = co_fork (&prod_fd, can_read, NULL);  /* make producer */
  if (prod_pid == 0) {  /* child to spew the 'input' string on stdout */
    /* This child's stdout will be redirected by the parent into the
     * stdin of the child that executes the cmd.  */
    if (input != OM) {
      file *f = find_file(fd_stdout);  /* as set up by co_fork() */
      /* Encourage a SIGPIPE, delivered if the cmd chokes off its
       * consumption of our output prematurely enough, to kill us.  */
      os_signal (SIGPIPE, SIG_DFL, NULL);
      if (put_chars(f,input)) {
        flush_file(f);
      }
    } else {
      /* This producer exits immediately, which is fine, though
       * obviously it would be possible to optimize this case and not
       * have a producer at all.  */
    }
    exit(0);
  } else if (prod_pid < 0) {  /* error from first co_fork() */
    /* This case is currently not possible, but still handled.  */
    restore_actions (&saver);  /* restore SIGINT and SIGQUIT */
    return OM;  /* errno reflects co_fork() failure */
  }
  /* Parent process continues here.  Spawn a child to execute cmd.
   * We want cmd to have the dispositions of SIGINT and SIGQUIT
   * that the parent had before the FILTER call.  We pass the
   * saved actions to co_fork() so that it can restore them first thing
   * in the child while leaving the parent "defended" from them:  */
  sh_pid = co_fork (&sh_fd, can_read, &saver);  /* make child for cmd */
  if (sh_pid == 0) {
    /* This child applies the shell to the string in cmd.
     * Its stdout is connected to the sh_fd held by the parent; we
     * redirect its stdin to the inherited readable prod_fd, in the
     * manner of std_redir().  We don't fuss with the SETL buffering,
     * as we are about to exec*().  */
    if (prod_fd != fd_stdin) {
      os_dup2 (prod_fd, fd_stdin);
      os_close (prod_fd);
    }
    os_exec_sh (&strelt(cmd,1));  /* exec sh -c {cmd} */
  } else if (sh_pid < 0) {  /* error from second co_fork() */
    /* This case is currently not possible, but still handled.
     * Since we now can't use the producer, close our fd to it, kill it,
     * and reap (and discard) its status.  Have errno reflect the
     * co_fork() failure.  */
    int co_fork_errno = errno;
    os_close (prod_fd);
    os_kill (prod_pid, SIGKILL);
    os_waitpid (prod_pid, NULL, 0);
    restore_actions (&saver);  /* restore SIGINT and SIGQUIT */
    errno = co_fork_errno;
    return OM;  /* errno reflects co_fork() failure */
  }
  /* Parent process continues here.  */
  os_close (prod_fd);  /* the child that needs this already has it */
  {
    /* Consume everything from sh_fd, which is connected to the cmd
     * child's stdout, and return that as the string r.  */
    int raw_status;
    string *r = null_string();
    /* Temporary io_buffer for reading the command output:  */
    io_buffer *b = new_io_buffer (sh_fd, 0, input_buffering);
    int c;
    pid_t rpid;
    while ( (c = get_char(b)) != EOF) {  /* get char from child stdout */
      str_tackon(&r, c);  /* add that char to the result string r */
    }
    del_io_buffer(b);  /* done with the temp io_buffer */
    os_close (sh_fd);  /* close our end of the pipe from the child */
    /* Now that the sh child has finished spewing and given us an EOF,
     * make sure the producer that feeds it is dead, in case it didn't
     * get to tell its whole story but somehow didn't die on a SIGPIPE
     * either.  */
    os_kill (prod_pid, SIGKILL);
    /* For extra luck even in a case that is either unlikely or impossible
     * depending on how the OS treats SIGKILL on a stopped process:  */
    os_kill (prod_pid, SIGCONT);
    /* Reap and discard the producer's exit status */
    os_waitpid (prod_pid, NULL, 0);
    /* Wait for the sh child to terminate, and grab its exit status */
    rpid = os_waitpid (sh_pid, &raw_status, 0);
    restore_actions (&saver);  /* restore SIGINT and SIGQUIT */
    if (rpid == sh_pid) {
      *statusp = raw_status;
      errno = saved_errno;  /* leave errno unmolested by do_filter() */
    } else {
      *statusp = no_status;  /* errno presumed set by os_waitpid() */
    }
    return r;
  }
} /* end do_filter */


/*
 *  fd := local_pump(duplex or can_read or can_write)
 *
 *  where fd is returned as -1 in the child process.
 *
 *  This creates a "local" subprocess as distinct from a subprocess in
 *  which a shell command is immediately launched.
 *
 *  The sense of can_read and can_write is from the parent's point of
 *  view.
 */
int local_pump(unsigned abilities) {
  int fd;
  pid_t pid = co_fork(&fd, abilities, NULL);
  if (pid > 0) {  /* parent process */
    file *f = new_pipe_file(fd, pipe_file, abilities, pid);
    f->local_child = true;
    return fd;
  } else if (pid == 0) {  /* child process */
    return -1;
  } else {
    unexpected (pid);  /* co_fork() should not return on error */
  }
} /* end local_pump */

/*
 *  fd := local_tty_pump(duplex)
 *
 *  where fd is returned as -1 in the child process.
 *
 *  This is like local_pump() but uses pty_fork() instead of co_fork().
 */
int local_tty_pump(unsigned abilities) {
  int fd;
  pid_t pid = pty_fork(&fd, abilities, NULL);
  if (pid > 0) {  /* parent process */
    file *f = new_pipe_file(fd, tty_pipe_file, abilities, pid);
    f->local_child = true;
    return fd;
  } else if (pid == 0) {  /* child process */
    return -1;
  } else {
    unexpected (pid);  /* pty_fork() should not return on error */
  }
} /* end local_pump */


/*
 *  For each open stream that is of a type that has an associated
 *  subprocess, send that child a TERM signal, as the stream is about
 *  to be closed prematurely and the child's termination waited for
 *  (by file_rites() via vrunerr(), in the one current example).
 */
void kill_coprocs(void) {
  int fd;
  for (fd=n_files; fd-->0; ) {  /* cute if not smart eh */
    file *f = find_file(fd);
    if (f->ftype != no_file) {
      if ((f->ftype == pipe_file ||
           f->ftype == tty_pipe_file)  &&
          f->ppid == os_getpid()) {
        os_kill(f->pid, SIGTERM);
      }
    }
  }
}


/*
 *  Ignore SIGINT and SIGQUIT in preparation for invoking an external
 *  command that is to be waited for, saving their old dispositions
 *  into *p.  See also restore_actions().
 *
 *  SYSTEM and FILTER, via do_system() and do_filter(), call this.
 *
 *  The value of errno is preserved across this call.
 */
static void save_and_defend (sigsave_t *p) {
  struct sigaction ignore;
  ignore.sa_handler = SIG_IGN;
  os_sigemptyset (&ignore.sa_mask);
  ignore.sa_flags = 0;
  /* Clear these signal masks even though they are ostensibly about to
   * be clobbered by os_sigaction(), just because the sample system()
   * implementation on the POSIX system() page (Issue 7) does so:  */
  os_sigemptyset (&p->saved_sigint.sa_mask);
  os_sigemptyset (&p->saved_sigquit.sa_mask);
  os_sigaction (SIGINT, &ignore, &p->saved_sigint);    /* ignore INT */
  os_sigaction (SIGQUIT, &ignore, &p->saved_sigquit);  /* ignore QUIT */
}

/*
 *  Restore the dispositions of SIGINT and SIGQUIT from *p as saved by
 *  save_and_defend().
 *
 *  The value of errno is preserved across this call.
 */
static void restore_actions (const sigsave_t *p) {
  os_sigaction (SIGQUIT, &p->saved_sigquit, NULL);  /* QUIT action */
  os_sigaction (SIGINT, &p->saved_sigint, NULL);    /* INT action */
}

/*
 *  Fork, and return to the parent (through 'pfd') a file descriptor
 *  for communication with the child.  If the can_write bit is set in
 *  'abilities', the fd is connected to the child's stdin.  If the
 *  can_read bit is set, the fd is connected to its stdout.  Both bits
 *  may be set at the same time.
 *
 *  If the 'saved_sigs' arg is non-NULL, the actions of SIGINT and
 *  SIGQUIT are restored from the referenced struct, as the first
 *  action within the child process, by sys_fork().
 *
 *  The yield is a pid, just as with fork().
 */
static int co_fork(int *pfd, unsigned abilities,
                   const sigsave_t *saved_sigs) {
  int p[2];                         /* 2 connected duplex sockets */
  pid_t pid;                        /* child process id or 0 */
  if (os_pipe(p) < 0) {             /* create the pipe p */
    os_error("os_pipe");            /* errno may be from socketpair() */
  }
  check_not_open(p[0]);             /* not open at SETL level */
  check_not_open(p[1]);
  pid = sys_fork(saved_sigs);       /* flush_all(), os_fork(), etc. */
  if (pid > 0) {                    /* parent process */
    os_close(p[1]);                 /* close the end the child uses */
    *pfd = p[0];                    /* talk to child through our end */
    return pid;
  } else if (pid == 0) {            /* child process */
    os_close(p[0]);                 /* close the end the parent uses */
    std_redir(p[1], abilities);     /* move our end into stdin/stdout */
    return 0;
  } else {
    unexpected (pid);  /* sys_fork() should not return on error */
  }
} /* end co_fork */

/*
 *  Like co_fork(), but uses a master-slave pseudo-tty (pty) pair for
 *  communication instead of a socketpair.  The fd for the master end
 *  is stored into the parameter *pfd in the parent.  In the child,
 *  the slave end is opened, the master end is closed, and the fd for
 *  the slave end is dup'ed into stdout and/or stdin as appropriate
 *  (and closed).  The parent can use the master fd just like a normal
 *  pump handle.  The child sees what appears to be a tty in "raw" mode
 *  as its stdin and/or stdout.
 *
 *  The point of all this is to give the child an environment that
 *  it will interpret as suitable for "line buffering" its stdout
 *  (see setvbuf(3), in particular the _IOLBF mode).  This is the
 *  stdio default when stdout is connected to a terminal (tty), and
 *  is similar to how we set the policy to line_buffering in SETL
 *  programs when isatty() indicates a tty - see file_init() in init.c
 *  and std_redir() here in sys.c.
 *
 *  Although the child environment is not directly suitable for an
 *  interactive shell, since the tty is not made the child's
 *  controlling terminal and is raw, the child process could still
 *  set up such a subshell within this environment if it wanted to
 *  do the Stevens "recording process" example after script(1) as
 *  featured on the front cover of his Unix Network Programming
 *  classic.
 *
 *  For example, this SETL program run on Linux approximates 'script'
 *  but appends to /tmp/reclog:
 *
 *   fd := fileno open ('exec setsid --ctty bash 2>&1', 'tty-pump');
 *   system('stty raw -echo');
 *   rec := fileno open('/tmp/reclog','a');
 *   tie(0,1);
 *   system('stty sane <&'+fd);
 *   pd := fileno open('stty size','pipe-from');
 *   [rows, cols] := split (getline pd);
 *   close(pd);
 *   system('stty rows '+rows+' cols '+cols+' <&'+fd);
 *   loop
 *     [ready] := select([{0,fd}]);
 *     if 0 in ready then
 *       c := getc 0;
 *       if eof then bye; end if;
 *       putc(fd,c);
 *     end if;
 *     if fd in ready then
 *       c := getc fd;
 *       if eof then bye; end if;
 *       putc(1,c);
 *       putc(rec,c);
 *       flush(rec);
 *     end if;
 *   end loop;
 *
 *   proc bye;
 *     system('stty sane echo');
 *     stop;
 *   end proc;
 *
 *  The 'tty-pump' OPEN mode uses pty_fork().
 */
static int pty_fork(int *pfd, unsigned abilities,
                    const sigsave_t *saved_sigs) {
  pid_t pid;
  char pts_name[20];
  int mfd = ptym_open(pts_name);  /* mfd := master fd */
  check_not_open(mfd);            /* not already open at SETL level */
  pid = sys_fork(saved_sigs);     /* flush_all(), os_fork(), etc. */
  if (pid > 0) {                  /* parent process */
    char c;
    /* wait until a newline comes from the child */
    if (os_read(mfd, &c, 1) != 1 || c != '\n') {
      runerr("Crash!  tty pump rendezvous failure,"
             " fd = %d, pts name = %s", mfd, pts_name);
    }
    *pfd = mfd;                   /* the fd for talking to the child */
    return pid;                   /* return child pid, like fork() */
  } else if (pid == 0) {          /* child process */
    const char c = '\n';
    int sfd = ptys_open(mfd, pts_name);  /* sfd := connected slave fd */
    check_not_open(sfd);          /* not already open at SETL level */
    os_close(mfd);                /* close the end the parent uses */
    tty_rawmode(sfd);             /* avoid tty-triggered surprises */
    std_redir(sfd, abilities);    /* move our end into stdin/stdout */
    /*
     *  Write a byte to tell the parent the echo is off, avoiding
     *  a race condition Stevens somehow doesn't mention.
     */
    os_write(fd_stdout, &c, 1);
    return 0;                     /* return 0, like fork() */
  } else {
    unexpected (pid);  /* sys_fork() should not return on error */
  }
} /* end pty_fork */


/*
 *  The reason sys_fork() (and hence co_fork() and pty_fork()) takes
 *  the signal dispositions saved by save_and_defend() is to allow
 *  them to be restored early in the child.  Doing things that way,
 *  rather than simply calling save_and_defend() in the parent _after_
 *  spawning the child, avoids the little race window where the parent
 *  is briefly undefended from those signals.
 *
 *  Apart from that, sys_fork() just extends os_fork() by flushing all
 *  SETL output streams before the fork and calling atfork_child() in
 *  the new child, passing saved_sigs.
 *
 *  sys_fork() abends the program if fork() fails, unlike do_fork().
 */
static pid_t sys_fork(const sigsave_t *saved_sigs) {
  pid_t pid;
  flush_all();  /* avoid late and duplicate SETL stream outputs */
  pid = os_fork();
  if (pid == 0) {  /* child process */
    atfork_child (saved_sigs);  /* signal/timer housekeeping */
  } else if (pid < 0) {  /* error */
    os_error("fork");
  }
  return pid;  /* positive pid in parent, 0 in child */
} /* end sys_fork */

/*
 *  Signal and timer housekeeping in a newly forked child process.
 *
 *  First, clear the ready_maps cache in order to avoid false
 *  positives from SELECT.
 *
 *  Then, if saved_sigs is non-NULL, restore the dispositions of
 *  SIGQUIT and SIGINT from it.
 *
 *  Then close all timer streams, and drain all signal streams of
 *  pending events.  Note that even if atfork_child() is called just
 *  before an exec*(), closing the timer streams can be significant if
 *  it then leaves an 'ignore' or 'default' stream open on SIGALRM.
 *
 *  Other streams are _not_ drained.  Even though this means that the
 *  same buffered input could be presented to both parent and child,
 *  any reasonable program (in my view) will have only one of those
 *  parties continuing to read from any given shared stream.  And we
 *  don't know which.
 */
static void atfork_child(const sigsave_t *saved_sigs) {
  int sig;
  if (setl_vm) setl_vm->ready_maps = NULL;
  if (saved_sigs != NULL) {
    restore_actions (saved_sigs);  /* restore SIGINT and SIGQUIT */
  }
  for (sig = 0; sig < N_SIGNAL; sig++) {
    sig_info *p = &sigfo[sig];
    if (p->readers != NULL) {  /* at least one reader of this signal */
      sigset_t old_mask;
      os_sigblock(sig, &old_mask);  /* critsect-enter (block sig) */
      if (sig != SIGALRM) {
        file *f;
        /*
         *  Zero the counts of pending (unread) signal events.
         */
        for (f = p->readers; f != NULL; f = f->next) {
          f->evcount = 0;
        }
      } else {  /* sig == SIGALRM, therefore timer semantics */
        /*
         *  Close all readers of this timer, of which there is at least
         *  one by the outer "if" above.
         */
        while (p->readers != NULL) {
          /* This loop is slightly tricky, as it relies on file_close()
           * to remove f from the list of readers (f->sig leads back to
           * the sigfo array entry).  */
          file *f = p->readers;  /* head of list of readers of sig */
          assert (f->ftype == real_ms_file);
          assert (f->sig == SIGALRM);
          /*
           *  Do a full file_close() here, including updating of the
           *  name_to_fd map and the ready_maps cache.
           *
           *  The close_atfork tells file_close() not to attempt a
           *  timer_delete() on what the system probably sees as an
           *  invalid timer id in this new child context.
           */
          file_close(f->fd, close_atfork);
        }
      }
      os_sigsetmask(&old_mask);  /* critsect-exit (restore mask) */
    }
  }
} /* end atfork_child */

/*
 *  Adjust file descriptor 0 (fd_stdin) and/or 1 (fd_stdout) in what
 *  is presumed to be a new child process.  The adjustment is done
 *  in accordance with the 'abilities' needed by the parent, by
 *  dup2()ing 'fd' into 0 and/or 1 as appropriate; fd will then be
 *  closed unless it was already 0 or 1 and is being retained.
 *
 *  New SETL-level buffering is arranged over 0 and/or 1.  That is
 *  redundant for callers that are about to exec*(), but harmless.
 *
 *  If neither can_read nor can_write is set in 'abilities', the only
 *  effect of this function is to close 'fd'.
 *
 *  We add can_persist to the abilities of the stdin and/or stdout
 *  buffer(s), in conformity with what file_init() in init.c does.
 */
static void std_redir(int fd, unsigned abilities) {

  bool close_fd = true;

  if (test_all(abilities, can_write)) {
    file *f = find_file(fd_stdin);  /* parent writes to our stdin */
    if (f->ftype != no_file) {
      /*
       *  For the sake of making std_redir() more general than it really
       *  needs to be, we do a file_close() on stdin rather than a mere
       *  del_file().  In a new child process (the intended use case),
       *  that will still just remove the SETL-level buffering.
       *
       *  The can_persist bit is used to make sure the OS-level fd_stdin
       *  is not closed by file_close(), as we want to let os_dup2() do
       *  the closing, which it does atomically with making fd_stdin (0)
       *  an alias of fd on Linux.  Note that this is a separate use case
       *  from the inclusion of can_persist in the abilities of the new
       *  fd_stdin buffer further below.
       *
       *  We really don't expect fd_stdin to be already connected to a
       *  child process of ours (how could that have happened already?),
       *  but in the unlikely event that it is, we use close_zombie on
       *  the fd_close() call so that we don't hang and the child's exit
       *  status is still recoverable.
       */
      f->abilities |= can_persist;  /* inhibit close() of fd_stdin */
      file_close(fd_stdin, close_zombie);  /* remove any old buffering */
    }
    if (fd != fd_stdin) {
      os_dup2(fd, fd_stdin);  /* as in shell <&fd */
    } else {
      close_fd = false;  /* fd is already fd_stdin */
    }
    /* Provide SETL-level buffering over fd_stdin */
    new_file (fd_stdin, stream_file, can_read | can_persist,
     input_buffering);
  }

  if (test_all(abilities, can_read)) {
    file *f = find_file(fd_stdout);  /* parent reads from our stdout */
    /*
     *  Comments similar to the fd_stdin case abvoe apply here.
     */
    if (f->ftype != no_file) {
      f->abilities |= can_persist;  /* inhibit close() of fd_stdout */
      file_close(fd_stdout, close_zombie);  /* remove any old buffering */
    }
    if (fd != fd_stdout) {
      os_dup2(fd, fd_stdout);  /* as in shell >&fd */
    } else {
      close_fd = false;  /* fd is already fd_stdout */
    }
    /* Provide SETL-level buffering over fd_stdout */
    new_file (fd_stdout, stream_file, can_write | can_persist,
     os_isatty(fd_stdout) ? line_buffering : full_buffering);
  }

  if (close_fd) {
    os_close(fd);
  }

} /* end std_redir */
