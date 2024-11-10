/*  ===  SETL run-time "library" routines  =========================  */

/*  $Id: lib.c,v 1.270 2024/11/10 04:03:13 setlorg Exp $  */

/*  Free software (c) dB - see file COPYING for license (GPL).  */

/*
 *  Essentially all of the SETL built-in procedures and operators
 *  should be represented here, so for example if you are looking
 *  for the definition of OPEN, go to 'l_open' in this file.
 *
 *  The SETLish code in the comments preceding some of the definitions
 *  is originally from Dewar (1979).  Some drift is possible both in
 *  those comments and in the actual semantics implemented; I try to
 *  point out any discrepancies or unusual semantics, and also any
 *  differences from Schwartz et al. (SDDS 1986) and from SETL2
 *  (Snyder, 1990, 1992).  However, the on-line documentation at
 *
 *    setl.org
 *
 *  is in general more up to date (and it's written in English, not
 *  SETLish).
 *
 *  One notable difference between the SETLish code in the comments and
 *  the semantics actually supported is that "mixed mode" arithmetic is
 *  permitted pervasively in GNU SETL, even for relational operators.
 *  This is done mainly by allowing an INTEGER to be supplied wherever
 *  a REAL would otherwise be required.  In those contexts, the INTEGER
 *  operand is first promoted to a REAL if possible, but note that
 *  because REAL is implemented with the C type 'double', big INTEGERs
 *  will only be converted approximately, and really big INTEGERs
 *  (those that won't even fit in a REAL) will be converted to
 *  floating-point Inf or -Inf.
 */

#include "setlrun.h"

/* "Basic" stream ids are those for which TUPLE designators (which are
 * specific to sockets, and perhaps someday also to timer streams) are
 * disallowed.  Actually, we are not enforcing that extra restriction
 * here, but in places like getfd() in sys.c.  */
#define BASIC_STREAM_ID  "INTEGER (fd) or STRING"
#define STREAM_ID  "INTEGER (fd) or STRING or TUPLE"

#define is_basic_stream_id(a)  (is_integer(a) || is_string(a))
#define is_stream_id(a)  (is_integer(a) || is_string(a) || is_tuple(a))

static void check_optargs_type (tuple *a, const char *what);
static bool is_local_subprocess (pid_t pid);
static bool is_allowed (allowed *list, const char *what);
static tuple *get_lines (file *f, long n);
static tuple *get_values (file *f, long n, const char *what, bool eat_line);
static string *get_one_char (file *f);
static string *get_chars (file *f, long n);
static string *peek_one_char (file *f);
static bool put_lines (file *f, tuple *t, long n, const char *what);
static bool put_values (file *f, tuple *t, long n);
static bool print_values (file *f, tuple *t, long n);
static void unget_chars (file *f, string *s);

#define unary_op_error(name, a, needed) \
      f_unary_op_error(name, (block *)(a), needed)
#define binary_op_error(name, a, b, needed) \
      f_binary_op_error(name, (block *)(a), (block *)(b), needed)
#define unary_proc_error(name, a, needed) \
      f_unary_proc_error(name, (block *)(a), needed)
#define binary_proc_error(name, a, b, needed) \
      f_binary_proc_error(name, (block *)(a), (block *)(b), needed)
#define ternary_proc_error(name, a, b, c, needed) \
      f_ternary_proc_error(name, (block *)(a), (block *)(b), \
                                                (block *)(c), needed)
#define unary_variadic_error(name, a, needed) \
      f_unary_variadic_error(name, (block *)(a), needed)

static void f_unary_op_error (const char *name, block *a,
                               const char *needed) NO_RETURN;
static void f_unary_proc_error (const char *name, block *a,
                                 const char *needed) NO_RETURN;
static void f_binary_op_error (const char *name, block *a, block *b,
                                const char *needed) NO_RETURN;
static void f_binary_proc_error (const char *name, block *a, block *b,
                                  const char *needed) NO_RETURN;
static void f_ternary_proc_error (const char *name, block *a, block *b,
                                   block *c, const char *needed) NO_RETURN;
static void f_unary_variadic_error (const char *name, block *a,
                                     const char *needed) NO_RETURN;

/*
 *  OP #(a);
 *    CASE OF
 *    (IS_STRING a):                $ length of string
 *      x := 0;
 *      (FOR y IN a) x +:= 1; END;
 *      RETURN x;
 *    (IS_SET a):                   $ cardinality of set
 *      x := 0;
 *      (FOR y IN a) x +:= 1; END;
 *      RETURN x;
 *    (IS_TUPLE a):                 $ length of tuple
 *      x := 0;
 *      (FOR y IN a) x +:= 1; END;
 *      RETURN x;
 *    ELSE ERROR;
 *    END CASE;
 *  END OP #;
 */
integer *l_card(block *a) {
  switch (setl_type(a)) {
  case set_type:
    return new_integer(((set *)a)->card);
  case string_type:
    return new_integer(((string *)a)->nchar);
  case tuple_type:
    return new_integer(((tuple *)a)->nelt);
  default:
    unary_op_error ("#", a, "SET, STRING, or TUPLE");
  }
} /* end l_card */

/*
 *  Note that all the standard binary arithmetic operators such as
 *  +, *, -, / are extended from the original SETL specification,
 *  to allow mixed-mode arithmetic, where the INTEGER operand is
 *  "promoted" to REAL and the result is then of course also REAL.
 */

/*
 *  OP *(a,b);
 *    CASE OF
 *    (IS_INTEGER a AND IS_INTEGER b):     $ integer multiplication
 *      RETURN "integer product a * b";
 *    (IS_REAL a AND IS_REAL b):           $ real multiplication
 *      RETURN "real product a * b";
 *    (IS_SET a AND IS_SET b):             $ set intersection
 *      RETURN {x : x IN a | x IN b};
 *    (IS_STRING a AND IS_INTEGER b):      $ string duplication
 *      IF b < 0 THEN ERROR; END IF;
 *      RETURN '' +/ [a : i IN {1..b}];
 *    (IS_INTEGER a AND IS_STRING b):      $ string duplication
 *      RETURN b * a;
 *    (IS_TUPLE a AND IS_INTEGER b):       $ tuple duplication
 *      IF b < 0 THEN ERROR; END IF;
 *      x := [];
 *      (FOR i IN [1..b])
 *        j := #x;
 *        (FOR k IN [1..#a])
 *          x(j+k) := a(k);
 *        END FOR k;
 *      END FOR i;
 *      RETURN x;
 *    (IS_INTEGER a AND IS_TUPLE b):       $ tuple duplication
 *      RETURN b * a;
 *    ELSE ERROR;
 *    END CASE;
 *  END OP *;
 */
block *l_star(block *a, block *b) {
  if        (is_integer(a) &&
             is_integer(b)) {
    return (block *)integer_mul ((integer *)a,
                                 (integer *)b);
  } else if (is_real(a) &&
             is_real(b)) {
    return (block *)new_real (real_to_double((real *)a) *
                              real_to_double((real *)b));
  } else if (is_integer(a) &&
             is_real(b)) {
    return (block *)new_real(integer_to_double((integer *)a) *
                                real_to_double((real    *)b));
  } else if (is_real(a) &&
             is_integer(b)) {
    return (block *)new_real(real_to_double((real    *)a) *
                          integer_to_double((integer *)b));
  } else if (is_set(a) &&
             is_set(b)) {
    set *sa = (set *)a;
    set *sb = (set *)b;
    set *r = sa->card <= sb->card ? set_intersection(sa,sb)
                                  : set_intersection(sb,sa);
    return (block *)r;
  } else if (is_string(a) &&
             is_integer(b)) {
    string *sa = (string *)a;   HANDLE ha = ref(sa);
    integer *ib = (integer *)b; HANDLE hb = ref(ib);
    string *r = OM;             HANDLE hr = ref(r);
    long i,m,n;
    m = get_nat_long(ib, "replication factor");
    n = sa->nchar;
    r = new_estring(m*n);
    for (i=0; i<m; i++) mvmem (&strelt(r,i*n+1), &strelt(sa,1), n);
    retire(hr);
    retire(hb);
    retire(ha);
    return (block *)r;
  } else if (is_integer(a) &&
             is_string(b)) {
    integer *ia = (integer *)a; HANDLE ha = ref(ia);
    string *sb = (string *)b;   HANDLE hb = ref(sb);
    string *r = OM;             HANDLE hr = ref(r);
    long i,m,n;
    m = get_nat_long(ia, "replication factor");
    n = sb->nchar;
    r = new_estring(m*n);
    for (i=0; i<m; i++) mvmem (&strelt(r,i*n+1), &strelt(sb,1), n);
    retire(hr);
    retire(hb);
    retire(ha);
    return (block *)r;
  } else if (is_tuple(a) &&
             is_integer(b)) {
    tuple *ta = (tuple *)a;     HANDLE ha = ref(ta);
    integer *ib = (integer *)b; HANDLE hb = ref(ib);
    tuple *r = OM;              HANDLE hr = ref(r);
    long i,j,k,m,n;
    m = get_nat_long(ib, "replication factor");
    n = ta->nelt;
    r = new_tuple(m*n);
    k = 0;
    for (i=1; i<=m; i++) {
      for (j=1; j<=n; j++) {
        ++k;
        let (tupelt(r,k), copy_value(tupelt(ta,j)));
      }
    }
    retire(hr);
    retire(hb);
    retire(ha);
    return (block *)r;
  } else if (is_integer(a) &&
             is_tuple(b)) {
    integer *ia = (integer *)a; HANDLE ha = ref(ia);
    tuple *tb = (tuple *)b;     HANDLE hb = ref(tb);
    tuple *r = OM;              HANDLE hr = ref(r);
    long i,j,k,m,n;
    m = get_nat_long(ia, "replication factor");
    n = tb->nelt;
    r = new_tuple(m*n);
    k = 0;
    for (i=1; i<=m; i++) {
      for (j=1; j<=n; j++) {
        ++k;
        let (tupelt(r,k), copy_value(tupelt(tb,j)));
      }
    }
    retire(hr);
    retire(hb);
    retire(ha);
    return (block *)r;
  }
  binary_op_error ("*", a, b, NULL);
} /* end l_star */

/*
 *  OP **(a,b);
 *    CASE OF
 *    (IS_INTEGER a AND IS_INTEGER b):     $ integer exponentiation
 *      IF b < 0 THEN ERROR; END IF;
 *      RETURN 1 * / [a : i IN {1..b}];
 *    (IS_REAL a AND IS_INTEGER b):        $ real to power of integer
 *      IF b < 0 THEN ERROR; END IF;
 *      RETURN 1.0 * / [a : i IN {1..b}];
 *    ELSE ERROR;
 *    END CASE;
 *  END OP **;
 *
 *  Extension:  raising to a REAL and/or negative power is also allowed.
 */
block *l_power(block *a, block *b) {
  if        (is_integer(a) &&
             is_integer(b)) {
    integer *i = (integer *)a;
    integer *j = (integer *)b;
    if (j->size < 0) {  /* b < 0 */
      return (block *)new_real(pow(integer_to_double(i),
                                   integer_to_double(j)));
    } else if (j->size == 0) {  /* a**0 is 1 for any a */
      return (block *)new_integer(1);
    } else if (i->size == 0) {  /* 0**b is 0 for any b > 0 */
      return (block *)new_integer(0);
    } else {
      ro_mpz(za, i)  /* const mpz_t za = alias of INTEGER a */
      if (mpz_cmp_si (za, 1) == 0) {  /* 1**b is 1 for any b */
        return (block *)new_integer(1);
      } else if (mpz_cmp_si (za, -1) == 0) {  /* (-1)**b */
        ro_mpz(zb, j)  /* const mpz_t zb = alias of INTEGER b */
        if (mpz_even_p(zb)) {
          return (block *)new_integer(1);
        } else {  /* b is odd */
          return (block *)new_integer(-1);
        }
      } else {
        /* Here we have b > 0, and the easy special cases a = -1, 0, 1
         * have already been taken care of.  The following restriction
         * on the size of b is thus of no practical importance, as
         * raising any other integer to a larger b would not fit on a
         * conceivable platform where ulong was that narrow anyway.
         * You could argue for a more generic "result would be too big"
         * diagnostic, however.  */
        ulong e = get_ulong(j, "second operand of \"**\"");
        return (block *)integer_pow(i, e);
      }
    }
  } else if (is_real(a) &&
             is_integer(b)) {
    return (block *)new_real(pow(real_to_double((real    *)a),
                              integer_to_double((integer *)b)));
  } else if (is_real(a) &&
             is_real(b)) {
    return (block *)new_real(pow(real_to_double((real *)a),
                                 real_to_double((real *)b)));
  } else if (is_integer(a) &&
             is_real(b)) {
    return (block *)new_real(pow(integer_to_double((integer *)a),
                                    real_to_double((real    *)b)));
  }
  binary_op_error ("**", a, b, "numeric operands");
} /* end l_power */

/*
 *  OP +(a);
 *    CASE OF
 *    (IS_INTEGER a):                      $ integer affirmation
 *      RETURN a;
 *    (IS_REAL a):                         $ real affirmation
 *      RETURN a;
 *    ELSE ERROR;
 *    END CASE;
 *  END OP +;
 */
block *l_uplus(block *a) {
  switch (setl_type(a)) {
  case integer_type:
    return (block *)copy_integer((integer *)a);
  case real_type:
    return (block *)copy_real((real *)a);
  default:
    unary_op_error ("+", a, "numeric");
  }
} /* end l_uplus */

/*
 *  OP +(a,b);
 *    CASE OF
 *    (IS_INTEGER a AND IS_INTEGER b):     $ integer addition
 *      RETURN a - -b;
 *    (IS_REAL a AND IS_REAL b):           $ real addition
 *      RETURN a - -b;
 *    (IS_SET a AND IS_SET b):             $ set union
 *      RETURN (a WITH/ b);
 *    (IS_STRING a AND IS_STRING b):       $ string concatenation
 *      RETURN "concatenation of strings a and b";
 *    (IS_TUPLE a AND IS_TUPLE b):         $ tuple concatenation
 *      x := a;
 *      (FOR i IN [1..#b])
 *        x(#a+i) := b(i);
 *      END FOR i;
 *      RETURN x;
 *    ELSE ERROR;
 *    END CASE;
 *  END OP +;
 *
 *  Extension:  if exactly one operand is a STRING, the other will be
 *  converted as if by STR and concatenated with it.
 */
block *l_bplus(block *a, block *b) {
  if (is_string(a) &&
      !is_string(b)) {
    return (block *)string_plus_block((string *)a, b);
  } else if (is_string(b) &&
             !is_string(a)) {
    return (block *)block_plus_string(a, (string *)b);
  } else if (is_integer(a) &&
             is_integer(b)) {
    return (block *)integer_add((integer *)a,
                                (integer *)b);
  } else if (is_real(a) &&
             is_real(b)) {
    return (block *)new_real(real_to_double((real *)a) +
                             real_to_double((real *)b));
  } else if (is_integer(a) &&
             is_real(b)) {
    return (block *)new_real(integer_to_double((integer *)a) +
                                real_to_double((real    *)b));
  } else if (is_real(a) &&
             is_integer(b)) {
    return (block *)new_real(real_to_double((real    *)a) +
                            integer_to_double((integer *)b));
  } else if (is_set(a) &&
             is_set(b)) {
    return (block *)set_union((set *)a,
                              (set *)b);
  } else if (is_string(a) &&
             is_string(b)) {
    return (block *)str_join((string *)a,
                             (string *)b);
  } else if (is_tuple(a) &&
             is_tuple(b)) {
    return (block *)tup_join((tuple *)a,
                             (tuple *)b);
  }
  binary_op_error ("+", a, b, NULL);
} /* end l_bplus */

/*
 *  Extension:  when the left operand of "+:=" is OM, it is treated
 *  as the identity element for the type on the right if there is one,
 *  i.e., 0 for INTEGER, 0.0 for REAL, '' for STRING, [] for TUPLE,
 *  and {} for SET.  If I recall correctly, this extension was first
 *  suggested by Bob Paige.
 *
 *  Note that this makes t +:= v valid in cases where t + v
 *  would be an error, when t is OM.  And s +:= 'x' puts 'x' in s
 *  while s + 'x' is '*x', when s is OM.
 *  
 */
void l_aplus(block **a, block *b) {
  block *t = *a;
  if (is_om(t)) {
    switch (setl_type(b)) {
    case integer_type:
      let (*a, (block *)copy_integer((integer *)b));
      return;
    case real_type:
      let (*a, (block *)copy_real((real *)b));
      return;
    case set_type:
      let (*a, (block *)copy_set((set *)b));
      return;
    case string_type:
      let (*a, (block *)copy_string((string *)b));
      return;
    case tuple_type:
      let (*a, (block *)copy_tuple((tuple *)b));
      return;
    default:
      runerr("OM on left of +:=, but %s on right lacks identity element",
                             TYPENAME(b));
    }
  }
  if (is_string(t)) {
    if (!is_string(b)) {
      HANDLE ht = ref(t);
      string *s = tostr(b);
      retire(ht);
      str_concat((string **)(void *)&t,s);
    } else {
      string *s = (string *)b;
      str_concat((string **)(void *)&t,s);
    }
  } else if (is_integer(t) &&
             is_integer(b)) {
    integer_inc((integer **)(void *)&t,
                (integer *)b);
  } else if (is_real(t) &&
             is_real(b)) {
    ((real *)t)->realval += ((real *)b)->realval;
  } else if (is_real(t) &&
             is_integer(b)) {
    ((real *)t)->realval += integer_to_double((integer *)b);
  } else if (is_integer(t) &&
             is_real(b)) {
    t = (block *)new_real(integer_to_double((integer *)t) +
                             real_to_double((real    *)b));
  } else if (is_set(t) &&
             is_set(b)) {
    set_extend((set **)(void *)&t,
               (set  *)b);
  } else if (is_tuple(t) &&
             is_tuple(b)) {
    tup_concat((tuple **)(void *)&t,
               (tuple  *)b);
  } else {
    binary_op_error ("+:=", t, b, NULL);  /* incompatible operands */
  }
  *a = t;
} /* end l_aplus */

/*
 *  OP -(a);
 *    CASE OF
 *    (IS_INTEGER a):                      $ integer negation
 *      RETURN 0 - a;
 *    (IS_REAL a):                         $ real negation
 *      RETURN 0.0 - a;
 *    ELSE ERROR;
 *    END CASE;
 *  END OP -;
 */
block *l_uminus(block *a) {
  switch (setl_type(a)) {
  case integer_type:
    return (block *)integer_neg((integer *)a);
  case real_type:
    return (block *)new_real(-real_to_double((real *)a));
  default:
    unary_op_error ("-", a, "numeric");
  }
} /* end l_uminus */

/*
 *  OP -(a,b);
 *    CASE OF
 *    (IS_INTEGER a AND IS_INTEGER b):     $ integer subtraction
 *      RETURN "integer difference a - b";
 *    (IS_REAL a AND IS_REAL b):           $ real subtraction
 *      RETURN "real difference a - b";
 *    (IS_SET a AND IS_SET b):             $ set difference
 *      RETURN (a LESS/ b);
 *    ELSE ERROR;
 *    END CASE;
 *  END OP -;
 */
block *l_bminus(block *a, block *b) {
  if        (is_integer(a) &&
             is_integer(b)) {
    return (block *)integer_sub((integer *)a,
                                (integer *)b);
  } else if (is_real(a) &&
             is_real(b)) {
    return (block *)new_real(real_to_double((real *)a) -
                             real_to_double((real *)b));
  } else if (is_integer(a) &&
             is_real(b)) {
    return (block *)new_real(integer_to_double((integer *)a) -
                                real_to_double((real    *)b));
  } else if (is_real(a) &&
             is_integer(b)) {
    return (block *)new_real(real_to_double((real    *)a) -
                          integer_to_double((integer *)b));
  } else if (is_set(a) &&
             is_set(b)) {
    return (block *)set_difference((set *)a,
                                   (set *)b);
  }
  binary_op_error ("-", a, b, NULL);
} /* end l_bminus */

void l_aminus(block **a, block *b) {
  block *t = *a;
  if        (is_integer(t) &&
             is_integer(b)) {
    integer_dec((integer **)(void *)&t,
                (integer *)b);
  } else if (is_real(t) &&
             is_real(b)) {
    ((real *)t)->realval -= ((real *)b)->realval;
  } else if (is_real(t) &&
             is_integer(b)) {
    ((real *)t)->realval -= integer_to_double((integer *)b);
  } else if (is_integer(t) &&
             is_real(b)) {
    t = (block *)new_real(integer_to_double((integer *)t) -
                             real_to_double((real    *)b));
  } else if (is_set(t) &&
             is_set(b)) {
    set_reduce((set **)(void *)&t,
               (set  *)b);
  } else {
    binary_op_error ("-:=", t, b, NULL);
  }
  *a = t;
} /* end l_aminus */

/*
 *  OP /(a,b);
 *    CASE OF
 *    (IS_INTEGER a AND IS_INTEGER b):     $ integer division, real quotient
 *      RETURN FLOAT a / FLOAT b;
 *    (IS_REAL a AND IS_REAL b):           $ real division
 *      RETURN "real quotient a/b";
 *    ELSE ERROR;
 *    END CASE;
 *  END OP /;
 *
 *  Extension:  the yield type of INTEGER / INTEGER depends on the
 *  setting of INTSLASH, which is FALSE by default, meaning yield
 *  REAL as above.
 *
 *  (Of course the mixed modes are also allowed for "/", and produce
 *  REAL results, as with all arithmetic binary operators.)
 */
block *l_slash(block *a, block *b) {
  if        (is_integer(a) &&
             is_integer(b)) {
    if (!get_intslash()) {
      return (block *)new_real(integer_to_double((integer *)a) /
                               integer_to_double((integer *)b));
    } else {
      if (((integer *)b)->size == 0) {
        runerr("INTEGER division by 0");
      }
      return (block *)integer_div((integer *)a,
                                  (integer *)b);
    }
  } else if (is_real(a) &&
             is_real(b)) {
    return (block *)new_real(real_to_double((real *)a) /
                             real_to_double((real *)b));
  } else if (is_integer(a) &&
             is_real(b)) {
    return (block *)new_real(integer_to_double((integer *)a) /
                                real_to_double((real    *)b));
  } else if (is_real(a) &&
             is_integer(b)) {
    return (block *)new_real(real_to_double((real    *)a) /
                          integer_to_double((integer *)b));
  }
  binary_op_error ("/", a, b, "numeric operands");
} /* end l_slash */


/*
 *  OP /=(a,b);
 *    RETURN NOT (a = b);
 *  END OP /=;
 *
 *  Extension:  If a and b are equal in value but one is REAL and the
 *  other is INTEGER, they compare equal.
 */
boolean *l_ne(block *a, block *b) {
  return new_boolean (!equal_value(a,b));
} /* end l_ne */

/*
 *  OP <(a,b);
 *    CASE OF
 *    (IS_INTEGER a AND IS_INTEGER b):     $ integer less than
 *      RETURN "integer a less than integer b";
 *    (IS_REAL a AND IS_REAL b):           $ real less than
 *      RETURN "real a less than real b";
 *    (IS_STRING a AND IS_STRING b):       $ string less than
 *      IF a = '' THEN
 *        RETURN b /= '';
 *      ELSEIF b = '' THEN
 *        RETURN FALSE;
 *      ELSE
 *        CASE OF
 *        (a(1) = b(1)):
 *          RETURN a(2..) < b(2..);
 *        ("a(1) precedes b(1) in this character set"):
 *          RETURN TRUE;
 *        ELSE
 *          RETURN FALSE;
 *        END CASE;
 *      END IF;
 *    ELSE ERROR;
 *    END CASE;
 *  END OP <;
 *
 *  Extension:  now also handles mixed-mode arithmetic and tuples.
 *  Comparison of any number with NaN yields FALSE.
 */
boolean *l_lt(block *a, block *b) {
  int c = compare_values(a,b,"<");
  switch (c) {
  case A_LT_B:
    return new_boolean(true);
  case A_EQ_B:
  case A_GT_B:
  case A_IS_NAN:
  case B_IS_NAN:
    return new_boolean(false);
  default:
    unexpected(c);
  }
} /* end l_lt */

/*
 *  OP <=(a,b);
 *    CASE OF
 *    (IS_INTEGER a AND IS_INTEGER b):     $ integer less than or equal
 *      RETURN a<b OR a=b;
 *    (IS_REAL a AND IS_REAL b):           $ real less than or equal
 *      RETURN a<b OR a=b;
 *    (IS_STRING a AND IS_STRING b):       $ string less than or equal
 *      RETURN a<b OR a=b;
 *    ELSE ERROR;
 *    END CASE;
 *  END OP <=;
 *
 *  Extension:  now also handles mixed-mode arithmetic and tuples.
 *  Comparison of any number with NaN yields FALSE.
 */
boolean *l_le(block *a, block *b) {
  int c = compare_values(a,b,"<=");
  switch (c) {
  case A_LT_B:
  case A_EQ_B:
    return new_boolean(true);
  case A_GT_B:
  case A_IS_NAN:
  case B_IS_NAN:
    return new_boolean(false);
  default:
    unexpected(c);
  }
} /* end l_le */

/*
 *  OP =(a,b);          $ equality test
 *    RETURN "a and b are identical in type and value";
 *  END OP =;
 *
 *  Extension:  If a and b are equal in value but one is REAL and the
 *  other is INTEGER, they compare equal.
 */
boolean *l_eq(block *a, block *b) {
  return new_boolean (equal_value(a,b));
} /* end l_eq */

/*
 *  OP >(a,b);
 *    CASE OF
 *    (IS_INTEGER a AND IS_INTEGER b):     $ integer greater than
 *      RETURN b < a;
 *    (IS_REAL a AND IS_REAL b):           $ real greater than
 *      RETURN b < a;
 *    (IS_STRING a AND IS_STRING b):       $ string greater than
 *      RETURN b < a;
 *    ELSE ERROR;
 *    END CASE;
 *  END OP >;
 *
 *  Extension:  now also handles mixed-mode arithmetic and tuples.
 *  Comparison of any number with NaN yields FALSE.
 */
boolean *l_gt(block *a, block *b) {
  int c = compare_values(a,b,">");
  switch (c) {
  case A_GT_B:
    return new_boolean(true);
  case A_EQ_B:
  case A_LT_B:
  case A_IS_NAN:
  case B_IS_NAN:
    return new_boolean(false);
  default:
    unexpected(c);
  }
} /* end l_gt */

/*
 *  OP >=(a,b);
 *    CASE OF
 *    (IS_INTEGER a AND IS_INTEGER b):     $ integer greater or equals
 *      RETURN a>b OR a=b;
 *    (IS_REAL a AND IS_REAL b):           $ real greater or equals
 *      RETURN a>b OR a=b;
 *    (IS_STRING a AND IS_STRING b):       $ string greater or equals
 *      RETURN a>b OR a=b;
 *    ELSE ERROR;
 *    END CASE;
 *  END OP >=;
 *
 *  Extension:  now also handles mixed-mode arithmetic and tuples.
 *  Comparison of any number with NaN yields FALSE.
 */
boolean *l_ge(block *a, block *b) {
  int c = compare_values(a,b,">=");
  switch (c) {
  case A_GT_B:
  case A_EQ_B:
    return new_boolean(true);
  case A_LT_B:
  case A_IS_NAN:
  case B_IS_NAN:
    return new_boolean(false);
  default:
    unexpected(c);
  }
} /* end l_ge */

/*
 *  OP ?(a,b);                   $ undefined interrogation
 *    CASE OF
 *    (a /= OM):
 *      RETURN a;
 *    (a = OM):
 *      RETURN b;
 *    END CASE;
 *  END OP ?;
 *
 *  Extension:  now short-circuited, like AND and OR.
 *
 *  The GNU SETL translator does not emit code that calls this
 *  (because of the short-circuiting), but here it is anyway in
 *  case something else wants the non-short-circuiting version.
 */
block *l_query(block *a, block *b) {
  return copy_value (a ? a : b);
} /* end l_query */


/*
 *  OP ABS(a);
 *    CASE OF
 *    (IS_INTEGER a):                      $ absolute value of integer
 *      CASE OF
 *      (a < 0):
 *        RETURN -a;
 *      (a >= 0):
 *        RETURN a;
 *      END CASE;
 *    (IS_REAL a):                         $ absolute value of real
 *      CASE OF
 *      (a < 0.0):
 *        RETURN -a;
 *      (a >= 0.0):
 *        RETURN a;
 *      END CASE;
 *    (IS_STRING a):                       $ absolute value of string
 *      IF #a /= 1 THEN
 *        ERROR;
 *      ELSE
 *        RETURN "integer code for 1-character string a";
 *      END IF;
 *    ELSE ERROR;
 *    END CASE;
 *  END OP ABS;
 *
 *  Extension:  ABS t for TUPLE of numbers t is sqrt (t dot t).
 *
 *  ICHAR is equivalent to ABS when applied to a STRING.
 */
block *l_abs(block *a) {
  switch (setl_type(a)) {
  case integer_type:
    return (block *)integer_abs((integer *)a);
  case real_type:
    {
      double x = real_to_double((real *)a);
      return (block *)new_real(fabs(x));
    }
  case string_type:
    {
      string *s = (string *)a;
      long n = s->nchar;
      if (n == 1) {
        return (block *)ulong_integer((uchar)strelt(s,1));
      } else {
        runerr("STRING operand of ABS must have 1 character, not %ld"
               " (but see UNPACK_INT etc.)",                     n);
      }
    }
  case tuple_type:
    {
      tuple *t = (tuple *)a;  HANDLE ht;
      long i, n = t->nelt;
      block *sumsq;  HANDLE h_sumsq;
      block *r;
      for (i=1; i<=n; i++) {
        block *x = tupelt(t,i);
        if (!is_numeric(x)) {
          runerr("TUPLE operand of ABS must consist entirely of numbers");
        }
      }
      ht = ref(t);
      sumsq = (block *)new_integer(0);
      h_sumsq = ref(sumsq);
      for (i=1; i<=n; i++) {
        block *x = tupelt(t,i);
        block *sq = l_star(x, x);
        l_aplus(&sumsq, sq);
      }
      r = (block *)l_sqrt((real *)sumsq);
      retire(h_sumsq);
      retire(ht);
      return r;
    }
  default:
    unary_op_error ("ABS", a, "INTEGER, REAL, or STRING");
  }
} /* end l_abs */


/*
 *  ACCEPT a  -- accept connection on TCP-SERVER or UNIX-SERVER socket
 *
 *  The single argument to ACCEPT must be a server socket opened by
 *  OPEN, or a 2-tuple that could be used with OPEN in "TCP-SERVER" or
 *  "UNIX-SERVER" mode.  The result is a socket fd for the new
 *  connection, open at the SETL level.  On error, the yield is OM,
 *  and errno (whence LAST_ERROR) is set.  See accept(2).
 */
integer *l_accept(block *a) {
  int fd, gd;
  if (!is_stream_id(a)) {
    unary_proc_error ("ACCEPT", a, STREAM_ID);
  }
  fd = getfd (a, getfd_accept);
  gd = file_accept (fd);  /* may set errno */
  return gd >= 0 ? new_integer(gd) : OM;
} /* end l_accept */

/*
 *  OP ACOS(a);                      $ arc cosine
 *    CASE OF
 *    (IS_REAL a):
 *      IF a <= 1.0 AND a >= -1.0 THEN
 *        RETURN "arc cosine of a";
 *      ELSE
 *        ERROR;
 *      END IF;
 *    ELSE ERROR;
 *    END CASE;
 *  END OP ACOS;
 */
real *l_acos(real *a) {
  double x = get_double(a,"ACOS operand");
  if (x < -1.0 || x > 1.0) {
    runerr("ACOS operand (%.*g) out of range -1 to +1",
             DOUBLE_REP_DIGS, x);
  }
  return new_real(acos(x));
} /* end l_acos */

/*
 *  OP AND(a,b);                     $ logical and
 *    CASE OF
 *    (a = FALSE):
 *      $ skip evaluation of b and
 *      RETURN FALSE;
 *    (a = TRUE):
 *      $ evaluate b and do
 *      CASE OF
 *      (b = TRUE):
 *        RETURN TRUE;
 *      (b = FALSE):
 *        RETURN FALSE;
 *      ELSE ERROR;
 *      END CASE;
 *    ELSE ERROR;
 *    END CASE;
 *  END OP AND;
 *
 *  The GNU SETL translator does not emit code that calls this
 *  (because of the short-circuiting), but here it is anyway in
 *  case something else wants the non-short-circuiting version.
 */
boolean *l_and(boolean *a, boolean *b) {
  if (is_boolean(a) &&
      is_boolean(b)) {
    return new_boolean(a->booval && b->booval);
  }
  binary_op_error ("AND", a, b, "BOOLEAN operands");
} /* end l_and */

/*
 *  N.B.  The SNOBOL-inspired pattern-matching functions work as in
 *  SETL2 rather than as in SDDS 1986.  They are rather horrid without
 *  good pattern-matching syntax and the backtracking matcher.
 */

/*
 *  ANY(RW a, b)  -- if first char of a is in charset b, remove and
 *                   return it, else ''
 */
string *l_any(string **a, string *b) {
  string *s = *a;     HANDLE hs = ref(s);
  string *t;
  if (!is_string(s) ||
      !is_string(b)) {
    binary_proc_error ("ANY", s, b, "(STRING, STRING)");
  }
  if (s->nchar > 0) {
    char c = strelt(s,1);
    long j;
    for (j=1; j<=b->nchar; j++) {
      if (c == strelt(b,j)) {
        s->nbefore++;
        s->nchar--;
        t = new_cstring(c);
        goto done;
      }
    }
  }
  t = null_string();
done:
  *a = s;
  retire(hs);
  return t;
} /* end l_any */

/*
 *  OP ARB(a);
 *    CASE OF
 *    (IS_SET a):                         $ arbitrary element of set
 *      IF a = {} THEN
 *        RETURN OM;
 *      ELSE
 *        RETURN "arbitrary element from set a";
 *      END IF;
 *    ELSE ERROR;
 *    END CASE;
 *  END OP ARB;
 */
block *l_arb(set *a) {
  if (!is_set(a)) {
    unary_op_error ("ARB", a, "SET");
  }
  if (a->card == 0) return OM;
  switch (a->stype) {
  case plain_set:
    {
      subnode *c;
      check (ordsee(a->tblval, 1, &c));
      return unkey(c->k);
    }
  case mmap:
    {
      HANDLE ha = ref(a);
      subnode *c = NULL;  HANDLE hc = ref(c);
      tuple *t = new_tuple(2);  HANDLE ht = ref(t);
      check (ordsee(a->tblval, 1, &c));
      let (tupelt(t,1), unkey(c->k));
      let (tupelt(t,2), l_arb((set *)c->d));
      retire(ht);
      retire(hc);
      retire(ha);
      return (block *)t;
    }
  case smap:
    {
      HANDLE ha = ref(a);
      subnode *c = NULL;  HANDLE hc = ref(c);
      tuple *t = new_tuple(2);  HANDLE ht = ref(t);
      check (ordsee(a->tblval, 1, &c));
      let (tupelt(t,1), unkey(c->k));
      let (tupelt(t,2), copy_value(c->d));
      retire(ht);
      retire(hc);
      retire(ha);
      return (block *)t;
    }
  default:
    unexpected (a->stype);
  }
} /* end l_arb */

real *l_arg(block *a) {
  runerr("ARG not yet implemented");
} /* end l_arg */

/*
 *  OP ASIN(a);                      $ arc sine
 *    CASE OF
 *    (IS_REAL a):
 *      IF a <= 1.0 AND a >= -1.0 THEN
 *        RETURN "arc sine of a";
 *      ELSE
 *        ERROR;
 *      END IF;
 *    ELSE ERROR;
 *    END CASE;
 *  END OP ASIN;
 */
real *l_asin(real *a) {
  double x = get_double(a,"ASIN operand");
  if (x < -1.0 || x > 1.0) {
    runerr("ASIN operand (%.*g) out of range -1 to +1",
            DOUBLE_REP_DIGS, x);
  }
  return new_real(asin(x));
} /* end l_asin */

/*
 *  OP ATAN(a);                      $ arc tangent
 *    CASE OF
 *    (IS_REAL a):
 *      RETURN "arc tangent of a";
 *    ELSE ERROR;
 *    END CASE;
 *  END OP ATAN;
 */
real *l_atan(real *a) {
  double x = get_double(a,"ATAN operand");
  return new_real(atan(x));
} /* end l_atan */

/*
 *  OP ATAN2(a,b);                   $ arc tangent of quotient
 *    CASE OF
 *    (IS_REAL a AND IS_REAL b):
 *      RETURN "arc tangent of quotient a/b, using both signs to
 *              determine quadrant of result";
 *    ELSE ERROR;
 *    END CASE;
 *  END OP ATAN2;
 */
real *l_atan2(real *a, real *b) {
  if        (is_real(a) &&
             is_real(b)) {
    return new_real (atan2 (real_to_double(a),
                            real_to_double(b)));
  } else if (is_real(a) &&
             is_integer(b)) {
    return new_real (atan2 (real_to_double(a),
                         integer_to_double((integer *)b)));
  } else if (is_integer(a) &&
             is_real(b)) {
    return new_real (atan2 (integer_to_double((integer *)a),
                               real_to_double(b)));
  } else if (is_integer(a) &&
             is_integer(b)) {
    return new_real (atan2 (integer_to_double((integer *)a),
                            integer_to_double((integer *)b)));
  }
  binary_op_error ("ATAN2", a, b, "numeric operands");
} /* end l_atan2 */

/*
 *  a BIT_AND b  -- logical AND of INTEGERs treated as bit sequences
 */
integer *l_bit_and(integer *a, integer *b) {
  if (is_integer(a) &&
      is_integer(b)) {
    return integer_and(a, b);
  }
  binary_op_error ("BIT_AND", a, b, "INTEGER operands");
} /* end l_bit_and */

/*
 *  BIT_NOT a  -- logical NOT of INTEGER treated as a bit sequence;
 *                for 2's complement this is -a - 1
 */
integer *l_bit_not(integer *a) {
  if (is_integer(a)) {
    return integer_com(a);
  }
  unary_op_error ("BIT_NOT", a, "INTEGER");
} /* end l_bit_not */

/*
 *  a BIT_OR b  -- logical OR of INTEGERs treated as bit sequences
 */
integer *l_bit_or(integer *a, integer *b) {
  if (is_integer(a) &&
      is_integer(b)) {
    return integer_or(a, b);
  }
  binary_op_error ("BIT_OR", a, b, "INTEGER operands");
} /* end l_bit_or */

/*
 *  a BIT_XOR b  -- logical XOR of INTEGERs treated as bit sequences
 */
integer *l_bit_xor(integer *a, integer *b) {
  if (is_integer(a) &&
      is_integer(b)) {
    return integer_xor(a, b);
  }
  binary_op_error ("BIT_XOR", a, b, "INTEGER operands");
} /* end l_bit_xor */

/*
 *  BREAK(RW a, b)  -- break off and return initial chars in a that are
 *                     not in charset b
 */
string *l_break(string **a, string *b) {
  string *s = *a;     HANDLE hs = ref(s);
  string *t;
  long i,j;
  if (!is_string(s) ||
      !is_string(b)) {
    binary_proc_error ("BREAK", s, b, "(STRING, STRING)");
  }
  for (i=1; i<=s->nchar; i++) {
    char c = strelt(s,i);
    for (j=1; j<=b->nchar; j++) {
      if (c == strelt(b,j)) goto done;
    }
  }
done:
  i--;
  t = copy_substring(s,1,i);
  s->nbefore += i;
  s->nchar -= i;
  *a = s;
  retire(hs);
  return t;
} /* end l_break */

void l_callf(block *a, block *b, block *c) {
  runerr("CALLF not implemented, whatever it was supposed to mean");
} /* end l_callf */

/*
 *  CALLOUT(a, b, c)  -- for SETL2-style callouts
 */
string *l_callout(integer *a, block *b, tuple *c) {
  int service;
  unsigned argi,argc;
  char **argv;
  char *x;
  string *r = OM;
  service = get_int(a, "first (service code) arg to CALLOUT");
  if (!is_om(b) ||
      !is_tuple(c)) {
    ternary_proc_error ("CALLOUT", a, b, c,
     "(INTEGER, OM, TUPLE)");
  }
  argc = c->nelt;
  argv = (char **) os_malloc((argc+1) * sizeof argv[0]);
  for (argi=0; argi!=argc; argi++) {
    string *s = (string *)tupelt(c,(long)argi+1);
    if (!is_string(s)) {
      runerr("Every element of third CALLOUT arg must be STRING,"
             " not %s", TYPENAME(s));
    }
    argv[argi] = strndup_malloc (&strelt(s,1), s->nchar);
  }
  if (restricted) runerr("CALLOUT is restricted");
  x = setl2_callout(service,argc,argv);  /* user-provided dispatcher */
  if (x) r = new_string(x);
  for (argi=argc; argi--; ) {
    os_free(argv[argi]);
  }
  os_free(argv);
  return r;
} /* end l_callout */

string *l_callout2(integer *a, block *b, tuple *c) {
  if (restricted) runerr("CALLOUT2 is restricted");
  runerr("CALLOUT2 not yet implemented");
} /* end l_callout2 */

/*
 *  OP CEIL(a);
 *    CASE OF
 *    (IS_REAL a):                         $ ceiling of real
 *      CASE OF
 *      (a < 0.0):
 *        LOOP INIT x := 0; DOING x -:= 1; DO
 *          IF FLOAT x < a THEN RETURN x + 1; END IF;
 *        END LOOP;
 *      (a >= 0.0):
 *        LOOP INIT x := 0; DOING x +:= 1; DO
 *          IF FLOAT x >= a THEN RETURN x; END IF;
 *        END LOOP;
 *      END CASE;
 *    ELSE ERROR;
 *    END CASE;
 *  END OP CEIL;
 */
integer *l_ceil(real *a) {
  switch (setl_type(a)) {
  case integer_type:
    return copy_integer ((integer *)a);
  case real_type:
    return double_to_integer (ceil (real_to_double(a)));
  default:
    unary_op_error ("CEIL", a, "numeric");
  }
} /* end l_ceil */

/*
 *  OP CHAR(a);                  $ character repr. of integer
 *    CASE OF
 *    (IS_INTEGER a):
 *      IF a >= 0 AND a <= 255 THEN
 *        RETURN "1-character string having internal code a";
 *      ELSE
 *        ERROR;
 *      END IF;
 *    ELSE ERROR;
 *    END CASE;
 *  END OP CHAR;
 */
string *l_char(integer *a) {
  /* You may be tempted to lift this 0..UCHAR_MAX restriction, but
   * consider carefully what you would do with 0, and with negative
   * numbers, if you said "let the string form be just as many bytes
   * as necessary to accommodate the number"...not to mention the
   * endianness dependency.  See too PACK_... and BIT_... .  */
  int i = get_long_in(a, 0,UCHAR_MAX, "CHAR operand");
  return new_cstring(i);
} /* end l_char */

/*
 *  CHDIR(s);  $ cd s
 *  CHDIR();   $ cd $HOME
 *
 *  On failure, errno (whence LAST_ERROR) is set.
 */
void l_chdir(tuple *a, long nargs) {
  check_optargs_type(a,"CHDIR");
  if (nargs == 0) {
    char *s = os_getenv("HOME");
    if (s) {
      if (restricted) runerr("CHDIR is restricted");
      os_chdir(s);  /* chdir() may set errno */
    } else {
      runerr("CHDIR needs 1 arg or HOME environment variable");
    }
  } else if (nargs == 1) {
    string *s = a->nelt == 0 ? OM : (string *)tupelt(a,1);
    if (!is_string(s)) {
      unary_proc_error ("CHDIR", s, "STRING or no");
    }
    if (restricted) runerr("CHDIR is restricted");
    os_chdir(&strelt(s,1));  /* chdir() may set errno */
  } else {
    runerr("CHDIR takes at most 1 (directory name) arg, not %ld", nargs);
  }
} /* end l_chdir */

/*
 *  CLEAR_ERROR;  -- clear last-system-call error indicator
 *
 *  Clear errno to 0, which is effectively LAST_ERROR := NO_ERROR.
 */
void l_clear_error(void) {
  errno = 0;    /* errno is defined by <errno.h> */
} /* end l_clear_error */

/*
 *  CLOCK  -- how many ms this program has been running for
 *
 *  Elapsed (real) time in milliseconds since this process began.
 */
integer *l_clock(void) {
  return integer_timespec_to_ms(os_elapsed());
} /* end l_clock */

/*
 *  PROC CLOSE(f, mode(*));
 *    VAR how;
 *    CASE NARGS OF
 *    (1):
 *      how := CLOSE_AWAIT;
 *    (2):
 *      how := mode(1);
 *      IF how NOTIN {CLOSE_AWAIT, CLOSE_AUTOREAP, CLOSE_ZOMBIE} THEN
 *        ERROR;
 *      END IF;
 *    ELSE
 *      ERROR;
 *    END CASE;
 *    ... close the stream, a serious and delicate matter...
 *  END PROC;
 *
 *  The optional 2nd arg shown above is an extension.
 *
 *  See file_close() in sys.c for details.
 */
void l_close(block *a, tuple *b, long nargs) {
  HANDLE hb;
  int fd;
  close_how how;
  file *f;
  /* This extension silently tolerates OM as the first arg, for the
   * ostensible huge convenience of being able to CLOSE(lookup(x))
   * without having to check first whether lookup(x) is OM:  */
  if (is_om(a)) return;
  hb = ref(b);
  if (!is_stream_id(a)) {
    unary_variadic_error ("CLOSE", a, STREAM_ID);
  }
  check_optargs_type(b,"CLOSE");
  if (nargs == 1) {
    how = close_await;
  } else if (nargs == 2) {
    integer *p = b->nelt == 0 ? OM : (integer *)tupelt(b,1);
    long mode = get_long(p, "second arg to CLOSE");
    switch (mode) {
    case close_await:     how = close_await;    break;
    case close_autoreap:  how = close_autoreap; break;
    case close_zombie:    how = close_zombie;   break;
    default:
      runerr("Second arg to CLOSE must be "
              "CLOSE_AWAIT, CLOSE_AUTOREAP, or CLOSE_ZOMBIE, not %ld",
                                                                 mode);
    }
  } else {
    runerr("CLOSE takes 1 or 2 args, not %ld", nargs);
  }
  fd = getfd (a, getfd_close);
  f = find_file(fd);
  if (restricted) {
    /*
     *  A restricted user can still close a fd that is given in an
     *  --allow-fd-open arg and open at the SETL level.  The opening
     *  at the SETL level may have been implicit as in a std stream
     *  or auto-opened stream, or explicit as in OPEN or MKSTEMP.
     */
    if (f->ftype == no_file) {
      runerr("In restricted mode, only SETL-opened streams may be CLOSEd");
    }
    if (fd == fd_stderr) {
      runerr("Cannot CLOSE STDERR in restricted mode");
    }
  }
  if (f->ftype != no_file) {
    /*
     *  Per policy change of 15 Sept 2019, an explicit CLOSE always
     *  overrides persistence of the underlying OS-level fd if any,
     *  even for an auto-opened stream over an existing OS-level fd.
     */
    f->abilities &= ~can_persist;
  }
  file_close(fd, how);  /* may set errno */
  retire(hb);
} /* end l_close */

/*
 *  COMMAND_LINE  -- the TUPLE of STRING args passed to the SETL program
 */
tuple *l_command_line(void) {
  return copy_tuple(command_line);
} /* end l_command_line */

/*
 *  COMMAND_NAME  -- the external STRING name of the SETL program
 */
string *l_command_name(void) {
  return copy_string(command_name);
} /* end l_command_name */

block *l_compile(block *a) {
  if (restricted) runerr("COMPILE is restricted");
  runerr("COMPILE not yet implemented");
} /* end l_compile */

block *l_complex(block *a) {
  runerr("COMPLEX not yet implemented");
} /* end l_complex */

/*
 *  OP COS(a);                      $ cosine
 *    CASE OF
 *    (IS_REAL a):
 *      RETURN "cosine of a";
 *    ELSE ERROR;
 *    END CASE;
 *  END OP COS;
 */
real *l_cos(real *a) {
  double x = get_double(a,"COS operand");
  return new_real(cos(x));
} /* end l_cos */

/*
 *  OP COSH(a);                      $ hyperbolic cosine
 *    CASE OF
 *    (IS_REAL a):
 *      RETURN "hyperbolic cosine of a";
 *    ELSE ERROR;
 *    END CASE;
 *  END OP COSH;
 */
real *l_cosh(real *a) {
  double x = get_double(a,"COSH operand");
  return new_real(cosh(x));
} /* end l_cosh */

/*
 *  DATE  -- FDATE(TOD,"%c")
 */
string *l_date(void) {
  int saved_errno = errno;  /* don't let DATE disturb errno */
  time_t t = time(0);
  struct tm *timptr = os_localtime(&t);
  char d[80];
  size_t d_size = os_strftime(d, sizeof d, "%c", timptr);
  assert (0 < d_size && d_size < sizeof d);
  errno = saved_errno;
  return new_string(d);
} /* end l_date */

/*
 *  DENOTYPE a  -- the TYPE of UNSTR a as a STRING, or OM
 */
string *l_denotype(string *a) {
  HANDLE ha = ref(a);
  int stopper = EOF;    /* "top-level" */
  long i = 1;           /* string index */
  blocktype t;
  if (!is_string(a)) {
    unary_op_error ("DENOTYPE", a, "STRING");
  }
  span_white(a, &i);  /* skip leading whitespace if any */
  t = denotype(a, &i, &stopper);
  retire(ha);
  if (t == invalid_type) return OM;
  span_white(a, &i);  /* skip whitespace after the denotation */
  if (i <= a->nchar) return OM;  /* there is junk after that */
  if (t == om_type) {
    return new_string(TYPENAME(OM));
  } else {
    block b;  /* just a dummy place to stash a typecode for TYPENAME */
    b.type = t;
    return new_string(TYPENAME(&b));
  }
} /* end l_denotype */

/*
 *  OP DIV(a,b);
 *    CASE OF
 *    (IS_INTEGER a AND IS_INTEGER b):     $ integer division
 *      IF b = 0 THEN ERROR; END IF;
 *      $ See also Dewar, p. 63 - treatment of negative operands
 *      $ is not to be consistent with that of MOD.
 *      x := ABS a;
 *      y := 0;
 *      (WHILE x >= ABS b)
 *        x -:= ABS b;
 *        y +:= 1;
 *      END WHILE;
 *      RETURN y * SIGN a * SIGN b;
 *    ELSE ERROR;
 *    END CASE;
 *  END OP DIV;
 *
 *  Unlike C, SETL defines its division-related operations in a
 *  machine-independent way.  The precise rules can be deduced from
 *  the following examples:
 *
 *    5 div  3 =  1  $ round towards 0
 *   -5 div  3 = -1
 *    5 div -3 = -1
 *   -5 div -3 =  1
 *
 *    5 rem  3 =  2  $ mod of magnitudes, sign follows first opnd
 *   -5 rem  3 = -2
 *    5 rem -3 =  2
 *   -5 rem -3 = -2
 *
 *    5 mod  3 =  2  $ mathematical modulo, result always non-negative
 *   -5 mod  3 =  1
 *    5 mod -3 =  2
 *   -5 mod -3 =  1
 */
integer *l_div(integer *a, integer *b) {
  if (is_integer(a) && is_integer(b)) {
    if (b->size == 0) {
      runerr("Second operand of DIV is 0");
    }
    return integer_div(a,b);
  }
  binary_op_error ("DIV", a, b, "INTEGER operands");
} /* end l_div */

/*
 *  OP DOMAIN(a);                    $ domain of map
 *    CASE OF
 *    (IS_MAP a):
 *      RETURN {x : [x,y] IN a};
 *    ELSE ERROR;
 *    END CASE;
 *  END OP DOMAIN;
 */
set *l_domain(set *a) {
  HANDLE ha = ref(a);
  set *r;  HANDLE hr;
  if (!is_set(a)) {
    unary_op_error ("DOMAIN", a, "SET (map)");
  }
  if (promote(a,mmap)) {
    long n = patsiz(a->tblval), i;
    r = null_set();  hr = ref(r);
    for (i=1; i<=n; i++) {
      subnode *c;
      check (ordsee(a->tblval, i, &c));
      set_insert (&r, unkey(c->k));
    }
    assert (r->card == n);
    retire(hr);
  } else {
    runerr("DOMAIN requires a set that is a map");
  }
  retire(ha);
  return r;
} /* end l_domain */

real *l_double(block *a) {
  runerr("DOUBLE operator not yet implemented");
} /* end l_double */

/*
 *  DUP a  -- low-level file descriptor (fd) duplication via dup()
 *
 *  POSIX dictates that this be the next available small integer.
 *  A fd refers to an open "file description" which is not closed by
 *  the OS until the last such reference is closed.
 *
 *  Note that a fd created or returned by DUP, DUP2, PIPE, SOCKETPAIR,
 *  or RECV_FD is only open at the operating system level, not at the
 *  buffered SETL level.  It can be opened at the SETL level by OPEN
 *  or by being auto-opened as the result of a first I/O op.
 */
integer *l_dup(integer *a) {
  int fd, fd2;
  fd = get_long_in(a, fd_lo,fd_hi-1, "fd passed to DUP");
  if (restricted) runerr("DUP is restricted");
  fd2 = os_dup(fd);
  if (fd2 < 0) return OM;  /* with errno as set by dup() */
  /* We do not check that fd2 is not open at the SETL level.
   * Thus we admit the possibility of someone wanting to reuse the
   * SETL file attributes and buffer on a fd2 that was just closed
   * by an extension package, rather than officiously flagging down
   * something that isn't currently logically possible anyway.  */
  return new_integer(fd2);  /* ready for next OPEN or auto-open */
} /* end l_dup */

/*
 *  DUP2(a, b)  -- low-level file descriptor (fd) duplication via dup2()
 *
 *  The behaviour of DUP2 follows that of POSIX dup2(), including the
 *  fact that it is a no-op (after sanity checks) when fd1 equals fd2.
 *  On Linux, the closing of fd2 is done atomically with the duplication
 *  of fd1.
 */
integer *l_dup2(integer *a, integer *b) {
  int fd1, fd2;
  fd1 = get_long_in(a, fd_lo,fd_hi-1, "first fd passed to DUP2");
  fd2 = get_long_in(b, fd_lo,fd_hi-1, "second fd passed to DUP2");
  if (restricted) runerr("DUP2 is restricted");
  /* We do not check that fd2 is not open at the SETL level.
   * This is important, so that e.g. DUP2(STDOUT, STDERR) can
   * be used for redirection equivalent to the shell's 2>&1
   * without disturbing STDERR's buffer and attributes.  */
  if (os_dup2(fd1,fd2) < 0) return OM;  /* with errno as set by dup2() */
  return new_integer(fd2);  /* ready for next OPEN or auto-open */
} /* end l_dup2 */

void l_eject(tuple *a, long nargs) {
  check_optargs_type(a,"EJECT");
  runerr("EJECT not implemented for this pilot.  Try printing ctrl-L.");
} /* end l_eject */

/*
 *  EOF     -- on last input operation
 *  EOF(f)  -- on last input operation on f
 *
 *  Check for end-of-file condition, either on the specified stream
 *  or (with no arguments) for the last input operation on any stream.
 *
 *  See eof_file() in sys.c, which is how these EOF indicators get set,
 *  and clear_eof() there, which is how they get cleared.
 */
boolean *l_eof(tuple *a, long nargs) {
  check_optargs_type(a,"EOF");
  if (nargs == 0) {
    return new_boolean(at_eof());
  } else if (nargs == 1) {
    int fd;
    file *f;
    io_buffer *b;
    block *p = a->nelt == 0 ? OM : tupelt(a,1);
    if (!is_stream_id(p)) {
      unary_proc_error ("EOF", p, STREAM_ID " or no");
    }
    fd = getfd (p, getfd_info);
    /* Signal and timer streams cannot currently make EOF true, but
     * we do not stop people from checking it.  */
    f = find_file(fd);
    b = f->buffer;
    return new_boolean(b != NULL ? b->eof : false);
  } else {
    runerr("EOF takes at most 1 (stream id) arg, not %ld", nargs);
  }
} /* end l_eof */

/*
 *  OP EVEN(a);                      $ test even integer
 *    CASE OF
 *    (IS_INTEGER a):
 *      RETURN a MOD 2 = 0;
 *    ELSE ERROR;
 *    END CASE;
 *  END OP EVEN;
 */
boolean *l_even(integer *a) {
  if (is_integer(a)) {
    ro_mpz(z, a)  /* const mpz_t z = alias of INTEGER a */
    return new_boolean (mpz_even_p(z) != 0);
  } else {
    unary_op_error ("EVEN", a, "INTEGER");
  }
} /* end l_even */

/*
 *  EXEC(pathname);              -- act like args = [pathname]
 *  EXEC(pathname, args);        -- pathname and args given
 *  EXEC(pathname, args, envt);  -- environment also given
 *
 *  This is deliberately rather low-level; if you want shell-style
 *  processing a la SYSTEM, use something like
 *
 *   EXEC ("/bin/sh", ["sh", "-c", "any shell command"]);
 *
 *    or
 *
 *   EXEC ("/bin/sh", ["-sh", "-c", "any shell command"]);
 *
 *  (the latter if you want to be replaced by a login shell).
 *
 *  Note that this call winds up at a POSIX execvp() or execve() call;
 *  execve() requires a full pathname for the file to be run, and is
 *  invoked if the 3rd arg (tuple of environment variable settings) is
 *  present.  Otherwise, the current envt var settings are used, and
 *  execvp(), which uses PATH to find an executable when the pathname
 *  contains no slash, is invoked.
 */
void l_exec(string *a, tuple *b, long nargs) {
  long i;
  long n_argv;
  long n_envp;
  char *default_argv[2];
  char **argv = NULL;
  char **envp = NULL;
  if (!is_string(a)) {
    unary_variadic_error ("EXEC", a, "STRING");
  }
  check_optargs_type(b,"EXEC");
  if (nargs > 3) {
    runerr("EXEC takes 1, 2, or 3 args (pathname[,args[,envt]]),"
           " not %ld", nargs);
  }
  if (nargs == 1) {
    /* Default argv[0] for new process is the pathname */
    default_argv[0] = &strelt(a,1);
    default_argv[1] = NULL;
    argv = default_argv;  /* point argv at default_argv */
  }
  if (nargs > 1) {
    /* Build argv from second arg to EXEC */
    tuple *t_argv = b->nelt == 0 ? OM : (tuple *)tupelt(b,1);
    if (!is_tuple(t_argv)) {
      runerr("Second arg (if any) to EXEC must be TUPLE, not %s",
                                                     TYPENAME(t_argv));
    }
    n_argv = t_argv->nelt;
    /* os_malloc() OK here since process image gets replaced.  */
    argv = (char **) os_malloc((n_argv+1) * sizeof argv[0]);
    for (i=1; i<=n_argv; i++) {
      string *s = (string *)tupelt(t_argv,i);
      if (!is_string(s)) {
        runerr("Every element of second arg to EXEC must be"
               " STRING, not %s", TYPENAME(s));
      }
      argv[i-1] = &strelt(s,1);  /* OK because l_exec doesn't return */
    }
    argv[n_argv] = NULL;
  }
  if (nargs == 3) {
    /* Build envp from third arg to EXEC */
    tuple *t_envp;
    t_envp = b->nelt < 2 ? OM : (tuple *)tupelt(b,2);
    if (!is_tuple(t_envp)) {
      runerr("Third arg (if any) to EXEC must be TUPLE, not %s",
                                                    TYPENAME(t_envp));
    }
    n_envp = t_envp->nelt;
    /* os_malloc() OK here since process image gets replaced.  */
    envp = (char **) os_malloc((n_envp+1) * sizeof envp[0]);
    for (i=1; i<=n_envp; i++) {
      string *s = (string *)tupelt(t_envp,i);
      if (!is_string(s)) {
        runerr("Every element of third arg to EXEC must be"
               " STRING, not %s", TYPENAME(s));
      }
      envp[i-1] = &strelt(s,1);  /* OK because l_exec doesn't return */
    }
    envp[n_envp] = NULL;
  }
  if (restricted) runerr("EXEC is restricted");
  /* Call execvp or execve according as an environment was specified: */
  if (nargs <= 2) os_execvp(&strelt(a,1), argv);
  else            os_execve(&strelt(a,1), argv, envp);
  /* The os_exec*() functions do not return.  */
} /* end l_exec */

block *l_execute(block *a) {
  if (restricted) runerr("EXECUTE is restricted");
  runerr("EXECUTE not yet implemented");
} /* end l_execute */

/*
 *  OP EXP(a);                       $ natural exponential
 *    CASE OF
 *    (IS_REAL a):
 *      RETURN "e**a, or error if a too big or too small";
 *    ELSE ERROR;
 *    END CASE;
 *  END OP EXP;
 */
real *l_exp(real *a) {
  double x = get_double(a,"EXP operand");
  return new_real(exp(x));
} /* end l_exp */

/*
 *  FDATE(ms)       -- ms rendered as calendar time using default fmt
 *  FDATE(ms, fmt)  -- ms rendered according to given fmt
 *
 *  Converts milliseconds to a calendar date and time in the manner of
 *  strftime() but with the extension that %s in the format expands to
 *  the millisecond mod 1000 as 3 chars (with 0 or more leading zeroes).
 *
 *  The second argument is optional, and its default is equivalent to
 *  "%a %b %e %H:%M:%S.%s %Z %Y", which could produce
 *
 *  "Sat Sep  4 04:20:55.690 EDT 1999"
 */
string *l_fdate(integer *ms, tuple *fmt, long nargs) {
  HANDLE hms = ref(ms);
  HANDLE hfmt = ref(fmt);
  string *r;
  int saved_errno = errno;  /* don't let FDATE disturb errno */
  const struct timeval t = integer_ms_to_timeval (ms,
                            "first arg to FDATE");
  const time_t sec = t.tv_sec;
  const int msec = t.tv_usec / 1000;
  struct tm *timptr = os_localtime(&sec);
  if (timptr == NULL || timptr->tm_year > INT_MAX - 1900) {
    /* Either localtime() got EOVERFLOW, or it produced a year in the
     * ticklish last 1900 years of timptr->tm_year's range (POSIX
     * requires that field to be an int).  It is ticklish because as of
     * this writing (early 2021), the Ubuntu 20.04LTS glibc strftime()
     * renders %Y as a negative value, a behaviour we don't want.  */
    runerr("First arg to FDATE would give year > %d", INT_MAX);
  }
  check_optargs_type(fmt, "FDATE");
  if (nargs == 1) {
    char format[30], result[50];
    size_t result_size;
    snprintf (format, sizeof format, "%%a %%b %%e %%T.%.3d %%Z %%Y", msec);
    result_size = os_strftime(result, sizeof result, format, timptr);
    assert (0 < result_size && result_size < sizeof result);
    r = new_string(result);
  } else if (nargs == 2) {
    /* Rather fiddly... */
    size_t i, j, argfmt_len, format_len;
    size_t room, result_size;
    long pctcnt;
    char *argfmt;
    char *format;
    char *result;
    string *x = fmt->nelt == 0 ? OM : (string *)tupelt(fmt,1);
    if (!is_string(x)) {
      binary_proc_error ("FDATE", ms, x, "(INTEGER, optional STRING)");
    }
    argfmt_len = x->nchar;
    if (UNLIKELY (argfmt_len == 0)) {
      r = null_string();
    } else {
      argfmt = &strelt(x,1);
      if (strlen(argfmt) != argfmt_len) {
        runerr("FDATE format string must not contain NUL characters");
      }
      /* Arb limit on fmt len to discourage malfeasance.  It's just
       * supposed to be for formatting a time and date, for heaven's
       * sake.  */
      if (argfmt_len > 1000) {
        runerr("Second arg (format) to FDATE is too long");
      }
      format_len = 2*argfmt_len + 2;
      format = (char *)arena_alloc(format_len);
      pctcnt = 0;
      for (i=0,j=0; i < argfmt_len && j+1 < format_len; i++,j++) {
        if (argfmt[i] == '%') {
          if (i+1 < argfmt_len && argfmt[i+1] == 's') {
            pctcnt++;
            snprintf (&format[j], format_len - j, "%.3d", msec);
            i++;
            j+=2;  /* the 2-char %s was replaced by 3 digits */
          } else if (i+1 < argfmt_len && argfmt[i+1] == '%') {
            format[j] = '%';
            format[j+1] = '%';
            i++;
            j++;
          } else {
            pctcnt++;
            format[j] = argfmt[i];
          }
        } else {
          format[j] = argfmt[i];
        }
      }
      assert (j < format_len);
      format[j] = '\0';
      room = 5*argfmt_len + 50*pctcnt + 500;
      result = (char *)arena_alloc(room);
      result_size = os_strftime(result, room, format, timptr);
      assert (result_size < room);
      /* Because of the theoretical possibility that strftime(3) could
       * legitimately produce a null string (for format %p in some
       * locales, ostensibly), we return a null string even in the
       * indistinguishable pathological error case where such a big
       * field width specifier appears in the format that the result
       * overflows our exceedingly generous 'room' provision (in which
       * case strftime returns 0).  */
      r = new_nstring(result, result_size);
      arena_free(result, room);
      arena_free(format, format_len);
    }
  } else {
    runerr("FDATE takes 1 or 2 args, not %ld", nargs);
  }
  retire(hfmt);
  retire(hms);
  errno = saved_errno;
  return r;
} /* end l_fdate */

/*
 *  FEXISTS a  -- whether the given pathname exists, as a BOOLEAN
 */
boolean *l_fexists(string *a) {
  int saved_errno = errno;  /* don't let FEXISTS disturb errno */
  struct stat statbuf;
  if (!is_string(a)) {
    unary_op_error ("FEXISTS", a, "STRING filename");
  }
  if (restricted) runerr("FEXISTS is restricted");
  /* An interface to access() would have sufficed here, but for
   * symmetry with l_lexists(), we use the stat() interface:  */
  if (os_stat(&strelt(a,1),&statbuf) == 0) {
    return new_boolean(true);  /* errno unchanged */
  } else {
    errno = saved_errno;  /* restore errno after failed stat() */
    return new_boolean(false);
  }
} /* end l_fexists */

/*
 *  FILENAME a  -- the STRING, 2-TUPLE, or INTEGER on which stream a
 *                 was opened
 */
block *l_filename(block *a) {
  int fd;
  block *name;
  if (!is_stream_id(a)) {
    unary_op_error ("FILENAME", a, STREAM_ID);
  }
  fd = getfd (a, getfd_info);
  name = make_name(fd);
  if (name == OM) {
    name = (block *) new_integer(fd);
  }
  return name;  /* STRING or 2-TUPLE or INTEGER */
} /* end l_filename */

/*
 *  FILENO a
 *
 *  FILENO gives you a file descriptor (fd) for a stream handle, but
 *  raises an exception if you pass it something else, such as OM.
 *
 *  So if you pass an open fd to FILENO, you just get it back, which
 *  is useful in the idiom
 *
 *    fd := FILENO OPEN (...)
 *
 *  as an easy way of making sure the program abends right away if
 *  OPEN returns OM.
 *
 *  You could also use FILENO for its nominal purpose of retrieving
 *  the fd associated with whatever you passed to OPEN or had
 *  auto-opened or got from MKSTEMP.
 */
integer *l_fileno(block *a) {
  int fd;
  if (!is_stream_id(a)) {
    unary_op_error ("FILENO", a, STREAM_ID);
  }
  fd = getfd (a, getfd_info);
  return new_integer(fd);
} /* end l_fileno */

/*
 *  FILEPOS a
 *
 *  Similar to SEEK(a, 0, SEEK_CUR) but without flushing or draining
 *  the stream.  It does, however, account for unflushed output and
 *  unconsumed input to give the same result as if those operations had
 *  been done.  Also, FILEPOS is allowed even if the stream is not
 *  seekable, in which case it gives the number of bytes read and/or
 *  written so far.  Furthermore, it does not auto-open.
 */
integer *l_filepos(block *a) {
  int fd;
  off_t pos;
  if (!is_stream_id(a)) {
    unary_op_error ("FILEPOS", a, STREAM_ID);
  }
  fd = getfd (a, getfd_info);
  pos = file_pos(fd);
  return new_integer(pos);
} /* end l_filepos */

/*
 *  FILTER(a)
 *  FILTER(a, b)
 *
 *  string_from_stdout := FILTER (command[, string_to_feed_stdin])
 *
 *  STATUS is set to the termination status of the command.
 *
 *  If status is unavailable due to an unexpected return value from
 *  the final waitpid() on the command, STATUS is set to OM and
 *  LAST_ERROR gives the waitpid() failure reason.
 *
 *  See do_filter() in sys.c for implementation details.
 */
string *l_filter(string *a, tuple *b, long nargs) {
  int raw_status;
  string *x;
  string *r;
  if (!is_string(a)) {
    unary_variadic_error ("FILTER", a, "STRING");
  }
  check_optargs_type(b, "FILTER");
  if (nargs == 1) {
    x = OM;
  } else if (nargs == 2) {
    x = b->nelt == 0 ? OM : (string *)tupelt(b,1);
    if (!is_string(x)) {
      binary_proc_error ("FILTER", a, x, "(STRING, optional STRING)");
    }
    if (x->nchar == 0) x = OM;
  } else {
    runerr("FILTER takes 1 or 2 args, not %ld", nargs);
  }
  if (restricted) {
    /* Case-sensitive check on shell cmd and args */
    const char *cmd = &strelt(a,1);
    if (!is_allowed(allowed_filter_list, cmd)) {
      runerr("FILTER command \"%s\" is restricted",
                           tame(cmd));
    }
  }
  r = do_filter(a,x,&raw_status);  /* waitpid() may set errno */
  set_raw_status(raw_status);  /* no_status if do_filter() failed */
  return r;
} /* end l_filter */

/*
 *  OP FIX(a);                       $ convert real to integer
 *    CASE OF
 *    (IS_REAL a):
 *      RETURN "result of truncating real a to integer (keep sign)";
 *    ELSE ERROR;
 *    END CASE;
 *  END OP FIX;
 *
 *  C did not use to specify that fixing a negative floating-point
 *  value to integer truncates towards zero, but SETL does.
 */
integer *l_fix(real *a) {
  switch (setl_type(a)) {
  case integer_type:
    return copy_integer ((integer *)a);
  case real_type:
    return double_to_integer (real_to_double(a));
  default:
    unary_op_error ("FIX", a, "numeric");
  }
} /* end l_fix */

/*
 *  The behaviour of FIXED is governed by that of snprintf(3), except
 *  that a negative "precision" (third argument to FIXED) is illegal.
 *  Negative "width" (second arg) means left justification instead of
 *  the usual right.
 */
string *l_fixed(real *v, integer *w, integer *a) {
  const double x = get_double(v, "First arg to FIXED");
  /* These restrictions should be lifted if multi-precision REALs
   * replace the current C double-based REALs:  */
  const int wid = get_long_in(w, -10000, 10000, "second (width) arg to FIXED");
  const int aft = get_long_in(a, 0, 10000, "third arg to FIXED");
  const int n = snprintf (NULL, 0, "%*.*f", wid, aft, x);
  if (n > 0) {  /* assume errno was not touched */
    string *r;
    char *buf = (char *)arena_alloc((size_t)n + 1);
    snprintf (buf, (size_t)n + 1, "%*.*f", wid, aft, x);
    assert (strlen(buf) == (size_t)n);
    r = new_string(buf);
    arena_free(buf, (size_t)n + 1);
    return r;
  } else {
    panic("snprintf (NULL, 0, \"%%*.*f\", %d, %d, %g): %s",
                                         wid, aft, x, os_strerror(errno));
  }
} /* end l_fixed */

/*
 *  OP FLOAT(a);                     $ convert integer to real
 *    CASE OF
 *    (IS_INTEGER a):
 *      RETURN "result of converting integer a to real";
 *    ELSE ERROR;
 *    END CASE;
 *  END OP FLOAT;
 *
 *  Extension:  I think FLOAT should be allowed to mean "make sure
 *  this is REAL" as well as "convert this to REAL", so I allow it:
 */
real *l_float(integer *a) {
  switch (setl_type(a)) {
  case integer_type:
    return integer_to_real(a);
  case real_type:
    return copy_real((real *)a);
  default:
    unary_op_error ("FLOAT", a, "numeric");
  }
} /* end l_float */

/*
 *  FLOATING is like FIXED, but uses %e format rather than %f.
 */
string *l_floating(real *v, integer *w, integer *a) {
  const double x = get_double(v, "First arg to FLOATING");
  /* These restrictions should be lifted if multi-precision REALs
   * replace the current C double-based REALs:  */
  const int wid = get_long_in(w, -10000, 10000, "second arg to FLOATING");
  const int aft = get_long_in(a, 0, 10000, "third arg to FLOATING");
  const int n = snprintf (NULL, 0, "%*.*e", wid, aft, x);
  if (n > 0) {  /* assume errno was not touched */
    string *r;
    char *buf = (char *)arena_alloc((size_t)n + 1);
    snprintf (buf, (size_t)n + 1, "%*.*e", wid, aft, x);
    assert (strlen(buf) == (size_t)n);
    r = new_string(buf);
    arena_free(buf, (size_t)n + 1);
    return r;
  } else {
    panic("snprintf (NULL, 0, \"%%*.*e\", %d, %d, %g): %s",
                                         wid, aft, x, os_strerror(errno));
  }
} /* end l_floating */

/*
 *  OP FLOOR(a);                     $ floor of real
 *    CASE OF
 *    (IS_REAL a):
 *      CASE OF
 *      (a < 0.0):
 *        LOOP INIT i := 0; DOING i -:= 1; DO
 *          IF FLOAT i <= a THEN RETURN i; END IF;
 *        END LOOP;
 *      (a >= 0.0):
 *        LOOP INIT i := 0; DOING i +:= 1; DO
 *          IF FLOAT i > a THEN RETURN i - 1; END IF;
 *        END LOOP;
 *      END CASE;
 *    ELSE ERROR;
 *    END CASE;
 *  END OP FLOOR;
 */
integer *l_floor(real *a) {
  switch (setl_type(a)) {
  case integer_type:
    return copy_integer ((integer *)a);
  case real_type:
    return double_to_integer (floor (real_to_double(a)));
  default:
    unary_op_error ("FLOOR", a, "numeric");
  }
} /* end l_floor */

/*
 *  PROC FLUSH(a);  -- for stream a
 *
 *  Write out any buffered output bytes.  A no-op if there aren't any.
 *
 *  Unconsumed input need not be drained, as that will already have
 *  happened preparatory to any output operation on the stream.
 *
 *  Failure of the underlying POSIX write() sets errno (whence
 *  LAST_ERROR).
 */
void l_flush(block *a) {
  int fd;
  if (!is_stream_id(a)) {
    unary_proc_error ("FLUSH", a, STREAM_ID);
  }
  fd = getfd (a, getfd_flush);
  file_flush(fd);  /* may set errno */
} /* end l_flush */

/*
 *  FORK is like POSIX fork() but with SETL stream buffering also handled.
 *
 *  Unlike for the other intrinsics that create subprocesses, failure of
 *  the underlying fork() is reflected by FORK as an OM return with errno
 *  set (whence LAST_ERROR) rather than as a program abend.
 *
 *  All output buffers are flushed as if by FLUSH before the spawning
 *  attempt.
 *
 *  In the child, all unread input on signal streams is drained, all
 *  timer streams are closed, and the time base for CLOCK is set to 0.
 */
integer *l_fork(void) {
  pid_t pid;
  if (restricted) runerr("FORK is restricted");
  pid = do_fork();
  if (pid < 0) return OM;  /* with errno as set by fork() */
  return new_integer(pid);  /* child pid in parent, 0 in child */
} /* end l_fork */

/*
 *  OP FROM(WR a, RW b);
 *    CASE OF
 *    (IS_SET b):                          $ take from set
 *      a := ARB b;
 *      IF a /= OM THEN b LESS:= a; END IF;
 *      RETURN a;
 *    ELSE ERROR;
 *    END CASE;
 *  END OP FROM;
 */
void l_from(block **a, set **b) {
  block *r;
  set *s = *b;
  if (!is_set(s)) {
    binary_op_error ("FROM", *a, s, "SET as second operand");
  }
  r = set_from(&s);
  *a = r;
  *b = s;
} /* end l_from */

/*
 *  OP FROMB(WR a, RW b);
 *    CASE OF
 *    (IS_STRING b):                     $ take from start of string
 *      IF b = '' THEN
 *        a := OM;
 *      ELSE
 *        a := b(1);
 *        b := b(2..);
 *      END IF;
 *    (IS_TUPLE b):                      $ take from start of tuple
 *      IF b = [] THEN
 *        a := OM;
 *      ELSE
 *        a := b(1);
 *        b := b(2..);
 *      END IF;
 *    ELSE ERROR;
 *    END CASE;
 *  END OP FROMB;
 */
void l_fromb(block **a, block **b) {
  switch (setl_type(*b)) {
  case string_type:
    {
      string *s = (string *)*b;
      if (s->nchar == 0) {
        *a = OM;
      } else {
        HANDLE hs = ref(s);
        block *p = (block *)new_cstring(strelt(s,1));
        s->nbefore++;
        s->nchar--;
        *a = p;
        *b = (block *)s;
        retire(hs);
      }
    }
    return;
  case tuple_type:
    {
      tuple *t = (tuple *)*b;
      if (t->nelt == 0) {
        *a = OM;
      } else {
        HANDLE ht = ref(t);
        block *p = copy_value(tupelt(t,1));
        t->npre++;
        t->nelt--;
        *a = p;
        *b = (block *)t;
        retire(ht);
      }
    }
    return;
  default:
    binary_op_error ("FROMB", *a, *b, "TUPLE or STRING as second operand");
  }
} /* end l_fromb */

/*
 *  OP FROME(WR a, RW b);
 *    CASE OF
 *    (IS_TUPLE b):                        $ take from end of tuple
 *      IF b = [] THEN
 *        a := OM;
 *      ELSE
 *        a := b(#b);
 *        b := b(1..#b-1);
 *      END IF;
 *    (IS_STRING b):                       $ take from end of string
 *      IF b = '' THEN
 *        a := OM;
 *      ELSE
 *        a := b(#b);
 *        b := b(1..#b-1);
 *      END IF;
 *      RETURN a;
 *    ELSE ERROR;
 *    END CASE;
 *  END OP FROME;
 */
void l_frome(block **a, block **b) {
  switch (setl_type(*b)) {
  case string_type:
    {
      string *s = (string *)*b;
      if (s->nchar == 0) {
        *a = OM;
      } else {
        HANDLE hs = ref(s);
        block *p = (block *)new_cstring(last_char(s));
        s->nafter++;
        s->nchar--;
        *a = p;
        *b = (block *)s;
        retire(hs);
      }
    }
    return;
  case tuple_type:
    {
      tuple *t = (tuple *)*b;
      if (t->nelt == 0) {
        *a = OM;
      } else {
        HANDLE ht = ref(t);
        block *p = copy_value(tupelt(t,t->nelt));
        t->nsuf++;
        t->nelt--;
        tup_truncate(&t);
        *a = p;
        *b = (block *)t;
        retire(ht);
      }
    }
    return;
  default:
    binary_op_error ("FROME", *a, *b, "TUPLE or STRING as second operand");
  }
} /* end l_frome */

/*
 *  FSIZE a
 *
 *  Size in bytes of object identified by stream designator or filename.
 *  This is upwardly compatible with the SETL2 version.
 */
integer *l_fsize(block *a) {
  int fd;
  struct stat statbuf;
  if (!is_stream_id(a)) {
    unary_op_error ("FSIZE", a, STREAM_ID);
  }
  fd = getfd (a, getfd_check);
  if (fd >= 0) {
    if (restricted) runerr("FSIZE is restricted");
    /*
     *  Note that we do no sanity check on the stream ftype, so FSIZE
     *  can be used on any fd for which the caller knows that the local
     *  fstat() gives a meaningful .st_size field in the stat struct.
     */
    if (os_fstat(fd, &statbuf) == 0) {
      return new_integer(statbuf.st_size);  /* errno not set */
    }
  } else {
    if (!is_string(a)) {
      unary_op_error ("FSIZE", a, "stream designator or STRING filename");
    }
    if (restricted) runerr("FSIZE is restricted");
    if (os_stat(&strelt((string *)a,1), &statbuf) == 0) {
      return new_integer(statbuf.st_size);  /* errno not set */
    }
  }
  return OM;  /* errno indicates why, per fstat() or stat() failure */
} /* end l_fsize */

/*
 *  PROC FTRUNC(a,b);  -- stream or filename a, size b
 *
 *  "Truncate" file (adjust size upward or downward), using POSIX
 *  ftruncate() or truncate().
 *
 *  If a is a stream, it is flushed and drained before the ftruncate().
 */
void l_ftrunc(block *a, integer *b) {
  off_t length;
  int fd;
  if (!is_stream_id(a)) {
    unary_op_error ("FTRUNC", a, STREAM_ID);
  }
  length = get_off_t(b, "second (length) arg to FTRUNC");
  fd = getfd (a, getfd_ftrunc);
  if (fd >= 0) {
    if (restricted) runerr("FTRUNC is restricted");
    /*
     *  If there is any unconsumed input, drain it from the input
     *  buffer.  Besides ensuring that the next demand for a byte
     *  causes a fresh system-level read, this has the side-effect of
     *  syncing the system-level file pos (and our file_pos) with the
     *  current SETL-level FILEPOS in preparation for the ftruncate()
     *  (which does not itself modify the file pos, per POSIX).
     *
     *  The draining thus makes sure that bytes that were in the file
     *  beyond its new length will not appear as input; the worst case
     *  is that some bytes may have to be re-fetched by a new read(),
     *  a tiny corner case not worth optimizing away.
     */
    file_drain(fd);
    os_ftruncate(fd, length);  /* may set errno */
  } else {
    if (!is_string(a)) {
      unary_op_error ("FTRUNC", a, "stream designator or STRING filename");
    }
    if (restricted) runerr("FTRUNC is restricted");
    os_truncate(&strelt((string *)a,1), length);  /* may set errno */
  }
} /* end l_ftrunc */

/*
 *  GET and GETA have signatures similar to READ and READA, as in SETL2.
 *  The old (SDDS 1986) signature of GET was a misfit, and has been
 *  more appropriately pinned on GETA.
 *
 *  PUT/PUTA have similarly been made consistent with PRINT/PRINTA.
 *
 *  See get_lines() for fine points of GET and GETA,
 *      get_values() for GETB, READ, and READA,
 *      get_one_char() for GETC and GETCHAR,
 *      get_chars() for GETN and GETS, and
 *      peek_one_char() for PEEKC and PEEKCHAR.
 *
 *  The above readers (GET etc.), along with GETFILE and GETLINE,
 *  all attempt to auto-open a stream in 'r' mode ('r+' for GETS)
 *  when presented with a plausible (but not open) filename or a fd
 *  that is only open at the OS level (no SETL buffer yet)...or in
 *  'real-ms' or bidirectional 'tcp-client' mode for certain forms
 *  of TUPLE.
 *
 *  Except for GETS, auto-opening by a reader marks the stream for
 *  auto-closing upon EOF (though 'real-ms' streams never end).
 *  See getfd() in sys.c for more details on auto-opening.
 *
 *  Before attempting input, the above readers all flush any output
 *  buffered for the stream or for any TIEd stream it has.
 *
 *  Finally, they all close the stream when it is marked for
 *  auto-closing and the EOF indicators are being set due to an
 *  end of input (end of file or input error).
 */

/*
 *  PROC GET(WR a(*));  -- read 0 or more lines from STDIN
 */
void l_get(tuple **a, long nargs) {
  file *f;
  check (fd_stdin == getfd ((block *)stdin_integer, getfd_read));
  f = find_fd_file(fd_stdin);
  let (*a, get_lines (f, nargs));  /* may set errno */
} /* end l_get */

/*
 *  PROC GETA(a, WR b(*));  -- read 0 or more lines from stream a
 */
void l_geta(block *a, tuple **b, long nargs) {
  int fd;
  file *f;
  if (!is_stream_id(a)) {
    unary_variadic_error ("GETA", a, STREAM_ID);
  }
  fd = getfd (a, getfd_read);
  f = find_file(fd);
  let (*b, get_lines (f, nargs-1));  /* may set errno */
} /* end l_geta */

/*
 *  PROC GETB(a, WR b(*));  -- read 0 or more values from stream a
 */
void l_getb(block *a, tuple **b, long nargs) {
  int fd;
  file *f;
  if (!is_stream_id(a)) {
    unary_variadic_error ("GETB", a, STREAM_ID);
  }
  fd = getfd (a, getfd_read);
  f = find_file(fd);
  let (*b, get_values (f, nargs-1, "GETB input", false));  /* may set errno */
} /* end l_getb */

/*
 *  GETC a  -- read 1 char from stream a, as a STRING
 */
string *l_getc(block *a) {
  int fd;
  file *f;
  if (!is_stream_id(a)) {
    unary_op_error ("GETC", a, STREAM_ID);
  }
  fd = getfd (a, getfd_read);
  f = find_file(fd);
  return get_one_char(f);  /* may set errno */
} /* end l_getc */

/*
 *  GETCHAR  -- read 1 char from STDIN, as a STRING
 */
string *l_getchar(void) {
  file *f;
  check (fd_stdin == getfd ((block *)stdin_integer, getfd_read));
  f = find_fd_file(fd_stdin);
  return get_one_char(f);  /* may set errno */
} /* end l_getchar */

void l_getem(block **a, block **b) {
  runerr("GETEM not implemented (whatever it means)");
} /* end l_getem */

/*
 *  GETEGID  -- get effective group ID
 */
integer *l_getegid(void) {
  if (restricted) runerr("GETEGID is restricted");
  return new_integer((long)os_getegid());
} /* end l_getegid */

/*
 *  GETENV a  -- get value of envt var named a
 */
string *l_getenv(string *a) {
  char *c;
  if (!is_string(a)) {
    unary_op_error ("GETENV", a, "STRING");
  }
  if (restricted) runerr("GETENV is restricted");
  c = os_getenv(&strelt(a,1));
  if (c == NULL) return OM;
  else return new_string(c);
} /* end l_getenv */

/*
 *  GETEGID  -- get effective user ID
 */
integer *l_geteuid(void) {
  if (restricted) runerr("GETEUID is restricted");
  return new_integer((long)os_geteuid());
} /* end l_geteuid */

void l_getf(block *a, tuple **b, long nargs) {
  runerr("GETF not implemented (whatever it means)");
} /* end l_getf */

/*
 *  GETFILE is unique among auto-openers in that if the file cannot
 *  be opened, GETFILE merely returns OM instead of triggering a
 *  'runerr', and sets errno to indicate why the file could not be
 *  auto-opened for sequential reading.
 *
 *  A successful GETFILE sets the SETL-level EOF indicators to TRUE.
 *  It doesn't matter to users.  A GETFILE that sets EOF is arguably
 *  anomalous relative to all the input intrinsics that only set it
 *  when they get nothing.  But against that, a nice clean way of
 *  defining the trigger for auto-closing is as the act of setting
 *  the EOF indicators.  So that definitional simplicity wins, and
 *  we use that by having eof_file(), the converter of pending EOFs
 *  to present ones, do any auto-closing.
 *
 *  If GETFILE fails on auto-open, it doesn't touch the EOF indicators.
 *  If it gets an error during reading, however, it does set them and
 *  also sets errno (whence LAST_ERROR).
 */
string *l_getfile(block *a) {
  string *r;  HANDLE hr;
  int c;
  int fd;
  file *f;
  io_buffer *p;
  if (!is_stream_id(a)) {
    unary_op_error ("GETFILE", a, STREAM_ID);
  }
  /* The only getfd() case for which an auto-open failure gives a fd
   * of -1 and sets errno, rather than abending, is getfd_getfile:  */
  fd = getfd (a, getfd_getfile);
  /* Note that if the file can't be auto-opened, EOF isn't touched.  */
  if (fd == -1) return OM;  /* with errno possibly set */
  /* We do not rule out GETFILE on signal or timer streams, though it
   * amounts to telling the program to pause, catching the signal or
   * SIGALRM and gradually using up all of memory in the get_char()
   * loop, until killed.  We do refuse to auto-open a timer stream in
   * the case of GETFILE, though; see getfd().  */
  f = find_file(fd);
  p = f->buffer;
  assert (p != NULL);
  r = null_string();
  hr = ref(r);
  clear_eof(p);  /* clear SETL-level EOF indicators but not eof_pending */
  while ((c=get_char(p)) != EOF) {  /* may set errno */
    str_tackon(&r,c);
  }
  assert (p->eof_pending);  /* per get_char() returning EOF */
  eof_file(f);  /* adjust EOF indicators; auto-close if appropriate */
  retire(hr);
  return r;  /* with errno maybe set by get_char() and/or file_close() */
} /* end l_getfile */

/*
 *  GETGID  -- get "real" group id (which is an INTEGER)
 */
integer *l_getgid(void) {
  if (restricted) runerr("GETGID is restricted");
  return new_integer((long)os_getgid());
} /* end l_getgid */

integer *l_getipp(block *a) {
  runerr("GETIPP not implemented (whatever it means)");
} /* end l_getipp */

void l_getk(tuple **a, long nargs) {
  runerr("GETK not implemented (whatever it means)");
} /* end l_getk */

/*
 *  line := GETLINE f
 *
 *  GETA(f,line), but in functional form, for getting just one line.
 */
string *l_getline(block *a) {
  string *r;  HANDLE hr;
  int c;
  int fd;
  file *f;
  io_buffer *p;
  if (!is_stream_id(a)) {
    unary_op_error ("GETLINE", a, STREAM_ID);
  }
  fd = getfd (a, getfd_read);
  f = find_file(fd);
  p = f->buffer;
  assert (p != NULL);
  r = null_string();
  hr = ref(r);
  clear_eof(p);  /* clear SETL-level EOF indicators but not eof_pending */
  while ((c=get_char(p))!=EOF && c!='\n') {  /* may set errno */
    str_tackon(&r,c);
  }
  if (r->nchar == 0 && p->eof_pending) {  /* no chars read; eof hit */
    eof_file(f);  /* adjust EOF indicators; auto-close if appropriate */
    r = OM;  /* with errno maybe set by get_char() and/or file_close() */
  } else {
    /* r is nonempty, or end of file wasn't met yet */
  }
  retire(hr);
  return r;  /* string or OM, with errno possibly set */
} /* end l_getline */

/*
 *  GETN(a, b)  -- read b bytes from stream a
 *
 *  This routine returns an empty string rather than OM when EOF is hit
 *  immediately, so callers can simply check for a returned string of
 *  an expected size instead of having to check for OM first.  Contrast
 *  GETLINE above, for which the natural test is for OM.
 */
string *l_getn(block *a, integer *b) {
  int fd;
  long n;
  file *f;
  if (!is_stream_id(a)) {
    binary_proc_error ("GETN", a, b, "(" STREAM_ID ", INTEGER)");
  }
  n = get_nat_long(b, "second arg to GETN (number of bytes)");
  fd = getfd (a, getfd_read);
  f = find_file(fd);
  return get_chars (f, n);  /* may set errno */
} /* end l_getn */

/*
 *  GETPGID(p)  -- get the process group ID of process p (or of self if
 *                 p is 0, in which case GETPGRP() is preferred)
 *
 *  ---> This function is now deprecated and removed from the lib doc.
 */
integer *l_getpgid(integer *a) {
  pid_t pid, pgid;
  if (restricted) runerr("GETPGID is restricted");
  /* We require 0 or a pid here; negative is error, not pgid:  */
  pid = get_nat_pid_t(a, "process ID passed to GETPGID");
  pgid = os_getpgid(pid);  /* errno is set if getpgid() gives -1 */
  return pgid != -1 ? new_integer(pgid) : OM;
} /* end l_getpgid */

/*
 *  GETPGRP  -- get caller's process group ID
 *
 *  Unlike GETPGID, GETPGRP does not set errno.  In the inconceivable
 *  case where the underlying getpgid(0) fails, it abends instead.
 */
integer *l_getpgrp(void) {
  /* if (restricted) runerr("GETPGRP is restricted"); */
  return new_integer(os_getpgrp());
} /* end l_getpgrp */

/*
 *  GETPID  -- get caller's process ID
 */
integer *l_getpid(void) {
  /* if (restricted) runerr("GETPID is restricted"); */
  return new_integer(os_getpid());
} /* end l_getpid */

/*
 *  GETPID  -- get caller's parent's process ID
 */
integer *l_getppid(void) {
  /* if (restricted) runerr("GETPPID is restricted"); */
  return new_integer(os_getppid());
} /* end l_getppid */

/*
 *  GETS(a, b, c, d);  -- d := c bytes starting at b from stream a
 *
 *  GETS is for SETL2 compatibility.  The WR parameter would make more
 *  sense if GET-type routines yielded some kind of error indication,
 *  but that's history for you.  It would have been better for such
 *  routines to yield strings.  Instead there is a mixed bag in which
 *  GETC, GETCHAR, GETN, GETLINE, and GETFILE do in fact yield strings,
 *  while GET, GETA, and this GETS return them via final WR parameters.
 *
 *  An alternative to using GETS is to SEEK and then GETN; don't forget
 *  that SEEK starts at 0 (it works in "offsets") whereas GETS indexes
 *  the characters of the file starting at 1.  Indeed, GETS operates by
 *  SEEKing the file to an offset equal to the starting index (b) less 1
 *  and then effectively calling GETN.  See also PUTS.
 */
void l_gets(block *a, integer *b, integer *c, string **d) {
  int fd;
  off_t start;
  long n;
  file *f;
  if (!is_stream_id(a)) {
    runerr("First arg to GETS must be " STREAM_ID ", not %s",
                                                       TYPENAME(a));
  }
  start = get_pos_off_t(b, "second arg to GETS (starting index)");
  n = get_nat_long(c, "third arg to GETS (length)");
  fd = getfd (a, getfd_gets);
  f = find_fd_file(fd);
  file_seek (fd, start-1, SEEK_SET);
  let (*d, get_chars(f, n));  /* may set errno */
} /* end l_gets */

/*
 *  GETSID(p)  --  get session id of process p (or of self if p is 0)
 *  GETSID     --  GETSID(0)
 */
integer *l_getsid(tuple *a, long nargs) {
  pid_t pid, sid;
  /* if (restricted) runerr("GETSID is restricted"); */
  check_optargs_type(a,"GETSID");
  switch (nargs) {
  case 0:
    pid = 0;  /* meaning the calling process */
    break;
  case 1:
    if (a->nelt == 0) {
      pid = 0;
    } else {
      pid = get_nat_pid_t((integer *)tupelt(a,1),
                          "process ID passed to GETSID");
    }
    break;
  default:
    runerr("GETSID takes at most 1 (process ID) arg, not %ld", nargs);
  }
  if (pid != 0 && restricted) runerr("GETSID(nonzero) is restricted");
  sid = os_getsid(pid);  /* errno is set if getsid() gives -1 */
  return sid != -1 ? new_integer(sid) : OM;
} /* end l_getsid */

string *l_getspp(block *a) {
  runerr("GETSPP not implemented (whatever it means)");
} /* end l_getspp */

/*
 *  GETUID  -- get "real" user id (which is an INTEGER)
 */
integer *l_getuid(void) {
  if (restricted) runerr("GETUID is restricted");
  return new_integer((long)os_getuid());
} /* end l_getuid */

/*
 *  GETWD  -- get pathname of current working directory
 */
string *l_getwd(void) {
  /* On Linux, PATH_MAX is usually 4096, like our default if somehow
   * the POSIX-required <limits.h> is missing.  _POSIX_PATH_MAX is 256.
   * We could be fussy and use a larger number for deserving
   * filesystems, but let us not.  */
  char cwd[PATH_MAX+1];
  if (restricted) runerr("GETWD is restricted");
  os_getcwd(cwd,PATH_MAX);
  return new_string(cwd);
} /* end l_getwd */

/*
 *  GLOB a  -- tuple of filenames (as strings) matching wildcard a
 *
 *  If there is no match for the shell "globbing" pattern in a,
 *  the empty tuple is returned.  If POSIX glob() fails, it isn't
 *  supposed to set errno, but the Linux glob(3) man page as of
 *  glibc 2.1 admits that possibility under BUGS.  We preserve
 *  errno across GLOB calls rather than reflect that possibility.
 */
tuple *l_glob(string *a) {
  tuple *r;
  glob_t raw_glob;  /* raw result from lower-level os_glob() */
  int saved_errno;
  if (!is_string(a)) {
    unary_op_error ("GLOB", a, "STRING");
  }
  if (restricted) runerr("GLOB is restricted");
  raw_glob.gl_pathc = 0;
  raw_glob.gl_pathv = NULL;
  raw_glob.gl_offs = 0;
  saved_errno = errno;  /* glob() may set errno on some systems */
  if (os_glob (&strelt(a,1), 0, NULL, &raw_glob) == 0) {
    tuple *t = new_tuple(raw_glob.gl_pathc);
    HANDLE ht = ref(t);
    long i;
    for (i=0; (size_t)i<raw_glob.gl_pathc; i++) {
      let (tupelt(t,i+1), (block *)new_string(raw_glob.gl_pathv[i]));
    }
    retire(ht);
    r = t;
  } else {
    r = null_tuple();
  }
  if (raw_glob.gl_pathv) {
    os_globfree (&raw_glob);
  }
  errno = saved_errno;  /* preserve from all glob() perils */
  return r;
} /* end l_glob */

/*
 *  GMARK(a, b)  -- TUPLE of index pairs locating pattern b in STRING a
 */
tuple *l_gmark(string *a, block *b) {
  if (!is_string(a) ||
      !(is_string(b) || is_tuple(b))) {
    binary_proc_error ("GMARK", a, b, "(STRING, STRING or TUPLE)");
  }
  if (is_tuple(b)) {
    tuple *t = (tuple *)b;
    long i;
    if (t->nelt != 2) {
      runerr("Second arg to GMARK, if TUPLE, must have 2 elements,"
             " not %ld", t->nelt);
    }
    for (i=1; i<=2; i++) {
      block *p = tupelt(t,i);
      if (!is_string(p)) {
        runerr("Element %ld of second arg to GMARK must be STRING,"
               " not %s", i, TYPENAME(p));
      }
    }
  }
  return rex_gmark(a,b);
} /* end l_gmark */

/*
 *  GSUB(RW a, b)
 *  GSUB(RW a, b, c)  -- TUPLE of substrings of a that match pattern b
 *                       and are replaced in a by c (default '')
 */
tuple *l_gsub(string **a, block *b, tuple *c, long nargs) {
  string *s = *a;
  if (!is_string(s)) {
    runerr("First (subject) arg to GSUB must be STRING, not %s",
                                                         TYPENAME(s));
  }
  if (!(is_string(b) || is_tuple(b))) {
    runerr("Second arg to GSUB must be STRING or TUPLE, not %s",
                                                         TYPENAME(b));
  }
  if (is_tuple(b)) {
    tuple *t = (tuple *)b;
    long i;
    if (t->nelt != 2) {
      runerr("Second arg to GSUB, if TUPLE, must have 2 elements,"
             " not %ld", t->nelt);
    }
    for (i=1; i<=2; i++) {
      block *p = tupelt(t,i);
      if (!is_string(p)) {
        runerr("Element %ld of second arg to GSUB must be STRING,"
               " not %s", i, TYPENAME(p));
      }
    }
  }
  check_optargs_type(c,"GSUB");
  if (nargs == 2) {
    HANDLE hs = ref(s);
    HANDLE hb = ref(b);
    string *x = null_string();
    tuple *r = rex_gsub(&s,b,x);
    *a = s;
    retire(hb);
    retire(hs);
    return r;
  } else if (nargs == 3) {
    tuple *r;
    string *x = c->nelt == 0 ? OM : (string *)tupelt(c,1);
    if (!is_string(x)) {
      runerr("Third arg to GSUB (if any) must be STRING, not %s",
                                                          TYPENAME(x));
    }
    r = rex_gsub(&s,b,x);
    *a = s;
    return r;
  } else {
    runerr("GSUB takes 2 or 3 args, not %ld", nargs);
  }
} /* end l_gsub */

/*
 *  HEX s  -- render s in hexadecimal, #HEX s = 2 * #s
 */
string *l_hex(string *a) {
  const char hexchars[] = "0123456789ABCDEF";
  HANDLE ha = ref(a);
  string *r;
  long i;
  if (!is_string(a)) {
    unary_op_error ("HEX", a, "STRING");
  }
  r = new_estring(2 * a->nchar);
  for (i=1; i<=a->nchar; i++) {
    /* implicit assumption of 8-bit chars here */
    uchar c = strelt(a,i);
    strelt(r,2*i-1) = hexchars[c>>4];
    strelt(r,2*i  ) = hexchars[c&0xf];
  }
  retire(ha);
  return r;
} /* end l_hex */

string *l_host(tuple *a, long nargs) {
  check_optargs_type(a,"HOST");
  runerr("HOST not implemented");
} /* end l_host */

/*
 *  The nullary HOSTADDR yields a plausible IPv4 or IPv6 address
 *  (dot or colon notation) for the local host, if possible.
 *
 *  The returned address comes from the first item on the list
 *  produced by getaddrinfo() on the name returned by gethostname(),
 *  such that the address family is AF_INET or AF_INET6.
 *
 *  If getaddrinfo() fails, errno is set and HOSTADDR returns OM.
 *  HOSTADDR also returns OM without setting errno if getaddrinfo()
 *  succeeds but the list it gives has no items of address family
 *  AF_INET or AF_INET6.
 *
 *  It is not clear that HOSTADDR is a useful primitive.
 */
string *l_hostaddr(void) {
  char              hostname[NI_MAXHOST+1];
  struct addrinfo  *head;
  string           *r = OM;  /* default, pending success */
  /* No hints here.  For this case, we are being inclusive about what we
   * accept from getaddrinfo().  */
  hostname[0] = '\0';  /* paranoia */
  if (os_gethostname(hostname, sizeof hostname) == 0 &&
      os_getaddrinfo(hostname, NULL, NULL, &head) == 0) {
    const struct addrinfo *ai;
    for (ai=head; ai; ai=ai->ai_next) {
      if (ai->ai_family == AF_INET ||
          ai->ai_family == AF_INET6) {
        r = sockaddr_address(ai->ai_addr, ai->ai_addrlen);
        break;  /* success; leave the for-loop */
      }
    }
    os_freeaddrinfo (head);  /* does not set errno */
  } else {
    /* os_getaddrinfo() may have set errno */
  }
  return r;  /* IPv4 addr with dots, or IPv6 addr with colons, or OM */
} /* end l_hostaddr */

/*
 *  HOSTNAME returns the local host name if gethostname() succeeds,
 *  but otherwise sets LAST_ERROR (errno) and returns OM.
 */
string *l_hostname(void) {
  char hostname[NI_MAXHOST+1];
  string *r = OM;  /* default, pending success */
  hostname[0] = '\0';  /* paranoia */
  if (os_gethostname(hostname, sizeof hostname) == 0) {
    r = new_string(hostname);
  }
  return r;  /* some name or OM */
} /* end l_hostname */

/*
 *  ICHAR is equivalent to ABS when applied to a STRING, and being
 *  more specific, is usually to be preferred.
 */
integer *l_ichar(string *a) {
  if (!is_string(a)) {
    unary_op_error ("ICHAR", a, "STRING");
  }
  if (a->nchar != 1) {
    runerr("ICHAR operand must contain 1 character, not %ld"
           " (but see UNPACK_INT etc.)",             a->nchar);
  }
  return ulong_integer((uchar)strelt(a,1));
} /* end l_ichar */

real *l_imag(block *a) {
  runerr("IMAG not yet implemented");
} /* end l_imag */

/*
 *  For historical reasons, the IMPL operator is not short-circuited;
 *  to get that effect you should replace
 *
 *    p IMPL q
 *
 *  with one of the propositionally equivalent
 *
 *    (NOT p) OR q
 *    q OR (NOT p)
 *
 *  which in most programming contexts (testing values, not proving
 *  theorems) is more natural anyway.
 */
boolean *l_impl(boolean *a, boolean *b) {
  if (is_boolean(a) && is_boolean(b)) {
    return new_boolean(!a->booval || b->booval);
  }
  binary_op_error ("IMPL", a, b, "BOOLEAN operands");
} /* end l_impl */

/*
 *  OP IN(a,b);
 *    IF a = OM THEN ERROR; END IF;
 *    CASE OF
 *    (IS_SET b):                          $ test element in set
 *      RETURN EXISTS x IN b | x = a;
 *    (IS_STRING a AND IS_STRING b):       $ test character in string
 *      IF #a /= 1 THEN ERROR; END;
 *      RETURN EXISTS x IN {1..#b} | a = b(x);
 *    (IS_TUPLE b):                        $ test element in tuple
 *      RETURN EXISTS x IN {1..#b} | a = b(x);
 *    ELSE ERROR;
 *    END CASE;
 *  END OP IN;
 *
 *  Extension for strings:  the first operand can now be a string of
 *  any length, and the test is for it being a substring of the second.
 *
 *  Technically, it is also an extension to allow "OM IN s" tests.
 *  For sets, it is defined here as FALSE.
 */
boolean *l_in(block *a, block *b) {
  if (is_set(b)) {
    if (is_om(a)) return new_boolean(false);
    return new_boolean(in_set((set *)b, a));
  } else if (is_string(b)) {
    string *p;  long pn;
    string *s;  long sn;
    long i,n;
    if (!is_string(a)) {
      binary_op_error ("IN", a, b,
       "STRING as first operand when second operand is STRING");
    }
    p = (string *)a;  pn = p->nchar;
    s = (string *)b;  sn = s->nchar;
    n = sn - pn + 1;
    for (i=1; i<=n; i++) {
      if (memcmp(&strelt(p,1),&strelt(s,i),pn) == 0) {
        return new_boolean(true);
      }
    }
    return new_boolean(false);
  } else if (is_tuple(b)) {
    tuple *t = (tuple *)b;
    long i, n = t->nelt;
    for (i=1; i<=n; i++) {
      if (equal_value(a,tupelt(t,i))) return new_boolean(true);
    }
    return new_boolean(false);
  }
  binary_op_error ("IN", a, b, "STRING, SET, or TUPLE as second operand");
} /* end l_in */

/*
 *  OP INCS(a,b);
 *    CASE OF
 *    (IS_SET a AND IS_SET b):             $ set inclusion test
 *      RETURN FORALL x IN b | x IN a;
 *    ELSE ERROR;
 *    END CASE;
 *  END OP INCS;
 */
boolean *l_incs(set *a, set *b) {
  HANDLE ha;
  HANDLE hb;
  iterator *i;  HANDLE hi;
  block *x;
  bool flag;
  if (!is_set(a) ||
      !is_set(b)) {
    binary_op_error ("INCS", a, b, "SET operands");
  }
  if (a->card < b->card) return new_boolean(false);
  ha = ref(a);
  hb = ref(b);
  i = init_iterator((block *)b);  hi = ref(i);
  flag = true;
  while (flag && step_iterator(&i,&x)) if (!in_set(a,x)) flag = false;
  retire(hi);
  retire(hb);
  retire(ha);
  return new_boolean(flag);
} /* end l_incs */

integer *l_int(block *a) {
  runerr("INT function not implemented");
} /* end l_int */

/*
 *  INTSLASH  -- FALSE by default, TRUE for impaired INTEGER division
 */
boolean *l_intslash(void) {
  return new_boolean(get_intslash());
} /* end l_intslash */

/*
 *  With 0 args, IP_ADDRESSES gives you a set of IP addresses by
 *  which your own host is known.  Now deprecating this case.
 *
 *  If there is 1 arg, it should be a string containing either an
 *  Internet host name or an Internet (IP) host address, and you get
 *  a set of IPv4 and/or IPv6 addresses by which that host is known.
 *
 *  In the 1-arg case, if errno is set, it is by gai_errno() following
 *  a failed getaddrinfo() on the given host, and the empty set is
 *  returned.
 */
set *l_ip_addresses(tuple *a, long nargs) {
  string *host;
  check_optargs_type(a,"IP_ADDRESSES");
  if (nargs == 0) {  /* deprecated case */
    char hostname[NI_MAXHOST+1];
    os_gethostname(hostname, sizeof hostname);
    host = new_string(hostname);
  } else if (nargs == 1) {
    host = a->nelt == 0 ? OM : (string *)tupelt(a,1);
    if (!is_string(host)) {
      unary_proc_error ("IP_ADDRESSES", host, "STRING");
    }
  } else {
    runerr("IP_ADDRESSES takes 1 arg, not %ld args", nargs);
  }
  if (host->nchar >= NI_MAXHOST) {
    runerr("Host name or address \"%s\" too long in IP_ADDRESSES call",
                       tame(&strelt(host,1)));
  }
  return ip_addresses(host);  /* set of strings, empty if it sets errno */
} /* end l_ip_addresses */

/*
 *  With 0 args, IP_NAMES gives you the set of names your own host
 *  goes by, including all locally known aliases.  Now deprecating
 *  that case.  Removed it from lib doc.
 *
 *  IP_NAMES(a)  -- all known names of a
 *
 *  The arg should be a STRING containing either an Internet host name
 *  or an Internet (IP) host address, and it returns all the known names
 *  of that host.
 *
 *  If LAST_ERROR is set, it is by gai_errno() following a failed
 *  getaddrinfo() (see ip_names()), and the empty set is returned.
 */
set *l_ip_names(tuple *a, long nargs) {
  string *host;
  check_optargs_type(a,"IP_NAMES");
  if (nargs == 0) {
    char hostname[NI_MAXHOST+1];
    os_gethostname(hostname, sizeof hostname);
    host = new_string(hostname);
  } else if (nargs == 1) {
    host = a->nelt == 0 ? OM : (string *)tupelt(a,1);
    if (!is_string(host)) {
      unary_proc_error ("IP_NAMES", host, "STRING");
    }
  } else {
    runerr("IP_NAMES takes 1 arg, not %ld args", nargs);
  }
  if (host->nchar >= NI_MAXHOST) {
    runerr("Host name or address \"%s\" too long in IP_NAMES call",
                       tame(&strelt(host,1)));
  }
  return ip_names(host);  /* set of strings, empty if it sets errno */
} /* end l_ip_names */

/*
 *  The IS_type operators:
 */

boolean *l_is_atom(block *a) {
  return new_boolean (is_atom(a));
} /* end l_is_atom */

boolean *l_is_boolean(block *a) {
  return new_boolean (is_boolean(a));
} /* end l_is_boolean */

boolean *l_is_complex(block *a) {
  runerr("IS_COMPLEX not yet implemented");
} /* end l_is_complex */

boolean *l_is_double(block *a) {
  runerr("IS_DOUBLE not yet implemented");
} /* end l_is_double */

boolean *l_is_float(block *a) {
  runerr("IS_FLOAT not yet implemented");
} /* end l_is_float */

boolean *l_is_integer(block *a) {
  return new_boolean (is_integer(a));
} /* end l_is_integer */

/* IS_MAP tests for a SET that is a mapping.  */
boolean *l_is_map(block *a) {
  if (is_set(a)) {
    return new_boolean (promote((set *)a,mmap));
  }
  return new_boolean(false);
} /* end l_is_map */

/* IS_MMAP is the same as IS_MAP.  */
boolean *l_is_mmap(block *a) {
  if (is_set(a)) {
    return new_boolean (promote((set *)a,mmap));
  }
  return new_boolean(false);
} /* end l_is_mmap */

/* IS_NUMERIC means IS_INTEGER or IS_REAL */
boolean *l_is_numeric(block *a) {
  return new_boolean (is_numeric(a));
} /* end l_is_numeric */

boolean *l_is_om(block *a) {
  return new_boolean (is_om(a));
} /* end l_is_om */

/* IS_OPEN tests if the operand is a stream (meaning it is open at the
 * SETL level), which is not strictly a type test.  */
boolean *l_is_open(block *a) {
  int fd;
  if (!is_stream_id(a)) {
    unary_op_error ("IS_OPEN", a, STREAM_ID);
  }
  fd = getfd (a, getfd_check);
  return new_boolean (fd >= 0);
} /* end l_is_open */

boolean *l_is_op(block *a) {
  runerr("IS_OP has been retired.  Please use IS_ROUTINE instead.");
} /* end l_is_op */

boolean *l_is_proc(block *a) {
  runerr("IS_PROC has been retired.  Please use IS_ROUTINE instead.");
} /* end l_is_proc */

boolean *l_is_real(block *a) {
  return new_boolean (is_real(a));
} /* end l_is_real */

boolean *l_is_routine(block *a) {
  return new_boolean (is_routine(a));
} /* end l_is_routine */

boolean *l_is_set(block *a) {
  return new_boolean (is_set(a));
} /* end l_is_set */

/* IS_MAP tests for a SET that is a single-valued mapping.  */
boolean *l_is_smap(block *a) {
  if (is_set(a)) {
    return new_boolean (promote((set *)a,smap));
  }
  return new_boolean(false);
} /* end l_is_smap */

boolean *l_is_string(block *a) {
  return new_boolean (is_string(a));
} /* end l_is_string */

boolean *l_is_there(block *a) {
  runerr("IS_THERE not yet implemented");
} /* end l_is_there */

boolean *l_is_tuple(block *a) {
  return new_boolean (is_tuple(a));
} /* end l_is_tuple */

/*
 *  PROC JOIN (tup, delim);
 *    ASSERT IS_TUPLE tup;
 *    ASSERT IS_STRING delim;
 *    $ "delim + s" is "delim + STR s" when s isn't already a STRING:
 *    RETURN ('' +/ [delim + s : s in tup])(#delim + 1 ..);
 *  END PROC;
 */
string *l_join(tuple *a, string *b) {
  HANDLE ha;
  HANDLE hb;
  string *r;  HANDLE hr;
  long i,j,na,nb,n;
  if (!is_tuple(a) ||
      !is_string(b)) {
    binary_proc_error ("JOIN", a, b, "(TUPLE, STRING)");
  }
  na = a->nelt;
  if (na == 0) return null_string();
  nb = b->nchar;
  ha = ref(a);
  hb = ref(b);
  n = 0;
  for (i=1; i<=na; i++) {
    block *p = tupelt(a,i);
    string *s = is_string(p) ? (string *)p : tostr(p);
    long ns = s->nchar;
    n += ns + nb;
  }
  n -= nb;
  r = new_estring(n);
  hr = ref(r);
  j = 1;
  for (i=1; i<=na; i++) {
    block *p = tupelt(a,i);
    string *s = is_string(p) ? (string *)p : tostr(p);  /* p or STR p */
    long ns = s->nchar;
    mvmem (&strelt(r,j), &strelt(s,1), ns);
    j += ns;
    if (i != na) {
      mvmem (&strelt(r,j), &strelt(b,1), nb);
      j += nb;
    }
  }
  assert (j == n+1);
  retire(hr);
  retire(hb);
  retire(ha);
  return r;
} /* end l_join */

/*
 *  KILL (pid, sig);  -- send signal sig (given as integer or string) to
 *                    -- process pid
 *
 *  KILL (pid);  -- send SIGTERM to process pid
 *
 *  See also PEXISTS.
 */
void l_kill(integer *a, tuple *b, long nargs) {
  int sig = 0;  /* initialize to prevent compiler warning */
  pid_t pid;
  pid = get_pid_t(a, "first arg to KILL (e.g. pid or -pgid)");
  check_optargs_type(b,"KILL");
  if (nargs == 1) {
    sig = SIGTERM;
  } else if (nargs == 2) {
    block *x = b->nelt == 0 ? OM : tupelt(b,1);
    switch (setl_type(x)) {
    case integer_type:
      {
        integer *i = (integer *)x;
        sig = get_int(i, "second arg to KILL (signal)");
      }
      break;
    case string_type:
      {
        string *s = str_upper((string *)x);
        const char *t = &strelt(s,1);
        /* Our feeble excuse for not unifying this with sig_num() in sys.c
         * is that this list of signals is much bigger.  There are many
         * more signals you can send than you can subscribe for.  Such as
         * SIGKILL, for example.  */
        if      (leq(t,"HUP" ) || leq(t,"SIGHUP" )) sig = SIGHUP;
        else if (leq(t,"INT" ) || leq(t,"SIGINT" )) sig = SIGINT;
        else if (leq(t,"QUIT") || leq(t,"SIGQUIT")) sig = SIGQUIT;
        else if (leq(t,"ILL" ) || leq(t,"SIGILL" )) sig = SIGILL;
#ifdef SIGTRAP
        else if (leq(t,"TRAP") || leq(t,"SIGTRAP")) sig = SIGTRAP;
#endif
        else if (leq(t,"ABRT") || leq(t,"SIGABRT")) sig = SIGABRT;
#ifdef SIGIOT
        else if (leq(t,"IOT" ) || leq(t,"SIGIOT" )) sig = SIGIOT;
#endif
#ifdef SIGBUS
        else if (leq(t,"BUS" ) || leq(t,"SIGBUS" )) sig = SIGBUS;
#endif
#ifdef SIGEMT
        else if (leq(t,"EMT" ) || leq(t,"SIGEMT" )) sig = SIGEMT;
#endif
        else if (leq(t,"FPE" ) || leq(t,"SIGFPE" )) sig = SIGFPE;
        else if (leq(t,"KILL") || leq(t,"SIGKILL")) sig = SIGKILL;
        else if (leq(t,"SEGV") || leq(t,"SIGSEGV")) sig = SIGSEGV;
#ifdef SIGSYS
        else if (leq(t,"SYS" ) || leq(t,"SIGSYS" )) sig = SIGSYS;
#endif
        else if (leq(t,"PIPE") || leq(t,"SIGPIPE")) sig = SIGPIPE;
        else if (leq(t,"ALRM") || leq(t,"SIGALRM")) sig = SIGALRM;
        else if (leq(t,"TERM") || leq(t,"SIGTERM")) sig = SIGTERM;
#ifdef SIGSTKFLT
        else if (leq(t,"STKFLT")||leq(t,"SIGSTKFLT"))sig= SIGSTKFLT;
#endif
        else if (leq(t,"USR1") || leq(t,"SIGUSR1")) sig = SIGUSR1;
        else if (leq(t,"USR2") || leq(t,"SIGUSR2")) sig = SIGUSR2;
        else if (leq(t,"CHLD") || leq(t,"SIGCHLD")) sig = SIGCHLD;
#ifdef SIGCLD
        else if (leq(t,"CLD")  || leq(t,"SIGCLD"))  sig = SIGCLD;
#endif
        else if (leq(t,"CONT") || leq(t,"SIGCONT")) sig = SIGCONT;
        else if (leq(t,"STOP") || leq(t,"SIGSTOP")) sig = SIGSTOP;
        else if (leq(t,"TSTP") || leq(t,"SIGTSTP")) sig = SIGTSTP;
        else if (leq(t,"TTIN") || leq(t,"SIGTTIN")) sig = SIGTTIN;
        else if (leq(t,"TTOU") || leq(t,"SIGTTOU")) sig = SIGTTOU;
#ifdef SIGURG
        else if (leq(t,"URG" ) || leq(t,"SIGURG" )) sig = SIGURG;
#endif
#ifdef SIGIO
        else if (leq(t,"IO"  ) || leq(t,"SIGIO"  )) sig = SIGIO;
#endif
#ifdef SIGPOLL
        else if (leq(t,"POLL") || leq(t,"SIGPOLL")) sig = SIGPOLL;
#endif
#ifdef SIGXCPU
        else if (leq(t,"XCPU") || leq(t,"SIGXCPU")) sig = SIGXCPU;
#endif
#ifdef SIGXFSZ
        else if (leq(t,"XFSZ") || leq(t,"SIGXFSZ")) sig = SIGXFSZ;
#endif
#ifdef SIGVTALRM
        else if (leq(t,"VTALRM")||leq(t,"SIGVTALRM"))sig= SIGVTALRM;
#endif
#ifdef SIGPROF
        else if (leq(t,"PROF") || leq(t,"SIGPROF")) sig = SIGPROF;
#endif
#ifdef SIGPWR
        else if (leq(t,"PWR" ) || leq(t,"SIGPWR" )) sig = SIGPWR;
#endif
#ifdef SIGINFO
        else if (leq(t,"INFO") || leq(t,"SIGINFO")) sig = SIGINFO;
#endif
#ifdef SIGLOST
        else if (leq(t,"LOST") || leq(t,"SIGLOST")) sig = SIGLOST;
#endif
#ifdef SIGWINCH
        else if (leq(t,"WINCH")|| leq(t,"SIGWINCH"))sig = SIGWINCH;
#endif
#ifdef SIGUNUSED
        else if (leq(t,"UNUSED")||leq(t,"SIGUNUSED"))sig= SIGUNUSED;
#endif
        /*
         *  Please feel free to recognize more signal names here
         *  (an #ifdef guard would be appreciated, of course).
         */
        else {
          runerr("Signal name \"%s\" unrecognized by KILL",
                            tame(t));
        }
      }
      break;
    default:
      /* Saying "pid" rather than the more specific "process ID" here,
       * so that for example process group IDs aren't ruled out:  */
      binary_proc_error ("KILL", a, x,
       "(INTEGER pid, optional STRING or INTEGER signal)");
    } /* end switch (setl_type(x)) */
  } else {
    runerr("KILL takes 1 or 2 args, not %ld", nargs);
  }
  if (restricted && !is_local_subprocess(pid)) {
    runerr("KILL of pid %ld is restricted", (long)pid);
  }
  os_kill(pid, sig);  /* kill() may set errno */
} /* end l_kill */

/*
 *  LAST_ERRNO  -- raw integer error code
 *
 *  ---> not shown in lib doc; probably best deprecated
 */
integer *l_last_errno(void) {
  return new_integer(errno);
} /* end l_last_errno */

/*
 *  LAST_ERROR  -- most recent "system"-level error message as a STRING
 */
string *l_last_error(void) {
  return new_string(os_strerror(errno));
} /* end l_last_error */

/*
 *  LEN(RW a, b)  -- remove and return the first b bytes from STRING a
 *
 *  It seems a little dubious that if there are not enough
 *  characters in the string to satisfy LEN, you just get what
 *  there are, rather than OM.  But for now, that's how it is for
 *  this and the rest of the SNOBOL-"inspired" crowd.
 *
 *  A similar comment applies to RLEN.
 */
string *l_len(string **a, integer *b) {
  string *s = *a;     HANDLE hs = ref(s);
  string *t;
  long i,nb;
  if (!is_string(s)) {
    binary_proc_error ("LEN", s, b, "(STRING, INTEGER)");
  }
  nb = get_nat_long(b, "second arg to LEN");
  i = MIN(nb,s->nchar);
  t = copy_substring(s,1,i);
  s->nbefore += i;
  s->nchar -= i;
  *a = s;
  retire(hs);
  return t;
} /* end l_len */

/*
 *  OP LESS(a,b);
 *    CASE OF
 *    (IS_SET a):                          $ remove set element
 *      RETURN {x IN a | x /= b};
 *    ELSE ERROR;
 *    END CASE;
 *  END OP LESS;
 *
 *  Extension:  s LESS OM is permitted for set s, and returns s.
 */
set *l_less(set *a, block *b) {
  HANDLE hb = ref(b);
  set *r;
  if (!is_set(a)) {
    binary_op_error ("LESS", a, b, "SET as first operand");
  }
  r = copy_set(a);
  if (!is_om(b)) set_delete(&r,b);
  retire(hb);
  return r;
} /* end l_less */

/*
 *  a LESS:= b;  -- remove element b from SET a
 */
void l_aless(set **a, block *b) {
  set *t = *a;
  if (!is_set(t)) {
    binary_op_error ("LESS:=", t, b, "SET as first operand");
  }
  if (!is_om(b)) set_delete(&t,b);
  *a = t;
} /* end l_aless */

/*
 *  OP LESSF(a,b);
 *    CASE OF
 *    (IS_MAP a):                          $ remove map element(s)
 *      RETURN {[x,y] IN a | x /= b};
 *    ELSE ERROR;
 *    END CASE;
 *  END OP LESSF;
 */
set *l_lessf(set *a, block *b) {
  HANDLE hb = ref(b);
  set *r;  HANDLE hr;
  key *k;
  if (!is_set(a)) {
    binary_op_error ("LESSF", a, b, "SET (map) as first operand");
  }
  if (is_om(b)) {
    binary_op_error ("LESSF", a, b, "non-OM value as right operand");
  }
  r = copy_set(a);   hr = ref(r);
  if (!promote(r,mmap)) runerr("First operand of LESSF must be a map");
  k = tokey(b);
  switch (r->stype) {
  case smap:
    if (keydel(r->tblval, k)) r->card--;
    break;
  case mmap:
    {
      subnode *c;
      if (keysee(r->tblval, k, &c)) {
        set *t = (set *)(c->d);
        assert (is_set(t));
        r->card -= t->card;
        keydel(r->tblval, k);
      }
    }
    break;
  default:
    unexpected (r->stype);
  }
  retire(hr);
  retire(hb);
  return r;
} /* end l_lessf */

/*
 *  a LESSF:= b;  -- remove domain element b from map a
 */
void l_alessf(set **a, block *b) {
  set *t = *a;  HANDLE ht = ref(t);
  HANDLE hb = ref(b);
  key *k;
  if (!is_set(t)) {
    binary_op_error ("LESSF:=", t, b, "SET (map) as first operand");
  }
  if (is_om(b)) {
    binary_op_error ("LESSF:=", a, b, "non-OM value as right operand");
  }
  if (!promote(t,mmap)) runerr("First operand of LESSF must be a map");
  k = tokey(b);
  switch (t->stype) {
  case smap:
    if (keydel(t->tblval, k)) t->card--;
    break;
  case mmap:
    {
      subnode *c;
      if (keysee(t->tblval, k, &c)) {
        set *s = (set *)(c->d);
        assert (is_set(s));
        t->card -= s->card;
        keydel(t->tblval, k);
      }
    }
    break;
  default:
    unexpected (t->stype);
  }
  retire(hb);
  retire(ht);
  *a = t;
} /* end l_alessf */

/*
 *  LEXISTS a  -- whether STRING a names a symlink
 */
boolean *l_lexists(string *a) {
  int saved_errno = errno;  /* don't let LEXISTS disturb errno */
  struct stat statbuf;
  if (!is_string(a)) {
    unary_op_error ("LEXISTS", a, "STRING filename");
  }
  if (restricted) runerr("LEXISTS is restricted");
  if (os_lstat(&strelt(a,1),&statbuf) == 0) {
    return new_boolean(true);  /* errno unchanged */
  } else {
    errno = saved_errno;  /* restore errno after failed lstat() */
    return new_boolean(false);
  }
} /* end l_lexists */

/*
 *  LINK(a, b);  -- make hard link named b the same as existing file a
 */
void l_link(string *a, string *b) {
  if (!is_string(a) ||
      !is_string(b)) {
    binary_proc_error ("LINK", a, b, "(STRING, STRING)");
  }
  if (restricted) runerr("LINK is restricted");
  os_link(&strelt(a,1),&strelt(b,1));  /* link() may set errno */
} /* end l_link */

/*
 *  OP LOG(a);                       $ natural logarithm
 *    CASE OF
 *    (IS_REAL a):
 *      IF a > 0.0 THEN
 *        RETURN "natural logarithm of a";
 *      ELSE
 *        ERROR;
 *      END IF;
 *    ELSE ERROR;
 *    END CASE;
 *  END OP LOG;
 */
real *l_log(real *a) {
  double x = get_double(a,"LOG operand");
  if (x <= 0.0) {
    runerr("LOG operand (%.*g) must be greater than 0",
             DOUBLE_REP_DIGS, x);
  }
  return new_real(log(x));
} /* end l_log */

/*
 *  LPAD(s, n) pads s on the left with blanks to reach length n or more
 *  (so LPAD "right-justifies" the string); n must be non-neg.
 */
string *l_lpad(string *a, integer *b) {
  long n;
  if (!is_string(a)) {
    binary_proc_error ("LPAD", a, b, "(STRING, INTEGER)");
  }
  n = get_nat_long(b, "second arg to LPAD (length)");
  return str_lpad(a,n,' ');
} /* end l_lpad */

/*
 *  MAGIC  -- current setting of ERE (vs literal match) switch
 */
boolean *l_magic(void) {
  return new_boolean(get_magic());
} /* end l_magic */

/*
 *  MARK(a, b)  -- index pair [i,j] locating pattern b in STRING a
 */
tuple *l_mark(string *a, block *b) {
  if (!is_string(a) ||
      !(is_string(b) || is_integer(b) || is_tuple(b))) {
    binary_proc_error ("MARK", a, b,
                        "(STRING, STRING or INTEGER or TUPLE)");
  }
  if (is_tuple(b)) {
    tuple *t = (tuple *)b;
    long i;
    if (t->nelt != 2) {
      runerr("Second arg to MARK, if TUPLE, must have 2 elements,"
             " not %ld", t->nelt);
    }
    for (i=1; i<=2; i++) {
      block *p = tupelt(t,i);
      if (!(is_string(p) || is_integer(p))) {
        runerr("Element %ld of second arg to MARK must be STRING or"
               " INTEGER, not %s", i, TYPENAME(p));
      }
    }
  }
  return rex_mark(a,b);
} /* end l_mark */

/*
 *  MATCH(RW a, b)  -- if a(1..#b) matches b, remove and return it, else ''
 *
 *  Consistent with the strangeness of LEN, you get '' instead of OM if
 *  a does not begin with b.
 *
 *  A similar comment applies to RMATCH.
 */
string *l_match(string **a, string *b) {
  string *s = *a;     HANDLE hs = ref(s);
  string *t;
  long na, nb;
  if (!is_string(s) ||
      !is_string(b)) {
    binary_proc_error ("MATCH", s, b, "(STRING, STRING)");
  }
  na = s->nchar;
  nb = b->nchar;
  if (na >= nb && memcmp(&strelt(s,1), &strelt(b,1), nb) == 0) {
    t = copy_string(b);
    s->nbefore += nb;
    s->nchar -= nb;
  } else {
    t = null_string();
  }
  *a = s;
  retire(hs);
  return t;
} /* end l_match */

/*
 *  OP MAX(a,b);
 *    CASE OF
 *    (IS_INTEGER a AND IS_INTEGER b):     $ integer maximum
 *      CASE OF
 *      (a>=b):  RETURN a;
 *      (a<=b):  RETURN b;
 *      END CASE;
 *    (IS_REAL a AND IS_REAL b):           $ real maximum
 *      CASE OF
 *      (a>=b):  RETURN a;
 *      (a<=b):  RETURN b;
 *      END CASE;
 *    ELSE ERROR;
 *    END CASE;
 *  END OP MAX;
 *
 *  Extension:  now also handles mixed-mode arithmetic, strings, and
 *  tuples.  The INTEGER is converted to REAL for comparison in mixed
 *  mode cases, but the winning number is returned in its original type.
 *  The left operand is returned in case of a tie.  If either number is
 *  NaN, the result is NaN, in contrast to POSIX/ISO C fmax().  (There
 *  are better ways to represent "placeholders" in SETL tuples over
 *  which you wish to apply MAX/.)
 */
block *l_max(block *a, block *b) {
  int c = compare_values(a,b,"MAX");
  switch (c) {
  case A_GT_B:
  case A_EQ_B:
  case A_IS_NAN:
    return copy_value(a);
  case A_LT_B:
  case B_IS_NAN:
    return copy_value(b);
  default:
    unexpected(c);
  }
} /* end l_max */

/*
 *  The MEM_* routines below have been removed from the lib doc for now
 *  as they would be better offered as a customization package, having
 *  no known use except with certain other cust pax.
 */

#if SIZEOF_LONG < SIZEOF_CHAR_P
#warning C long too small to hold char pointer - MEM_* ops may not work.
#endif

typedef union {
  ulong addr;  /* "address" */
  void *ptr;   /* "pointer" */
} mem_conv_union;  /* to avoid nasty casts in the "mem" routines... */

/*
 *  MEM_ALLOC(n)  -- allocate n bytes and return the address
 */
integer *l_mem_alloc(integer *a) {
  size_t nbytes;
  mem_conv_union p;
  nbytes = get_ulong(a,"arg to MEM_ALLOC");
  if (restricted) runerr("MEM_ALLOC is restricted");
  p.ptr = os_malloc(nbytes);  /* succeeds or abends */
  return ulong_integer(p.addr);
} /* end l_mem_alloc */

/*
 *  MEM_FREE(a);  -- release memory gotten from MEM_ALLOC or MEM_REALLOC
 */
void l_mem_free(integer *a) {
  mem_conv_union p;
  p.addr = get_ulong(a,"arg to MEM_FREE");
  if (restricted) runerr("MEM_FREE is restricted");
  os_free(p.ptr);  /* does not modify errno */
} /* end l_mem_free */

/*
 *  MEM_REALLOC(a, n)  -- resize memory area and return new address
 */
integer *l_mem_realloc(integer *a, integer *b) {
  mem_conv_union p, r;
  size_t nbytes;
  p.addr = get_ulong(a,"first (address) arg to MEM_REALLOC");
  nbytes = get_ulong(b,"second (length) arg to MEM_REALLOC");
  if (restricted) runerr("MEM_REALLOC is restricted");
  r.ptr = os_realloc(p.ptr, nbytes);  /* succeeds or abends */
  return ulong_integer(r.addr);
} /* end l_mem_realloc */

/*
 *  MEM_COPY(dst, src, n);  -- memory-to-memory copy of n bytes
 */
void l_mem_copy(integer *a, integer *b, integer *c) {
  mem_conv_union src, dst;
  size_t nbytes;
  dst.addr = get_ulong(a,"first (destination address) arg to MEM_COPY");
  src.addr = get_ulong(b,"second (source address) arg to MEM_COPY");
  nbytes = get_ulong(c,"third (length) arg to MEM_COPY");
  if (restricted) runerr("MEM_COPY is restricted");
  mvmem (dst.ptr, src.ptr, nbytes);
} /* end l_mem_copy */

typedef union {
  ulong addr;  /* "address" */
  char *ptr;   /* "pointer" */
} mem_conv_str_union;  /* like mem_conv_union, but for char strings */

/*
 *  MEM_FETCH_STRING(src, n)  -- get n bytes from memory as a STRING
 */
string *l_mem_fetch_string(integer *a, integer *b) {
  mem_conv_str_union p;
  long n;
  p.addr = get_ulong(a,"first (address) arg to MEM_FETCH_STRING");
  n = get_nat_long(b,"second (length) arg to MEM_FETCH_STRING");
  if (restricted) runerr("MEM_FETCH_STRING is restricted");
  return new_nstring(p.ptr, n);
} /* end l_mem_fetch_string */

/*
 *  MEM_FETCH_C_STRING(src)  -- get memory bytes before \0 as a STRING
 */
string *l_mem_fetch_c_string(integer *a) {
  mem_conv_str_union p;
  long n;
  p.addr = get_ulong(a,"arg to MEM_FETCH_C_STRING (address)");
  if (restricted) runerr("MEM_FETCH_C_STRING is restricted");
  n = (long) strlen(p.ptr);
  return new_nstring(p.ptr, n);
} /* end l_mem_fetch_c_string */

/*
 *  MEM_STORE_STRING does not append a NUL, but MEM_STORE_C_STRING does.
 *  Also, MEM_STORE_STRING stores the number of chars that are actually
 *  in the string, while MEM_STORE_C_STRING stops at the first NUL.
 */

/*
 *  MEM_STORE_STRING(a, dst);  -- copy STRING content to memory
 */
void l_mem_store_string(string *a, integer *b) {
  mem_conv_str_union p;
  long n;
  if (!is_string(a)) {
    binary_proc_error ("MEM_STORE_STRING", a, b, "(STRING, INTEGER)");
  }
  p.addr = get_ulong(b,"second (address) arg to MEM_STORE_STRING");
  if (restricted) runerr("MEM_STORE_STRING is restricted");
  n = a->nchar;
  mvmem (p.ptr, &strelt(a,1), n);
} /* end l_mem_store_string */

/*
 *  MEM_STORE_C_STRING(a, dst);  -- copy bytes before \0 to memory
 */
void l_mem_store_c_string(string *a, integer *b) {
  mem_conv_str_union p;
  if (!is_string(a)) {
    binary_proc_error ("MEM_STORE_C_STRING", a, b, "(STRING, INTEGER)");
  }
  p.addr = get_ulong(b,"second (address) arg to MEM_STORE_C_STRING");
  if (restricted) runerr("MEM_STORE_C_STRING is restricted");
  strcpy (p.ptr, &strelt(a,1));
} /* end l_mem_store_c_string */

/*
 *  OP MIN(a,b);
 *    CASE OF
 *    (IS_INTEGER a AND IS_INTEGER b):     $ integer minimum
 *      CASE OF
 *      (a<=b):  RETURN a;
 *      (a>=b):  RETURN b;
 *      END CASE;
 *    (IS_REAL a AND IS_REAL b):           $ real minimum
 *      CASE OF
 *      (a<=b):  RETURN a;
 *      (a>=b):  RETURN b;
 *      END CASE;
 *    ELSE ERROR;
 *    END CASE;
 *  END OP MIN;
 *
 *  Extension:  now also handles mixed-mode arithmetic, strings, and
 *  tuples.  The INTEGER is converted to REAL for comparison in mixed
 *  mode cases, but the winning number is returned in its original type.
 *  The left operand is returned in case of a tie.  If either number is
 *  NaN, the result is NaN, in contrast to POSIX/ISO C fmin().  (There
 *  are better ways to represent "placeholders" in SETL tuples over
 *  which you wish to apply MIN/.)
 */
block *l_min(block *a, block *b) {
  int c = compare_values(a,b,"MIN");
  switch (c) {
  case A_LT_B:
  case A_EQ_B:
  case A_IS_NAN:
    return copy_value(a);
  case A_GT_B:
  case B_IS_NAN:
    return copy_value(b);
  default:
    unexpected(c);
  }
} /* end l_min */

/*
 *  PROC MKSTEMP (RW template);
 *
 *  Creates a unique temporary file based on the STRING 'template',
 *  which must end in the characters 'XXXXXX' (which are replaced),
 *  opens that file for direct access, and returns a file descriptor.
 *
 *  If the underlying POSIX mkstemp() fails, MKSTEMP returns OM and
 *  the template is not modified; errno is set in that case.
 */
integer *l_mkstemp(string **a) {
  int fd;
  if (!is_string(*a)) {
    unary_proc_error ("MKSTEMP", *a, "STRING");
  }
  if (restricted) {
    /* This is a case-sensitive check even though the underlying
     * filesystem policy might be case-insensitive; the pattern
     * specified by the caller must match an --allow-mkstemp arg
     * exactly:  */
    const char *templat = &strelt(*a,1);
    if (!is_allowed(allowed_mkstemp_list, templat)) {
      runerr("MKSTEMP on template \"%s\" is restricted",
                                tame(templat));
    }
  }
  fd = file_mkstemp(a);  /* may set errno */
  return fd >= 0 ? new_integer(fd) : OM;
} /* end l_mkstemp */

/*
 *  OP MOD(a,b);
 *    CASE OF
 *    (IS_INTEGER a AND IS_INTEGER b):     $ integer modulus
 *      RETURN a - b * (a DIV b);
 *    ELSE ERROR;
 *    END CASE;
 *  END OP MOD;
 *
 *  Note that the above prototype is incorrect for negative
 *  second operands.  GNU SETL does it right:  see DIV for
 *  discussion of DIV, REM, and MOD for INTEGERs.
 *
 *  Moreover, MOD can now apply also to sets, and means symmetric
 *  difference, i.e., union minus intersection, a commutative
 *  meaning introduced by the redoubtable SETL2.
 */
block *l_mod(block *a, block *b) {
  if        (is_integer(a) &&
             is_integer(b)) {
    integer *ia = (integer *)a;
    integer *ib = (integer *)b;
    if (ib->size == 0) {
      runerr("Second operand of MOD is 0");
    }
    return (block *)integer_mod(ia, ib);
  } else if (is_set(a) &&
             is_set(b)) {
    set *sa = (set *)a;  HANDLE ha = ref(sa);
    set *sb = (set *)b;  HANDLE hb = ref(sb);
    set *su = set_union(sa,sb);  HANDLE hu = ref(su);
    set *si = set_intersection(sa,sb);
    set *r = set_difference(su,si);
    retire(hu);
    retire(hb);
    retire(ha);
    return (block *)r;
  }
  binary_op_error ("MOD", a, b, "INTEGER MOD INTEGER or SET MOD SET");
} /* end l_mod */

/* NARGS is special-cased in the interpreter loop.  */

/*
 *  NEWAT  -- new ATOM, a bumbly little counter posing as an abstraction
 */
atom *l_newat(void) {
  return new_atom(newat());
} /* end l_newat */

/*
 *  OP NOT(a);                       $ logical not
 *    CASE OF
 *    (a=TRUE):
 *      RETURN FALSE;
 *    (a=FALSE):
 *      RETURN TRUE;
 *    ELSE ERROR;
 *    END CASE;
 *  END OP NOT;
 */
boolean *l_not(boolean *a) {
  if (!is_boolean(a)) {
    unary_op_error ("NOT", a, "BOOLEAN");
  }
  return new_boolean(!a->booval);
} /* end l_not */

/*
 *  NOTANY(RW a, b)  -- if first char of a is not in charset b,
 *                      remove and return it, else ''
 */
string *l_notany(string **a, string *b) {
  string *s = *a;     HANDLE hs = ref(s);
  string *t;
  if (!is_string(s) ||
      !is_string(b)) {
    binary_proc_error ("NOTANY", s, b, "(STRING, STRING)");
  }
  if (s->nchar > 0) {
    char c = strelt(s,1);
    long j;
    for (j=1; j<=b->nchar; j++) {
      if (c == strelt(b,j)) {
        t = null_string();
        goto done;
      }
    }
    s->nbefore++;
    s->nchar--;
    t = new_cstring(c);
  } else {
    t = null_string();
  }
done:
  *a = s;
  retire(hs);
  return t;
} /* end l_notany */

/*
 *  OP NOTIN(a,b);
 *    CASE OF
 *    (IS_SET b):                          $ test element not in set
 *      RETURN NOT a IN b;  -- note the low precedence of NOT
 *    (IS_STRING a AND IS_STRING b):       $ test character not in string
 *      RETURN NOT a IN b;
 *    (IS_TUPLE b):                        $ test element not in tuple
 *      RETURN NOT a IN b;
 *    ELSE ERROR;
 *    END CASE;
 *  END OP NOTIN;
 *
 *  Extension for strings:  the first operand can now be a string of any
 *  length, and the test is for it not being a substring of the second.
 *
 *  Technically, it is also an extension to allow "OM NOTIN s" tests.
 *  For sets, it is defined here as TRUE.
 */
boolean *l_notin(block *a, block *b) {
  if (is_set(b)) {
    if (is_om(a)) return new_boolean(true);
    return new_boolean(!in_set((set *)b, a));
  } else if (is_string(b)) {
    string *p;  long pn;
    string *s;  long sn;
    long i,n;
    if (!is_string(a)) {
      binary_op_error ("NOTIN", a, b,
       "STRING as first operand when second operand is STRING");
    }
    p = (string *)a;  pn = p->nchar;
    s = (string *)b;  sn = s->nchar;
    n = sn - pn + 1;
    for (i=1; i<=n; i++) {
      if (memcmp(&strelt(p,1),&strelt(s,i),pn) == 0) {
        return new_boolean(false);
      }
    }
    return new_boolean(true);
  } else if (is_tuple(b)) {
    tuple *t = (tuple *)b;
    long i, n = t->nelt;
    for (i=1; i<=n; i++) {
      if (equal_value(a,tupelt(t,i))) return new_boolean(false);
    }
    return new_boolean(true);
  }
  binary_op_error ("NOTIN", a, b,
                    "STRING, SET, or TUPLE as second operand");
} /* end l_notin */

/*
 *  OP NPOW(a,b);
 *    CASE OF
 *    (IS_INTEGER a AND IS_SET b):         $ subsets of given size
 *      IF a<0 THEN ERROR; END;
 *      RETURN {x IN POW b | #x = a};
 *    (IS_SET a AND IS_INTEGER b):         $ subsets of given size
 *      IF b<0 THEN ERROR; END;
 *      RETURN {x IN POW a | #x = b};
 *    ELSE ERROR;
 *    END CASE;
 *  END OP NPOW;
 */
set *l_npow(block *a, block *b) {
  if (is_integer(a) && is_set(b)) {
    long *d;  /* 0th element of this array is unused */
    const long k = get_long_in((integer *)a, 0, LONG_MAX/sizeof d[0] - 1,
                               "integer operand of NPOW");
    const size_t dsize = (k+1) * sizeof d[0];
    set *sb = (set *)b;  HANDLE hb;
    const long n = sb->card;
    const long L = n - k;
    long i, j;
    tuple *s;
    HANDLE hs;
    set *r;  HANDLE hr;
    if (k > n) return null_set();
    /* Copy set b to tuple s for efficiency:  */
    hb = ref(sb);
    demote (sb, plain_set);
    s = new_tuple(n);
    hs = ref(s);
    for (i=1; i<=n; i++) {
      subnode *c;
      check (ordsee(sb->tblval, i, &c));
      let (tupelt(s,i), unkey(c->k));
    }
    r = null_set();  hr = ref(r);
    d = (long *) arena_alloc(dsize);
    /* The k used elements of d are a strictly monotonic sequence of
     * indices into the tuple s:  */
    for (i=1; i<=k; i++) d[i] = i;
    for (;;) {
      /* Form a subset of b of size k by selecting k elements of s:  */
      set *t = null_set();
      for (i=1; i<=k; i++) {
        block *v = tupelt(s, d[i]);  /* element d[i] of s */
        set_insert(&t, v);
      }
      set_insert(&r, (block *)t);  /* add that subset to the result */
      if (k == 0) goto out_there;
      j = k;
      while (++d[j] > L+j) if (--j == 0) goto out_there;
      for (i=j; ++i<=k; ) d[i] = d[j] + (i-j);
    }
out_there:
    arena_free(d, dsize);
    retire(hr);
    retire(hs);
    retire(hb);
    return r;
  } else if (is_set(a) && is_integer(b)) {
    return l_npow(b,a);
  }
  binary_op_error ("NPOW", a, b, "INTEGER NPOW SET or SET NPOW INTEGER");
} /* end l_npow */

/*
 *  NPRINT(a(*));  -- like PRINT, but omit trailing newline
 */
void l_nprint(tuple *a, long nargs) {
  HANDLE ha = ref(a);
  file *f;
  check_optargs_type(a,"NPRINT");
  check (fd_stdout == getfd ((block *)stdout_integer, getfd_write));
  f = find_fd_file(fd_stdout);
  print_values (f, a, nargs);  /* may set errno */
  retire(ha);
} /* end l_nprint */

/*
 *  NPRINTA(a(*));  -- like PRINTA, but omit trailing newline
 */
void l_nprinta(block *a, tuple *b, long nargs) {
  HANDLE hb = ref(b);
  int fd;
  file *f;
  if (!is_stream_id(a)) {
    unary_variadic_error ("NPRINTA", a, STREAM_ID);
  }
  check_optargs_type(b,"NPRINTA");
  fd = getfd (a, getfd_write);
  f = find_fd_file(fd);
  print_values (f, b, nargs-1);  /* may set errno */
  retire(hb);
} /* end l_nprinta */

/*
 *  OP ODD(a);
 *    CASE OF
 *    (IS_INTEGER a):                      $ test odd integer
 *      RETURN a MOD 2 /= 0;
 *    ELSE ERROR;
 *    END CASE;
 *  END OP ODD;
 */
boolean *l_odd(integer *a) {
  if (is_integer(a)) {
    ro_mpz(z, a)  /* const mpz_t z = alias of INTEGER a */
    return new_boolean (mpz_odd_p(z) != 0);
  } else {
    unary_op_error ("ODD", a, "INTEGER");
  }
} /* end l_odd */

/*
 *  PROC OPEN(a,how);
 *    fd := ... integer file descriptor or OM;
 *    RETURN fd;
 *  END PROC OPEN;
 *
 *  See the setl.org SETL lib doc and the extensive comments before
 *  'file_open' in sys.c for the full story on what OPEN can do.
 */
integer *l_open(block *a, string *b) {
  integer *r;
  const char *mode_name;
  const open_mode *mode;
  int fd;
  if (!(is_string(a) || is_integer(a) || is_tuple(a)) ||
       !is_string(b)) {
    binary_proc_error ("OPEN", a, b,
                       "(STRING or INTEGER or TUPLE, STRING)");
  }
  mode_name = &strelt(b,1);
  mode = lookup_open_mode(mode_name);
  if (!mode) {
    runerr("Unrecognized mode (second arg to OPEN) \"%s\"",
                                                 tame(mode_name));
  }
  fd = file_open(a,mode->how,restricted);  /* may set errno */
  r = (fd >= 0) ? new_integer(fd) : OM;
  return r;
} /* end l_open */

/*
 *  The GNU SETL translator does not emit code that calls this
 *  (because of the short-circuiting), but here it is anyway in
 *  case something else wants the non-short-circuiting version.
 *
 *  OP OR(a,b);                     $ logical or
 *    CASE OF
 *    (a = TRUE):
 *      $ skip evaluation of b and
 *      RETURN TRUE;
 *    (a = FALSE):
 *      $ evaluate b and do
 *      CASE OF
 *      (b = TRUE):
 *        RETURN TRUE;
 *      (b = FALSE):
 *        RETURN FALSE;
 *      ELSE ERROR;
 *      END CASE;
 *    ELSE ERROR;
 *    END CASE;
 *  END OP OR;
 */
boolean *l_or(boolean *a, boolean *b) {
  if (is_boolean(a) && is_boolean(b)) {
    return new_boolean(a->booval || b->booval);
  }
  binary_op_error ("OR", a, b, "BOOLEAN operands");
} /* end l_or */

/*
 *  Unary operators to pack C number reprs into STRINGs.
 *
 *  These take only INTEGER operands, and yield a STRING whose length
 *  is determined by the C ABI of the GNU SETL build of the current
 *  interpreter (SETL VM):
 *
 *   PACK_SHORT a
 *   PACK_UNSIGNED_SHORT a
 *   PACK_INT a
 *   PACK_UNSIGNED_INT a
 *   PACK_LONG a
 *   PACK_UNSIGNED_LONG a
 *   PACK_LONG_LONG a
 *   PACK_UNSIGNED_LONG_LONG a
 *
 *   PACK_INTEGER a  -- unbounded
 *
 *  These take only REAL operands:
 *
 *   PACK_DOUBLE a
 *   PACK_FLOAT a
 *
 *   PACK_REAL a
 *
 *  Note that the PACK_... and UNPACK_... operators are currently
 *  "undocumented" in the lib doc (setl-lib.texi).
 */

string *l_pack_short(integer *a) {
  short i = get_short(a, "PACK_SHORT operand");
  return new_nstring((const char *)&i, sizeof i);
} /* end l_pack_short */

string *l_pack_unsigned_short(integer *a) {
  ushort i = get_ushort(a, "PACK_UNSIGNED_SHORT operand");
  return new_nstring((const char *)&i, sizeof i);
} /* end l_pack_ushort */

string *l_pack_int(integer *a) {
  int i = get_int(a, "PACK_INT operand");
  return new_nstring((const char *)&i, sizeof i);
} /* end l_pack_int */

string *l_pack_unsigned_int(integer *a) {
  uint i = get_uint(a, "PACK_UNSIGNED_INT operand");
  return new_nstring((const char *)&i, sizeof i);
} /* end l_pack_unsigned_int */

string *l_pack_long(integer *a) {
  long i = get_long(a, "PACK_LONG operand");
  return new_nstring((const char *)&i, sizeof i);
} /* end l_pack_long */

string *l_pack_unsigned_long(integer *a) {
  ulong i = get_ulong(a, "PACK_UNSIGNED_LONG operand");
  return new_nstring((const char *)&i, sizeof i);
} /* end l_pack_unsigned_long */

string *l_pack_long_long(integer *a) {
#if HAVE_LLONG
  llong i = get_llong(a, "PACK_LONG_LONG operand");
  return new_nstring((const char *)&i, sizeof i);
#else
  runerr("PACK_LONG_LONG not available on this platform");
#endif
} /* end l_pack_long_long */

string *l_pack_unsigned_long_long(integer *a) {
#if HAVE_LLONG
  ullong i = get_ullong(a, "PACK_UNSIGNED_LONG_LONG operand");
  return new_nstring((const char *)&i, sizeof i);
#else
  runerr("PACK_UNSIGNED_LONG_LONG not available on this platform");
#endif
} /* end l_pack_unsigned_long_long */

/*
 *  PACK_INTEGER's result is a ssize_t size field whose abs val is n,
 *  followed by n limbs in little-endian order.  The order of bytes
 *  within the size field and within each limb is that of the host
 *  architecture.  If limbs have GMP "nails" (unlikely, but see
 *  GMP_NAIL_BITS in <gmp.h> or "gmp.h" as appropriate), they will
 *  show up in the packed integer too.
 */
string *l_pack_integer(integer *a) {
  HANDLE ha = ref(a);
  const ssize_t size = a->size;
  const size_t n = sizeof_limbs(a);  /* #bytes in the limbs */
  const long limbs_offset = sizeof size;
  string *r = new_estring(limbs_offset + n);
  mvmem (&strelt(r, 1), &size, limbs_offset);
  mvmem (&strelt(r, 1 + limbs_offset), a->limbs, n);
  retire(ha);
  return r;
} /* end l_pack_integer */

/*
 *  This is currently equivalent to PACK_REAL, but get_double() will
 *  need to do more work to extract a double if REALs acquire a more
 *  general representation:
 */
string *l_pack_double(real *a) {
  double x = get_double(a,"PACK_DOUBLE operand");
  return new_nstring((const char *)&x, sizeof x);
} /* end l_pack_double */

/*
 *  This loses range and precision but can be useful for producing
 *  files in funky old "binary" formats:
 */
string *l_pack_float(real *a) {
  double d = get_double(a,"PACK_FLOAT operand");
  float x = d;  /* range and precision can be lost here */
  return new_nstring((const char *)&x, sizeof x);
} /* end l_pack_float */

/*
 *  This is currently equivalent to PACK_DOUBLE, but will become
 *  capable of producing more general results if REALs acquire a
 *  more general representation:
 */
string *l_pack_real(real *a) {
  double x = get_double(a,"PACK_REAL operand");
  return new_nstring((const char *)&x, sizeof x);
} /* end l_pack_real */

/*
 *  PEEKC a  -- get next char from stream a but leave it in the input
 */
string *l_peekc(block *a) {
  int fd;
  file *f;
  if (!is_stream_id(a)) {
    unary_op_error ("PEEKC", a, STREAM_ID);
  }
  fd = getfd (a, getfd_read);
  f = find_file(fd);
  return peek_one_char(f);  /* may set errno */
} /* end l_peekc */

/*
 *  PEEKCHAR  -- PEEKC STDIN
 */
string *l_peekchar(void) {
  file *f;
  check (fd_stdin == getfd ((block *)stdin_integer, getfd_read));
  f = find_fd_file(fd_stdin);
  return peek_one_char(f);  /* may set errno */
} /* end l_peekchar */

/*
 *  PEER_ADDRESS returns the IP address of the peer on the connected
 *  socket (client or ACCEPT-created server-side socket).
 *
 *  It sets LAST_ERROR (errno) and returns OM if getpeername()
 *  either fails or gives a peer that is not in address family
 *  AF_INET or AF_INET6.
 */
string *l_peer_address(block *a) {
  int fd;
  struct sockaddr_storage ss;
  struct sockaddr *sa = (struct sockaddr *)&ss;
  socklen_t salen = sizeof ss;
  if (!is_stream_id(a)) {
    unary_op_error ("PEER_ADDRESS", a, STREAM_ID);
  }
  fd = getfd (a, getfd_sockaddr);
  if (os_getpeername(fd, sa, &salen)) {
    if (salen >= sizeof sa->sa_family &&
        (sa->sa_family == AF_INET ||
         sa->sa_family == AF_INET6)) {
      return sockaddr_address(sa, salen);  /* no change to errno */
    } else {
      errno = EAFNOSUPPORT;  /* maybe a Unix-domain socket */
      return OM;
    }
  } else {
    return OM;  /* with errno set by getpeername() */
  }
} /* end l_peer_address */

/*
 *  PEER_NAME returns the hostname of the peer on the connected
 *  socket (client or ACCEPT-created server-side socket).
 *
 *  It sets LAST_ERROR (errno) and returns OM if getpeername()
 *  either fails or gives a peer that is not in address family
 *  AF_INET or AF_INET6, or if getnameinfo() fails on the address
 *  produced by getpeername().
 *
 *  Peer hostnames are not cached locally, and getnameinfo() may
 *  contact a nameserver, causing possible delays.
 */
string *l_peer_name(block *a) {
  int fd;
  struct sockaddr_storage ss;
  struct sockaddr *sa = (struct sockaddr *)&ss;
  socklen_t salen = sizeof ss;
  if (!is_stream_id(a)) {
    unary_op_error ("PEER_NAME", a, STREAM_ID);
  }
  fd = getfd (a, getfd_sockaddr);
  if (os_getpeername(fd, sa, &salen)) {
    if (salen >= sizeof sa->sa_family &&
        (sa->sa_family == AF_INET ||
         sa->sa_family == AF_INET6)) {
      return sockaddr_name(sa, salen);  /* may set errno and give OM */
    } else {
      errno = EAFNOSUPPORT;  /* maybe a Unix-domain socket */
      return OM;
    }
  } else {
    return OM;  /* with errno set by getpeername() */
  }
} /* end l_peer_name */

/*
 *  PEER_PORT returns the port number of the peer on the client
 *  socket or ACCEPT-created server-side socket.  For the latter,
 *  the result is usually the client's "ephemeral" port number.
 *
 *  Same story as PEER_ADDRESS on the error cases.
 */
integer *l_peer_port(block *a) {
  int fd;
  struct sockaddr_storage ss;
  struct sockaddr *sa = (struct sockaddr *)&ss;
  socklen_t salen = sizeof ss;
  if (!is_stream_id(a)) {
    unary_op_error ("PEER_PORT", a, STREAM_ID);
  }
  fd = getfd (a, getfd_sockaddr);
  if (os_getpeername(fd, sa, &salen)) {
    if (salen >= sizeof sa->sa_family &&
        (sa->sa_family == AF_INET ||
         sa->sa_family == AF_INET6)) {
      return sockaddr_portnum(sa, salen);  /* no change to errno */
    } else {
      errno = EAFNOSUPPORT;  /* maybe a Unix-domain socket */
      return OM;
    }
  } else {
    return OM;  /* with errno set by getpeername() */
  }
} /* end l_peer_port */

/*
 *  PEER_SOCKADDR a means [PEER_ADDRESS a, PEER_PORT a],
 *  or OM (with errno set) if getpeername() fails.
 */
tuple *l_peer_sockaddr(block *a) {
  int fd;
  struct sockaddr_storage ss;
  struct sockaddr *sa = (struct sockaddr *)&ss;
  socklen_t salen = sizeof ss;
  if (!is_stream_id(a)) {
    unary_op_error ("PEER_SOCKADDR", a, STREAM_ID);
  }
  fd = getfd (a, getfd_sockaddr);
  if (os_getpeername(fd, sa, &salen)) {
    if (salen >= sizeof sa->sa_family &&
        (sa->sa_family == AF_INET ||
         sa->sa_family == AF_INET6)) {
      return sockaddr_tuple(sa, salen);  /* no change to errno */
    } else {
      errno = EAFNOSUPPORT;  /* maybe a Unix-domain socket */
      return OM;
    }
  } else {
    return OM;  /* with errno set by getpeername() */
  }
} /* end l_peer_sockaddr */

/*
 *  PEXISTS pid  -- process with process ID 'pid' exists
 */
boolean *l_pexists(integer *a) {
  pid_t pid;
  int saved_errno;
  int rc;
  bool r;
  if (restricted) runerr("PEXISTS is restricted");
  pid = get_pid_t(a, "PEXISTS operand (e.g. pid or -pgid)");
  saved_errno = errno;  /* don't let PEXISTS disturb errno */
  rc = os_kill(pid, 0);  /* signal 0 means just interrogate */
  r = rc != -1 || errno != ESRCH;  /* false only on ESRCH error */
  errno = saved_errno;  /* restore errno in case kill() failed */
  return new_boolean(r);  /* rc 0 from os_kill(), or non-ESRCH error */
} /* end l_pexists */

/*
 *  PID  -- means getpid(); now deprecated and removed from lib doc
 *
 *  PID(x)  -- means pid of pipe or pump
 */
integer *l_pid(tuple *a, long nargs) {
  check_optargs_type(a,"PID");
  if (nargs == 0) {
    return new_integer(os_getpid());
  } else if (nargs == 1) {
    int fd;
    file *f;
    block *x = a->nelt == 0 ? OM : tupelt(a,1);
    if (!is_stream_id(x)) {
      /* unary_proc_error ("PID", x, STREAM_ID " or no"); */
      unary_proc_error ("PID", x, STREAM_ID);
    }
    /*
     *  Similarly as in l_flush(), it would be possible to have a
     *  specialized getfd_getpid and thus tighten up the find_file()
     *  to find_fd_file() and even already know that the ftype was
     *  acceptable.  But we are just using the more lenient getfd_info
     *  and doing the extra checking here.
     */
    fd = getfd (x, getfd_info);
    f = find_file(fd);
    if (f->ftype != pipe_file &&
        f->ftype != tty_pipe_file) {
      runerr("PID arg must be a pipe, pump, or tty-pump stream");
    }
    /*
     *  For a stream that was created by opening an existing fd as a
     *  pipe, pump, or tty-pump stream, the returned pid will rather
     *  arbitrarily be -1 here.
     */
    return new_integer(f->pid);  /* now that we know the ftype is OK */
  } else {
    /* runerr("PID takes at most 1 (stream id) arg, not %ld args", n); */
    runerr("PID takes 1 arg (a stream id), not %ld args", nargs);
  }
} /* end l_pid */

/*
 *  PIPE
 *
 *  A synonym for SOCKETPAIR suitable for use when you intend to do only
 *  reading on the first fd and only writing on the second fd of the
 *  returned pair.  (In fact, if os_pipe() is implemented using POSIX
 *  pipe(), the fds may or may not be bidirectional.  In Linux pipe(),
 *  they are not.  But our os_pipe() tries for a socketpair first.)
 */
tuple *l_pipe(void) {
  int fds[2];
  integer *fd1;  HANDLE h1;
  integer *fd2;  HANDLE h2;
  tuple *r;
  if (restricted) runerr("PIPE is restricted");
  if (os_pipe(fds) < 0) return OM;  /* with errno set */
  /* Like in l_dup(), we do not check against existing SETL buffers,
   * to help support imaginary opt pak tricks involving fd subst.  */
  fd1 = new_integer(fds[0]);  h1 = ref(fd1);
  fd2 = new_integer(fds[1]);  h2 = ref(fd2);
  r = new_tuple(2);
  tupelt(r,1) = (block *)fd1;
  tupelt(r,2) = (block *)fd2;
  retire(h2);
  retire(h1);
  return r;
} /* l_pipe */

/*
 *  PIPE_FROM_CHILD  -- spawn child, give readable stream from its STDOUT
 */
integer *l_pipe_from_child(void) {
  int fd = local_pump(can_read);  /* may abend, not set errno */
  return new_integer(fd);
} /* end l_pipe_from_child */

/*
 *  PIPE_TO_CHILD  -- spawn child, give writable stream to its STDIN
 */
integer *l_pipe_to_child(void) {
  int fd = local_pump(can_write);  /* may abend, not set errno */
  return new_integer(fd);
} /* end l_pipe_to_child */

/*
 *  PORT a  -- local port number of network socket a
 *
 *  If getsockname() fails, or gives a sockaddr that isn't in
 *  address family AF_INET or AF_INET6, PORT sets LAST_ERROR (errno)
 *  and returns OM.
 */
integer *l_port(block *a) {
  int fd;
  struct sockaddr_storage ss;
  struct sockaddr *sa = (struct sockaddr *)&ss;
  socklen_t salen = sizeof ss;
  if (!is_stream_id(a)) {
    unary_op_error ("PORT", a, STREAM_ID);
  }
  fd = getfd (a, getfd_sockaddr);
  if (os_getsockname(fd, sa, &salen)) {
    if (salen >= sizeof sa->sa_family &&
        (sa->sa_family == AF_INET ||
         sa->sa_family == AF_INET6)) {
      return sockaddr_portnum(sa, salen);  /* no change to errno */
    } else {
      errno = EAFNOSUPPORT;  /* maybe a Unix-domain socket */
      return OM;
    }
  } else {
    return OM;  /* with errno set by getsockname() */
  }
} /* end l_port */

/*
 *  OP POW(a);
 *    CASE OF
 *    (IS_SET a):                          $ power set of set
 *      IF a = {} THEN
 *        RETURN {{}};
 *      ELSE
 *        x := ARB a;
 *        y := POW (a LESS x);
 *        RETURN y + {z WITH x : z IN y};
 *      END;
 *    ELSE ERROR;
 *    END CASE;
 *  END OP POW;
 */
set *l_pow(set *a) {
  HANDLE ha = ref(a);
  set *r;  HANDLE hr;
  tuple *s;  HANDLE hs;
  long i,j,m,n;
  if (!is_set(a)) {
    unary_op_error ("POW", a, "SET");
  }
  demote (a, plain_set);
  r = null_set();  hr = ref(r);
  m = a->card;
  /* Copy set 'a' to tuple 's' for efficiency:  */
  s = new_tuple(m);  hs = ref(s);
  for (j=1; j<=m; j++) {
    subnode *c;
    check (ordsee(a->tblval, j, &c));
    let (tupelt(s,j), unkey(c->k));
  }
  n = 1 << m;   /* i.e., 2**m */
  for (i=1; i<=n; i++) {
    unsigned long k = i-1;
    set *t = null_set();
    for (j=1; j<=m; j++) {
      if (k & 1) {  /* a 1 in the j'th "bit position" of i-1 */
        block *v = tupelt(s,j);
        set_insert(&t, v);
      }
      k >>= 1;
    }
    assert(k==0);
    set_insert(&r, (block *)t);
  }
  retire(hs);
  retire(hr);
  retire(ha);
  return r;
} /* end l_pow */

/*
 *  OP PRETTY leaves the 95 characters that ASCII considers
 *  "printable" alone except for the apostrophe (single quote),
 *  which becomes two apostrophes in a row, and the backslash,
 *  which becomes two backslashes in a row.  An apostrophe is
 *  also added at each end.  Among the other codes, the
 *  audible alarm (ctrl-G), backspace, formfeed, newline,
 *  carriage return, horizontal tab, and vertical tab are
 *  converted to \a, \b, \f, \n, \r, \t, and \v respectively
 *  (these are the same as the C conventions), and all
 *  remaining codes are converted to \nnn, where the n are
 *  octal digits.
 *
 *  EBCDIC provides a character that can be used for each of the
 *  95 printable ASCII codes, so if this C source is treated with
 *  appropriate caution (be particularly careful with tilde!), this
 *  ASCII-specific definition should work properly even on your
 *  EBCDIC system.
 *
 *  Extension:  a non-STRING arg is converted as if by STR before
 *  being prettified.
 */
string *l_pretty(block *a) {
  return str_dress (is_string(a) ? (string *)a : tostr(a));
} /* end l_pretty */

/*
 *  PRINT(a(*));  -- "print" values on STDOUT, followed by \n
 */
void l_print(tuple *a, long nargs) {
  HANDLE ha = ref(a);
  file *f;
  check_optargs_type(a,"PRINT");
  check (fd_stdout == getfd ((block *)stdout_integer, getfd_write));
  f = find_fd_file(fd_stdout);
  /* print_values() or put_char() may set errno */
  if (print_values (f, a, nargs)) put_char ('\n', f->buffer);
  retire(ha);
} /* end l_print */

/*
 *  PRINTA(a, b(*));  -- "print" values on stream a, followed by \n
 */
void l_printa(block *a, tuple *b, long nargs) {
  HANDLE hb = ref(b);
  int fd;
  file *f;
  if (!is_stream_id(a)) {
    unary_variadic_error ("PRINTA", a, STREAM_ID);
  }
  check_optargs_type(b,"PRINTA");
  fd = getfd (a, getfd_write);
  f = find_fd_file(fd);
  /* print_values() or put_char() may set errno */
  if (print_values (f, b, nargs-1)) put_char ('\n', f->buffer);
  retire(hb);
} /* end l_printa */

/*
 *  PUMP  -- spawn child, give stream connected to its STDIN and STDOUT
 */
integer *l_pump(void) {
  int fd = local_pump(duplex);  /* may abend, not set errno */
  return new_integer(fd);  /* bidirectional stream */
} /* end l_pump */

/*
 *  PUT and PUTA have signatures like PRINT and PRINTA, as in SETL2.
 *  The old (SDDS 1986) signature of PUT was a misfit, and has been
 *  more appropriately pinned on PUTA.
 *
 *  GET/GETA have similarly been made consistent with READ/READA.
 *
 *  See put_lines() for fine points of PUT, PUTA, and PUTLINE,
 *      put_values() for PUTB, WRITE, and WRITEA,
 *      print_values() for NPRINT, NPRINTA, PRINT, and PRINTA, and
 *      put_chars() (in util.c) for PUTC, PUTCHAR, PUTFILE, and PUTS.
 *
 *  The above writers (PUT etc.) all attempt to auto-open a stream
 *  in 'w' mode ('r+' for PUTS) when presented with a plausible (but
 *  not open) filename or a fd that is only open at the OS level (no
 *  SETL buffer yet)...or in 'real-ms' or bidirectional 'tcp-client'
 *  mode for certain forms of TUPLE.
 *
 *  The helpers (put_lines() etc.) all return true for success, or
 *  false for having set errno, but clients (such as PUT) often ignore
 *  that and let errno (whence LAST_ERROR) be watched directly.
 *
 *  See getfd() in sys.c for more details on auto-opening.
 */

/*
 *  PUT(a(*));  -- write 0 or more lines to STDOUT
 */
void l_put(tuple *a, long nargs) {
  HANDLE ha = ref(a);
  file *f;
  check_optargs_type(a,"PUT");
  check (fd_stdout == getfd ((block *)stdout_integer, getfd_write));
  f = find_fd_file(fd_stdout);
  put_lines (f, a, nargs, "PUT");  /* may set errno */
  retire(ha);
} /* end l_put */

/*
 *  PUTA(a, b(*));  -- write 0 or more lines to stream a
 */
void l_puta(block *a, tuple *b, long nargs) {
  /* Functionally identical to PUTLINE */
  HANDLE hb = ref(b);
  int fd;
  file *f;
  if (!is_stream_id(a)) {
    unary_variadic_error ("PUTA", a, STREAM_ID);
  }
  check_optargs_type(b,"PUTA");
  fd = getfd (a, getfd_write);
  f = find_fd_file(fd);
  put_lines (f, b, nargs-1, "PUTA");  /* may set errno */
  retire(hb);
} /* end l_puta */

/*
 *  PUTB(a, b(*));  -- write 0 or more values to stream a
 */
void l_putb(block *a, tuple *b, long nargs) {
  /* Functionally identical to WRITEA */
  HANDLE hb = ref(b);
  int fd;
  file *f;
  if (!is_stream_id(a)) {
    unary_variadic_error ("PUTB", a, STREAM_ID);
  }
  check_optargs_type(b,"PUTB");
  fd = getfd (a, getfd_write);
  f = find_fd_file(fd);
  /* put_values() or put_char() may set errno */
  if (put_values (f, b, nargs-1)) put_char ('\n', f->buffer);
  retire(hb);
} /* end l_putb */

/*
 *  PUTC(a, b);  -- write the chars of STRING b to stream a
 */
void l_putc(block *a, string *b) {
  HANDLE hb = ref(b);
  int fd;
  file *f;
  if (!is_stream_id(a) ||
      !is_string(b)) {
    binary_proc_error ("PUTC", a, b, "(" STREAM_ID ", STRING)");
  }
  fd = getfd (a, getfd_write);
  f = find_fd_file(fd);
  put_chars (f, b);  /* may set errno */
  retire(hb);
} /* end l_putc */

/*
 *  PUTCHAR(a);  -- write the chars of STRING a to STDOUT
 */
void l_putchar(string *a) {
  HANDLE ha = ref(a);
  file *f;
  if (!is_string(a)) {
    unary_proc_error ("PUTCHAR", a, "STRING");
  }
  check (fd_stdout == getfd ((block *)stdout_integer, getfd_write));
  f = find_fd_file(fd_stdout);
  put_chars (f, a);  /* may set errno */
  retire(ha);
} /* end l_putchar */

/*
 *  PUTENV('name=value') and PUTENV('name') are an old-fashioned way
 *  of setting and unsetting an environment variable.
 *
 *  The more recommended way is to use SETENV and UNSETENV.
 *
 *  PUTENV is now deprecated and has been removed from the lib doc.
 */
void l_putenv(string *a) {
  if (!is_string(a)) {
    unary_proc_error ("PUTENV", a, "STRING");
  }
  if (restricted) runerr("PUTENV is restricted");
  /* This space is never released; contrast l_setenv():  */
  os_putenv (strdup_malloc(&strelt(a,1)));  /* may abend, not set errno */
} /* end l_putenv */

void l_putf(block *a, tuple *b, long nargs) {
  check_optargs_type(b,"PUTF");
  runerr("PUTF not implemented (whatever it means)");
} /* end l_putf */

/*
 *  PUTFILE is like PUTC, except that if it auto-opens an output
 *  stream, it also auto-closes it after writing out the string.
 *
 *  LAST_ERROR may be set on failure of write() or close().
 */
void l_putfile(block *a, string *b) {
  HANDLE hb = ref(b);
  int fd;
  file *f;
  if (!is_stream_id(a) ||
      !is_string(b)) {
    binary_proc_error ("PUTFILE", a, b, "(" STREAM_ID ", STRING)");
  }
  fd = getfd (a, getfd_putfile);
  f = find_fd_file(fd);
  put_chars (f, b);  /* write() may set errno */
  if (f->auto_close_output) {  /* this PUTFILE auto-opened the stream */
    file_flush(fd);               /* write() may set errno */
    file_close(fd, close_await);  /* close() may set errno */
  }
  retire(hb);
} /* end l_putfile */

void l_putk(tuple **a, long nargs) {
  runerr("PUTK not implemented (whatever it means)");
} /* end l_putk */

/*
 *  PUTLINE(a, b(*));  -- write 0 or more lines to stream a
 */
void l_putline(block *a, tuple *b, long nargs) {
  /* Functionally identical to PUTA */
  HANDLE hb = ref(b);
  int fd;
  file *f;
  if (!is_stream_id(a)) {
    unary_variadic_error ("PUTLINE", a, STREAM_ID);
  }
  check_optargs_type(b,"PUTLINE");
  fd = getfd (a, getfd_write);
  f = find_fd_file(fd);
  put_lines (f, b, nargs-1, "PUTLINE");  /* may set errno */
  retire(hb);
} /* end l_putline */

/*
 *  There is no PUTN to go with GETN, because PUTC takes a string
 *  of any length.
 */

/*
 *  PUTS(a, b, c);  -- write STRING c to stream a at file index b 
 *
 *  PUTS is for SETL2 compatibility.
 *
 *  An alternative to using PUTS is to SEEK and then PUTC; don't forget
 *  that SEEK starts at 0 (it works in "offsets") whereas PUTS indexes
 *  the characters of the file starting at 1.  Indeed, PUTS operates by
 *  SEEKing the file to an offset equal to the starting index (b) less 1
 *  and then effectively calling PUTC.  See also GETS.
 */
void l_puts(block *a, integer *b, string *c) {
  HANDLE hc = ref(c);
  int fd;
  off_t start;
  file *f;
  if (!is_stream_id(a) ||
      !is_string(c)) {
    ternary_proc_error ("PUTS", a, b, c,
     "(" STREAM_ID ", INTEGER, STRING)");
  }
  start = get_pos_off_t(b, "second arg to PUTS (starting index)");
  fd = getfd (a, getfd_puts);
  f = find_fd_file(fd);
  file_seek (fd, start-1, SEEK_SET);
  put_chars (f, c);  /* may set errno */
  retire(hc);
} /* end l_puts */

/*
 *  OP RANDOM(a);
 *    CASE OF
 *    (IS_INTEGER a):                    $ random integer
 *      IF a >= 0 THEN
 *        RETURN FIX RANDOM FLOAT (a+1);
 *      ELSE
 *        RETURN FIX RANDOM FLOAT (a-1);
 *      END IF;
 *    (IS_REAL a):                       $ random real
 *      IF a = 0.0 THEN
 *        RETURN 0.0;
 *      ELSEIF a < 0.0 THEN
 *        RETURN - RANDOM -a;
 *      ELSE
 *        RETURN "random real in the
 *                half open interval [0,a)";
 *      END IF;
 *    (IS_TUPLE a):                      $ random element from tuple
 *      IF #a = 0 THEN
 *        RETURN OM;
 *      ELSE
 *        RETURN a(1 + RANDOM (#a-1));
 *      END IF;
 *    (IS_SET a):                        $ random element from set
 *      IF a = {} THEN
 *        RETURN OM;
 *      ELSE
 *        RETURN RANDOM [x : x IN a];
 *      END IF;
 *    ELSE ERROR;
 *    END CASE;
 *  END OP RANDOM;
 *
 *  Extension:  RANDOM can also be applied to a STRING, from which
 *  it randomly selects a character.  If the string is empty,
 *  RANDOM yields OM.
 *
 *  The definition for INTEGER is a SETL idiosyncrasy, yielding
 *  numbers from the set {0..a} (or {a..0} if a is negative), rather
 *  than from the set {0..a-1} (or {a+1..0}).
 *
 *  For some extreme values of REAL operand a, RANDOM can set errno
 *  to ERANGE, and in that case the result is not to be relied upon
 *  as a pseudo-random sample point in [0,a).
 */
block *l_random(block *a) {
  switch (setl_type(a)) {
  case integer_type:
    {
      integer *n = (integer *)a;
      integer *r;
      if (n->size == 0) {  /* a = 0 */
        r = new_integer(0);
      } else {
        HANDLE hn = ref(n);
        integer *one = new_integer(1);
        if (n->size > 0) {  /* a > 0 */
          r = integer_add(n, one);  /* r := a + 1 */
          r = integer_random(r);    /* r := something in 0 .. a */
        } else {  /* a < 0 */
          r = integer_sub(n, one);  /* r := a - 1 */
          r->size = -r->size;       /* r := |a| + 1 */
          r = integer_random(r);    /* r := something in 0 .. |a| */
          r->size = -r->size;       /* r := -r */
        }
        retire(hn);
      }
      return (block *)r;
    }
  case real_type:
    {
      /*
       *  Generate a 64-bit integer and multiply it by 'a' to get b,
       *  and then ldexp(b,-64) is the answer.  Example:  for a=1,
       *  the result is in [0,1).  Actually LONG_BIT not 64, and sorry
       *  if that is 32 on your poor old system and you don't get an
       *  honest mantissa's worth of randomness.
       *
       *  Note that ldexp() can set errno to ERANGE.  It won't happen
       *  except for callers that pass rather exceptional REAL values
       *  to RANDOM, and those who do might like to experiment; so we
       *  don't preserve errno across the ldexp() call.
       */
      double x, b, r;
      x = real_to_double((real *)a);
      b = x * gmp_urandomb_ui (randstate, LONG_BIT);
      r = ldexp(b, -LONG_BIT);  /* may set errno */
      return (block *)new_real(r);
    }
  case set_type:
    {
      set *sa = (set *)a;
      if (sa->card == 0) return OM;
      switch (sa->stype) {
      case plain_set:
        {
          subnode *c;
          long u = random_ulong(sa->card);
          check (ordsee(sa->tblval, u + 1, &c));
          return unkey(c->k);
        }
      case mmap:
        {
          HANDLE hsa = ref(sa);
          subnode *c = NULL;  HANDLE hc = ref(c);
          tuple *t = new_tuple(2);  HANDLE ht = ref(t);
          table *st = sa->tblval;
          long u = random_ulong(patsiz(st));
          check (ordsee(st, u + 1, &c));
          let (tupelt(t,1), unkey(c->k));
          let (tupelt(t,2), l_random(c->d));
          retire(ht);
          retire(hc);
          retire(hsa);
          return (block *)t;
        }
      case smap:
        {
          HANDLE hsa = ref(sa);
          subnode *c = NULL;  HANDLE hc = ref(c);
          tuple *t = new_tuple(2);  HANDLE ht = ref(t);
          long u = random_ulong(sa->card);
          check (ordsee(sa->tblval, u + 1, &c));
          let (tupelt(t,1), unkey(c->k));
          let (tupelt(t,2), copy_value(c->d));
          retire(ht);
          retire(hc);
          retire(hsa);
          return (block *)t;
        }
      default:
        unexpected (sa->stype);
      }
    }
  case string_type:
    {
      string *sa = (string *)a;
      long n = sa->nchar;
      if (LIKELY (n != 0)) {
        long u = random_ulong(n);
        return (block *) new_cstring (strelt(sa, u + 1));
      } else {
        return OM;
      }
    }
  case tuple_type:
    {
      tuple *ta = (tuple *)a;
      long n = ta->nelt;
      if (LIKELY (n != 0)) {
        long u = random_ulong(n);
        return copy_value (tupelt(ta, u + 1));
      } else {
        return OM;
      }
    }
  default:
    unary_op_error ("RANDOM", a, "SET, STRING, TUPLE, or numeric");
  }
} /* end l_random */

/*
 *  OP RANGE(a);
 *    CASE OF
 *    (IS_MAP a):                        $ range of map
 *      RETURN {y : [x,y] IN a};
 *    ELSE ERROR;
 *    END CASE;
 *  END OP RANGE;
 */
set *l_range(set *a) {
  HANDLE ha = ref(a);
  set *r;  HANDLE hr;
  if (!is_set(a)) {
    unary_op_error ("RANGE", a, "SET (map)");
  }
  if (promote(a,mmap)) {
    long i,n;
    r = null_set();  hr = ref(r);
    switch (a->stype) {
    case smap:
      n = a->card;
      for (i=1; i<=n; i++) {
        subnode *c;
        check (ordsee(a->tblval, i, &c));
        set_insert (&r, c->d);  /* set_insert copies c->d */
      }
      break;
    case mmap:
      n = patsiz(a->tblval);
      for (i=1; i<=n; i++) {
        subnode *c;
        check (ordsee(a->tblval, i, &c));
        set_extend (&r, (set *)c->d);
      }
      break;
    default:
      unexpected (a->stype);
    }
    retire(hr);
  } else {
    runerr("RANGE requires a set that is a map");
  }
  retire(ha);
  return r;
} /* end l_range */

/*
 *  RANY(RW a, b)  -- if last char of a is in charset b, remove and
 *                    return it, else ''
 */
string *l_rany(string **a, string *b) {
  string *s = *a;     HANDLE hs = ref(s);
  string *t;
  if (!is_string(s) ||
      !is_string(b)) {
    binary_proc_error ("RANY", s, b, "(STRING, STRING)");
  }
  if (s->nchar > 0) {
    char c = last_char(s);
    long j;
    for (j=1; j<=b->nchar; j++) {
      if (c == strelt(b,j)) {
        last_char(s) = '\0';
        s->nchar--;
        s->nafter++;
        t = new_cstring(c);
        goto done;
      }
    }
  }
  t = null_string();
done:
  *a = s;
  retire(hs);
  return t;
} /* end l_rany */

/*
 *  RBREAK(RW a, b)  -- break off and return trailing chars in a that are
 *                      not in charset b
 */
string *l_rbreak(string **a, string *b) {
  string *s = *a;     HANDLE hs = ref(s);
  string *t;
  long i,j,k;
  if (!is_string(s) ||
      !is_string(b)) {
    binary_proc_error ("RBREAK", s, b, "(STRING, STRING)");
  }
  for (i=s->nchar; i>=1; i--) {
    char c = strelt(s,i);
    for (j=1; j<=b->nchar; j++) {
      if (c == strelt(b,j)) goto done;
    }
  }
done:
  t = copy_substring(s,i+1,s->nchar);
  k = s->nchar - i;
  s->nafter += k;
  s->nchar -= k;
  strelt(s,s->nchar+1) = '\0';
  *a = s;
  retire(hs);
  return t;
} /* end l_rbreak */

/*
 *  PROC READ(WR a(*));  -- READA(STDIN, a(*));
 */
void l_read(tuple **a, long nargs) {
  file *f;
  check (fd_stdin == getfd ((block *)stdin_integer, getfd_read));
  f = find_fd_file(fd_stdin);
  let (*a, get_values (f, nargs, "READ input", true));  /* may set errno */
} /* end l_read */

/*
 *  PROC READA(a, WR b(*));  -- read 0 or more values from stream a
 *
 *  After reading the requested number of values, READA continues
 *  reading characters until it either absorbs a newline or hits an
 *  end of input.  This distinguishes it from GETB, which stops
 *  reading after the end of the last value read.
 */
void l_reada(block *a, tuple **b, long nargs) {
  int fd;
  file *f;
  if (!is_stream_id(a)) {
    unary_variadic_error ("READA", a, STREAM_ID);
  }
  fd = getfd (a, getfd_read);
  /* The attempt to READA a value from a signal or timer stream will
   * produce a result similar to GETFILE, except without running out
   * of memory.  */
  f = find_file(fd);
  let (*b, get_values (f, nargs-1, "READA input", true));  /* may set errno */
} /* end l_reada */

/*
 *  READLINK a  -- content of symlink at pathname a if any; else OM
 */
string *l_readlink(string *a) {
  string *r;
  /*
   *  Since pathconf() on Linux/ext3 for _PC_SYMLINK_MAX just returns -1,
   *  we pick a biggish number for the os_readlink() return buffer size
   *  and keep doubling it until os_readlink() says it's big enough.
   *  Rather silly, really.
   */
  size_t m = 2048;   /* half of init buffer size for os_readlink() */
  ssize_t n;         /* number of bytes placed in the buffer */
  char *buf = NULL;  /* the buffer to catch the symlink */
  if (!is_string(a)) {
    unary_op_error ("READLINK", a, "STRING");
  }
  do {
    /* With buf NULL (init state), this acts as arena_alloc(m+m):  */
    buf = (char *) arena_realloc (buf, m, m+m);
    m += m;
    n = os_readlink(&strelt(a,1), buf, m);
    if (n < 0) {
      arena_free(buf, m);
      return OM;  /* with errno set by readlink() */
    }
  } while (n == (ssize_t)m);  /* repeat while link size is >= m */
  r = new_nstring(buf, n);
  arena_free(buf, m);
  return r;  /* with errno unchanged */
} /* end l_readlink */

/*
 *  PROC READS(a, WR b(*));  -- "read" 0 or more values from STRING a
 *
 *  After SETL2.
 */
void l_reads(string *a, tuple **b, long nargs) {
  HANDLE ha = ref(a);
  tuple *r = OM;      HANDLE hr = ref(r);
  long i, n;
  long j;
  if (!is_string(a)) {
    unary_variadic_error ("READS", a, "STRING");
  }
  n = nargs - 1;
  r = new_tuple(n);
  j = 1;
  for (i=1; i<=n; i++) {
    int stopper = EOF;
    block *x;
    span_white(a, &j);  /* skip leading whitespace if any */
    if (j > a->nchar) {  /* we have reached the end of the string */
      break;  /* exit for-loop */
    }
    x = unstr(a,&j,&stopper,"first arg to READS");
    tupelt(r,i) = x;
  }
  /*
   *  READS, like READ and READA, doesn't care if there is stuff
   *  left over after it is satisfied.  (Contrast UNSTR, which does.)
   */
  tup_truncate(&r);
  retire(hr);
  retire(ha);
  *b = r;
} /* end l_reads */

#define MAX_DATAGRAM 65536

/*
 *  datagram := RECV f
 *
 *   where f must be a UDP client socket (see OPEN).
 *
 *  Note that there is no EOF indication, since UDP servers do not
 *  automatically notify clients when they unbind ports.
 *
 *  Zero-length datagrams are permitted, and users should not allow
 *  themselves to be misled by reading this on the Issue 7 recv() page:
 *
 *  "Upon successful completion, recv() shall return the length of the
 *  message in bytes.  If no messages are available to be received and
 *  the peer has performed an orderly shutdown, recv() shall return 0.
 *  Otherwise, -1 shall be returned and errno set to indicate the error."
 *
 *  Presumably, orderly shutdowns only apply to TCP servers.
 *
 *  RECV does, however, return OM if there is an error at the system
 *  recv() level.  SETL programmers may consult LAST_ERROR in that case.
 *
 *  Incoming datagrams are limited to MAX_DATAGRAM characters in size by
 *  both RECV and RECVFROM.
 */
string *l_recv(block *a) {
  int fd;
  file *f;
  ssize_t n;
  char buf[MAX_DATAGRAM];
  if (!is_stream_id(a)) {
    unary_op_error ("RECV", a, STREAM_ID);
  }
  fd = getfd (a, getfd_recv);
  f = find_fd_file(fd);
  assert (test_all(f->abilities, can_recv));
  /* In fact, f->ftype == udp_client_file, but we have no pressing need
   * to assert that here.  */
  n = os_recv(fd, buf, sizeof buf, 0);  /* may set errno */
  if (n < 0) return OM;  /* with errno set by recv() */
  return new_nstring(buf, n);  /* with errno unchanged */
} /* end l_recv */

/*
 *  [[address, portnum], datagram] := RECVFROM f  -- UDP
 *  [pathname, datagram] := RECVFROM f            -- Unix-domain
 *
 *  where f must be a UDP or Unix-domain datagram server socket
 *  (see OPEN; cf. RECV) or can be auto-opened as a UDP server socket.
 *  The first element of the returned 2-tuple identifies the source of
 *  the datagram.  It is a null string instead of a pathname if the
 *  source is unnamed, as is the case for a sender of mode
 *  UNIX-DATAGRAM-CLIENT.
 */
tuple *l_recvfrom(block *a) {
  int fd;
  file *f;
  ssize_t n;
  struct sockaddr_storage ss;
  struct sockaddr *sa = (struct sockaddr *)&ss;
  socklen_t salen = sizeof ss;
  char buf[MAX_DATAGRAM];
  if (!is_stream_id(a)) {
    unary_op_error ("RECVFROM", a, STREAM_ID);
  }
  fd = getfd (a, getfd_recvfrom);
  f = find_fd_file(fd);
  assert (test_all(f->abilities, can_recvfrom));
  n = os_recvfrom(fd, buf, sizeof buf, 0, sa, &salen);  /* may set errno */
  if (n >= 0) {
    block *from = OM;       HANDLE h_from = ref(from);
    string *datagram = OM;  HANDLE h_datagram = ref(datagram);
    tuple *r;
    switch (f->ftype) {
    case udp_server_file:
      from = (block *) sockaddr_tuple(sa, salen);  /* a 2-TUPLE */
      break;
    case unix_datagram_server_file:
      from = (block *) sockaddr_pathname(sa, salen);  /* a STRING */
      break;
    default:
      unexpected (f->ftype);
    }
    datagram = new_nstring(buf, n);
    r = new_tuple(2);
    tupelt(r,1) = from;
    tupelt(r,2) = (block *)datagram;
    retire(h_datagram);
    retire(h_from);
    return r;  /* with errno unchanged */
  }
  return OM;  /* with errno set by recvfrom() */
} /* end l_recvfrom */

/*
 *  RECV_FD f  -- receive file descriptor over Unix-domain socket stream
 */
integer *l_recv_fd(block *a) {
  int fd, received_fd;
  file *f;
  io_buffer *p;
  if (!is_stream_id(a)) {
    unary_op_error ("RECV_FD", a, STREAM_ID);
  }
  fd = getfd (a, getfd_recv_fd);
  assert (fd >= 0);
  f = find_fd_file(fd);
  p = f->buffer;
  clear_eof(p);  /* clear SETL-level EOF indicators but not eof_pending */
  received_fd = os_recv_fd(fd);  /* recvmsg() or ioctl() may set errno */
  if (received_fd < 0) {  /* "EOF" or error */
    /* Act like this was an EOF on a plain data fetch.  */
    eof_file(f);  /* adjust EOF indicators; auto-close if appropriate */
    return OM;  /* with errno maybe set by os_recv_fd() or file_close() */
  }
  /* Admit the possibility of an extant SETL buffer, as in l_dup().  */
  return new_integer(received_fd);  /* with errno unchanged */
} /* end l_recv_fd */

/*
 *  See DIV for discussion of DIV, REM, and MOD.
 */
integer *l_rem(integer *a, integer *b) {
  if (is_integer(a) && is_integer(b)) {
    if (b->size == 0) {
      runerr("Second operand of REM is 0");
    }
    return integer_rem(a, b);
  }
  binary_op_error ("REM", a, b, "INTEGER operands");
} /* end l_rem */

/*
 *  RENAME(a, b);  -- change pathname a to b
 */
void l_rename(string *a, string *b) {
  if (!is_string(a) ||
      !is_string(b)) {
    binary_proc_error ("RENAME", a, b, "(STRING, STRING)");
  }
  if (restricted) runerr("RENAME is restricted");
  os_rename(&strelt(a,1),&strelt(b,1));  /* rename() may set errno */
} /* end l_rename */

/*
 *  REVERSE a  -- characters of a in reverse order
 *  REVERSE t  -- elements of TUPLE t in reverse order
 */
block *l_reverse(block *a) {
  if (is_string(a)) {
    string *s = (string *)a;    HANDLE hs = ref(s);
    long n = s->nchar;
    string *r = new_estring(n);
    long i;
    for (i=1; i<=n; i++) {
      strelt(r,n-i+1) = strelt(s,i);
    }
    strelt(r,n+1) = '\0';  /* redundant (done by new_estring()) */
    retire(hs);
    return (block *)r;
  } else if (is_tuple(a)) {
    tuple *t = (tuple *)a;      HANDLE ht = ref(t);
    long n = t->nelt;
    tuple *r = new_tuple(n);    HANDLE hr = ref(r);
    long i;
    for (i=1; i<=n; i++) {
      let (tupelt(r,n-i+1), copy_value(tupelt(t,i)));
    }
    retire(hr);
    retire(ht);
    return (block *)r;
  } else {
    unary_op_error ("REVERSE", a, "STRING or TUPLE");
  }
} /* end l_reverse */

/*
 *  REWIND(f);  -- SEEK(f, 0, SEEK_SET);
 */
void l_rewind(block *a) {
  int fd;
  if (!is_stream_id(a)) {
    unary_proc_error ("REWIND", a, STREAM_ID);
  }
  fd = getfd (a, getfd_seek);
  file_seek (fd, 0, SEEK_SET);  /* does not set errno */
} /* end l_rewind */

/*
 *  RLEN(RW a, b)  -- remove and return the last b bytes from STRING a
 *
 *  It seems a little dubious that if there are not enough
 *  characters in the string to satisfy RLEN, you just get what
 *  there are, rather than OM.
 *
 *  A similar comment applies to LEN.
 */
string *l_rlen(string **a, integer *b) {
  string *s = *a;     HANDLE hs = ref(s);
  string *t;
  long i,nb;
  if (!is_string(s)) {
    binary_proc_error ("RLEN", s, b, "(STRING, INTEGER)");
  }
  nb = get_nat_long(b, "second arg to RLEN");
  i = MIN(nb,s->nchar);
  t = copy_substring(s,s->nchar+1-i,s->nchar);
  s->nafter += i;
  s->nchar -= i;
  strelt(s,s->nchar+1) = '\0';
  *a = s;
  retire(hs);
  return t;
} /* end l_rlen */

/*
 *  RMATCH(RW a, b)  -- if a's tail matches b, remove and return it, else ''
 *
 *  Consistent with the strangeness of RLEN, you get '' instead of OM if
 *  a does not end with b.
 *
 *  A similar comment applies to MATCH.
 */
string *l_rmatch(string **a, string *b) {
  string *s = *a;     HANDLE hs = ref(s);
  string *t;
  long na, nb;
  if (!is_string(s) ||
      !is_string(b)) {
    binary_proc_error ("RMATCH", s, b, "(STRING, STRING)");
  }
  na = s->nchar;
  nb = b->nchar;
  if (na >= nb && memcmp(&strelt(s,1+na-nb), &strelt(b,1), nb) == 0) {
    t = copy_string(b);
    s->nafter += nb;
    s->nchar -= nb;
    strelt(s,s->nchar+1) = '\0';
  } else {
    t = null_string();
  }
  *a = s;
  retire(hs);
  return t;
} /* end l_rmatch */

/*
 *  RNOTANY(RW a, b)  -- if last char of a is not in charset b,
 *                       remove and return it, else ''
 */
string *l_rnotany(string **a, string *b) {
  string *s = *a;     HANDLE hs = ref(s);
  string *t;
  if (!is_string(s) ||
      !is_string(b)) {
    binary_proc_error ("RNOTANY", s, b, "(STRING, STRING)");
  }
  if (s->nchar > 0) {
    char c = last_char(s);
    long j;
    for (j=1; j<=b->nchar; j++) {
      if (c == strelt(b,j)) {
        t = null_string();
        goto done;
      }
    }
    last_char(s) = '\0';
    s->nchar--;
    s->nafter++;
    t = new_cstring(c);
  } else {
    t = null_string();
  }
done:
  *a = s;
  retire(hs);
  return t;
} /* end l_rnotany */

/*
 *  ROUND a  -- nearest INTEGER to the REAL a
 */
integer *l_round(real *a) {
  switch (setl_type(a)) {
  case integer_type:
    return copy_integer ((integer *)a);
  case real_type:
    return double_to_integer (round (real_to_double(a)));
  default:
    unary_op_error ("ROUND", a, "numeric");
  }
} /* end l_round */

/*
 *  RPAD(s, n) pads s on the right with blanks to reach length n or more
 *  (so RPAD "left-justifies" the string); n < 0 is an error.
 */
string *l_rpad(string *a, integer *b) {
  long n;
  if (!is_string(a)) {
    binary_proc_error ("RPAD", a, b, "(STRING, INTEGER)");
  }
  n = get_nat_long(b, "second arg to RPAD (length)");
  return str_rpad(a,n,' ');
} /* end l_rpad */

/*
 *  RSPAN(RW a, b)  -- break off and return trailing chars in a that are
 *                     in charset b
 */
string *l_rspan(string **a, string *b) {
  string *s = *a;     HANDLE hs = ref(s);
  string *t;
  long i,j,k;
  if (!is_string(s) ||
      !is_string(b)) {
    binary_proc_error ("RSPAN", s, b, "(STRING, STRING)");
  }
  for (i=s->nchar; i>=1; i--) {
    char c = strelt(s,i);
    for (j=1; j<=b->nchar; j++) {
      if (c == strelt(b,j)) goto next;
    }
    goto done;
next: ;
  }
done:
  t = copy_substring(s,i+1,s->nchar);
  k = s->nchar - i;
  s->nafter += k;
  s->nchar -= k;
  strelt(s,s->nchar+1) = '\0';
  *a = s;
  retire(hs);
  return t;
} /* end l_rspan */

/*
 *  SEEK(a, b)          -- seek stream a to offset b
 *  SEEK(a, b, whence)  -- seek stream a to offset b relative to whence
 *
 *  where whence is SEEK_SET, SEEK_CUR, or SEEK_END.
 *
 *  SEEK numbers bytes starting at 0, consistent with OS conventions,
 *  rather than 1; but see also GETS and PUTS, which are the SETL2
 *  direct-access intrinsics.
 *
 *  The return value is the new offset, a.k.a. file position.
 *
 *  See also FILEPOS, which gives the current position (offset), and
 *  REWIND, which sets it to 0.
 */
integer *l_seek(block *a, integer *b, tuple *c, long nargs) {
  off_t offset, new_offset;
  int whence;
  int fd;
  if (!is_stream_id(a)) {
    runerr("First arg to SEEK must be " STREAM_ID ", not %s",
                                                         TYPENAME(a));
  }
  offset = get_off_t(b, "second (offset) arg to SEEK");
  check_optargs_type(c,"SEEK");
  if (nargs == 2) {
    whence = SEEK_SET;
  } else if (nargs == 3) {
    integer *x = c->nelt == 0 ? OM : (integer *)tupelt(c,1);
    whence = get_int(x, "optional third arg to SEEK");
    switch (whence) {
    case SEEK_SET:
    case SEEK_CUR:
    case SEEK_END:
      break;
    default:
      runerr("Optional third arg to SEEK must be "
            "SEEK_SET (%d), SEEK_CUR (%d), or SEEK_END (%d), not %d",
                 SEEK_SET,      SEEK_CUR,         SEEK_END,  whence);
    }
  } else {
    runerr("SEEK takes 2 or 3 args, not %ld", nargs);
  }
  fd = getfd (a, getfd_seek);
  /* file_seek() may abend, but does not set errno */
  new_offset = file_seek (fd, offset, whence);
  return new_integer(new_offset);
} /* end l_seek */

/*
 *  This SELECT is based on the Unix 'select', but is much nicer.
 *
 *  It expects a triple of sets of stream designators and an optional
 *  integer timeout in milliseconds:
 *
 *   yield := SELECT (triple, timeout)
 *
 *  where the triple is:
 *
 *   [{readers},{writers},{raisers}].
 *
 *  This is exactly the form of the yielded triple too.
 *
 *  Missing elements of the argument triple are taken to be empty sets
 *  and will be returned as empty sets.
 *
 *  This routine is co-ordinated with the SETL buffering.  In
 *  particular, the designator of any stream for which there is still
 *  buffered input waiting to be read will be counted "ready" to read.
 *  Likewise, while there is still room in the output buffer for a
 *  given stream designator, it is considered "ready" for writing,
 *  and any signals that have been received but not yet read by the
 *  SETL program are considered "ready input".
 *
 *  A server socket on which ACCEPT is guaranteed not to block
 *  (because there is a pending connection request from some client)
 *  is also considered "ready to read", even though there is no
 *  actual input from such a stream.
 *
 *  If nothing is already ready when SELECT is called, recourse is
 *  made to potentially blocking system-level functions.
 *
 *  Signals and timers can be awaited simultaneously with other
 *  streams by including them in the set of readers.
 *
 *  SELECT returns a triple whenever some stream mentioned in the
 *  argument triple becomes ready for an appropriate I/O operation,
 *  or when the timeout elapses, whichever comes first.  A timeout
 *  of 0 is equivalent to "polling" the streams.  A missing timeout
 *  means unlimited time.  It follows that the call
 *
 *   SELECT(OM);  -- or equivalently, SELECT([{},{}]) etc.
 *
 *  specifies an indefinite wait (sleep) that can only be broken by
 *  receipt of a (probably process-terminating) signal.
 *
 *  Thru version 2.3.5, any number of streams could be returned at a
 *  time, just as for the underlying POSIX pselect().  But that can
 *  lead to a subtle source of errors, as a stream closed by one
 *  response could still be present in a "ready" set and thus
 *  mistakenly handled by another.
 *
 *  So in the name of motherhood, SELECT returns at most one stream
 *  at a time, strangely packaged in the name of history, by caching
 *  results and doling them out one by one.  We are also careful to
 *  remove streams from the cache when they are closed.
 */
tuple *l_select(tuple *a, tuple *t, long nargs) {
  /* a = OM is equivalent to the null tuple */
  if (!(is_om(a) || is_tuple(a))) {
    unary_variadic_error ("SELECT", a, "TUPLE or OM");
  }
  check_optargs_type(t,"SELECT");
  if (!is_om(a)) {
    long i;
    if (a->nelt > 3) {
      runerr("First arg to SELECT has more than 3 elements");
    }
    for (i=1; i<=a->nelt; i++) {
      set *s = (set *)tupelt(a,i);
      /* set of readers, writers, or raisers of interest */
      if (!(is_om(s) || is_set(s))) {
        runerr("Element %ld of the SELECT tuple must be SET or OM,"
               " not %s", i, TYPENAME(s));
      }
    }
  }
  if (nargs > 2) {
    runerr("SELECT takes 1 or 2 args, not %ld", nargs);
  }
  if (nargs == 2 && t->nelt == 0) {
    runerr("Second arg to SELECT (if any) must be INTEGER, not OM");
  }
  /*
   *  In order to make SELECT revert to giving more than one stream at
   *  a time, it suffices to change this sys_select() to do_select():
   */
  return sys_select(a,t);  /* does not set errno */
} /* end l_select */

/*
 *  SEND(f, datagram);
 *
 *  Send datagram on UDP or Unix-domain client socket.
 */
void l_send(block *a, string *b) {
  int fd;
  file *f;
  HANDLE hb;
  if (!is_stream_id(a) ||
      !is_string(b)) {
    binary_proc_error ("SEND", a, b, "(" STREAM_ID ", STRING)");
  }
  hb = ref(b);
  fd = getfd (a, getfd_send);
  f = find_fd_file(fd);
  assert (test_all(f->abilities, can_send));
  os_send(fd, &strelt(b,1), b->nchar, 0);  /* send() may set errno */
  retire(hb);
} /* end l_send */

/*
 *  SENDTO(f, destination, datagram);
 *
 *  Send datagram on UDP or Unix-domain server socket.
 *
 *  The destination is either a [host, port] pair (for UDP) or a
 *  pathname (for Unix-domain datagrams).
 */
void l_sendto(block *a, block *b, string *c) {
  HANDLE hb, hc;
  int fd;
  file *f;
  if (!is_stream_id(a) ||
      !(is_tuple(b) || is_string(b)) ||
      !is_string(c)) {
    ternary_proc_error ("SENDTO", a, b, c,
                         "(" STREAM_ID ", TUPLE or STRING, STRING)");
  }
  /* Doing this getfd() early because subsequent code takes
   * addresses of things on the heap (which can move), so we
   * don't want any SETL heap allocation to happen in that code.
   * Maintainer note:  if that ever becomes necessary, write the
   * extra code to use local buffers for the nodename and servname
   * in all cases, not just some.  */
  hb = ref(b);
  hc = ref(c);
  fd = getfd (a, getfd_sendto);
  f = find_fd_file(fd);
  assert (test_all(f->abilities, can_sendto));
  retire(hc);
  retire(hb);
  if (is_tuple(b)) {
    const char *nodename;  /* host name or address */
    const char *servname;  /* service name or port number */
    char serv[12];
    struct addrinfo hints;
    struct addrinfo *head;
    tuple *t = (tuple *)b;
    /* Not allowing degenerate tuples as we do in OPEN */
    if (t->nelt != 2) {
      runerr("SENDTO destination arg must have 2 elements, not %ld",
                                                               t->nelt);
    }
    /* But we can allow the nodename to be OM or '' */
    if (is_om(tupelt(t,1))) {
      nodename = NULL;  /* this means the loopback interface */
    } else if (is_string(tupelt(t,1))) {
      string *s = (string *)tupelt(t,1);
      if (s->nchar == 0) {
        nodename = NULL;
      } else {
        nodename = &strelt(s,1);  /* transient ptr into SETL heap */
      }
    } else {
      runerr("Host name or address passed to SENDTO must be STRING or OM,"
             " not %s", TYPENAME(tupelt(t,1)));
    }
    if (is_string(tupelt(t,2))) {
      /* An empty string or "0" here would make no sense, but we leave
       * that to getaddrinfo() or sendto() to diagnose.  */
      string *s = (string *)tupelt(t,2);
      servname = &strelt(s,1);  /* transient ptr into SETL heap */
    } else if (is_integer(tupelt(t,2))) {
      integer *i = (integer *)tupelt(t,2);
      ro_mpz(z, i)  /* const mpz_t z = alias of INTEGER i */
      if (mpz_sizeinbase (z, 10) > sizeof serv - 2) {  /* many digits */
        runerr("Port number too large in SENDTO destination arg");
      }
      /* No explicit check against negative port#, but os_getaddrinfo()
       * probably wouldn't like a minus sign in serv:  */
      mpz_get_str (serv, 10, z);  /* serv := port# as string */
      servname = serv;  /* points to local array on stack */
    } else {
      runerr("Service name or port number passed to SENDTO must be"
             "STRING or INTEGER, not %s", TYPENAME(tupelt(t,2)));
    }
    memset (&hints, 0, sizeof hints);
    hints.ai_family = AF_UNSPEC;
    hints.ai_socktype = SOCK_DGRAM;
    /* If we start specifying a protocol of IPPROTO_UDP when we create
     * UDP server sockets (I currently believe that to be redundant),
     * we should also put hints.ai_protocol = IPPROTO_UDP here, and
     * assert (ai->ai_protocol == IPPROTO_UDP) below.  */
    if (os_getaddrinfo(nodename, servname, &hints, &head) == 0) {
      /* Not caching the mapping from nodename to address here for the
       * presumably common case where the nodename remains the same
       * between calls to SENDTO on the given fd.  This is a possible
       * later featurette, but unlikely to be seriously wanted, because
       * SENDTO is normally used to reply to clients whose sockaddr has
       * come in from a RECVFROM, where the address and port are already
       * in numeric form (so the above os_getaddrinfo() call won't
       * require a name lookup).  */
      const struct addrinfo *ai;
      int saved_errno = errno;
      for (ai=head; ai; ai=ai->ai_next) {
        assert (ai->ai_socktype == SOCK_DGRAM);
        if (os_sendto(fd, &strelt(c,1), c->nchar, 0,
                       ai->ai_addr, ai->ai_addrlen) >= 0) {  /* yay */
          /* Restore errno in case it was set by an sendto() failure
           * earlier in the list.  */
          errno = saved_errno;
          break;  /* escape the for-loop */
        } else {
          /* errno was set by sendto() */
        }
      }
      os_freeaddrinfo (head);  /* does not (further) change errno */
    } else {
      /* errno was set by gai_errno() on a failed getaddrinfo() */
    }
    /* Return with errno unmodified on success, or as set by
     * os_getaddrinfo() or the last sendto() on failure.  */
  } else if (is_string(b)) {
    string *s = (string *)b;
    const char *pathname = &strelt(s, 1);
    const socklen_t n = s->nchar;
    struct sockaddr_un sockname;
    if (n >= sizeof sockname.sun_path) {
      runerr("Unix-domain socket pathname \"%s\" passed to SENDTO is too long",
                            tame(pathname));
    }
    memset(&sockname, 0, sizeof sockname);
    sockname.sun_family = AF_UNIX;
    memcpy(sockname.sun_path, pathname, n);
    os_sendto(fd, &strelt(c,1), c->nchar, 0,
               (const struct sockaddr *)&sockname,
               n + offsetof(struct sockaddr_un, sun_path));
    /* Return with errno unmodified on success, or as set by
     * sendto() on failure.  */
  } else {
    unexpected (b->type);
  }
} /* end l_sendto */

/*
 *  SEND_FD(f, fd);  -- send file descriptor over Unix-domain socket
 */
void l_send_fd(block *a, integer *b) {
  int fd, fd_to_send;
  if (!is_stream_id(a)) {
    binary_proc_error ("SEND_FD", a, b, STREAM_ID " arg");
  }
  fd_to_send = get_long_in(b, fd_lo,fd_hi-1,
                        "second arg to SEND_FD (the fd to send)");
  fd = getfd (a, getfd_send_fd);
  os_send_fd(fd, fd_to_send);  /* sendmsg() or ioctl() may set errno */
  /* Any error will be reflected in errno, though the anomalous
   * (and I believe theoretically impossible under POSIX) case of
   * "succeeding" in writing only 0 of 1 check bytes under the hood in
   * os_send_fd() does not set errno.  Success for the blocking
   * sendmsg() should really only be claimed if it wrote that byte.  */
} /* end l_send_fd */

/*
 *  SETCTTY(f);  -- acquire controlling terminal
 */
void l_setctty(block *a) {
  int fd;
  if (restricted) runerr("SETCTTY is restricted");
  if (!is_stream_id(a)) {
    unary_proc_error ("SETCTTY", a, STREAM_ID);
  }
  fd = getfd (a, getfd_info);
  os_setctty(fd);  /* ioctl() or lack of TIOCSCTTY may set errno */
} /* end l_setctty */

/*
 *  SETEGID(a);  -- set effective group ID
 */
void l_setegid(integer *a) {
  gid_t gid;
  if (restricted) runerr("SETEGID is restricted");
  gid = get_gid_t(a, "group ID passed to SETEGID");
  os_setegid(gid);  /* setegid() may set errno */
} /* end l_setegid */

void l_setem(block *a, block *b) {
  runerr("SETEM not implemented (whatever it means)");
} /* end l_setem */

/*
 *  SETENV(name, value)
 *
 *  SETENV(name)  -- equivalent to SETENV(name,'')
 *
 *  SETENV and UNSETENV are greatly preferable to PUTENV, which entails
 *  a memory leak.  (High-frequency setting and unsetting of envt vars
 *  is never to be recommended, but balanced use of SETENV and UNSETENV
 *  should at least not leak memory in any reasonable implementation.)
 *
 *  They do not set errno, but abend the program on error.
 */
void l_setenv(string *a, tuple *b, long nargs) {
  if (!is_string(a)) {
    unary_variadic_error ("SETENV", a, "STRING");
  }
  check_optargs_type(b,"SETENV");
  if (nargs == 1) {
    if (restricted) runerr("SETENV is restricted");
    os_setenv(&strelt(a,1), "");  /* set to the empty string */
  } else if (nargs == 2) {
    string *x = b->nelt == 0 ? OM : (string *)tupelt(b,1);
    if (!is_string(x)) {
      binary_proc_error ("SETENV", a, x, "(STRING, optional STRING)");
    }
    if (restricted) runerr("SETENV is restricted");
    os_setenv(&strelt(a,1), &strelt(x,1));
  } else {
    runerr("SETENV takes 1 or 2 args, not %ld", nargs);
  }
} /* end l_setenv */

/*
 *  SETEUID(a);  -- set effective user ID
 */
void l_seteuid(integer *a) {
  uid_t uid;
  if (restricted) runerr("SETEUID is restricted");
  uid = get_uid_t(a, "user ID passed to SETEUID");
  os_seteuid(uid);  /* seteuid() may set errno */
} /* end l_seteuid */

/*
 *  SETGID(a);  -- set "real" group ID
 */
void l_setgid(integer *a) {
  gid_t gid;
  if (restricted) runerr("SETGID is restricted");
  gid = get_gid_t(a, "group ID passed to SETGID");
  os_setgid(gid);  /* setgid() may set errno */
} /* end l_setgid */

/*
 *  SETPGID(p, pg);  -- set process group ID of p to pg (put p in pg)
 */
void l_setpgid(integer *a, integer *b) {
  pid_t pid, pgid;
  if (restricted) runerr("SETPGID is restricted");
  pid = get_nat_pid_t(a, "first arg to SETPGID (process ID)");
  pgid = get_nat_pid_t(b, "second arg to SETPGID (process group ID)");
  os_setpgid(pid, pgid);  /* setpgid() may set errno */
} /* end l_setpgid */

/*
 *  SETPGRP;  -- SETPGID(0, 0);
 *
 *  This interface is now deprecated and removed from the lib doc.
 */
void l_setpgrp(void) {
  if (restricted) runerr("SETPGRP is restricted");
  os_setpgrp();  /* setpgid() may set errno */
} /* end l_setpgrp */

/*
 *  SETRANDOM(seed); -- seeds the GMP-based random number generator
 *
 *  If seed is 0, it seeds with the best available source of entropy
 *  it can easily find.
 */
void l_setrandom(integer *a) {
  if (is_integer(a)) {
    if (a->size == 0) {  /* meaning 'a' (the seed arg) is 0 */
      /*
       *  Try for a nice big seed from /dev/urandom.
       */
      mp_limb_t limbs[16];  /* 1024 bits if limbs are 8 bytes each */
      ssize_t k = 0;
      int saved_errno = errno;
      int fd = os_open("/dev/urandom", O_RDONLY);
      if (fd != -1) {
        k = os_read(fd, limbs, sizeof limbs);
        os_close(fd);
      }
      errno = saved_errno;
      if (k == sizeof limbs) {
        /*
         *  Successful read of a seed from /dev/urandom into 'limbs'.
         *
         *  The decl of 'seed' and call to mpz_roinit_n() below would be
         *  replaced by this line but for the remote possibility of the
         *  high-order limb being 0 and thus violating the documented
         *  contract of MPZ_ROINIT_N(), mere initializer though it is:
         *
         *   const mpz_t seed = MPZ_ROINIT_N(limbs, numberof(limbs));
         */
        mpz_t seed;
        mpz_roinit_n (seed, limbs, numberof(limbs));  /* make the alias */
        gmp_randseed (randstate, seed);
      } else {
        /*
         *  No /dev/urandom?  How unusual.  Fall back to the number of
         *  ms since the beginning of 1970, modulo the range of ulong.
         */
        struct timespec t = os_tod();
        ulong seed = t.tv_sec*1000 +
                     t.tv_nsec/1000000;  /* ms; ignore overflow */
        gmp_randseed_ui (randstate, seed);
      }
    } else {
      ro_mpz(seed, a)  /* const mpz_t seed = alias of INTEGER a */
      gmp_randseed (randstate, seed);
    }
  } else {
    unary_proc_error ("SETRANDOM", a, "INTEGER");
  }
} /* end l_setrandom */

/*
 *  SETSID;  -- create new session (for job control)
 */
void l_setsid(void) {
  if (restricted) runerr("SETSID is restricted");
  os_setsid();  /* setsid() may set errno */
} /* end l_setsid */

/*
 *  SETUID(a);  -- set "real" user ID
 */
void l_setuid(integer *a) {
  uid_t uid;
  if (restricted) runerr("SETUID is restricted");
  uid = get_uid_t(a, "user ID passed to SETUID");
  os_setuid(uid);  /* setuid() may set errno */
} /* end l_setuid */

/*
 *  SET_INTSLASH(a);  -- INTSLASH := a;
 */
boolean *l_set_intslash(boolean *a) {
  bool arg;
  if (!is_boolean(a)) {
    unary_proc_error ("SET_INTSLASH", a, "BOOLEAN");
  }
  arg = a->booval;
  return new_boolean(set_intslash(arg));
} /* end l_set_intslash */

/*
 *  SET_MAGIC(a);  -- MAGIC := a;
 */
boolean *l_set_magic(boolean *a) {
  bool arg;
  if (!is_boolean(a)) {
    unary_proc_error ("SET_MAGIC", a, "BOOLEAN");
  }
  arg = a->booval;
  return new_boolean(set_magic(arg));
} /* end l_set_magic */

/*
 *  SHUTDOWN(f, SHUT_RD);    -- shut down reading on stream f
 *  SHUTDOWN(f, SHUT_WR);    -- shut down writing on stream f
 *  SHUTDOWN(f, SHUT_RDWR);  -- shut down reading and writing on f
 *
 *  Shut down reading and/or writing on stream f but without closing
 *  the SETL stream or the underlying fd.
 */
void l_shutdown(block *a, integer *b) {
  int how;
  int fd;
  getfd_op intent;
  if (!is_stream_id(a)) {
    binary_proc_error ("SHUTDOWN", a, b, "(" STREAM_ID ", INTEGER)");
  }
  how = get_int(b, "second arg to SHUTDOWN (how)");
  switch (how) {
  case SHUT_RD:    intent = getfd_shut_r;  break;
  case SHUT_WR:    intent = getfd_shut_w;  break;
  case SHUT_RDWR:  intent = getfd_shut_rw; break;
  default:
    runerr("Second arg to SHUTDOWN must be "
            "SHUT_RD (%d), SHUT_WR (%d), or SHUT_RDWR (%d), not %d",
                 SHUT_RD,      SHUT_WR,         SHUT_RDWR,      how);
  }
  /* this getfd() flushes in SHUT_WR and SHUT_RDWR cases */
  fd = getfd (a, intent);
  /* shutdown() may set errno */
  file_shutdown(fd, how);  /* record reduced abilities, and shutdown() */
} /* end l_shutdown */

/*
 *  OP SIGN(a);
 *    CASE OF
 *    (IS_INTEGER a):                      $ integer sign
 *      CASE OF
 *      (a<0):  RETURN -1;
 *      (a=0):  RETURN 0;
 *      (a>0):  RETURN +1;
 *      END CASE;
 *    (IS_REAL a):                         $ real sign
 *      CASE OF
 *      (a<0.0):  RETURN -1;
 *      (a=0.0):  RETURN 0;
 *      (a>0.0):  RETURN +1;
 *      END CASE;
 *    ELSE ERROR;
 *    END CASE;
 *  END OP SIGN;
 */
integer *l_sign(block *a) {
  switch (setl_type(a)) {
  case integer_type:
    return new_integer (SIGN(((integer *)a)->size));
  case real_type:
    {
      double d = real_to_double((real *)a);
      if (d < 0.0) return new_integer(-1);
      else if (d == 0.0) return new_integer(0);
      else /* if (d > 0.0) */ return new_integer(1);
    }
  default:
    unary_op_error ("SIGN", a, "numeric");
  }
} /* end l_sign */

/*
 *  OP SIN(a);                      $ sine
 *    CASE OF
 *    (IS_REAL a):
 *      RETURN "sine of a";
 *    ELSE ERROR;
 *    END CASE;
 *  END OP SIN;
 */
real *l_sin(real *a) {
  double x = get_double(a,"SIN operand");
  return new_real(sin(x));
} /* end l_sin */

/*
 *  OP SINH(a);                      $ hyperbolic sine
 *    CASE OF
 *    (IS_REAL a):
 *      RETURN "hyperbolic sine of a";
 *    ELSE ERROR;
 *    END CASE;
 *  END OP SINH;
 */
real *l_sinh(real *a) {
  double x = get_double(a,"SINH operand");
  return new_real(sinh(x));
} /* end l_sinh */

/*
 *  SOCKADDR a  -- local address and port number of network socket a
 *
 *  If getsockname() fails, or gives a sockaddr that isn't in
 *  address family AF_INET or AF_INET6, SOCKADDR sets LAST_ERROR (errno)
 *  and returns OM.
 */
tuple *l_sockaddr(block *a) {
  int fd;
  struct sockaddr_storage ss;
  struct sockaddr *sa = (struct sockaddr *)&ss;
  socklen_t salen = sizeof ss;
  if (!is_stream_id(a)) {
    unary_op_error ("SOCKADDR", a, STREAM_ID);
  }
  fd = getfd (a, getfd_sockaddr);
  if (os_getsockname(fd, sa, &salen)) {
    if (salen >= sizeof sa->sa_family &&
        (sa->sa_family == AF_INET ||
         sa->sa_family == AF_INET6)) {
      return sockaddr_tuple(sa, salen);  /* no change to errno */
    } else {
      errno = EAFNOSUPPORT;  /* maybe a Unix-domain socket */
      return OM;
    }
  } else {
    return OM;  /* with errno set by getsockname() */
  }
} /* end l_sockaddr */

/*
 *  SOCKETPAIR
 *
 *  A pair of connected file descriptors created as if by the POSIX call
 *
 *   socketpair (AF_UNIX, SOCK_STREAM, 0, pair)
 *
 *  and subject to the semantics and capacity limits of the underlying
 *  bidirectional channel.  Purely for low-level use, like PIPE, to
 *  which it is functionally identical, in situations where you need
 *  the granularity of FORK, DUP2, etc.
 *
 *  As with l_pipe(), it is possible on a truly ancient pre-POSIX
 *  platform to get a merely unidirectional channel, created by pipe(),
 *  in which case os.c should have given a #warning when it was compiled.
 */
tuple *l_socketpair(void) {
  int fds[2];
  integer *fd1;  HANDLE h1;
  integer *fd2;  HANDLE h2;
  tuple *r;
  if (restricted) runerr("SOCKETPAIR is restricted");
  if (os_pipe(fds) < 0) return OM;  /* with errno set by socketpair() */
  /* Like in l_dup(), we do not check against existing SETL buffers,
   * on the off chance that they might occasionally exist on purpose
   * someday.  */
  fd1 = new_integer(fds[0]);  h1 = ref(fd1);
  fd2 = new_integer(fds[1]);  h2 = ref(fd2);
  r = new_tuple(2);
  tupelt(r,1) = (block *)fd1;
  tupelt(r,2) = (block *)fd2;
  retire(h2);
  retire(h1);
  return r;
} /* l_socketpair */

/*
 *  SPAN(RW a, b)  -- break off and return initial chars in a that are
 *                    in charset b
 */
string *l_span(string **a, string *b) {
  string *s = *a;     HANDLE hs = ref(s);
  string *t;
  long i,j;
  if (!is_string(s) ||
      !is_string(b)) {
    binary_proc_error ("SPAN", s, b, "(STRING, STRING)");
  }
  for (i=1; i<=s->nchar; i++) {
    char c = strelt(s,i);
    for (j=1; j<=b->nchar; j++) {
      if (c == strelt(b,j)) goto next;
    }
    goto done;
next: ;
  }
done:
  i--;
  t = copy_substring(s,1,i);
  s->nbefore += i;
  s->nchar -= i;
  *a = s;
  retire(hs);
  return t;
} /* end l_span */

/*
 *  SPLIT(s, delim)  -- split s at substrings matching delim
 *  SPLIT(s)         -- split s at whitespace
 *
 *  Split STRING s into a TUPLE at delim, which defaults to whitespace.
 *  For example, for delim ', *' (comma followed by 0 or more spaces),
 *  SPLIT(' a,b,c ,d, e ', ', *') = [' a','b','c ','d','e '].
 */
tuple *l_split(string *a, tuple *b, long nargs) {
  string *x;
  if (!is_string(a)) {
    unary_variadic_error ("SPLIT", a, "STRING");
  }
  check_optargs_type(b,"SPLIT");
  if (nargs == 1) {
    HANDLE ha = ref(a);
    tuple *t;
    bool saved_magic = set_magic(true);
#ifdef USE_REGEX
    /* USE_REGEX means POSIX "extended" syntax (EREs).  */
    x = new_string("[ \f\n\r\t\v]+");
#else
#error Non-USE_REGEX case no longer supported.
#endif
    retire(ha);
    t = rex_split(a,x);  /* compiling considered cheap, evidently */
    set_magic(saved_magic);
    return t;
  } else if (nargs == 2) {
    x = b->nelt == 0 ? OM : (string *)tupelt(b,1);
    if (!is_string(x)) {
      binary_proc_error ("SPLIT", a, x, "(STRING, optional STRING)");
    }
    return rex_split(a,x);
  } else {
    runerr("SPLIT takes 1 or 2 args, not %ld", nargs);
  }
} /* end l_split */

/*
 *  OP SQRT(a);                      $ square root
 *    CASE OF
 *    (IS_REAL a):
 *      IF a >= 0.0 THEN
 *        RETURN "square root of a";
 *      ELSE
 *        ERROR;
 *      END IF;
 *    ELSE ERROR;
 *    END CASE;
 *  END OP SQRT;
 */
real *l_sqrt(real *a) {
  double x = get_double(a,"SQRT operand");
  if (x < 0.0) {
    runerr("SQRT operand (%.*g) must be at least 0",
            DOUBLE_REP_DIGS, x);
  }
  return new_real(sqrt(x));
} /* end l_sqrt */

/*
 *  STATUS  -- status of last successfully waited-for child process
 *
 *  When raw_status == no_status, STATUS is OM.
 *
 *  For POSIX WIFEXITED(raw_status), indicating a normal child exit,
 *  STATUS is the POSIX WEXITSTATUS(raw_status) as an INTEGER.  This
 *  is the low-order 8 bits, as an unsigned quantity, of what the
 *  child passed to exit() or the like, or returned from main(), or
 *  gave as the arg to the STOP statement in SETL.
 *
 *  For POSIX WIFSIGNALED(raw_status), we let STATUS be -128 + sig,
 *  where sig is the number of the signal that caused the termination.
 *  This allows the SETL program to distinguish normal exits from
 *  termination by signal, but also dovetails with shell conventions
 *  (128 plus the signal number) when taken modulo 256.  An older
 *  version of GNU SETL used -WTERMSIG(raw_status) rather than
 *  -128 + WTERMSIG(raw_status).
 *
 *  For POSIX WIFSTOPPED(raw_status), we let STATUS be -32 + sig,
 *  where sig is the one of {SIGSTOP, SIGTSTP, SIGTTIN, SIGTTOU} that
 *  caused the stoppage.  Those 4 signal numbers lie within the range
 *  17 to 28 on all known POSIX implementations, putting the
 *  corresponding STATUS values in an otherwise unused range of
 *  small negative integers.
 *
 *  For POSIX WIFCONTINUED(raw_status), we let STATUS be -32.
 */
integer *l_status(void) {
  int raw_status = get_raw_status();  /* no_status or waitpid() result */
  if (raw_status == no_status) {  /* per init or per failed waitpid() */
    return OM;
  } else if (WIFEXITED (raw_status)) {
    return new_integer (WEXITSTATUS (raw_status));
  } else if (WIFSIGNALED (raw_status)) {
    return new_integer (-128 + WTERMSIG (raw_status));
  } else if (WIFSTOPPED (raw_status)) {
    return new_integer (-32 + WSTOPSIG (raw_status));
#ifdef WIFCONTINUED
  } else if (WIFCONTINUED (raw_status)) {
    return new_integer (-32);
#endif
  } else {
    unexpected (raw_status);
  }
} /* end l_status */

/*
 *  OP STR(a);
 *    CONST dg = '0123456789',
 *          uc = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ',
 *          lc = 'abcdefghijklmnopqrstuvwxyz',
 *           q = '''';
 *    IF a = OM THEN RETURN '*'; END IF;
 *    CASE OF
 *    (IS_ATOM a):                         $ atom to string
 *      RETURN '#' + STR "unique atom number of a";
 *    (IS_BOOLEAN a):                      $ boolean to string
 *      CASE a OF
 *        (TRUE):   RETURN '#T';
 *        (FALSE):  RETURN '#F';
 *      END CASE;
 *    (IS_INTEGER a):                      $ integer to string
 *      y := ABS a;
 *      (UNTIL y=0)
 *        x +:= dg(y MOD 10);
 *        y := y DIV 10;
 *      END;
 *      RETURN IF a < 0 THEN '-' + x ELSE x END;
 *    (IS_REAL a):                         $ real to string
 *      RETURN "string repr. of the real a, see Dewar p.58";
 *    (IS_SET a):                          $ set to string
 *      x := '';
 *      (FOR y IN a)
 *        x +:= ' ' + STR y;
 *      END;
 *      RETURN '{' + x(2..) + '}';
 *    (IS_STRING a):                       $ string to string
 *      IF a /= '' AND a(1) IN uc+lc AND
 *             (FORALL x IN a(2..) | x IN uc+lc+dg+'_') THEN
 *        RETURN a;
 *      ELSE
 *        RETURN q +/ [IF y=q THEN q+q ELSE y END : y IN a] + q;
 *      END IF;
 *    (IS_TUPLE a):                        $ tuple to string
 *      x := '';
 *      (FOR y IN a)
 *        x +:= ' ' + STR y;
 *      END;
 *      RETURN '[' + x(2..) + ']';
 *    ELSE ERROR;
 *    END CASE;
 *  END OP STR;
 *
 *  You can also convert a ROUTINE, though for that the string
 *  form is just '<ROUTINE>', so it's not reversible via UNSTR.
 *  (Anyway, ATOMs never were, and REALs may be only approximately so.)
 */
string *l_str(block *a) {
  return tostr(a);
} /* end l_str */

/*
 *  STRAD(a, b)  -- INTEGER a represented as STRING in base b
 *
 *  Convert 'a' to string representation in radix 'b', including the
 *  conventional "radix#" prefix in the resulting denotation.
 *  For example, STRAD(20, 16) = '16#14'.
 */
string *l_strad(integer *a, integer *b) {
  int radix;
  if (!is_integer(a)) {
    binary_proc_error ("STRAD", a, b, "(INTEGER, INTEGER)");
  }
  radix = get_long_in(b, 2, 36, "second (radix) arg to STRAD");
  return tostrad(a, radix);
} /* end l_strad */

/*
 *  SUB(RW a, b)
 *  SUB(RW a, b, c)  -- substring of a matching b; a(b) := c (default '')
 */
string *l_sub(string **a, block *b, tuple *c, long nargs) {
  string *s = *a;
  if (!is_string(s)) {
    runerr("First (subject) arg to SUB must be STRING, not %s",
                                                         TYPENAME(s));
  }
  if (!(is_string(b) || is_integer(b) || is_tuple(b))) {
    runerr("Second arg to SUB must be STRING, INTEGER, or TUPLE,"
           " not %s", TYPENAME(b));
  }
  if (is_tuple(b)) {
    tuple *t = (tuple *)b;
    long i;
    if (t->nelt != 2) {
      runerr("Second arg to SUB, if TUPLE, must have 2 elements,"
             " not %ld", t->nelt);
    }
    for (i=1; i<=2; i++) {
      block *p = tupelt(t,i);
      if (!(is_string(p) || is_integer(p))) {
        runerr("Element %ld of second arg to SUB must be STRING or"
               " INTEGER, not %s", i, TYPENAME(p));
      }
    }
  }
  check_optargs_type(c,"SUB");
  if (nargs == 2) {
    HANDLE hs = ref(s);
    HANDLE hb = ref(b);
    string *x = null_string();
    string *r = rex_sub(&s,b,x);
    *a = s;
    retire(hb);
    retire(hs);
    return r;
  } else if (nargs == 3) {
    string *r;
    string *x = c->nelt == 0 ? OM : (string *)tupelt(c,1);
    if (!is_string(x)) {
      runerr("Third arg to SUB (if any) must be STRING, not %s",
                                                          TYPENAME(x));
    }
    r = rex_sub(&s,b,x);
    *a = s;
    return r;
  } else {
    runerr("SUB takes 2 or 3 args, not %ld", nargs);
  }
} /* end l_sub */

/*
 *  OP SUBSET(a,b);
 *    CASE OF
 *    (IS_SET a AND IS_SET b):             $ subset test
 *      RETURN FORALL x IN a | x IN b;
 *    ELSE ERROR;
 *    END CASE;
 *  END OP SUBSET;
 */
boolean *l_subset(set *a, set *b) {
  HANDLE ha;
  HANDLE hb;
  iterator *i;  HANDLE hi;
  block *x;
  bool flag;
  if (!is_set(a) ||
      !is_set(b)) {
    binary_op_error ("SUBSET", a, b, "SET operands");
  }
  if (a->card > b->card) return new_boolean(false);
  ha = ref(a);
  hb = ref(b);
  i = init_iterator((block *)a);  hi = ref(i);
  flag = true;
  while (flag && step_iterator(&i,&x)) if (!in_set(b,x)) flag = false;
  retire(hi);
  retire(hb);
  retire(ha);
  return new_boolean(flag);
} /* end l_subset */

/*
 *  SYMLINK(a, b);  -- make b be a symbolic link with the text a
 */
void l_symlink(string *a, string *b) {
  if (!is_string(a) ||
      !is_string(b)) {
    binary_proc_error ("SYMLINK", a, b, "(STRING, STRING)");
  }
  if (restricted) runerr("SYMLINK is restricted");
  os_symlink(&strelt(a,1),&strelt(b,1));  /* symlink() may set errno */
} /* end l_symlink */

/*
 *  SYSTEM(a)  -- run shell command as child and get its exit status
 */
integer *l_system(string *a) {
  int raw_status;
  /* We could support letting 'a' be OM to simulate calling system(NULL),
   * which is supposed to return 1 if there is a shell, but since in
   * reality an OM arg here is much more likely to be a mistake, we
   * treat it that way.  */
  if (!is_string(a)) {
    unary_proc_error ("SYSTEM", a, "STRING");
  }
  if (restricted) {
    /* Case-sensitive check on shell cmd and args */
    const char *cmd = &strelt(a,1);
    if (!is_allowed(allowed_system_list, cmd)) {
      runerr("SYSTEM(\"%s\") is restricted",
                   tame(cmd));
    }
  }
  raw_status = do_system(&strelt(a,1));  /* waitpid() may set errno */
  set_raw_status(raw_status);  /* no_status (OM) if do_system() failed */
  return l_status();  /* SYSTEM returns the current value of STATUS */
} /* end l_system */

/*
 *  SYS_READ(fd, n)  -- get n bytes from fd using POSIX read()
 */
string *l_sys_read(integer *a, integer *b) {
  int fd;
  long n;
  ssize_t k;
  string *r;
  fd = get_long_in(a, fd_lo,fd_hi-1, "first arg to SYS_READ (fd)");
  n = get_nat_long(b, "second arg to SYS_READ (number of bytes)");
  if (restricted) runerr("SYS_READ is restricted");
  r = new_estring(n);
  k = os_read(fd, &strelt(r,1), n);
  if (k < 0) return OM;  /* with errno set by read() */
  if (k < n) str_resize(&r,k);
  return r;  /* with errno not set */
} /* end l_sys_read */

/*
 *  SYS_WRITE(fd, s)  -- write STRING s to fd and get POSIX write() result
 */
integer *l_sys_write(integer *a, string *b) {
  int fd;
  long n;
  ssize_t k;
  if (!is_string(b)) {
    binary_proc_error ("SYS_WRITE", a, b, "(INTEGER, STRING)");
  }
  fd = get_long_in(a, fd_lo,fd_hi-1, "first arg to SYS_WRITE (fd)");
  if (restricted) runerr("SYS_WRITE is restricted");
  n = b->nchar;
  k = os_write(fd, &strelt(b,1), n);  /* write() may set errno */
  return new_integer(k);
} /* end l_sys_write */

/*
 *  OP TAN(a);                      $ tangent
 *    CASE OF
 *    (IS_REAL a):
 *      RETURN "tangent of a";
 *    ELSE ERROR;
 *    END CASE;
 *  END OP TAN;
 */
real *l_tan(real *a) {
  double x = get_double(a,"TAN operand");
  return new_real(tan(x));
} /* end l_tan */

/*
 *  OP TANH(a);                      $ hyperbolic tangent
 *    CASE OF
 *    (IS_REAL a):
 *      RETURN "hyperbolic tangent of a";
 *    ELSE ERROR;
 *    END CASE;
 *  END OP TANH;
 */
real *l_tanh(real *a) {
  double x = get_double(a,"TANH operand");
  return new_real(tanh(x));
} /* end l_tanh */

/*
 *  TCGETPGRP(f)  -- foreground process group ID for stream f if open
 *                   on caller's controlling terminal
 *
 *  Actually the rules are a little subtle if f is so open but there is
 *  no foreground process group, because POSIX tcgetpgrp() then gives a
 *  value that is greater than 1 but is not a pgid.  We return it in
 *  that case anyway.  A return of -1 from tcgetpgrp() sets LAST_ERROR
 *  and makes us return OM.
 */
integer *l_tcgetpgrp(block *a) {
  int fd;
  pid_t pgid;
  if (restricted) runerr("TCGETPGRP is restricted");
  if (!is_stream_id(a)) {
    unary_proc_error ("TCGETPGRP", a, STREAM_ID);
  }
  fd = getfd (a, getfd_info);
  pgid = os_tcgetpgrp(fd);  /* tcgetpgrp() may set errno and give -1 */
  return pgid != -1 ? new_integer(pgid) : OM;
} /* end l_tcgetpgrp */

/*
 *  TCSETPGRP(f, p);  -- set foreground process group ID for stream f
 *                       if open on caller's controlling terminal and
 *                       provided p is a pgid in the caller's session
 */
void l_tcsetpgrp(block *a, integer *b) {
  int fd;
  pid_t pgid;
  if (restricted) runerr("TCSETPGRP is restricted");
  if (!is_stream_id(a) ||
      !is_integer(b)) {
    binary_proc_error ("TCSETPGRP", a, b, "(" STREAM_ID ", INTEGER)");
  }
  fd = getfd (a, getfd_info);
  pgid = get_nat_pid_t(b, "second arg to TCSETPGRP (process group ID)");
  os_tcsetpgrp(fd, pgid);  /* tcsetpgrp() may set errno */
} /* end l_tcsetpgrp */

/*
 *  TIE(a, b);  -- auto-flush a on seq read from b, and vice versa
 *
 *  Any existing TIE associations a or b may have are first dissolved.
 */
void l_tie(block *a, block *b) {
  int fd, gd;
  file *f, *g;
  HANDLE ha = ref(a);
  HANDLE hb = ref(b);
  if (!is_stream_id(a) ||
      !is_stream_id(b)) {
    binary_proc_error ("TIE", a, b, "(" STREAM_ID "," STREAM_ID ")");
  }
  fd = getfd (a, getfd_info);
  gd = getfd (b, getfd_info);
  f = find_file(fd);
  g = find_file(gd);
  if ((test_all(f->abilities, can_read) &&
       test_all(g->abilities, can_write)) ||
      (test_all(f->abilities, can_write) &&
       test_all(g->abilities, can_read))) {
    file_tie(fd, gd);
  } else {
    runerr("Inappropriate stream pairing for TIE");
  }
  retire(hb);
  retire(ha);
} /* end l_tie */

/*
 *  TIME
 *
 *  Milliseconds of CPU time used by this process and all its completed
 *  child processes so far
 */
integer *l_time(void) {
  return integer_timeval_to_ms(os_cputime());
} /* end l_time */

void l_title(tuple *a, long nargs) {
  check_optargs_type(a,"TITLE");
  runerr("TITLE not implemented");
} /* end l_title */

/*
 *  Deprecated and obsolescent - please use MKSTEMP instead
 *
 *  Now also commented out in setl-lib.texi doc, though not removed
 *  from service, as it is a convenient thing to have in limited envts
 *  where conflicts over /tmp namespace are not an issue.  No point in
 *  wantonly breaking old code either.
 */
string *l_tmpnam(void) {
  const char *t;
  if (restricted) {
    runerr("TMPNAM is restricted");
  }
  t = os_tmpnam();  /* tmpnam() may set errno */
  return t != NULL ? new_string(t) : OM;
} /* end l_tmpnam */

/*
 *  TO_LOWER a  -- convert STRING a to lowercase
 */
string *l_to_lower(string *a) {
  if (!is_string(a)) {
    unary_op_error ("TO_LOWER", a, "STRING");
  }
  return str_lower(a);
} /* end l_to_lower */

/*
 *  TO_UPPER a  -- convert STRING a to uppercase
 */
string *l_to_upper(string *a) {
  if (!is_string(a)) {
    unary_op_error ("TO_UPPER", a, "STRING");
  }
  return str_upper(a);
} /* end l_to_upper */

/*
 *  TOD  -- "time of day" (milliseconds since the beginning of 1970 UTC)
 */
integer *l_tod(void) {
  return integer_timespec_to_ms(os_tod());
} /* end l_tod */

/*
 *  TTY_PUMP  -- like PUMP but with pty pair rather than socketpair
 */
integer *l_tty_pump(void) {
  int fd = local_tty_pump(duplex);  /* may abend, not set errno */
  return new_integer(fd);
} /* end l_tty_pump */

/*
 *  TYPE a  -- STRING mnemonic for a's current type, e.g., 'SET'
 */
string *l_type(block *a) {
  return new_string(TYPENAME(a));
} /* end l_type */

/*
 *  UMASK or UMASK()  -- returns current file creation (mode bits) mask
 *
 *  UMASK(mask)  -- also sets the mask to a new value
 *
 *  UMASK always succeeds, and does not modify errno.
 */
integer *l_umask(tuple *a, long nargs) {
  check_optargs_type(a,"UMASK");
  if (nargs == 0) {
    mode_t mask = os_get_umask();  /* current mask */
    return new_integer((long)mask);
  } else if (nargs == 1) {
    mode_t new_mask, old_mask;
    integer *x = a->nelt == 0 ? OM : (integer *)tupelt(a,1);
    if (!is_integer(x)) {
      unary_proc_error ("UMASK", x, "INTEGER or no");
    }
    new_mask = get_mode_t(x, "UMASK arg");
    if (restricted) {
      /* In restricted mode, do not allow the mask to be more permissive
       * than it was in the SETL program's initial environment.  */
      new_mask |= initial_umask;
    }
    old_mask = os_set_umask(new_mask);
    return new_integer((long)old_mask);
  } else {
    runerr("UMASK takes at most 1 arg, not %ld args", nargs);
  }
} /* end l_umask */

/*
 *  UNGETC(f, s);  -- push STRING s back onto stream f
 */
void l_ungetc(block *a, string *b) {
  HANDLE hb = ref(b);
  int fd;
  file *f;
  if (!is_stream_id(a) ||
      !is_string(b)) {
    binary_proc_error ("UNGETC", a, b, "(" STREAM_ID ", STRING)");
  }
  /* This is getfd_info rather than getfd_read, so that the stream
   * won't be auto-opened, since we can't allow UNGETC (of more than
   * 0 chars) if the input buffer is empty:  */
  fd = getfd (a, getfd_info);
  f = find_file(fd);
  if (!test_all(f->abilities, can_read)) {
    stream_error("Stream %@ is not open for input", a);
  }
  unget_chars (f, b);  /* does not set errno */
  retire(hb);
} /* end l_ungetc */

/*
 *  UNGETCHAR(s);  -- push STRING s back onto STDOUT
 */
void l_ungetchar(string *a) {
  file *f;
  if (!is_string(a)) {
    unary_proc_error ("UNGETCHAR", a, "STRING");
  }
  f = find_file_unchecked(fd_stdin);
  if (!test_all(f->abilities, can_read)) {
    runerr("Standard input stream is not open for input");
  }
  unget_chars (f, a);  /* does not set errno */
} /* end l_ungetchar */

/*
 *  UNHEX a  -- s = UNHEX HEX s = UNHEX TO_LOWER HEX s = UNHEX TO_UPPER HEX s
 */
string *l_unhex(string *a) {
  HANDLE ha;
  string *r;
  long i, n;
  if (!is_string(a)) {
    unary_op_error ("UNHEX", a, "STRING");
  }
  if (a->nchar % 2 != 0) return OM;
  ha = ref(a);
  n = a->nchar / 2;
  r = new_estring(n);
  retire(ha);
  for (i=1; i<=n; i++) {
    uchar c = strelt(a,2*i-1);
    uchar d = strelt(a,2*i  );
    if (!hexdigit[c] || !hexdigit[d]) return OM;
    strelt(r,i) = (hexval[c]<<4) | hexval[d];
  }
  return r;
} /* end l_unhex */

/*
 *  UNLINK(a);  -- remove pathname from filesystem
 */
void l_unlink(string *a) {
  if (!is_string(a)) {
    unary_proc_error ("UNLINK", a, "STRING");
  }
  if (restricted) runerr("UNLINK is restricted");
  os_unlink(&strelt(a,1));  /* unlink() may set errno */
} /* end l_unlink */

/*
 *  Unary operators to unpack C number reprs from strings.
 *
 *  These yield INTEGERs, and expect a STRING whose length is
 *  determined by the C ABI of the GNU SETL build of the current
 *  interpreter (SETL VM):
 *
 *   UNPACK_SHORT a
 *   UNPACK_UNSIGNED_SHORT a
 *   UNPACK_INT a
 *   UNPACK_UNSIGNED_INT a
 *   UNPACK_LONG a
 *   UNPACK_UNSIGNED_LONG a
 *   UNPACK_LONG_LONG a
 *   UNPACK_UNSIGNED_LONG_LONG a
 *
 *   UNPACK_INTEGER a  -- unbounded
 *
 *  These yield REALs:
 *
 *   UNPACK_DOUBLE a
 *   UNPACK_FLOAT a
 *
 *   UNPACK_REAL a
 */

integer *l_unpack_short(string *a) {
  short i;
  if (!is_string(a)) {
    unary_op_error ("UNPACK_SHORT", a, "STRING");
  }
  if (a->nchar != sizeof i) {
    runerr("UNPACK_SHORT operand must have %zu characters, not %ld",
                                      sizeof i,           a->nchar);
  }
  mvmem (&i, &strelt(a,1), sizeof i);
  return new_integer(i);
} /* end l_unpack_short */

integer *l_unpack_unsigned_short(string *a) {
  unsigned short i;
  if (!is_string(a)) {
    unary_op_error ("UNPACK_UNSIGNED_SHORT", a, "STRING");
  }
  if (a->nchar != sizeof i) {
    runerr("UNPACK_UNSIGNED_SHORT operand must have %zu characters, not %ld",
                                               sizeof i,           a->nchar);
  }
  mvmem (&i, &strelt(a,1), sizeof i);
  return ulong_integer(i);
} /* end l_unpack_unsigned_short */

integer *l_unpack_int(string *a) {
  int i;
  if (!is_string(a)) {
    unary_op_error ("UNPACK_INT", a, "STRING");
  }
  if (a->nchar != sizeof i) {
    runerr("UNPACK_INT operand must have %zu characters, not %ld",
                                    sizeof i,           a->nchar);
  }
  mvmem (&i, &strelt(a,1), sizeof i);
  return new_integer(i);
} /* end l_unpack_int */

integer *l_unpack_unsigned_int(string *a) {
  unsigned int i;
  if (!is_string(a)) {
    unary_op_error ("UNPACK_UNSIGNED_INT", a, "STRING");
  }
  if (a->nchar != sizeof i) {
    runerr("UNPACK_UNSIGNED_INT operand must have %zu characters, not %ld",
                                             sizeof i,           a->nchar);
  }
  mvmem (&i, &strelt(a,1), sizeof i);
  return ulong_integer(i);
} /* end l_unpack_unsigned_int */

integer *l_unpack_long(string *a) {
  long i;
  if (!is_string(a)) {
    unary_op_error ("UNPACK_LONG", a, "STRING");
  }
  if (a->nchar != sizeof i) {
    runerr("UNPACK_LONG operand must have %zu characters, not %ld",
                                     sizeof i,           a->nchar);
  }
  mvmem (&i, &strelt(a,1), sizeof i);
  return new_integer(i);
} /* end l_unpack_long */

integer *l_unpack_unsigned_long(string *a) {
  unsigned long i;
  if (!is_string(a)) {
    unary_op_error ("UNPACK_UNSIGNED_LONG", a, "STRING");
  }
  if (a->nchar != sizeof i) {
    runerr("UNPACK_UNSIGNED_LONG operand must have %zu characters, not %ld",
                                              sizeof i,           a->nchar);
  }
  mvmem (&i, &strelt(a,1), sizeof i);
  return ulong_integer(i);
} /* end l_unpack_unsigned_long */

integer *l_unpack_long_long(string *a) {
#if HAVE_LLONG
  llong i;
  if (!is_string(a)) {
    unary_op_error ("UNPACK_LONG_LONG", a, "STRING");
  }
  if (a->nchar != sizeof i) {
    runerr("UNPACK_LONG_LONG operand must have %zu characters, not %ld",
                                          sizeof i,           a->nchar);
  }
  mvmem (&i, &strelt(a,1), sizeof i);
  return llong_integer(i);
#else
  runerr("UNPACK_LONG_LONG not available on this platform");
#endif
} /* end l_unpack_long_long */

integer *l_unpack_unsigned_long_long(string *a) {
#if HAVE_LLONG
  ullong i;
  if (!is_string(a)) {
    unary_op_error ("UNPACK_UNSIGNED_LONG_LONG", a, "STRING");
  }
  if (a->nchar != sizeof i) {
    runerr("UNPACK_UNSIGNED_LONG_LONG operand must have %zu characters, not %ld",
                                                   sizeof i,           a->nchar);
  }
  mvmem (&i, &strelt(a,1), sizeof i);
  return ullong_integer(i);
#else
  runerr("UNPACK_UNSIGNED_LONG_LONG not available on this platform");
#endif
} /* end l_unpack_unsigned_long_long */

/*
 *  See PACK_INTEGER comments.
 */
integer *l_unpack_integer(string *a) {
  HANDLE ha = ref(a);
  integer *r;
  ssize_t size;
  size_t n;
  const long limbs_offset = sizeof size;
  mvmem (&size, &strelt(a, 1), limbs_offset);
  r = alloc_integer(ABS(size));
  r->size = size;
  n = sizeof_limbs(r);  /* #bytes in the limbs */
  mvmem (r->limbs, &strelt(a, 1 + limbs_offset), n);
  retire(ha);
  return r;
} /* end l_unpack_integer */

/*
 *  See PACK_DOUBLE comments.
 */
real *l_unpack_double(string *a) {
  double x;
  if (!is_string(a)) {
    unary_op_error ("UNPACK_DOUBLE", a, "STRING");
  }
  if (a->nchar != sizeof x) {
    runerr("UNPACK_DOUBLE operand must have %zu characters, not %ld",
                                       sizeof x,           a->nchar);
  }
  mvmem (&x, &strelt(a,1), sizeof x);
  return new_real(x);
} /* end l_unpack_double */

real *l_unpack_float(string *a) {
  float x;
  if (!is_string(a)) {
    unary_op_error ("UNPACK_FLOAT", a, "STRING");
  }
  if (a->nchar != sizeof x) {
    runerr("UNPACK_FLOAT operand must have %zu characters, not %ld",
                                      sizeof x,           a->nchar);
  }
  mvmem (&x, &strelt(a,1), sizeof x);
  return new_real(x);  /* x automatically promoted to 'double' here */
} /* end l_unpack_float */

/*
 *  See PACK_REAL comments.
 */
real *l_unpack_real(string *a) {
  double x;
  if (!is_string(a)) {
    unary_op_error ("UNPACK_REAL", a, "STRING");
  }
  if (a->nchar != sizeof x) {
    runerr("UNPACK_REAL operand must have %zu characters, not %ld",
                                     sizeof x,           a->nchar);
  }
  mvmem (&x, &strelt(a,1), sizeof x);
  return new_real(x);
} /* end l_unpack_real */

/*
 *  UNPRETTY a  -- content of a STRING that PRETTY might have produced
 *
 *  UNPRETTY accepts a "pretty" string, which is somewhat restricted
 *  (and formal).  Its first and last characters must match and be
 *  either an apostrophe (single quote) or double quote.  Call this
 *  the "quote" char.  The only chars allowed between the quotes are
 *  the 95 "printable" ASCII chars, including blank.
 *
 *  UNPRETTY makes the following interpretations in transforming your
 *  pretty string into an unrestricted one.
 *
 *  An internal quote is represented as a consecutive pair of quotes,
 *  or by a backslash-escaped quote, or by an octal or hex escape.
 *
 *  Backslash followed by any of the 32 "glyphs" (that is, all of the
 *  ASCII "printable" characters apart from alphanumerics and blank)
 *  means just that glyph.
 *
 *  Backslash followed by up to 3 octal digits starting with 0-3 means
 *  a character having the bit pattern indicated by the digits, as in C.
 *
 *  Backslash followed by x and then 1 or 2 hexadecimal digits is an
 *  alternative to the octal escape.
 *
 *   N.B. If CHAR_BIT is more than 8 on your system, this convention
 *   should be relaxed to allow more octal or hexadecimal digits after
 *   the backslash.  This will require you to write some new code in
 *   the body of 'str_undress' below.  Please "#if" based on CHAR_BIT
 *   or provide more general code.
 *
 *  Backslash followed by a,b,f,n,r,t, or v means the same thing as
 *  it does in C (i.e., audible alarm, backspace, formfeed, newline,
 *  carriage return, horizontal tab, or vertical tab respectively).
 *
 *  Nothing else is permitted after backslash.
 *
 *  Currently these are also the rules governing what can be in a
 *  literal character string in SETL source code, and the escape
 *  sequences there have the same meanings.
 *
 *  The rules may be liberalized in the future, however, because I
 *  don't think insisting on the use of octal or hexadecimal escapes
 *  for specifying, say, the ESC character is more portable than just
 *  embedding the thing right into a string---a good ASCII<->EBCDIC
 *  translation of the source code would leave ESC meaning ESC either
 *  way, whereas the use of \x1b (the ASCII code for ESC) would be an
 *  error in an EBCDIC environment if the source code was just
 *  translated and otherwise unmodified.
 *
 *  Maybe you want an escape convention that allows special characters
 *  to be identified by name, e.g., ESC, SOH, SO, etc.  Possible TODO.
 */
string *l_unpretty(string *a) {
  if (!is_string(a)) {
    unary_op_error ("UNPRETTY", a, "STRING");
  }
  return str_undress(a, runerr);
} /* end l_unpretty */

/*
 *  UNSETCTTY(f);  -- give up controlling terminal attached to stream f
 */
void l_unsetctty(block *a) {
  int fd;
  if (restricted) runerr("UNSETCTTY is restricted");
  if (!is_stream_id(a)) {
    unary_proc_error ("UNSETCTTY", a, STREAM_ID);
  }
  fd = getfd (a, getfd_info);
  os_unsetctty(fd);  /* ioctl() or lack of TIOCNOTTY may set errno */
} /* end l_unsetctty */

/*
 *  UNSETENV(a);  -- remove environment variable
 *
 *  See also SETENV.
 */
void l_unsetenv(string *a) {
  if (!is_string(a)) {
    unary_proc_error ("UNSETENV", a, "STRING");
  }
  if (restricted) runerr("UNSETENV is restricted");
  os_unsetenv(&strelt(a,1));  /* abends program on error */
} /* end l_unsetenv */

/*
 *  UNSTR s  -- "read" a value from s, which might have come from STR
 *
 *  UNSTR abends if it cannot recognize a value (which may be preceded
 *  and/or followed only by whitespace).  See also VAL.
 */
block *l_unstr(string *a) {
  HANDLE ha = ref(a);
  int stopper = EOF;    /* "top-level" */
  long i = 1;           /* string index */
  block *r;
  if (!is_string(a)) {
    unary_op_error ("UNSTR", a, "STRING");
  }
  span_white(a, &i);  /* skip leading whitespace if any */
  if (i > a->nchar) {  /* empty string or nothing but whitespace */
    runerr("No denotation (token) found in UNSTR operand");
  }
  r = unstr(a, &i, &stopper, "UNSTR operand");
  span_white(a, &i);  /* skip whitespace after the denotation */
  if (i <= a->nchar) {  /* that still left some junk in the string */
    runerr("Junk after initial denotation in \"%s\" in UNSTR operand",
                                   tame(&strelt(a,1)));
  }
  retire(ha);
  return r;
} /* end l_unstr */

/*
 *  UNTIE(a, b);  -- for symmetry with TIE, undo its effects
 */
void l_untie(block *a, block *b) {
  int fd, gd;
  file *f, *g;
  HANDLE ha = ref(a);
  HANDLE hb = ref(b);
  if (!is_stream_id(a) ||
      !is_stream_id(b)) {
    binary_proc_error ("UNTIE", a, b, "(" STREAM_ID "," STREAM_ID ")");
  }
  fd = getfd (a, getfd_info);
  gd = getfd (b, getfd_info);
  f = find_file(fd);
  g = find_file(gd);
  if (f->tie == gd && g->tie == fd) {
    file_untie(fd);
  } else {
    runerr("Cannot UNTIE streams that are not already linked with TIE");
  }
  retire(hb);
  retire(ha);
} /* end l_untie */

/*
 *  VAL s  -- like UNSTR s for numbers, but gives OM on recog failure
 */
block *l_val(string *a) {
  if (!is_string(a)) {
    unary_op_error ("VAL", a, "STRING");
  }
  return val(a);
} /* end l_val */

/*
 *  WAIT(...) is the same as WAITPID(-1,...).
 *
 *  So WAIT() means WAIT(TRUE), which means block until a child process
 *  exits; WAIT(FALSE) means do not block, and yields 0 immediately if
 *  no child has yet completed.
 *
 *  Actually, errors can cause a yield of -1 too, but the "normal"
 *  return is the pid of a child process that has terminated, in which
 *  case STATUS is then valid too (otherwise it is OM, and errno is set).
 */
integer *l_wait(tuple *a, long nargs) {
  int waitflags;
  pid_t r;
  check_optargs_type(a,"WAIT");
  if (nargs == 0) {
    waitflags = 0;
  } else if (nargs == 1) {
    boolean *x = a->nelt == 0 ? OM : (boolean *)tupelt(a,1);
    if (!is_boolean(x)) {
      unary_proc_error ("WAIT", x, "BOOLEAN or no");
    }
    waitflags = x->booval ? 0 : WNOHANG;  /* FALSE => WNOHANG */
  } else {
    runerr("WAIT takes at most 1 (TRUE or FALSE) arg, not %ld args",
                                                             nargs);
  }
#ifdef WCONTINUED
  r = do_waitpid(-1, waitflags | WUNTRACED | WCONTINUED);
#else
  r = do_waitpid(-1, waitflags | WUNTRACED);
#endif
  return new_integer(r);
} /* end l_wait */

/*
 *  WAITPID(pid)  -- waitflag defaults to TRUE
 *  WAITPID(pid, waitflag)
 *
 *  The sensible alternative to WAIT; the 1st arg is a pid, or
 *  in POSIX (opengroup.org) terms:
 *
 *   - If pid is equal to -1, status is requested for any
 *  child process. In this respect, waitpid() is then equivalent
 *  to wait().
 *
 *   - If pid is greater than 0, it specifies the process ID of a
 *  single child process for which status is requested.
 *
 *   - If pid is 0, status is requested for any child process whose
 *  process group ID is equal to that of the calling process.
 *
 *   - If pid is less than -1, status is requested for any
 *  child process whose process group ID is equal to the absolute
 *  value of pid.
 *
 *  The optional 2nd arg controls whether WNOHANG is cleared, as
 *  with WAIT.  It defaults to TRUE (do wait).
 *
 *  The return value is:
 *
 *   - If >0, the pid of a successfully waited-for child.  In this
 *  case, set_raw_status() is called with the raw status coughed up
 *  by os_waitpid().  STATUS is derived from the saved raw status.
 *
 *   - If 0, it means that the optional arg to WAITPID was specified
 *  as FALSE ("don't actually wait") and there were no children with
 *  status available.  STATUS is set to OM.
 *
 *   - If -1, waitpid() returned -1 and errno (LAST_ERROR) was set.
 *  STATUS is set to OM.
 *
 *  The main purpose of WAITPID is to wait for children created
 *  by FORK, though it has some other marginal uses too such as
 *  pre-empting CLOSE on a pipe/pump stream or reaping status from
 *  a zombie created by CLOSE(fd, CLOSE_ZOMBIE).
 *
 *  It also now returns when a selected child stops or continues,
 *  setting STATUS accordingly (see l_status()).
 *
 *  SIGCHLD is blocked around the os_waitpid() call.
 */
integer *l_waitpid(integer *a, tuple *b, long nargs) {
  int waitflags;
  pid_t p;  /* arg */
  pid_t r;  /* ret */
  p = get_pid_t(a, "first arg to WAITPID (e.g. pid or -pgid)");
  check_optargs_type(b,"WAITPID");
  if (nargs == 1) {
    waitflags = 0;
  } else if (nargs == 2) {
    boolean *x = b->nelt == 0 ? OM : (boolean *)tupelt(b,1);
    if (!is_boolean(x)) {
      binary_proc_error ("WAITPID", a, x, "(INTEGER, optional BOOLEAN)");
    }
    waitflags = x->booval ? 0 : WNOHANG;  /* FALSE => WNOHANG */
  } else {
    runerr("WAITPID takes 1 or 2 args, not %ld", nargs);
  }
#ifdef WCONTINUED
  r = do_waitpid(p, waitflags | WUNTRACED | WCONTINUED);
#else
  r = do_waitpid(p, waitflags | WUNTRACED);
#endif
  return new_integer(r);
} /* end l_waitpid */

/*
 *  WHOLE(v,w)  -- STRING rendering of number v with field width w
 *
 *  If w is negative, the result is left-justified.
 *
 *  REAL v is truncated as if by FIX.
 */
string *l_whole(block *v, integer *w) {
  long wid = get_long(w, "second arg to WHOLE (width)");
  switch (setl_type(v)) {
  case integer_type:
    {
      string *s = tostr(v);
      if (wid > s->nchar) {
        return str_lpad(s,wid,' ');
      } else if (-wid > s->nchar) {
        return str_rpad(s,-wid,' ');
      } else {
        return s;
      }
    }
  case real_type:
    {
      double d = round(real_to_double((real *)v));
      bool pad = true;
      string *s;
      if (isnan(d)) {
        s = new_string("nan");
      } else if (isinf(d)) {
        s = new_string(signbit(d) ? "-inf" : "inf");
      } else {
        HANDLE hw = ref(w);
        integer *i = double_to_integer(d);  /* like FIX */
        if (i != OM) {
          s = l_whole((block *)i, w);  /* call self on integer */
          pad = false;  /* already padded */
        } else {
          /* Since we already covered NaNs and infinities above, OM
           * should theoretically be impossible here; we rather
           * arbitrarily give "nan" for this conversion failure.  */
          s = new_string("nan");
        }
        retire(hw);
      }
      if (pad && wid > s->nchar) {
        return str_lpad(s,wid,' ');
      } else if (pad && -wid > s->nchar) {
        return str_rpad(s,-wid,' ');
      } else {
        return s;
      }
    }
  default:
    binary_proc_error ("WHOLE", v, w, "(INTEGER or REAL, INTEGER)");
  }
} /* end l_whole */

/*
 *  OP WITH(a,b);
 *    CASE OF
 *    (IS_SET a):                         $ add element to set
 *      RETURN {x : x IN [y : y IN a] WITH b};
 *    (IS_TUPLE a):                       $ add element to tuple
 *      x := a;
 *      x(#x+1) := b;
 *      RETURN x;
 *    ELSE ERROR;
 *    END CASE;
 *  END OP WITH;
 */
block *l_with(block *a, block *b) {
  if (is_set(a)) {
    HANDLE hb = ref(b);
    set *r = copy_set((set *)a);
    if (!is_om(b)) set_insert(&r,b);
    retire(hb);
    return (block *)r;
  } else if (is_tuple(a)) {
    HANDLE hb = ref(b);
    tuple *r = copy_tuple((tuple *)a);
    if (!is_om(b)) tup_tackon(&r,b);
    retire(hb);
    return (block *)r;
  } else {
    runerr("First operand of WITH must be SET or TUPLE, not %s",
                                                          TYPENAME(a));
  }
} /* end l_with */

/*
 *  a WITH:= b;  -- add element b to SET a or append b to TUPLE a
 */
void l_awith(block **a, block *b) {
  block *t = *a;
  if (is_set(t)) {
    if (!is_om(b)) set_insert((set **)(void *)&t,b);
  } else if (is_tuple(t)) {
    if (!is_om(b)) tup_tackon((tuple **)(void *)&t,b);
  } else {
    runerr("First operand of WITH:= must be SET or TUPLE, not %s",
                                                          TYPENAME(t));
  }
  *a = t;
} /* end l_awith */

/*
 *  WRITE(a(*));  -- write 0 or more values to STDOUT
 */
void l_write(tuple *a, long nargs) {
  HANDLE ha = ref(a);
  file *f;
  check_optargs_type(a,"WRITE");
  check (fd_stdout == getfd ((block *)stdout_integer, getfd_write));
  f = find_fd_file(fd_stdout);
  /* put_values() or put_char() may set errno */
  if (put_values (f, a, nargs)) put_char ('\n', f->buffer);
  retire(ha);
} /* end l_write */

/*
 *  WRITEA(a, b(*));  -- write 0 or more values to stream a
 */
void l_writea(block *a, tuple *b, long nargs) {
  /* Functionally identical to PUTB */
  HANDLE hb = ref(b);
  int fd;
  file *f;
  if (!is_stream_id(a)) {
    unary_variadic_error ("WRITEA", a, STREAM_ID);
  }
  check_optargs_type(b,"WRITEA");
  fd = getfd (a, getfd_write);
  f = find_fd_file(fd);
  /* put_values() or put_char() may set errno */
  if (put_values (f, b, nargs-1)) put_char ('\n', f->buffer);
  retire(hb);
} /* end l_writea */


/*
 *  Check that the translator has emitted code which results in a
 *  SETL tuple being passed as the final argument to routines that
 *  expect a variable number of them (as signified by "(*)" after
 *  the last formal in a SETL procedure):
 */
static void check_optargs_type (tuple *a, const char *what) {
  if (!is_tuple(a)) {
    tranerr("Expected trailing TUPLE but got %s on call to %s",
                                     TYPENAME(a),        what);
  }
}

static bool is_local_subprocess (pid_t pid) {
  if (pid > 0) {  /* pid of a single process, not a group */
    pid_t caller_pid = os_getpid();
    int fd;
    for (fd=fd_lo; fd<fd_hi; fd++) {
      file *f = find_file_unchecked(fd);
      if (f->local_child &&
          f->pid == pid &&
          f->ppid == caller_pid) {
        /* Child was created by PIPE_FROM_CHILD, PIPE_TO_CHILD, PUMP, or
         * TTY_PUMP in the current process (caller is its parent).  */
        return true;
      }
    }
  }
  return false;
}

static bool is_allowed (allowed *list, const char *what) {
  for (; list != NULL; list = list->next) {
    if (leq(list->what, what)) {
      return true;
    }
  }
  return false;
}

/*
 *  Helper for GET and GETA:  get_lines(f, n)
 *
 *  Returns a TUPLE of up to n STRINGs representing input lines from
 *  stream f, robbed of their trailing newlines.
 *
 *  The last line in an input stream need not be newline-terminated.
 *
 *  If no lines could be read because an end of input was hit first,
 *  the EOF indicators are set (and maybe LAST_ERROR).  If at least
 *  one line was read first, the indicators remain pending.
 *
 *  See also the general info about readers just above l_get().
 */
static tuple *get_lines (file *f, long n) {
  tuple *r = OM;  HANDLE hr = ref(r);
  io_buffer *p = f->buffer;
  long i;
  assert (p != NULL);
  r = new_tuple(n);
  clear_eof(p);  /* clear SETL-level EOF indicators but not eof_pending */
  for (i=1; i<=n; i++) {  /* loop over lines in r indexed by i */
    int c;
    string *line;  HANDLE h_line;
    /* If no initial char for this line, leave the loop over lines
     * without creating a string for this line.  */
    if (peek_char(p) == EOF) {  /* no initial char for this line */
      break;  /* exit for-loop with p->eof_pending set */
    }
    line = null_string();
    h_line = ref(line);
    while ((c=get_char(p))!=EOF && c!='\n') {
      str_tackon(&line,c);
    }
    tupelt(r,i) = (block *)line;
    retire(h_line);
    /* If eof_pending (no \n terminating this line), the peek_char() on
     * the next loop round if any will see it as EOF and break out of
     * the loop.  */
  }
  tup_truncate(&r);
  if (r->nelt == 0 && p->eof_pending) {  /* no lines read; eof hit */
    /* We don't assert n > 0 here, because p->eof_pending is allowed to
     * have been set before the get_lines(), e.g. on a previous call.
     * So a call to GET or GETA with no input-receiving args can still
     * set EOF.  */
    eof_file(f);  /* adjust EOF indicators; auto-close if appropriate */
  } else {  /* got lines, and/or eof not hit */
    /* It cannot be the case that #r is 0 when n > 0 here, else
     * eof_pending would have been raised.  And of course #r can never
     * be > 0 if n (the number of lines requested) is 0.  Truly an
     * insane sanity check, this is, though obviously provably valid.  */
    assert ((r->nelt == 0) == (n == 0));  /* a iff b */
  }
  retire(hr);
  return r;  /* a tuple of 0 or more lines as strings */
} /* end get_lines */

/*
 *  Helper for GETB, READ and READA:  get_values(f, n, what, eat_line)
 *
 *  Returns a TUPLE of up to n values parsed from stream f (possibly
 *  fewer than n if an end of input is encountered).  The input value
 *  denotations are separated by whitespace ('[ \f\n\r\t\v]+'), and
 *  converted as if by UNSTR.
 *
 *  The 'what' arg is "GETB input", "READ input", or "READA input".
 *
 *  The eat_line arg is true for READ and READA, false for GETB, and
 *  says whether bytes should continue to be read after the last value
 *  requested has been read, until \n is read or end of input is hit.
 *
 *  If no values could be read because an end of input was hit first,
 *  the EOF indicators are set (and maybe LAST_ERROR).  If at least
 *  one value was read first, the indicators remain pending.
 *
 *  See also the general info about readers just above l_get().
 */
static tuple *get_values (file *f, long n, const char *what, bool eat_line) {
  tuple *r = OM;  HANDLE hr = ref(r);
  io_buffer *p = f->buffer;
  long i;
  assert (p != NULL);
  r = new_tuple(n);
  clear_eof(p);  /* clear SETL-level EOF indicators but not eof_pending */
  for (i=1; i<=n; i++) {  /* loop over values in r indexed by i */
    int stopper = EOF;
    eat_white(p);  /* read through any leading whitespace */
    let (tupelt(r,i), getval(p, &stopper, what));
    if (p->eof_pending) {  /* getval() reached end of file */
      break;  /* leave for-loop */
    }
  }
  if (eat_line) {  /* READ or READA */
    int c;
    /* Eat up all remaining characters on the current line.  If eof
     * is encountered before \n, p->eof_pending is set true.  */
    while ((c=get_char(p))!=EOF && c!='\n') ;
  } /* end for i */
  tup_truncate(&r);
  /* Note that a READ or READA with no input-receiving args is a valid
   * way to skip an input line.  If there is not even an empty line
   * (\n) but only the end of file as reflected in p->eof_pending,
   * therefore, it is appropriate that in the eat_line case, we set
   * the SETL-level EOF indicators.  By contrast, in the non-eat_line
   * case (GETB), no input-receiving args means no setting of SETL-level
   * EOF indicators after their initial clearing, and is thus an
   * unconditional way of clearing them.  */
  if (r->nelt == 0 && p->eof_pending) {  /* no values read; eof hit */
    /* We don't assert n > 0 here, because p->eof_pending is allowed to
     * have been set before the call to get_values(), and additionally
.    * because in the eat_line case, p->eof_pending can be set even
     * when there are no input-receiving args.  */
    eof_file(f);  /* adjust EOF indicators; auto-close if appropriate */
  } else {  /* got values, or eof not hit */
    /* The only effect of eat_line being true is to add cases where
     * eof_pending might become true and thus trigger an eof_file()
     * above.  That does not change the relationship of #r to n for
     * READ/READA in comparison to GET/GETA, however, so we can evince
     * the same insanity here as in the similar context in get_lines().  */
    assert ((r->nelt == 0) == (n == 0));  /* a iff b */
  }
  retire(hr);
  return r;  /* a tuple of 0 or more values */
} /* end get_values */

/*
 *  Helper for GETC and GETCHAR:  get_one_char(f)
 *
 *  Returns a one-character STRING, or OM if it cannot, in which case
 *  the EOF indicators (and LAST_ERROR if appropriate) are set.
 *
 *  See also the general info about readers just above l_get().
 */
static string *get_one_char (file *f) {
  io_buffer *p = f->buffer;
  int c;
  assert (p != NULL);
  clear_eof(p);  /* clear SETL-level EOF indicators but not eof_pending */
  c = get_char(p);
  if (c != EOF) {
    return new_cstring(c);
  } else {  /* no char read; eof hit */
    eof_file(f);  /* adjust EOF indicators; auto-close if appropriate */
    return OM;  /* errno maybe set by get_char() and/or file_close() */
  }
} /* end get_one_char */

/*
 *  Helper for GETN and GETS:  get_chars(f, n)
 *
 *  Returns a STRING of length 0 to n.  If 0 when n > 0, the EOF
 *  indicators (and maybe LAST_ERROR) are set.  If fewer than n but
 *  more than 0, those indicators remain pending.
 *
 *  See also the general info about readers just above l_get().
 */
static string *get_chars (file *f, long n) {
  string *r;  HANDLE hr;
  io_buffer *p = f->buffer;
  long i;
  int c;
  assert (p != NULL);
  clear_eof(p);  /* clear SETL-level EOF indicators but not eof_pending */
  r = new_estring(n);
  hr = ref(r);
  for (i=1; i<=n && (c=get_char(p))!=EOF; i++) strelt(r,i) = c;
  if (i-1 < n) str_resize(&r,i-1);
  if (r->nchar == 0 && p->eof_pending) {  /* no chars read; eof hit */
    /* Direct-access files currently can be auto-opened, but are never
     * marked for auto-closing.  But we call eof_file() here to be like
     * other library input routines, which means this will work even if
     * that marking policy changes:  */
    eof_file(f);  /* adjust EOF indicators; auto-close if appropriate */
    /* r is returned as an empty string */
  } else {
    /* r is nonempty, or eof wasn't met yet */
  }
  retire(hr);
  return r;
} /* end get_chars */

/*
 *  Helper for PEEKC and PEEKCHAR:  peek_one_char(f)
 *
 *  Just like get_one_char(f), but if it returns a 1-character STRING,
 *  it also leaves that char in the input buffer.
 *
 *  See also the general info about readers just above l_get().
 */
static string *peek_one_char (file *f) {
  io_buffer *p = f->buffer;
  int c;
  assert (p != NULL);
  clear_eof(p);  /* clear SETL-level EOF indicators but not eof_pending */
  c = peek_char(p);
  if (c != EOF) {
    return new_cstring(c);
  } else {  /* no char read; eof hit */
    eof_file(f);  /* adjust EOF indicators; auto-close if appropriate */
    return OM;
  }
} /* end peek_one_char */

/*
 *  Helper for PUT, PUTA, and PUTLINE:  put_lines(f, t, n, what)
 *
 *  Writes the (NARGS-based) n STRINGS in TUPLE t out to f, with a
 *  newline after each.  The 'what' arg is "PUT", "PUTA", or "PUTLINE".
 *
 *  See also the general info about writers just above l_put().
 */
static bool put_lines (file *f, tuple *t, long n, const char *what) {
  io_buffer *p = f->buffer;
  long i;
  bool r;
  assert (p != NULL);
  r = true;
  for (i=1; i<=t->nelt && r; i++) {
    string *line = (string *)tupelt(t,i);
    if (!is_string(line)) {
      runerr("Line arg %ld to %s must be STRING, not %s",
                        i,   what,           TYPENAME(line));
    }
    r = put_chars(f,line);  /* false if put_chars() sets errno */
    r = r && put_char('\n',p);  /* false if put_char() sets errno */
  }
  if (n >= i) {
    runerr("Line arg %ld to %s must be STRING, not OM",
                      i,   what);
  }
  return r;
} /* end put_lines */

/*
 *  Helper for PUTB, WRITE, and WRITEA:  put_values(f, t, n)
 *
 *  Writes the (NARGS-based) n members of TUPLE t out to f, converted
 *  as if by STR and separated by spaces.
 *
 *  See also the general info about writers just above l_put().
 */
static bool put_values (file *f, tuple *t, long n) {
  HANDLE ht = ref(t);
  io_buffer *p = f->buffer;
  long i;
  bool r;
  assert (p != NULL);
  r = true;
  for (i=1; i<=t->nelt && r; i++) {
    if (i>1) r = put_char(' ',p);  /* false if put_char() sets errno */
    if (r) {
      block *q = tupelt(t,i);
      string *s = tostr(q);  /* effectively apply STR, even to STRINGs */
      r = put_chars(f,s);  /* false if put_chars() sets errno */
    }
  }
  for (; i<=n && r; i++) {
    if (i>1) r = put_char(' ',p);  /* false if put_char() sets errno */
    r = r && put_char('*',p);  /* false if put_char() sets errno */
  }
  retire(ht);
  return r;
} /* end put_values */

/*
 *  Helper for NPRINT, NPRINTA, PRINT, and PRINTA:  print_values(f, t, n)
 *
 *  Like put_values(), but only applies STR to non-STRINGs.
 */
static bool print_values (file *f, tuple *t, long n) {
  HANDLE ht = ref(t);
  io_buffer *p = f->buffer;
  long i;
  bool r;
  assert (p != NULL);
  r = true;
  for (i=1; i<=t->nelt && r; i++) {
    if (i>1) r = put_char(' ',p);  /* false if put_char() sets errno */
    if (r) {
      block *q = tupelt(t,i);
      /* Unlike put_values(), we apply tostr() only to non-STRINGs */
      string *s = is_string(q) ? (string *)q : tostr(q);
      r = put_chars(f,s);  /* false if put_chars() sets errno */
    }
  }
  for (; i<=n && r; i++) {
    if (i>1) r = put_char(' ',p);  /* false if put_char() sets errno */
    r = r && put_char('*',p);  /* false if put_char() sets errno */
  }
  retire(ht);
  return r;
} /* end print_values */

/*
 *  Helper for UNGETC and UNGETCHAR:  unget_chars(f, s);
 *
 *  Only a received string may be "pushed back" into the input buffer.
 *  Only one character of pushback is guaranteed to be possible, and
 *  even then only after a successful get_char() since initialization
 *  or the last drain or unget.
 *
 *  EOF is not cleared by a successful pushback of one or more chars,
 *  though it will be on the next successful get_char().  Nor is
 *  eof_pending cleared, so it can still turn into a "real" EOF after
 *  those pushed back chars are reread.  Likewise for pending_errno
 *  giving a deferred LAST_ERROR setting.
 *
 *  Anyone with a buffer can push back 0 bytes anytime, with no effect.
 */
static void unget_chars (file *f, string *s) {
  io_buffer *p = f->buffer;
  /* This may seem bold, but even a signal stream (like every other
   * readable stream) is allowed one character of pushback, which
   * happens to be implemented by giving the stream a buffer of which
   * one character is used:  */
  assert (p != NULL);
  if (s->nchar > 0) {
    const long n = s->nchar;
    const long room = p->j_in;
    if (n > room) {
      runerr("Cannot push back %ld characters onto stream", n);
    }
    /* Could optimize the common n == 1 case here:  */
    if (memcmp (&strelt(s,1), &p->in[room - n], n) != 0) {
      runerr("\"%s\" does not match input buffer contents",
           tame(&strelt(s,1)));
    }
    p->j_in -= n;
  }
} /* end unget_chars */


static void f_unary_op_error (const char *name, block *a,
                               const char *needed) {
  if (needed) {
    runerr("%s %s invalid - need %s operand",
         name, TYPENAME(a), needed);
  } else {
    runerr("%s %s invalid",
         name, TYPENAME(a));
  }
}

static void f_binary_op_error (const char *name, block *a, block *b,
                                const char *needed) {
  if (needed) {
    runerr("%s %s %s invalid - need %s",
     TYPENAME(a), name, TYPENAME(b), needed);
  } else {
    runerr("%s %s %s invalid - incompatible operands",
     TYPENAME(a), name, TYPENAME(b));
  }
}

static void f_unary_proc_error (const char *name, block *a,
                                 const char *needed) {
  if (needed) {
    runerr("%s (%s) invalid - need %s arg",
         name, TYPENAME(a), needed);
  } else {
    runerr("%s (%s) invalid",
         name, TYPENAME(a));
  }
}

static void f_binary_proc_error (const char *name, block *a, block *b,
                                  const char *needed) {
  if (needed) {
    runerr("%s (%s, %s) invalid - need %s",
         name, TYPENAME(a), TYPENAME(b), needed);
  } else {
    runerr("%s (%s, %s) invalid",
         name, TYPENAME(a), TYPENAME(b));
  }
}
static void f_ternary_proc_error (const char *name, block *a, block *b,
                                   block *c, const char *needed) {
  if (needed) {
    runerr("%s (%s, %s, %s) invalid - need %s",
         name, TYPENAME(a), TYPENAME(b), TYPENAME(c), needed);
  } else {
    runerr("%s (%s, %s, %s) invalid",
         name, TYPENAME(a), TYPENAME(b), TYPENAME(c));
  }
}

static void f_unary_variadic_error (const char *name, block *a,
                                     const char *needed) {
  if (needed) {
    runerr("%s (%s, ...) invalid - need %s as first arg",
         name, TYPENAME(a), needed);
  } else {
    runerr("%s (%s, ...) invalid",
         name, TYPENAME(a));
  }
}
