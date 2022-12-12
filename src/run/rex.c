/*  ===  String matching using regular expressions  ================  */

/*  $Id: rex.c,v 1.33 2021/05/20 18:09:09 setlorg Exp $  */

/*  Free software (c) dB - see file COPYING for license (GPL).  */

/*
 *  See also "regex.h" and "regex.c".
 */

#include "setlrun.h"

#ifdef USE_REGEX  /* POSIX */

#include <regex.h>

typedef regex_t  pat;  /* regex pattern buffer */
#define NOMINAL_BUFSIZ  64

#else /* !USE_REGEX */

#include <regexp.h>

typedef struct {char *buffer; int circf;}  pat;
#define INIT_BUFSIZ  32

#endif /* !USE_REGEX */

typedef struct {long i; long j;}  intpair;
#define MAX_MATCHGROUPS 10  /* including \0, a.k.a. & */
typedef struct {
  long nmatches;  /* -1 for integers or non-MAGIC (suppress rhs subst) */
  intpair matches[MAX_MATCHGROUPS];
} matchgroups;


/* Gimme one match */
static intpair rex_bounds(string *s, block *p);
static intpair rex_stringbounds(string *s, long k, string *p);
static intpair rex_slicebounds(string *s, block *p, block *q);

/* Gimme matches in mg */
static void rex_matches(string *s, block *p, matchgroups *mg);
static void rex_stringmatches(string *s, long k, string *p,
                                             matchgroups *mg);
static void rex_slicematches(string *s, block *p, block *q,
                                             matchgroups *mg);

static void rex_combinegroups(const matchgroups *mgp,
                              const matchgroups *mgq,
                                    matchgroups *mg);

/* Substitute in host, using mg and \digit (and &) in x */
static void rex_replace(string **host, string *x,
                                       const matchgroups *mg);
static string *rex_replacement(string *s, string *x,
                                       const matchgroups *mg);

/* Non-magic rex_step and rex_matchstep */
static bool rex_find(string *s, long k, string *p, intpair *ind);
static bool rex_matchfind(string *s, long k, string *p,
                                             matchgroups *mg);

/* Foundations */
static void rex_comp(pat *r, string *p);
static bool rex_step(string *s, long k, const pat *p, intpair *ind);
static bool rex_matchstep(string *s, long k, const pat *p,
                                             matchgroups *mg);
static void rex_done(pat *p);

#ifndef USE_REGEX
static void rex_error(int c) NO_RETURN;
#endif

#define infinitely_many_nulls \
 runerr("Infinitely many occurrences of the empty string");


string *rex_fetch(string *s, string *p) {
  string *r = OM;
  intpair t;
  t = rex_stringbounds(s,1,p);
  if (t.i > 0) r = copy_substring(s,t.i,t.j);
  return r;
} /* end rex_fetch */

void rex_store(string **host, string *p, string *x) {
  string *s = *host;
  matchgroups mg;
  rex_stringmatches(s,1,p,&mg);
  if (mg.nmatches != 0) rex_replace(&s,x,&mg);
  *host = s;
} /* end rex_store */

string *rex_getslice(string *s, block *p, block *q) {
  string *r = OM;
  intpair t;
  t = rex_slicebounds(s,p,q);
  if (t.i > 0) r = copy_substring(s,t.i,t.j);
  return r;
} /* end rex_getslice */

void rex_insertslice(string **host, block *p, block *q, string *x) {
  string *s = *host;
  matchgroups mg;
  rex_slicematches(s,p,q,&mg);
  if (mg.nmatches != 0) rex_replace(&s,x,&mg);
  *host = s;
} /* end rex_insertslice */

tuple *rex_mark(string *s, block *p) {
  intpair t = rex_bounds(s, p);
  if (t.i > 0) {
    tuple *r = new_tuple(2);  HANDLE hr = ref(r);
    let (tupelt(r,1), (block *)new_integer(t.i));
    let (tupelt(r,2), (block *)new_integer(t.j));
    retire(hr);
    return r;
  }
  return OM;
} /* end rex_mark */

string *rex_sub(string **host, block *p, string *x) {
  string *s = *host;
  matchgroups mg;
  rex_matches(s,p,&mg);
  if (mg.nmatches != 0) {
    HANDLE hs = ref(s);
    HANDLE hx = ref(x);
    intpair t = mg.matches[0];
    string *r = copy_substring(s,t.i,t.j);
    HANDLE hr = ref(r);
    rex_replace(&s,x,&mg);
    retire(hr);
    retire(hx);
    retire(hs);
    *host = s;
    return r;
  }
  return OM;
} /* end rex_sub */

tuple *rex_gmark(string *s, block *p) {
  HANDLE hs = ref(s);
  HANDLE hp = ref(p);
  tuple *r = null_tuple();  HANDLE hr = ref(r);
  tuple *t = NULL;          HANDLE ht = ref(t);
  assert (is_string(s));
  switch (setl_type(p)) {
  case string_type:
    {
      intpair ind;
      long k;
      #define gmark_do_string(STEP)                           \
      k = 1;                                                  \
      while (k <= s->nchar && (STEP)) {                       \
        long i = ind.i;                                       \
        long j = ind.j;                                       \
        if (j < i) infinitely_many_nulls                      \
        t = new_tuple(2);                                     \
        let (tupelt(t,1), (block *)new_integer(i));           \
        let (tupelt(t,2), (block *)new_integer(j));           \
        tup_tackon(&r,(block *)t);                            \
        k = j + 1;                                            \
      }
      if (get_magic()) {
        pat b;
        rex_comp(&b,(string *)p);
        gmark_do_string(rex_step(s,k,&b,&ind))
        rex_done(&b);
      } else {
        gmark_do_string(rex_find(s,k,(string *)p,&ind))
      }
    }
    break;
  case tuple_type:
    assert (((tuple *)p)->nelt == 2);
    {
      intpair ind1, ind2;
      long k;
      #define gmark_do_tuple(STEP1,STEP2)                     \
      k = 1;                                                  \
      while (k <= s->nchar && (STEP1)) {                      \
        long m = ind1.j + 1;                                  \
        if (m <= s->nchar && (STEP2)) {                       \
          long i = ind1.i;                                    \
          long j = ind2.j;                                    \
          if (j < i) infinitely_many_nulls                    \
          t = new_tuple(2);                                   \
          let (tupelt(t,1), (block *)new_integer(i));         \
          let (tupelt(t,2), (block *)new_integer(j));         \
          tup_tackon(&r,(block *)t);                          \
          k = j + 1;                                          \
        } else {                                              \
          break;  /* from while loop */                       \
        }                                                     \
      }
      if (get_magic()) {
        pat b1, b2;
        rex_comp(&b1,(string *)tupelt((tuple *)p,1));
        rex_comp(&b2,(string *)tupelt((tuple *)p,2));
        gmark_do_tuple(
         rex_step(s,k,&b1,&ind1),
         rex_step(s,m,&b2,&ind2))
        rex_done(&b2);
        rex_done(&b1);
      } else {
        gmark_do_tuple(
         rex_find(s,k,(string *)tupelt((tuple *)p,1),&ind1),
         rex_find(s,m,(string *)tupelt((tuple *)p,2),&ind2))
      }
    }
    break;
  default:
    unexpected (setl_type(p));
  }
  retire(ht);
  retire(hr);
  retire(hp);
  retire(hs);
  return r;
} /* end rex_gmark */

tuple *rex_gsub(string **host, block *p, string *x) {
  string *s = *host;          HANDLE hs = ref(s);
  HANDLE hp = ref(p);
  HANDLE hx = ref(x);
  string *t = OM;             HANDLE ht = ref(t);
  string *a = null_string();  HANDLE ha = ref(a);
  tuple *r = null_tuple();    HANDLE hr = ref(r);
  assert (is_string(x));
  switch (setl_type(p)) {
  case string_type:
    {
      matchgroups mg;
      long k;
      #define gsub_do_string(STEP)                            \
      k = 1;                                                  \
      while (k <= s->nchar && (STEP)) {                       \
        intpair ind = mg.matches[0];                          \
        long i = ind.i;                                       \
        long j = ind.j;                                       \
        if (j < i) infinitely_many_nulls                      \
        t = copy_substring(s,i,j);                            \
        tup_tackon(&r,(block *)t);                            \
        str_concat_substring(&a,s,k,i-1);                     \
        str_concat(&a,rex_replacement(s,x,&mg));              \
        k = j + 1;                                            \
      }                                                       \
      str_concat_substring(&a,s,k,s->nchar);
      if (get_magic()) {
        pat b;
        rex_comp(&b,(string *)p);
        gsub_do_string(rex_matchstep(s,k,&b,&mg))
        rex_done(&b);
      } else {
        gsub_do_string(rex_matchfind(s,k,(string *)p,&mg))
      }
    }
    break;
  case tuple_type:
    {
      matchgroups mg1, mg2;
      long k;
      #define gsub_do_tuple(STEP1,STEP2)                      \
      k = 1;                                                  \
      while (k <= s->nchar && (STEP1)) {                      \
        intpair ind1 = mg1.matches[0];                        \
        long m = ind1.j + 1;                                  \
        if (m <= s->nchar && (STEP2)) {                       \
          matchgroups mg;                                     \
          intpair ind2 = mg2.matches[0];                      \
          long i = ind1.i;                                    \
          long j = ind2.j;                                    \
          if (j < i) infinitely_many_nulls                    \
          t = copy_substring(s,i,j);                          \
          tup_tackon(&r,(block *)t);                          \
          str_concat_substring(&a,s,k,i-1);                   \
          mg.matches[0].i = i;                                \
          mg.matches[0].j = j;                                \
          rex_combinegroups(&mg1,&mg2,&mg);                   \
          str_concat(&a,rex_replacement(s,x,&mg));            \
          k = j + 1;                                          \
        } else {                                              \
          break;  /* from while loop */                       \
        }                                                     \
      }                                                       \
      str_concat_substring(&a,s,k,s->nchar);
      if (get_magic()) {
        pat b1, b2;
        rex_comp(&b1,(string *)tupelt((tuple *)p,1));
        rex_comp(&b2,(string *)tupelt((tuple *)p,2));
        gsub_do_tuple(
         rex_matchstep(s,k,&b1,&mg1),
         rex_matchstep(s,m,&b2,&mg2))
        rex_done(&b2);
        rex_done(&b1);
      } else {
        gsub_do_tuple(
         rex_matchfind(s,k,(string *)tupelt((tuple *)p,1),&mg1),
         rex_matchfind(s,m,(string *)tupelt((tuple *)p,2),&mg2))
      }
    }
    break;
  default:
    unexpected (setl_type(p));
  }
  retire(hr);
  retire(ha);
  retire(ht);
  retire(hx);
  retire(hp);
  retire(hs);
  *host = a;
  return r;
} /* end rex_gsub */


tuple *rex_split(string *s, string *p) {

  HANDLE hs = ref(s);
  HANDLE hp = ref(p);
  tuple *r = null_tuple();      HANDLE hr = ref(r);
  string *t;
  pat b;
  long k, n;
  long i_offset, j_offset;
  intpair ind, first_ind, last_ind;
  bool magic = get_magic();

  assert (is_string(s));
  assert (is_string(p));

# define split_a_gut(string,index) \
  (magic ? rex_step(string,index,&b,&ind)  \
         : rex_find(string,index,p,&ind))

  if (s->nchar == 0) goto byebye_rex_split;  /* returning null tuple */

  if (magic) rex_comp(&b,p);

  first_ind.i = 0;
  first_ind.j = -1;  /* initialize to appease compilers */
  last_ind.i = -1;  /* initialize to appease compilers */
  last_ind.j = 0;

  for (k = 1; k <= s->nchar && split_a_gut(s,k); k += j_offset) {
    if (k == 1) first_ind = ind;
    last_ind = ind;
    i_offset = ind.i - k;
    j_offset = ind.j - k + 1;
    if (j_offset <= i_offset) infinitely_many_nulls
    /* N.B. Not using new_nstring() here, because &strelt(s,k) as an
     * arg to it would be an unsafe pointer into the SETL heap:  */
    t = new_estring(i_offset);
    mvmem (&strelt(t,1), &strelt(s,k), i_offset);
    strelt(t,i_offset+1) = '\0';
    tup_tackon(&r,(block *)t);
  }
  i_offset = s->nchar - k + 1;
  t = new_estring(i_offset);
  mvmem (&strelt(t,1), &strelt(s,k), i_offset);
  strelt(t,i_offset+1) = '\0';
  tup_tackon(&r,(block *)t);

  if (first_ind.i == 1) {
    assert (((string *)tupelt(r,1))->nchar == 0);  /* r(1) = '' */
    n = first_ind.j + 1 - first_ind.i;  /* len of 1st matched delim */
    t = new_estring(2*n);  /* make its double, literally! */
    mvmem (&strelt(t,1), &strelt(s,1), n);
    mvmem (&strelt(t,n+1), &strelt(s,1), n);
    strelt(t,2*n+1) = '\0';
    if (split_a_gut(t,1) /* <== sets ind */ && ind.j == 2*n) {
      /*
       *  Delimiter regexp matches doubled version, so leading
       *  delimiters merge, so remove bogus leading ('') element
       *  of split.
       */
      r->npre++;
      r->nelt--;
    }
  }

  if ((last_ind.j == s->nchar) && (r->nelt > 0)) {
    assert (((string *)tupelt(r,r->nelt))->nchar == 0);  /* r(#r) = '' */
    n = last_ind.j + 1 - last_ind.i;  /* len of last matched delim */
    t = new_estring(2*n);  /* make its double, literally! */
    mvmem (&strelt(t,1), &strelt(s,last_ind.i), n);
    mvmem (&strelt(t,n+1), &strelt(s,last_ind.i), n);
    strelt(t,2*n+1) = '\0';
    if (split_a_gut(t,1) /* <== sets ind */ && ind.j == 2*n) {
      /*
       *  Delimiter regexp matches doubled version, so trailing
       *  delimiters merge, so remove bogus trailing ('') element
       *  of split.
       */
      r->nsuf++;
      r->nelt--;
    }
  }

  if (magic) rex_done(&b);

byebye_rex_split:

  retire(hr);
  retire(hp);
  retire(hs);

  return r;

} /* end rex_split */


static intpair rex_bounds(string *s, block *p) {
  intpair t;
  switch (setl_type(p)) {
  case string_type:
    t = rex_stringbounds(s,1,(string *)p);
    break;
  case integer_type:
    t.i = get_pos_long((integer *)p, "string subscript");
    t.j = t.i <= s->nchar ? t.i : t.i - 1;
    break;
  case tuple_type:
    assert (((tuple *)p)->nelt == 2);
    t = rex_slicebounds(s,tupelt((tuple *)p,1),
                          tupelt((tuple *)p,2));
    break;
  default:
    unexpected (setl_type(p));
  }
  return t;
} /* end rex_bounds */

static intpair rex_stringbounds(string *s, long k, string *p) {
  intpair r;
  if (get_magic()) {
    pat b;
    rex_comp(&b,p);
    rex_step(s,k,&b,&r);
    rex_done(&b);
  } else {
    rex_find(s,k,p,&r);
  }
  return r;
} /* end rex_stringbounds */

static intpair rex_slicebounds(string *s, block *p, block *q) {
  intpair t;
  long i,j,k;
  assert (is_string(s));
  switch (setl_type(p)) {
  case integer_type:
    i = get_pos_long((integer *)p, "string slice lower index");
    k = i;
    break;
  case string_type:
    t = rex_stringbounds(s,1,(string *)p);
    if (t.i == 0) goto done;
    i = t.i;
    k = t.j + 1;
    break;
  default:
    unexpected (setl_type(p));
  }
  switch (setl_type(q)) {
  case integer_type:
    j = get_nat_long((integer *)q, "string slice upper index");
    if (j < i-1) j = i-1;  /* an empty string slice */
    break;
  case string_type:
    t = rex_stringbounds(s,k,(string *)q);
    if (t.i == 0) goto done;
    j = t.j;
    break;
  default:
    unexpected (setl_type(q));
  }
  t.i = i;
  t.j = j;
done:
  return t;
} /* end rex_slicebounds */


static void rex_matches(string *s, block *p, matchgroups *mg) {
  switch (setl_type(p)) {
  case string_type:
    rex_stringmatches(s,1,(string *)p,mg);
    break;
  case integer_type:
    mg->nmatches = -1;
    mg->matches[0].i = mg->matches[0].j =
     get_pos_long((integer *)p, "string subscript");
    break;
  case tuple_type:
    assert (((tuple *)p)->nelt == 2);
    rex_slicematches(s, tupelt((tuple *)p,1), tupelt((tuple *)p,2), mg);
    break;
  default:
    unexpected (setl_type(p));
  }
} /* end rex_matches */

static void rex_stringmatches(string *s, long k, string *p,
                                                     matchgroups *mg) {
  if (get_magic()) {
    pat b;
    rex_comp(&b,p);
    rex_matchstep(s,k,&b,mg);
    rex_done(&b);
  } else {
    rex_matchfind(s,k,p,mg);
  }
} /* end rex_stringmatches */

static void rex_slicematches(string *s, block *p, block *q,
                                                     matchgroups *mg) {
  long i,j,k;
  matchgroups mgp, mgq;
  assert (is_string(s));
  mgp.nmatches = 0;  /* default */
  mgq.nmatches = 0;  /* default */
  switch (setl_type(p)) {
  case integer_type:
    i = get_pos_long((integer *)p, "string slice lower index");
    mgp.nmatches = -1;
    k = i;
    break;
  case string_type:
    rex_stringmatches(s,1,(string *)p,&mgp);
    if (mgp.nmatches == 0) goto nomatch;
    i = mgp.matches[0].i;
    k = mgp.matches[0].j + 1;
    break;
  default:
    unexpected (setl_type(p));
  }
  switch (setl_type(q)) {
  case integer_type:
    j = get_nat_long((integer *)q, "string slice upper index");
    if (j < i-1) j = i-1;  /* the empty string slice just before i */
    mgq.nmatches = -1;
    break;
  case string_type:
    rex_stringmatches(s,k,(string *)q,&mgq);
    if (mgq.nmatches == 0) goto nomatch;
    j = mgq.matches[0].j;
    break;
  default:
    unexpected (setl_type(q));
  }
  /* Make \0 refer to the whole matched slice:  */
  mg->matches[0].i = i;
  mg->matches[0].j = j;
  /* Make \1, \2, ... refer to the "concatenation" of parenthesized
   * groups:  */
  rex_combinegroups(&mgp,&mgq,mg);
  return;
nomatch:
  mg->nmatches = 0;
} /* end rex_slicematches */

static void rex_combinegroups(const matchgroups *mgp,
                              const matchgroups *mgq,
                                    matchgroups *mg) {
  /* Make \1, \2, ... refer to parenthesized groups in p followed by
   * parenthesized groups in q.  */
  long i,j,k;
  long pm = mgp->nmatches;
  long qm = mgq->nmatches;
  if ((pm < 0 && qm < 0) || !get_magic()) {
    mg->nmatches = -1;
    return;
  }
  pm = ABS(pm);
  qm = ABS(qm);
  mg->nmatches = MIN (pm + qm - 1, MAX_MATCHGROUPS);
  k = 1;
  for (i = 1; i < pm && k < mg->nmatches; i++, k++) {
    mg->matches[k] = mgp->matches[i];
  }
  for (j = 1; j < qm && k < mg->nmatches; j++, k++) {
    mg->matches[k] = mgq->matches[j];
  }
  assert (k == mg->nmatches);
} /* end rex_combinegroups */


static void rex_replace(string **host, string *x,
                                               const matchgroups *mg) {
  string *s = *host;  HANDLE hs = ref(s);
  string *y = rex_replacement(s,x,mg);
  intpair ind = mg->matches[0];
  str_insert(&s,ind.i,ind.j,y);
  retire(hs);
  *host = s;
} /* end rex_replace */

static string *rex_replacement(string *s, string *x,
                                               const matchgroups *mg) {
  HANDLE hs;
  HANDLE hx;
  string *r;
  HANDLE hr;
  intpair ind;
  long i;
  if (mg->nmatches < 0) return copy_string(x);
  assert (mg->nmatches > 0);
  hs = ref(s);
  hx = ref(x);
  r = NULL;
  hr = ref(r);
  r = null_string();
  for (i=1; i<=x->nchar; i++) {
    char c = strelt(x,i);
    switch (c) {
    case '\\':
      i++;
      if (i<=x->nchar) {
        int d;
        c = strelt(x,i);
        d = digval[(uchar)c];
        if (d >= 0) {
          if (d < mg->nmatches) {
            ind = mg->matches[d];
            if (ind.i > 0) {
              str_concat_substring(&r,s,ind.i,ind.j);  /* d'th subexpr */
            }
          }
          /* \digit for no such subexpr --> empty string */
        } else {
          str_tackon(&r,c);  /* any non-digit after backslash */
        }
      }
      break;
    case '&':
      ind = mg->matches[0];
      str_concat_substring(&r,s,ind.i,ind.j);  /* whole matched substr */
      break;
    default:
      str_tackon(&r,c);  /* normal char (not backslash or ampersand) */
    }
  }
  /* The portion of s to be replaced is specified by mg->matches[0].  */
  retire(hr);
  retire(hx);
  retire(hs);
  return r;
} /* end rex_replace */


static bool rex_find(string *s, long k, string *p, intpair *ind) {
  long lim = s->nchar - p->nchar + 1;
  while (k <= lim && memcmp(&strelt(s,k),&strelt(p,1),p->nchar) != 0) {
    k++;
  }
  if (k > lim) goto nomatch;  /* after the code pattern in rex_step() */
  ind->i = k;
  ind->j = k + p->nchar - 1;
  return true;
nomatch:
  ind->i = 0;
  ind->j = 0;
  return false;
} /* end rex_find */

static bool rex_matchfind(string *s, long k, string *p,
                                                     matchgroups *mg) {
  intpair ind;
  if (!rex_find(s,k,p,&ind)) goto nomatch;
  mg->nmatches = -1;
  mg->matches[0] = ind;
  return true;
nomatch:
  mg->nmatches = 0;
  return false;
} /* end rex_matchfind */


#ifdef USE_REGEX

static void rex_comp(pat *r, string *p) {
  int rc;
  assert (is_string(p));
  if (strlen(&strelt(p,1)) != (size_t)p->nchar) {
    runerr("Regular expression pattern must not contain NUL characters");
  }
  rc = regcomp(r, &strelt(p,1), REG_EXTENDED);
  if (rc != 0) {
    char msg[100];
    regerror(rc, r, msg, sizeof msg);
    runerr("%s", msg);  /* runerr(msg) were fragile */
  }
} /* end rex_comp */

static bool rex_step(string *s, long k, const pat *p, intpair *ind) {
  int rc;
  regmatch_t pmatch[1];
  assert (is_string(s));
  assert (k > 0);
  if (strlen(&strelt(s,1)) != (size_t)s->nchar) {
    runerr("String to be matched by regexp must not contain NUL chars");
  }
  if (k > s->nchar + 1) goto nomatch;
  rc = regexec(p, &strelt(s,k), 1, pmatch, 0);
  if (rc != 0) goto nomatch;
  ind->i = pmatch[0].rm_so + k;
  ind->j = pmatch[0].rm_eo + k - 1;
  return true;
nomatch:
  ind->i = 0;
  ind->j = 0;
  return false;
} /* end rex_step */

static bool rex_matchstep(string *s, long k, const pat *p,
                                                     matchgroups *mg) {
  int rc;
  size_t i,n;
  regmatch_t pmatch[MAX_MATCHGROUPS];
  assert (is_string(s));
  assert (k > 0);
  if (strlen(&strelt(s,1)) != (size_t)s->nchar) {
    runerr("String to be matched by regexp must not contain NUL chars");
  }
  if (k > s->nchar + 1) goto nomatch;
  n = MIN(p->re_nsub+1,MAX_MATCHGROUPS);
  rc = regexec(p, &strelt(s,k), n, pmatch, 0);
  if (rc != 0) goto nomatch;
  mg->nmatches = n;
  for (i=0; i<n; i++) {
    if (pmatch[i].rm_so == -1) {
      mg->matches[i].i = 0;
      mg->matches[i].j = 0;
    } else {
      mg->matches[i].i = pmatch[i].rm_so + k;
      mg->matches[i].j = pmatch[i].rm_eo + k - 1;
    }
  }
  return true;
nomatch:
  mg->nmatches = 0;
  return false;
} /* end rex_matchstep */

static void rex_done(pat *p) {
  regfree(p);
} /* end rex_done */


#else /* !USE_REGEX */

#error Non-USE_REGEX case no longer supported.

#if 0
#include <setjmp.h>

static jmp_buf context;

static void rex_error(int c) {
  switch (c) {
  case 11:  runerr("Range endpoint too large in regular expression");
  case 16:  runerr("Bad number in regular expression");
  case 25:  runerr("\"\\digit\" out of range in regular expression");
  case 36:  runerr("Illegal or missing delimiter in regular expression");
  case 41:  runerr("No remembered search string in regular expression");
  case 42:  runerr("\\( \\) imbalance in regular expression");
  case 43:  runerr("Too many \\( in regular expression");
  case 44:  runerr("More than 2 numbers given in \\{ \\} in regular expression");
  case 45:  runerr("} expected after \\ in regular expression");
  case 46:  runerr("First number exceeds second in \\{ \\} in regular expression");
  case 48:  runerr("Invalid end point in range expression in regular expression");
  case 49:  runerr("[ ] imbalance in regular expression");
  case 50:  longjmp(context,1);  /* go try with more memory */
  case 70:  runerr("Invalid endpoint in range in regular expression");
  default:  runerr("Error of unknown code from <regexp.h>");
  }
} /* end rex_error */

#define INIT        register char *sp = instring;
#define GETC()     (*sp++)
#define PEEKC()    (*sp)
#define UNGETC(c)  (--sp)
#define RETURN(c)   return c;
#define ERROR(c)    rex_error(c)

#include <regexp.h>

static void rex_comp(pat *r, string *p) {
  assert (is_string(p));
  if (p->nchar > 0) {
    static char *buf;
    static long bufsiz;
    buf = NULL;
    bufsiz = INIT_BUFSIZ;
    if (setjmp(context)) bufsiz *= 2;
    if (!buf) buf = (char *)os_malloc(bufsiz);
    else buf = (char *)os_realloc(buf, bufsiz);
    compile(&strelt(p,1), &buf[0], &buf[bufsiz], '\0');
    r->buffer = buf;
    r->circf = circf;
  } else {
    r->buffer = NULL;
    r->circf = false;
  }
} /* end rex_comp */

static bool rex_step(string *s, long k, pat *p, intpair *ind) {
  assert (is_string(s));
  assert (k > 0);
  if (k > s->nchar + 1) {
    ind->i = 0;
    ind->j = 0;
    return false;
  } else if (!p->buffer) {
    loc1 = loc2 = &strelt(s,k);
    ind->i = k;
    ind->j = k - 1;
    return true;
  } else {
    circf = p->circf;
    if (step(&strelt(s,k), p->buffer)) {
      char *base = &strelt(s,1);
      ind->i = loc1 - base + 1;
      ind->j = loc2 - base;
      return true;
    } else {
      ind->i = 0;
      ind->j = 0;
      return false;
    }
  }
} /* end rex_step */

static void rex_done(pat *p) {
  if (p->buffer) {
    os_free(p->buffer);
    p->buffer = NULL;
  }
} /* end rex_done */
#endif


#endif /* !USE_REGEX */
