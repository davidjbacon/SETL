/*  ===  String matching using regular expressions  ================  */

/*  $Id: rex.c,v 1.35 2025/02/07 16:56:39 setlorg Exp $  */

/*  Free software (c) dB - see file COPYING for license (GPL).  */

/*
 *  See also "regex.h" and "regex.c".
 */

#include "setlrun.h"

#ifndef USE_REGEX  /* POSIX EREs */
#error  POSIX regex support required - see setlrun.h
#endif

#include <regex.h>

typedef struct {long i; long j;}  intpair;  /* 1-origined indices */
static const intpair zero_intpair = {0};

#define MAX_MATCHGROUPS  10  /* including "group 0" (\0 aka &) */
typedef struct {  /* descriptor for a match, or integer bound */
  /* nmatches is 1 more than re_nsub, the number of parenthesized
   * (sub)groups found in the pattern; or -1 for an integer bound or
   * non-MAGIC match in matches[0]; or 0 (default) for non-match.  */
  long nmatches;  /* up to MAX_MATCHGROUPS, including group 0 */
  /* The meaning of \- in a replacement string for s(p..q) when p and q
   * are string patterns is everything between p and q _exclusive_.
   * But when either bound is an integer, \- includes that bound; so
   * for s(p..) aka s(p..#s), \- is everything after p; and for s(..q)
   * aka s(1..q), \- is everything before q.  \- is only meaningful
   * for p..q and [p,q] patterns.  */
  intpair between;  /* for \-; between.i == 0 means no "between" */
  intpair matches[MAX_MATCHGROUPS];  /* for each \{digit} */
  #define whole_match  matches[0]  /* group 0, or nmatches == -1 */
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

/* Replace matched substr of *host with rex_replacement(*host,x,mg) */
static void rex_replace(string **host, string *x, const matchgroups *mg);
/* Replacement for s substr given match mg and replacement pattern x */
static string *rex_replacement(string *s, string *x, const matchgroups *mg);

/* Non-magic rex_markstep and rex_matchstep */
static bool rex_markfind(string *s, long k, string *p,
                          intpair *ind);
static bool rex_matchfind(string *s, long k, string *p,
                           matchgroups *mg);

/* true if ind := where p occurs in s, starting at s(k) */
static bool rex_markstep(string *s, long k, const regex_t *p,
                          intpair *ind);
/* true if mg := where p occurs in s, and groups, starting at s(k) */
static bool rex_matchstep(string *s, long k, const regex_t *p,
                           matchgroups *mg);

/* Foundations */
static void rex_comp(regex_t *r, string *p);
static bool rex_exec(string *s, long k, const regex_t *p,
                      matchgroups *mg, size_t max_matchgroups);
static void rex_done(regex_t *p);


#define infinitely_many_nulls \
 runerr("Infinitely many occurrences of the empty string");


string *rex_fetch(string *s, string *p) {
  string *r = OM;
  intpair t = rex_stringbounds(s,1,p);
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
  intpair t = rex_slicebounds(s,p,q);
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
    intpair t = mg.whole_match;
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
        regex_t b;
        rex_comp(&b,(string *)p);
        gmark_do_string(rex_markstep(s,k,&b,&ind))
        rex_done(&b);
      } else {
        gmark_do_string(rex_markfind(s,k,(string *)p,&ind))
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
        regex_t b1, b2;
        rex_comp(&b1,(string *)tupelt((tuple *)p,1));
        rex_comp(&b2,(string *)tupelt((tuple *)p,2));
        gmark_do_tuple(
         rex_markstep(s,k,&b1,&ind1),
         rex_markstep(s,m,&b2,&ind2))
        rex_done(&b2);
        rex_done(&b1);
      } else {
        gmark_do_tuple(
         rex_markfind(s,k,(string *)tupelt((tuple *)p,1),&ind1),
         rex_markfind(s,m,(string *)tupelt((tuple *)p,2),&ind2))
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
        intpair ind = mg.whole_match;                         \
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
        regex_t b;
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
        intpair ind1 = mg1.whole_match;                       \
        long m = ind1.j + 1;                                  \
        if (m <= s->nchar && (STEP2)) {                       \
          matchgroups mg;                                     \
          intpair ind2 = mg2.whole_match;                     \
          long i = ind1.i;                                    \
          long j = ind2.j;                                    \
          if (j < i) infinitely_many_nulls                    \
          t = copy_substring(s,i,j);                          \
          tup_tackon(&r,(block *)t);                          \
          str_concat_substring(&a,s,k,i-1);                   \
          mg.whole_match.i = i;                               \
          mg.whole_match.j = j;                               \
          mg.between.i = ind1.j + 1;  /* m */                 \
          mg.between.j = ind2.i - 1;                          \
          rex_combinegroups(&mg1,&mg2,&mg);                   \
          str_concat(&a,rex_replacement(s,x,&mg));            \
          k = j + 1;                                          \
        } else {                                              \
          break;  /* from while loop */                       \
        }                                                     \
      }                                                       \
      str_concat_substring(&a,s,k,s->nchar);
      if (get_magic()) {
        regex_t b1, b2;
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
  regex_t b;
  long k, n;
  long i_offset, j_offset;
  intpair ind = zero_intpair,
    first_ind = zero_intpair,
     last_ind = zero_intpair;
  bool magic = get_magic();

  assert (is_string(s));
  assert (is_string(p));

# define split_a_gut(string,index) \
  (magic ? rex_markstep(string,index,&b,&ind)  \
         : rex_markfind(string,index,p,&ind))

  if (s->nchar == 0) goto byebye_rex_split;  /* returning null tuple */

  if (magic) rex_comp(&b,p);

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
    regex_t b;
    rex_comp(&b,p);
    rex_markstep(s,k,&b,&r);
    rex_done(&b);
  } else {
    rex_markfind(s,k,p,&r);
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
    {
      long i = get_pos_long((integer *)p, "string subscript");
      mg->nmatches = -1;
      mg->whole_match.i = i;
      mg->whole_match.j = i;
    }
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
    regex_t b;
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
  switch (setl_type(p)) {
  case integer_type:
    i = get_pos_long((integer *)p, "string slice lower index");
    mgp.nmatches = -1;
    k = i;
    break;
  case string_type:
    rex_stringmatches(s,1,(string *)p,&mgp);
    if (mgp.nmatches == 0) goto nomatch;
    i = mgp.whole_match.i;
    k = mgp.whole_match.j + 1;
    break;
  default:
    unexpected (setl_type(p));
  }
  mg->whole_match.i = i;  /* left end of the whole matched slice \0 */
  mg->between.i = k;  /* left end of \- */
  switch (setl_type(q)) {
  case integer_type:
    j = get_nat_long((integer *)q, "string slice upper index");
    if (j < i-1) j = i-1;  /* the empty string slice just before i */
    k = j;
    mgq.nmatches = -1;
    break;
  case string_type:
    rex_stringmatches(s,k,(string *)q,&mgq);
    if (mgq.nmatches == 0) goto nomatch;
    j = mgq.whole_match.j;
    k = mgq.whole_match.i - 1;
    break;
  default:
    unexpected (setl_type(q));
  }
  mg->whole_match.j = j;  /* right end of the whole matched slice \0 */
  mg->between.j = k;  /* right end of \- */
  rex_combinegroups(&mgp,&mgq,mg);  /* "concatenate" subgroup matches */
  return;
nomatch:
  mg->nmatches = 0;
} /* end rex_slicematches */

/* "Concatenate" (sub)group matches into mg->matches[1..] but assume
 * caller inits mg->matches[0] (mg->whole_match) and mg->between.  */
static void rex_combinegroups(const matchgroups *mgp,
                              const matchgroups *mgq,
                                    matchgroups *mg) {
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
  intpair ind = mg->whole_match;
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
  long i;
  bool between_is_defined;
  if (mg->nmatches < 0) return copy_string(x);
  assert (mg->nmatches > 0);  /* make sure mg isn't a non-match */
  /* "between" is only defined for p..q and [p,q] patterns */
  between_is_defined = mg->between.i > 0;
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
        if (d >= 0) {  /* \0 thru \9 */
          if (d < mg->nmatches) {
            intpair ind = mg->matches[d];
            if (ind.i > 0) {
              str_concat_substring(&r,s,ind.i,ind.j);
            }
          }
          /* if d >= nmatches or matches[d] <= 0, \{d} is empty */
        } else if (between_is_defined && c == '-') {
          intpair ind = mg->between;
          str_concat_substring(&r,s,ind.i,ind.j);
        } else {
          str_tackon(&r,c);  /* anything else after backslash */
        }
      }
      break;
    case '&':  /* same as \0 */
      {
        intpair ind = mg->whole_match;  /* i.e., mg->matches[0] */
        str_concat_substring(&r,s,ind.i,ind.j);
      }
      break;
    default:
      str_tackon(&r,c);  /* normal char (not backslash or ampersand) */
    }
  }
  retire(hr);
  retire(hx);
  retire(hs);
  return r;
} /* end rex_replacement */


static bool rex_markfind(string *s, long k, string *p,
                          intpair *ind) {
  long lim = s->nchar - p->nchar + 1;
  while (k <= lim) {
    char *t = (char *)memchr(&strelt(s,k), strelt(p,1), (lim - k) + 1);
    if (!t) goto nomatch;
    k += t - &strelt(s,k);
    if (memcmp(&strelt(s,k), &strelt(p,1), p->nchar) == 0) break;
    k++;
  }
  if (k > lim) goto nomatch;
  ind->i = k;
  ind->j = k + p->nchar - 1;
  return true;
nomatch:
  *ind = zero_intpair;
  return false;
} /* end rex_markfind */

static bool rex_matchfind(string *s, long k, string *p,
                           matchgroups *mg) {
  intpair ind;
  if (!rex_markfind(s,k,p,&ind)) goto nomatch;
  mg->nmatches = -1;
  mg->whole_match = ind;
  return true;
nomatch:
  mg->nmatches = 0;
  return false;
} /* end rex_matchfind */


static bool rex_markstep(string *s, long k, const regex_t *p,
                          intpair *ind) {
  matchgroups mg;
  if (rex_exec(s, k, p, &mg, 1)) {
    *ind = mg.whole_match;  /* i.e., mg.matches[0] */
    return true;
  } else {
    *ind = zero_intpair;
    return false;
  }
} /* end rex_markstep */

static bool rex_matchstep(string *s, long k, const regex_t *p,
                           matchgroups *mg) {
  return rex_exec(s, k, p, mg, MAX_MATCHGROUPS);
} /* end rex_matchstep */


static void rex_comp(regex_t *r, string *p) {
  int rc;
  assert (is_string(p));
  if (strlen(&strelt(p,1)) != (size_t)p->nchar) {
    runerr("Regular expression pattern must not contain NUL characters");
  }
  /* Compile as extended (ERE) not basic (BRE) regexp.  */
  rc = regcomp(r, &strelt(p,1), REG_EXTENDED);
  if (rc != 0) {
    char msg[100];
    regerror(rc, r, msg, sizeof msg);
    runerr("%s", msg);  /* runerr(msg) were fragile */
  }
} /* end rex_comp */

/* Populate n = mg->nmatches and n elements of mg->matches by
 * applying regexec at s(k..) using compiled pattern p.  */
static bool rex_exec(string *s, long k, const regex_t *p,
                      matchgroups *mg, size_t max_matchgroups) {
  int rc;
  long i;
  long n = MIN(p->re_nsub + 1, max_matchgroups);
  regmatch_t pmatch[n];
  assert (is_string(s));
  assert (k > 0);
  if (k > s->nchar + 1) goto nomatch;
#ifndef REG_STARTEND
  if (strlen(&strelt(s,k)) != (size_t)(s->nchar + 1 - k)) {
    runerr("String to be matched by regexp must not contain NUL chars");
  }
  rc = regexec(p, &strelt(s,k), n, pmatch, 0);
#else  /* NUL chars are allowed */
  pmatch[0].rm_so = 0;
  pmatch[0].rm_eo = s->nchar + 1 - k;
  rc = regexec(p, &strelt(s,k), n, pmatch, REG_STARTEND);
#endif
  if (rc != 0) goto nomatch;
  /* There is no "between" for foundational string matches.  */
  mg->between = zero_intpair;
  mg->nmatches = n;
  for (i=0; i<n; i++) {
    if (pmatch[i].rm_so == -1) {
      /* Assume this case can exist, say for a group that is part of an
       * alternation branch not taken; an empty expansion would seem
       * approp in replacements.  */
      mg->matches[i] = zero_intpair;
    } else {
      mg->matches[i].i = pmatch[i].rm_so + k;  /* indices include k */
      mg->matches[i].j = pmatch[i].rm_eo + k - 1;
    }
  }
  return true;
nomatch:
  mg->nmatches = 0;
  return false;
} /* end rex_exec */

static void rex_done(regex_t *p) {
  regfree(p);
} /* end rex_done */
