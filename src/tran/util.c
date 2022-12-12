/*  ===   Miscellaneous creepy crawlers  ===========================  */

/*  $Id: util.c,v 1.20 2020/12/23 02:53:48 setlorg Exp $  */

/*  Free software (c) dB - see file COPYING for license (GPL).  */


/* ------------------------------------------------------------------ */

#include "setltran.h"
#include "y.tab.h"


/* Local routines */

static void trtab(char *t, const char *from, const char *to);
static void trttab(bool *t, const char *members);

static void errhead(void);  /* me */


/* ------------------------------------------------------------------ */

/* Local data */

static char lc[] = "abcdefghijklmnopqrstuvwxyz";
static char uc[] = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
static char to_lower[CHARSETSIZE];
static char to_upper[CHARSETSIZE];
static bool digital[CHARSETSIZE];
static bool space_or_tab[CHARSETSIZE];


/* ------------------------------------------------------------------ */

/*
 *  Initialize some character recognition and translation tables
 */
void utilinit(void) {
  trtab(to_lower,uc,lc);
  trtab(to_upper,lc,uc);
  trttab(digital,"0123456789");
  trttab(space_or_tab," \t");
}

/* .................................................................. */

/*
 *  Construct a "translate table"
 */
static void trtab(char *t, const char *from, const char *to) {
  int i, n;
  for (i=0; i<CHARSETSIZE; i++) t[i] = i;
  assert (strlen(from)==strlen(to));
  n = strlen(from);
  for (i=0; i<n; i++) t[(unsigned char)(from[i])] = to[i];
}

/* .................................................................. */

/*
 *  Construct a "translate and test table"
 */
static void trttab(bool *t, const char *members) {
  int i, n;
  for (i=0; i<CHARSETSIZE; i++) t[i] = false;
  n = strlen(members);
  for (i=0; i<n; i++) t[(unsigned char)(members[i])] = true;
}

/* .................................................................. */

/*
 *  This was a local convenience function for tranerr() to use,
 *  but then scan() over in tokenize.c wanted it too.
 *
 *  It recognizes both "#" and "#line" directives (which are the
 *  same as each other apart from that first token); # is the
 *  only one actually emitted by setlcpp.  The #line form is
 *  meant for everyone else to use.  Since setlcpp recognizes and
 *  transforms #line to #, we will only see #line directly when
 *  preprocessing is skipped.
 *
 *  The first parm is value-result, and the rest are result-only.
 *  None are updated unless the function returns true (see end).
 *
 *  This function has also been cloned and adapted as
 *  cpp_emission() in ../run/errage.c.
 */
bool cpp_emission(long *pi, long *pn, long *pf, long *pg) {
  long i, j, k, n, f, g;
  i = *pi;
  if (src[i] != '#') return false;
  if (i > 0 && src[i-1] != '\n') return false;
  k = j = ++i;  /* point i, j, and k at char after # */
  while (k < srclen && src[k] != '\n') k++;  /* point k at \n */
  if (k == srclen) return false;  /* unterminated line */
  while (j < k && !space_or_tab[(unsigned char)src[j]]) j++;
  if (j == k) return false;  /* short line */
  if (j != i &&  /* not # */
     (j != i+4 || !lpfx(&src[i],"line"))) return false;  /* not #line */
  while (j < k && space_or_tab[(unsigned char)src[j]]) j++;
  if (j == k || !digital[(unsigned char)src[j]]) return false;
  n = 0;
  while (j < k && digital[(unsigned char)src[j]]) {
    n = 10*n + (src[j] - '0');
    j++;
  }
  if (j == k) return false;  /* short line */
  /* we permit #line 55"spam" as does GNU cpp */
  while (j < k && space_or_tab[(unsigned char)src[j]]) j++;
  if (j == k) return false;  /* short line */
  if (src[j] != '"') return false;  /* no "filename" */
  f = j;  /* point f at opening doublequote */
  j = k;
  while (src[j] != '"') j--;  /* scan right-to-left for last doublequote */
  if (j == f) return false;  /* there is only one doublequote */
  g = j;  /* location of closing doublequote */
  *pi = k;  /* loc of trailing \n */
  *pn = n;  /* line number */
  *pf = f;  /* loc of opening doublequote of filename */
  *pg = g;  /* loc of closing doublequote of filename */
  return true;
}

/* .................................................................. */

/*
 *  Flag an error detected by the translator.
 *
 *  Some of this code was retrohacked from setlrun, if that helps to
 *  explain its oddness.  It is boring to note that the diagnostics
 *  for syntax errors in SETL programs haven't caught up with the
 *  reporting elsewhere in the system in the matter of actually
 *  substituting helpful pieces of input in the messages.  There's
 *  not much point in upgrading this aspect until the parser is
 *  replaced with one that knows how to say something more intelligent
 *  than "syntax error", however.
 */

#define PRE_CONTEXT 5 /* lines */
#define POST_CONTEXT 2 /* lines */

void tranerr(k,message)
long k;
const char *message;
{
  long i,j,n,f,g,m;
  long lq[PRE_CONTEXT];
  const char *name;
  if (k > srclen-1) k = srclen-1;
  for (n=0; n<PRE_CONTEXT; n++) lq[n] = 0;
  f = -1;  /* last "filename" we found in a # or #line directive */
  g = -1;  /* trailing " on that filename */
  m = 1;  /* last line we got a # or #line directive for */
  n = 1;  /* line we're scanning */
  for (i=0; i<k; i++) {
    if (src[i] == '\n') {
      n++;
      lq[n%PRE_CONTEXT] = i+1;
    } else if (cpp_emission(&i,&n,&f,&g)) {
      /* i is loc of trailing \n, and n is line num from directive */
      m = n;
      lq[n%PRE_CONTEXT] = i+1;
    }
  }
  if (f == -1) {
    name = source;
  } else {
    name = strnmake(&src[f+1], g-f-1);
  }
  errhead();
  if (!parsing_done) {
    fprintf (stderr,"***  Error in or before line %ld of %s:\n",
                                                  n,     name);
  } else {
    fprintf (stderr,"***  Error at line %ld of %s:\n",
                                        n,     name);
  }
  fprintf (stderr,"***\n");
  /* If m is too far back, move it up to a few lines before the error */
  if (m < n - (PRE_CONTEXT - 1)) m = n - (PRE_CONTEXT - 1);
  if (m < 1) m = 1;
  i = lq[m%PRE_CONTEXT];  /* offset of context line in src */
  while (m <= n) {
    assert (i == lq[m%PRE_CONTEXT]);
#define start_highlight fprintf(stderr,BOLD)
#define stop_highlight fprintf(stderr,NORM)
    if (m == n) start_highlight;
    fprintf (stderr, "%6ld  ", m);
    while (i < srclen && src[i] != '\n') {
      putc(src[i], stderr);
      i++;
    }
    if (m == n) stop_highlight;
    putc('\n', stderr);
    i++;
    m++;
  }
  start_highlight;
  for (j=0; j<8; j++) putc(' ', stderr);
  j = lq[n%PRE_CONTEXT];
  while (j<k) {
    if (src[j] == '\t') putc('\t', stderr);
    else putc(' ', stderr);
    j++;
  }
  putc('$',stderr);
  stop_highlight;
  putc('\n',stderr);
  while (m <= n+POST_CONTEXT && i < srclen && !cpp_emission(&i,&n,&f,&g)) {
    fprintf (stderr, "%6ld  ", m);
    while (i < srclen && src[i] != '\n') {
      putc(src[i], stderr);
      i++;
    }
    putc('\n',stderr);
    i++;
    m++;
  }
  fprintf (stderr,"***\n");
  fprintf (stderr,"***  %s  ***\n", message);
  fprintf (stderr, "***\n"
   "***  This is version %s of GNU SETL.\n"
   "***\n",              PACKAGE_VERSION);
  nerrors++;
  quit();
}

/* .................................................................. */

/* Token-oriented version of 'tranerr' */
void tokerr(tn,message)
long tn;
const char *message;
{
  assert (tn >= 0);
  if (tn >= ntokens) tn = ntokens-1;
  tranerr(tn >= 0 ? tokens[tn].srcloc : srclen, message);
}

/* .................................................................. */

/* An exit of just slightly less than perfect grace */
void quit(void)
{
  exit(1);
}

/* .................................................................. */

static void errhead(void) {
  fprintf (stderr,
   "***\a\n"
   "***  Report from '%s'  ***\n"
   "***\n",
                      tranprog);
}

void bugerr(const char *msg, ...) {
  va_list args;
  va_start(args, msg);
  errhead();
  fprintf (stderr, "***  INTERNAL ERROR:\n");
  fprintf (stderr, "***  ");
  vfprintf (stderr,      msg, args);
  fprintf (stderr,               "  ***\n");
  fprintf (stderr, "***\n"
   "***  Please report this to %s, including sources and data\n"
   "***  if possible.\n",      PACKAGE_BUGREPORT);
  fprintf (stderr, "***\n"
   "***  This is version %s of GNU SETL.\n"
   "***\n",              PACKAGE_VERSION);
  va_end(args);
  fprintf (stderr, "***  Now calling abort()...\n");
  abort();
} /* end bugerr */

/* .................................................................. */

void internal_assert_failure(const char *assertion,
                             const char *filename,
                             int line) {
  bugerr("Assertion \"%s\" failed at %s:%d",
                      assertion,     filename,line);
}

void internal_check_failure(const char *expr,
                            const char *filename,
                            int line) {
  bugerr("Expression \"%s\" failed at %s:%d",
                       expr,          filename,line);
}

void internal_case_failure(const char *expr,
                           int value,
                           const char *filename,
                           int line) {
  bugerr("Unexpected value (%d) of %s at %s:%d",
                           value,  expr, filename,line);
}

/* .................................................................. */

/* A replacement for 'strdup' that halts if it can't get the space */
char *strmake(s)
const char *s;
{
  char *r;
  getmem(r,strlen(s)+1,char);
  strcpy(r,s);
  return r;
}

/* .................................................................. */

/* A replacement for 'strcat' which allocates space for the result */
char *strjoin(s,t)
const char *s,*t;
{
  char *r;
  getmem(r,strlen(s)+strlen(t)+1,char);
  strcpy(r,s);
  strcat(r,t);
  return r;
}

/* .................................................................. */

/* Just a bit cleaner than nesting calls to 'strjoin' */
char *strjoin3(s,t,u)
const char *s,*t,*u;
{
  char *r;
  getmem(r,strlen(s)+strlen(t)+strlen(u)+1,char);
  strcpy(r,s);
  strcat(r,t);
  strcat(r,u);
  return r;
}

/* .................................................................. */

/* More of the same */
char *strjoin4(s,t,u,v)
const char *s,*t,*u,*v;
{
  char *r;
  getmem(r,strlen(s)+strlen(t)+strlen(u)+strlen(v)+1,char);
  strcpy(r,s);
  strcat(r,t);
  strcat(r,u);
  strcat(r,v);
  return r;
}

/* .................................................................. */

/* This is getting a bit silly, isn't it */
char *strjoin5(s,t,u,v,w)
const char *s,*t,*u,*v,*w;
{
  char *r;
  getmem(r,strlen(s)+strlen(t)+strlen(u)+strlen(v)+strlen(w)+1,char);
  strcpy(r,s);
  strcat(r,t);
  strcat(r,u);
  strcat(r,v);
  strcat(r,w);
  return r;
}

/* .................................................................. */

/* Like strmake but copies up to n chars and then pads to n+1 with \0 */
char *strnmake(s,n)
const char *s;
size_t n;
{
  char *r;
  getmem(r,n+1,char);
  if (n > 0) strncpy(r,s,n);  /* copy up to n chars or \0; pad to n */
  r[n] = '\0';  /* ensure nul termination */
  return r;
}

/* .................................................................. */

/* Return freshly-allocated lowercase copy of string */
char *strmakelower(s)
const char *s;
{
  char *t;
  int i,n;
  t = strmake(s);
  n = strlen(t);
  /*
   *  'tolower' is undefined for many ASCII characters in
   *  some BSD systems (strange but true).
   */
  for (i=0; i<n; i++) t[i] = to_lower[(unsigned char)t[i]];
  return t;
}

/* .................................................................. */

/* Return freshly-allocated uppercase copy of string */
char *strmakeupper(s)
const char *s;
{
  char *t;
  int i,n;
  t = strmake(s);
  n = strlen(t);
  /*
   *  'toupper' is undefined for many ASCII characters in
   *  some BSD systems (strange but true).
   */
  for (i=0; i<n; i++) t[i] = to_upper[(unsigned char)t[i]];
  return t;
}

/* .................................................................. */

/* Return true iff there are no uppercase letters in string */
bool strislower(s)
const char *s;
{
  int i,n;
  n = strlen(s);
  for (i=0; i<n; i++) if (isupper((unsigned char)s[i])) return false;
  return true;
}

/* .................................................................. */

/* Return true iff there are no lowercase letters in string */
bool strisupper(s)
const char *s;
{
  int i,n;
  n = strlen(s);
  for (i=0; i<n; i++) if (islower((unsigned char)s[i])) return false;
  return true;
}

/* .................................................................. */

/*
 *  Determine whether an expression of the form x IN y is
 *  restricted enough that it could be used as the guts of
 *  a set or tuple former
 */
bool is_special_former(p,s)
node *p;      /* expression list */
symstack *s;  /* symbol table stack to use to determine RWability */
{
  node *q;
  token *t;
  if (p->nsub == 1) {  /* expr_list contains but 1 expr */
    q = sub0(p);
    if (q->type == N_binary) {  /* which is a binary expr */
      t = (token *)sub1(q);
      if (t->code == IN) {          /* of the form x IN y */
        if (is_special_lhs(sub0(q),s)) {  /* where x is rwable */
          return true;
        }
      }
    }
  }
  return false;
}

/* .................................................................. */

/*
 *  Determine whether an expression is RWable and consists of
 *  either a name or a possibly nested "tuple display" of names
 */
bool is_special_lhs(p,s)
node *p;
symstack *s;
{
  node *q;
  int i;
  switch (p->type) {
  case N_nameseq:
    return true;
  case N_Token:
    return false;
  case N_rw2:
  case N_lhs2:
  case N_tuple1:
    q = sub0(p);
    if (q->nsub == 0) return false;
    for (i=0; i<q->nsub; i++) {
      if (!is_special_lhs(subi(q,i),s)) return false;
    }
    return true;
  default:
    return false;
  }
}

/* .................................................................. */

/*
 *  Convert a non-negative integer to a string
 */
#define MAXDIGITS 25
static const char *decimal_digits = "0123456789";

char *ntoa(n)
int n;
{
  int t;
  int k,i,j,loc[MAXDIGITS+1];
  char *r;
  assert (n>=0);
  k = 0;
  for (t=n; t>0; t/=10) {
    loc[k] = t % 10;
    k++;
    assert (k<MAXDIGITS);
  }
  if (k == 0) {
    loc[0] = 0;
    k = 1;
  }
  getmem(r,k+1,char);
  j = k;
  r[j] = '\0';
  for (i=0; i<k; i++) {
    j--;
    r[j] = decimal_digits[loc[i]];
  }
  return r;
}

/* .................................................................. */

int millisec(void) {
#if HAVE_TIMES
#ifdef CLK_TCK
  int freq = CLK_TCK;
#else
  int freq = sysconf(_SC_CLK_TCK);
#endif
  int ticks;
  struct tms t;
  (void) times(&t);
  ticks = t.tms_utime +
          t.tms_stime +
          t.tms_cutime +
          t.tms_cstime;
  return ((ticks / freq) * 1000) + (((ticks % freq) * 1000) / freq);
#elif HAVE_GETRUSAGE
  struct rusage res;
  int ms;
  getrusage(RUSAGE_SELF,&res);
  ms = (res.ru_utime.tv_usec + res.ru_stime.tv_usec) / 1000 +
       (res.ru_utime.tv_sec + res.ru_stime.tv_sec) * 1000;
  getrusage(RUSAGE_CHILDREN,&res);
  ms += (res.ru_utime.tv_usec + res.ru_stime.tv_usec) / 1000 +
        (res.ru_utime.tv_sec + res.ru_stime.tv_sec) * 1000;
  return ms;
#else
  return clock()/1000;
#endif
}
