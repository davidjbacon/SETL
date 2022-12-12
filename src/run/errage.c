/*  ===  Error-reporting utilities  ================================  */

/*  $Id: errage.c,v 1.22 2021/11/26 03:26:27 setlorg Exp $  */

/*  Free software (c) dB - see file COPYING for license (GPL).  */

#include "setlrun.h"

static void vrunerr(const char *fmt, va_list args) LIKE_VPRINTF NO_RETURN;
static void errhead(void);  /* this is me - I like lots of errtime */
static void version_trailer(void);
static void show_msg(const char *fmt, va_list args) LIKE_VPRINTF;
static void error_context(void);
static bool cpp_emission(string *srctext, long *pi, long *pn, long *pf, long *pg);
static void traceback(void);

/*
 *  For use within error functions, where failure of the regular
 *  assert() (see "setlrun.h") could lead to unbounded recursion:
 */
#define assert_in_errage(x)  do { \
  if (!(x)) abort();  /* error while trying to report error */ \
} while (0)

void bugerr(const char *fmt, ...) {
  va_list args;
  error_context();
  print_stderr("***\n"
               "***  INTERNAL ERROR:\n");
  va_start(args, fmt);
  show_msg(fmt, args);
  va_end(args);
  print_stderr("***  Please report this to %s, including SETL source\n"
               "***  and data if possible.  Thanks!\n", PACKAGE_BUGREPORT);
  version_trailer();
  print_stderr("***  Now calling abort()...\n");
  abort();
}

void cmdlinerr(const char *fmt, ...) {
  va_list args;
  errhead();
  print_stderr("***  Error in command line:\n");
  va_start(args, fmt);
  show_msg(fmt, args);
  va_end(args);
  print_stderr("***  Try \"%s --help\".\n", setlprog);
  version_trailer();
  exit(1);
}

void tranerr(const char *fmt, ...) {
  va_list args;
  /*
   *  TODO: Let this be meaningful at assemble() time, using some
   *  form of iv and vcode that assemble() makes available so that
   *  the right lines of the textual VM code can be cited instead
   *  of just "***  Error before SETL VM execution:" which ensues
   *  from the fact that setl_vm is NULL during assemble().
   */
  error_context();
  print_stderr("***\n"
               "***  Error caused by bad SETL VM code:\n");
  va_start(args, fmt);
  show_msg(fmt, args);
  va_end(args);
  /* "setltran" is in quotes here because the translator that was
   * actually used may have a different (though hopefully related)
   * name:  */
  print_stderr("***  If that code came from \"setltran\", please report it to\n"
               "***  %s, %s", PACKAGE_BUGREPORT, "and include SETL source,\n"
               "***  VM code, and GNU SETL version num if you can.  Thanks!\n");
  version_trailer();
  /*
   *  Pending provision of the above TODO, this abort() makes it
   *  possible to use a debugger (e.g. gdb) to look at the approp vars
   *  in assemble() if that's where the error was diagnosed, so you can
   *  see the context such as what VM instruction was being processed.
   */
  print_stderr("***  Now calling abort() in the hope that you can inspect\n"
               "***  %s's state for details on the bad VM code...\n",
                     setlprog);
  abort();  /* to facilitate use of debugger here, not to indicate crash */
}

/*
 *  A variant of tranerr() that is suitable for SETL VM code errors
 *  reflecting cases of symbols declared well enough for the translator
 *  to emit calls to them but never actually defined.  A good message to
 *  pass here is something "not found".
 */
void linkerr(const char *fmt, ...) {
  va_list args;
  error_context();
  print_stderr("***\n"
               "***  Error in SETL VM code:\n");
  va_start(args, fmt);
  show_msg(fmt, args);
  va_end(args);
  print_stderr("***  Use \"%s -c ...\" for VM code on stdout.\n",
               setlprog);
  version_trailer();
  exit(1);
}

void runerr(const char *fmt, ...) {
  va_list args;
  va_start(args, fmt);
  vrunerr(fmt, args);  /* doesn't return */
  /* We don't reach this point, but we put va_end in places like this
   * anyway in case va_end is a macro that is required for syntactic
   * reasons (such as containing a closing brace to match an opening
   * one in va_start):  */
  va_end(args);
}

void panic(const char *fmt, ...) {
  const char *pfx = "Panic! ";
  char *panic_msg;
  va_list args;
  va_start(args, fmt);
  /* Since panic() is used when system functions fail unexpectedly,
   * make sure we don't call file_rites() in vrunerr(), but do call
   * abort(), since such failures probably merit investigation with
   * the debugger:  */
  abort_on_error = true;
  panic_msg = (char *) alloca(strlen(pfx) + strlen(fmt) + 1);
  if (UNLIKELY (panic_msg == NULL)) {
    vrunerr(fmt, args);  /* just skip the pfx */
  } else {
    strcpy(panic_msg, pfx);  /* prepend the pfx */
    strcat(panic_msg, fmt);
    vrunerr(panic_msg, args);
  }
  va_end(args);
}

static void vrunerr(const char *fmt, va_list args) {
  kill_coprocs();  /* send SIGTERM to stream-associated subprocesses */
  error_context();
  show_msg(fmt, args);
  traceback();
  version_trailer();
#ifdef ABORT_ON_RUNERR
  print_stderr("***  Calling abort() due to ABORT_ON_RUNERR defn...\n");
  abort();
#else
  if (abort_on_error) {
    print_stderr("***  Calling abort() due to abort_on_error flag...\n");
    abort();
  } else {
    file_rites();  /* flush all streams; close all but 'can_persist' */
    _exit(1);
  }
#endif
}

static void errhead(void) {
  print_stderr("***\a\n"
               "***  Report from '%s'  ***\n"
               "***\n",        setlprog);
}

static void version_trailer(void) {
  print_stderr("***\n"
               "***  This is version %s of GNU SETL.\n"
               "***\n",        PACKAGE_VERSION);
}

#define PRE_CONTEXT 5 /* lines */
#define POST_CONTEXT 2 /* lines */

/* This squishes multiple spaces into single (FIXME?).
 * It also doesn't put extra indentation on "continuation" lines,
 * which is probably fine.  Nor is it sensitive to the output device
 * width, which is not great but no big deal (it tries to limit lines
 * to 72 columns, but will not insert a newline before column 36).  */
static void show_msg(const char *fmt, va_list args) {
  va_list args2;
  size_t buflen;
  char *buf;
  char *token;
  va_copy (args2, args);
  print_stderr("***\n");
  buflen = (size_t)vsnprintf (NULL, 0, fmt, args);
  buf = (char *)alloca(buflen + 1);
  assert_in_errage (buf != NULL);
  vsnprintf (buf, buflen + 1, fmt, args2);
  va_end (args2);
  assert_in_errage (strlen(buf) == buflen);
  token = strtok(buf, " ");
  while (true) {
    size_t n = 0;
    print_stderr("*** ");
    while (token != NULL && (n <= 36 || n+1+strlen(token) <= 72)) {
      print_stderr(" %s", token);
      n += 1+strlen(token);
      token = strtok(NULL, " ");
    }
    print_stderr("\n");
    if (token == NULL) break;
  }
  print_stderr("***\n");
}

static void error_context(void) {
  long pc,iv,srcnum,k,i,j,f,g,n,m,lq[PRE_CONTEXT];
  int c;
  tuple *code;
  tuple *vcode, *sources;
  codeline *line;
  instr *ip;
  symtab *sym;
  source *src;
  string *filename,*srctext;
  const char *name;
  if (!setl_vm) goto before_ex;
  pc = setl_vm->pc;
  code = setl_vm->code;
  assert_in_errage (is_tuple(code));
  assert_in_errage (pc >= 1);
  assert_in_errage (pc <= code->nelt);
  ip = (instr *)tupelt(code,pc);
  assert_in_errage (is_instr(ip));
  iv = ip->vindex;
  sym = setl_vm->sym;
  if (!sym) goto unknown_loc;
  assert_in_errage (is_symtab(sym));
  vcode = sym->vcode;
  sources = sym->sources;
  assert_in_errage (is_tuple(vcode));
  assert_in_errage (is_tuple(sources));
  assert_in_errage (iv >= 1);
  assert_in_errage (iv <= vcode->nelt);
  line = (codeline *)tupelt(vcode,iv);
  assert_in_errage (is_codeline(line));
  srcnum = line->srcnum;
  if (srcnum < 1 || srcnum > sources->nelt) goto unknown_loc;
  k = line->offset;
  src = (source *)tupelt(sources,srcnum);
  assert_in_errage (is_source(src));
  filename = src->filename;
  srctext = src->srctext;
  if (!filename || !srctext) goto unknown_loc;
  assert_in_errage (is_string(filename));
  assert_in_errage (is_string(srctext));
  if (k > srctext->nchar) goto unknown_loc;
  for (n=0; n<PRE_CONTEXT; n++) lq[n] = 1;
  f = -1;  /* last "filename" we found in a # or #line directive */
  g = -1;  /* trailing " on that filename */
  m = 1;  /* last line we got a # or #line directive for */
  n = 1;  /* line we're scanning */
  for (i=1; i<=k; i++) {
    if (strelt(srctext,i) == '\n') {
      n++;
      lq[n%PRE_CONTEXT] = i+1;
    } else if (cpp_emission(srctext,&i,&n,&f,&g)) {
      /* i is loc of trailing \n, and n is line num from directive */
      m = n;
      lq[n%PRE_CONTEXT] = i+1;
    }
  }
  if (f == -1) {
    name = &strelt(filename,1);
  } else {  /* f set by cpp_emission() to loc of opening doublequote */
    char *namebuf;
    namebuf = (char *) alloca(g-f);
    if (namebuf != NULL) {
      strncpy_plus_nul(namebuf, &strelt(srctext,f+1), g-f-1);
      name = namebuf;
    } else {
      name = "<unknown>";  /* couldn't copy the given filename */
    }
  }
  errhead();
  print_stderr("***  Error in or near line %ld of %s:\n", n, name);
  print_stderr("***\n");
  /* If m is too far back, move it up to a few lines before the error */
  if (m < n - (PRE_CONTEXT - 1)) m = n - (PRE_CONTEXT - 1);
  if (m < 1) m = 1;
  i = lq[m%PRE_CONTEXT];
  while (m <= n) {
    assert_in_errage (i == lq[m%PRE_CONTEXT]);
#define start_highlight print_stderr(BOLD)
#define stop_highlight print_stderr(NORM)
    if (m == n) start_highlight;
    print_stderr("%6ld  ", m);
    while (i<=srctext->nchar && (c=strelt(srctext,i))!='\n') {
      putc_stderr(c);
      i++;
    }
    if (m == n) stop_highlight;
    putc_stderr('\n');
    i++;
    m++;
  }
  start_highlight;
  for (j=0; j<8; j++) putc_stderr(' ');
  j = lq[n%PRE_CONTEXT];
  while (j<=k) {
    if (strelt(srctext,j) == '\t') putc_stderr('\t');
    else putc_stderr(' ');
    j++;
  }
  putc_stderr('$');
  stop_highlight;
  putc_stderr('\n');
  while (m<=n+POST_CONTEXT &&
         i<=srctext->nchar &&
         !cpp_emission(srctext,&i,&n,&f,&g)) {
    print_stderr("%6ld  ", m);
    while (i<=srctext->nchar && (c=strelt(srctext,i)) != '\n') {
      putc_stderr(c);
      i++;
    }
    putc_stderr('\n');
    i++;
    m++;
  }
  return;
unknown_loc:
  errhead();
  print_stderr("***  Error at SETL VM code line %ld:\n", iv);
  return;
before_ex:
  errhead();
  print_stderr("***  Error before SETL VM execution:\n");
  return;
} /* end error_context */

/* adapted from ../tran/util.c cpp_emission(), which see for deets */
static bool cpp_emission(string *srctext, long *pi, long *pn, long *pf, long *pg) {
  const char *src;
  long srclen, i, j, k, n, f, g;
  /* we do no SETL heap allocation here, so this alias is safe: */
  src = &strelt(srctext,1);
  srclen = srctext->nchar;
  i = *pi - 1;  /* for local 0-based indexing of src */
  if (src[i] != '#') return false;
  if (i > 0 && src[i-1] != '\n') return false;
  k = j = ++i;  /* point i, j, and k at char after # */
  while (k < srclen && src[k] != '\n') k++;  /* point k at \n */
  if (k == srclen) return false;  /* unterminated line */
  while (j < k && !blankortab[(uchar)src[j]]) j++;
  if (j == k) return false;  /* short line */
  if (j != i &&  /* not # */
     (j != i+4 || !lpfx(&src[i],"line"))) return false;  /* not #line */
  while (j < k && blankortab[(uchar)src[j]]) j++;
  if (j == k || !decdigit[(uchar)src[j]]) return false;
  n = 0;
  while (j < k && decdigit[(uchar)src[j]]) {
    n = 10*n + (src[j] - '0');
    j++;
  }
  if (j == k) return false;  /* short line */
  /* we permit #line 55"spam" as does GNU cpp */
  while (j < k && blankortab[(uchar)src[j]]) j++;
  if (j == k) return false;  /* short line */
  if (src[j] != '"') return false;  /* no "filename" */
  f = j;  /* point f at opening doublequote */
  j = k;
  while (src[j] != '"') j--;  /* scan right-to-left for last doublequote */
  if (j == f) return false;  /* there is only one doublequote */
  g = j;  /* location of closing doublequote */
  *pi = k+1;  /* 1-based index of trailing \n */
  *pn = n;    /* line number */
  *pf = f+1;  /* 1-based index of opening doublequote of filename */
  *pg = g+1;  /* 1-based index of closing doublequote of filename */
  return true;
}

static void traceback(void) {
  if (setl_vm && setl_vm->sym) {
    symtab *sym = setl_vm->sym;
    tuple *procnames = sym->procnames;
    frame *fp;
    print_stderr("***  Active routines:\n"
                 "***\n");
    /*
     *  The alternative to "eating" this display in order to give the
     *  traceback would be to make a copy of it first.
     */
    while (setl_vm->level != NO_LEVEL &&
           (fp = setl_vm->display[setl_vm->level]) != NULL) {
      proc *pp = (proc *)tupelt(setl_vm->code, fp->proc_pc);
      long procnum = pp->procnum.longval;
      if (1 <= procnum && procnum <= procnames->nelt) {
        string *procid = (string *)tupelt(procnames,procnum);
        print_stderr("***  %s\n", tame(&strelt(procid,1)));
      } else {
        print_stderr("***  <unknown routine of internal number %ld>\n",
                                                               procnum);
      }
      setl_vm->display[setl_vm->level] = fp->link;
      setl_vm->level = fp->caller_level;
    }
  }
} /* end traceback */


void internal_assert_failure(const char *assertion,
                             const char *filename,
                             long line) {
  bugerr("Assertion \"%s\" failed at %s:%ld",
                      assertion,     filename,line);
}

void internal_check_failure(const char *expr,
                            const char *filename,
                            long line) {
  bugerr("Expression \"%s\" failed at %s:%ld",
                       expr,          filename,line);
}

void internal_case_failure(const char *expr,
                           long value,
                           const char *filename,
                           long line) {
  bugerr("Unexpected value (%ld) of %s at %s:%ld",
                           value,   expr, filename,line);
}

void unexpected_string_value(const char *expr,
                             const char *value,
                             const char *filename,
                             long line) {
  bugerr("Unexpected value (%s) of %s at %s:%ld",
                      tame(value), expr, filename,line);
}

void tupelt_error(long i, long n,
                   const char *filename, long line) {
  bugerr("Tuple subscript %ld outside range 1..%ld at %s:%ld",
                           i,                   n, filename,line);
}

void strelt_error(long i, long n,
                   const char *filename, long line) {
  bugerr("String subscript %ld outside range 1..%ld at %s:%ld",
                            i,                   n, filename,line);
}
