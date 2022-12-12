/*  ===  Global initializations  ===================================  */

/*  $Id: init.c,v 1.75 2021/12/03 16:27:23 setlorg Exp $  */

/*  Free software (c) dB - see file COPYING for license (GPL).  */

#include "setlrun.h"

static void char_init(void);
static void char_fini(void);
static void trtinit(bool trttab[], const char chars[]);
static void trinit(char trtab[], const char from[], const char to[]);

static void gmplib_init(void);
static void gmplib_fini(void);

static void table_init(void);
static void table_fini(void);
static void entryinit(table *tbl, const char *str, int cod);

static void sig_init(void);
static void sig_fini(void);
static void free_pid_list(pid_list *list);
static void sigchld_handler(int sig);

static void file_init(void);
static void file_fini(void);

/*
 *  Exported data.
 */

/*
 *  Initialized by 'char_init':
 */
bool terminator[CHARSETSIZE];  /* line terminator '\n' */
bool separator [CHARSETSIZE];  /* input field separator '\t\0' */
bool prefix    [CHARSETSIZE];  /* input operand prefix */
bool alphabetic[CHARSETSIZE];  /* a-z and A-Z */
bool idtail    [CHARSETSIZE];  /* alpha+digit+'_' */
bool strok     [CHARSETSIZE];  /* unmodified by PRETTY */
bool musttwin  [CHARSETSIZE];  /* repeated by PRETTY */
bool ctrlchar  [CHARSETSIZE];  /* '\a\b\f\n\r\t\v' */
bool whitespace[CHARSETSIZE];  /* ' \f\n\r\t\v' */
bool blankortab[CHARSETSIZE];  /* ' \t' */
bool valsep    [CHARSETSIZE];  /* whitespace+',' */
bool nongarbage[CHARSETSIZE];  /* valsep+']}' */
bool exponent  [CHARSETSIZE];  /* 'eE' */
bool plusminus [CHARSETSIZE];  /* '+-' */
bool numeric   [CHARSETSIZE];  /* '0123456789+-.eE' */
bool decdigit  [CHARSETSIZE];  /* '0123456789' */
bool posdigit  [CHARSETSIZE];  /* '123456789' */
bool hexdigit  [CHARSETSIZE];  /* '0123456789abcdefABCDEF' */
bool octdigit  [CHARSETSIZE];  /* '01234567' */

char to_lower  [CHARSETSIZE];  /* upper- to lowercase */
char to_upper  [CHARSETSIZE];  /* lower- to uppercase */

int  hexval    [CHARSETSIZE];   /* base 16 */
int  octval    [CHARSETSIZE];   /* base 8 */
int  digval    [CHARSETSIZE];   /* base 10 */
int  setl2digval[CHARSETSIZE];  /* base 2 thru 36 */

backslash_chaser escapee[CHARSETSIZE];

/*
 *  Initialized by 'gmplib_init':
 */
#if HAVE_LLONG
mpz_t            const_mpz_llong_min;  /* LLONG_MIN as an mpz_t */
#endif
gmp_randstate_t  randstate;    /* random integer generator state */
static void *(*saved_alloc)(size_t);
static void *(*saved_realloc)(void *, size_t, size_t);
static void (*saved_free)(void *, size_t);

/*
 *  Initialized by 'table_init':
 */
table  *optab;        /* map from string opcode to integer opcode */
HANDLE h_optab;
table  *sysdats;      /* map from system "variable" name to number */
HANDLE h_sysdats;
table  *sysrots;      /* map from system routine name to number */
HANDLE h_sysrots;
table  *sysrotsigs;   /* map from system routine name to signature */
HANDLE h_sysrotsigs;
set    *name_to_fd;   /* mmap from name to file descriptor */
HANDLE h_name_to_fd;

/*
 *  Initialized by 'sig_init':
 */
sig_info *sigfo;               /* info about signals */
pid_list *reapable;            /* CLOSE_AUTOREAP pids */
pid_list *reaped;              /* a free list */

/*
 *  Initialized by 'file_init':
 */
int  fd_lo, fd_hi;             /* normal fd range */
int  sd_lo, sd_hi;             /* pseudo-fd range */
int  n_files;                  /* aggregate limit */
file *files;                   /* addressed by fd */


void init(void) {
  char_init();   /* initialize character recognition etc. tables */
  arena_init();  /* initialize transient-arena allocator */
  gmplib_init(); /* initialize some GNU MP library items */
  heap_init();   /* initialize relocatable heap memory allocator (heap.c) */
  table_init();  /* initialize optab, sysrots, sysdats, and name_to_fd */
  sig_init();    /* initialize signal info and establish SIGCHLD handler */
  file_init();   /* initialize 'files' and open default std SETL streams */
}

void fini(void) {
  file_fini();   /* close remaining SETL streams and retire 'files' */
  sig_fini();    /* restore SIGCHLD action and finalize signal info */
  table_fini();  /* release table handles */
  heap_fini();   /* heap memory allocator (heap.c) finalization */
  gmplib_fini(); /* GNU MP library finalization */
  arena_fini();  /* transient-arena allocator finalization */
  char_fini();   /* character table finalization if any */
}


#define alphabet_lower "abcdefghijklmnopqrstuvwxyz"
#define alphabet_upper "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
#define alphabet alphabet_lower alphabet_upper

/*
 *  Fill in character recognition, translation, and classification tables:
 */
static void char_init(void) {

  int i,j,n;

  trtinit(terminator,"\n");
  trtinit(separator,"\t");
  separator['\0'] = true;       /* hard to pass to 'trtinit' in C */
  trtinit(prefix,"<>:");
  trtinit(alphabetic,alphabet);
  trtinit(idtail,alphabet "_0123456789");
  trtinit(strok,alphabet " 0123456789`~!@#$%^&*()-_=+[{]}\\|;:'\",<.>/?");
  trtinit(musttwin,"'\\");
  trtinit(ctrlchar,"\a\b\f\n\r\t\v");
  trtinit(whitespace, " \f\n\r\t\v");
  trtinit(blankortab, " \t");
  trtinit(valsep,     " \f\n\r\t\v,");
  /* This one must be equal to 'valsep' plus ']' plus '}':  */
  trtinit(nongarbage," ,\f\n\r\t\v]}");
  trtinit(exponent,"eE");
  trtinit(plusminus,"+-");
  trtinit(numeric,"0123456789+-.eE");
  trtinit(decdigit,"0123456789");
  trtinit(posdigit,"123456789");
  trtinit(hexdigit,"0123456789abcdefABCDEF");
  trtinit(octdigit,"01234567");

  trinit(to_lower, alphabet_upper, alphabet_lower);
  trinit(to_upper, alphabet_lower, alphabet_upper);

# define deftab(t,c) {\
  n = CHARSETSIZE;\
  for (i=0; i<n; i++) t[i] = c;\
}
# define cchar(t,s,c) {\
  n = strlen(s);\
  for (i=0; i<n; i++) t[(uchar)(s[i])] = c;\
}

  deftab (escapee,unescapable);
  cchar  (escapee," `~!@#$%^&*()-_=+[{]}\\|;:'\",<.>/?",repself);
  cchar  (escapee,"abfnrtv",control);
  cchar  (escapee,"x",hexlead);
  cchar  (escapee,"0123",octlead);

  deftab(hexval,-9999);
  j = 0; cchar (hexval,"0123456789abcdefg",j++);
  j = 0; cchar (hexval,"0123456789ABCDEFG",j++);

  deftab(octval,-9999);
  j = 0; cchar (octval,"01234567",j++);

  deftab(digval,-9999);
  j = 0; cchar (digval,"0123456789",j++);

  deftab(setl2digval,-9999);
  j = 0; cchar (setl2digval,"0123456789" alphabet_lower, j++);
  j = 0; cchar (setl2digval,"0123456789" alphabet_upper, j++);

} /* end char_init */

static void char_fini(void) {
}

/* "trt" after the IBM /370 "translate and test" instruction:  */
static void trtinit(bool trttab[], const char chars[]) {
  int i;
  for (i=0; i<CHARSETSIZE; i++) trttab[i] = false;
  for (i=0; chars[i]; i++) trttab[(uchar)(chars[i])] = true;
}

/* "tr" after the IBM /370 "translate" instruction:  */
static void trinit(char trtab[], const char from[], const char to[]) {
  int i, n;
  for (i=0; i<CHARSETSIZE; i++) trtab[i] = i;
  assert (strlen(from) == strlen(to));
  n = strlen(from);
  for (i=0; i<n; i++) trtab[(uchar)(from[i])] = to[i];
}

static void gmplib_init(void) {
#if HAVE_LLONG
  /* This goes before the switch to the arena allocator, so that GMP
   * will use the default allocator for these long-lived "constants":  */
  mpz_init (const_mpz_llong_min);  /* there is no mpz_init_ui_pow_ui() */
  mpz_ui_pow_ui (const_mpz_llong_min, 2, LLONG_BIT - 1);  /* e.g. 2**63 */
  mpz_neg (const_mpz_llong_min, const_mpz_llong_min);
#endif
  /* This goes before the switch to the arena allocator, so that GMP
   * will use the default allocator for the long-lived randstate:  */
  gmp_randinit_default(randstate);  /* init GMP random number generator state */
  /* Save existing allocator functions and replace with ours:  */
  mp_get_memory_functions (&saved_alloc, &saved_realloc, &saved_free);
  mp_set_memory_functions ( arena_alloc,  arena_realloc,  arena_free);
}

static void gmplib_fini(void) {
  /* Restore GMP's default allocator functions */
  mp_set_memory_functions (saved_alloc, saved_realloc, saved_free);
  gmp_randclear(randstate);  /* finalize GMP random number generator state */
#if HAVE_LLONG
  mpz_clear (const_mpz_llong_min);
#endif
}

static void sysrotsig_init(const char *name, long nopnd);
static void sysrotsig_init(const char *name, long nopnd) {
  tuple *t = new_tuple(nopnd);  HANDLE ht = ref(t);
  key *k = tokey((block *)new_string(name));
  insert_tuple (sysrotsigs, k, t);  /* later entries clobber earlier */
  retire(ht);
}

static void sysrotsig_parm(const char *name, long iopnd, const char *p);
static void sysrotsig_parm(const char *name, long iopnd, const char *p) {
  key *k = tokey((block *)new_string(name));  HANDLE hk = ref(k);
  tuple *t = lookup_tuple (sysrotsigs, k);  HANDLE ht = ref(t);
  let (tupelt(t,iopnd), (block *)new_string(p));  /* parm incl direction */
  insert_tuple (sysrotsigs, k, t);  /* since we didn't track the subnode */
  retire(ht);
  retire(hk);
}

static void table_init(void) {

  /* Tables, with handles that outlive this procedure execution */
  optab = empty_table();       h_optab = ref(optab);
  sysdats = empty_table();     h_sysdats = ref(sysdats);
  sysrots = empty_table();     h_sysrots = ref(sysrots);
  sysrotsigs = empty_table();  h_sysrotsigs = ref(sysrotsigs);

  /* Fill in the tables, using auto-generated code */
# define optab_init(str,cod)  entryinit(optab,str,cod)
# define sysrot_init(str,cod) entryinit(sysrots,str,cod)
# define sysdat_init(str,cod) entryinit(sysdats,str,cod)
# include "opcode_inits.c"
# include "sysdat_inits.c"
# include "sysrot_inits.c"
# include "sysrotsigs.c"

  /* Mmap from filenames (and like 2-tuples) to file descriptors */
  name_to_fd = null_set();  h_name_to_fd = ref(name_to_fd);

} /* end table_init */

static void table_fini(void) {
  /* Give back the table handles */
  retire(h_name_to_fd);  name_to_fd = NULL;  /* ossia OM */
  retire(h_sysrotsigs);  sysrotsigs = NULL;
  retire(h_sysrots);     sysrots = NULL;
  retire(h_sysdats);     sysdats = NULL;
  retire(h_optab);       optab = NULL;
} /* end table_fini */

static void entryinit(table *tbl, const char *str, int cod) {
  HANDLE h = ref(tbl);
  string *s = new_string(str);  HANDLE hs = ref(s);
  key *k = tokey((block *)s);   HANDLE hk = ref(k);
  small *i = new_small(cod);    HANDLE hi = ref(i);
  subnode *b;
  keysub(tbl,k,&b);
  b->d = (block *)i;
  retire(hi);
  retire(hk);
  retire(hs);
  retire(h);
} /* end entryinit */


/*
 *  Allocate and initialize the 'sigfo' array, save the existing
 *  SIGCHLD action (which must not already be to call our handler),
 *  and put our handler (sigchld_handler()) in place.
 */
static void sig_init(void) {

  int sig;
  sigset_t old_mask;

  sigfo = (sig_info *) os_malloc(N_SIGNAL * sizeof sigfo[0]);

  for (sig=0; sig<N_SIGNAL; sig++) {
    sig_info *p = &sigfo[sig];
    p->readers = NULL;
    p->ignorers = NULL;
    p->defaulters = NULL;
    /* p->timerid is left uninitialized. */
    /* p->old_action is left uninitialized. */
  }

  reapable = NULL;
  reaped = NULL;

  /* Block SIGCHLD while we arrange to catch it... */
  os_sigblock(SIGCHLD, &old_mask);    /* critsect-enter */
  /* Save old signal action and establish new handler.  */
  os_signal(SIGCHLD, sigchld_handler, &sigfo[SIGCHLD].old_action);
  os_sigsetmask(&old_mask);           /* critsect-exit */

} /* end sig_init */

/*
 *  Restore the SIGCHLD handling to what it was before sig_init(),
 *  free the 'reaped' and 'reapable' lists, and free the 'sigfo' array.
 */
static void sig_fini(void) {

  sigset_t old_mask;

  os_sigblock(SIGCHLD, &old_mask);    /* critsect-enter */
  os_sigrestore(SIGCHLD, &sigfo[SIGCHLD].old_action);
  os_sigsetmask(&old_mask);           /* critsect-exit */

  free_pid_list(reaped);    /* free all the members of 'reaped' */
  reaped = NULL;            /* defensive */
  free_pid_list(reapable);  /* free all the members of 'reapable' */
  reapable = NULL;          /* defensive */

  os_free(sigfo);

} /* end sig_fini */

static void free_pid_list(pid_list *list) {
  while (list) {
    pid_list *save = list->next;  /* hang on to list->next */
    os_free(list);  /* because this may clobber it */
    list = save;    /* so you can't just say list = list->next here */
  }
} /* end free_pid_list */

/*
 *  This SIGCHLD catcher reaps and discards the termination statuses of
 *  child processes associated with pipe/pump I/O streams that have been
 *  CLOSEd in CLOSE_AUTOREAP mode.
 *
 *  Note that SIGCHLD is blocked during the handler execution.  Other
 *  manipulators of the 'reapable' and 'reaped' lists should also block
 *  SIGCHLD around their work, so that this handler doesn't interfere.
 */
static void sigchld_handler(int sig) {
  pid_list *rover, *plover, *grover;  /* 'reapable' list ptrs */
  assert (sig == SIGCHLD);
  /* Take care of the other user-level signal-handling semantics */
  sig_handler(sig);  /* "hook" the (almost) all-purpose handler */
  plover = NULL;  /* predecessor if any of rover in 'reapable' list */
  for (rover = reapable; rover != NULL; rover = grover) {
    pid_t pid = os_waitpid(rover->pid, NULL, WNOHANG);
    grover = rover->next;  /* preserve rover->next */
    if (pid != 0) {  /* "reaped" or "error" (cf. file_close) */
      /*
       *  We do not bother to capture the raw status here, because
       *  the SIGCHLD event that caused us to be here occurs
       *  asynchronously.  (Contrast this with the synchronous contexts
       *  of SYSTEM, FILTER, WAIT, WAITPID, and CLOSE(...,CLOSE_AWAIT).
       *  In those places, os_waitpid() is called without WNOHANG,
       *  and the resulting raw status is passed to set_raw_status().)
       *
       *  Since free() isn't async-signal-safe, we can't call os_free()
       *  here - this handler could be entered in the middle of a
       *  malloc() or free() that was already in progress.  So we just
       *  move the retiring member to the 'reaped' list, access to which
       *  only occurs when SIGCHLD is blocked.  CLOSE preferentially
       *  allocates from 'reaped' whenever it wants to add a member to
       *  'reapable'.
       */
      if (plover) {  /* rover has a predecessor */
        plover->next = grover;  /* excise rover by bypassing it */
      } else {  /* rover must still be the head ('reapable') */
        reapable = grover;  /* excise rover by updating head */
      }
      rover->next = reaped;  /* link rover into 'reaped' list */
      reaped = rover;     /* make rover the new 'reaped' head */
      /* since we "stole" rover, leave plover alone */
    } else {
      /* pid exists but was not waited away; leave node on 'reapable' */
      plover = rover;  /* "previous" rover for next loop iter */
    }
  }
} /* end sigchld_handler */


static void file_init(void) {

  int nfd, fd;

  nfd = os_max_open_files();  /* 1 + maximum allowed fd */

  fd_lo = 0*nfd;        /* C index into 'files' for a normal fd */
  fd_hi = 0*nfd + nfd;  /* 1 beyond their range */
  sd_lo = 1*nfd;        /* C index into 'files' for a pseudo-fd */
  sd_hi = 1*nfd + nfd;  /* 1 beyond their range */
  n_files = 2*nfd;      /* total number of elements of 'files' */

  /*
   *  The array of 'file' structs is kept outside of the relocatable
   *  SETL heap, as are the dynamically allocated I/O buffers and
   *  char arrays for things like filenames that may exist when the
   *  file is open (type other than no_file).  Their turnover rate
   *  and space requirements are expected to be modest enough not to
   *  raise fragmentation concerns.
   */
  files = (file *) os_malloc(n_files * sizeof files[0]);
  for (fd=0; fd<n_files; fd++) {
    file *f = &files[fd];
    f->fd = fd;
    init_file(f, no_file, 0);
  }

  /*
   *  The can_persist ability causes these std streams to retain their
   *  SETL-level buffers across program executions.  The SETL-level
   *  opens implied by these new_file() calls thus don't get answered
   *  with corresponding closes until file_fini() (below) is called,
   *  except where an explicit CLOSE was done.
   *
   *  The function that examines can_persist is file_rites() in sys.c,
   *  called in go()'s main loop over SETL pgms, after execute() returns.
   *  With can_persist set, file_rites() does a mere file_flush()
   *  rather than a full file_close().  Currently, it is set only on the
   *  std streams.  See also std_redir() in sys.c.
   *
   *  The can_persist ability is also recognized by del_file(), which
   *  file_close() calls in most contexts.  Its role in del_file() is
   *  to inhibit closing of the OS-level fd.
   *
   *  See also std_redir() in sys.c, which seeks to make the fd_stdin
   *  and fd_stdout streams in a new child process look as they do upon
   *  creation here.
   */
  if (os_is_fd (fd_stdin)) {  /* fd 0 is open at the POSIX level */
    new_file (fd_stdin, stream_file, can_read | can_persist,
                                                        input_buffering);
  }
  if (os_is_fd (fd_stdout)) {  /* fd 1 is open at the POSIX level */
    new_file (fd_stdout, stream_file, can_write | can_persist,
                 os_isatty(fd_stdout) ? line_buffering : full_buffering);
  }
  if (os_is_fd (fd_stderr)) {  /* fd 2 is open at the POSIX level */
    /* Although we say line_buffering here, new_file() makes a special
     * case of fd_stderr, and makes it byte_buffering unless the
     * SETL_LINEBUF_STDERR envt var is defined:  */
    new_file (fd_stderr, stream_file, can_write | can_persist,
                                                         line_buffering);
  }

  errno = 0;  /* LAST_ERROR is initially NO_ERROR */

} /* end file_init */

/*
 *  See comments in file_rites(), which is called after each execute().
 */
static void file_fini(void) {

  int fd;

  flush_all();  /* flush all stream output buffers */
  /*
   *  This should end up seeing only file_rites()'s leftovers, which is
   *  to say the can_persist std files.  In calling file_close() on them,
   *  we will flush and release their SETL buffers, but not close() them.
   *
   *  Since file_fini() is called as part of shutting down the SETL VM
   *  after the last SETL program has finished executing on it, any std
   *  streams that were open when the VM was started and haven't been
   *  reopened during some SETL pgm execution will remain open after the
   *  VM finalization.
   */
  for (fd=n_files; fd-->0; ) {
    file *f = find_file(fd);
    /*
     *  If the fd is connected to a child process, close_zombie
     *  says abandon the process after closing the connection, as is
     *  appropriate when we are about to exit.  In reality, all but
     *  the std streams will have been closed already by file_rites()
     *  by the time we are called.
     */
    if (f->ftype != no_file) file_close(fd, close_zombie);
  }

  os_free(files);

} /* end file_fini */
