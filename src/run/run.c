/*  ===  SETL top-level driver  ====================================  */

/*  $Id: run.c,v 1.95 2022/11/17 14:16:07 setlorg Exp $  */

/*  Free software (c) dB - see file COPYING for license (GPL).  */

/*
 *  Previously, the sources here were used to generate 'setlrun',
 *  which embodied the SETL virtual machine.  In those days, the
 *  'setl' command was generated in ../driver/.
 *
 *  But now 'setl' is generated here, and performs the offices of both
 *  the old "driver" and the run-time (VM).  An advantage of this is
 *  that the process started by the user is also the run-time process.
 *
 *  Another improvement is that the first fd available to the user's
 *  program is typically 3 rather than some higher number.
 *
 *  Retained is the ability to process multiple programs separated by
 *  %END lines (and terminated by end of file or two %ENDs in a row).
 *
 *  This 'setl' also invokes 'setlcpp' (the GNU SETL preprocessor) and
 *  'setltran' (the GNU SETL translator, i.e., compiler) as needed.
 */

#include "setlrun.h"

/* Default command name stems for the preprocessor and the translator:  */
#ifndef CPPSTUB
#define CPPSTUB  "setlcpp"
#endif
#ifndef TRANSTUB
#define TRANSTUB "setltran"
#endif

/* An extra prefix to help support weird versions of the above:  */
#ifndef PFX
#define PFX ""
#endif

const char *setlprog;    /* GNU SETL driver/run-time command basename */
bool    spew;            /* spew assembled code; don't 'execute' */

bool    verbose;         /* running commentary on execution */
bool    debug;           /* gory details of memory, etc. */

bool    abort_on_error;  /* abort() rather than exit() on SETL error */

bool    restricted;      /* disallow security hazards */

size_t  maxmem,memsize;  /* control the allocator */

allowed  *allowed_open_list;     /* for --allow-open options */
allowed  *allowed_fd_open_list;  /* for --allow-fd-open options */
allowed  *allowed_mkstemp_list;  /* for --allow-mkstemp options */
allowed  *allowed_filter_list;   /* for --allow-filter options */
allowed  *allowed_system_list;   /* for --allow-system options */


tuple   *command_line;   /* execution (run-time) arguments */
HANDLE  h_command_line;

string  *command_name;   /* source name or "setl" */
HANDLE  h_command_name;

integer *stdin_integer;   /* fd_stdin as an INTEGER */
HANDLE  h_stdin_integer;
integer *stdout_integer;  /* fd_stdout as an INTEGER */
HANDLE  h_stdout_integer;

mode_t  initial_umask;   /* umask in effect when this pgm was invoked */

long    atom_counter;    /* for NEWAT calls */

machine *setl_vm;  /* current SETL virtual machine instance */
HANDLE  h_setl_vm;

static const char *setlpath;  /* argv[0] of this program */
static string *obj;  static HANDLE h_obj;
static long obj_index;

static allowed *add_allowed(allowed **head);
static void free_allowed(allowed **head);
static void get_input (string **str, io_buffer *b);
static bool ortho_line_directive (const char *text);
static string *apply_cpp (const char *command,
                          const char **args,
                          string     *in,
                          int        *statusp);
static string *apply_tran (const char *command,
                           const char **args,
                           string     *in,
                           int        *statusp);
static string *apply_cmd (const char *command,
                          const char *cmdname,
                          const char **args,
                          string     *in,
                          int        *statusp);
static string *quote (const char *s);
static void usage(void);


int run (unsigned argc, char *const argv[]) {

  const char **cppargs;   unsigned ncarg;  /* args for preprocessor */
  const char **tranargs;  unsigned ntarg;  /* args for translator */
  const char **userargs;  unsigned nuarg;  /* args for user program */
  const char *inputarg;
  const char *setlcpp;
  const char *setltran;
  unsigned argi;
  int raw_status;    /* like from waitpid() or system() */
  int exit_status;   /* what main() will return if we return */
  enum {cpp_unknown, cpp_yes, cpp_no} cpp;  /* whether to apply cpp */
  bool want_version;
  bool tran_only;
  bool already_translated;
  bool use_srcname;  /* COMMAND_NAME is user pgm name (true) or our name */
  bool cmdline_pgm;  /* pgm is contained in a cmd line arg */
  bool success;
  io_buffer *inbuf;

  setlpath = argv[0];    /* ostensibly our invocation pathname */
  setlprog = strrchr (setlpath, dirsepchar);
  setlprog = setlprog == NULL ? setlpath : &setlprog[1];  /* basename */

  spew    = false;         /* default is to execute code, not spew it */
  verbose = false;         /* default is terse */
  debug   = false;         /* default is no debug output */
  abort_on_error = false;  /* default is exit() on SETL error */
  restricted = false;      /* default is unrestricted rights */
  maxmem  = ULONG_MAX;     /* default maximum memory size */
  memsize = 100*1000UL;    /* default starting memory size */

  cppargs = (const char **) os_malloc ((argc+2) * sizeof cppargs[0]);
  cppargs[0] = "-C";
  cppargs[1] = "-lang-setl";
  ncarg = 2;

  tranargs = (const char **) os_malloc (argc * sizeof tranargs[0]);
  ntarg = 0;

  userargs = (const char **) os_malloc (argc * sizeof userargs[0]);
  nuarg = 0;

  inputarg = NULL;  /* no input arg seen yet */
  setlcpp = NULL;   /* no preprocessor command arg seen yet */
  setltran = NULL;  /* no translator command arg seen yet */

  allowed_open_list = NULL;
  allowed_fd_open_list = NULL;
  allowed_mkstemp_list = NULL;
  allowed_filter_list = NULL;
  allowed_system_list = NULL;

  initial_umask = os_get_umask();

  cpp = cpp_unknown;  /* don't yet know if we need to apply cpp */
  want_version = false;  /* turned on by -V (--version) */
  tran_only = false;     /* turned true by -c (--compile) */
  already_translated = false;  /* turned true by -t (--translated) */
  use_srcname = false;  /* by default, COMMAND_NAME is setlprog */
  cmdline_pgm = false;  /* pgm in arg is not the default */

  for (argi=1; argi<argc; argi++) {

    const char *a = argv[argi];

    if (inputarg != NULL) {
      /* We have seen the arg designating the program.  Everything after
       * that is an arg for the program.  */
      userargs[nuarg++] = a;

    } else if (leq(a,"-") ||
               leq(a,"--")) {
      /* '--' is a traditional and POSIXy way of separating options for
       * the language processor from those intended for the user pgm.
       * Since we default to stdin when there is no input arg, we
       * pretend '--' was spelled '-', which is the explicit stdin.  */
      inputarg = "-";

    } else if (a[0] == '-') {

      if        (leq(a,"-h") ||
                 leq(a,"-?") ||
                 leq(a,"-help") ||
                 leq(a,"--help")) {
        usage();
        return 0;

      } else if (leq(a,"-V") ||
                 leq(a,"-version") ||
                 leq(a,"--version")) {
        want_version = true;

      } else if (leq(a,"-c") ||
                 leq(a,"-compile") ||
                 leq(a,"--compile")) {
        tran_only = true;

      } else if (leq(a,"-font-hints") ||
                 leq(a,"--font-hints")) {
        cpp = cpp_no;  /* can be overridden by subsequent -cpp */
        tran_only = true;
        tranargs[ntarg++] = "--font-hints";

      } else if (lpfx(a,"-keyword-case=") ||
                 lpfx(a,"--keyword-case=") ||
                 lpfx(a,"-keycase=") ||
                 lpfx(a,"--keycase=")) {
        const char *p = &strchr(a,'=')[1];
        if      (leq_ic(p,"upper")) tranargs[ntarg++] = "--keycase=upper";
        else if (leq_ic(p,"lower")) tranargs[ntarg++] = "--keycase=lower";
        else if (leq_ic(p,"any"))   tranargs[ntarg++] = "--keycase=any";
        else cmdlinerr ("Keyword case must be upper, lower, or any,"
                        " not \"%s\"", tame(p));

      } else if (lpfx(a,"-identifier-case=") ||
                 lpfx(a,"--identifier-case=") ||
                 lpfx(a,"-tagcase=") ||
                 lpfx(a,"--tagcase=")) {
        const char *p = &strchr(a,'=')[1];
        if      (leq_ic(p,"upper")) tranargs[ntarg++] = "--tagcase=upper";
        else if (leq_ic(p,"lower")) tranargs[ntarg++] = "--tagcase=lower";
        else if (leq_ic(p,"mixed")) tranargs[ntarg++] = "--tagcase=mixed";
        else if (leq_ic(p,"any"))   tranargs[ntarg++] = "--tagcase=any";
        else cmdlinerr ("Identifier case must be upper, lower, mixed,"
                        " or any, not \"%s\"", tame(p));

      } else if (leq(a,"-cpp") ||
                 leq(a,"--cpp")) {
        cpp = cpp_yes;

      } else if (leq(a,"-nocpp") ||
                 leq(a,"--nocpp") ||
                 leq(a,"-no-cpp") ||
                 leq(a,"--no-cpp")) {
        cpp = cpp_no;

      } else if (lpfx(a,"-I") ||
                 lpfx(a,"-D") ||
                 lpfx(a,"-U")) {
        cpp = cpp_yes;
        cppargs[ncarg++] = a;

      } else if (leq(a,"-t") ||
                 leq(a,"-translated") ||
                 leq(a,"--translated")) {
        already_translated = true;

      } else if (leq(a,"-spew") ||
                 leq(a,"--spew")) {
        spew = true;

      } else if (leq(a,"-r") ||
                 leq(a,"-restricted") ||
                 leq(a,"--restricted")) {
        restricted = true;

      } else if (lpfx(a,"-allow-open=") ||
                 lpfx(a,"--allow-open=")) {
        const char *what = &strchr(a,'=')[1];  /* "what,mode" part */
        const char *comma = strrchr(what,',');  /* ",mode" part */
        const char *mode_name;
        const open_mode *mode;
        size_t n;
        if (comma != NULL) {
          n = comma-what;  /* length of "what" part */
          mode_name = &comma[1];  /* "mode" part */
        } else {  /* no comma, "what" part only */
          n = strlen(what);  /* length of "what" */
          mode_name = "R";  /* implicit R mode */
        }
        if (n == 0) {
          cmdlinerr("Missing what to allow in \"%s\"", tame(a));
        }
        mode = lookup_open_mode(mode_name);
        if (mode != NULL) {
          /* Add the what and mode to the allowed_open_list */
          allowed *allowed_open = add_allowed(&allowed_open_list);
          allowed_open->what = strndup_malloc(what, n);
          allowed_open->how = mode->how;
        } else {
          cmdlinerr("Unrecognized I/O mode after comma in \"%s\"",
                                                        tame(a));
        }

      } else if (lpfx(a,"-allow-fd-open=") ||
                 lpfx(a,"--allow-fd-open=")) {
        const char *what = &strchr(a,'=')[1];  /* "fd,mode" part */
        const char *comma = strrchr(what,',');  /* ",mode" part */
        const char *mode_name;
        const open_mode *mode;
        size_t n;
        if (comma != NULL) {
          n = comma-what;  /* length of "fd" part */
          mode_name = &comma[1];  /* "mode" part */
        } else {  /* no comma, "fd" part only */
          n = strlen(what);  /* length of "fd" */
          mode_name = "R";  /* implicit R mode */
        }
        if (n == 0) {
          cmdlinerr("Missing file descriptor in \"%s\"", tame(a));
        }
        mode = lookup_open_mode(mode_name);
        if (mode != NULL) {
          if (isdigit((uchar)what[0])) {
            long fd;
#ifndef __clang__
GCC_DIAG_OFF(discarded-qualifiers)
#else
GCC_DIAG_OFF(incompatible-pointer-types-discards-qualifiers)
#endif
            char *e = what;  /* blame strtol() for this tossing of const */
#ifndef __clang__
GCC_DIAG_ON(discarded-qualifiers)
#else
GCC_DIAG_ON(incompatible-pointer-types-discards-qualifiers)
#endif
            int saved_errno = errno;
            errno = 0;
            fd = strtol(what,&e,10);
            if (errno == 0 && (e[0]=='\0' || e[0]==',') &&
                 0 <= fd && fd <= os_max_open_files()) {
              /* Add the fd and mode to the allowed_fd_open_list */
              allowed *allowed_fd_open = add_allowed(&allowed_fd_open_list);
              allowed_fd_open->fd = fd;
              allowed_fd_open->how = mode->how;
              errno = saved_errno;
            } else {
              cmdlinerr("Invalid file descriptor in \"%s\"", tame(a));
            }
          } else {
            cmdlinerr("Malformed file descriptor in \"%s\"", tame(a));
          }
        } else {
          cmdlinerr("Unrecognized I/O mode after comma in \"%s\"",
                                                        tame(a));
        }

      } else if (lpfx(a,"-allow-mkstemp=") ||
                 lpfx(a,"--allow-mkstemp=")) {
        const char *what = &strchr(a,'=')[1];  /* filename template */
        const size_t n = strlen(what);  /* length of template */
        if (n > 6 && leq(&what[n-6],"XXXXXX")) {
          /* Add the template to the allowed_mkstemp_list */
          allowed *allowed_mkstemp = add_allowed(&allowed_mkstemp_list);
          allowed_mkstemp->what = strdup_malloc(what);
        } else {
          cmdlinerr("Filename template in \"%s\" must end in XXXXXX",
                                        tame(a));
        }

      } else if (lpfx(a,"-allow-filter=") ||
                 lpfx(a,"--allow-filter=")) {
        const char *what = &strchr(a,'=')[1];  /* command */
        /* Add the command to the allowed_filter_list */
        allowed *allowed_filter = add_allowed(&allowed_filter_list);
        allowed_filter->what = strdup_malloc(what);

      } else if (lpfx(a,"-allow-system=") ||
                 lpfx(a,"--allow-system=")) {
        const char *what = &strchr(a,'=')[1];  /* command */
        /* Add the command to the allowed_system_list */
        allowed *allowed_system = add_allowed(&allowed_system_list);
        allowed_system->what = strdup_malloc(what);

      } else if (leq(a,"-k")) {
        /* Warnings suck, but here's one anyway, as an example:  */
        print_stderr("%s:  ignoring obsolete option -k\n", setlprog);

      } else if (lpfx(a,"-maxmem=") ||
                 lpfx(a,"--maxmem=")) {
        const char *d = &strchr(a,'=')[1];  /* ptr to the size */
        if (isdigit((uchar)d[0])) {
#ifndef __clang__
GCC_DIAG_OFF(discarded-qualifiers)
#else
GCC_DIAG_OFF(incompatible-pointer-types-discards-qualifiers)
#endif
          char *e = d;
#ifndef __clang__
GCC_DIAG_ON(discarded-qualifiers)
#else
GCC_DIAG_ON(incompatible-pointer-types-discards-qualifiers)
#endif
          int saved_errno = errno;
          errno = 0;
          maxmem = strtoul(d,&e,10);  /* try to convert the number */
          if (errno == 0) {
            while (e[0] != '\0') {  /* account for suffixes if any */
              if        ((e[0] == 'K' || e[0] == 'k') &&
                         maxmem <= (ULONG_MAX >> 10)) {
                maxmem <<= 10;
              } else if ((e[0] == 'M' || e[0] == 'm') &&
                         maxmem <= (ULONG_MAX >> 20)) {
                maxmem <<= 20;
              } else if ((e[0] == 'G' || e[0] == 'g') &&
                         maxmem <= (ULONG_MAX >> 30)) {
                maxmem <<= 30;
#if LONG_BIT >= 64
              } else if ((e[0] == 'T' || e[0] == 't') &&
                         maxmem <= (ULONG_MAX >> 40)) {
                maxmem <<= 40;
              } else if ((e[0] == 'P' || e[0] == 'p') &&
                         maxmem <= (ULONG_MAX >> 50)) {
                maxmem <<= 50;
              } else if ((e[0] == 'E' || e[0] == 'e') &&
                         maxmem <= (ULONG_MAX >> 60)) {
                maxmem <<= 60;
#endif
              } else {
                cmdlinerr("Invalid size suffix in \"%s\"", tame(a));
              }
              e = &e[1];
            }
          } else {
            cmdlinerr("Invalid size in \"%s\"", tame(a));
          }
          errno = saved_errno;
        } else {
          cmdlinerr("Malformed size in \"%s\"", tame(a));
        }
        /* 0 means explicit selection of the default "unlimited" */
        if (maxmem == 0) maxmem = ULONG_MAX;

      } else if (lpfx(a,"-setlcpp=") ||
                 lpfx(a,"--setlcpp=")) {
        setlcpp = &strchr(a,'=')[1];

      } else if (lpfx(a,"-setltran=") ||
                 lpfx(a,"--setltran=")) {
        setltran = &strchr(a,'=')[1];

      } else if (leq(a,"-v") ||
                 leq(a,"-verbose") ||
                 leq(a,"--verbose")) {
        verbose = true;
        cppargs[ncarg++] = "-v";
        tranargs[ntarg++] = "--verbose";

      } else if (leq(a,"-debug") ||
                 leq(a,"--debug")) {
        debug = true;
        /* Don't use -g3; it produces a bunch of #defines on setlcpp's
         * stdout before the regular output, confusing setltran:  */
/*      cppargs[ncarg++] = "-g3"; */
        tranargs[ntarg++] = "--debug";

      } else if (leq(a,"-abort-on-error") ||
                 leq(a,"--abort-on-error")) {
        /* Not currently supported for the translator or preprocessor;
         * otherwise extend tranargs and/or cppargs here.  */
        abort_on_error = true;

      } else if (isdigit((uchar)a[1])) {
        /* Not actually an option, but rather a fd to open, making it
         * the input arg.  */
        long i = 2;
        while (isdigit((uchar)a[i])) i++;
        if (a[i] == '\0') {
          inputarg = a;
        } else {
          cmdlinerr("Malformed fd in \"%s\"", tame(a));
        }
      } else {
        cmdlinerr("Unrecognized option \"%s\"", tame(a));
      }

    } else if (inputarg == NULL) {
      inputarg = a;

    } else {
      userargs[nuarg++] = a;
    }

  } /* end for */

  cppargs[ncarg] = NULL;
  tranargs[ntarg] = NULL;

  if (inputarg == NULL) inputarg = "-";  /* stdin */

  if (memsize > maxmem) memsize = maxmem;

  if (want_version) {
    os_printf("GNU SETL %s\n", PACKAGE_VERSION);
    if (verbose) {

      /* adapted from Linux feature_test_macros(7) page */

      #ifdef _POSIX_SOURCE
      os_printf("_POSIX_SOURCE defined\n");
      #endif

      #ifdef _POSIX_C_SOURCE
      os_printf("_POSIX_C_SOURCE defined: %ldL\n", (long) _POSIX_C_SOURCE);
      #endif

      #ifdef _ISOC99_SOURCE
      os_printf("_ISOC99_SOURCE defined\n");
      #endif

      #ifdef _ISOC11_SOURCE
      os_printf("_ISOC11_SOURCE defined\n");
      #endif

      #ifdef _XOPEN_SOURCE
      os_printf("_XOPEN_SOURCE defined: %d\n", _XOPEN_SOURCE);
      #endif

      #ifdef _XOPEN_SOURCE_EXTENDED
      os_printf("_XOPEN_SOURCE_EXTENDED defined\n");
      #endif

      #ifdef _LARGEFILE64_SOURCE
      os_printf("_LARGEFILE64_SOURCE defined\n");
      #endif

      #ifdef _FILE_OFFSET_BITS
      os_printf("_FILE_OFFSET_BITS defined: %d\n", _FILE_OFFSET_BITS);
      #endif

      #ifdef _BSD_SOURCE
      os_printf("_BSD_SOURCE defined\n");
      #endif

      #ifdef _SVID_SOURCE
      os_printf("_SVID_SOURCE defined\n");
      #endif

      #ifdef _DEFAULT_SOURCE
      os_printf("_DEFAULT_SOURCE defined\n");
      #endif

      #ifdef _ATFILE_SOURCE
      os_printf("_ATFILE_SOURCE defined\n");
      #endif

      #ifdef _GNU_SOURCE
      os_printf("_GNU_SOURCE defined\n");
      #endif

      #ifdef _REENTRANT
      os_printf("_REENTRANT defined\n");
      #endif

      #ifdef _THREAD_SAFE
      os_printf("_THREAD_SAFE defined\n");
      #endif

      #ifdef _FORTIFY_SOURCE
      os_printf("_FORTIFY_SOURCE defined: %d\n", _FORTIFY_SOURCE);
      #endif

      #ifdef __USE_ISOC11
      os_printf("__USE_ISOC11 = %d\n", __USE_ISOC11);
      #endif

      #ifdef __USE_ISOC99
      os_printf("__USE_ISOC99 = %d\n", __USE_ISOC99);
      #endif

      #ifdef __USE_ISOC95
      os_printf("__USE_ISOC95 = %d\n", __USE_ISOC95);
      #endif

      #ifdef __USE_ISOCXX11
      os_printf("__USE_ISOCXX11 = %d\n", __USE_ISOCXX11);
      #endif

      #ifdef __USE_POSIX
      os_printf("__USE_POSIX = %d\n", __USE_POSIX);
      #endif

      #ifdef __USE_POSIX2
      os_printf("__USE_POSIX2 = %d\n", __USE_POSIX2);
      #endif

      #ifdef __USE_POSIX199309
      os_printf("__USE_POSIX199309 = %d\n", __USE_POSIX199309);
      #endif

      #ifdef __USE_POSIX199506
      os_printf("__USE_POSIX199506 = %d\n", __USE_POSIX199506);
      #endif

      #ifdef __USE_XOPEN
      os_printf("__USE_XOPEN = %d\n", __USE_XOPEN);
      #endif

      #ifdef __USE_XOPEN_EXTENDED
      os_printf("__USE_XOPEN_EXTENDED = %d\n", __USE_XOPEN_EXTENDED);
      #endif

      #ifdef __USE_UNIX98
      os_printf("__USE_UNIX98 = %d\n", __USE_UNIX98);
      #endif

      #ifdef __USE_XOPEN2K
      os_printf("__USE_XOPEN2K = %d\n", __USE_XOPEN2K);
      #endif

      #ifdef __USE_XOPEN2KXSI
      os_printf("__USE_XOPEN2KXSI = %d\n", __USE_XOPEN2KXSI);
      #endif

      #ifdef __USE_XOPEN2K8
      os_printf("__USE_XOPEN2K8 = %d\n", __USE_XOPEN2K8);
      #endif

      #ifdef __USE_XOPEN2K8XSI
      os_printf("__USE_XOPEN2K8XSI = %d\n", __USE_XOPEN2K8XSI);
      #endif

      #ifdef __USE_LARGEFILE
      os_printf("__USE_LARGEFILE = %d\n", __USE_LARGEFILE);
      #endif

      #ifdef __USE_LARGEFILE64
      os_printf("__USE_LARGEFILE64 = %d\n", __USE_LARGEFILE64);
      #endif

      #ifdef __USE_FILE_OFFSET64
      os_printf("__USE_FILE_OFFSET64 = %d\n", __USE_FILE_OFFSET64);
      #endif

      #ifdef __USE_MISC
      os_printf("__USE_MISC = %d\n", __USE_MISC);
      #endif

      #ifdef __USE_ATFILE
      os_printf("__USE_ATFILE = %d\n", __USE_ATFILE);
      #endif

      #ifdef __USE_GNU
      os_printf("__USE_GNU = %d\n", __USE_GNU);
      #endif

      #ifdef __USE_FORTIFY_LEVEL
      os_printf("__USE_FORTIFY_LEVEL = %d\n", __USE_FORTIFY_LEVEL);
      #endif

      #ifdef __GLIBC_USE_ISOC2X
      os_printf("__GLIBC_USE_ISOC2X = %d\n", __GLIBC_USE_ISOC2X);
      #endif

      #ifdef __GLIBC_USE_DEPRECATED_GETS
      os_printf("__GLIBC_USE_DEPRECATED_GETS = %d\n", __GLIBC_USE_DEPRECATED_GETS);
      #endif

      #ifdef __GLIBC_USE_DEPRECATED_SCANF
      os_printf("__GLIBC_USE_DEPRECATED_SCANF = %d\n", __GLIBC_USE_DEPRECATED_SCANF);
      #endif
    }
    return 0;
  }

  init();       /* initialize SETL environment */

  /* "Open" the input (source or already translated vcode)... */

  inbuf = NULL;

  if (inputarg[0] == '-') {

    long fdarg;
#ifndef __clang__
GCC_DIAG_OFF(discarded-qualifiers)
#else
GCC_DIAG_OFF(incompatible-pointer-types-discards-qualifiers)
#endif
    char *e = &inputarg[1];
#ifndef __clang__
GCC_DIAG_ON(discarded-qualifiers)
#else
GCC_DIAG_ON(incompatible-pointer-types-discards-qualifiers)
#endif
    int saved_errno = errno;
    errno = 0;
    fdarg = inputarg[1] == '\0' ? 0 : strtol(&inputarg[1],&e,10);
    assert (errno == 0 && fdarg >= 0 && e[0] == '\0');
    errno = saved_errno;
    if (is_normal_fd(fdarg)) {
      file *f = find_fd_file(fdarg);
      if (f->ftype == no_file) {
        integer *p = new_integer(fdarg);
        int fd = file_open((block *)p, open_read, false);
        if (fd >= 0) {
          assert (fd == fdarg);
        } else {
          runerr("Fd %ld in arg is not open", fdarg);
        }
      } else {
        /* already open (normally this will be stdin) */
      }
      inbuf = f->buffer;
      assert (inbuf);
    } else {
      runerr("Fd %ld in arg is out of range %d <= fd < %d",
               fdarg,                    fd_lo,     fd_hi);
    }

  } else if (inputarg[0] == '|') {

    /* read from process */
    string *p = new_string(&inputarg[1]);
    int fd = file_open((block *)p, open_pipe_from, false);
    if (fd >= 0) {
      file *f = find_fd_file(fd);
      inbuf = f->buffer;
      use_srcname = true;
    } else {
      runerr("Cannot pipe from %s", &inputarg[1]);
    }

  } else {

    /* try for file */
    string *p = new_string(inputarg);
    int fd = file_open((block *)p, open_read, false);
    if (fd >= 0) {
      /* opened file named by arg */
      file *f = find_fd_file(fd);
      inbuf = f->buffer;
      use_srcname = true;
    } else {
      /* assume pgm in arg; leave errno set for no particular reason */
      cmdline_pgm = true;
    }
  }

  obj = OM;
  h_obj = ref(obj);

  raw_status = no_status;
  exit_status = 0;
  success = true;

  if (already_translated) {

    /* The input is "object" code.  Get it into 'obj'.  */
    if (cmdline_pgm) {
      obj = new_string (inputarg);  /* weird, but OK... */
    } else {
      obj = null_string();
      get_input (&obj, inbuf);
    }

  } else {

    /* The input is source code.  Get it into 'src'.  */
    string *src = OM;
    HANDLE h_src = ref(src);
    if (cmdline_pgm) {
      src = new_string ("#line 1 \"command-line argument\"\n");
      str_append (&src, inputarg);
      str_tackon (&src, '\n');
    } else {
      src = new_string ("#line 1 \"");
      if (leq(inputarg,"-")) {
        str_append (&src, "standard input");
      } else {
        str_append (&src, inputarg);
      }
      str_append (&src, "\"\n");
      get_input (&src, inbuf);
    }

    /* The %END feature is silly, but here's some spam to make sure it
     * doesn't contribute to confusing diagnostics while I dither over
     * whether to retire it.  No point in making it fast, anyway.  */
    if (lpfx(&strelt(src,1), "%END\n") ||
       strstr(&strelt(src,1), "\n%END\n")) {
      string *new_src = null_string();
      HANDLE h_new_src = ref(new_src);
      long n = 0;  /* current src line counter */
      bool bol = true;  /* beginning of line */
      long i;
      for (i=1; i<=src->nchar; i++) {
        const char c = strelt(src,i);
        str_tackon (&new_src, c);
        if (c == '\n') {
          n++;
          bol = true;
        } else if (bol && c == '%' && lpfx(&strelt(src,i), "%END\n") &&
                    i + (long)strlen("END\n") < src->nchar) {
          char buf[40];
          n++;
          snprintf (buf, sizeof buf, "END\n#line %ld \"", n);
          str_append (&new_src, buf);
          if (cmdline_pgm) {
            str_append (&new_src, "command-line argument\"\n");
          } else if (leq(inputarg,"-")) {
            str_append (&new_src, "standard input\"\n");
          } else {
            str_append (&new_src, inputarg);
            str_append (&new_src, "\"\n");
          }
          i += strlen("%END");
        } else {
          bol = false;
        }
      }
      retire (h_new_src);
      src = new_src;
    }

    /* Determine whether to apply cpp (really setlcpp) or not.  */
    if (cpp == cpp_unknown) {
      const char *s = &strelt(src,1);
      const long n = src->nchar;
      long i;
      for (i=0; i<n && cpp==cpp_unknown; i++) {
        /*
         * Any '#' as the first non-whitespace character on a line
         * will trigger cpp invocation unless the line turns out
         * to be a canonical "#line" directive or a "#!" line.
         */
        const char *next = strchr(&s[i],'#');
        if (next) {
          const long j = next - s;  /* index of '#' */
          long k = j-1;
          while (k>=0 && s[k]!='\n' && isspace((unsigned char)s[k])) {
            k--;
          }
          if (k<0 || s[k]=='\n') {
            /*
             * This indicates some kind of cpp directive or a "#!" on
             * the beginning of the line.
             *
             * Unless it's a rather restricted form of #line directive
             * or "#!", assume this means we will need cpp.
             */
            if (k != j-1 ||
                (!ortho_line_directive(next) && !lpfx(next,"#!"))) {
              cpp = cpp_yes;
            }
          }
          i = j;  /* i := index of '#', but about to be incremented */
        } else {
          /*
           * No qualifying "#..." line was found.
           *
           * Look for "__", tolerating false positives, like when the
           * double underscore occurs in a string, comment, or middle of
           * an identifier (predefined preprocessor symbols _begin_ with
           * double underscores).
           *
           * Failing that, look for backslash, again tolerating false
           * positives.  Really we're only interested in line-ending
           * backslashes (which we'd like the preprocessor to handle,
           * since we don't accommodate them in the translator's lexer),
           * but we trigger cpp on any backslash.
           */
          cpp = strstr(s,"__") ? cpp_yes :
                strchr(s,'\\') ? cpp_yes : cpp_no;
        }
      }
    }

    /* Preprocess src with either cpp or a cheap surrogate.  */
    if (cpp == cpp_yes) {
      src = apply_cpp (setlcpp, cppargs, src, &raw_status);
      success = WIFEXITED(raw_status) && WEXITSTATUS(raw_status) == 0;
    }

    if (success) {
      /* Apply the translator to the preprocessed source to get 'obj'.  */
      obj = apply_tran (setltran, tranargs, src, &raw_status);
      success = WIFEXITED(raw_status) && WEXITSTATUS(raw_status) == 0;
    }

    retire (h_src);
  }

  if (success) {

    if (tran_only) {

      /* emit obj */
      const char *s = &strelt(obj,1);
      const size_t n = obj->nchar;
      check (n == fwrite (s, 1, n, stdout));

    } else {

      command_name = new_string (use_srcname ? inputarg : setlprog);
      h_command_name = ref(command_name);

      command_line = new_tuple(nuarg);
      h_command_line = ref(command_line);
      for (argi=0; argi!=nuarg; argi++) {
        let (tupelt(command_line, (long)argi+1),
             (block *) new_string(userargs[argi]));
      }

      stdin_integer = new_integer(fd_stdin);
      h_stdin_integer = ref(stdin_integer);
      stdout_integer = new_integer(fd_stdout);
      h_stdout_integer = ref(stdout_integer);

      atom_counter = 0;  /* for NEWAT calls */

      setl_vm = OM;
      h_setl_vm = ref(setl_vm);

      obj_index = 1;
      exit_status = go();  /* assemble; then spewcode or execute */

      retire(h_setl_vm);        setl_vm = OM;  /* defensive NULLing */

      retire(h_stdout_integer); stdout_integer = OM;
      retire(h_stdin_integer);  stdin_integer = OM;

      retire(h_command_line);   command_line = OM;
      retire(h_command_name);   command_name = OM;
    }

  } else {

    /*
     * The preprocessor or translator failed.  In theory, one of
     * WIFEXITED (for normal termination) and WIFSIGNALED (for
     * termination by signal) will be true.  In the latter case, we
     * use the same convention as the shell.  In the theoretically
     * impossible case, we return -1, which a shell would see as 255.
     */
    exit_status = WIFEXITED(raw_status) ? WEXITSTATUS(raw_status) :
                WIFSIGNALED(raw_status) ? 128 + WTERMSIG(raw_status) :
                                          -1;
  }

  retire (h_obj);

  fini();      /* clean up, including giving back the heap */

  free_allowed(&allowed_system_list);
  free_allowed(&allowed_filter_list);
  free_allowed(&allowed_mkstemp_list);
  free_allowed(&allowed_fd_open_list);
  free_allowed(&allowed_open_list);

  return exit_status;

} /* end run */


/*
 *  Allocate and initialize an 'allowed' struct, and push it onto a
 *  list that records all instances of a given --allow-* option.
 */
static allowed *add_allowed(allowed **head) {
  allowed *r = (allowed *)os_malloc(sizeof *r);
  r->what = NULL;  /* default; checked by free_allowed() */
  r->next = *head;
  *head = r;
  return r;
}

/*
 *  Release a list of 'allowed' structs, leaving its head NULL.
 */
static void free_allowed(allowed **head) {
  while (*head != NULL) {
    allowed *a = *head;
    if (a->what != NULL) {
      os_free(a->what);
    }
    *head = a->next;
    os_free(a);
  }
}


/* Get all available input and close its source */
static void get_input (string **str, io_buffer *b) {
  int c;
  while ( (c = get_char(b)) != EOF) str_tackon (str, c);
  file_close (b->fd, close_await);
}

/*
 * Rather strict check for "#line" directives; we only count "pure"
 * ones and consider anything else a candidate for cpp.
 *
 * "Pure" here means <<#line digits "text"\n>> where line is literal,
 * digits must have at least 1 digit, and there must be exactly one
 * blank char and one newline where shown and no newline within text.
 *
 * For example, if someone has inserted the special extra tokens
 * that GNU cpp supports on #line directives, they want cpp and
 * not our el cheapo substitution to "#", so we don't count that a
 * "pure" #line directive.  All the ones inserted by this program
 * are "pure".
 */
static bool ortho_line_directive (const char *text) {
  if (lpfx(text, "#line ")) {
    const char *s = &text[6];  /* right after "#line " */
    if (isdigit((unsigned char)s[0])) {
      const char *t = strchr(s, ' ');  /* next blank */
      while (isdigit((unsigned char)(++s)[0])) ;
      if (s == t) {  /* digits field is all digits */
        if (s[1] == '"') {  /* doublequote right after blank */
          const char *u = strchr(&s[2], '\n');  /* newline */
          if (u && u[-1] == '"') {  /* doublequote right before \n */
            if (u-s >= 3) {  /* not just <<#line digits "\n>> */
              return true;
            }
          }
        }
      }
    }
  }
  return false;
}

static string *apply_cpp (const char *command,
                          const char **args,
                          string *in,
                          int *statusp) {
  return apply_cmd (command, PFX CPPSTUB, args, in, statusp);
}

static string *apply_tran (const char *command,
                           const char **args,
                           string *in,
                           int *statusp) {
  return apply_cmd (command, PFX TRANSTUB, args, in, statusp);
}

static string *apply_cmd (const char *command,  /* name given by option */
                          const char *cmdname,  /* default name */
                          const char **args,
                          string *in,
                          int *statusp) {
  HANDLE h_in = ref(in);
  string *cmd = OM;       HANDLE h_cmd = ref(cmd);
  string *out;
  unsigned argi;
  int raw_status;
  if (command != NULL) {
    cmd = new_string (command);  /* command string given by option */
  } else {
    /* If there is a DIRSEP in the argv[0] of how we were
     * invoked, prepend argv[0]'s dirname to the cmdname.  */
    const char *setldir = strrchr (setlpath, dirsepchar);
    if (setldir) {
      const size_t n = setldir - setlpath;  /* length of dir component */
      cmd = new_nstring (setlpath, n+1);  /* explicit path */
    } else {
      cmd = null_string();  /* command name to be sought in PATH */
    }
    str_append (&cmd, cmdname);
  }
  /* Append args */
  for (argi=0; args[argi]; argi++) {
    str_tackon (&cmd, ' ');
    str_concat (&cmd, quote(args[argi]));
  }
  out = do_filter (cmd, in, &raw_status);  /* run the command */
  if (out == OM) {
    runerr("\"%s\" failed", &strelt(cmd,1));
  }
  retire (h_cmd);
  retire (h_in);
  *statusp = raw_status;
  return out;  /* output from command */
}

static string *quote (const char *s) {
  string *r = new_cstring ('\'');
  HANDLE hr = ref(r);
  size_t i;
  for (i=0; s[i]; i++) {
    if (s[i] == '\'') {
      str_append (&r, "'\\''");
    } else {
      str_tackon (&r, s[i]);
    }
  }
  str_tackon (&r, '\'');
  retire (hr);
  return r;
}

/*
 *  'go' gets its lines from us using this.
 */
string *get_line (void) {
  const char *p = &strelt(obj, obj_index);
  const char *q = strchr(p, '\n');
  if (q) {
    const long len = q - p;
    string *r = copy_substring (obj, obj_index, obj_index + len - 1);
    obj_index += len + 1;
    return r;
  }
  /* Since we are dealing in object code here, which is supposed to be
   * properly terminated with "%EXECUTE\n", we will not accommodate
   * incomplete (missing newline) last lines here.  */
  return OM;
}

static void usage(void) {
  os_printf("\n"
   "GNU SETL programming language processor\n"
   "\n"
   "Usage: %s [OPTIONS] [INPUT] [ARGS]\n", setlprog);
  os_printf("\n"
   "  --[no]cpp           force [non]use of preprocessor\n"
   "  -I..., -D..., -U...  passed to setlcpp; these imply --cpp\n"
   "  --compile, -c       emit VM code on stdout, don't run\n"
   "  --translated, -t    input is VM code, not SETL source\n"
   "  --font-hints        just emit source prettyprinting hints\n"
   /* leaving --spew undocumented for now */
   "  --keyword-case=any|upper|lower    (\"stropping\" convention) -\n"
   "                       control keyword recognition (default any)\n"
   "  --identifier-case=any|upper|lower|mixed    control recognition\n"
   "                       of user variable names (default any)\n"
   "  --maxmem=N          limit memory use (k, m, or g suffix OK)\n"
   "  --restricted, -r    restrict rights, for untrusted code\n"
   "  --allow-open=WHAT,HOW ...   restriction exemptions for open()\n"
   "  --allow-fd-open=FD,HOW ...   exemptions for open() over fd\n"
   "  --allow-mkstemp=TEMPLATE ...   exemptions for mkstemp() calls\n"
   "  --allow-filter=COMMAND ...   exemptions for filter() calls\n"
   "  --allow-system=COMMAND ...   exemptions for system() calls\n"
   "  --setlcpp=COMMAND   specify preprocessor command\n"
   "  --setltran=COMMAND  specify translator command\n"
   "  --help, -h          display this help on stdout and exit\n"
   "  --version           display version info on stdout and exit\n"
   "  --verbose, -v       make noise on stderr\n"
   "  --debug             make more noise on stderr\n"
   "  --abort-on-error    dump core for SETL-level error\n"
   "\n"
   "  -FD                 input from numeric file descriptor FD\n"
   "  |COMMAND            input from piped stdout of COMMAND\n"
   "  FILENAME            input from file FILENAME\n"
   "  STRING              get whole program directly from STRING\n"
   "  -, --               input from stdin (default)\n");
  os_printf("\n"
   "Examples:\n");
  os_printf("  %s mypgm.setl my args\n", setlprog);
  os_printf("  %s 'print (\"Hello, world.\");'\n", setlprog);
  os_printf("\n"
   "If the Texinfo documentation is installed, \"info setl\" may work.\n"
   "PDF and HTML docs are usually under share/doc/setl/ somewhere.\n");
  os_printf("\n"
   "See setl.org for more documentation, source code, etc.\n");
  os_printf("\n"
   "Please report bugs to %s.  Thanks for using\n"
   "SETL, the World's Most Wonderful Programming Language!\n"
   "\n",                  PACKAGE_BUGREPORT);
} /* end usage */
