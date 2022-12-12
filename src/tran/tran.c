/*  ===  The SETL Translator  ======================================  */

/*  $Id: tran.c,v 1.38 2021/11/26 03:26:27 setlorg Exp $  */

/*  Free software (c) dB - see file COPYING for license (GPL).  */

/*
 *  The 'tran' routine here is called directly by 'main'.
 */

/* ------------------------------------------------------------------ */

#include "setltran.h"
#include "y.tab.h"

enum font_enum {
  roman_font  = 1,
  italic_font = 2,
  bold_font   = 3,
fonts_end};
typedef enum font_enum font;

/* "Definitions" for the "declarations" in setltran.h:  */
const char *tranprog;   /* external name of this translator program */
const char *source;     /* external location of SETL source text */
const char *sink;       /* external destination of "object" code */
bool verbose;           /* verbose flag */
bool strict;            /* strict language flag */
bool debug;             /* translator debugging flag */
bool spew_font_hints;   /* spew prettyprinting font hints */
namecase keycase;       /* how keywords are recognized */
namecase tagcase;       /* how user tags are recognized */
int maxerrors;          /* max #errors before quitting */
int nerrors;            /* this many detected so far */
bool lexing_done;       /* translator state flag */
bool preparse_done;     /* translator state flag */
bool parsing_done;      /* translator state flag */
bool analyzing_done;    /* translator state flag */
bool generating_done;   /* translator state flag */
bool optimizing_done;   /* translator state flag */
bool spewing_done;      /* translator state flag (the normal spewing) */
char *src;              /* the entire program source text */
long srclen;            /* its length in bytes */
token *tokens;          /* array of input tokens */
long ntokens;           /* number of tokens in the array */
node *root;             /* the distinguished parse tree node */
symtab sysrottab;       /* "system procedure" symbol table */
symtab publictab;       /* symtabs from package specs */
symtab privatetab;      /* PROGRAM/PACKAGE private symtabs */
symtab paxinuse;        /* PACKAGEs in USE by the current unit */
symtab usetabs;         /* map of 'paxinuse' sets, one per unit */
symtab unittabs;        /* map of tables of imported symbols */
int scopectr;           /* scope number generator */
scopestack *curscope;   /* the route of all evil */
csect *virtcode;        /* list of routines made by 'gencode' */

/* Local data */
static bool cmdlineprog;
static bool src_eof;
static int argprogress;

/* Local routines */
static bool getsrc(void);
static void tran_one(void);
static font font_hint(int);
static void usage(void);



/* ------------------------------------------------------------------ */


/* Set up and call the translator */

void tran(unsigned argc, char *const argv[]) {

  unsigned argi;
  int gotfilename;
  bool help, version;  /* some modal cmd line args we handle locally */

  mem_init();           /* Initialize ephemeral memory manager */
  utilinit();           /* 'util.c' has to do some initializations */
  charinit();           /* Initialize character tables */

  maxerrors = 5;        /* #errors tolerated before quitting */
  nerrors = 0;          /* #errors flagged so far */

  /* Our basename: */
  tranprog = strrchr (argv[0], dirsepchar);
  tranprog = tranprog == NULL ? argv[0] : &tranprog[1];

  verbose = false;      /* default is terse */
  strict  = false;      /* default is language extensions */
  debug   = false;      /* default is no translator debugging */
  help    = false;      /* default is not help mode */
  version = false;      /* default is not version mode */
  spew_font_hints = false;  /* default is to spew code, not font hints */
  keycase = anycase;    /* default is keywords may be any case */
  tagcase = anycase;    /* default is user tags may be any case */

  gotfilename = false;
  cmdlineprog = false;

  root         = NULL;
  sysrottab    = NULL;
  publictab    = NULL;
  privatetab   = NULL;
  paxinuse     = NULL;
  usetabs      = NULL;
  unittabs     = NULL;

  for (argi=1; argi!=argc; argi++) {
    if (strlen(argv[argi]) > 1 && *argv[argi] == '-') {
      /*
       *  Be a little unforgiving about command-line options here,
       *  since "normally" we will be called from a massaging driver
       *  like the 'setl' command.
       */
      if      (leq(argv[argi],"-v") ||
               leq(argv[argi],"-verbose") ||
               leq(argv[argi],"--verbose"))  verbose = true;
      else if (leq(argv[argi],"-strict") ||
               leq(argv[argi],"--strict"))   strict = true;
      else if (leq(argv[argi],"-debug") ||
               leq(argv[argi],"--debug"))    debug = true;
      else if (leq(argv[argi],"-h") ||
               leq(argv[argi],"-?") ||
               leq(argv[argi],"-help") ||
               leq(argv[argi],"--help"))     help = true;
      else if (leq(argv[argi],"-version") ||
               leq(argv[argi],"--version"))  version = true;
      /*
       *  The -font-hints command-line option causes
       *  lines of the form "i j k" to be spewed on stdout,
       *  where i is the source character offset beginning
       *  each token, j is the offset ending it (j-i == length)
       *  and k is a font hint:  1==roman, 2==italic, 3==bold.
       *
       *  With this option, the font hints are spewed right after
       *  pre-parsing, and the translator then exits immediately:
       */
      else if (leq(argv[argi],"-font-hints") ||
               leq(argv[argi],"--font-hints")) spew_font_hints = true;
      else if (leq(argv[argi],"-keyword-case=upper") ||
               leq(argv[argi],"--keyword-case=upper") ||
               leq(argv[argi],"-keycase=upper") ||
               leq(argv[argi],"--keycase=upper"))
                                               keycase = uppercase;
      else if (leq(argv[argi],"-keyword-case=lower") ||
               leq(argv[argi],"--keyword-case=lower") ||
               leq(argv[argi],"-keycase=lower") ||
               leq(argv[argi],"--keycase=lower"))
                                               keycase = lowercase;
      else if (leq(argv[argi],"-keyword-case=any") ||
               leq(argv[argi],"--keyword-case=any") ||
               leq(argv[argi],"-keycase=any") ||
               leq(argv[argi],"--keycase=any"))
                                               keycase = anycase;
      else if (leq(argv[argi],"-identifier-case=upper") ||
               leq(argv[argi],"--identifier-case=upper") ||
               leq(argv[argi],"-tagcase=upper") ||
               leq(argv[argi],"--tagcase=upper"))
                                               tagcase = uppercase;
      else if (leq(argv[argi],"-identifier-case=lower") ||
               leq(argv[argi],"--identifier-case=lower") ||
               leq(argv[argi],"-tagcase=lower") ||
               leq(argv[argi],"--tagcase=lower"))
                                               tagcase = lowercase;
      else if (leq(argv[argi],"-identifier-case=any") ||
               leq(argv[argi],"--identifier-case=any") ||
               leq(argv[argi],"-tagcase=any") ||
               leq(argv[argi],"--tagcase=any"))
                                               tagcase = anycase;
      else if (leq(argv[argi],"-identifier-case=mixed") ||
               leq(argv[argi],"--identifier-case=mixed") ||
               leq(argv[argi],"-tagcase=mixed") ||
               leq(argv[argi],"--tagcase=mixed"))
                                               tagcase = mixedcase;
      else {
        fprintf(stderr,"%s:  unrecognized option \"%s\"\n",
                        tranprog,                  argv[argi]);
        nerrors++;
      }
    } else if (leq(argv[argi],"-")) {
      source = "standard input";
      gotfilename = true;
    } else if (gotfilename) {
      fprintf(stderr,"%s:  extra argument \"%s\"\n",
                      tranprog,             argv[argi]);
      nerrors++;
    } else {
      source = argv[argi];
      gotfilename = true;
      if (freopen(source,"r",stdin) == NULL) {
        /* cannot open as file, so assume the arg is the program */
        cmdlineprog = true;
      }
    }
  }

  if (nerrors > 0) quit();

  if (help) {
    usage();
    exit(0);
  }

  if (version) {
    printf("GNU SETL %s translator\n", PACKAGE_VERSION);
    exit(0);
  }

  if (!gotfilename) {
    source = "standard input";
  }

  if (verbose) {
    if (leq(source,"standard input")) {
      fprintf(stderr,"%s:  reading from standard input\n",
                      tranprog);
    } else if (cmdlineprog) {
      fprintf(stderr,"%s:  reading from string \"%s\"\n",
                      tranprog,                  source);
    } else {
      fprintf(stderr,"%s:  reading from file \"%s\"\n",
                      tranprog,                source);
    }
  }

  sink = "standard output";

  toksyminit();                 /* Initialize token symbol table */
  sysrotinit();                 /* Initialize sysrot symbol table */
  src_eof = false;              /* Source EOF has not yet occurred */
  argprogress = 0;              /* Command-line program digestion */

  mem_ephemeral(true);

  if (verbose) fprintf(stderr,"%s:  done initialization (%d ms)\n",
                               tranprog,                 millisec());
  while (getsrc()) {
    if (verbose) fprintf(stderr,"%s:  read src of %ld bytes (%d ms)\n",
                                 tranprog,        srclen,   millisec());
    if (cmdlineprog) source = "command-line argument";
    tran_one();                 /* Apply the translator proper */
    if (verbose) fprintf(stderr,"%s:  done translation (%d ms)\n",
                                 tranprog,              millisec());
    mem_reset();
    if (verbose) fprintf(stderr,"%s:  done garbage collection (%d ms)\n",
                                 tranprog,                  millisec());
  }

  exit(0);                      /* Terminate gracefully */

} /* end tran */


/* .................................................................. */


/*
 *  Read source into a string 'src' which grows as necessary.
 *
 *  The source ends at a line containing just "%END" or at end of
 *  file.  If there is no more input, or if the very first thing
 *  is "%END", getsrc() yields false; otherwise src is all the
 *  source up to but not including the %END, and srclen its length.
 *  A newline is generously tacked on to the last input line if it
 *  doesn't have one (input was ended by end of file in that case).
 */

static bool getsrc(void) {
  long curchunk;
  int c;

# define TXTCHUNK 4096  /* #chars to allocate for source initially */

# define addchar(c) {\
    if (srclen >= curchunk) {\
      resize(src, 2*curchunk, char);\
      curchunk *= 2;\
    }\
    src[srclen++] = c;\
  }

  srclen = 0;
  if (src_eof) return false;

  /* This is silly, but it seemed easier to implement it than to
   * document some inconsistency regarding %END:  */
  if (cmdlineprog) {
    const char *start = &source[argprogress];
    if (strncmp(start, "%END\n", strlen("%END\n")) != 0) {
      const char *stop = strstr(start, "\n%END\n");
      if (stop) {
        srclen = (stop - start) + 1;  /* + 1 to account for \n */
        argprogress += srclen+strlen("%END\n");
        getmem(src, srclen+1, char);  /* + 1 more for the NUL */
        strncpy(src, start, srclen);
        src[srclen] = '\0';
        return true;
      }
      srclen = strlen(start);
      if (srclen > 0 && start[srclen-1] != '\n') {
        srclen++;
        getmem(src, srclen+1, char);
        strncpy(src, start, srclen-1);
        src[srclen-1] = '\n';
      } else {
        getmem(src, srclen+1, char);
        strcpy(src, start);
      }
      src[srclen] = '\0';
      src_eof = true;
      if (srclen > 0) return true;
      return false;
    }
    src_eof = true;
    return false;
  }

  curchunk = TXTCHUNK;
  getmem(src, curchunk, char);
  while ((c = getchar()) != EOF) {
    addchar(c);
    if (c == '%' && (srclen == 1 || src[srclen-2] == '\n')) {
      if ((c = getchar()) == EOF) goto done;
      addchar(c);
      if (c == 'E') {
        if ((c = getchar()) == EOF) goto done;
        addchar(c);
        if (c == 'N') {
          if ((c = getchar()) == EOF) goto done;
          addchar(c);
          if (c == 'D') {
            if ((c = getchar()) == EOF) goto done;
            addchar(c);
            if (c == '\n') {
              srclen -= strlen("%END\n");
              goto done;
            }
          }
        }
      }
    }
  }
done:
  if (c == EOF) {
    src_eof = true;
  }
  if (srclen == 0) {
    src_eof = true;
    return false;
  }
  if (src[srclen-1] != '\n') {
    addchar('\n');
  }
  addchar('\0');
  srclen--;

  /* src now ends with a newline followed by a null */

  return true;

} /* end getsrc */


/* .................................................................. */


/* Do one complete SETL program translation */

static void tran_one(void) {

  lexing_done = false;
  preparse_done = false;
  parsing_done = false;
  analyzing_done = false;
  generating_done = false;
  optimizing_done = false;
  spewing_done = false;

  tokenize();                   /* split source into "tokens" */
  if (verbose) fprintf(stderr,"%s:  done lexical analysis (%d ms)\n",
                                tranprog, millisec());
  lexing_done = true;
  if (nerrors > 0) quit();

  preparse();                   /* scan for user-defined operators */
  if (verbose) fprintf(stderr,"%s:  done pre-parse (%d ms)\n",
                                tranprog, millisec());
  preparse_done = true;
  if (nerrors > 0) quit();

  if (spew_font_hints) {
    long i;
    for (i=0; i<ntokens; i++) {
      token *t = &tokens[i];
      printf("%ld %ld %d\n",t->srcloc,t->srcend,font_hint(t->code));
    }
    fflush(stdout);
    return;
  }

  parse();                      /* do syntax analysis */
  if (verbose) fprintf(stderr,"%s:  done parsing (%d ms)\n",
                                tranprog,       millisec());
  parsing_done = true;
  if (nerrors > 0) quit();

  analyze();                    /* do semantic analysis */
  if (verbose) fprintf(stderr,"%s:  done semantic analysis (%d ms)\n",
                                tranprog,               millisec());
  analyzing_done = true;
  if (nerrors > 0) quit();

  gencode();                    /* construct virtual machine code */
  if (verbose) fprintf(stderr,"%s:  done virtual code generation (%d ms)\n",
                                tranprog,               millisec());
  generating_done = true;
  if (nerrors > 0) quit();

  optimize();                   /* do some code optimizations */
  if (verbose) fprintf(stderr,"%s:  done optimization (%d ms)\n",
                                tranprog,               millisec());
  optimizing_done = true;
  if (nerrors > 0) quit();

  spewobj();                    /* spew or store "object" file */
  if (verbose) fprintf(stderr,"%s:  done code emission (%d ms)\n",
                                tranprog,               millisec());
  spewing_done = true;
  if (nerrors > 0) quit();

} /* end tran_one */


/* .................................................................. */


static font font_hint(int code) {
  font hint = bold_font;
  switch (code) {
  case C_code:     hint = roman_font; break;
  case Comment:    hint = roman_font; break;  /* should never really occur */
  case Integer:    hint = roman_font; break;
  case Machine:    hint = roman_font; break;
  case Macro_name: hint = italic_font; break;
  case Name:       hint = italic_font; break;
  case Real_:      hint = roman_font; break;
  case Ref_name:   hint = italic_font; break;
  case String:     hint = roman_font; break;
  }
  return hint;
}


/* .................................................................. */


static void usage(void) {
  printf("\n"
   "GNU SETL programming language translator (compiler)\n"
   "\n"
   "Usage: %s [OPTIONS] [FILENAME | - | STRING]\n",
           tranprog);
  printf("\n"
   "  --help, -h      display this help on stdout and exit\n"
   "  --version       display version info on stdout and exit\n"
   "  --font-hints    emit source prettyprinting hints, period\n"
   "  --verbose, -v   otiose sucrose on stderr\n"
   "  --debug         trace parsing, etc. on stderr\n"
   "  --keyword-case=any|upper|lower   (\"stropping\" convention) -\n"
   "                   control SETL keyword recognition (default any)\n"
   "  --identifier-case=any|upper|lower|mixed   control recognition\n"
   "                   of user variable names (default any)\n"
   "\n"
   "The %s command reads from standard input by default or if \"-\"\n",
        tranprog);
  printf(
   "is specified.  Otherwise, if FILENAME names a readable file, it reads\n"
   "from there.  Failing that, it reads directly from STRING.\n");
  printf("\n"
   "When the translator is invoked by a command like \"setl -c ...\", the\n"
   "preprocessor (setlcpp) is applied first if necessary.\n");
  printf("\n"
   "If the Texinfo documentation is installed, \"info setltran\" may work.\n"
   "PDF and HTML docs are usually under share/doc/setl/ somewhere.\n");
  printf("\n"
   "See setl.org for more documentation, source code, etc.\n");
  printf("\n"
   "Please report bugs to %s.\n"
   "\n",                  PACKAGE_BUGREPORT);
} /* end usage */
