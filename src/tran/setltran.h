/*  --  Declarations shared by the SETL translator modules  --  */

/*  $Id: setltran.h,v 1.28 2024/04/08 18:57:22 setlorg Exp $  */

/*  Free software (c) dB - see file COPYING for license (GPL).  */

#ifndef _setltran_h
#define _setltran_h


/*  This header file is probably in "../":  */
#include "common.h"


/*
 *  Some attributes that SETL variables can have:
 */
#define rdable 1
#define wrable 2
#define rwable (rdable|wrable)
#define starred 4
#define backtrack 8

/*
 *  Some things you can do with SETL variables:
 */
#define rd rdable
#define wr wrable
#define rw rwable
#define unprefixed 0    /* rd,wr,rw also designate opnd prefixes */


/*
 *  Enumeration types
 */

enum namecase_enum {    /* ways of recognizing tags (lexical names) */
  lowercase,              /* only recognized in lowercase */
  uppercase,              /* only recognized in uppercase */
  anycase,                /* case not significant */
  mixedcase,              /* mixed case, case signifcant */
namecases_end};

/* Check 'debug_...' in util.c if you modify this enumeration: */
enum nodetype_enum {    /* types of parse tree nodes */
  null = 0,               /* empty node (should never occur) */
  N_Token,                /* node that is really a token (terminal) */
#include "Nodetypes"
nodetypes_end};

/*
 *  Warning - I still use "tag" to mean "identifier", as in Algol 68,
 *  not "run-time type code" as in Ada parlance.
 */
enum symtype_enum {     /* user tag symbol types */
  sysrotsym,              /* "system routine" listed in ../sysrots */
  packsym,                /* PACKAGE name */
  progsym,                /* PROGRAM name */
  procsym,                /* name of user-declared proc */
  bopsym,                 /* name of user-declared binary op */
  uopsym,                 /* name of user-declared unary op */
  cmdsym,                 /* name of user-declared command */
  labsym,                 /* statement label */
  refsym,                 /* refinement name */
  parsym,                 /* formal parameter */
  varsym,                 /* symbol declared as VAR (even implicitly) */
  initsym,                /* symbol declared with INIT */
  constsym,               /* symbol declared as CONST */
  selsym,                 /* symbol declared as SEL */
  symtabsym,              /* dummy symbol to hold a symtab */
  nosym,                  /* diagonal! - no symbol can have this type */
symtypes_end};

enum scopetype_enum {   /* "control scope" types */
  proc_scope,             /* in a scope with decls and code */
  decls_scope,            /* in declarations */
  stmts_scope,            /* in outer level of statements */
  refinement_scope,       /* in refinement */
  if_scope,               /* in "IF" statement */
  if_expr_scope,          /* in "IF" expression */
  case_of_scope,          /* in "CASE OF" statement */
  case_ex_scope,          /* in "CASE expr OF" statement */
  case_of_expr_scope,     /* in "CASE OF" expression */
  case_ex_expr_scope,     /* in "CASE expr OF" expression */
  expr_clause_scope,      /* in "EXPR" clause */
  loopframe_scope,        /* in loop or former */
  loopbody_scope,         /* in loop, after headers */
  init_scope,             /* in "INIT" loop header */
  doing_scope,            /* in "DOING" loop header */
  step_scope,             /* in "STEP" loop header */
  term_scope,             /* in "TERM" loop header */
scopetypes_end};

enum optype_enum {      /* virtual opcodes */
#include "Optypes"
optypes_end};

enum operatype_enum {   /* operand types */
  unsigned_opera,         /* unsigned integer operand */
  machine_opera,          /* "machine code" operand */
  c_code_opera,           /* "embedded C++ code" operand */
  label_opera,            /* label operand */
  deno_opera,             /* name or literal operand */
operatypes_end};


/*
 *  Typedefs
 */

typedef enum namecase_enum      namecase;
typedef enum nodetype_enum      nodetype;
typedef enum symtype_enum       symtype;
typedef enum scopetype_enum     scopetype;
typedef enum optype_enum        optype;
typedef enum operatype_enum     operatype;
typedef unsigned int            bits;   /* small bit-set */
typedef struct token_struct     token;
typedef struct node_struct      node;
typedef struct subnode *        table;
typedef struct subnode *        symtab;
typedef struct symbol_struct    symbol;
typedef struct symstack_struct  symstack;
typedef struct scopestack_struct scopestack;
typedef struct csect_struct     csect;
typedef struct block_struct     block;
typedef struct label_struct     label;
typedef struct instr_struct     instr;
typedef struct opera_struct     opera;
typedef union datum_union       datum;
typedef node *                  nodeptr;

/*
 *  Type for yacc's "value stack"
 */
#ifndef YYSTYPE
#define YYSTYPE nodeptr
#endif


/*
 *  Record (struct) types
 */

/*
 *  The terminal ('token') and nonterminal ('node') structs are
 *  implicitly derived from an imaginary base class that begins with
 *  a type field followed by a tn (most relevant token number) field.
 *  When the type field is N_Token, parse tree traversers must cast
 *  "node *" to "token *" and process the node as a terminal.
 *
 *  YYSTYPE is declared as "node *".  This is used by the skeleton
 *  parser (in the "y.c" file emitted by yacc or bison) to declare
 *  yyval, yylval, and the elements of its semantic value stack.
 */

struct token_struct {   /* lexical token */
  nodetype type;          /* N_Token, to indicate a leaf node */
  long tn;                /* token number (counting from 0) */
  long srcloc;            /* where the token begins in the source */
  long srcend;            /* where it ends (length = srcend - srcloc) */
  int code;               /* lexical category (see "lexicon" file) */
  const char *graphic;    /* canonical string representation or tag */
};

struct node_struct {    /* parse tree node */
  nodetype type;          /* node type */
  long tn;                /* most relevant token number, if any */
  /* FIXME - Occurrences of int like this could overflow in case of
   * 2**31 subnodes on a 32-bit system, such as in a tuple literal
   * with a list that long.  A workaround is to disallow very long
   * program texts.  Some indexes, such as the token number 'tn'
   * above (max 'ntokens'), have already adopted 'long' instead:  */
  int nsub;               /* number of subnode pointers */
  node *sub[FLEX];        /* array of subnode pointers */
};

struct symbol_struct {  /* symbol as recorded in a symbol table */
  symtype type;           /* symbol type */
  long tn;                /* point of declaration or 1st occurrence */
  const char *tag;        /* normally also the "key" */
  symbol *owner;          /* used in figuring genealogies */
  /* The remaining fields could really just be a union:  */
  int refinement_calls;   /* spam for a weird check we need */
  int scopenum;           /* used for labels */
  bits ability;           /* e.g., rdable+wrable+starred */
  symtab stab;            /* local symbol table (for routine) */
  /* FIXME - 2**31 formals in a decl would blow my tiny mind here:  */
  int nparms;             /* number of formals (for routine), or 0 */
  bits parms[FLEX];       /* array of formals */
};

struct symstack_struct {/* symbol table stack member */
  symbol *holder;         /* symbol that contains a non-NULL symtab */
  symstack *ptr;          /* next lower (outermore) stack element */
};

struct scopestack_struct {/* control scope stack member */
  scopetype type;         /* scope type */
  /* FIXME - usual 32-bit paranoia applies, this time to the number of
   * control scopes in a program:  */
  int scopenum;           /* every control scope has a unique number */
  int subscope;           /* 'gencode' bumps this along for labels */
  scopestack *ptr;        /* stack pointer, top-to-bottom */
};

/*
 *  The next 5 struct declarations show how SETL virtual machine code
 *  is arranged internally.  This structure is traversed to emit the
 *  weird assembler-like form that comes out of the SETL translator.
 */

struct csect_struct {   /* virtual code for a routine */
  block *blocks;          /* head of list of basic blocks */
  csect *next;            /* next control section */
};

struct block_struct {   /* basic block (ends with a branch) */
  label *labels;          /* head of list of labels */
  instr *instrs;          /* head of list of intructions */
  block *next;            /* next basic block */
};

struct label_struct {   /* code label list member */
  const char *lab;        /* text of the label */
  long tn;                /* its token number */
  label *next;            /* next label */
};

struct instr_struct {   /* instruction list member */
  optype opcode;          /* command to the SETL virtual machine */
  long tn;                /* which token gets blamed for this instr */
  opera *operas;          /* head of list of operands */
  instr *next;            /* next instruction */
};

struct opera_struct {   /* instruction operand list member */
  operatype type;         /* never seria */
  bits acc;               /* ability or type of access wanted */
  const char *libretto;   /* text of this operand */
  opera *next;            /* next operand */
};


/*
 *  United types
 */

union datum_union {     /* what can be associated with a key */
  int code;               /* token code */
  long tn;                /* token number */
  int valence;            /* -arity */
  char *string;           /* pointer to arbitrary character data */
  symbol *symptr;         /* pointer to a symbol */
  table subtab;           /* sub-table */
  block *blkptr;          /* address of code block */
  int count;              /* reference count */
};



/*
 *  Interface to "table" manager (Patricia)
 */

/*
 *  And a rather silly interface it is too, but we are reaching
 *  far back into history here.
 *
 *  Anyway, the one totally confusing thing you should know about
 *  is how these definitions of DATA, FREE, and MALLOC find their
 *  way into patrie.c and patricia.c when the latter files don't
 *  #include our setltran.h (this file).  Well, what happens is that
 *  those .c files don't get compiled on their own, they are only
 *  #included by patty.c, which contains just these 3 lines:
 *
 *   #include "setltran.h"
 *   #include "patrie.c"
 *   #include "patricia.c"
 *
 *  Better would be to require you to provide "patuser.h" with these
 *  definitions, have patrie.h and patricia.h #include patuser.h,
 *  and do away with patty.c entirely.
 *
 *  Actually, the convention about DATA is silly, too.  Since the
 *  routines which mention them in their signature always return a
 *  pointer to data (the formal is "DATA **" in every case), DATA
 *  itself might as well just be void like it should be.  It's the
 *  most natural thing in the world in a C environment to expect
 *  callers to cast such returned pointers to the type they're
 *  expecting (and from there a type-test might get you to the
 *  specific derivative).  Since PATRICIA has no interest in your
 *  data, you don't need to put the whole truth in patuser.h:
 *  patrie.c and patricia.c can take the view that DATA is void,
 *  and if you intend always to cast the results (as I intend to),
 *  you can use the same declarations.  I was going to say that
 *  you could declare them differently if you had some more
 *  specific pointer type to use for all your data, but in fact
 *  that would cause a link-time mismatch on modern linkers
 *  (especially if you compile the code as C++).  So you should
 *  at least be consistent in the declarations.  I suppose I must
 *  have thought the 'datum' union was cleaner than casting when I
 *  wrote this, and felt I had bought the whole "setltran.h" ticket
 *  anyway.
 */

#define DATA datum
#define FREE mem_free
#define MALLOC mem_alloc

#include "patrie.h"

/*
 *  Note - The type 'table' is used in this translator in preference
 *  to the preprocessor symbol PATRIE introduced in "patrie.h".  They
 *  mean the same thing.  Likewise datum is used instead of DATA, but
 *  KEY and ORD are proper typedef-defined types so we use them
 *  directly.
 */



/*
 *  Environmental hazards
 */

extern const char *tranprog;  /* external name of this translator program */
extern const char *source;    /* external location of SETL source text */
extern const char *sink;      /* external destination of "object" code */

extern bool verbose;          /* verbose flag */
extern bool strict;           /* strict language flag */
extern bool debug;            /* translator debugging flag */
extern bool spew_font_hints;  /* spew font hints after preparsing, quit */

extern namecase keycase;      /* how keywords are recognized */
extern namecase tagcase;      /* how user tags are recognized */

extern int maxerrors;         /* max #errors before quitting */
extern int nerrors;           /* this many detected so far */

extern bool lexing_done;      /* translator state flag */
extern bool preparse_done;    /* translator state flag */
extern bool parsing_done;     /* translator state flag */
extern bool analyzing_done;   /* translator state flag */
extern bool generating_done;  /* translator state flag */
extern bool optimizing_done;  /* translator state flag */
extern bool spewing_done;     /* translator state flag */

/* Input stuff */
extern char *src;             /* the entire program source text */
extern long srclen;           /* its length in bytes */

/* Lexing and parsing stuff */
extern token *tokens;         /* array of input tokens */
extern long ntokens;          /* number of tokens in the array */
extern node *root;            /* the distinguished parse tree node */
extern bool pre_parsing;      /* the lexer needs to know this */
extern node *yylval;          /* current token seen by yyparse */
extern int yydebug;           /* controls yacc-style parse tracing */

/* Symbol stuff */
extern symtab sysrottab;      /* "system routine" symbol table */
extern symtab publictab;      /* symtabs from package specs */
extern symtab privatetab;     /* PROGRAM/PACKAGE private symtabs */
extern symtab paxinuse;       /* PACKAGEs in USE by the current unit */
extern symtab usetabs;        /* map of 'paxinuse' sets, by unit */
extern symtab unittabs;       /* map of tables of imported symbols */

/* Scope stuff */
extern int scopectr;          /* scope number generator */
extern scopestack *curscope;  /* the route of all evil */

/* "Object" code stuff */
extern csect *virtcode;       /* list of routines made by 'gencode' */



/*
 *  Utility macros
 */

#ifndef USE_SYSTEM_ASSERT
#ifdef assert
#undef assert
#endif
#ifdef NDEBUG
#define assert(expr) \
  ((void) 0)
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
 *  This 'unexpected' macro is for use in 'switch' statements where
 *  the case analysis is unexpectedly incomplete.  Actually, it can
 *  be used anywhere you are dealing with an int or enum, and want
 *  the value included in the error message:
 */
#define unexpected(expr) \
  internal_case_failure(#expr, expr, FILEBASE, __LINE__)

/*
 *  Now back to the more application-specific macros...
 */

#define nodesize(k)  (offsetof(node,sub) + \
                (k) * elmtsize(node,sub))
#define symsize(k)   (offsetof(symbol,parms) + \
                (k) * elmtsize(symbol,parms))

#define getmem(p,n,type)  p = (type *) mem_alloc((n)*sizeof(type))
#define resize(p,n,type)  p = (type *) mem_realloc(p,(n)*sizeof(type))
#define allocnode(p,k)    p = (node *) mem_alloc(nodesize(k))
#define allocsym(p,k)     p = (symbol *) mem_alloc(symsize(k))
#define release(p)        mem_free(p)

#define sub0(p)   ((p)->sub[0])
#define sub1(p)   ((p)->sub[1])
#define sub2(p)   ((p)->sub[2])
#define sub3(p)   ((p)->sub[3])
#define sub4(p)   ((p)->sub[4])
#define sub5(p)   ((p)->sub[5])
#define sub6(p)   ((p)->sub[6])
#define subi(p,i) ((p)->sub[i])


/*
 *  Translator routine prototypes
 */

/* "analyze.c" */
extern void analyze(void);

/* "canonize.c" */
extern void canonize(node *p);

/* "debug.c" */
extern void dumptree(FILE *stream, node *nod, int lev, int isub);

/* "flatten.c" */
extern void flatten(node *p);

/* "gencode.c" */
extern void gencode(void);

/* "identify.c" */
extern void identify(node *p);

/* "mem.c" */
extern void mem_init(void);
extern bool mem_ephemeral(bool flag);
extern void *mem_alloc(size_t nbytes);
extern void *mem_realloc(void *p, size_t nbytes);
extern void mem_free(void *p);
extern void mem_reset(void);

/* "optimize.c" */
extern void optimize(void);

/* "parse.c" */
extern void parse(void);
extern void yyerror(const char *s);
extern int yylex(void);
extern void ok_autosemi(void);
extern void no_autosemi(void);
extern void init_ctrlscope(int x);
extern void push_ctrlscope(node *nod);
extern void pop_ctrlscope(node *nod);
extern void check_ctrlscope(int i);
extern void init_loopscope(int x);
extern void push_loopscope(node *nod);
extern void pop_loopscope(void);
extern node *match_loop(node *nod);
extern void check_cmd_arg(node *nod);
extern node *nullnode(void);
extern node *node0(nodetype type, node *t);
extern node *node1(nodetype type, node *t, node *a);
extern node *node2(nodetype type, node *t, node *a, node *b);
extern node *node3(nodetype type, node *t, node *a, node *b, node *c);
extern node *node4(nodetype type, node *t, node *a, node *b, node *c, node *d);
extern node *node5(nodetype type, node *t, node *a, node *b, node *c, node *d, node *e);
extern node *node6(nodetype type, node *t, node *a, node *b, node *c, node *d, node *e, node *f);
extern node *noden(nodetype type, node *t, int n);

/* "scopes.c" */
extern void initscope(scopetype type);
extern void pushscope(scopetype type);
extern void popscope(scopetype type);
extern void endscope(scopetype type);

/* "spewobj.c" */
extern void spewobj(void);

/* "symtab.c" */
extern symtab new_symtab(void);
extern symtab symtabcopy(symtab s);
extern symbol *new_symbol(const char *tag, symtype type, long tn, symbol *owner, int nparms);
extern symbol *new_symtabholder(symtab s);
extern symbol *symcopy(symtab s, symbol *y);
extern symbol *syminsert(symtab s, const char *tag, symtype type, long tn, symbol *owner, int nparms, symtype prev);
extern symbol *symdef(symbol *holder, token *t, symtype type, int nparms);
extern symbol *symfind(symtab s, const char *tag);
extern symbol *symi(symtab s,int i);
extern symbol *symstackfind(symstack *p, const char *tag);
extern symbol *symresolve(symstack *p, node *q, int *k);
extern void symstackpush(symstack **p, symbol *holder);
extern void symstackpop(symstack **p);
extern void symtabdump(FILE *stream, symtab s, int lev);

/* "sysrot.c" */
extern void sysrotinit(void);

/* "tokenize.c" */
extern void charinit(void);
extern void tokenize(void);
extern void preparse(void);

/* "toksym.c" */
extern void toksyminit(void);
extern int get_tcode(const char *s);
extern void toksymname(token *t);
extern void toksymid(token *t);
extern token *name_token(node *a);
extern token *cmdname_token(node *a);
extern token *bopname_token(node *a);
extern token *uopname_token(node *a);
extern token *refname_token(node *a);
extern token *syscon_token(node *a);
extern token *sysval_token(node *a);
extern token *sysvar_token(node *a);
extern token *sysproc_token(node *a);
extern token *machine_token(node *a);
extern token *c_code_token(node *a);

/* "tran.c" */
extern void tran(unsigned argc, char *const argv[]) NO_RETURN;

/* "util.c" */
extern void utilinit(void);
extern bool cpp_emission(long *pi, long *pn, long *pf, long *pg);
extern void tranerr(long k, const char *message) NO_RETURN;
extern void tokerr(long tn, const char *message) NO_RETURN;
extern void quit(void) NO_RETURN;
extern void bugerr(const char *msg, ...) NOISY_QUITTER;
extern void internal_assert_failure(const char *assertion,
                       const char *filename, int line) NO_RETURN;
extern void internal_check_failure(const char *expr,
                       const char *filename, int line) NO_RETURN;
extern void internal_case_failure(const char *expr, int value,
                       const char *filename, int line) NO_RETURN;
extern char *strmake(const char *s);
extern char *strjoin(const char *s, const char *t);
extern char *strjoin3(const char *s, const char *t, const char *u);
extern char *strjoin4(const char *s, const char *t, const char *u, const char *v);
extern char *strjoin5(const char *s, const char *t, const char *u, const char *v, const char *w);
extern char *strnmake(const char *s, size_t n);
extern char *strmakelower(const char *s);
extern char *strmakeupper(const char *s);
extern bool strislower(const char *s);
extern bool strisupper(const char *s);
extern bool is_special_former(node *p, symstack *s);
extern bool is_special_lhs(node *p, symstack *s);
extern char *ntoa(int n);
extern int millisec(void);

/* "y.c" */
extern int yyparse(void);

#endif /* _setltran_h */
