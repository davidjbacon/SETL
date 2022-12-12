/*  ===  Rigorous semantic analysis and symtab completion  =========  */

/*  $Id: canonize.c,v 1.28 2021/03/02 21:00:13 setlorg Exp $  */

/*  Free software (c) dB - see file COPYING for license (GPL).  */

/*
 *  This semantic analysis subpass enters used but undeclared variables
 *  into the appropriate symbol table.  Apart from that, it does no
 *  real work, just a lot of semantic checking.  No errors it misses
 *  should be detectable until run-time.
 * 
 *  It has to track lexical scopes exactly the same way 'identify'
 *  does in order to check GOTO's properly.
 * 
 *  It could have made some syntax tree changes such as tuple-izing
 *  final actual args for starred formals, changing f(x,y)
 *  map references to f([x,y]), and unifying the categories
 *  expr, lhs, lhs_or_dash, and rw, but I have chosen to leave the
 *  tree alone here so that its structure does not change in any
 *  pass after 'flatten' (and even 'flatten' rationalizes it in a
 *  very consistent way from what the raw grammar produces).
 *  This saves us work here and only adds a tiny amount of fuss
 *  to the code emitter's job.
 * 
 *  This subpass operates by processing all parts of the compilation
 *  units, starting with the package specs, which cannot refer to
 *  symbols in other packages.  USE clauses are next, and establish
 *  what symbols are accessible from what packages.  Finally, all
 *  remaining symbol occurrences are analyzed, and valid occurrences
 *  of undeclared symbols are entered into local private symbol
 *  tables.
 * 
 *  The SETL2 rules are that (1) a local declaration always hides
 *  (but does not render inaccessible) a more global one, (2) a local
 *  or global declaration hides a declaration implied by USE, and
 *  (3) multiple declarations imported by USE all hide each other.
 *  In case 3, a valid naked occurrence of the mutually hidden symbol
 *  constitutes an implicit declaration as if the symbol had never
 *  been declared (weird).  Implicit declarations are only accessible
 *  in the scopes they occur in.
 *
 *  That rule for implicit declaration of a mutually hidden symbol
 *  that is nonetheless referenced has the weird consequence that an
 *  addition to the symbols brought in by USE (by a new package or an
 *  extension of an old one) can change an existing package symbol
 *  reference to an implicit local one.  The danger one normally
 *  thinks of when bringing in new sets of symbols is that some might
 *  override existing implicit locals, not create them.
 *
 *  TODO:  consider language change to fix that, so that an unscoped
 *  reference to a visible symbol in two or more packages is flagged
 *  as ambiguous, not taken as an implicit local.
 */

/* ------------------------------------------------------------------ */

#include "setltran.h"
#include "y.tab.h"

/* Local routines */
static void c_uses(node *p);
static void c_use(node *p);
static void c_routines(node *p);
static void c_routine(node *p);
static void c_form_specs(node *p);
static void c_form_spec(node *p);
static void c_body(node *p);
static void c_decls(node *p);
static void c_decl(node *p);
static void c_vars(node *p, node *b);
static void c_consts(node *p);
static void c_inits(node *p);
static void c_sels(node *p);
static void c_decl_expr(node *p);
static void c_refinements(node *p);
static void c_refinement(node *p);
static void c_stmts(node *p);
static void c_stmt(node *p);
static void c_elifs(node *p);
static void c_elif(node *p);
static void c_when_stmts(node *p);
static void c_when_stmt(node *p);
static void c_loop_head(node *p);
static void c_loop_init(node *p);
static void c_loop_doing(node *p);
static void c_loop_while(node *p);
static void c_loop_step(node *p);
static void c_loop_until(node *p);
static void c_loop_term(node *p);
static void c_iterator(node *p);
static void c_simpliter_list(node *p);
static void c_simpliter(node *p);
static void c_expr_list(node *p, bits need);
static void c_lhs_list(node *p, bits need);
static void c_rw_list(node *p, bits need);
static void c_lhs_or_dash_list(node *p, bits need);
static void c_expr(node *p, bits need);
static void c_command(node *p);
static void c_choices(node *p);
static void c_choice(node *p);
static void c_elif_exprs(node *p);
static void c_elif_expr(node *p);
static void c_selector(node *p);
static void c_selnames(node *p, int k);
static void c_former(node *p);
static void c_check_acc(int tn, bits need, bits ability);
static void c_call(int tn, symbol *y, node *p, bool vactual);
static void c_check_caller_variadicity(int tn, bool vactual);


/* ------------------------------------------------------------------ */

/* Local data */

#include "symstack.h"

/* ------------------------------------------------------------------ */

void canonize(p)
node *p;
{
  int i;
  token *t;
  node *q,*r;
  symbol *v,*w,*x,*y,*z;
  symbol *sysrottab_holder;
  glowball = (symbol *)NULL;
  inner = (symstack *)NULL;  /* initially empty stack of symtabs */
  sysrottab_holder = new_symtabholder(sysrottab);
  pushsymtab(sysrottab_holder);  /* push sysrot table onto symstack */
  if (p) {
    switch (p->type) {
    case N_unnamed_program:
      y = symfind(privatetab,"_unnamed_SETL_program");
      assert (y->type == progsym);
      x = symfind(y->stab,"_MAIN");
      assert (x->type == procsym);
      pushsymtab(y);  /* program unit-global symstack level */
      q = sub0(p); assert (q->type == N_body);
      initscope(proc_scope);  /* decls and code coming */
      glowball = x;  /* for tags in rhs of global decls */
      c_decls(sub0(q));  /* program globals */
      glowball = (symbol *)NULL;
      pushsymtab(x);  /* mainproc on behalf of program unit */
      pushscope(stmts_scope);  /* stmts coming */
      c_stmts(sub1(q));
      popscope(stmts_scope);  /* back to decls and code */
      c_refinements(sub2(q));
      endscope(proc_scope);  /* back to no control scope */
      assert (curholder == x);  /* still at mainproc level */
      popsymtab();  /* from mainproc back to program unit level */
      c_routines(sub1(p));  /* PROC/OP/COMMAND defns */
      assert (curholder == y);  /* at program unit level */
      popsymtab();  /* from program unit back to outer level */
      x = (symbol *)NULL;
      y = (symbol *)NULL;
      break;
    case N_complex_program:
      /*
       *  The levels of symbol table stack (symstack) used here are
       *
       *   (a) 'sysrottab' (pushed above);
       *
       *   (b) symtab containing the current package/program ("unit")
       *       name and any names imported by USE, including the
       *       package names themselves;
       *
       *   (c) the globals owned by each unit; and
       *
       *   (d) procedure-local symbols.
       *
       *  Also, when 'glowball' is non-NULL, c_decl_expr() pushes
       *  'glowball' onto the symstack around its call to c_expr().
       *  This is used during processing of global decls, so
       *  'glowball' goes at level (d) at that time.
       *
       *  The design is rather complex and fragile, and could use more
       *  documentation if not a complete redesign and rewrite, the
       *  latter being particularly desirable if the silly global
       *  visibility rules are ever reformed.
       */
      p = sub0(p);  /* tsk, tsk */
      assert (p->type == N_compilation_units);
      /*
       *  On this first loop over compilation units, process just the
       *  package specs, for their global decls.
       */
      for (i=0; i<p->nsub; i++) {
        q = subi(p,i);
        if (q->type == N_package_spec) {
          t = name_token(sub0(q));  /* current package name */
          /*
           *  In case initializations in the package specs refer to
           *  the package name itself, we push a symtab (symbol z
           *  here) onto the stack with an entry containing just the
           *  package name.  Note that there are no USE stmts in 
           *  package specs.
           */
          z = new_symtabholder(new_symtab());
          symdef (z, t, packsym, 0);  /* put pkg name from t into z */
          y = symfind(publictab,t->graphic);
          assert (y->type == packsym);
          /*
           *  This is the name of the routine generated for public
           *  variable initializations.  For convenience, we let this
           *  routine carry the incidental variables and temporaries
           *  of the rhs parts of the global decls processed by c_decls
           *  below.  That routine's symbol (x here) has the routine's
           *  symtab, and is fed implicitly to c_decls via 'glowball'.
           */
          x = symfind(y->stab,strjoin("_PUBINIT_",t->graphic));
          assert (x->type == procsym);
          pushsymtab(z);  /* the level with just the package name (b) */
          pushsymtab(y);  /* package-global level (c) */
          initscope(proc_scope);  /* decls and code coming */
          glowball = x;  /* for tags in rhs of global decls */
          c_decls(sub2(q));  /* public globals */
          glowball = (symbol *)NULL;
          endscope(proc_scope);  /* back to no control scope */
          assert (curholder == y);  /* at package level (c) */
          popsymtab();  /* from package back to package name level (b) */
          assert (curholder == z);  /* at package name level (b) */
          popsymtab();  /* back to outer level (a, sysrots) */
          x = (symbol *)NULL;
          y = (symbol *)NULL;
          z = (symbol *)NULL;
        }
      }
      /*
       *  In this second loop over compilation units, process the
       *  package bodies and the program unit for their USE stmts,
       *  their VAR/CONST/INIT decls, which are private globals of the
       *  unit, and their PROC/OP/COMMAND defns.
       */
      usetabs = new_symtab();  /* the set of names USEd by each unit */
      unittabs = new_symtab();  /* table of tables of imported symbols */
      for (i=0; i<p->nsub; i++) {
        q = subi(p,i);
        switch (q->type) {
        case N_package_spec:
          break;  /* did specs in first loop above */
        case N_package_body:
          t = name_token(sub0(q));  /* current package name */
          /*
           *  Init z like in the package specs loop, but then augment
           *  it with the symbols made visible by USE stmts.
           */
          z = new_symtabholder(new_symtab());
          symdef (z, t, packsym, 0);  /* put pkg name from t into z */
          /*
           *  The rule is that no package/program names may conflict.
           *  Also, a package or program name hides names imported
           *  by USE (just like declared globals do), and imported
           *  names hide each other in case of conflict.
           */
          pushsymtab(z);  /* make c_uses complete z->stab (level b) */
          c_uses(sub2(q));  /* c_uses also fills in paxinuse */
          v = syminsert (usetabs, t->graphic, packsym, t->tn, NULL, 0, nosym);
          v->stab = symtabcopy(paxinuse);  /* add paxinuse to usetabs */
          v = (symbol *)NULL;
          w = syminsert (unittabs, t->graphic, packsym, t->tn, NULL, 0, nosym);
          w->stab = symtabcopy(z->stab);  /* t and USE-imported symbols */
          w = (symbol *)NULL;
          y = symfind(privatetab,t->graphic);
          assert (y->type == packsym);
          x = symfind(y->stab,strjoin("_PRIVINIT_",t->graphic));
          assert (x->type == procsym);
          pushsymtab(y);  /* package-global level (c) */
          initscope(proc_scope);  /* decls and code coming */
          glowball = x;  /* for tags in rhs of global decls */
          c_decls(sub3(q));  /* globals private to the package */
          glowball = (symbol *)NULL;
          endscope(proc_scope);  /* back to no control scope */
          c_routines(sub4(q));  /* PROC/OP/COMMAND defns */
          assert (curholder == y);  /* at package level (c) */
          popsymtab();  /* back to USE-populated symtab level (b) */
          assert (curholder == z);  /* at USE-populated symtab level (b) */
          popsymtab();  /* back to outer level (a, sysrots) */
          x = (symbol *)NULL;
          y = (symbol *)NULL;
          z = (symbol *)NULL;
          break;
        case N_program_unit:
          /* The processing here thru the decls (c_decls call) resembles
           * that for N_package_body */
          t = name_token(sub0(q));  /* program unit name */
          z = new_symtabholder(new_symtab());
          symdef (z, t, progsym, 0);  /* put pgm name from t into z */
          pushsymtab(z);  /* make c_uses complete z->stab (level b) */
          c_uses(sub1(q));  /* c_uses also fills in paxinuse */
          v = syminsert (usetabs, t->graphic, progsym, t->tn, NULL, 0, nosym);
          v->stab = symtabcopy(paxinuse);  /* add paxinuse to usetabs */
          v = (symbol *)NULL;
          w = syminsert (unittabs, t->graphic, progsym, t->tn, NULL, 0, nosym);
          w->stab = symtabcopy(z->stab);  /* t and USE-imported symbols */
          w = (symbol *)NULL;
          y = symfind(privatetab,t->graphic);
          assert (y->type == progsym);
          x = symfind(y->stab,"_MAIN");
          assert (x->type == procsym);
          pushsymtab(y);  /* program unit-global level (c) */
          r = sub2(q); assert (r->type == N_body);
          initscope(proc_scope);  /* decls and code coming */
          glowball = x;  /* for tags in rhs of global decls */
          c_decls(sub0(r));  /* globals private to the program unit */
          glowball = (symbol *)NULL;
          pushsymtab(x);  /* mainproc on behalf of pgm unit (level d) */
          pushscope(stmts_scope);  /* stmts coming */
          c_stmts(sub1(r));
          popscope(stmts_scope);  /* back to decls and code */
          c_refinements(sub2(r));
          endscope(proc_scope);  /* back to no control scope */
          assert (curholder == x);  /* still at mainproc level (d) */
          popsymtab();  /* back to program unit level (c) */
          c_routines(sub3(q));  /* PROC/OP/COMMAND defns */
          assert (curholder == y);  /* at program unit level (c) */
          popsymtab();  /* back to USE-populated symtab level (b) */
          assert (curholder == z);  /* at USE-populated symtab level (b) */
          popsymtab();  /* back to outer level (a, sysrots) */
          x = (symbol *)NULL;
          y = (symbol *)NULL;
          z = (symbol *)NULL;
          break;
        default:  unexpected(q->type);
        }
      }
      break;
    default:  unexpected(p->type);
    }
  } else {
    /* null program */
    y = symfind(privatetab,"_empty_SETL_program");
    assert (y->type == progsym);
    x = symfind(y->stab,"_MAIN");
    assert (x->type == procsym);
    x = (symbol *)NULL;
    y = (symbol *)NULL;
  }
  assert (curholder == sysrottab_holder);  /* at outer level (sysrots) */
  assert (cursymtab == sysrottab);
  popsymtab();
  assert (inner == (symstack *)NULL);
} /* end canonize */

/* .................................................................. */

/*
 *  Make 'paxinuse' a symtab representing the set of package names
 *  that appear in all the current compilation unit's USE statements.
 *
 *  Finish 'cursymtab', which ends up with the program name or
 *  current package name, plus the names of all packages imported
 *  by USE, plus all public symbols in those imported packages that
 *  do not duplicate each other nor the current program/package name
 *  nor the imported package names:
 */
static void c_uses(p)
node *p;
{
  int i,j,m,n;
  symbol *y,*z,*w;
  table occ;  /* local record of occurrences of each imported symbol */
  datum *d;
  paxinuse = new_symtab();  /* set of pkg names */
  switch (p->type) {
  case N_uses:
    /* Add a symbol for each valid USE arg to 'paxinuse' */
    for (i=0; i<p->nsub; i++) c_use(subi(p,i));
    break;
  default:  unexpected(p->type);
  }
  /* Add the names of imported packages to cursymtab */
  n = size(paxinuse);
  for (i=1; i<=n; i++) {  /* paxinuse acts as an enumerable set */
    y = symi(paxinuse,i);  /* the i'th symbol */
    z = syminsert (cursymtab, y->tag, packsym, y->tn, NULL, 0, nosym);
  }
  /*
   *  For each imported symbol that does not conflict with a
   *  program or package name already recorded in cursymtab,
   *  store in the local table a pointer to the symbol unless
   *  there is already a table entry for that symbol, in which
   *  case null the pointer there to signify the mutual hiding:
   */
  patcre(&occ);
  n = size(paxinuse);
  for (i=1; i<=n; i++) {
    y = symi(paxinuse,i);
    z = symfind(publictab,y->tag);
    assert (z->type == packsym);
    m = size(z->stab);
    for (j=1; j<=m; j++) {
      w = symi(z->stab,j);
      if (symfind(cursymtab,w->tag) == (symbol *)NULL) {
        if (!keysub(occ,w->tag,&d)) {
          d->symptr = w;  /* symbol ptr (alias) */
        } else {
          d->symptr = (symbol *)NULL;
        }
      }
    }
  }
  /* Add to cursymtab all imported symbols that were counted once */
  n = size(occ);
  for (i=1; i<=n; i++) {
    check (ordsub(occ,i,&d));
    if (d->symptr != (symbol *)NULL) {
      symcopy(cursymtab,d->symptr);
    }
  }
}

/* .................................................................. */

static void c_use(p)
node *p;
{
  int i;
  node *q;
  token *t;
  symbol *y,*z;
  switch (p->type) {
  case N_use:
    q = sub0(p);
    assert (q->type == N_name_list);
    for (i=0; i<q->nsub; i++) {
      t = name_token(subi(q,i));
      y = symfind(publictab,t->graphic);
      if (y == (symbol *)NULL) {
        tokerr(t->tn,"Package not found");
      }
      if (y->type != packsym) {
        tokerr(t->tn,"Not a package name");
      }
      /*
       *  Multiple USEs of a package are okay (why not?).
       *  If you don't like that, change the final 'packsym' to 'nosym':
       */
      z = syminsert (paxinuse, t->graphic, packsym, t->tn, NULL, 0, packsym);
      (void)z;  /* avoid GCC's "set but not used" warning */
    }
    break;
  default:  unexpected(p->type);
  }
}

/* .................................................................. */

static void c_routines(p)
node *p;
{
  int i;
  switch (p->type) {
  case N_routines:
    for (i=0; i<p->nsub; i++) c_routine(subi(p,i));
    break;
  default:  unexpected(p->type);
  }
}

/* .................................................................. */

static void c_routine(p)
node *p;
{
  token *t;
  symbol *y;
  initscope(proc_scope);
  switch (p->type) {
  case N_procdef:
    t = name_token(sub0(p));
    y = symfind(cursymtab,t->graphic);
    assert (y->type == procsym);
    pushsymtab(y);
    c_form_specs(sub1(p));
    c_body(sub3(p));
    popsymtab();
    break;
  case N_cmddef:
    t = cmdname_token(sub0(p));
    y = symfind(cursymtab,t->graphic);
    assert (y->type == cmdsym);
    pushsymtab(y);
    c_form_specs(sub1(p));
    c_body(sub3(p));
    popsymtab();
    break;
  case N_binopdef:
    t = bopname_token(sub0(p));
    y = symfind(cursymtab,t->graphic);
    assert (y->type == bopsym);
    pushsymtab(y);
    c_form_spec(sub1(p));
    c_form_spec(sub2(p));
    c_body(sub3(p));
    popsymtab();
    break;
  case N_unopdef:
    t = uopname_token(sub0(p));
    y = symfind(cursymtab,t->graphic);
    assert (y->type == uopsym);
    pushsymtab(y);
    c_form_spec(sub1(p));
    c_body(sub2(p));
    popsymtab();
    break;
  default:  unexpected(p->type);
  }
  endscope(proc_scope);
} /* end c_routine */

/* .................................................................. */

static void c_form_specs(p)
node *p;
{
  int i;
  switch (p->type) {
  case N_form_specs:
    for (i=0; i<p->nsub; i++) c_form_spec(subi(p,i));
    break;
  default:  unexpected(p->type);
  }
}

/* .................................................................. */

static void c_form_spec(p)
node *p;
{
  token *t;
  symbol *y;
  switch (p->type) {
  case N_rd_spec:
  case N_wr_spec:
  case N_rw_spec:
    t = name_token(sub0(p));
    y = symfind(cursymtab,t->graphic);
    assert (y->type == parsym);
    break;
  default:  unexpected(p->type);
  }
}

/* .................................................................. */

static void c_body(p)
node *p;
{
  switch (p->type) {
  case N_body:
    c_decls(sub0(p));
    pushscope(stmts_scope);
    c_stmts(sub1(p));
    popscope(stmts_scope);
    c_refinements(sub2(p));
    break;
  default:  unexpected(p->type);
  }
}

/* .................................................................. */

static void c_decls(p)
node *p;
{
  int i;
  pushscope(decls_scope);
  switch (p->type) {
  case N_decls:
    for (i=0; i<p->nsub; i++) c_decl(subi(p,i));
    break;
  default:  unexpected(p->type);
  }
  popscope(decls_scope);
}

/* .................................................................. */

static void c_decl(p)
node *p;
{
  switch (p->type) {
  case N_var_decl:
    c_vars(sub0(p),sub1(p));
    break;
  case N_const_decl:
    c_consts(sub0(p));
    break;
  case N_init_decl:
    c_inits(sub0(p));
    break;
  case N_sel_decl:
    c_sels(sub0(p));
    break;
  case N_repr_decl:
    break;
  default:  unexpected(p->type);
  }
}

/* .................................................................. */

static void c_vars(p,b)
node *p;
node *b;  /* possible ":BACK" qualifier */
{
  int i;
  node *q;
  token *t;
  symbol *y;
  switch (p->type) {
  case N_var_list:
    for (i=0; i<p->nsub; i++) {
      q = subi(p,i);
      assert (q->type == N_var);
      t = name_token(sub0(q));
      y = symfind(cursymtab,t->graphic);
      assert (y->type == varsym || y->type == initsym);
      if (sub1(q) != (node *)NULL) c_decl_expr(sub1(q));
    }
    break;
  default:  unexpected(p->type);
  }
}

/* .................................................................. */

static void c_consts(p)
node *p;
{
  int i;
  node *q;
  token *t;
  symbol *y;
  switch (p->type) {
  case N_ascription_list:
    for (i=0; i<p->nsub; i++) {
      q = subi(p,i);
      assert (q->type == N_ascription);
      t = name_token(sub0(q));
      y = symfind(cursymtab,t->graphic);
      assert (y->type == constsym);
      /* N.B. If this is null, the implicit constant is a string: */
      if (sub1(q) != (node *)NULL) c_decl_expr(sub1(q));
    }
    break;
  default:  unexpected(p->type);
  }
}

/* .................................................................. */

static void c_inits(p)
node *p;
{
  int i;
  node *q;
  token *t;
  symbol *y;
  switch (p->type) {
  case N_initial_list:
    for (i=0; i<p->nsub; i++) {
      q = subi(p,i);
      assert (q->type == N_initial);
      t = name_token(sub0(q));
      y = symfind(cursymtab,t->graphic);
      assert (y->type == varsym || y->type == initsym);
      c_decl_expr(sub1(q));
    }
    break;
  default:  unexpected(p->type);
  }
}

/* .................................................................. */

static void c_sels(p)
node *p;
{
  int i;
  node *q;  token *t;
  symbol *y;
  switch (p->type) {
  case N_sel_list:
    for (i=0; i<p->nsub; i++) {
      q = subi(p,i);
      assert (q->type == N_sel);
      t = name_token(sub0(q));
      y = symfind(cursymtab,t->graphic);
      assert (y->type == selsym);
      c_decl_expr(sub1(q));
    }
    break;
  default:  unexpected(p->type);
  }
}

/* .................................................................. */

static void c_decl_expr(p)
node *p;
{
  /*
   *  Tags that appear only on the right-hand sides of global decls
   *  go into the symtab in the symtab the symbol 'glowball' points at,
   *  if any.  This provides a rather back-door mechanism to let them
   *  be processed as locals even in the context of a global decl.
   */
  if (glowball != (symbol *)NULL) {
    pushsymtab(glowball);
    c_expr(p,rdable);
    popsymtab();
  } else {
    c_expr(p,rdable);
  }
}

/* .................................................................. */

static void c_refinements(p)
node *p;
{
  int i;
  switch (p->type) {
  case N_refinements:
    for (i=0; i<p->nsub; i++) c_refinement(subi(p,i));
    break;
  default:  unexpected(p->type);
  }
}

/* .................................................................. */

static void c_refinement(p)
node *p;
{
  token *t;
  symbol *y;
  switch (p->type) {
  case N_refinement:
    t = refname_token(sub0(p));
    y = symstackfind(inner,t->graphic);
    assert (y && y->type == refsym);
    if (y->refinement_calls < 1) {
      tokerr(p->tn,"refinement nowhere used");
    }
    if (y->refinement_calls > 1) {
      tokerr(p->tn,"refinement can only be \"called\" from one place");
    }
    pushscope(refinement_scope);
    c_stmts(sub1(p));
    popscope(refinement_scope);
    break;
  default:  unexpected(p->type);
  }
}

/* .................................................................. */

static void c_stmts(p)
node *p;
{
  int i;
  switch (p->type) {
  case N_stmts:
    for (i=0; i<p->nsub; i++) c_stmt(subi(p,i));
    break;
  default:  unexpected(p->type);
  }
}

/* .................................................................. */

static void c_stmt(p)
node *p;
{
  int k;
  token *t;
  node *q,*r,*s;
  symbol *y;
  scopestack *rover;
  switch (p->type) {
  case N_labelled_stmt:
    c_stmt(sub1(p));
    break;
  case N_assert_stmt:
    c_expr(sub0(p),rdable);
    break;
  case N_continue_stmt:
  case N_quit_stmt:
    t = (token *)sub0(p); assert (t->type == N_Token);
    break;
  case N_fail_stmt:
  case N_pass_stmt:
  case N_succeed_stmt:
    break;
  case N_goto_stmt:
    t = name_token(sub0(p));
    y = symstackfind(inner,t->graphic);
    if (y == (symbol *)NULL || y->type != labsym) {
      tokerr(t->tn,"no such label");
    }
    rover = curscope;
    while (rover != (scopestack *)NULL &&
           rover->scopenum != y->scopenum) rover = rover->ptr;
    if (rover == (scopestack *)NULL) {
      tokerr(t->tn,"cannot jump to this label from here");
    }
    break;
  case N_return_stmt:
  case N_stop_stmt:
    if (sub0(p) != (node *)NULL) c_expr(sub0(p),rdable);
    break;
  case N_yield_stmt:
    c_expr(sub0(p),rdable);
    break;
  case N_if_stmt:
    /* Currently this allows jumping among the parts of the IF.  */
    pushscope(if_scope);
    c_expr(sub0(p),rdable);  /* test expression */
    c_stmts(sub1(p));             /* then-part */
    c_elifs(sub2(p));             /* opt-elifs */
    c_stmts(sub3(p));             /* else-part */
    popscope(if_scope);
    break;
  case N_case_of_stmt:
    pushscope(case_of_scope);
    /*
     *  These should begin with a choice_stmt, etc.
     *  Actually, it's just "unreachable code" if the first
     *  statement here isn't a "choice statement," unless of
     *  course some other label makes it reachable.
     *  The "strict syntax" would consider it an error.
     *  I'm just going to let it go for now.
     */
    c_stmts(sub0(p));             /* choice-parts */
    c_stmts(sub1(p));             /* else-part */
    popscope(case_of_scope);
    break;
  case N_case_ex_stmt:
    pushscope(case_ex_scope);
    c_expr(sub0(p),rdable);  /* test expression */
    /* Similar laxity of checking here as for the N_case_of_stmt.  */
    c_stmts(sub1(p));             /* choice-parts */
    c_stmts(sub2(p));             /* else-part */
    popscope(case_ex_scope);
    break;
  case N_case_of_when_stmt:
    pushscope(case_of_scope);
    c_when_stmts(sub0(p));      /* when-parts */
    c_stmts(sub1(p));           /* otherwise-part */
    popscope(case_of_scope);
    break;
  case N_case_ex_when_stmt:
    pushscope(case_ex_scope);
    c_expr(sub0(p),rdable);     /* test expression */
    c_when_stmts(sub1(p));      /* when-parts */
    c_stmts(sub2(p));           /* otherwise-part */
    popscope(case_ex_scope);
    break;
  case N_choice_stmt:
    assert (curscope->type == case_of_scope ||
           curscope->type == case_ex_scope);
    c_expr_list(sub0(p),rdable);
    c_stmt(sub1(p));
    break;
  case N_loop_stmt:
    pushscope(loopframe_scope);
    if (sub0(p) != (node *)NULL) c_iterator(sub0(p));
    if (sub1(p) != (node *)NULL) c_loop_head(sub1(p));
    pushscope(loopbody_scope);
    c_stmts(sub2(p));
    popscope(loopbody_scope);
    popscope(loopframe_scope);
    break;
  case N_assignment_stmt:
    t = (token *)sub1(p); assert (t->type == N_Token);
    switch (t->code) {
    case Becomes:
      c_expr(sub0(p),wrable);
      c_expr(sub2(p),rdable);
      break;
    case Accum:
      c_expr(sub0(p),rwable);
      c_expr(sub2(p),rdable);
      break;
    default:  unexpected(t->code);
    }
    break;
  case N_call_stmt:  /* "lhs_or_call ;" */
    q = sub0(p);
    switch (q->type) {
    case N_lhs1:  /* simple call of form "nameseq ;" */
      r = sub0(q);
      assert (r->type == N_nameseq);
      y = symresolve(inner,r,&k);
      if (y == (symbol *)NULL) goto cantcall;
      switch (y->type) {
      case refsym:
        y->refinement_calls++;
        break;
      case procsym:
        c_call(r->tn,y,(node *)NULL,false);
        break;
      default:
        goto cantcall;
      }
      break;
    case N_lhs2:  /* "[ lhs_or_dash_list ] ;" */
      goto noncall;
    case N_lhs3:  /* "lhs_or_call ( ) ;" or "lhs_or_call selector ;" */
      r = sub0(q);  /* lhs_or_call */
      switch (r->type) {
      case N_lhs1:  /* "nameseq ( ) ;" or "nameseq selector ;" */
        r = sub0(r);  /* reuse r for nameseq; oh well */
        assert (r->type == N_nameseq);
        /*
         *  Note that the only kind of "selector" a nameseq is
         *  allowed to have on its right here is a selector1
         *  (parenthesized expr_list with possible trailing (*));
         *  selector4 is precluded by the shift-over-reduce
         *  interpretation of the grammar.
         *
         *  A "null" selector "()" is perfectly okay.
         */
        s = sub1(q);  /* selector or null */
        if (s && s->type != N_selector1) goto noncall;
        y = symresolve(inner,r,&k);
        if (!y || y->type != procsym) goto cantcall;
        if (s == (node *)NULL) {
          /* Fixed number (0) of args */
          c_call(r->tn,y,(node *)NULL,false);
        } else {
          /* sub0(s) is an expr_list giving args, and
           * sub1(s) is non-null iff (*) is present */
          c_call(r->tn,y,sub0(s),sub1(s)!=NULL);
        }
        break;
      case N_lhs2:
      case N_lhs3:
      case N_Token:
        goto noncall;
      default:  unexpected(r->type);
      }
      break;
    case N_Token:
      goto noncall;
    default:  unexpected(q->type);
    }
    break;
  cantcall:
    tokerr(p->tn,"cannot \"call\" this");
    break;
  noncall:
    tokerr(p->tn,"not a valid statement form");
    break;
  case N_syscall_stmt:  /* Sysproc ( expr_list [(*)] ) ; */
                        /* Sysproc ( ) ; */
                        /* Sysproc ; */
    /*
     *  Might want to give a warning about syscalls that only
     *  make sense in expressions.  Probably won't bother.
     */
    t = sysproc_token(sub0(p));
    y = symstackfind(inner,t->graphic);
    assert (y && y->type == sysrotsym);
    c_call(t->tn,y,sub1(p),sub2(p)!=NULL);
    break;
  case N_from_stmt:
    c_expr(sub0(p),wrable);
    c_expr(sub2(p),rwable);
    break;
  case N_indirect_call_stmt:  /* CALL ( expr [, expr_list [(*)] ] ) ; */
    c_expr(sub0(p),rdable);
    c_expr_list(sub1(p),rdable);
    c_check_caller_variadicity(p->tn,sub2(p)!=NULL);
    break;
  case N_cmd_stmt:
    c_command(sub0(p));
    break;
  case N_expr_stmt:
    c_expr(sub0(p),rdable);
    break;
  case N_machine_stmt:
    break;
  case N_embedded_c_code:
    break;
  case N_null_stmt:
    break;
  default:  unexpected(p->type);
  }
} /* end c_stmt */

/* .................................................................. */

static void c_elifs(p)
node *p;
{
  int i;
  switch (p->type) {
  case N_elifs:
    for (i=0; i<p->nsub; i++) c_elif(subi(p,i));
    break;
  default:  unexpected(p->type);
  }
}

/* .................................................................. */

static void c_elif(p)
node *p;
{
  switch (p->type) {
  case N_elif:
    c_expr(sub0(p),rdable);
    c_stmts(sub1(p));
    break;
  default:  unexpected(p->type);
  }
}

/* .................................................................. */

static void c_when_stmts(p)
node *p;
{
  int i;
  switch (p->type) {
  case N_when_stmts:
    for (i=0; i<p->nsub; i++) c_when_stmt(subi(p,i));
    break;
  default:  unexpected(p->type);
  }
}

/* .................................................................. */

static void c_when_stmt(p)
node *p;
{
  switch (p->type) {
  case N_when_stmt:
    c_expr_list(sub0(p),rdable);
    c_stmts(sub1(p));
    break;
  default:  unexpected(p->type);
  }
}

/* .................................................................. */

static void c_loop_head(p)
node *p;
{
  switch (p->type) {
  case N_loop_head:
    if (sub0(p) != (node *)NULL) c_loop_init(sub0(p));
    if (sub1(p) != (node *)NULL) c_loop_doing(sub1(p));
    if (sub2(p) != (node *)NULL) c_loop_while(sub2(p));
    if (sub3(p) != (node *)NULL) c_loop_step(sub3(p));
    if (sub4(p) != (node *)NULL) c_loop_until(sub4(p));
    if (sub5(p) != (node *)NULL) c_loop_term(sub5(p));
    break;
  default:  unexpected(p->type);
  }
}

/* .................................................................. */

static void c_loop_init(p)
node *p;
{
  switch (p->type) {
  case N_loop_init:
    pushscope(init_scope);
    c_stmts(sub0(p));
    popscope(init_scope);
    break;
  default:  unexpected(p->type);
  }
}

/* .................................................................. */

static void c_loop_doing(p)
node *p;
{
  switch (p->type) {
  case N_loop_doing:
    pushscope(doing_scope);
    c_stmts(sub0(p));
    popscope(doing_scope);
    break;
  default:  unexpected(p->type);
  }
}

/* .................................................................. */

static void c_loop_while(p)
node *p;
{
  switch (p->type) {
  case N_loop_while:
    c_expr(sub0(p),rdable);
    break;
  default:  unexpected(p->type);
  }
}

/* .................................................................. */

static void c_loop_step(p)
node *p;
{
  switch (p->type) {
  case N_loop_step:
    pushscope(step_scope);
    c_stmts(sub0(p));
    popscope(step_scope);
    break;
  default:  unexpected(p->type);
  }
}

/* .................................................................. */

static void c_loop_until(p)
node *p;
{
  switch (p->type) {
  case N_loop_until:
    c_expr(sub0(p),rdable);
    break;
  default:  unexpected(p->type);
  }
}

/* .................................................................. */

static void c_loop_term(p)
node *p;
{
  switch (p->type) {
  case N_loop_term:
    pushscope(term_scope);
    c_stmts(sub0(p));
    popscope(term_scope);
    break;
  default:  unexpected(p->type);
  }
}

/* .................................................................. */

static void c_iterator(p)
node *p;
{
  switch (p->type) {
  case N_iterator:
    c_simpliter_list(sub0(p));
    if (sub1(p) != (node *)NULL) c_expr(sub1(p),rdable);  /* ST */
    break;
  default:  unexpected(p->type);
  }
}

/* .................................................................. */

static void c_simpliter_list(p)
node *p;
{
  int i;
  switch (p->type) {
  case N_simpliter_list:
    for (i=0; i<p->nsub; i++) c_simpliter(subi(p,i));
    break;
  default:  unexpected(p->type);
  }
}

/* .................................................................. */

static void c_simpliter(p)
node *p;
{
  switch (p->type) {
  case N_simpliter1:  /* lhs IN expr */
    c_expr(sub0(p),wrable);
    c_expr(sub1(p),rdable);
    break;
  case N_simpliter2:  /* lhs = compound_or_predef ( lhs_list ) */
  case N_simpliter3:  /* lhs = compound_or_predef { lhs_list } */
    c_expr(sub0(p),wrable);
    c_expr(sub1(p),rdable);
    c_lhs_list(sub2(p),wrable);
    break;
  default:  unexpected(p->type);
  }
}

/* .................................................................. */

static void c_expr_list(p,need)
node *p;
bits need;  /* see c_expr */
{
  int i;
  switch (p->type) {
  case N_expr_list:
    for (i=0; i<p->nsub; i++) c_expr(subi(p,i),need);
    break;
  default:  unexpected(p->type);
  }
}

/* .................................................................. */

static void c_lhs_list(p,need)
node *p;
bits need;  /* see c_expr */
{
  int i;
  switch (p->type) {
  case N_lhs_list:
    for (i=0; i<p->nsub; i++) c_expr(subi(p,i),need);
    break;
  default:  unexpected(p->type);
  }
}

/* .................................................................. */

static void c_rw_list(p,need)
node *p;
bits need;  /* see c_expr */
{
  int i;
  switch (p->type) {
  case N_rw_list:
    for (i=0; i<p->nsub; i++) c_expr(subi(p,i),need);
    break;
  default:  unexpected(p->type);
  }
}

/* .................................................................. */

static void c_lhs_or_dash_list(p,need)
node *p;
bits need;  /* see c_expr */
{
  int i;
  switch (p->type) {
  case N_lhs_or_dash_list:
    for (i=0; i<p->nsub; i++) c_expr(subi(p,i),need);
    break;
  default:  unexpected(p->type);
  }
}

/* .................................................................. */

static void c_expr(p,need)
node *p;
bits need;  /* type(s) of access desired */
{
  int i,k;
  token *t;
  node *q,*r,*s;
  symbol *y;
  switch (p->type) {
  case N_Token:  /* dash, etc. */
    t = (token *)p;
    switch (t->code) {
    case '-':
      c_check_acc(t->tn,need,wrable);
      break;
    case Integer:
    case Real_:
    case String:
    case Syscon:
    case Sysval:
      c_check_acc(t->tn,need,rdable);
      break;
    case Sysvar:
      c_check_acc(t->tn,need,rdable|wrable);  /* taut. */
      break;
    case Machine:
      /* Ability is assumed to be whatever is needed. */
      break;
    default:  unexpected(t->code);
    }
    break;
  /*
   *  Note that we don't want to make any presumptions about what
   *  the need should be in cases like the following.  We wish, at this
   *  stage, to consider things like lhs and rw as rather abstract
   *  categories, little different except in name:
   */
  case N_compound:
  case N_lhs1:
  case N_rw1:
    q = sub0(p);
    assert (q->type == N_nameseq);
    y = symresolve(inner,q,&k);
    if (y == (symbol *)NULL) {
      assert (k == 0);
      t = name_token(sub0(q));
      y = symdef(curholder,t,varsym,0);
      y->ability = rwable;
      k++;
    }
    if (y->type == procsym) {
      if (k == q->nsub) {
        tokerr(q->tn,"calls in expressions need trailing parentheses");
      } else {
        tokerr(q->tn,"no such name, or attempt to select from routine");
      }
    }
    c_check_acc(q->tn,need,y->ability);
    c_selnames(q,k);
    break;
  case N_lhs2:    /* [ lhs_or_dash_list ] */
    pushscope(loopframe_scope);  /* in case it's needed */
    c_lhs_or_dash_list(sub0(p),need);
    popscope(loopframe_scope);
    break;
  case N_rw2:     /* [ rw_list ] */
    pushscope(loopframe_scope);  /* in case it's needed */
    c_rw_list(sub0(p),need);
    popscope(loopframe_scope);
    break;
  case N_tuple1:  /* [ expr_list ] */
    pushscope(loopframe_scope);  /* in case it's needed */
    c_expr_list(sub0(p),need);
    popscope(loopframe_scope);
    break;
  case N_set1:    /* { expr_list } */
    c_check_acc(p->tn,need,rdable);
    pushscope(loopframe_scope);  /* in case it's needed */
    c_expr_list(sub0(p),need);
    popscope(loopframe_scope);
    break;
  case N_lhs3:    /* lhs_or_call selector  */
                  /* lhs_or_call ( ) */
                  /* lhs selector */
  case N_rw3:     /* rw selector */
  case N_applic:  /* operand selector */
                  /* operand ( ) */
    r = sub0(p);
    s = sub1(p);  /* NULL if the only "selector" is the trailing () */
    switch (r->type) {
    case N_Token:
      switch (((token *)r)->code) {
      case Sysval:
        c_check_acc(r->tn,need,rdable);
        /*
         *  We allow trailing () on Sysval for SETL2 compatibility.
         *
         *  If there is anything between the parentheses, it is
         *  treated as a selection on the Sysval value.
         */
        if (s != (node *)NULL) goto noncall;
        break;
      case String:
      case Syscon:
        c_check_acc(r->tn,need,rdable);
        goto noncall;
      case Sysvar:
        /*
         *  This is arguably a little tight for Sysvar, though unless
         *  you think there should be things like global control maps
         *  rather than functions to manipulate global system state,
         *  the use case for admitting such selections seems limited.
         *
         *  But if it turns out to be wanted, I think it suffices to
         *  put this for the Sysvar case:
         *
         *    c_check_acc(t->tn,need,rdable|wrable);  / * taut. * /
         *    goto noncall;
         */
        tokerr(r->tn,"cannot call or subscript this");
      case Integer:
      case Real_:
      case Machine:
        tokerr(r->tn,"cannot call or subscript this");
      default:
        unexpected(((token *)r)->code);
      }
      break;
    case N_compound:
    case N_lhs1:
    case N_rw1:
      q = sub0(r);
      assert (q->type == N_nameseq);
      if (s && s->type != N_selector1) goto noncall;
      y = symresolve(inner,q,&k);
      if (!y || y->type != procsym) goto noncall;
      c_check_acc(q->tn,need,rdable);
      /* See similar case in c_stmt(); non-null sub1(s) means (*)  */
      if (s == (node *)NULL) c_call(q->tn,y,(node *)NULL,false);
      else                   c_call(q->tn,y,sub0(s),sub1(s)!=NULL);
      break;
    default:
      goto noncall;
    }
    break;
  noncall:
    if (s == (node *)NULL) {
      tokerr(p->tn,"trailing \"()\" on this non-function is illegal decoration");
    }
    /* For an expr that syntactically could be a call (has a selector
     * of selector1 form), since we know here that it isn't a call,
     * make sure there is no trailing (*) after the expr_list:  */
    if (s->type == N_selector1 && sub1(s) != NULL) {
      tokerr(s->tn,"variadic notation in selection is not allowed");
    }
    /*
     *  Strictly speaking, we should check to make sure r
     *  doesn't have the lhs2/rw2/tuple1 form [...] if the
     *  need includes writeability (see p. 93 Schwartz et al.),
     *  but I think it's more silly to make an exception of that
     *  case than just to allow the somewhat dubious feature
     *  (you can write "[a,b,c](2..) := [5,6,7]" if you like).
     *
     *  The thing being selected on, r, may require fetch operations
     *  even if the selection is nominally for the purpose of storing
     *  into r:
     */
    c_expr(r,need|rdable);
    c_selector(s);
    break;
  case N_syscall: /* Sysproc ( expr_list [(*)] ) */
                  /* Sysproc ( ) */
                  /* Sysproc */
    /*
     *  It would be appropriate here also to check for silly use of
     *  some syscalls within expressions (i.e., the ones that are
     *  meant to be used just as statements).  Hardly worth the bother.
     */
    c_check_acc(p->tn,need,rdable);
    t = sysproc_token(sub0(p));
    y = symstackfind(inner,t->graphic);
    assert (y && y->type == sysrotsym);
    c_call(t->tn,y,sub1(p),sub2(p)!=NULL);
    break;
  case N_tuple2:  /* [ former ] */
  case N_set2:    /* { former } */
    c_check_acc(p->tn,need,rdable);
    pushscope(loopframe_scope);  /* in case it's needed */
    c_former(sub0(p));
    popscope(loopframe_scope);
    break;
  case N_assignment:
    t = (token *)sub1(p); assert (t->type == N_Token);
    c_check_acc(t->tn,need,rdable);
    switch (t->code) {
    case Becomes:
      c_expr(sub0(p),wrable);
      c_expr(sub2(p),rdable);
      break;
    case Accum:
      c_expr(sub0(p),rwable);
      c_expr(sub2(p),rdable);
      break;
    default:  unexpected(t->code);
    }
    break;
  case N_binary:
  case N_bincomb:
    t = (token *)sub1(p); assert (t->type == N_Token);
    c_check_acc(t->tn,need,rdable);
    if (t->code == Bop_name) {
      y = symstackfind(inner,t->graphic);
      if (y == (symbol *)NULL) {
        tokerr(t->tn,"undeclared binary operator");
      } else if (y->type != bopsym) {
        tokerr(t->tn,"not a binary operator");
      }
    }
    if ((p->type == N_binary) &&
        (leq(t->graphic,"AND") ||
         leq(t->graphic,"OR")  ||
         leq(t->graphic,"?"))) {
      pushscope(if_expr_scope);
      c_expr(sub0(p),rdable);
      c_expr(sub2(p),rdable);
      popscope(if_expr_scope);
    } else {
      /* This reflects the assumption that operands are RD-only: */
      c_expr(sub0(p),rdable);
      c_expr(sub2(p),rdable);
    }
    break;
  case N_unary:
    t = (token *)sub0(p); assert (t->type == N_Token);
    c_check_acc(t->tn,need,rdable);
    if (t->code == Uop_name) {
      y = symstackfind(inner,t->graphic);
      if (y == (symbol *)NULL) {
        tokerr(t->tn,"undeclared unary operator");
      } else if (y->type != uopsym) {
        tokerr(t->tn,"not a unary operator");
      }
    }
    /* This reflects the assumption that operands are RD-only: */
    c_expr(sub1(p),rdable);
    break;
  case N_uncomb:
    t = (token *)sub0(p); assert (t->type == N_Token);
    c_check_acc(t->tn,need,rdable);
    if (t->code == Bop_name) {
      y = symstackfind(inner,t->graphic);
      if (y == (symbol *)NULL) {
        tokerr(t->tn,"undeclared binary operator");
      } else if (y->type != bopsym) {
        tokerr(t->tn,"not a binary operator");
      }
    }
    /* This reflects the assumption that operands are RD-only: */
    c_expr(sub1(p),rdable);
    break;
  case N_expr:  /* parenthesized expression */
    c_expr(sub0(p),need);
    break;
  case N_case_of_expr:  /* CASE OF choices ELSE expr END */
    c_check_acc(p->tn,need,rdable);
    pushscope(case_of_expr_scope);
    c_choices(sub0(p));
    c_expr(sub1(p),rdable);
    popscope(case_of_expr_scope);
    break;
  case N_case_ex_expr:  /* CASE expr OF choices ELSE expr END */
    c_check_acc(p->tn,need,rdable);
    pushscope(case_ex_expr_scope);
    c_expr(sub0(p),rdable);
    c_choices(sub1(p));
    c_expr(sub2(p),rdable);
    popscope(case_ex_expr_scope);
    break;
  case N_if_expr:  /* IF expr THEN expr opt_elif_exprs ELSE expr END */
    c_check_acc(p->tn,need,rdable);
    pushscope(if_expr_scope);
    c_expr(sub0(p),rdable);
    c_expr(sub1(p),rdable);
    c_elif_exprs(sub2(p));
    c_expr(sub3(p),rdable);
    popscope(if_expr_scope);
    break;
  case N_expr_clause:
    c_check_acc(p->tn,need,rdable);
    pushscope(expr_clause_scope);
    c_stmts(sub0(p));
    popscope(expr_clause_scope);
    break;
  case N_exists:     /* EXISTS simpliter_list | expr */
  case N_notexists:  /* NOTEXISTS simpliter_list | expr */
  case N_forall:     /* FORALL simpliter_list | expr */
    c_check_acc(p->tn,need,rdable);
    pushscope(loopframe_scope);
    c_simpliter_list(sub0(p));
    c_expr(sub1(p),rdable);
    popscope(loopframe_scope);
    break;
  case N_procval:    /* PROC compound */
                     /* '(' PROC ')' compound */
                     /* ROUTINE compound */
                     /* '(' ROUTINE ')' compound */
    c_check_acc(p->tn,need,rdable);
    q = sub0(p);
    assert (q->type == N_compound);
    r = sub0(q);
    assert (r->type == N_nameseq);
    y = symresolve(inner,r,&k);
    if (y == (symbol *)NULL || y->type != procsym) {
      tokerr(q->tn,"No known procedure has this name");
    }
    for (i=0; i<y->nparms; i++) {
      if ((y->parms[i] & rw) != rd) {
        tokerr(q->tn,"cannot snarf a PROC having WR or RW arguments");
      }
    }
    break;
    /*
     *  I forget why I insist on Command_name here instead of
     *  just a 'compound' that ends in a command name:
     */
  case N_cmdval:   /* COMMAND Command_name */
    c_check_acc(p->tn,need,rdable);
    t = cmdname_token(sub0(p));
    y = symstackfind(inner,t->graphic);
    if (y == (symbol *)NULL || y->type != cmdsym) {
      tokerr(t->tn,"no known command has this name");
    }
    for (i=0; i<y->nparms; i++) {
      if ((y->parms[i] & rw) != rd) {
        tokerr(t->tn,"cannot snarf a COMMAND having WR or RW arguments");
      }
    }
    break;
  case N_indirect_call:   /* CALL ( expr [, expr_list [(*)] ] ) */
    c_check_acc(p->tn,need,rdable);
    c_expr(sub0(p),rdable);
    c_expr_list(sub1(p),rdable);
    c_check_caller_variadicity(p->tn,sub2(p)!=NULL);
    break;
  case N_command:
    c_check_acc(p->tn,need,rdable);
    c_command(p);
    break;
  default:  unexpected(p->type);
  }
} /* end c_expr */

/* .................................................................. */

static void c_command(p)
node *p;
{
  token *t;
  symbol *y;
  switch (p->type) {
  case N_command:  /* Command_name cmdargs [(*)] */
                   /* Command_name */
    t = cmdname_token(sub0(p));
    y = symstackfind(inner,t->graphic);
    assert (y && y->type == cmdsym);
    c_call(t->tn,y,sub1(p),sub2(p)!=NULL);
    break;
  default:  unexpected(p->type);
  }
}

/* .................................................................. */

static void c_choices(p)
node *p;
{
  int i;
  switch (p->type) {
  case N_choices:
    for (i=0; i<p->nsub; i++) c_choice(subi(p,i));
    break;
  default:  unexpected(p->type);
  }
}

/* .................................................................. */

static void c_choice(p)
node *p;
{
  switch (p->type) {
  case N_choice:  /* ( expr_list ) : expr */
    c_expr_list(sub0(p),rdable);
    c_expr(sub1(p),rdable);
    break;
  default:  unexpected(p->type);
  }
}

/* .................................................................. */

static void c_elif_exprs(p)
node *p;
{
  int i;
  switch (p->type) {
  case N_elif_exprs:
    for (i=0; i<p->nsub; i++) c_elif_expr(subi(p,i));
    break;
  default:  unexpected(p->type);
  }
}

/* .................................................................. */

static void c_elif_expr(p)
node *p;
{
  switch (p->type) {
  case N_elif_expr:  /* ELSEIF expr THEN expr */
    c_expr(sub0(p),rdable);
    c_expr(sub1(p),rdable);
    break;
  default:  unexpected(p->type);
  }
}

/* .................................................................. */

/*
 *  The stuff in a selector always only needs to be read; the
 *  syntactically similar iterators are handled separately as
 *  'iterator' and 'simpliter'.
 */
static void c_selector(p)
node *p;
{
  switch (p->type) {
  case N_selector1:  /* ( expr_list [(*)] ) */
    assert (sub1(p) == NULL);  /* no (*) in non-call contexts */
    c_expr_list(sub0(p),rdable);
    break;
  case N_selector2:  /* { expr_list } */
    c_expr_list(sub0(p),rdable);
    break;
  case N_selector3:  /* ( opt_expr Dots opt_expr ) */
    if (sub0(p) != (node *)NULL) c_expr(sub0(p),rdable);
    if (sub1(p) != (node *)NULL) c_expr(sub1(p),rdable);
    break;
  case N_selector4:  /* . nameseq */
    c_selnames(sub0(p),0);
    break;
  default:  unexpected(p->type);
  }
}

/* .................................................................. */

static void c_selnames(node *p, int k) {
  int i;
  switch (p->type) {
  case N_nameseq:
    for (i=k; i<p->nsub; i++) {
      token *t = name_token(subi(p,i));
      symbol *y = symstackfind(inner,t->graphic);
      if (y == (symbol *)NULL || y->type != selsym) {
        /* This is the new "normal" case, where m.x means m('x') if x is
         * not already a SEL.  So now allowing this case.  */
#if 0
        tokerr(t->tn,"not a valid selector or subname");
#endif
      }
      if (y && y->type != selsym) {
        /* The symbol exists, but is not a SEL.  Since keywords such as
         * PRINT are already ruled out syntactically, it might seem
         * natural to extend that to user-defined proc names and
         * other symbols.  But that change, further restricting the
         * vocabulary (namespace) available after dots, would be
         * unhelpful and disruptive.  The disambiguating context of
         * the dot allows that namespace to coexist peacefully with
         * the proc and variable namespace, which for its part needn't
         * be invaded by the tags after dots.  So now allowing this
         * case too.  */
#if 0
        tokerr(t->tn,"not a valid selector or subname");
#endif
      }
    }
    break;
  default:  unexpected(p->type);
  }
}

/* .................................................................. */

static void c_former(p)
node *p;
{
  node *q;
  switch (p->type) {
  case N_former1:  /* expr :              loop_head   */
                   /* expr :     iterator loop_head   */
                   /* expr : FOR iterator loop_head   */
    c_expr(sub0(p),rdable);
    if (sub1(p) != (node *)NULL) c_iterator(sub1(p));
    c_loop_head(sub2(p));
    break;
  case N_former2:  /* expr_list .. expr */
    q = sub0(p);
    assert (q->type == N_expr_list);
    assert (q->nsub > 0);
    if (q->nsub > 2) {
      tokerr(q->tn,"more than 2 expressions before dots");
    }
    c_expr_list(q,rdable);
    c_expr(sub1(p),rdable);
    break;
  case N_former3:  /* operand IN expr | expr */
    c_expr(sub0(p),rwable);
    c_expr(sub1(p),rdable);
    c_expr(sub2(p),rdable);
    break;
  default:  unexpected(p->type);
  }
} /* end c_former */

/* .................................................................. */

/* Local utility - does not correspond to a parse tree node type:  */
static void c_check_acc(tn,need,ability)
int tn;
bits need,ability;
{
  bits deficiency = need & ~ability;
  if (deficiency == rdable) tokerr(tn,"cannot fetch from this");
  if (deficiency == wrable) tokerr(tn,"cannot store into this");
  if (deficiency) tokerr(tn,"cannot fetch nor store");
}

/* .................................................................. */

/* Another local utility, perhaps the most important in the system:  */
static void c_call(tn,y,p,vactual)
int tn;  /* point of call */
symbol *y;  /* symbol representing routine to call */
node *p;  /* list of actual args (null or a list of nodes) */
bool vactual;  /* "(*)" after actual args */
{
  int i;
  int nformal,nactual;  /* number of formal, actual args */
  bool vformal;  /* "(*)" after formal parameters */
  c_check_caller_variadicity(tn,vactual);
  nformal = y->nparms;
  assert (nformal >= 0);  /* negative y->nparms is impossible */
  vformal = nformal > 0 && !!(y->parms[nformal-1] & starred);
  nactual = p==(node *)NULL ? 0 : p->nsub;
  assert (nactual >= 0);  /* negative p->nsub is impossible */
  if (nactual == 0) {  /* no args supplied */
    assert (!vactual);  /* "(*)" syntactically impossible with no args */
    if (nformal == 0) return;  /* no args expected */
    if (nformal == 1 && vformal) return;  /* all args optional */
    tokerr(tn,"at least one arg required on this call");
  }
  /* At this point we know nactual > 0 */
  if (nformal == 0) {
    tokerr(tn,"this routine takes no args");
  }
  /* Now we also know nformal > 0 */
  switch (p->type) {
  /*
   *  We allow N_lhs_list, N_rw_list, and N_lhs_or_dash_list
   *  in addition to the "normal" N_expr_list and N_cmdargs here,
   *  though the grammar doesn't currently allow anything but
   *  those two syntactic categories as an actual arg list.
   *  The main thing about these node types is that they all bear
   *  a list of nodes that are appropriate to pass to to c_expr().
   *  The extra generality could help support syntactic extensions
   *  for writable args, such as the lone hyphen.
   */
  case N_expr_list:
  case N_lhs_list:
  case N_rw_list:
  case N_lhs_or_dash_list:
  case N_cmdargs:
    if (vactual && !vformal) {
      tokerr(tn,"trailing (*) only allowed for variadic callee");
    }
    if (vformal && !vactual) {
      /* Variadic callee, and no (*) after actual args.  */
      if (nactual < nformal-1) {
        tokerr(tn,"too few args on this call");
      }
    } else {
      /* Either the callee is not variadic (!vformal), or it is but
       * the call also has a (*), in which case nactual may exceed
       * nformal.  */
      assert (vformal == vactual);  /* by above if's */
      if (nactual < nformal) {
        tokerr(tn,"too few args on this call");
      }
      if (!vformal && nactual > nformal) {
        tokerr(tn,"too many args on this call");
      }
    }
    /* This is what the above checking is supposed to achieve:  */
    if (!vformal) {  /* non-variadic callee */
      assert (nactual == nformal);
    } else if (vactual) {  /* variadic callee, with (*) call */
      assert (nactual >= nformal);
    } else {  /* variadic callee, with ordinary non-(*) call */
      assert (nactual >= nformal-1);  /* all optional args omitted */
    }
    for (i=0; i<nformal-1; i++) {  /* required args */
      c_expr(subi(p,i),y->parms[i]&rwable);
    }
    for (; i<nactual; i++) {  /* last arg or optional args */
      c_expr(subi(p,i),y->parms[nformal-1]&rwable);
    }
    break;
  default:  unexpected(p->type);
  }
} /* end c_call */

/* .................................................................. */

static void c_check_caller_variadicity(tn,vactual)
int tn;  /* point of call */
bool vactual;  /* "(*)" after actual args */
{
  if (vactual) {
    /* Make sure the caller is also variadic */
    int caller_nformal = curholder->nparms;
    bool caller_vformal = (caller_nformal > 0) &&
         (curholder->parms[caller_nformal - 1] & starred);
    if (!caller_vformal) {
      tokerr(tn,"caller must be variadic to make (*) call");
    }
  }
} /* end c_check_caller_variadicity */
