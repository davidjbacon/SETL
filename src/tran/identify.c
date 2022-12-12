/*  ===  Set up "global" symbol table(s)  ==========================  */

/*  $Id: identify.c,v 1.21 2021/01/08 15:25:18 setlorg Exp $  */

/*  Free software (c) dB - see file COPYING for license (GPL).  */

/*
 *  All explicit declarations, labels, and refinement labels
 *  cause this subpass to make an entry in a symbol table.
 * 
 *  This subpass and 'canonize' are quite similar in structure,
 *  and similarly maintain a stack of symbol tables (a symstack).
 *  They also recognize and respond to lexical "scopes" in the
 *  same way, and the code generator is meant to mimic that.
 *  The suspiciously alert reader may recognize that the parser
 *  could have put this scope information right into the parse tree
 *  originally, but it's just as easy to track it with the "scope
 *  manager" here instead, and saves tree clutter.
 * 
 *  I refer to things introduced via VAR, INIT, and CONST
 *  declarations as "variables" here.
 * 
 *  Entities introduced by PROC, PROCEDURE, OP, OPERATOR, or COMMAND
 *  are called "routines" here.
 * 
 *  Note that REPR declarations were parsed (rather shallowly)
 *  but are ignored here and hereafter.
 * 
 *
 *  Package specs are processed before package bodies and
 *  programs, to produce a set of public global symbol
 *  tables.  These are then copied as the initial values
 *  of packages' private global symbol tables.  Finally,
 *  the package bodies and programs are processed to extend
 *  their private global and local symbol tables based on
 *  explicit declarations.
 * 
 *  So what you end up with from this pass, symbol-table-wise,
 *  is 'publictab' which is complete, and 'privatetab' which
 *  is complete except for the implicit declarations local to
 *  programs and procedures.
 * 
 *  This pass has the luxury of only having to worry about
 *  explicit declarations, which have uncomplicated naked
 *  names.
 *
 *
 *  Note - most instances of i_stmts are guarded by an enclosing
 *  pushscope-popscope envelope.  The exceptions occur where we
 *  allow jumping among the parts of, e.g., IF statements.  It
 *  is probably silly to support such abusage, and it would clean
 *  things up a bit to put the guard right into i_stmts itself.
 */

/* ------------------------------------------------------------------ */

#include "setltran.h"
#include "y.tab.h"

/* Local routines */
static void i_routine_specs(node *p);
static void i_routine_spec(node *p);
static void i_routines(node *p);
static void i_routine(node *p);
static void i_form_specs(node *p);
static void i_form_spec(node *p);
static void i_body(node *p);
static void i_decls(node *p);
static void i_decl(node *p);
static void i_vars(node *p, node *b);
static void i_consts(node *p);
static void i_inits(node *p);
static void i_sels(node *p);
static void i_decl_expr(node *p);
static void i_refinements(node *p);
static void i_refinement(node *p);
static void i_stmts(node *p);
static void i_stmt(node *p);
static void i_elifs(node *p);
static void i_elif(node *p);
static void i_when_stmts(node *p);
static void i_when_stmt(node *p);
static void i_loop_head(node *p);
static void i_loop_init(node *p);
static void i_loop_doing(node *p);
static void i_loop_while(node *p);
static void i_loop_step(node *p);
static void i_loop_until(node *p);
static void i_loop_term(node *p);
static void i_iterator(node *p);
static void i_simpliter_list(node *p);
static void i_simpliter(node *p);
static void i_expr_list(node *p);
static void i_lhs_list(node *p);
static void i_rw_list(node *p);
static void i_lhs_or_dash_list(node *p);
static void i_expr(node *p);
static void i_command(node *p);
static void i_cmdargs(node *p);
static void i_cmdarg(node *p);
static void i_choices(node *p);
static void i_choice(node *p);
static void i_elif_exprs(node *p);
static void i_elif_expr(node *p);
static void i_selector(node *p);
static void i_former(node *p);


/* ------------------------------------------------------------------ */

/* Local data */

static bool in_routine;  /* true only where RETURN stmts are allowed */

#include "symstack.h"

/* ------------------------------------------------------------------ */

void identify(p)
node *p;
{
  int i;
  token *t;
  node *q,*r;
  symbol *x,*y;
  symbol *sysrottab_holder;
  bool program_defined = false;
  in_routine = false;
  glowball = (symbol *)NULL;
  publictab = new_symtab();
  privatetab = new_symtab();
  inner = (symstack *)NULL;  /* initially empty stack of symtabs */
  sysrottab_holder = new_symtabholder(sysrottab);
  pushsymtab(sysrottab_holder);  /* push sysrot table onto symstack */
  if (p) {
    switch (p->type) {
    case N_unnamed_program:
      /*
       *  This is like a degenerate case of an N_program_unit in an
       *  N_complex_program.
       */
      y = syminsert (privatetab, "_unnamed_SETL_program", progsym, -1, NULL, 0, nosym);
      y->stab = new_symtab();
      x = syminsert (y->stab, "_MAIN", procsym, -1, y, 0, nosym);
      x->stab = new_symtab();
      pushsymtab(y);  /* program unit-global symstack level */
      q = sub0(p); assert (q->type == N_body);
      initscope(proc_scope);  /* decls and code coming */
      glowball = x;  /* for tags in rhs of global decls */
      i_decls(sub0(q));  /* program globals */
      glowball = (symbol *)NULL;
      pushsymtab(x);  /* mainproc on behalf of program unit */
      pushscope(stmts_scope);  /* stmts coming */
      i_stmts(sub1(q));
      popscope(stmts_scope);  /* back to decls and code */
      i_refinements(sub2(q));
      endscope(proc_scope);  /* back to no control scope */
      assert (curholder == x);  /* still at mainproc level */
      popsymtab();  /* from mainproc back to program unit level */
      i_routines(sub1(p));  /* PROC/OP/COMMAND defns */
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
       *   (b) the globals owned by each package/program unit; and
       *
       *   (c) procedure-local symbols.
       *
       *  Also, when 'glowball' is non-NULL, i_decl_expr() pushes
       *  'glowball' onto the symstack around its call to i_expr().
       *  This is used during processing of global decls, so
       *  'glowball' goes at level (c) at that time.
       */
      p = sub0(p);  /* shoot me */
      assert (p->type == N_compilation_units);
      /* Loop over the package specs */
      for (i=0; i<p->nsub; i++) {
        q = subi(p,i);
        if (q->type == N_package_spec) {
          t = name_token(sub0(q));
          y = syminsert (publictab, t->graphic, packsym, t->tn, NULL, 0, nosym);
          y->stab = new_symtab();
          /* See canonize() in canonize.c for more info on this name: */
          x = syminsert (y->stab, strjoin("_PUBINIT_",t->graphic), procsym, -1, y, 0, nosym);
          x->stab = new_symtab();
          pushsymtab(y);  /* package-global level (b) */
          initscope(proc_scope);  /* decls and code coming */
          glowball = x;  /* for tags in rhs of global decls */
          i_decls(sub2(q));  /* public globals */
          glowball = (symbol *)NULL;
          endscope(proc_scope);  /* back to no control scope */
          i_routine_specs(sub3(q));  /* public procedure specs */
          assert (curholder == y);  /* at package level (b) */
          popsymtab();  /* back to outer level (a, sysrots) */
          x = (symbol *)NULL;
          y = (symbol *)NULL;
        }
      }
      /* Loop over the program unit and package bodies */
      for (i=0; i<p->nsub; i++) {
        q = subi(p,i);
        switch (q->type) {
        case N_package_spec:
          break;  /* did specs in first loop above */
        case N_package_body:
          t = name_token(sub0(q));
          x = symfind(publictab,t->graphic);
          if (x == (symbol *)NULL) {
            tokerr(t->tn,"Missing package specifications");
          }
          assert (x->type == packsym);
          y = syminsert (privatetab, t->graphic, packsym, t->tn, NULL, 0, nosym);
          y->stab = symtabcopy (x->stab);
          x = syminsert (y->stab, strjoin("_PRIVINIT_",t->graphic), procsym, -1, y, 0, nosym);
          x->stab = new_symtab();
          pushsymtab(y);  /* package-global level (b) */
          initscope(proc_scope);  /* decls and code coming */
          glowball = x;  /* for tags in rhs of global decls */
          i_decls(sub3(q));  /* globals private to the package */
          glowball = (symbol *)NULL;
          endscope(proc_scope);  /* back to no control scope */
          i_routines(sub4(q));  /* PROC/OP/COMMAND defns */
          assert (curholder == y);  /* at package level (b) */
          popsymtab();  /* back to outer level (a, sysrots) */
          x = (symbol *)NULL;
          y = (symbol *)NULL;
          break;
        case N_program_unit:
          t = name_token(sub0(q));
          if (program_defined) {
            tokerr(t->tn,"Only one PROGRAM may be defined");
          }
          program_defined = true;
          y = syminsert (privatetab, t->graphic, progsym, t->tn, NULL, 0, nosym);
          y->stab = new_symtab();
          x = syminsert (y->stab, "_MAIN", procsym, -1, y, 0, nosym);
          x->stab = new_symtab();
          r = sub2(q); assert (r->type == N_body);
          pushsymtab(y);  /* program unit-global level (b) */
          initscope(proc_scope);  /* decls and code coming */
          glowball = x;  /* for tags in rhs of global decls */
          i_decls(sub0(r));  /* globals private to the program unit */
          glowball = (symbol *)NULL;
          pushsymtab(x);  /* mainproc on behalf of pgm unit (level c) */
          pushscope(stmts_scope);  /* stmts coming */
          i_stmts(sub1(r));
          popscope(stmts_scope);  /* back to decls and code */
          i_refinements(sub2(r));
          endscope(proc_scope);  /* back to no control scope */
          assert (curholder == x);  /* still at mainproc level (c) */
          popsymtab();  /* back to program unit level (b) */
          i_routines(sub3(q));  /* PROC/OP/COMMAND defns */
          assert (curholder == y);  /* at program unit level (b) */
          popsymtab();  /* back to outer level (a, sysrots) */
          x = (symbol *)NULL;
          y = (symbol *)NULL;
          break;
        default:  unexpected(q->type);
        }
      }
      if (!program_defined) {
        tranerr(srclen,"No PROGRAM defined");
      }
      break;
    default:  unexpected(p->type);
    } /* end switch (p->type) */
  } else {
    /* null program */
    y = syminsert (privatetab, "_empty_SETL_program", progsym, -1, NULL, 0, nosym);
    y->stab = new_symtab();
    x = syminsert (y->stab, "_MAIN", procsym, -1, y, 0, nosym);
    x->stab = new_symtab();
    x = (symbol *)NULL;
    y = (symbol *)NULL;
  }
  assert (curholder == sysrottab_holder);
  assert (cursymtab == sysrottab);
  popsymtab();  /* back to no symstack at all */
  assert (inner == (symstack *)NULL);
} /* end identify */

/* .................................................................. */

static void i_routine_specs(p)
node *p;
{
  int i;
  switch (p->type) {
  case N_routine_specs:
    for (i=0; i<p->nsub; i++) i_routine_spec(subi(p,i));
    break;
  default:  unexpected(p->type);
  }
}

/* .................................................................. */

static void i_routine_spec(p)
node *p;
{
  int i;
  token *t;
  node *q,*f;
  symbol *y;
  initscope(proc_scope);
  switch (p->type) {
  case N_proc_spec:
    t = name_token(sub0(p));
    q = sub1(p); assert (q->type == N_form_specs);
    y = symdef(curholder,t,procsym,q->nsub);
    for (i=0; i<q->nsub; i++) {
      f = subi(q,i);
      switch (f->type) {
      case N_rd_spec:  y->parms[i] = rdable; break;
      case N_wr_spec:  y->parms[i] = wrable; break;
      case N_rw_spec:  y->parms[i] = rwable; break;
      default:  unexpected(f->type);
      }
    }
    if (sub2(p) != (node *)NULL) {
      assert (q->nsub > 0);
      y->parms[q->nsub-1] |= starred;
    }
    break;
  case N_cmd_spec:
    t = cmdname_token(sub0(p));
    q = sub1(p); assert (q->type == N_form_specs);
    y = symdef(curholder,t,cmdsym,q->nsub);
    for (i=0; i<q->nsub; i++) {
      f = subi(q,i);
      switch (f->type) {
      case N_rd_spec:  y->parms[i] = rdable; break;
      case N_wr_spec:  y->parms[i] = wrable; break;
      case N_rw_spec:  y->parms[i] = rwable; break;
      default:  unexpected(f->type);
      }
    }
    if (sub2(p) != (node *)NULL) {
      assert (q->nsub > 0);
      y->parms[q->nsub-1] |= starred;
    }
    break;
  case N_binop_spec:
    t = bopname_token(sub0(p));
    y = symdef(curholder,t,bopsym,2);
    /* Operands assumed RD-only! */
    y->parms[0] = rdable;
    y->parms[1] = rdable;
    break;
  case N_unop_spec:
    t = uopname_token(sub0(p));
    y = symdef(curholder,t,uopsym,1);
    /* Operands assumed RD-only! */
    y->parms[0] = rdable;
    break;
  default:  unexpected(p->type);
  }
  endscope(proc_scope);
}

/* .................................................................. */

static void i_routines(p)
node *p;
{
  int i;
  switch (p->type) {
  case N_routines:
    for (i=0; i<p->nsub; i++) i_routine(subi(p,i));
    break;
  default:  unexpected(p->type);
  }
}

/* .................................................................. */

static void i_routine(p)
node *p;
{
  int i;
  bits a;
  token *t;
  node *q;
  node *f = NULL;
  symbol *y;
  in_routine = true;
  initscope(proc_scope);
  switch (p->type) {
  case N_procdef:
    t = name_token(sub0(p));
    q = sub1(p); assert (q->type == N_form_specs);
    y = symfind(cursymtab,t->graphic);
    if (y != (symbol *)NULL) {
      if (y->type != procsym) tokerr(t->tn,"redeclaration");
      if (y->stab != (symtab)NULL) tokerr(t->tn,"redefinition");
      if (q->nsub != y->nparms) {
        tokerr(t->tn,"number of parameters conflicts with specification");
      }
      for (i=0; i<q->nsub; i++) {
        f = subi(q,i);
        switch (f->type) {
        case N_rd_spec:  a = rdable; break;
        case N_wr_spec:  a = wrable; break;
        case N_rw_spec:  a = rwable; break;
        default:  unexpected(f->type);
        }
        if (a != (y->parms[i] & rwable)) {
          tokerr(f->tn,"r/w indicator conflicts with specification");
        }
      }
      if (q->nsub > 0) {
        if (((y->parms[q->nsub-1] & starred) == 0) !=
            (sub2(p) == (node *)NULL)) {
          tokerr(f->tn,"\"(*)\" indicator conflicts with specification");
        }
      }
    } else {
      y = symdef(curholder,t,procsym,q->nsub);
      for (i=0; i<q->nsub; i++) {
        f = subi(q,i);
        switch (f->type) {
        case N_rd_spec:  y->parms[i] = rdable; break;
        case N_wr_spec:  y->parms[i] = wrable; break;
        case N_rw_spec:  y->parms[i] = rwable; break;
        default:  unexpected(f->type);
        }
      }
      if (sub2(p) != (node *)NULL) {
        assert (q->nsub > 0);
        y->parms[q->nsub-1] |= starred;
      }
    }
    y->stab = new_symtab();
    pushsymtab(y);  /* routine locals */
    i_form_specs(sub1(p));
    i_body(sub3(p));
    popsymtab();  /* back to program or package level */
    break;
  case N_cmddef:
    t = cmdname_token(sub0(p));
    q = sub1(p); assert (q->type == N_form_specs);
    y = symfind(cursymtab,t->graphic);
    if (y != (symbol *)NULL) {
      if (y->type != cmdsym) tokerr(t->tn,"redeclaration");
      if (y->stab != (symtab)NULL) tokerr(t->tn,"redefinition");
      if (q->nsub != y->nparms) {
        tokerr(t->tn,"number of parameters conflicts with specification");
      }
      for (i=0; i<q->nsub; i++) {
        f = subi(q,i);
        switch (f->type) {
        case N_rd_spec:  a = rdable; break;
        case N_wr_spec:  a = wrable; break;
        case N_rw_spec:  a = rwable; break;
        default:  unexpected(f->type);
        }
        if (a != (y->parms[i] & rwable)) {
          tokerr(f->tn,"r/w indicator conflicts with specification");
        }
      }
      if (q->nsub > 0) {
        if (((y->parms[q->nsub-1] & starred) == 0) !=
            (sub2(p) == (node *)NULL)) {
          tokerr(f->tn,"\"(*)\" indicator conflicts with specification");
        }
      }
    } else {
      y = symdef(curholder,t,cmdsym,q->nsub);
      for (i=0; i<q->nsub; i++) {
        f = subi(q,i);
        switch (f->type) {
        case N_rd_spec:  y->parms[i] = rdable; break;
        case N_wr_spec:  y->parms[i] = wrable; break;
        case N_rw_spec:  y->parms[i] = rwable; break;
        default:  unexpected(f->type);
        }
      }
      if (sub2(p) != (node *)NULL) {
        assert (q->nsub > 0);
        y->parms[q->nsub-1] |= starred;
      }
    }
    y->stab = new_symtab();
    pushsymtab(y);  /* routine locals */
    i_form_specs(sub1(p));
    i_body(sub3(p));
    popsymtab();  /* back to program or package level */
    break;
  case N_binopdef:
    t = bopname_token(sub0(p));
    y = symfind(cursymtab,t->graphic);
    if (y != (symbol *)NULL) {
      if (y->type != bopsym) tokerr(t->tn,"redeclaration");
      if (y->stab != (symtab)NULL) tokerr(t->tn,"redefinition");
    } else {
      y = symdef(curholder,t,bopsym,2);
      y->parms[0] = rdable;
      y->parms[1] = rdable;
    }
    y->stab = new_symtab();
    pushsymtab(y);  /* routine locals */
    i_form_spec(sub1(p));
    i_form_spec(sub2(p));
    i_body(sub3(p));
    popsymtab();  /* back to program or package level */
    break;
  case N_unopdef:
    t = uopname_token(sub0(p));
    y = symfind(cursymtab,t->graphic);
    if (y != (symbol *)NULL) {
      if (y->type != uopsym) tokerr(t->tn,"redeclaration");
      if (y->stab != (symtab)NULL) tokerr(t->tn,"redefinition");
    } else {
      y = symdef(curholder,t,uopsym,1);
      y->parms[0] = rdable;
    }
    y->stab = new_symtab();
    pushsymtab(y);  /* routine locals */
    i_form_spec(sub1(p));
    i_body(sub2(p));
    popsymtab();  /* back to program or package level */
    break;
  default:  unexpected(p->type);
  }
  endscope(proc_scope);
  in_routine = false;
} /* end i_routine */

/* .................................................................. */

static void i_form_specs(p)
node *p;
{
  int i;
  switch (p->type) {
  case N_form_specs:
    for (i=0; i<p->nsub; i++) i_form_spec(subi(p,i));
    break;
  default:  unexpected(p->type);
  }
}

/* .................................................................. */

static void i_form_spec(p)
node *p;
{
  token *t;
  symbol *y;
  switch (p->type) {
  case N_rd_spec:
  case N_wr_spec:
  case N_rw_spec:
    /* All formals are rwable within the routine. */
    t = name_token(sub0(p));
    y = symdef(curholder,t,parsym,0);
    y->ability = rwable;
    break;
  default:  unexpected(p->type);
  }
}

/* .................................................................. */

static void i_body(p)
node *p;
{
  switch (p->type) {
  case N_body:
    i_decls(sub0(p));
    pushscope(stmts_scope);
    i_stmts(sub1(p));
    popscope(stmts_scope);
    i_refinements(sub2(p));
    break;
  default:  unexpected(p->type);
  }
}

/* .................................................................. */

static void i_decls(p)
node *p;
{
  int i;
  pushscope(decls_scope);
  switch (p->type) {
  case N_decls:
    for (i=0; i<p->nsub; i++) i_decl(subi(p,i));
    break;
  default:  unexpected(p->type);
  }
  popscope(decls_scope);
}

/* .................................................................. */

static void i_decl(p)
node *p;
{
  switch (p->type) {
  case N_var_decl:
    i_vars(sub0(p),sub1(p));
    break;
  case N_const_decl:
    i_consts(sub0(p));
    break;
  case N_init_decl:
    i_inits(sub0(p));
    break;
  case N_sel_decl:
    i_sels(sub0(p));
    break;
  case N_repr_decl:
    break;
  default:  unexpected(p->type);
  }
}

/* .................................................................. */

static void i_vars(p,b)
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
      q = subi(p,i); assert (q->type == N_var);
      t = name_token(sub0(q));
      y = symdef(curholder,t,varsym,0);
      y->ability = rwable;
      if (b != (node *)NULL) y->ability |= backtrack;
      if (sub1(q) != (node *)NULL) i_decl_expr(sub1(q));
    }
    break;
  default:  unexpected(p->type);
  }
}

/* .................................................................. */

static void i_consts(p)
node *p;
{
  int i;
  node *q;
  token *t;
  symbol *y;
  switch (p->type) {
  case N_ascription_list:
    for (i=0; i<p->nsub; i++) {
      q = subi(p,i); assert (q->type == N_ascription);
      t = name_token(sub0(q));
      y = symdef(curholder,t,constsym,0);
      y->ability = rdable;
      if (sub1(q) != (node *)NULL) i_decl_expr(sub1(q));
    }
    break;
  default:  unexpected(p->type);
  }
}

/* .................................................................. */

static void i_inits(p)
node *p;
{
  int i;
  node *q;
  token *t;
  symbol *y;
  switch (p->type) {
  case N_initial_list:
    for (i=0; i<p->nsub; i++) {
      q = subi(p,i); assert (q->type == N_initial);
      t = name_token(sub0(q));
      /*
       *  You can say VAR and then INIT on the same variable (not the
       *  other way around, though):
       */
      y = syminsert (cursymtab, t->graphic, initsym, t->tn, curholder, 0, varsym);
      y->ability = rwable;
      i_decl_expr(sub1(q));
    }
    break;
  default:  unexpected(p->type);
  }
}

/* .................................................................. */

static void i_sels(p)
node *p;
{
  int i;
  node *q;
  token *t;
  symbol *y;
  switch (p->type) {
  case N_sel_list:
    for (i=0; i<p->nsub; i++) {
      q = subi(p,i); assert (q->type == N_sel);
      t = name_token(sub0(q));
      y = symdef(curholder,t,selsym,0);
      y->ability = rdable;
      i_decl_expr(sub1(q));
    }
    break;
  default:  unexpected(p->type);
  }
}

/* .................................................................. */

static void i_decl_expr(p)
node *p;
{
  /*
   *  We don't want tags that appear only on the right-hand side
   *  of global decls to go into the global symtab.
   *
   *  They don't actually get entered into a symtab at all until
   *  canonize.c, but this sets the pattern.
   */
  if (glowball != (symbol *)NULL) {
    pushsymtab(glowball);  /* extra symstack level to catch the tags */
    i_expr(p);
    popsymtab();
  } else {
    i_expr(p);
  }
}

/* .................................................................. */

static void i_refinements(p)
node *p;
{
  int i;
  switch (p->type) {
  case N_refinements:
    for (i=0; i<p->nsub; i++) i_refinement(subi(p,i));
    break;
  default:  unexpected(p->type);
  }
}

/* .................................................................. */

static void i_refinement(p)
node *p;
{
  token *t;
  symbol *y;
  switch (p->type) {
  case N_refinement:
    t = refname_token(sub0(p));
    y = symdef(curholder,t,refsym,0);
    (void)y;  /* avoid GCC's "set but not used" warning */
    pushscope(refinement_scope);
    i_stmts(sub1(p));
    popscope(refinement_scope);
    break;
  default:  unexpected(p->type);
  }
}

/* .................................................................. */

static void i_stmts(p)
node *p;
{
  int i;
  switch (p->type) {
  case N_stmts:
    for (i=0; i<p->nsub; i++) i_stmt(subi(p,i));
    break;
  default:  unexpected(p->type);
  }
}

/* .................................................................. */

static void i_stmt(p)
node *p;
{
  token *t;
  symbol *y;
  switch (p->type) {
  case N_labelled_stmt:
    t = name_token(sub0(p));
    /*
     *  If it is ever desired to allow label names to "conflict" with
     *  other tags, perhaps a prefix convention would do...
     */
    y = symdef(curholder,t,labsym,0);
    y->scopenum = curscope->scopenum;
    i_stmt(sub1(p));
    break;
  case N_assert_stmt:
    i_expr(sub0(p));
    break;
  case N_continue_stmt:
  case N_fail_stmt:
  case N_succeed_stmt:
  case N_goto_stmt:
  case N_pass_stmt:
  case N_quit_stmt:
    break;
  case N_return_stmt:
    /*
     *  We could allow RETURN as an equivalent to STOP in the main
     *  program if that was really wanted.  An easy but sleazy way to
     *  allow it would be to replace the following check with:
     *
     *    if (!in_routine) p->type = N_stop_stmt;
     *
     *  But I don't think there are any other places where I zap the
     *  parse tree like that, and it isn't worth doing so for this one
     *  case, not to mention the fact that run-time errors relating to
     *  the RETURN would then refer to it as STOP.  One could implement
     *  that dubious feature properly in the code generator and run time
     *  with a little more effort and no parse tree hacking, of course.
     */
    if (!in_routine) tokerr(p->tn,"must use STOP to exit main program");
    if (sub0(p) != (node *)NULL) i_expr(sub0(p));
    break;
  case N_stop_stmt:
    if (sub0(p) != (node *)NULL) i_expr(sub0(p));
    break;
  case N_yield_stmt:
    i_expr(sub0(p));
    break;
  case N_if_stmt:
    pushscope(if_scope);
    i_stmts(sub1(p));
    i_elifs(sub2(p));
    i_stmts(sub3(p));
    popscope(if_scope);
    break;
  case N_case_of_stmt:
    pushscope(case_of_scope);
    i_stmts(sub0(p));
    i_stmts(sub1(p));
    popscope(case_of_scope);
    break;
  case N_case_ex_stmt:
    pushscope(case_ex_scope);
    i_stmts(sub1(p));
    i_stmts(sub2(p));
    popscope(case_ex_scope);
    break;
  case N_case_of_when_stmt:
    pushscope(case_of_scope);
    i_when_stmts(sub0(p));
    i_stmts(sub1(p));
    popscope(case_of_scope);
    break;
  case N_case_ex_when_stmt:
    pushscope(case_ex_scope);
    i_when_stmts(sub1(p));
    i_stmts(sub2(p));
    popscope(case_ex_scope);
    break;
  case N_choice_stmt:
    if (curscope->type != case_of_scope &&
        curscope->type != case_ex_scope) {
      tokerr(p->tn,"must occur in a CASE scope");
    }
    i_stmt(sub1(p));
    break;
  case N_loop_stmt:
    pushscope(loopframe_scope);
    if (sub0(p) != (node *)NULL) i_iterator(sub0(p));
    if (sub1(p) != (node *)NULL) i_loop_head(sub1(p));
    pushscope(loopbody_scope);
    i_stmts(sub2(p));
    popscope(loopbody_scope);
    popscope(loopframe_scope);
    break;
  case N_assignment_stmt:
    i_expr(sub0(p));
    i_expr(sub2(p));
    break;
  case N_call_stmt:
    i_expr(sub0(p));
    break;
  case N_syscall_stmt:
    i_expr_list(sub1(p));
    break;
  case N_from_stmt:
    i_expr(sub0(p));
    i_expr(sub2(p));
    break;
  case N_indirect_call_stmt:
    i_expr(sub0(p));
    i_expr_list(sub1(p));
    break;
  case N_cmd_stmt:
    i_command(sub0(p));
    break;
  case N_expr_stmt:
    i_expr(sub0(p));
    break;
  case N_machine_stmt:
  case N_embedded_c_code:
  case N_null_stmt:
    break;
  default:  unexpected(p->type);
  }
} /* end i_stmt */

/* .................................................................. */

static void i_elifs(p)
node *p;
{
  int i;
  switch (p->type) {
  case N_elifs:
    for (i=0; i<p->nsub; i++) i_elif(subi(p,i));
    break;
  default:  unexpected(p->type);
  }
}

/* .................................................................. */

static void i_elif(p)
node *p;
{
  switch (p->type) {
  case N_elif:
    i_expr(sub0(p));
    i_stmts(sub1(p));
    break;
  default:  unexpected(p->type);
  }
}

/* .................................................................. */

static void i_when_stmts(p)
node *p;
{
  int i;
  switch (p->type) {
  case N_when_stmts:
    for (i=0; i<p->nsub; i++) i_when_stmt(subi(p,i));
    break;
  default:  unexpected(p->type);
  }
}

/* .................................................................. */

static void i_when_stmt(p)
node *p;
{
  switch (p->type) {
  case N_when_stmt:
    i_expr_list(sub0(p));
    i_stmts(sub1(p));
    break;
  default:  unexpected(p->type);
  }
}

/* .................................................................. */

static void i_loop_head(p)
node *p;
{
  switch (p->type) {
  case N_loop_head:
    if (sub0(p) != (node *)NULL) i_loop_init(sub0(p));
    if (sub1(p) != (node *)NULL) i_loop_doing(sub1(p));
    if (sub2(p) != (node *)NULL) i_loop_while(sub2(p));
    if (sub3(p) != (node *)NULL) i_loop_step(sub3(p));
    if (sub4(p) != (node *)NULL) i_loop_until(sub4(p));
    if (sub5(p) != (node *)NULL) i_loop_term(sub5(p));
    break;
  default:  unexpected(p->type);
  }
}

/* .................................................................. */

static void i_loop_init(p)
node *p;
{
  switch (p->type) {
  case N_loop_init:
    pushscope(init_scope);
    i_stmts(sub0(p));
    popscope(init_scope);
    break;
  default:  unexpected(p->type);
  }
}

/* .................................................................. */

static void i_loop_doing(p)
node *p;
{
  switch (p->type) {
  case N_loop_doing:
    pushscope(doing_scope);
    i_stmts(sub0(p));
    popscope(doing_scope);
    break;
  default:  unexpected(p->type);
  }
}

/* .................................................................. */

static void i_loop_while(p)
node *p;
{
  switch (p->type) {
  case N_loop_while:
    i_expr(sub0(p));
    break;
  default:  unexpected(p->type);
  }
}

/* .................................................................. */

static void i_loop_step(p)
node *p;
{
  switch (p->type) {
  case N_loop_step:
    pushscope(step_scope);
    i_stmts(sub0(p));
    popscope(step_scope);
    break;
  default:  unexpected(p->type);
  }
}

/* .................................................................. */

static void i_loop_until(p)
node *p;
{
  switch (p->type) {
  case N_loop_until:
    i_expr(sub0(p));
    break;
  default:  unexpected(p->type);
  }
}

/* .................................................................. */

static void i_loop_term(p)
node *p;
{
  switch (p->type) {
  case N_loop_term:
    pushscope(term_scope);
    i_stmts(sub0(p));
    popscope(term_scope);
    break;
  default:  unexpected(p->type);
  }
}

/* .................................................................. */

static void i_iterator(p)
node *p;
{
  switch (p->type) {
  case N_iterator:
    i_simpliter_list(sub0(p));
    if (sub1(p) != (node *)NULL) i_expr(sub1(p));  /* ST */
    break;
  default:  unexpected(p->type);
  }
}

/* .................................................................. */

static void i_simpliter_list(p)
node *p;
{
  int i;
  switch (p->type) {
  case N_simpliter_list:
    for (i=0; i<p->nsub; i++) i_simpliter(subi(p,i));
    break;
  default:  unexpected(p->type);
  }
}

/* .................................................................. */

static void i_simpliter(p)
node *p;
{
  switch (p->type) {
  case N_simpliter1:  /* lhs IN expr */
    i_expr(sub0(p));
    i_expr(sub1(p));
    break;
  case N_simpliter2:  /* lhs = compound_or_predef ( lhs_list ) */
  case N_simpliter3:  /* lhs = compound_or_predef { lhs_list } */
    i_expr(sub0(p));
    i_expr(sub1(p));
    i_lhs_list(sub2(p));
    break;
  default:  unexpected(p->type);
  }
}

/* .................................................................. */

static void i_expr_list(p)
node *p;
{
  int i;
  switch (p->type) {
  case N_expr_list:
    for (i=0; i<p->nsub; i++) i_expr(subi(p,i));
    break;
  default:  unexpected(p->type);
  }
}

/* .................................................................. */

static void i_lhs_list(p)
node *p;
{
  int i;
  switch (p->type) {
  case N_lhs_list:
    for (i=0; i<p->nsub; i++) i_expr(subi(p,i));
    break;
  default:  unexpected(p->type);
  }
}

/* .................................................................. */

static void i_rw_list(p)
node *p;
{
  int i;
  switch (p->type) {
  case N_rw_list:
    for (i=0; i<p->nsub; i++) i_expr(subi(p,i));
    break;
  default:  unexpected(p->type);
  }
}

/* .................................................................. */

static void i_lhs_or_dash_list(p)
node *p;
{
  int i;
  switch (p->type) {
  case N_lhs_or_dash_list:
    for (i=0; i<p->nsub; i++) i_expr(subi(p,i));
    break;
  default:  unexpected(p->type);
  }
}

/* .................................................................. */

/*
 *  It may seem silly to have this, but there may come a time when
 *  declarations can occur inside expressions.
 */
static void i_expr(p)
node *p;
{
  token *t;
  switch (p->type) {
  case N_Token:
  case N_compound:
  case N_lhs1:
  case N_rw1:
    break;
  case N_lhs2:    /* [ lhs_or_dash_list ] */
    pushscope(loopframe_scope);  /* in case it's needed */
    i_lhs_or_dash_list(sub0(p));
    popscope(loopframe_scope);
    break;
  case N_rw2:     /* [ rw_list ] */
    pushscope(loopframe_scope);  /* in case it's needed */
    i_rw_list(sub0(p));
    popscope(loopframe_scope);
    break;
  case N_tuple1:  /* [ expr_list ] */
  case N_set1:    /* { expr_list } */
    pushscope(loopframe_scope);  /* in case it's needed */
    i_expr_list(sub0(p));
    popscope(loopframe_scope);
    break;
  case N_lhs3:    /* lhs_or_call selector  */
                  /* lhs_or_call ( ) */
                  /* lhs selector */
  case N_rw3:     /* rw selector */
  case N_applic:  /* operand selector */
                  /* operand ( ) */
    i_expr(sub0(p));
    if (sub1(p) != (node *)NULL) i_selector(sub1(p));
    break;
  case N_syscall: /* Sysproc ( expr_list [(*)] ) */
                  /* Sysproc ( ) */
                  /* Sysproc */
    i_expr_list(sub1(p));
    break;
  case N_tuple2:  /* [ former ] */
  case N_set2:    /* { former } */
    pushscope(loopframe_scope);  /* in case it's needed */
    i_former(sub0(p));
    popscope(loopframe_scope);
    break;
  case N_assignment:
    i_expr(sub0(p));
    i_expr(sub2(p));
    break;
  case N_binary:
    t = (token *)sub1(p);  assert (t->type == N_Token);
    if (leq(t->graphic,"AND") ||
        leq(t->graphic,"OR")  ||
        leq(t->graphic,"?")) {
      pushscope(if_expr_scope);
      i_expr(sub0(p));
      i_expr(sub2(p));
      popscope(if_expr_scope);
    } else {
      i_expr(sub0(p));
      i_expr(sub2(p));
    }
    break;
  case N_bincomb:
    i_expr(sub0(p));
    i_expr(sub2(p));
    break;
  case N_unary:
  case N_uncomb:
    i_expr(sub1(p));
    break;
  case N_expr:  /* parenthesized expression */
    i_expr(sub0(p));
    break;
  case N_case_of_expr:  /* CASE OF choices ELSE expr END */
    pushscope(case_of_expr_scope);
    i_choices(sub0(p));
    i_expr(sub1(p));
    popscope(case_of_expr_scope);
    break;
  case N_case_ex_expr:  /* CASE expr OF choices ELSE expr END */
    pushscope(case_ex_expr_scope);
    i_expr(sub0(p));
    i_choices(sub1(p));
    i_expr(sub2(p));
    popscope(case_ex_expr_scope);
    break;
  case N_if_expr:  /* IF expr THEN expr opt_elif_exprs ELSE expr END */
    pushscope(if_expr_scope);
    i_expr(sub0(p));
    i_expr(sub1(p));
    i_elif_exprs(sub2(p));
    i_expr(sub3(p));
    popscope(if_expr_scope);
    break;
  case N_expr_clause:
    pushscope(expr_clause_scope);
    i_stmts(sub0(p));
    popscope(expr_clause_scope);
    break;
  case N_exists:     /* EXISTS simpliter_list | expr */
  case N_notexists:  /* NOTEXISTS simpliter_list | expr */
  case N_forall:     /* FORALL simpliter_list | expr */
    pushscope(loopframe_scope);
    i_simpliter_list(sub0(p));
    i_expr(sub1(p));
    popscope(loopframe_scope);
    break;
  case N_procval:    /* PROC compound */
                     /* '(' PROC ')' compound */
                     /* ROUTINE compound */
                     /* '(' ROUTINE ')' compound */
  case N_cmdval:     /* COMMAND Command_name */
    break;
  case N_indirect_call:   /* CALL ( expr [, expr_list [(*)] ] ) */
    i_expr(sub0(p));
    i_expr_list(sub1(p));
    break;
  case N_command:
    i_command(p);
    break;
  default:  unexpected(p->type);
  }
} /* end i_expr */

/* .................................................................. */

static void i_command(p)
node *p;
{
  switch (p->type) {
  case N_command:
    i_cmdargs(sub1(p));
    break;
  default:  unexpected(p->type);
  }
}

/* .................................................................. */

static void i_cmdargs(p)
node *p;
{
  int i;
  switch (p->type) {
  case N_cmdargs:
    for (i=0; i<p->nsub; i++) i_cmdarg(subi(p,i));
    break;
  default:  unexpected(p->type);
  }
}

/* .................................................................. */

static void i_cmdarg(p)
node *p;
{
  switch (p->type) {
  case N_Token:
  case N_compound:
    break;
  case N_expr:
    i_expr(sub0(p));
    break;
  case N_tuple1:
  case N_set1:
    i_expr_list(sub0(p));
    break;
  case N_tuple2:
  case N_set2:
    pushscope(loopframe_scope);  /* in case it's needed */
    i_former(sub0(p));
    popscope(loopframe_scope);
    break;
  default:  unexpected(p->type);
  }
}

/* .................................................................. */

static void i_choices(p)
node *p;
{
  int i;
  switch (p->type) {
  case N_choices:
    for (i=0; i<p->nsub; i++) i_choice(subi(p,i));
    break;
  default:  unexpected(p->type);
  }
}

/* .................................................................. */

static void i_choice(p)
node *p;
{
  switch (p->type) {
  case N_choice:  /* ( expr_list ) : expr */
    i_expr_list(sub0(p));
    i_expr(sub1(p));
    break;
  default:  unexpected(p->type);
  }
}

/* .................................................................. */

static void i_elif_exprs(p)
node *p;
{
  int i;
  switch (p->type) {
  case N_elif_exprs:
    for (i=0; i<p->nsub; i++) i_elif_expr(subi(p,i));
    break;
  default:  unexpected(p->type);
  }
}

/* .................................................................. */

static void i_elif_expr(p)
node *p;
{
  switch (p->type) {
  case N_elif_expr:  /* ELSEIF expr THEN expr */
    i_expr(sub0(p));
    i_expr(sub1(p));
    break;
  default:  unexpected(p->type);
  }
}

/* .................................................................. */

static void i_selector(p)
node *p;
{
  switch (p->type) {
  case N_selector1:  /* ( expr_list [(*)] ) */
  case N_selector2:  /* { expr_list } */
    i_expr_list(sub0(p));
    break;
  case N_selector3:  /* ( opt_expr Dots opt_expr ) */
    if (sub0(p) != (node *)NULL) i_expr(sub0(p));
    if (sub1(p) != (node *)NULL) i_expr(sub1(p));
    break;
  case N_selector4:  /* . nameseq */
    break;
  default:  unexpected(p->type);
  }
}

/* .................................................................. */

static void i_former(p)
node *p;
{
  switch (p->type) {
  case N_former1:  /* expr :              loop_head */
                   /* expr :     iterator loop_head */
                   /* expr : FOR iterator loop_head */
    i_expr(sub0(p));
    if (sub1(p) != (node *)NULL) i_iterator(sub1(p));
    i_loop_head(sub2(p));
    break;
  case N_former2:  /* expr_list .. expr */
    i_expr_list(sub0(p));
    i_expr(sub1(p));
    break;
  case N_former3:  /* operand IN expr | expr */
    i_expr(sub0(p));
    i_expr(sub1(p));
    i_expr(sub2(p));
    break;
  default:  unexpected(p->type);
  }
} /* end i_former */
