/*  ===  Virtual Code Generation  =================================== */

/*  $Id: gencode.c,v 1.33 2021/03/02 21:00:13 setlorg Exp $  */

/*  Free software (c) dB - see file COPYING for license (GPL).  */

/*
 *  This pass builds a chain of basic blocks for each routine.
 * 
 *  Each basic block is a sequence of instructions.
 * 
 *  Each instruction is an opcode and zero or more operands.
 * 
 *  Some minor optimizations are also done here (but see 'optimize').
 */

/*
 *  Implementation note:  calls to 'initscope', 'pushscope',
 *  'popscope', and 'endscope' must be kept in synchrony between
 *  this pass and 'identify' and 'canonize'.  Some kind of scope
 *  numbers in the parse tree might have furnished a less hazardous
 *  approach.
 */

/* ------------------------------------------------------------------ */

#include "setltran.h"
#include "y.tab.h"

enum former_enum {
  tuple_former,
  set_former
};
typedef enum former_enum former;


/* Local routines */
static void g_setl_program(node *p);
static void g_routines(node *p);
static void g_routine(node *p);
static void g_body(node *p);
static void g_decls(node *p);
static void g_decl(node *p);
static void g_vars(node *p);
static void g_consts(node *p);
static void g_inits(node *p);
static void g_sels(node *p);
static void g_refinements(node *p);
static void g_refinement(node *p);
static void g_stmts(node *p);
static void g_stmt(node *p);
static void g_elifs(node *p);
static void g_elif(node *p,int i);
static void g_when_stmts(node *p);
static void g_when_stmt(node *p);

static const char *eval(node *p);
static const char *get_lhs(node *p);
static void eval_into(const char *e, node *p);
static void eval_decl_rhs_into(const char *e, node *p);
static void special_former_into(const char *e, node *q, former type);
static void former_into(const char *e, node *p, former type);
static void former1_into(const char *e, node *p, node *q, node *r, former type);
static void former2_into(const char *e, node *p, node *q, node *r, former type);
static void former3_into(const char *e, node *p, node *q, node *r, former type);

static void generalized_iterator(node *it, node *head);
static void iterator(node *p);
static void simpliter(node *p, int i);

static void void_call(symbol *y, node *p, bool vactual);
static void call(const char *r, symbol *y, node *p, bool vactual);
static void indirect_call(const char *r, node *p);
static void special_call(const char *s);

static void becomes(node *to, node *from);
static void assigning(node *to, token *op, node *from);
static void assign(node *p, const char *x);

static void get_selected(const char *e, const char *x, node *p);
static void update_selected(const char *e, node *p, const char *x);
static void upchuck(const char *e, node *q, const char *x, int i);
static const char *tup_sel_if_nec(node *p);

static void init_virtcode(void);
static void newcsect(void);
static void newblock(void);
static void endblock(void);
static void newlabel(void);
static void newinstr(void);
static void newopera(void);
static void v_label(const char *s);
static void v_label_ctrl(const char *stem);
static void v_label_ctrl_sub(const char *stem, int sub);
static void v_copy(const char *from, const char *to);
static void v_copy_indexed(const char *from, int i, const char *to);
static void v_routine(const char *rot, const char *to);
static void v_call(const char *rot);
static void v_vcall(const char *rot);
static void v_call_assigning(const char *rot);
static void v_call_bincomb(const char *rot);
static void v_call_uncomb(const char *rot);
static void v_end(const char *rot);
static void v_opcode(optype op);
static void v_opnd(operatype type, bits acc, const char *libretto);
static void v_opnd_name(bits acc, const char *name);
static void v_opnd_lit(const char *s);
static void v_opnd_reg(bits acc, const char *s);
static void v_opnd_label(const char *s);
static void v_opnd_unsigned(int n);
static void v_opnd_ctrl_scope(const char *stem, int scopenum);
static void v_opnd_ctrl(const char *stem);
static void v_opnd_ctrl_sub(const char *stem, int sub);
static void v_tn(node *p);
static void v_etn(node *p);
static void fix_virtcode(void);

static const char *full_name(symbol *y);
static void symdcls(symtab t);
static node *split(node *p, int k);
static void embed_c_code(const char *s);
static void embed_c_include(const char *s);
static void progtop(const char *s);
static void proctop(const char *s);
static void vproctop(const char *s);
static void tempinit(void);
static const char *temp_name(void);
static const char *user_name(const char *s);
static const char *system_name(const char *s);
static const char *dummy_name(void);
static const char *return_reg(void);
static const char *status_reg(void);
static const char *machine_reg(void);
static const char *user_label(const char *s);
static const char *refinement_label(const char *s);
static const char *refret_label(const char *s);
static const char *control_label(const char *s);
static const char *return_label(void);
static const char *integer_lit(const char *s);
static const char *real_lit(const char *s);
static const char *string_lit(const char *s);
static const char *true_lit(void);
static const char *false_lit(void);
static const char *om_lit(void);
static const char *null_tuple_lit(void);
static const char *empty_set_lit(void);

static void push_case_ex_reg(const char *reg);
static const char *cur_case_ex_reg(void);
static void pop_case_ex_reg(void);

static void push_yield_reg(const char *reg);
static const char *cur_yield_reg(void);
static void pop_yield_reg(void);

static void push_loop_tn_scope(int tn);
static int find_loop_tn_scope(int tn);
static void pop_loop_tn_scope(void);


/* ------------------------------------------------------------------ */

/* Local data */

#include "symstack.h"

/* Stack of names of temporaries for "CASE expr OF" statements: */
struct case_ex_reg {
  const char *reg;
  struct case_ex_reg *prev;
};
static struct case_ex_reg *case_ex_reg_stack;

/* Stack of temporaries' names for "EXPR ... YIELD" pairs: */
struct yield_reg {
  const char *reg;
  struct yield_reg *prev;
};
static struct yield_reg *yield_reg_stack;

/* Stack of loop scopes, giving starting token numbers: */
struct loop_tn_scope {
  int tn;
  int scopenum;
  struct loop_tn_scope *prev;
};
static struct loop_tn_scope *loop_tn_scope_stack;

static int curtemp;             /* counter of temporary variables */


/* ------------------------------------------------------------------ */

void gencode(void)
{
  g_setl_program(root);
}

/* .................................................................. */

static void g_setl_program(p)
node *p;
{
  int i;
  token *t,*u;
  node *q,*r;
  symbol *v,*x,*y,*z;
  symbol *sysrottab_holder;
  const char *rot,*progrot;
  glowball = (symbol *)NULL;
  inner = (symstack *)NULL;  /* initially empty stack of symtabs */
  sysrottab_holder = new_symtabholder(sysrottab);
  pushsymtab(sysrottab_holder);  /* push sysrot table onto symstack */
  case_ex_reg_stack = (struct case_ex_reg *)NULL;
  yield_reg_stack = (struct yield_reg *)NULL;
  loop_tn_scope_stack = (struct loop_tn_scope *)NULL;
  init_virtcode();
  v_tn(p);
  if (p) {
    switch (p->type) {
    case N_unnamed_program:
      y = symfind(privatetab,"_unnamed_SETL_program");
      assert (y->type == progsym);
      x = symfind(y->stab,"_MAIN");
      assert (x->type == procsym);
      symdcls(y->stab);  /* globals private to the program */
      progrot = user_name("_MAIN");
      progtop(progrot);  /* mainproc U__MAIN */
      rot = user_name("_unnamed_SETL_program");
      special_call(rot);
      v_copy(integer_lit("0"),status_reg());
      v_opcode(stop_op);
      v_opnd_reg(rd,status_reg());
      endblock();  /* stop_op ends a basic block */
      v_end(progrot);  /* end U__MAIN */
      proctop(rot);  /* proc U__unnamed_SETL_program */
      v_opnd_reg(wr,return_reg());
      /*
       *  Note that although we pinned the locals of the program
       *  on _MAIN in the privatetab, they really belong to the
       *  unnamed program, so their decls are emitted here:
       */
      symdcls(x->stab);  /* locals private to the program */
      pushsymtab(y);  /* program unit-global symstack level */
      q = sub0(p); assert (q->type == N_body);
      initscope(proc_scope);  /* decls and code coming */
      glowball = x;  /* for tags in rhs of global decls */
      g_decls(sub0(q));  /* program globals */
      glowball = (symbol *)NULL;
      pushsymtab(x);  /* mainproc on behalf of program unit */
      pushscope(stmts_scope);  /* stmts coming */
      g_stmts(sub1(q));
      v_copy(integer_lit("0"),status_reg());
      v_opcode(stop_op);
      v_opnd_reg(rd,status_reg());
      endblock();  /* stop_op ends a basic block */
      popscope(stmts_scope);  /* back to decls and code */
      g_refinements(sub2(q));
      endscope(proc_scope);  /* back to no control scope */
      assert (curholder == x);  /* still at mainproc level */
      popsymtab();  /* from mainproc back to program unit level */
      v_end(rot);  /* end U__unnamed_SETL_program */
      g_routines(sub1(p));  /* PROC/OP/COMMAND defns */
      assert (curholder == y);  /* at program unit level */
      popsymtab();  /* from program unit back to outer level */
      x = (symbol *)NULL;
      y = (symbol *)NULL;
      break;
    case N_complex_program:
      /*
       *  Treatment of symstack here is like in canonize.c
       */
      p = sub0(p);  /* sigh */
      assert (p->type == N_compilation_units);
      /*
       *  Global declarations first.  Note that package bodies will
       *  repeat declarations made in package specs, but the run-time
       *  program can tolerate redundant declarations.
       */
      rot = NULL;  /* assure the C compiler that rot is initted */
      for (i=0; i<p->nsub; i++) {
        q = subi(p,i);
        switch (q->type) {
        case N_package_spec:
          t = name_token(sub0(q));
          y = symfind(publictab,t->graphic);
          assert (y->type == packsym);
          symdcls(y->stab);  /* public globals */
          y = (symbol *)NULL;
          break;
        case N_package_body:
          t = name_token(sub0(q));
          y = symfind(privatetab,t->graphic);
          assert (y->type == packsym);
          symdcls(y->stab);  /* globals private to the package */
          y = (symbol *)NULL;
          break;
        case N_program_unit:
          t = name_token(sub0(q));
          y = symfind(privatetab,t->graphic);
          assert (y->type == progsym);
          rot = user_name(t->graphic);  /* program unit name */
          symdcls(y->stab);  /* globals private to the program unit */
          y = (symbol *)NULL;
          break;
        default:  unexpected(q->type);
        }
      }
      progrot = user_name("_MAIN");
      progtop(progrot);  /* mainproc U__MAIN */
      /*
       *  Calls to initialization routines, first public, then private.
       */
      for (i=0; i<p->nsub; i++) {
        q = subi(p,i);
        if (q->type == N_package_spec) {
          t = name_token(sub0(q));
          special_call(user_name(strjoin("_PUBINIT_",t->graphic)));
        }
      }
      for (i=0; i<p->nsub; i++) {
        q = subi(p,i);
        if (q->type == N_package_body) {
          t = name_token(sub0(q));
          special_call(user_name(strjoin("_PRIVINIT_",t->graphic)));
        }
      }
      special_call(rot);  /* call to the proc named after the pgm unit */
      v_copy(integer_lit("0"),status_reg());
      v_opcode(stop_op);  /* a second chance to halt? */
      v_opnd_reg(rd,status_reg());
      endblock();  /* stop_op ends a basic block */
      v_end(progrot);  /* end main pgm */
      /*
       *  Initialization routines for specs, and user-defined routines
       *  for package and program units...
       */
      for (i=0; i<p->nsub; i++) {
        q = subi(p,i);
        v_tn(q);
        switch (q->type) {
        case N_package_spec:
          t = name_token(sub0(q));  /* current package name */
          paxinuse = (symtab)NULL;  /* defensively avoid leftovers */
          /*
           *  Same deal with the one-entry symtab here as in canonize.c
           */
          z = new_symtabholder(new_symtab());
          symdef (z, t, packsym, 0);  /* put pkg name from t into z */
          y = symfind(publictab,t->graphic);
          assert (y->type == packsym);
          rot = user_name(strjoin("_PUBINIT_",t->graphic));
          proctop(rot);  /* begin pkg spec init */
          v_opnd_reg(wr,return_reg());
          x = symfind(y->stab,strjoin("_PUBINIT_",t->graphic));
          assert (x->type == procsym);
          symdcls(x->stab);  /* locals of the global initializers */
          pushsymtab(z);  /* the level with just the package name (b) */
          pushsymtab(y);  /* package-global level (c) */
          r = sub1(q);
          if (r != (node *)NULL) {
            /*
             *  Although we are in the scope of an init proc here, it
             *  is intended that the C code here, which was right after
             *  "PACKAGE name;" in the SETL source, be placed at
             *  file scope by any back end that processes our output.
             *
             *  That stands in contrast to C code in the context of
             *  executable statements in the SETL source (see g_stmt()),
             *  which is intended for some kind of incorporation in a
             *  stream of emitted executable C code by that back end.
             *
             *  It seems unlikely that such an undisciplined interface
             *  to C code will or should ever be wanted.
             *
             *  Note that there will be no N_embedded_c_code nodes in
             *  what this code generator sees unless EMBEDDED_C is
             *  #defined in tokenize.c to enable recognition of the
             *  escape sequence (\c...\s) for such embeds.
             */
            assert (r->type == N_embedded_c_code);
            u = c_code_token(sub0(r));
            v_etn(r);
            embed_c_include(u->graphic);
          }
          initscope(proc_scope);  /* decls and code coming */
          glowball = x;  /* for tags in rhs of global decls */
          g_decls(sub2(q));  /* public globals */
          glowball = (symbol *)NULL;
          v_opcode(return_op);
          endblock();  /* return_op ends a basic block */
          endscope(proc_scope);  /* back to no control scope */
          v_end(rot);  /* end pkg spec init */ 
          assert (curholder == y);  /* at package level (c) */
          popsymtab();  /* from package back to package name level (b) */
          assert (curholder == z);  /* at package name level (b) */
          popsymtab();  /* back to outer level (a, sysrots) */
          x = (symbol *)NULL;
          y = (symbol *)NULL;
          z = (symbol *)NULL;
          break;
        case N_package_body:
          t = name_token(sub0(q));  /* current package name */
          v = symfind(usetabs,t->graphic);
          assert (v->type == packsym);
          paxinuse = v->stab;
          v = (symbol *)NULL;
          z = symfind(unittabs,t->graphic);
          assert (z->type == packsym);
          y = symfind(privatetab,t->graphic);
          assert (y->type == packsym);
          rot = user_name(strjoin("_PRIVINIT_",t->graphic));
          proctop(rot);  /* begin pkg body init */
          v_opnd_reg(wr,return_reg());
          x = symfind(y->stab,strjoin("_PRIVINIT_",t->graphic));
          assert (x->type == procsym);
          symdcls(x->stab);  /* locals of the global initializers */
          pushsymtab(z);  /* t and USE-imported symbols (level b) */
          pushsymtab(y);  /* package-global level (c) */
          r = sub1(q);
          if (r != (node *)NULL) {
            /* as for the N_package_spec case above */
            assert (r->type == N_embedded_c_code);
            u = c_code_token(sub0(r));
            v_etn(r);
            embed_c_include(u->graphic);
          }
          initscope(proc_scope);  /* decls and code coming */
          glowball = x;  /* for tags in rhs of global decls */
          g_decls(sub3(q));  /* globals private to the package */
          glowball = (symbol *)NULL;
          v_opcode(return_op);
          endblock();  /* return_op ends a basic block */
          endscope(proc_scope);  /* back to no control scope */
          v_end(rot);  /* end pkg body init */
          g_routines(sub4(q));  /* PROC/OP/COMMAND defns */
          assert (curholder == y);  /* at package level (c) */
          popsymtab();  /* from package back to package name level (b) */
          assert (curholder == z);  /* at package name level (b) */
          popsymtab();  /* back to outer level (a, sysrots) */
          x = (symbol *)NULL;
          y = (symbol *)NULL;
          z = (symbol *)NULL;
          break;
        case N_program_unit:
          t = name_token(sub0(q));  /* program unit name */
          v = symfind(usetabs,t->graphic);
          assert (v->type == progsym);
          paxinuse = v->stab;
          v = (symbol *)NULL;
          z = symfind(unittabs,t->graphic);
          assert (z->type == progsym);
          y = symfind(privatetab,t->graphic);
          assert (y->type == progsym);
          rot = user_name(t->graphic);
          proctop(rot);  /* proc U_... (named after program unit) */
          v_opnd_reg(wr,return_reg());
          x = symfind(y->stab,"_MAIN");
          assert (x->type == procsym);
          /*
           *  Note that although we pinned the locals of the program
           *  unit on _MAIN in the privatetab, they really belong to
           *  the program, and their decls are accordingly emitted here
           *  within the proc named after the program unit:
           */
          symdcls(x->stab);  /* locals private to the program unit */
          pushsymtab(z);  /* t and USE-imported symbols (level b) */
          pushsymtab(y);  /* program unit-global level (c) */
          r = sub2(q); assert (r->type == N_body);
          initscope(proc_scope);  /* decls and code coming */
          glowball = x;  /* for tags in rhs of global decls */
          g_decls(sub0(r));  /* globals private to the program unit */
          glowball = (symbol *)NULL;
          pushsymtab(x);  /* mainproc on behalf of pgm unit (level d) */
          pushscope(stmts_scope);  /* stmts coming */
          g_stmts(sub1(r));
          v_copy(integer_lit("0"),status_reg());
          v_opcode(stop_op);
          v_opnd_reg(rd,status_reg());
          endblock();  /* stop_op ends a basic block */
          popscope(stmts_scope);  /* back to decls and code */
          g_refinements(sub2(r));
          endscope(proc_scope);  /* back to no control scope */
          assert (curholder == x);  /* still at mainproc level (d) */
          popsymtab();  /* back to program unit level (c) */
          v_end(rot);  /* end named pgm */
          g_routines(sub3(q));  /* PROC/OP/COMMAND defns */
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
    progrot = user_name("_MAIN");
    progtop(progrot);
    v_copy(integer_lit("0"),status_reg());
    v_opcode(stop_op);
    v_opnd_reg(rd,status_reg());
    endblock();  /* stop_op ends a basic block */
    v_end(progrot);
    x = (symbol *)NULL;
    y = (symbol *)NULL;
  }
  fix_virtcode();
  assert (case_ex_reg_stack == (struct case_ex_reg *)NULL);
  assert (yield_reg_stack == (struct yield_reg *)NULL);
  assert (loop_tn_scope_stack == (struct loop_tn_scope *)NULL);
  assert (curholder == sysrottab_holder);
  assert (cursymtab == sysrottab);
  popsymtab();
  assert (inner == (symstack *)NULL);
} /* end g_setl_program */

/* .................................................................. */

static void g_routines(p)
node *p;
{
  int i;
  switch (p->type) {
  case N_routines:
    for (i=0; i<p->nsub; i++) g_routine(subi(p,i));
    break;
  default:  unexpected(p->type);
  }
}

/* .................................................................. */

static void g_routine(p)
node *p;
{
  int i;
  token *t,*u;
  node *q,*r;
  symbol *y;
  const char *rot;  /* name of routine */
  const char *s;
  initscope(proc_scope);
  v_tn(p);
  switch (p->type) {
  case N_procdef:
  case N_cmddef:
    if (p->type == N_procdef) t = name_token(sub0(p));
    else                      t = cmdname_token(sub0(p));
    y = symfind(cursymtab,t->graphic);
    if (p->type == N_procdef) assert (y->type == procsym);
    else                      assert (y->type == cmdsym);
    q = sub1(p);
    assert (q->type == N_form_specs);
    rot = full_name(y);
    if (sub2(p) == NULL) proctop(rot);
    else                vproctop(rot);
    pushsymtab(y);
    for (i=0; i<q->nsub; i++) {
      r = subi(q,i);
      v_tn(r);
      u = name_token(sub0(r));
      s = user_name(u->graphic);
      switch (r->type) {
      case N_rd_spec:
        assert ((y->parms[i] & rwable) == rdable);
        v_opnd_name(rd,s);
        break;
      case N_wr_spec:
        assert ((y->parms[i] & rwable) == wrable);
        v_opnd_name(wr,s);
        break;
      case N_rw_spec:
        assert ((y->parms[i] & rwable) == rwable);
        v_opnd_name(rw,s);
        break;
      default:  unexpected(r->type);
      }
    }
    v_opnd_reg(wr,return_reg());
    v_etn(sub2(p));  /* opt_star */
    symdcls(cursymtab);
    g_body(sub3(p));
    v_opcode(return_op);
    endblock();  /* return_op ends a basic block */
    popsymtab();
    v_end(rot);
    break;
  case N_binopdef:
    t = bopname_token(sub0(p));
    y = symfind(cursymtab,t->graphic);
    assert (y->type == bopsym);
    rot = full_name(y);
    proctop(rot);
    pushsymtab(y);
    for (i=0; i<=1; i++) {
      r = subi(p,i+1);
      v_tn(r);
      u = name_token(sub0(r));
      s = user_name(u->graphic);
      switch (r->type) {
      case N_rd_spec:
        assert ((y->parms[i] & rwable) == rdable);
        v_opnd_name(rd,s);
        break;
      case N_wr_spec:
        assert ((y->parms[i] & rwable) == wrable);
        v_opnd_name(wr,s);
        break;
      case N_rw_spec:
        assert ((y->parms[i] & rwable) == rwable);
        v_opnd_name(rw,s);
        break;
      default:  unexpected(r->type);
      }
      v_etn(r);
    }
    v_opnd_reg(wr,return_reg());
    symdcls(cursymtab);
    g_body(sub3(p));
    v_opcode(return_op);
    endblock();  /* return_op ends a basic block */
    popsymtab();
    v_end(rot);
    break;
  case N_unopdef:
    t = uopname_token(sub0(p));
    y = symfind(cursymtab,t->graphic);
    assert (y->type == uopsym);
    rot = full_name(y);
    proctop(rot);
    pushsymtab(y);
    i = 0;
    r = subi(p,i+1);
    v_tn(r);
    u = name_token(sub0(r));
    s = user_name(u->graphic);
    switch (r->type) {
    case N_rd_spec:
      assert ((y->parms[i] & rwable) == rdable);
      v_opnd_name(rd,s);
      break;
    case N_wr_spec:
      assert ((y->parms[i] & rwable) == wrable);
      v_opnd_name(wr,s);
      break;
    case N_rw_spec:
      assert ((y->parms[i] & rwable) == rwable);
      v_opnd_name(rw,s);
      break;
    default:  unexpected(r->type);
    }
    v_etn(r);
    v_opnd_reg(wr,return_reg());
    symdcls(cursymtab);
    g_body(sub2(p));
    v_opcode(return_op);
    endblock();  /* return_op ends a basic block */
    popsymtab();
    v_end(rot);
    break;
  default:  unexpected(p->type);
  }
  v_etn(p);
  endscope(proc_scope);
} /* end g_routine */

/* .................................................................. */

static void g_body(p)
node *p;
{
  v_tn(p);
  switch (p->type) {
  case N_body:
    g_decls(sub0(p));
    pushscope(stmts_scope);
    g_stmts(sub1(p));
    v_copy(om_lit(),return_reg());
    v_opcode(jmp_op); v_opnd_label(return_label());
    popscope(stmts_scope);
    g_refinements(sub2(p));
    v_label(return_label());
    break;
  default:  unexpected(p->type);
  }
  v_etn(p);
}
    
/* .................................................................. */

static void g_decls(p)
node *p;
{
  int i;
  pushscope(decls_scope);
  switch (p->type) {
  case N_decls:
    for (i=0; i<p->nsub; i++) g_decl(subi(p,i));
    break;
  default:  unexpected(p->type);
  }
  popscope(decls_scope);
}

/* .................................................................. */

static void g_decl(p)
node *p;
{
  v_tn(p);
  switch (p->type) {
  case N_var_decl:
    g_vars(sub0(p));
    break;
  case N_const_decl:
    g_consts(sub0(p));
    break;
  case N_init_decl:
    g_inits(sub0(p));
    break;
  case N_sel_decl:
    g_sels(sub0(p));
    break;
  case N_repr_decl:
    break;
  default:  unexpected(p->type);
  }
  v_etn(p);
}

/* .................................................................. */

static void g_vars(p)
node *p;
{
  int i;
  node *q;
  token *t;
  symbol *y;
  v_tn(p);
  switch (p->type) {
  case N_var_list:
    for (i=0; i<p->nsub; i++) {
      q = subi(p,i);
      v_tn(q);
      assert (q->type == N_var);
      t = name_token(sub0(q));
      y = symfind(cursymtab,t->graphic);
      assert (y->type == varsym || y->type == initsym);
      if (sub1(q) != (node *)NULL) {
        eval_decl_rhs_into(full_name(y),sub1(q));
      }
    }
    break;
  default:  unexpected(p->type);
  }
  v_etn(p);
}

/* .................................................................. */

static void g_consts(p)
node *p;
{
  int i;
  node *q;
  token *t;
  symbol *y;
  v_tn(p);
  switch (p->type) {
  case N_ascription_list:
    for (i=0; i<p->nsub; i++) {
      q = subi(p,i);
      v_tn(q);
      assert (q->type == N_ascription);
      t = name_token(sub0(q));
      y = symfind(cursymtab,t->graphic);
      assert (y->type == constsym);
      if (sub1(q) == (node *)NULL) {
        /* Silly feature! */
        token u;
        u.type = N_Token;
        u.tn = -1;
        u.srcloc = 0;
        u.srcend = 0;
        u.graphic = strjoin3("\'",strmakeupper(t->graphic),"\'");
        u.code = String;
        eval_into(full_name(y),(node *)(void *)&u);
      } else {
        eval_decl_rhs_into(full_name(y),sub1(q));
      }
    }
    break;
  default:  unexpected(p->type);
  }
  v_etn(p);
}

/* .................................................................. */

static void g_inits(p)
node *p;
{
  int i;
  node *q;
  token *t;
  symbol *y;
  v_tn(p);
  switch (p->type) {
  case N_initial_list:
    for (i=0; i<p->nsub; i++) {
      q = subi(p,i);
      v_tn(q);
      assert (q->type == N_initial);
      t = name_token(sub0(q));
      y = symfind(cursymtab,t->graphic);
      assert (y->type == varsym || y->type == initsym);
      eval_decl_rhs_into(full_name(y),sub1(q));
    }
    break;
  default:  unexpected(p->type);
  }
  v_etn(p);
}

/* .................................................................. */

static void g_sels(p)
node *p;
{
  int i;
  node *q;
  token *t;
  symbol *y;
  v_tn(p);
  switch (p->type) {
  case N_sel_list:
    for (i=0; i<p->nsub; i++) {
      q = subi(p,i);
      v_tn(q);
      assert (q->type == N_sel);
      t = name_token(sub0(q));
      y = symfind(cursymtab,t->graphic);
      assert (y->type == selsym);
      eval_decl_rhs_into(full_name(y),sub1(q));
    }
    break;
  default:  unexpected(p->type);
  }
  v_etn(p);
}

/* .................................................................. */

static void g_refinements(p)
node *p;
{
  int i;
  switch (p->type) {
  case N_refinements:
    for (i=0; i<p->nsub; i++) g_refinement(subi(p,i));
    break;
  default:  unexpected(p->type);
  }
}

/* .................................................................. */

static void g_refinement(p)
node *p;
{
  token *t;
  v_tn(p);
  switch (p->type) {
  case N_refinement:
    t = refname_token(sub0(p));
    v_label(refinement_label(t->graphic));
    pushscope(refinement_scope);
    g_stmts(sub1(p));
    v_etn(p);
    v_opcode(jmp_op); v_opnd_label(refret_label(t->graphic));
    popscope(refinement_scope);
    break;
  default:  unexpected(p->type);
  }
  v_etn(p);
}

/* .................................................................. */

static void g_stmts(p)
node *p;
{
  int i;
  switch (p->type) {
  case N_stmts:
    for (i=0; i<p->nsub; i++) g_stmt(subi(p,i));
    break;
  default:  unexpected(p->type);
  }
}

/* .................................................................. */

static void g_stmt(p)
node *p;
{
  int i,k;
  token *t;
  node *q,*r,*s;
  symbol *y;
  const char *e,*e1,*e2;
  v_tn(p);
  switch (p->type) {
  case N_labelled_stmt:
    v_etn(sub0(p));
    t = name_token(sub0(p));
    v_label(user_label(t->graphic));
    g_stmt(sub1(p));
    break;
  case N_assert_stmt:
    e = eval(sub0(p));
    v_etn(p);
    v_opcode(assert_op);
    v_opnd_name(rd,e);
    break;
  case N_continue_stmt:
    v_etn(p);
    t = (token *)sub0(p); assert (t->type == N_Token);
    v_opcode(jmp_op); v_opnd_ctrl_scope("start",find_loop_tn_scope(t->tn));
    break;
  case N_quit_stmt:
    v_etn(p);
    t = (token *)sub0(p); assert (t->type == N_Token);
    v_opcode(jmp_op); v_opnd_ctrl_scope("term",find_loop_tn_scope(t->tn));
    break;
  case N_fail_stmt:
    v_etn(p);
    v_opcode(fail_op);
    endblock();  /* fail_op ends a basic block */
    break;
  case N_pass_stmt:
    break;
  case N_stop_stmt:
    /* STOP now takes an optional arg that defaults to 0. */
    if (sub0(p) != (node *)NULL) {
      eval_into(status_reg(),sub0(p));
    } else {
      v_etn(p);
      v_copy(integer_lit("0"),status_reg());
    }
    v_opcode(stop_op);
    v_opnd_reg(rd,status_reg());
    endblock();  /* stop_op ends a basic block */
    break;
  case N_succeed_stmt:
    v_etn(p);
    v_opcode(succeed_op);
    break;
  case N_goto_stmt:
    v_etn(p);
    t = name_token(sub0(p));
    v_opcode(jmp_op); v_opnd_label(user_label(t->graphic));
    break;
  case N_return_stmt:
    if (sub0(p) != (node *)NULL) {
      eval_into(return_reg(),sub0(p));
    } else {
      v_etn(p);
      v_copy(om_lit(),return_reg());
    }
    v_opcode(jmp_op); v_opnd_label(return_label());
    break;
  case N_yield_stmt:
    eval_into(cur_yield_reg(),sub0(p));
    /* Surely this would be another place a basic block always ends? */
    break;
  case N_if_stmt:
    pushscope(if_scope);
    e = eval(sub0(p));    /* test expression */
    v_opcode(jmpfalse_op);
    v_opnd_name(rd,e);
    v_opnd_ctrl("elifs");
    g_stmts(sub1(p));             /* then-part */
    v_opcode(jmp_op); v_opnd_ctrl("endif");
    v_label_ctrl("elifs");
    g_elifs(sub2(p));             /* opt-elifs */
    g_stmts(sub3(p));             /* else-part */
    v_etn(p);
    v_label_ctrl("endif");
    popscope(if_scope);
    break;
  case N_case_of_stmt:
    pushscope(case_of_scope);
    v_opcode(jmp_op); v_opnd_ctrl_sub("test_of",curscope->subscope);
    g_stmts(sub0(p));             /* choice-parts */
    v_opcode(jmp_op); v_opnd_ctrl("endcase_of");
    v_label_ctrl_sub("test_of",curscope->subscope);
    g_stmts(sub1(p));             /* else-part */
    v_etn(p);
    v_label_ctrl("endcase_of");
    popscope(case_of_scope);
    break;
  case N_case_ex_stmt:
    pushscope(case_ex_scope);
    e = eval(sub0(p));    /* test expression */
    push_case_ex_reg(e);
    v_opcode(jmp_op); v_opnd_ctrl_sub("test_ex",curscope->subscope);
    g_stmts(sub1(p));             /* choice-parts */
    v_opcode(jmp_op); v_opnd_ctrl("endcase_ex");
    v_label_ctrl_sub("test_ex",curscope->subscope);
    g_stmts(sub2(p));             /* else-part */
    v_etn(p);
    v_label_ctrl("endcase_ex");
    pop_case_ex_reg();
    popscope(case_ex_scope);
    break;
  case N_case_of_when_stmt:
    pushscope(case_of_scope);
    v_opcode(jmp_op); v_opnd_ctrl_sub("test_of",curscope->subscope);
    g_when_stmts(sub0(p));      /* when-parts */
    v_opcode(jmp_op); v_opnd_ctrl("endcase_of");
    v_label_ctrl_sub("test_of",curscope->subscope);
    g_stmts(sub1(p));           /* otherwise-part */
    v_etn(p);
    v_label_ctrl("endcase_of");
    popscope(case_of_scope);
    break;
  case N_case_ex_when_stmt:
    pushscope(case_ex_scope);
    e = eval(sub0(p));          /* test expression */
    push_case_ex_reg(e);
    v_opcode(jmp_op); v_opnd_ctrl_sub("test_ex",curscope->subscope);
    g_when_stmts(sub1(p));      /* when-parts */
    v_opcode(jmp_op); v_opnd_ctrl("endcase_ex");
    v_label_ctrl_sub("test_ex",curscope->subscope);
    g_stmts(sub2(p));           /* otherwise-part */
    v_etn(p);
    v_label_ctrl("endcase_ex");
    pop_case_ex_reg();
    popscope(case_ex_scope);
    break;
  case N_choice_stmt:
    switch (curscope->type) {
    case case_of_scope:
      q = sub0(p);
      assert (q->type == N_expr_list);
      v_opcode(jmp_op); v_opnd_ctrl("endcase_of");
      v_label_ctrl_sub("test_of",curscope->subscope);
      for (i=0; i<q->nsub; i++) {
        e = eval(subi(q,i));
        v_opcode(jmptrue_op);
        v_opnd_name(rd,e);
        v_opnd_ctrl_sub("choice_of",curscope->subscope);
      }
      v_etn(q);
      v_opcode(jmp_op); v_opnd_ctrl_sub("test_of",curscope->subscope + 1);
      v_label_ctrl_sub("choice_of",curscope->subscope);
      curscope->subscope += 1;
      break;
    case case_ex_scope:
      q = sub0(p);
      assert (q->type == N_expr_list);
      v_opcode(jmp_op); v_opnd_ctrl("endcase_ex");
      v_label_ctrl_sub("test_ex",curscope->subscope);
      for (i=0; i<q->nsub; i++) {
        e = eval(subi(q,i));
        v_opcode(jmpeq_op);
        v_opnd_name(rd,cur_case_ex_reg());
        v_opnd_name(rd,e);
        v_opnd_ctrl_sub("choice_ex",curscope->subscope);
      }
      v_etn(q);
      v_opcode(jmp_op); v_opnd_ctrl_sub("test_ex",curscope->subscope + 1);
      v_label_ctrl_sub("choice_ex",curscope->subscope);
      curscope->subscope += 1;
      break;
    default:  unexpected(curscope->type);
    }
    g_stmt(sub1(p));
    break;
  case N_loop_stmt:
    pushscope(loopframe_scope);
    push_loop_tn_scope(p->tn);
    generalized_iterator(sub0(p),sub1(p));
    v_opcode(jmp_op); v_opnd_ctrl("done");
    v_label_ctrl("body");
    pushscope(loopbody_scope);
    g_stmts(sub2(p));
    popscope(loopbody_scope);
    v_opcode(jmp_op); v_opnd_ctrl("step");
    v_etn(p);
    v_label_ctrl("done");
    pop_loop_tn_scope();
    popscope(loopframe_scope);
    break;
  case N_assignment_stmt:
    t = (token *)sub1(p); assert (t->type == N_Token);
    switch (t->code) {
    case Becomes:
      becomes(sub0(p),sub2(p));
      break;
    case Accum:
      assigning(sub0(p),t,sub2(p));
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
      assert (k <= r->nsub);
      assert (y != (symbol *)NULL);
      switch (y->type) {
      case refsym:
        t = name_token(subi(r,k-1));
        v_opcode(jmp_op); v_opnd_label(refinement_label(t->graphic));
        v_label(refret_label(t->graphic));
        break;
      case procsym:
        void_call(y,(node *)NULL,false);
        break;
      default:  unexpected(y->type);
      }
      break;
    case N_lhs3:  /* "lhs_or_call ( ) ;" or "lhs_or_call selector ;" */
      r = sub0(q);
      switch (r->type) {
      case N_lhs1:  /* "nameseq ( ) ;" or "nameseq selector ;" */
        r = sub0(r);  /* no comment */
        assert (r->type == N_nameseq);
        y = symresolve(inner,r,&k);
        assert (k <= r->nsub);
        assert (y && y->type == procsym);
        s = sub1(q);
        if (s == (node *)NULL) {
          void_call(y,(node *)NULL,false);
        } else {
          assert (s->type == N_selector1);
          void_call(y,sub0(s),sub1(s)!=NULL);
        }
        break;
      default:  unexpected(r->type);
      }
      break;
    default:  unexpected(q->type);
    }
    v_etn(p);
    break;
  case N_syscall_stmt:
    t = sysproc_token(sub0(p));
    y = symstackfind(inner,t->graphic);
    assert (y && y->type == sysrotsym);
    void_call(y,sub1(p),sub2(p)!=NULL);
    break;
  case N_from_stmt:
    t = (token *)sub1(p); assert (t->type == N_Token);
    e1 = get_lhs(sub0(p));
    e2 = eval(sub2(p));
    v_call(system_name(t->graphic));
    v_opnd_name(wr,e1);
    v_opnd_name(rw,e2);
    assign(sub0(p),e1);  /* no-op if sub0(p) is a simple user tag */
    assign(sub2(p),e2);  /* likewise for sub2(p) */
    break;
  case N_indirect_call_stmt:
    indirect_call((const char *)NULL,p);
    break;
  case N_cmd_stmt:
    q = sub0(p);  assert (q->type == N_command);
    t = cmdname_token(sub0(q));
    y = symstackfind(inner,t->graphic);
    assert (y && y->type == cmdsym);
    void_call(y,sub1(q),sub2(q)!=NULL);
    break;
  case N_expr_stmt:
    /*
     *  An alternative use of this fictitious statement type might
     *  be to print the result of evaluating the expression.
     */
    (void)eval(sub0(p));  /* evaluate expression for side-effects */
    break;
  case N_machine_stmt:
    /* Execute the "machine code" for its side-effects. */
    t = machine_token(sub0(p));
    v_etn(p);
    v_opcode(machine_code_op);
    v_opnd(machine_opera,0,t->graphic);
    break;
  case N_embedded_c_code:
    t = c_code_token(sub0(p));
    v_etn(p);
    embed_c_code(t->graphic);
    break;
  case N_null_stmt:
    break;
  default:  unexpected(p->type);
  }
  v_etn(p);
} /* end g_stmt */

/* .................................................................. */

static void g_elifs(p)
node *p;
{
  int i;
  switch (p->type) {
  case N_elifs:
    for (i=0; i<p->nsub; i++) g_elif(subi(p,i),i);
    break;
  default:  unexpected(p->type);
  }
}

/* .................................................................. */

static void g_elif(p,i)
node *p;
int i;
{
  const char *e;
  v_tn(p);
  switch (p->type) {
  case N_elif:
    e = eval(sub0(p));
    v_opcode(jmpfalse_op);
    v_opnd_name(rd,e);
    v_opnd_ctrl_sub("elif",i);
    g_stmts(sub1(p));
    v_opcode(jmp_op); v_opnd_ctrl("endif");
    v_label_ctrl_sub("elif",i);
    break;
  default:  unexpected(p->type);
  }
  v_etn(p);
}

/* .................................................................. */

static void g_when_stmts(p)
node *p;
{
  int i;
  switch (p->type) {
  case N_when_stmts:
    for (i=0; i<p->nsub; i++) g_when_stmt(subi(p,i));
    break;
  default:  unexpected(p->type);
  }
}

/* .................................................................. */

static void g_when_stmt(p)
node *p;
{
  node *q;
  const char *e;
  int i;
  v_tn(p);
  switch (p->type) {
  case N_when_stmt:
    switch (curscope->type) {
    case case_of_scope:
      q = sub0(p);
      assert (q->type == N_expr_list);
      v_opcode(jmp_op); v_opnd_ctrl("endcase_of");
      v_label_ctrl_sub("test_of",curscope->subscope);
      for (i=0; i<q->nsub; i++) {
        e = eval(subi(q,i));
        v_opcode(jmptrue_op);
        v_opnd_name(rd,e);
        v_opnd_ctrl_sub("choice_of",curscope->subscope);
      }
      v_etn(q);
      v_opcode(jmp_op); v_opnd_ctrl_sub("test_of",curscope->subscope + 1);
      v_label_ctrl_sub("choice_of",curscope->subscope);
      curscope->subscope += 1;
      break;
    case case_ex_scope:
      q = sub0(p);
      assert (q->type == N_expr_list);
      v_opcode(jmp_op); v_opnd_ctrl("endcase_ex");
      v_label_ctrl_sub("test_ex",curscope->subscope);
      for (i=0; i<q->nsub; i++) {
        e = eval(subi(q,i));
        v_opcode(jmpeq_op);
        v_opnd_name(rd,cur_case_ex_reg());
        v_opnd_name(rd,e);
        v_opnd_ctrl_sub("choice_ex",curscope->subscope);
      }
      v_etn(q);
      v_opcode(jmp_op); v_opnd_ctrl_sub("test_ex",curscope->subscope + 1);
      v_label_ctrl_sub("choice_ex",curscope->subscope);
      curscope->subscope += 1;
      break;
    default:  unexpected(curscope->type);
    }
    g_stmts(sub1(p));
    break;
  default:  unexpected(p->type);
  }
  v_etn(p);
}


/* ------------------------------------------------------------------ */

/* Expression Evaluation */

/*
 *  The principal routine of this section, 'eval', takes an
 *  arbitrary expression and generates code to evaluate it,
 *  yielding a name as a handle for the value.
 * 
 *  The "expression" is actually any one of several types
 *  of node.  There really should have been only one such
 *  type, "N_expr", but the context-free grammar has left
 *  us with things like "N_lhs" and so on which can also
 *  sometimes appear in evaluable contexts.
 * 
 *  If the expression reduces to a simple user tag or system
 *  tag, 'eval' just maps it to a name and does not generate
 *  code.  Therefore, if you are a user of 'eval', be careful
 *  that the code you generate always copies the value if it
 *  needs to.  Assignment is a prime example.  Actual args
 *  on calls are not, because routines are supposed to be
 *  guaranteed to use value and/or result semantics.
 * 
 *  There is also 'eval_into', in case you have your own
 *  ideas about where you'd like the result of a particular
 *  evaluation deposited.  Naturally, 'eval' uses it.
 *  It usually copies something, except when the source and
 *  the destination turn out to be just the same name.
 *
 *
 *  If, not knowing that Knuth once identified "premature
 *  optimization" as the source of most errors in programming,
 *  you are tempted to implement any more shortcuts here, just
 *  remember to be very scrupulous in making sure you have
 *  fully evaluated the rhs before you clobber anything on
 *  the lhs.  Aliases are the work of the devil, you know.
 *
 *  In fact, I hope that the only "optimizations" in here are
 *  the following very simple and powerful ones:
 * 
 *   1.  When you 'eval' something that is just a name
 *       (possibly compound, but fully resolvable),
 *       you should get no code, just the appropriate
 *       string for use here.  In cases where the thing
 *       you have eval'ed is supposed to be assigned into
 *       as well, you as caller are assumed to be intending
 *       to call 'assign' later on the same node.
 * 
 *   2.  When you 'get_lhs', likewise.  As caller, you are
 *       assumed to be intending to do an 'assign' on the
 *       same node later in this case.
 * 
 *   3.  When you 'assign', and the destination turns out
 *       to be a fully resolvable name that matches the
 *       source, you get no code.
 * 
 *  The above three optimizations turn out to be fairly
 *  important for the situations in which arbitrarily
 *  complicated (but probably simple) operands are fed
 *  through weird and wonderful machinery, which leaves
 *  results in arbitrarily complicated (but probably
 *  simple) destinations.  When operands and destinations
 *  are simple, the machinery can operate directly and
 *  benefit from the strict copying semantics of SETL.
 * 
 *   4.  Certain special operators (see 'assigning')
 *       are optimized for the common "incremental" use
 *       typified by "s +:= {x}".  There are special
 *       opcodes and run-time support for these.
 * 
 *  Any optimizations beyond these should be at the pleasure
 *  of the fabled dataflow analyzer.
 *
 *  Just to re-emphasize the point, note that even these
 *  simple optimizations have to be treated with sufficient
 *  caution.  For example, I fucked up the following before
 *  I discovered what I had done:
 * 
 *      t := [1,[2,3],4];
 *      [a,t,b] := t;
 * 
 *  This should give a=1, t=[2,3], b=4, but actually put OM
 *  into b before I realized my implementation of [...] := rhs
 *  was improper.  The reason was that t got assigned too early,
 *  and it was the (nonexistent) third element of t's new value
 *  that got assigned into b.
 */

/* .................................................................. */

/*
 *  Evaluate expression and gimme a string to denote it by
 */
static const char *eval(p)
node *p;
{
  int k;
  const char *e;
  node *q;
  token *tp;
  symbol *y;
  v_tn(p);
  switch (p->type) {
  case N_Token:
    tp = (token *)p;
    switch (tp->code) {
    case Integer:
      e = integer_lit(tp->graphic);
      break;
    case Real_:
      e = real_lit(tp->graphic);
      break;
    case String:
      e = string_lit(tp->graphic);
      break;
    case Syscon:
      e = system_name(tp->graphic);
      break;
    case Sysval:
    case Sysvar:
      e = temp_name();
      eval_into(e,p);
      break;
    default:
      e = temp_name();
      eval_into(e,p);
    }
    break;
  case N_compound:
  case N_lhs1:
  case N_rw1:
    q = sub0(p);
    assert (q->type == N_nameseq);
    y = symresolve(inner,q,&k);
    assert (k <= q->nsub);
    assert (y != (symbol *)NULL);
    if (k == q->nsub) {
      e = full_name(y);
    } else {
      e = temp_name();
      eval_into(e,p);
    }
    break;
  default:
    e = temp_name();
    eval_into(e,p);
  }
  v_etn(p);
  return(e);
}

/* .................................................................. */

/*
 *  Gimme the most direct and appropriate name for p, the idea
 *  being that where p is complicated, 'assign' will later be
 *  used to copy the value from here to its true destination
 */
static const char *get_lhs(p)
node *p;
{
  int k;
  const char *e;
  node *q;
  token *tp;
  symbol *y;
  v_tn(p);
  switch (p->type) {
  case N_Token:
    tp = (token *)p;
    switch (tp->code) {
    case '-':
      e = dummy_name();
      break;
    default:
      e = temp_name();
    }
    break;
  case N_compound:
  case N_lhs1:
  case N_rw1:
    q = sub0(p);
    assert (q->type == N_nameseq);
    y = symresolve(inner,q,&k);
    assert (k <= q->nsub);
    assert (y != (symbol *)NULL);
    if (k == q->nsub) {
      e = full_name(y);
    } else {
      e = temp_name();
    }
    break;
  default:
    e = temp_name();
  }
  v_etn(p);
  return(e);
}

/* .................................................................. */

static void eval_decl_rhs_into(e,p)
const char *e;
node *p;
{
  if (glowball != (symbol *)NULL) {
    pushsymtab(glowball);
    eval_into(e,p);
    popsymtab();
  } else {
    eval_into(e,p);
  }
}

/* .................................................................. */

/* Evaluate expression and store result against given name
 */
static void eval_into(e,p)
const char *e;
node *p;
{
  int i,j,k;
  token *tp,*t;
  node *q,*r,*s,*iter;
  symbol *y;
  const char *x,*xt,*e1,*e2;
  v_tn(p);
  switch (p->type) {
  case N_Token:  /* dash, etc. */
    tp = (token *)p;
    switch (tp->code) {
    case '-':
      unexpected(tp->code);  /* semantic analysis bug */
    case Integer:
      v_etn(p);
      v_copy(integer_lit(tp->graphic),e);
      break;
    case Real_:
      v_etn(p);
      v_copy(real_lit(tp->graphic),e);
      break;
    case String:
      v_etn(p);
      v_copy(string_lit(tp->graphic),e);
      break;
    case Syscon:
      v_etn(p);
      v_copy(system_name(tp->graphic),e);
      break;
    case Sysval:
    case Sysvar:
      v_etn(p);
      y = symstackfind(inner,tp->graphic);
      assert (y && y->type == sysrotsym);
      call(e,y,(node *)NULL,false);
      break;
    case Machine:
      /*
       *  Execute the "machine code" and then copy MACHINE into e.
       *  This is a very silly feature.
       */
      v_etn(p);
      v_opcode(machine_code_op);
      v_opnd(machine_opera,0,tp->graphic);
      v_copy(machine_reg(),e);
      break;
    default:  unexpected(tp->code);
    }
    break;
  case N_compound:
  case N_lhs1:
  case N_rw1:
    q = sub0(p);
    assert (q->type == N_nameseq);
    y = symresolve(inner,q,&k);
    assert (k <= q->nsub);
    assert (y != (symbol *)NULL);
    if (k == q->nsub) {
      v_etn(p);
      x = full_name(y);
      v_copy(x,e);
    } else {
      s = split(p,k);
      eval_into(e,s);
    }
    break;
  case N_lhs2:    /* [ lhs_or_dash_list ] */
  case N_rw2:     /* [ rw_list ] */
  case N_tuple1:  /* [ expr_list ] */
    pushscope(loopframe_scope);  /* in case it's needed */
    assert (p->nsub == 1);
    q = sub0(p);
    if (is_special_former(q,inner)) {
      special_former_into(e,q,tuple_former);
    } else {
      xt = temp_name();
      v_copy(null_tuple_lit(),xt);
      for (i=0; i<q->nsub; i++) {
        x = eval(subi(q,i));
        v_opcode(extend_tuple_op);
        v_opnd_name(rd,x);
        v_opnd_name(rw,xt);
      }
      v_opcode(truncate_op);
      v_opnd_name(rw,xt);
      v_copy(xt,e);
    }
    popscope(loopframe_scope);
    v_etn(p);
    break;
  case N_set1:    /* { expr_list } */
    pushscope(loopframe_scope);  /* in case it's needed */
    q = sub0(p);
    if (is_special_former(q,inner)) {
      special_former_into(e,q,set_former);
    } else {
      xt = temp_name();
      v_copy(empty_set_lit(),xt);
      for (i=0; i<q->nsub; i++) {
        x = eval(subi(q,i));
        v_opcode(extend_set_op);
        v_opnd_name(rd,x);
        v_opnd_name(rw,xt);
      }
      v_copy(xt,e);
    }
    popscope(loopframe_scope);
    v_etn(p);
    break;
  case N_lhs3:    /* lhs_or_call selector  */
                  /* lhs_or_call ( ) */
                  /* lhs selector */
  case N_rw3:     /* rw selector */
  case N_applic:  /* operand selector */
                  /* operand ( ) */
    r = sub0(p);
    s = sub1(p);
    switch (r->type) {
    case N_Token:
      if (s) {
        x = eval(r);
        get_selected(e,x,s);  /* e := x s, where s is the selector */
      } else {
        eval_into(e,r);
      }
      break;
    case N_compound:
    case N_lhs1:
    case N_rw1:
      q = sub0(r);
      assert (q->type == N_nameseq);
      if (s != (node *)NULL && s->type != N_selector1) goto noncall;
      y = symresolve(inner,q,&k);
      assert (k <= q->nsub);
      assert (y != (symbol *)NULL);
      if (y->type != procsym) goto noncall;
      if (s == (node *)NULL) call(e,y,(node *)NULL,false);
      else                   call(e,y,sub0(s),sub1(s)!=NULL);
      break;
    default:
      goto noncall;
    }
    v_etn(p);
    break;
  noncall:
    assert (s != (node *)NULL);
    x = eval(r);
    get_selected(e,x,s);  /* e := x s, where s is the selector */
    v_etn(p);
    break;
  case N_syscall: /* Sysproc ( expr_list [(*)] ) */
                  /* Sysproc ( ) */
                  /* Sysproc */
    t = sysproc_token(sub0(p));
    y = symstackfind(inner,t->graphic);
    assert (y && y->type == sysrotsym);
    call(e,y,sub1(p),sub2(p)!=NULL);
    v_etn(p);
    break;
  case N_tuple2:  /* [ former ] */
    pushscope(loopframe_scope);  /* in case it's needed */
    former_into(e,sub0(p),tuple_former);
    popscope(loopframe_scope);
    v_etn(p);
    break;
  case N_set2:    /* { former } */
    pushscope(loopframe_scope);  /* in case it's needed */
    former_into(e,sub0(p),set_former);
    popscope(loopframe_scope);
    v_etn(p);
    break;
  case N_assignment:
    t = (token *)sub1(p); assert (t->type == N_Token);
    switch (t->code) {
    case Becomes:
      becomes(sub0(p),sub2(p));
      break;
    case Accum:
      assigning(sub0(p),t,sub2(p));
      break;
    default:  unexpected(t->code);
    }
    eval_into(e,sub0(p));
    v_etn(p);
    break;
  case N_binary:
    t = (token *)sub1(p); assert (t->type == N_Token);
    if (leq(t->graphic,"AND")) {
      pushscope(if_expr_scope);
      e1 = eval(sub0(p));
      v_opcode(jmpfalse_op);
      v_opnd_name(rd,e1);
      v_opnd_ctrl("false_expr");
      eval_into(e,sub2(p));
      v_opcode(jmp_op); v_opnd_ctrl("end_and");
      v_label_ctrl("false_expr");
      v_copy(false_lit(),e);
      v_label_ctrl("end_and");
      popscope(if_expr_scope);
    } else if (leq(t->graphic,"OR")) {
      pushscope(if_expr_scope);
      e1 = eval(sub0(p));
      v_opcode(jmptrue_op);
      v_opnd_name(rd,e1);
      v_opnd_ctrl("true_expr");
      eval_into(e,sub2(p));
      v_opcode(jmp_op); v_opnd_ctrl("end_or");
      v_label_ctrl("true_expr");
      v_copy(true_lit(),e);
      v_label_ctrl("end_or");
      popscope(if_expr_scope);
    } else if (leq(t->graphic,"?")) {
      pushscope(if_expr_scope);
      eval_into(e,sub0(p));
      v_opcode(jmpne_op);
      v_opnd_name(rd,e);  /* instead */
      v_opnd_name(rd,om_lit());
      v_opnd_ctrl("end_query");
      eval_into(e,sub2(p));
      v_label_ctrl("end_query");
      popscope(if_expr_scope);
    } else {
      e1 = eval(sub0(p));
      e2 = eval(sub2(p));
      y = symstackfind(inner,t->graphic);
      v_call(y ? full_name(y) : system_name(t->graphic));
      v_opnd_name(rd,e1);
      v_opnd_name(rd,e2);
      v_opnd_name(wr,e);
    }
    v_etn(p);
    break;
  case N_bincomb:
    t = (token *)sub1(p); assert (t->type == N_Token);
    e1 = eval(sub0(p));
    e2 = eval(sub2(p));
    y = symstackfind(inner,t->graphic);
    v_call_bincomb(y ? full_name(y) : system_name(t->graphic));
    v_opnd_name(rd,e1);
    v_opnd_name(rd,e2);
    v_opnd_name(wr,e);
    v_etn(p);
    break;
  case N_unary:
    t = (token *)sub0(p); assert (t->type == N_Token);
    e1 = eval(sub1(p));
    y = symstackfind(inner,t->graphic);
    v_call(y ? full_name(y) : system_name(t->graphic));
    v_opnd_name(rd,e1);
    v_opnd_name(wr,e);
    v_etn(p);
    break;
  case N_uncomb:
    t = (token *)sub0(p); assert (t->type == N_Token);
    e1 = eval(sub1(p));
    y = symstackfind(inner,t->graphic);
    v_call_uncomb(y ? full_name(y) : system_name(t->graphic));
    v_opnd_name(rd,e1);
    v_opnd_name(wr,e);
    v_etn(p);
    break;
  case N_expr:  /* parenthesized expression */
    eval_into(e,sub0(p));
    v_etn(p);
    break;
  case N_case_of_expr:  /* CASE OF choices ELSE expr END */
    pushscope(case_of_expr_scope);
    q = sub0(p);  /* choices */
    assert (q->type == N_choices);
    for (i=0; i<q->nsub; i++) {
      r = subi(q,i);  /* choice */
      assert (r->type == N_choice);
      s = sub0(r);  /* expr_list */
      assert (s->type == N_expr_list);
      v_tn(s);
      v_label_ctrl_sub("test_of_expr",i);
      for (j=0; j<s->nsub; j++) {
        x = eval(subi(s,j));  /* boolean case label part */
        v_opcode(jmptrue_op);
        v_opnd_name(rd,x);
        v_opnd_ctrl_sub("choice_of_expr",i);
      }
      v_opcode(jmp_op); v_opnd_ctrl_sub("test_of_expr",i+1);
      v_label_ctrl_sub("choice_of_expr",i);
      eval_into(e,sub1(r));  /* chosen expression */
      v_opcode(jmp_op); v_opnd_ctrl("endcase_of_expr");
    }
    v_label_ctrl_sub("test_of_expr",q->nsub);
    eval_into(e,sub1(p));  /* else-expression */
    v_etn(p);
    v_label_ctrl("endcase_of_expr");
    popscope(case_of_expr_scope);
    break;
  case N_case_ex_expr:  /* CASE expr OF choices ELSE expr END */
    pushscope(case_ex_expr_scope);
    e1 = eval(sub0(p));   /* test expression */
    q = sub1(p);  /* choices */
    assert (q->type == N_choices);
    for (i=0; i<q->nsub; i++) {
      r = subi(q,i);  /* choice */
      assert (r->type == N_choice);
      s = sub0(r);  /* expr_list */
      assert (s->type == N_expr_list);
      v_tn(s);
      v_label_ctrl_sub("test_ex_expr",i);
      for (j=0; j<s->nsub; j++) {
        e2 = eval(subi(s,j));  /* comparison expression */
        v_opcode(jmpeq_op);
        v_opnd_name(rd,e1);
        v_opnd_name(rd,e2);
        v_opnd_ctrl_sub("choice_ex_expr",i);
      }
      v_opcode(jmp_op); v_opnd_ctrl_sub("test_ex_expr",i+1);
      v_label_ctrl_sub("choice_ex_expr",i);
      eval_into(e,sub1(r));  /* chosen expression */
      v_opcode(jmp_op); v_opnd_ctrl("endcase_ex_expr");
    }
    v_label_ctrl_sub("test_ex_expr",q->nsub);
    eval_into(e,sub2(p));  /* else-expression */
    v_etn(p);
    v_label_ctrl("endcase_ex_expr");
    popscope(case_ex_expr_scope);
    break;
  case N_if_expr:  /* IF expr THEN expr opt_elif_exprs ELSE expr END */
    pushscope(if_expr_scope);
    x = eval(sub0(p));   /* test expression */
    v_opcode(jmpfalse_op);
    v_opnd_name(rd,x);
    v_opnd_ctrl("elif_exprs");
    eval_into(e,sub1(p));   /* then-part */
    v_opcode(jmp_op); v_opnd_ctrl("endif_expr");
    v_label_ctrl("elif_exprs");
    q = sub2(p);  /* elif_exprs */
    assert (q->type == N_elif_exprs);
    for (i=0; i<q->nsub; i++) {
      r = subi(q,i);
      assert (r->type == N_elif_expr);
      x = eval(sub0(r));
      v_opcode(jmpfalse_op);
      v_opnd_name(rd,x);
      v_opnd_ctrl_sub("elif_expr",i);
      eval_into(e,sub1(r));
      v_opcode(jmp_op); v_opnd_ctrl("endif_expr");
      v_label_ctrl_sub("elif_expr",i);
    }
    eval_into(e,sub3(p));  /* else-part */
    v_etn(p);
    v_label_ctrl("endif_expr");
    popscope(if_expr_scope);
    break;
  case N_expr_clause:
    pushscope(expr_clause_scope);
    push_yield_reg(e);
    g_stmts(sub0(p));
    pop_yield_reg();
    popscope(expr_clause_scope);
    v_etn(p);
    break;
  case N_exists:     /* EXISTS simpliter_list | expr */
    iter = node2(N_iterator,p,sub0(p),sub1(p));
    pushscope(loopframe_scope);
    xt = temp_name();
    v_copy(false_lit(),xt);
    generalized_iterator(iter,(node *)NULL);
    v_opcode(jmp_op); v_opnd_ctrl("done");
    v_label_ctrl("body");
    v_copy(true_lit(),xt);
    v_opcode(jmp_op); v_opnd_ctrl("term");
    v_label_ctrl("done");
    v_copy(xt,e);
    v_etn(p);
    popscope(loopframe_scope);
    release(iter);
    break;
  case N_notexists:  /* NOTEXISTS simpliter_list | expr */
    iter = node2(N_iterator,p,sub0(p),sub1(p));
    pushscope(loopframe_scope);
    xt = temp_name();
    v_copy(true_lit(),xt);
    generalized_iterator(iter,(node *)NULL);
    v_opcode(jmp_op); v_opnd_ctrl("done");
    v_label_ctrl("body");
    v_copy(false_lit(),xt);
    v_opcode(jmp_op); v_opnd_ctrl("term");
    v_label_ctrl("done");
    v_copy(xt,e);
    v_etn(p);
    popscope(loopframe_scope);
    release(iter);
    break;
  case N_forall:     /* FORALL simpliter_list | expr */
    iter = node2(N_iterator,p,sub0(p),(node *)NULL);
    pushscope(loopframe_scope);
    xt = temp_name();
    v_copy(true_lit(),xt);
    generalized_iterator(iter,(node *)NULL);
    v_opcode(jmp_op); v_opnd_ctrl("done");
    v_label_ctrl("body");
    x = eval(sub1(p));
    v_opcode(jmptrue_op);
    v_opnd_name(rd,x);
    v_opnd_ctrl("step");
    v_copy(false_lit(),xt);
    v_opcode(jmp_op); v_opnd_ctrl("term");
    v_label_ctrl("done");
    v_copy(xt,e);
    v_etn(p);
    popscope(loopframe_scope);
    release(iter);
    break;
  case N_procval:    /* PROC compound */
                     /* '(' PROC ')' compound */
                     /* ROUTINE compound */
                     /* '(' ROUTINE ')' compound */
    q = sub0(p);
    assert (q->type == N_compound);
    r = sub0(q);
    assert (r->type == N_nameseq);
    y = symresolve(inner,r,&k);
    assert (k <= r->nsub);
    assert (y && y->type == procsym);
    x = full_name(y);
    v_routine(x,e);
    v_etn(p);
    break;
  case N_cmdval:     /* COMMAND Command_name */
    t = cmdname_token(sub0(p));
    y = symstackfind(inner,t->graphic);
    assert (y && y->type == cmdsym);
    x = full_name(y);
    v_routine(x,e);
    v_etn(p);
    break;
  case N_indirect_call:   /* CALL ( expr [, expr_list [(*)] ] ) */
    indirect_call(e,p);
    break;
  case N_command:
    t = cmdname_token(sub0(p));
    y = symstackfind(inner,t->graphic);
    assert (y && y->type == cmdsym);
    call(e,y,sub1(p),sub2(p)!=NULL);
    v_etn(p);
    break;
  default:  unexpected(p->type);
  }
  v_etn(p);
} /* end eval_into */

/* .................................................................. */

static void special_former_into(e,p,type)
const char *e;
node *p;
former type;
{
  node *q;
  token *t;
  v_tn(p);
  assert (is_special_former(p,inner));
  assert (p->nsub == 1);
  q = sub0(p);
  assert (q->type == N_binary);
  t = (token *)sub1(q);
  assert (t->code == IN);
  former3_into(e,sub0(q),sub2(q),(node *)NULL,type);
  v_etn(p);
}

/* .................................................................. */

static void former_into(e,p,type)
const char *e;
node *p;
former type;
{
  node *q;
  v_tn(p);
  switch (p->type) {
  case N_former1:  /* expr :              loop_head   */
                   /* expr :     iterator loop_head   */
                   /* expr : FOR iterator loop_head   */
    former1_into(e,sub0(p),sub1(p),sub2(p),type);
    break;
  case N_former2:  /* expr       .. expr   */
                   /* expr, expr .. expr   */
    q = sub0(p);
    switch (q->nsub) {
    case 1:
      former2_into(e,sub0(q),(node *)NULL,sub1(p),type);
      break;
    case 2:
      former2_into(e,sub0(q),sub1(q),     sub1(p),type);
      break;
    default:  unexpected(q->nsub);
    }
    break;
  case N_former3:  /* operand IN expr | expr   */
    former3_into(e,sub0(p),sub1(p),sub2(p),type);
    break;
  default:  unexpected(p->type);
  }
  v_etn(p);
} /* end former_into */

/* .................................................................. */

static void former1_into(e,p,q,r,type)
const char *e;
node *p,*q,*r;  /* expr : iterator loop_head */
former type;
{
  const char *x,*xt;
  xt = temp_name();
  switch (type) {
  case tuple_former:
    v_copy(null_tuple_lit(),xt);
    v_opcode(jmp_op); v_opnd_ctrl("init");
    v_label_ctrl("body");
    x = eval(p);
    v_opcode(extend_tuple_op);
    v_opnd_name(rd,x);
    v_opnd_name(rw,xt);
    v_opcode(jmp_op); v_opnd_ctrl("step");
    break;
  case set_former:
    v_copy(empty_set_lit(),xt);
    v_opcode(jmp_op); v_opnd_ctrl("init");
    v_label_ctrl("body");
    x = eval(p);
    v_opcode(extend_set_op);
    v_opnd_name(rd,x);
    v_opnd_name(rw,xt);
    v_opcode(jmp_op); v_opnd_ctrl("step");
    break;
  default:  unexpected(type);
  }
  v_label_ctrl("init");
  generalized_iterator(q,r);
  if (type == tuple_former) {
    v_opcode(truncate_op);
    v_opnd_name(rw,xt);
  }
  v_copy(xt,e);
} /* end former1_into */

/* .................................................................. */

static void former2_into(e,p,q,r,type)
const char *e;
node *p,*q,*r;  /* expr, expr .. expr */
former type;
{
  const char *x1;
  const char *x2 = NULL;
  const char *x3;
  x1 = eval(p);
  if (q != (node *)NULL) x2 = eval(q);
  x3 = eval(r);
  switch (type) {
  case tuple_former:
    if (q != (node *)NULL) {
      v_opcode(tuple_fnl_op);
      v_opnd_name(rd,x1);
      v_opnd_name(rd,x2);
      v_opnd_name(rd,x3);
      v_opnd_name(wr,e);
    } else {
      v_opcode(tuple_fl_op);
      v_opnd_name(rd,x1);
      v_opnd_name(rd,x3);
      v_opnd_name(wr,e);
    }
    break;
  case set_former:
    if (q != (node *)NULL) {
      v_opcode(set_fnl_op);
      v_opnd_name(rd,x1);
      v_opnd_name(rd,x2);
      v_opnd_name(rd,x3);
      v_opnd_name(wr,e);
    } else {
      v_opcode(set_fl_op);
      v_opnd_name(rd,x1);
      v_opnd_name(rd,x3);
      v_opnd_name(wr,e);
    }
    break;
  default:  unexpected(type);
  }
} /* end former2_into */

/* .................................................................. */

static void former3_into(e,p,q,r,type)
const char *e;
node *p,*q,*r;  /* operand IN expr | expr */
former type;
{
  const char *x,*xt;
  node *iter,*slist,*simp;
  simp = node2(N_simpliter1,p,p,q);
  slist = node1(N_simpliter_list,simp,simp);
  iter = node2(N_iterator,slist,slist,r);
  xt = temp_name();
  switch (type) {
  case tuple_former:
    v_copy(null_tuple_lit(),xt);
    v_opcode(jmp_op); v_opnd_ctrl("init");
    v_label_ctrl("body");
    x = eval(p);
    v_opcode(extend_tuple_op);
    v_opnd_name(rd,x);
    v_opnd_name(rw,xt);
    v_opcode(jmp_op); v_opnd_ctrl("step");
    break;
  case set_former:
    v_copy(empty_set_lit(),xt);
    v_opcode(jmp_op); v_opnd_ctrl("init");
    v_label_ctrl("body");
    x = eval(p);
    v_opcode(extend_set_op);
    v_opnd_name(rd,x);
    v_opnd_name(rw,xt);
    v_opcode(jmp_op); v_opnd_ctrl("step");
    break;
  default:  unexpected(type);
  }
  v_label_ctrl("init");
  generalized_iterator(iter,(node *)NULL);
  if (type == tuple_former) {
    v_opcode(truncate_op);
    v_opnd_name(rw,xt);
  }
  v_copy(xt,e);
  release(iter);
  release(slist);
  release(simp);
} /* end former3_into */


/* ------------------------------------------------------------------ */

/* General Iteration and Looping */

/*
 *  The mechanism here is general enough to support a
 *  combination of "iterator" looping and "loop header"
 *  (i.e., INIT, DOING, WHILE, STEP, UNTIL, and TERM)
 *  looping.
 * 
 *  The principal routine of this section, 'generalized_iterator',
 *  generates a code block with labels containing numbers
 *  corresponding to the current lexical scope.  This
 *  scope should be of type loopframe_scope.
 * 
 *  This allows you to branch to the key entry points
 *  of the iterator and be branched to from the
 *  appropriate places.  It is understood that the
 *  code block is "flowed into" initially from the top.
 * 
 *  Informally, the structure of the code block is:
 * 
 *      INIT statements
 *      initialize iterator
 *   start:
 *      if iterator exhausted go to term
 *      define bound variables
 *      DOING statements
 *      if not WHILE-expr go to term
 *      go to body
 *   step:  (to which body may "return")
 *      STEP statements
 *      IF UNTIL-expr go to term
 *      go to start
 *   term:
 *      undefine bound variables
 *      TERM statements
 * 
 *  The convention for labels includes a string representation
 *  of the current (unique) scope number.  See 'v_label_ctrl';
 *  the "stem" of each label is as shown here, e.g., "start".
 * 
 *  Your code, at the "body" label, is either a loop body
 *  or code to evaluate an expression in the process of
 *  forming a set or tuple or evaluating a quantifier.
 * 
 *  You should implement QUIT as a branch to the "term"
 *  label and CONTINUE as a branch to the "start" label.
 *  Otherwise, to continue normal iteration, branch to "step".
 * 
 *  Iterator exhaustion causes all bound variables to
 *  become undefined (set to OM).
 * 
 *  If the such-that part of the iterator is missing,
 *  it is taken to be TRUE.
 * 
 *  If the entire iterator is missing, the relevant parts
 *  of the code block will be omitted.
 * 
 *  If a particular loop header clause is missing, that
 *  part of the code block will be omitted.
 */

/* .................................................................. */

static void generalized_iterator(it,head)
node *it;    /* iterator */
node *head;  /* loop header */
{
  node *q;
  const char *x;
  assert (curscope->type == loopframe_scope);
  if (head != (node *)NULL && (q=sub0(head)) != (node *)NULL) {
    assert (q->type == N_loop_init);
    pushscope(init_scope);
    g_stmts(sub0(q));
    popscope(init_scope);
  }
  iterator(it);
  if (head != (node *)NULL && (q=sub1(head)) != (node *)NULL) {
    assert (q->type == N_loop_doing);
    pushscope(doing_scope);
    g_stmts(sub0(q));
    popscope(doing_scope);
  }
  if (head != (node *)NULL && (q=sub2(head)) != (node *)NULL) {
    assert (q->type == N_loop_while);
    x = eval(sub0(q));
    v_opcode(jmpfalse_op);
    v_opnd_name(rd,x);
    v_opnd_ctrl("term");
  }
  v_opcode(jmp_op); v_opnd_ctrl("body");
  v_label_ctrl("step");
  if (head != (node *)NULL && (q=sub3(head)) != (node *)NULL) {
    assert (q->type == N_loop_step);
    pushscope(step_scope);
    g_stmts(sub0(q));
    popscope(step_scope);
  }
  if (head != (node *)NULL && (q=sub4(head)) != (node *)NULL) {
    assert (q->type == N_loop_until);
    x = eval(sub0(q));
    v_opcode(jmptrue_op);
    v_opnd_name(rd,x);
    v_opnd_ctrl("term");
  }
  v_opcode(jmp_op); v_opnd_ctrl("start");
  v_label_ctrl("term");
  if (head != (node *)NULL && (q=sub5(head)) != (node *)NULL) {
    assert (q->type == N_loop_term);
    pushscope(term_scope);
    g_stmts(sub0(q));
    popscope(term_scope);
  }
  v_etn(head);
} /* end generalized_iterator */

/* .................................................................. */

/*
 *  The code we want here is to be a chain of simpliters.
 * 
 *  What is supposed to happen is kind of a cascade effect:
 * 
 *  Control flows down through the simpliters, each one
 *  getting defined in terms of the variables set up by
 *  the previous one, until they're all initialized
 *  and have produced values.
 * 
 *  When you want the "next" set of variables from the
 *  compound iterator, you branch to "start", which
 *  actually jumps immediately to the "next" point of
 *  the last (innermost) simpliter.  When a simpliter
 *  is exhausted, it branches to the "next" point of
 *  the simpliter just above (outside) it, and so on
 *  until the top-level (outermost) iterator is
 *  exhausted.  The label it branches to is immediately
 *  followed by a branch to the "term" point.
 */

static void iterator(p)
node *p;   /*  iterator :  simpliter_list | expr   */
           /*  iterator :  simpliter_list          */
{
  node *q,*r;
  const char *x;
  int i;
  v_tn(p);
  if (p == (node *)NULL) {
    v_label_ctrl("start");  /* a null iterator is never exhausted */
    return;
  }
  assert (p->type == N_iterator);
  q = sub0(p);  /* simpliter_list */
  assert (q->type == N_simpliter_list);
  v_opcode(jmp_op);  v_opnd_ctrl("cascade");
  v_label_ctrl("start");  /* the "restart" point */
  v_opcode(jmp_op);  v_opnd_ctrl_sub("next",q->nsub);
  v_label_ctrl_sub("next",0);  /* here when the cascade runs out */
  v_opcode(jmp_op);  v_opnd_ctrl("term");
  v_label_ctrl("cascade");
  for (i=0; i<q->nsub; i++) {
    simpliter(subi(q,i),i);
  }
  r = sub1(p);
  if (r != (node *)NULL) {
    x = eval(r);
    v_opcode(jmpfalse_op);
    v_opnd_name(rd,x);
    v_opnd_ctrl("start");
  }
  v_etn(p);
} /* end iterator */

/* .................................................................. */

static void simpliter(p,i)
node *p;        /* simpliter        */
int i;          /* labelling number */
{
  node *q,*r,*s,*f,*n,*l,*d;
  const char *x,*xf,*xn,*xl,*e,*lhs,*list;
  bool counted_range;
  v_tn(p);
  switch (p->type) {
  case N_simpliter1:  /* lhs IN expr */
    q = sub1(p);
    counted_range = (q->type == N_tuple2 || q->type == N_set2) &&
               sub0(q)->type == N_former2;
    if (counted_range) {  /* e.g., [1..n], {2,4..x*y}  */
      /* Simple optimization for a common case */
      r = sub0(q);
      s = sub0(r);  assert (s->type == N_expr_list);
      f = sub0(s);   /* first */
      if (s->nsub == 1) {
        n = NULL;    /* implicit next = first + 1 */
      } else {
        n = sub1(s);  /* explicit next */
      }
      l = sub1(r);    /* last */
      xf = eval(f);
      xn = n ? eval(n) : NULL;
      xl = eval(l);
      e = temp_name();
      if (n) v_opcode(init_counter_fnl_op);
      else   v_opcode(init_counter_fl_op);
      v_opnd_name(rd,xf);
      if (n) v_opnd_name(rd,xn);
      v_opnd_name(rd,xl);
      v_opnd_name(wr,e);
    } else {
      x = eval(q);
      e = temp_name();
      v_opcode(init_iterator_op);
      v_opnd_name(rd,x);
      v_opnd_name(wr,e);
    }
    v_label_ctrl_sub("next",i+1);
    lhs = get_lhs(sub0(p));
    if (counted_range) {
      v_opcode(step_counter_op);
    } else {
      v_opcode(step_iterator_op);
    }
    v_opnd_name(rw,e);
    v_opnd_name(wr,lhs);
    v_opnd_ctrl_sub("good",i+1);
    assign(sub0(p),om_lit());  /* give that lhs an om_lit! */
    v_opcode(jmp_op); v_opnd_ctrl_sub("next",i);
    v_label_ctrl_sub("good",i+1);
    assign(sub0(p),lhs);
    break;
  case N_simpliter2:  /* lhs = compound_or_predef ( lhs_list ) */
    x = eval(sub1(p));
    e = temp_name();
    v_opcode(init_itersmap_op);
    v_opnd_name(rd,x);
    v_opnd_name(wr,e);
    v_label_ctrl_sub("next",i+1);
    lhs = get_lhs(sub0(p));
    q = sub2(p);
    assert (q->type == N_lhs_list);
    if (q->nsub > 1) {
      list = temp_name();
      d = node1(N_lhs2,q,q);  /* e.g. treat z=f(x,y) as z=f([x,y]) */
    } else {
      list = get_lhs(sub0(q));
      d = sub0(q);
    }
    v_opcode(step_itersmap_op);
    v_opnd_name(rw,e);
    v_opnd_name(wr,list);
    v_opnd_name(wr,lhs);
    v_opnd_ctrl_sub("good",i+1);
    assign(d,om_lit());
    assign(sub0(p),om_lit());
    v_opcode(jmp_op); v_opnd_ctrl_sub("next",i);
    v_label_ctrl_sub("good",i+1);
    assign(d,list);
    assign(sub0(p),lhs);
    if (q->nsub > 1) release(d);
    break;
  case N_simpliter3:  /* lhs = compound_or_predef { lhs_list } */
    x = eval(sub1(p));
    e = temp_name();
    v_opcode(init_itermmap_op);
    v_opnd_name(rd,x);
    v_opnd_name(wr,e);
    v_label_ctrl_sub("next",i+1);
    lhs = get_lhs(sub0(p));
    q = sub2(p);
    assert (q->type == N_lhs_list);
    if (q->nsub > 1) {
      list = temp_name();
      d = node1(N_lhs2,q,q);  /* e.g. treat z=f{x,y} as z=f{[x,y]} */
    } else {
      list = get_lhs(sub0(q));
      d = sub0(q);
    }
    v_opcode(step_itermmap_op);
    v_opnd_name(rw,e);
    v_opnd_name(wr,list);
    v_opnd_name(wr,lhs);
    v_opnd_ctrl_sub("good",i+1);
    assign(d,om_lit());
    assign(sub0(p),om_lit());
    v_opcode(jmp_op); v_opnd_ctrl_sub("next",i);
    v_label_ctrl_sub("good",i+1);
    assign(d,list);
    assign(sub0(p),lhs);
    if (q->nsub > 1) release(d);
    break;
  default:  unexpected(p->type);
  }
  v_etn(p);
} /* end simpliter */


/* ------------------------------------------------------------------ */

/* General Call */

/* .................................................................. */

/* Call a routine and throw away the result */
static void void_call(y,p,vactual)
symbol *y;      /* symbol representing routine to call */
node *p;        /* arg list (or NULL) */
bool vactual;   /* trailing (*) on list of actual args */
{
  call((const char *)NULL,y,p,vactual);
}

/* .................................................................. */

/* Acceptable node types for the arg list p (p->type) are at least
 * N_expr_list, N_lhs_list, N_rw_list, N_lhs_or_dash_list, N_cmdargs;
 * but really any list of plausible args will do.  */
static void call(r,y,p,vactual)
const char *r;  /* where to return the result (or NULL) */
symbol *y;      /* symbol representing routine to call */
node *p;        /* arg list (or NULL) */
bool vactual;   /* trailing (*) on list of actual args */
{
  int i,nformal,nactual;
  const char **a;
  const char *rot;
  v_tn(p);
  if (r == (const char *)NULL) r = dummy_name();
  switch (y->type) {
  case procsym:
  case sysrotsym:
  case cmdsym:
    rot = full_name(y);
    break;
  default:  unexpected(y->type);
  }
  nformal = y->nparms;
  assert (nformal >= 0);
  nactual = p==(node *)NULL ? 0 : p->nsub;
  assert (nactual >= 0);
  if (vactual) assert (nactual > 0);
  if (nactual > 0) {
    assert (nformal > 0);  /* or call is illegal */
    getmem(a, nactual, const char *);
    for (i=0; i<nactual; i++) {
      int j = i<nformal ? i : nformal-1;
      switch (y->parms[j] & rwable) {
        case rdable:  a[i] = eval(subi(p,i));     break;
        case wrable:  a[i] = get_lhs(subi(p,i));  break;
        case rwable:  a[i] = eval(subi(p,i));     break;
        default:  unexpected (y->parms[j] & rwable);
      }
    }
    v_etn(p);
    if (vactual) v_vcall(rot);
    else v_call(rot);
    for (i=0; i<nactual; i++) {
      int j = i<nformal ? i : nformal-1;
      switch (y->parms[j] & rwable) {
        case rdable:  v_opnd_name(rd,a[i]);  break;
        case wrable:  v_opnd_name(wr,a[i]);  break;
        case rwable:  v_opnd_name(rw,a[i]);  break;
        default:  unexpected (y->parms[j] & rwable);
      }
    }
    v_opnd_name(wr,r);
    for (i=0; i<nactual; i++) {
      int j = i<nformal ? i : nformal-1;
      switch (y->parms[j] & rwable) {
        case rdable:  break;
        case wrable:  assign(subi(p,i),a[i]);  break;
        case rwable:  assign(subi(p,i),a[i]);  break;
        default:  unexpected (y->parms[j] & rwable);
      }
    }
    release(a);
  } else {  /* nactual == 0 */
    assert (!vactual);
    v_etn(p);
    v_call(rot);
    v_opnd_name(wr,r);
  }
} /* end call */

/* .................................................................. */

/* Indirect call */
static void indirect_call(r,p)
const char *r;  /* where to stash result, or NULL */
node *p;        /* sub0 is routine, sub1 is arg list, sub2 is opt (*) */
{
  const char *rot;
  const char **a;
  node *q;
  int i,nactual;
  rot = eval(sub0(p));
  q = sub1(p);
  nactual = q->nsub;
  if (nactual > 0) getmem (a, nactual, const char *);
  else a = NULL;
  for (i=0; i<nactual; i++) {
    a[i] = eval(subi(q,i));
  }
  v_etn(p);
  if (sub2(p) == NULL) {
    v_opcode(icall_op);
  } else {
    v_opcode(vicall_op);
  }
  v_opnd_name(rd,rot);
  for (i=0; i<nactual; i++) {
    v_opnd_name(rd,a[i]);
  }
  v_opnd_name(wr, r ? r : dummy_name() );
  if (nactual > 0) release(a);
} /* end indirect_call */

/* .................................................................. */

/* Nullary call for special purposes (e.g., package initialization) */
static void special_call(s)
const char *s;
{
  v_call(s);
  v_opnd_name(wr,dummy_name());
}


/* ------------------------------------------------------------------ */

/* General Assignment */

/*
 *  As you're wont to read in the books, SETL has a rather
 *  elegant and powerful model of assignments.
 * 
 *  You can assign to a slice, which can change the length of
 *  the host tuple or string, you can update smaps and tuple
 *  elements identified as e(i), or mmaps with e{i}, and you
 *  can assign to displays that look like tuples with some
 *  elements being dashes (throwaways).
 * 
 *  If e is an "lhs" (informal term for assignment destination,
 *  which may in fact be one of several node types in this
 *  translator), and t is a temporary variable, and p is a
 *  "selector" ((i),(i,...,j),{i},{i,...,j},(i..j),(i..),(..j)),
 *  then
 *      e p := x
 *  means
 *      t := e; t p := x; e := t.
 *  Since e itself may have further selection complexity,
 *  this definition applies recursively.
 * 
 *  As for the primitives:
 *      t(i) := x  just replace single elements and are easy
 *      t{i} := x  just replaces some pairs in a map
 *      t(i..j) := x  means t := t(1..i-1) + x + t(j+1..)
 *  but none of these are worrisome to us here, because we
 *  just generate instructions that identify all the parts.
 * 
 *  The form, e.g.:
 *      [a,-,c] := x
 *  is only slightly worse, and we will handle it by generating
 *  instructions that mean
 *      t := value of x
 *      a := t(1);  $ except that if t=OM, a:=OM
 *      c := t(3);  $ similarly
 */

/* .................................................................. */

/*
 *  General to := from
 */
static void becomes(to,from)
node *to,*from;
{
  const char *x;
  x = eval(from);
  assign(to,x);
  v_etn(from);
} /* end becomes */

/* .................................................................. */

/*
 *  General to op:= from, where 'to' is both source and dest
 */
static void assigning(to,op,from)
node *to;
token *op;
node *from;
{
  const char *e1,*e2;
  const char *s;
  symbol *y;
  /* Certain special cases are optimized to be "incremental" */
  assert (op->type == N_Token);
  assert (op->code == Accum);
  s = strmakeupper(op->graphic);
  if (leq(s,"AND")) {
    pushscope(if_expr_scope);
    e1 = eval(to);
    v_opcode(jmpfalse_op);
    v_opnd_name(rd,e1);
    v_opnd_ctrl("done_and");
    e2 = eval(from);
    assign(to,e2);
    v_label_ctrl("done_and");
    popscope(if_expr_scope);
  } else if (leq(s,"OR")) {
    pushscope(if_expr_scope);
    e1 = eval(to);
    v_opcode(jmptrue_op);
    v_opnd_name(rd,e1);
    v_opnd_ctrl("done_or");
    e2 = eval(from);
    assign(to,e2);
    v_label_ctrl("done_or");
    popscope(if_expr_scope);
  } else if (leq(s,"?")) {
    pushscope(if_expr_scope);
    e1 = eval(to);
    v_opcode(jmpne_op);
    v_opnd_name(rd,e1);
    v_opnd_name(rd,om_lit());
    v_opnd_ctrl("done_query");
    e2 = eval(from);
    assign(to,e2);
    v_label_ctrl("done_query");
    popscope(if_expr_scope);
  } else {
    e1 = eval(to);
    e2 = eval(from);
    if (leq(s,"+") ||
        leq(s,"-") ||
/*      leq(s,"*") || */
        leq(s,"WITH") ||
        leq(s,"LESS") ||
        leq(s,"LESSF")) {
      v_call_assigning(system_name(op->graphic));
      v_opnd_name(rw,e1);
      v_opnd_name(rd,e2);
    } else {
      y = symstackfind(inner,op->graphic);
      v_call(y ? full_name(y) : system_name(op->graphic));
      v_opnd_name(rd,e1);
      v_opnd_name(rd,e2);
      v_opnd_name(wr,e1);
    }
    assign(to,e1);
  }
  /* (I'm trusting eval and assign to take care of the v_tn nonsense) */
} /* end assigning */

/* .................................................................. */

/*
 *  The master of assignment:  p := x for arbitrary lhs p and name x
 */
static void assign(p,x)
node *p;        /* destination */
const char *x;  /* source value (a name or literal) */
{
  int i,k;
  token *tp;
  node *q,*r,*s;
  symbol *y;
  const char *e, *xt, *rot;
  v_tn(p);
  switch (p->type) {
  case N_Token:  /* dash, etc. */
    tp = (token *)p;
    switch (tp->code) {
    case '-':
      v_etn(p);
      break;  /* throw away value */
    case Sysvar:
      v_etn(p);
      y = symstackfind(inner,strjoin("SET_",tp->graphic));
      assert (y && y->type == sysrotsym);
      rot = full_name(y);
      v_call(rot);
      v_opnd_name(rd,x);
      v_opnd_name(wr,dummy_name());
      break;
    case Machine:
      /*
       * There is no syntax for having machine code in an lhs position,
       * but this case would handle it if there were.  The rhs (x) is
       * copied into the MACHINE register, and then the machine code
       * is executed for its side-effects.  A most dubious feature.
       */
      v_etn(p);
      v_copy(x,machine_reg());
      v_opcode(machine_code_op);
      v_opnd(machine_opera,0,tp->graphic);
      break;
    default:  unexpected(tp->code);
    }
    break;
  case N_compound:
  case N_lhs1:
  case N_rw1:
    q = sub0(p);
    assert (q->type == N_nameseq);
    y = symresolve(inner,q,&k);
    assert (k <= q->nsub);
    assert (y != (symbol *)NULL);
    if (k == q->nsub) {
      e = full_name(y);
      v_copy(x,e);
    } else {
      s = split(p,k);
      assign(s,x);
    }
    break;
  case N_lhs2:    /* [ lhs_or_dash_list ], or fakery from 'simpliter' */
  case N_rw2:     /* [ rw_list ] */
  case N_tuple1:  /* [ expr_list ], hopefully checked for rw_listness */
    assert (p->nsub == 1);
    q = sub0(p);
    xt = temp_name();
    v_copy(x,xt);
    for (i=0; i<q->nsub; i++) {
      e = get_lhs(subi(q,i));
      v_copy_indexed(xt,i+1,e);
      assign(subi(q,i),e);
    }
    v_etn(p);
    break;
  case N_lhs3:    /* lhs_or_call selector  */
                  /* lhs selector */
  case N_rw3:     /* rw selector */
  case N_applic:  /* operand selector */
    r = sub0(p);
    s = sub1(p);
    assert (s != (node *)NULL);
    switch (r->type) {
    case N_compound:
    case N_lhs1:
    case N_rw1:
      q = sub0(r);
      assert (q->type == N_nameseq);
      if (s->type != N_selector1) goto noncall;
      y = symresolve(inner,q,&k);
      assert (k <= q->nsub);
      assert (y != (symbol *)NULL);
      if (y->type != procsym) goto noncall;
      bugerr("Can't assign to a call");
    default:
      goto noncall;
    }
  noncall:
    /* Note the resemblance to eqn 5, p. 93 Schwartz et al.: */
    e = eval(r);
    update_selected(e,s,x);  /* e s := x, where s is the selector */
    assign(r,e);
    v_etn(p);
    break;
  case N_expr:  /* parenthesized expression */
    assign(sub0(p),x);
    v_etn(p);
    break;
  default:  unexpected(p->type);  /* semantic analysis fuckup? */
  }
  v_etn(p);
} /* end assign */


/* ------------------------------------------------------------------ */

/* Selection */

/* .................................................................. */

/*
 *  e := x p, where p is a "selector", e.g. (s), {s}, (s1..s2)
 */
static void get_selected(e,x,p)
const char *e;  /* name for destination */
const char *x;  /* object being selected from */
node *p;        /* selector */
{
  int i;
  node *q,*q1,*q2;
  const char *s,*s1,*s2,*f,*g;
  token *t;
  symbol *y;
  v_tn(p);
  switch (p->type) {
  case N_selector1:  /* ( expr_list ) */
    s = tup_sel_if_nec(p);
    /* e := x(s), where x should be an smap, tuple, or string */
    v_etn(p);
    v_opcode(copy_from_smap_op);
    v_opnd_name(rd,x);
    v_opnd_name(rd,s);
    v_opnd_name(wr,e);
    break;
  case N_selector2:  /* { expr_list } */
    s = tup_sel_if_nec(p);
    /* e := x{s}, where x should be an mmap */
    v_etn(p);
    v_opcode(copy_from_mmap_op);
    v_opnd_name(rd,x);
    v_opnd_name(rd,s);
    v_opnd_name(wr,e);
    break;
  case N_selector3:  /* ( expr .. expr ) */
                     /* ( expr ..      ) */
                     /* (      .. expr ) */
    q1 = sub0(p);
    q2 = sub1(p);
    if (q1 == (node *)NULL) {
      s1 = integer_lit("1");
    } else {
      s1 = eval(q1);
    }
    if (q2 == (node *)NULL) {
      s2 = temp_name();
      v_call(system_name("#"));  /* "#" means cardinality */
      v_opnd_name(rd,x);
      v_opnd_name(wr,s2);
    } else {
      s2 = eval(q2);
    }
    /* e := x(s1..s2), where x should be a tuple or string */
    v_etn(p);
    v_opcode(copy_from_slice_op);
    v_opnd_name(rd,x);
    if (q1 == (node *)NULL) v_opnd_lit(s1);
    else                    v_opnd_name(rd,s1);
    v_opnd_name(rd,s2);
    v_opnd_name(wr,e);
    break;
  case N_selector4:  /* . nameseq */
    q = sub0(p);
    assert (q->type == N_nameseq);
    g = NULL;
    for (i=0; i<q->nsub; i++) {
      t = name_token(subi(q,i));
      y = symstackfind(inner,t->graphic);
      if (y != (symbol *)NULL && y->type == selsym) {
        s = full_name(y);
      } else {
        const char *ss = (tagcase == anycase) ? strmakelower(t->graphic) :
                                                       t->graphic;
        s = string_lit(strjoin3("\'",ss,"\'"));
      }
      f = (i==0) ? x : g;  /* i.e., the g of the prev loop iter */
      g = (i<q->nsub-1) ? temp_name() : e;
      v_etn((node *)t);
      v_opcode(copy_from_smap_op);
      v_opnd_name(rd,f);
      v_opnd_name(rd,s);
      v_opnd_name(wr,g);
    }
    break;
  default:  unexpected(p->type);
  }
  v_etn(p);
} /* end get_selected */

/* .................................................................. */

/*
 *  e p := x, where p is a "selector", e.g. (s), {s}, (s1..s2)
 */
static void update_selected(e,p,x)
const char *e;  /* subject */
node *p;        /* selector */
const char *x;  /* value to put in */
{
  node *q,*q1,*q2;
  const char *s,*s1,*s2;
  v_tn(p);
  switch (p->type) {
  case N_selector1:  /* ( expr_list ) */
    s = tup_sel_if_nec(p);
    /* e(s) := x, where e should be an smap, tuple, or string */
    v_etn(p);
    v_opcode(copy_into_smap_op);
    v_opnd_name(rd,x);
    v_opnd_name(rw,e);
    v_opnd_name(rd,s);
    break;
  case N_selector2:  /* { expr_list } */
    s = tup_sel_if_nec(p);
    /* e{s} := x, where e should be an mmap */
    v_etn(p);
    v_opcode(copy_into_mmap_op);
    v_opnd_name(rd,x);
    v_opnd_name(rw,e);
    v_opnd_name(rd,s);
    break;
  case N_selector3:  /* ( expr .. expr ) */
                     /* ( expr ..      ) */
                     /* (      .. expr ) */
    q1 = sub0(p);
    q2 = sub1(p);
    if (q1 == (node *)NULL) {
      s1 = integer_lit("1");
    } else {
      s1 = eval(q1);
    }
    if (q2 == (node *)NULL) {
      s2 = temp_name();
      v_call(system_name("#"));  /* "#" means cardinality */
      v_opnd_name(rd,e);
      v_opnd_name(wr,s2);
    } else {
      s2 = eval(q2);
    }
    /* e(s1..s2) := x, where e should be a tuple or string */
    v_etn(p);
    v_opcode(copy_into_slice_op);
    v_opnd_name(rd,x);
    v_opnd_name(rw,e);
    if (q1 == (node *)NULL) v_opnd_lit(s1);
    else                    v_opnd_name(rd,s1);
    v_opnd_name(rd,s2);
    break;
  case N_selector4:  /* . nameseq */
    q = sub0(p);
    assert (q->type == N_nameseq);
    upchuck(e,q,x,0);  /* see recursive service routine below */
    break;
  default:  unexpected(p->type);
  }
  v_etn(p);
} /* end update_selected */

static void upchuck(const char *e, node *q, const char *x, int i) {
  token *t;
  symbol *y;
  const char *s,*f;
  t = name_token(subi(q,i));
  y = symstackfind(inner,t->graphic);
  if (y != (symbol *)NULL && y->type == selsym) {
    s = full_name(y);
  } else {
    const char *ss = (tagcase == anycase) ? strmakelower(t->graphic) :
                                                         t->graphic;
    s = string_lit(strjoin3("\'",ss,"\'"));
  }
  v_etn((node *)t);
  if (i < q->nsub-1) {
    f = temp_name();
    v_opcode(copy_from_smap_op);
    v_opnd_name(rd,e);
    v_opnd_name(rd,s);
    v_opnd_name(wr,f);
    upchuck(f,q,x,i+1);
    v_opcode(copy_into_smap_op);
    v_opnd_name(rd,f);
    v_opnd_name(rw,e);
    v_opnd_name(rd,s);
  } else {
    v_opcode(copy_into_smap_op);
    v_opnd_name(rd,x);
    v_opnd_name(rw,e);
    v_opnd_name(rd,s);
  }
} /* end upchuck */

/* .................................................................. */

/*
 *  If the expression list under p has one element, evaluate the
 *  element.  Otherwise, evaluate p as if it were a tuple (this is
 *  for SETL's nice rule that, e.g., f{x,y} means f{[x,y]}).
 */
static const char *tup_sel_if_nec(p)
node *p;
{
  node *q,*r;
  const char *e;
  v_tn(p);
  q = sub0(p);
  assert (q->type == N_expr_list);
  if (q->nsub == 1) {
    e = eval(sub0(q));
  } else {
    r = node1(N_tuple1,q,q);
    e = eval(r);
    release(r);
  }
  v_etn(p);
  return(e);
}


/* ------------------------------------------------------------------ */

/*  Code-generation ("spewing") utilities:  */

/* ------------------------------------------------------------------ */

/*  Recall that each routine is a list of basic blocks.  */

/* .................................................................. */

static int genstate;
static int curtn;

/*
 *  Push all the lists "backwards" and later "fix" them to be forwards.
 *  This means we only have to keep one "current" pointer for the
 *  whole data structure while we're building it.  We use 'virtcode'
 *  for that.
 */

static void init_virtcode(void)
{
  virtcode = (csect *)NULL;
  newcsect();  /* sets 'virtcode' to pushed new csect */
  curtn = -1;
}

static void newcsect(void)
{
  csect *c;
  getmem(c,1,csect);
  c->blocks = (block *)NULL;
  c->next = virtcode;  /* link to old list head */
  virtcode = c;  /* set head to new csect */
  genstate = 0;  /* need new block */
}

static void newblock(void)
{
  block *p;
  getmem(p,1,block);
  p->labels = (label *)NULL;
  p->instrs = (instr *)NULL;
  p->next = virtcode->blocks;
  virtcode->blocks = p;
  genstate = 1;  /* can now use cur block */
}

static void endblock(void)
{
  genstate = 0;  /* end the current basic block */
}

static void newlabel(void)
{
  label *l;
  if (virtcode->blocks->instrs != (instr *)NULL) endblock();
  if (genstate == 0) newblock();
  getmem(l,1,label);
  l->tn = curtn;
  l->next = virtcode->blocks->labels;
  virtcode->blocks->labels = l;
}

static void newinstr(void)
{
  instr *i;
  if (genstate == 0) newblock();
  getmem(i,1,instr);
  i->tn = curtn;
  i->operas = (opera *)NULL;
  i->next = virtcode->blocks->instrs;
  virtcode->blocks->instrs = i;
}

static void newopera(void)
{
  opera *o;
  getmem(o,1,opera);
  o->next = virtcode->blocks->instrs->operas;
  virtcode->blocks->instrs->operas = o;
}

static void v_label(s)
const char *s;
{
  newlabel();
  virtcode->blocks->labels->lab = s;
}

/* control label */
static void v_label_ctrl(stem)
const char *stem;
{
  const char *t;
  t = strjoin3(stem,"_",ntoa(curscope->scopenum));
  v_label(control_label(t));
}

/* control sublabel */
static void v_label_ctrl_sub(stem,sub)
const char *stem;
int sub;
{
  const char *t;
  t = strjoin5(stem,"_",ntoa(curscope->scopenum),"_",ntoa(sub));
  v_label(control_label(t));
}

static void v_copy(from,to)
const char *from;
const char *to;
{
  if (lne(from,to)) {
    v_opcode(copy_op);
    v_opnd_name(rd,from);
    v_opnd_name(wr,to);
  }
}

static void v_copy_indexed(from,i,to)
const char *from;
int i;
const char *to;
{
  v_opcode(copy_indexed_op);
  v_opnd_name(rd,from);
  v_opnd_unsigned(i);
  v_opnd_name(wr,to);
}

static void v_routine(rot,to)
const char *rot;
const char *to;
{
  v_opcode(routine_op);
  v_opnd_name(unprefixed,rot);
  v_opnd_name(wr,to);
}

/*
 *  These 5 little "back door" routines just make sure to emit the
 *  right kind of "call" opcode (and the first operand, the callee name)
 *  for the kind of routine being (directly) called; note that this
 *  convention for naming "system" tags (the "S_" prefix) must agree
 *  with the 'system_name' routine below:
 */

#define is_sys_name(rot) ((rot)[0] == 'S' && (rot)[1] == '_')

static void v_call(rot)
const char *rot;
{
  v_opcode(is_sys_name(rot) ? scall_op
                            :  call_op);
  v_opnd_name(unprefixed,rot);
}

static void v_vcall(rot)
const char *rot;
{
  v_opcode(is_sys_name(rot) ? vscall_op
                            :  vcall_op);
  v_opnd_name(unprefixed,rot);
}

static void v_call_assigning(rot)
const char *rot;
{
  v_opcode(is_sys_name(rot) ? scall_assigning_op
                            :  call_assigning_op);
  v_opnd_name(unprefixed,rot);
}

static void v_call_bincomb(rot)
const char *rot;
{
  v_opcode(is_sys_name(rot) ? scall_bincomb_op
                            :  call_bincomb_op);
  v_opnd_name(unprefixed,rot);
}

static void v_call_uncomb(rot)
const char *rot;
{
  v_opcode(is_sys_name(rot) ? scall_uncomb_op
                            :  call_uncomb_op);
  v_opnd_name(unprefixed,rot);
}

static void v_end(rot)
const char *rot;
{
  v_opcode(end_op);
  v_opnd_name(unprefixed,rot);
}

static void v_opcode(op)
optype op;
{
  newinstr();
  virtcode->blocks->instrs->opcode = op;
}

static void v_opnd(type,acc,libretto)
operatype type;
bits acc;
const char *libretto;
{
  opera *p;
  newopera();
  p = virtcode->blocks->instrs->operas;
  p->type = type;
  p->acc = acc;
  p->libretto = libretto;
  if (type == label_opera) endblock();
}

static void v_opnd_name(acc,name)
bits acc;
const char *name;
{
  v_opnd(deno_opera,acc,name);
}

static void v_opnd_lit(s)
const char *s;
{
  v_opnd_name(rd,s);
}

static void v_opnd_reg(acc,s)
bits acc;
const char *s;
{
  v_opnd_name(acc,s);
}

static void v_opnd_label(s)
const char *s;
{
  v_opnd(label_opera,0,s);
}

/* Non-negative integer operand */
static void v_opnd_unsigned(n)
int n;
{
  v_opnd(unsigned_opera,0,ntoa(n));
}

/* Control label operand in a specified scope */
static void v_opnd_ctrl_scope(stem,scopenum)
const char *stem;
int scopenum;
{
  const char *t;
  t = strjoin3(stem,"_",ntoa(scopenum));
  v_opnd(label_opera,0,control_label(t));
}

/* Control label operand in the current scope */
static void v_opnd_ctrl(stem)
const char *stem;
{
  const char *t;
  t = strjoin3(stem,"_",ntoa(curscope->scopenum));
  v_opnd(label_opera,0,control_label(t));
}

/* Control sublabel operand in the current scope */
static void v_opnd_ctrl_sub(stem,sub)
const char *stem;
int sub;
{
  const char *t;
  t = strjoin5(stem,"_",ntoa(curscope->scopenum),"_",ntoa(sub));
  v_opnd(label_opera,0,control_label(t));
}

/* Make 'curtn' reflect the "current token" in the source code */
static void v_tn(p)
node *p;
{
  if (p == (node *)NULL) return;
  if (p->tn > curtn) curtn = p->tn;
}

/* Point 'curtn' at the last token under the current node */
static void v_etn(p)
node *p;
{
  int i,n;
  if (p == (node *)NULL) return;
  if (p->type == N_Token) v_tn(p);
  else {
    n = p->nsub;
    for (i=n-1; i>=0; i--) {
      if (subi(p,i) != (node *)NULL) {
        v_etn(subi(p,i));
        return;
      }
    }
  }
}

#define fix_list(x,type) \
  { type *p,*q,*r; \
    p = x; \
    r = (type *)NULL; \
    while (p != (type *)NULL) { \
      q = p->next; \
      p->next = r; \
      r = p; \
      p = q; \
    } \
    x = r; \
  }

static void fix_virtcode(void)
{
  csect *c;
  block *b;
  instr *i;
  fix_list(virtcode,csect);
  for (c=virtcode; c!=(csect *)NULL; c=c->next) {
    fix_list(c->blocks,block);
    for (b=c->blocks; b!=(block *)NULL; b=b->next) {
      fix_list(b->instrs,instr);
      for (i=b->instrs; i!=(instr *)NULL; i=i->next) {
        fix_list(i->operas,opera);
      }
    }
  }
}


/* ------------------------------------------------------------------ */

/* Miscellaneous Support */

/* .................................................................. */

/*
 *  The fully qualified name for a given symbol
 */
static const char *full_name(y)
symbol *y;
{
  if (y->owner != (symbol *)NULL) {
    switch (y->owner->type) {
    case packsym:
    case progsym:
      return(user_name(strjoin3(y->owner->tag,".",y->tag)));
    case procsym:
    case bopsym:
    case uopsym:
    case cmdsym:
      return(user_name(y->tag));
    default:
      unexpected(y->owner->type);
    }
  } else {
    return(system_name(y->tag));
  }
}

/* .................................................................. */

/*
 *  Emit declarative stmts for the non-routine members of a symbol table
 */
static void symdcls(t)
symtab t;
{
  datum *d;
  symbol *y;
  const char *s;
  int i,n;
  n = size(t);
  for (i=1; i<=n; i++) {
    check (ordsub(t,i,&d));
    y = d->symptr;
    switch (y->type) {
    case parsym:
    case varsym:
    case initsym:
    case constsym:
    case selsym:
      assert (y->owner != (symbol *)NULL);
      switch (y->owner->type) {
      case packsym:
      case progsym:
        v_opcode(global_op);
        break;
      case procsym:
      case bopsym:
      case uopsym:
      case cmdsym:
        v_opcode(local_op);
        break;
      default:  unexpected(y->owner->type);
      }
      s = full_name(y);
      v_opnd_name(unprefixed,s);
      if (y->ability & backtrack) {
        v_opcode(backtrack_op);
        v_opnd_name(unprefixed,s);
      }
      break;
    default:
      break;
    }
  }
}

/* .................................................................. */

/*
 *  "Split" a heterogeneous nameseq into the equivalent application-type
 *  node
 */
static node *split(p,k)
node *p;
int k;
{
  int i;
  node *q,*l,*r,*sl,*sr;
  token *tl,*tr;
  nodetype type;
  q = sub0(p);
  assert (q->type == N_nameseq);
  assert (k < q->nsub);
  switch (p->type) {  /* for some kind of pseudo-consistency */
  case N_compound:  type = N_applic; break;
  case N_lhs1:  type = N_lhs3;   break;
  case N_rw1:   type = N_rw3;    break;
  default:      unexpected(p->type);
  }
  tl = name_token(sub0(q));     /* just for sake of tn */
  tr = name_token(subi(q,k));
  sl = noden(N_nameseq,(node *)tl,k);
  sr = noden(N_nameseq,(node *)tr,q->nsub-k);
  for (i=0; i<k;       i++) subi(sl,i  ) = subi(q,i);
  for (i=k; i<q->nsub; i++) subi(sr,i-k) = subi(q,i);
  l = node1(p->type,    (node *)tl,sl);
  r = node1(N_selector4,(node *)tr,sr);
  return(node2(type,p,l,r));
}

/* .................................................................. */

/*
 *  Embedded C++ code
 */
static void embed_c_code(s)
const char *s;
{
  v_opcode(cplusplus_code_op);
  v_opnd(c_code_opera,0,s);
}

/* .................................................................. */

/*
 *  Embedded C++ code intended to end up at file scope
 */
static void embed_c_include(s)
const char *s;
{
  v_opcode(cplusplus_include_op);
  v_opnd(c_code_opera,0,s);
}

/* .................................................................. */

/*
 *  Code to begin a program
 */
static void progtop(s)
const char *s;
{
  newcsect();
  tempinit();
  v_opcode(mainproc_op);
  v_opnd_name(unprefixed,s);
}

/* .................................................................. */

/*
 *  Code to begin a routine
 */
static void proctop(s)
const char *s;
{
  newcsect();
  tempinit();
  v_opcode(proc_op);
  v_opnd_name(unprefixed,s);
}

/* .................................................................. */

/*
 *  Code to begin a variadic routine
 */
static void vproctop(s)
const char *s;
{
  newcsect();
  tempinit();
  v_opcode(vproc_op);
  v_opnd_name(unprefixed,s);
}

/* .................................................................. */

/*
 *  Initialize generator of temporary variable names
 */
static void tempinit(void)
{
  curtemp = 0;
}

/* .................................................................. */

/*
 *  Create a new temporary variable and make a string representing it
 */
static const char *temp_name(void)
{
  curtemp++;
  return(strjoin("T_",ntoa(curtemp)));
}

/* .................................................................. */

/*
 *  Make a string representing a user tag
 */
static const char *user_name(s)
const char *s;
{
  return(strjoin("U_",s));
}

/* .................................................................. */

/*
 *  Make a string representing a system tag
 */
static const char *system_name(s)
const char *s;
{
  return(strjoin("S_",strmakeupper(s)));
}

/* .................................................................. */

/*
 *  Make a string representing the garbage can
 */
static const char *dummy_name(void)
{
  return("-");  /* natural enough, eh? */
}

/* .................................................................. */

/*
 *  Make a string representing the "return" register
 */
static const char *return_reg(void)
{
  return("RET");
}

/* .................................................................. */

/*
 *  Make a string representing the "status" register
 */
static const char *status_reg(void)
{
  return("STATUS");
}

/* .................................................................. */

/*
 *  Make a string representing the "machine code" register
 */
static const char *machine_reg(void)
{
  return("MACHINE");
}

/* .................................................................. */

/*
 *  Make a string representing a user label
 */
static const char *user_label(s)
const char *s;
{
  return(strjoin("L_",s));
}

/* .................................................................. */

/*
 *  Make a string representing a refinement label
 */
static const char *refinement_label(s)
const char *s;
{
  return(strjoin("RL_",s));
}

/* .................................................................. */

/*
 *  Make a string representing a refinement return label
 */
static const char *refret_label(s)
const char *s;
{
  return(strjoin("RR_",s));
}

/* .................................................................. */

/*
 *  Make a string representing a control label or sublabel
 */
static const char *control_label(s)
const char *s;
{
  return(strjoin("C_",s));
}

/* .................................................................. */

/*
 *  Make a string representing the routine return label
 */
static const char *return_label(void)
{
  return("C_return");
}

/* .................................................................. */

/*
 *  Make a string representing an integer literal
 */
static const char *integer_lit(s)
const char *s;
{
  return(strjoin("I_",s));
}

/* .................................................................. */

/*
 *  Make a string representing a real literal
 */
static const char *real_lit(s)
const char *s;
{
  return(strjoin("R_",s));
}

/* .................................................................. */

/*
 *  Make a string representing a string literal
 */
static const char *string_lit(s)
const char *s;
{
  return(s);
}

/* .................................................................. */

/*
 *  Make a string representing TRUE
 */
static const char *true_lit(void)
{
  return(system_name("TRUE"));
}

/* .................................................................. */

/*
 *  Make a string representing FALSE
 */
static const char *false_lit(void)
{
  return(system_name("FALSE"));
}

/* .................................................................. */

/*
 *  Make a string representing OM
 */
static const char *om_lit(void)  /* with cheese? */
{
  return(system_name("OM"));
}

/* .................................................................. */

/*
 *  Make a string representing the empty tuple
 */
static const char *null_tuple_lit(void)
{
  return("[]");
}

/* .................................................................. */

/*
 *  Make a string representing the empty set
 */
static const char *empty_set_lit(void)
{
  return("{}");
}

/* .................................................................. */

/*
 *  Zits for managing CASE statement code emission (too bad about that
 *  LALR(1) parser, eh?)...
 */

static void push_case_ex_reg(reg)
const char *reg;
{
  struct case_ex_reg *a;
  getmem(a,1,struct case_ex_reg);
  a->reg = reg;
  a->prev = case_ex_reg_stack;
  case_ex_reg_stack = a;
}
static const char *cur_case_ex_reg(void)
{
  return(case_ex_reg_stack->reg);
}
static void pop_case_ex_reg(void)
{
  struct case_ex_reg *a;
  a = case_ex_reg_stack;
  case_ex_reg_stack = a->prev;
  release(a);  /* a no-op given that we're in "ephemeral mode" */
}

/* .................................................................. */

/*
 *  Another such "manager", for the EXPR/YIELD abomination...
 *  This one's not absolutely necessary, but more convenient than
 *  passing a name down through all the levels of statement and
 *  expression processing.
 */

static void push_yield_reg(reg)
const char *reg;
{
  struct yield_reg *a;
  getmem(a,1,struct yield_reg);
  a->reg = reg;
  a->prev = yield_reg_stack;
  yield_reg_stack = a;
}
static const char *cur_yield_reg(void)
{
  return(yield_reg_stack->reg);
}
static void pop_yield_reg(void)
{
  struct yield_reg *a;
  a = yield_reg_stack;
  yield_reg_stack = a->prev;
  release(a);  /* a no-op given that we're in "ephemeral mode" */
}

/* .................................................................. */

/*
 *  This little manager is like the above two, but gives us
 *  the association between tn and scope number for the
 *  beginnings of loops, starting with the current innermost
 *  at the stacktop.  Thus when we encounter a QUIT or CONTINUE
 *  statment, we can find the proper "term" or "start" label
 *  to branch to based on the raw token pointer we get from
 *  the parser on these statements.
 */

static void push_loop_tn_scope(tn)
int tn;
{
  struct loop_tn_scope *a;
  getmem(a,1,struct loop_tn_scope);
  a->tn = tn;
  a->scopenum = curscope->scopenum;
  a->prev = loop_tn_scope_stack;
  loop_tn_scope_stack = a;
}
static int find_loop_tn_scope(tn)
int tn;
{
  struct loop_tn_scope *p;
  for (p = loop_tn_scope_stack;
       p != (struct loop_tn_scope *)NULL; 
       p = p->prev) {
    if (p->tn == tn) return(p->scopenum);
  }
  bugerr("Couldn't find loop header tn %d", tn);
}
static void pop_loop_tn_scope(void)
{
  struct loop_tn_scope *a;
  a = loop_tn_scope_stack;
  loop_tn_scope_stack = a->prev;
  release(a);  /* a no-op given that we're in "ephemeral mode" */
}
