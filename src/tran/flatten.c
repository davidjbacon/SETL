/*  ===  List flattening  ==========================================  */

/*  $Id: flatten.c,v 1.6 2010/09/19 14:58:49 bacon Exp $  */

/*  Free software (c) dB - see file COPYING for license (GPL).  */


/*
 *  The glossies on yacc say that left recursion is noble because
 *  it reduces the chances of a yak stacc overflow.  It has the
 *  side-effect of making "lists" of things look in the parse
 *  tree like long man o' war tails hanging down to the left.
 *  This flattening spam is to turn them into something a bit more
 *  tuple-like, and will have the side effect of reducing the
 *  chances of a C stack overflow in the rest of semantic analysis
 *  and code generation.
 */


/* ------------------------------------------------------------------ */

#include "setltran.h"

/* Local routines */
static void f_list(nodetype listtype, node **np);
static void f_compilation_unit(node *p);
static void f_use(node *p);
static void f_routine_spec(node *p);
static void f_routine(node *p);
static void f_body(node *p);
static void f_decl(node *p);
static void f_var(node *p);
static void f_ascription(node *p);
static void f_initial(node *p);
static void f_sel(node *p);
static void f_refinement(node *p);
static void f_stmt(node *p);
static void f_elif(node *p);
static void f_when_stmt(node *p);
static void f_loop_head(node *p);
static void f_loop_init(node *p);
static void f_loop_doing(node *p);
static void f_loop_while(node *p);
static void f_loop_step(node *p);
static void f_loop_until(node *p);
static void f_loop_term(node *p);
static void f_iterator(node *p);
static void f_simpliter(node *p);
static void f_compound_or_predef(node *p);
static void f_lhs_or_call(node *p);
static void f_lhs(node *p);
static void f_rw(node *p);
static void f_lhs_or_dash(node *p);
static void f_syscall(node *p);
static void f_selector(node *p);
static void f_expr(node *p);
static void f_term(node *p);
static void f_operand(node *p);
static void f_command(node *p);
static void f_cmdarg(node *p);
static void f_compound(node *p);
static void f_former(node *p);
static void f_case_expr(node *p);
static void f_choice(node *p);
static void f_if_expr(node *p);
static void f_elif_expr(node *p);


/* ------------------------------------------------------------------ */

/*
 *  This routine does all the work; it turns a man o' war tail
 *  into a nice plain array.  If the "tail" is a null pointer, it
 *  changes it to an array of length 0.  These arrays will still be
 *  called "lists", as in Icon; I'll reserve the word "tuple" for the
 *  SETL kind.
 */
static void f_list(listtype,np)
nodetype listtype;
node **np;  /* The man o' war tail to replace */
{
  int i,n;
  node *p,*q,*r;
  p = *np;
  n = 0;
  while (p != (node *)NULL) {
    assert (p->type == listtype);
    p = sub0(p);
    n = n + 1;
  }
  p = *np;
  r = noden(listtype,p,n);
  i = n;
  while (p != (node *)NULL) {
    i = i - 1;
    subi(r,i) = sub1(p);
    q = sub0(p);
    release(p);  /* a no-op given that we're in "ephemeral mode" */
    p = q;
  }
  *np = r;
} /* end f_list */

/* .................................................................. */

/* 'flatten' calls her subjects using these: */
#define f_opt(f,p) if ((p) != (node *)NULL) f(p)
#define f_mand(f,p) f(p)
#define f_meta(f,p) {int i; for (i=0; i<(p)->nsub; i++) f(subi(p,i));}

/* .................................................................. */

void flatten(p)
node *p;
{
  switch (p->type) {
  case N_unnamed_program:
    f_mand(f_body,sub0(p));
    f_list(N_routines,&sub1(p));
    f_meta(f_routine,sub1(p));
    break;
  case N_complex_program:
    f_list(N_compilation_units,&sub0(p));
    f_meta(f_compilation_unit,sub0(p));
    break;
  default:  unexpected(p->type);
  }
}

/* .................................................................. */

static void f_compilation_unit(p)
node *p;
{
  switch (p->type) {
  case N_package_spec:
    /* nothing needful for the opt_c_code */
    f_list(N_decls,&sub2(p));
    f_meta(f_decl,sub2(p));
    f_list(N_routine_specs,&sub3(p));
    f_meta(f_routine_spec,sub3(p));
    break;
  case N_package_body:
    f_list(N_uses,&sub2(p));
    f_meta(f_use,sub2(p));
    f_list(N_decls,&sub3(p));
    f_meta(f_decl,sub3(p));
    f_list(N_routines,&sub4(p));
    f_meta(f_routine,sub4(p));
    break;
  case N_program_unit:
    f_list(N_uses,&sub1(p));
    f_meta(f_use,sub1(p));
    f_mand(f_body,sub2(p));
    f_list(N_routines,&sub3(p));
    f_meta(f_routine,sub3(p));
    break;
  default:  unexpected(p->type);
  }
}

/* .................................................................. */

static void f_use(p)
node *p;
{
  switch (p->type) {
  case N_use:
    f_list(N_name_list,&sub0(p));
    break;
  default:  unexpected(p->type);
  }
}

/* .................................................................. */

static void f_routine_spec(p)
node *p;
{
  switch (p->type) {
  case N_proc_spec:
  case N_cmd_spec:
    f_list(N_form_specs,&sub1(p));
    break;
  case N_binop_spec:
  case N_unop_spec:
    break;
  default:  unexpected(p->type);
  }
}

/* .................................................................. */

static void f_routine(p)
node *p;
{
  switch (p->type) {
  case N_procdef:
  case N_cmddef:
    f_list(N_form_specs,&sub1(p));
    f_mand(f_body,sub3(p));
    break;
  case N_binopdef:
    f_mand(f_body,sub3(p));
    break;
  case N_unopdef:
    f_mand(f_body,sub2(p));
    break;
  default:  unexpected(p->type);
  }
}

/* .................................................................. */

static void f_body(p)
node *p;
{
  switch (p->type) {
  case N_body:
    f_list(N_decls,&sub0(p));
    f_meta(f_decl,sub0(p));
    f_list(N_stmts,&sub1(p));
    f_meta(f_stmt,sub1(p));
    f_list(N_refinements,&sub2(p));
    f_meta(f_refinement,sub2(p));
    break;
  default:  unexpected(p->type);
  }
}

/* .................................................................. */

static void f_decl(p)
node *p;
{
  switch (p->type) {
  case N_var_decl:
    f_list(N_var_list,&sub0(p));
    f_meta(f_var,sub0(p));
    break;
  case N_const_decl:
    f_list(N_ascription_list,&sub0(p));
    f_meta(f_ascription,sub0(p));
    break;
  case N_init_decl:
    f_list(N_initial_list,&sub0(p));
    f_meta(f_initial,sub0(p));
    break;
  case N_sel_decl:
    f_list(N_sel_list,&sub0(p));
    f_meta(f_sel,sub0(p));
    break;
  case N_repr_decl:
    break;
  default:  unexpected(p->type);
  }
}

/* .................................................................. */

static void f_var(p)
node *p;
{
  switch (p->type) {
  case N_var:
    f_opt(f_expr,sub1(p));
    break;
  default:  unexpected(p->type);
  }
}

/* .................................................................. */

static void f_ascription(p)
node *p;
{
  switch (p->type) {
  case N_ascription:
    f_opt(f_expr,sub1(p));
    break;
  default:  unexpected(p->type);
  }
}

/* .................................................................. */

static void f_initial(p)
node *p;
{
  switch (p->type) {
  case N_initial:
    f_mand(f_expr,sub1(p));
    break;
  default:  unexpected(p->type);
  }
}

/* .................................................................. */

static void f_sel(p)
node *p;
{
  switch (p->type) {
  case N_sel:
    f_mand(f_expr,sub1(p));
    break;
  default:  unexpected(p->type);
  }
}

/* .................................................................. */

static void f_refinement(p)
node *p;
{
  switch (p->type) {
  case N_refinement:
    f_list(N_stmts,&sub1(p));
    f_meta(f_stmt,sub1(p));
    break;
  default:  unexpected(p->type);
  }
}

/* .................................................................. */

static void f_stmt(p)
node *p;
{
  switch (p->type) {
  case N_labelled_stmt:
    f_mand(f_stmt,sub1(p));
    break;
  case N_continue_stmt:
  case N_fail_stmt:
  case N_goto_stmt:
  case N_pass_stmt:
  case N_quit_stmt:
  case N_succeed_stmt:
  case N_machine_stmt:
  case N_embedded_c_code:
  case N_null_stmt:
    break;
  case N_assert_stmt:
  case N_yield_stmt:
    f_mand(f_expr,sub0(p));
    break;
  case N_return_stmt:
  case N_stop_stmt:
    f_opt(f_expr,sub0(p));
    break;
  case N_if_stmt:
    f_mand(f_expr,sub0(p));
    f_list(N_stmts,&sub1(p));
    f_meta(f_stmt,sub1(p));
    f_list(N_elifs,&sub2(p));
    f_meta(f_elif,sub2(p));
    f_list(N_stmts,&sub3(p));
    f_meta(f_stmt,sub3(p));
    break;
  case N_case_of_stmt:
    f_list(N_stmts,&sub0(p));
    f_meta(f_stmt,sub0(p));
    f_list(N_stmts,&sub1(p));
    f_meta(f_stmt,sub1(p));
    break;
  case N_case_ex_stmt:
    f_mand(f_expr,sub0(p));
    f_list(N_stmts,&sub1(p));
    f_meta(f_stmt,sub1(p));
    f_list(N_stmts,&sub2(p));
    f_meta(f_stmt,sub2(p));
    break;
  case N_case_of_when_stmt:
    f_list(N_when_stmts,&sub0(p));
    f_meta(f_when_stmt,sub0(p));
    f_list(N_stmts,&sub1(p));
    f_meta(f_stmt,sub1(p));
    break;
  case N_case_ex_when_stmt:
    f_mand(f_expr,sub0(p));
    f_list(N_when_stmts,&sub1(p));
    f_meta(f_when_stmt,sub1(p));
    f_list(N_stmts,&sub2(p));
    f_meta(f_stmt,sub2(p));
    break;
  case N_choice_stmt:
    f_list(N_expr_list,&sub0(p));
    f_meta(f_expr,sub0(p));
    f_mand(f_stmt,sub1(p));
    break;
  case N_loop_stmt:
    f_opt(f_iterator,sub0(p));
    f_opt(f_loop_head,sub1(p));
    f_list(N_stmts,&sub2(p));
    f_meta(f_stmt,sub2(p));
    break;
  case N_assignment_stmt:
    f_mand(f_lhs_or_call,sub0(p));
    f_mand(f_expr,sub2(p));
    break;
  case N_call_stmt:
    f_mand(f_lhs_or_call,sub0(p));
    break;
  case N_syscall_stmt:
    f_list(N_expr_list,&sub1(p));
    f_meta(f_expr,sub1(p));
    break;
  case N_from_stmt:
    f_mand(f_lhs_or_call,sub0(p));
    f_mand(f_rw,sub2(p));
    break;
  case N_indirect_call_stmt:
    f_mand(f_expr,sub0(p));
    f_list(N_expr_list,&sub1(p));
    f_meta(f_expr,sub1(p));
    break;
  case N_cmd_stmt:
    f_mand(f_command,sub0(p));
    break;
  case N_expr_stmt:
    f_mand(f_expr,sub0(p));
    break;
  default:  unexpected(p->type);
  }
} /* end f_stmt */

/* .................................................................. */

static void f_elif(p)
node *p;
{
  switch (p->type) {
  case N_elif:
    f_mand(f_expr,sub0(p));
    f_list(N_stmts,&sub1(p));
    f_meta(f_stmt,sub1(p));
    break;
  default:  unexpected(p->type);
  }
}

/* .................................................................. */

static void f_when_stmt(p)
node *p;
{
  switch (p->type) {
  case N_when_stmt:
    f_list(N_expr_list,&sub0(p));
    f_meta(f_expr,sub0(p));
    f_list(N_stmts,&sub1(p));
    f_meta(f_stmt,sub1(p));
    break;
  default:  unexpected(p->type);
  }
}

/* .................................................................. */

static void f_loop_head(p)
node *p;
{
  switch (p->type) {
  case N_loop_head:
    f_opt(f_loop_init, sub0(p));
    f_opt(f_loop_doing,sub1(p));
    f_opt(f_loop_while,sub2(p));
    f_opt(f_loop_step, sub3(p));
    f_opt(f_loop_until,sub4(p));
    f_opt(f_loop_term, sub5(p));
    break;
  default:  unexpected(p->type);
  }
}

/* .................................................................. */

static void f_loop_init(p)
node *p;
{
  switch (p->type) {
  case N_loop_init:
    f_list(N_stmts,&sub0(p));
    f_meta(f_stmt,sub0(p));
    break;
  default:  unexpected(p->type);
  }
}

/* .................................................................. */

static void f_loop_doing(p)
node *p;
{
  switch (p->type) {
  case N_loop_doing:
    f_list(N_stmts,&sub0(p));
    f_meta(f_stmt,sub0(p));
    break;
  default:  unexpected(p->type);
  }
}

/* .................................................................. */

static void f_loop_while(p)
node *p;
{
  switch (p->type) {
  case N_loop_while:
    f_mand(f_expr,sub0(p));
    break;
  default:  unexpected(p->type);
  }
}

/* .................................................................. */

static void f_loop_step(p)
node *p;
{
  switch (p->type) {
  case N_loop_step:
    f_list(N_stmts,&sub0(p));
    f_meta(f_stmt,sub0(p));
    break;
  default:  unexpected(p->type);
  }
}

/* .................................................................. */

static void f_loop_until(p)
node *p;
{
  switch (p->type) {
  case N_loop_until:
    f_mand(f_expr,sub0(p));
    break;
  default:  unexpected(p->type);
  }
}

/* .................................................................. */

static void f_loop_term(p)
node *p;
{
  switch (p->type) {
  case N_loop_term:
    f_list(N_stmts,&sub0(p));
    f_meta(f_stmt,sub0(p));
    break;
  default:  unexpected(p->type);
  }
}

/* .................................................................. */

static void f_iterator(p)
node *p;
{
  switch (p->type) {
  case N_iterator:
    f_list(N_simpliter_list,&sub0(p));
    f_meta(f_simpliter,sub0(p));
    f_opt(f_expr,sub1(p));
    break;
  default:  unexpected(p->type);
  }
}

/* .................................................................. */

static void f_simpliter(p)
node *p;
{
  switch (p->type) {
  case N_simpliter1:
    f_mand(f_lhs,sub0(p));
    f_mand(f_expr,sub1(p));
    break;
  case N_simpliter2:
  case N_simpliter3:
    f_mand(f_lhs,sub0(p));
    f_mand(f_compound_or_predef,sub1(p));
    f_list(N_lhs_list,&sub2(p));
    f_meta(f_lhs,sub2(p));
    break;
  default:  unexpected(p->type);
  }
}

/* .................................................................. */

static void f_compound_or_predef(p)
node *p;
{
  switch (p->type) {
  case N_Token:
    break;
  case N_compound:
    f_compound(p);
    break;
  default:  unexpected(p->type);
  }
}

/* .................................................................. */

static void f_lhs_or_call(p)
node *p;
{
  switch (p->type) {
  case N_lhs1:
    f_list(N_nameseq,&sub0(p));
    break;
  case N_lhs2:
    f_list(N_lhs_or_dash_list,&sub0(p));
    f_meta(f_lhs_or_dash,sub0(p));
    break;
  case N_lhs3:
    f_mand(f_lhs_or_call,sub0(p));
    f_opt(f_selector,sub1(p));
    break;
  case N_Token:
    /* Currently used just for Sysvar */
    break;
  default:  unexpected(p->type);
  }
}

/* .................................................................. */

static void f_lhs(p)
node *p;
{
  switch (p->type) {
  case N_lhs1:
    f_list(N_nameseq,&sub0(p));
    break;
  case N_lhs2:
    f_list(N_lhs_or_dash_list,&sub0(p));
    f_meta(f_lhs_or_dash,sub0(p));
    break;
  case N_lhs3:
    f_mand(f_lhs,sub0(p));
    f_mand(f_selector,sub1(p));
    break;
  case N_Token:
    /* Currently used just for Sysvar */
    break;
  default:  unexpected(p->type);
  }
}

/* .................................................................. */

static void f_rw(p)
node *p;
{
  switch (p->type) {
  case N_rw1:
    f_list(N_nameseq,&sub0(p));
    break;
  case N_rw2:
    f_list(N_rw_list,&sub0(p));
    f_meta(f_rw,sub0(p));
    break;
  case N_rw3:
    f_mand(f_rw,sub0(p));
    f_mand(f_selector,sub1(p));
    break;
  case N_Token:
    /* Currently used just for Sysvar */
    break;
  default:  unexpected(p->type);
  }
}

/* .................................................................. */

static void f_lhs_or_dash(p)
node *p;
{
  switch (p->type) {
  case N_Token:
    break;
  case N_lhs1:
  case N_lhs2:
  case N_lhs3:
    f_lhs(p);
    break;
  default:  unexpected(p->type);
  }
}

/* .................................................................. */

static void f_syscall(p)
node *p;
{
  switch (p->type) {
  case N_syscall:
    f_list(N_expr_list,&sub1(p));
    f_meta(f_expr,sub1(p));
    break;
  default:  unexpected(p->type);
  }
}

/* .................................................................. */

static void f_selector(p)
node *p;
{
  switch (p->type) {
  case N_selector1:
  case N_selector2:
    f_list(N_expr_list,&sub0(p));
    f_meta(f_expr,sub0(p));
    break;
  case N_selector3:
    f_opt(f_expr,sub0(p));
    f_opt(f_expr,sub1(p));
    break;
  case N_selector4:
    f_list(N_nameseq,&sub0(p));
    break;
  default:  unexpected(p->type);
  }
}

/* .................................................................. */

static void f_expr(p)
node *p;
{
  switch (p->type) {
  case N_assignment:
    f_mand(f_operand,sub0(p));
    f_mand(f_expr,sub2(p));
    break;
  default:
    f_term(p);
    break;
  }
}

/* .................................................................. */

static void f_term(p)
node *p;
{
  switch (p->type) {
  case N_binary:
  case N_bincomb:
    f_mand(f_term,sub0(p));
    f_mand(f_term,sub2(p));
    break;
  case N_unary:
  case N_uncomb:
    f_mand(f_term,sub1(p));
    break;
  default:
    f_operand(p);
    break;
  }
}

/* .................................................................. */

static void f_operand(p)
node *p;
{
  switch (p->type) {
  case N_Token:
    break;
  case N_compound:
    f_compound(p);
    break;
  case N_syscall:
    f_syscall(p);
    break;
  case N_expr:
    f_mand(f_expr,sub0(p));
    break;
  case N_case_of_expr:
  case N_case_ex_expr:
    f_case_expr(p);
    break;
  case N_if_expr:
    f_if_expr(p);
    break;
  case N_tuple1:
  case N_set1:
    f_list(N_expr_list,&sub0(p));
    f_meta(f_expr,sub0(p));
    break;
  case N_tuple2:
  case N_set2:
    f_mand(f_former,sub0(p));
    break;
  case N_applic:
    f_mand(f_operand,sub0(p));
    f_opt(f_selector,sub1(p));
    break;
  case N_expr_clause:
    f_list(N_stmts,&sub0(p));
    f_meta(f_stmt,sub0(p));
    break;
  case N_exists:
  case N_notexists:
  case N_forall:
    f_list(N_simpliter_list,&sub0(p));
    f_meta(f_simpliter,sub0(p));
    f_mand(f_expr,sub1(p));
    break;
  case N_procval:
    f_mand(f_compound,sub0(p));
    break;
  case N_cmdval:
    break;
  case N_indirect_call:
    f_mand(f_expr,sub0(p));
    f_list(N_expr_list,&sub1(p));
    f_meta(f_expr,sub1(p));
    break;
  case N_command:
    f_command(p);
    break;
  default:  unexpected(p->type);
  }
} /* end f_operand */

/* .................................................................. */

static void f_command(p)
node *p;
{
  switch (p->type) {
  case N_command:
    f_list(N_cmdargs,&sub1(p));
    f_meta(f_cmdarg,sub1(p));
    break;
  default:  unexpected(p->type);
  }
}

/* .................................................................. */

static void f_cmdarg(p)
node *p;
{
  switch (p->type) {
  case N_Token:
    break;
  case N_compound:
    f_compound(p);
    break;
  case N_expr:
    f_mand(f_expr,sub0(p));
    break;
  case N_tuple1:
  case N_set1:
    f_list(N_expr_list,&sub0(p));
    f_meta(f_expr,sub0(p));
    break;
  case N_tuple2:
  case N_set2:
    f_mand(f_former,sub0(p));
    break;
  default:  unexpected(p->type);
  }
} /* end f_cmdarg */

/* .................................................................. */

static void f_compound(p)
node *p;
{
  switch (p->type) {
  case N_compound:
    f_list(N_nameseq,&sub0(p));
    break;
  default:  unexpected(p->type);
  }
}

/* .................................................................. */

static void f_former(p)
node *p;
{
  switch (p->type) {
  case N_former1:
    f_mand(f_expr,sub0(p));
    f_opt(f_iterator,sub1(p));
    f_mand(f_loop_head,sub2(p));
    break;
  case N_former2:
    f_list(N_expr_list,&sub0(p));
    f_meta(f_expr,sub0(p));
    f_mand(f_expr,sub1(p));
    break;
  case N_former3:
    f_mand(f_operand,sub0(p));
    f_mand(f_expr,sub1(p));
    f_mand(f_expr,sub2(p));
    break;
  default:  unexpected(p->type);
  }
}

/* .................................................................. */

static void f_case_expr(p)
node *p;
{
  switch (p->type) {
  case N_case_of_expr:
    f_list(N_choices,&sub0(p));
    f_meta(f_choice,sub0(p));
    f_mand(f_expr,sub1(p));
    break;
  case N_case_ex_expr:
    f_mand(f_expr,sub0(p));
    f_list(N_choices,&sub1(p));
    f_meta(f_choice,sub1(p));
    f_mand(f_expr,sub2(p));
    break;
  default:  unexpected(p->type);
  }
}

/* .................................................................. */

static void f_choice(p)
node *p;
{
  switch (p->type) {
  case N_choice:
    f_list(N_expr_list,&sub0(p));
    f_meta(f_expr,sub0(p));
    f_mand(f_expr,sub1(p));
    break;
  default:  unexpected(p->type);
  }
}

/* .................................................................. */

static void f_if_expr(p)
node *p;
{
  switch (p->type) {
  case N_if_expr:
    f_mand(f_expr,sub0(p));
    f_mand(f_expr,sub1(p));
    f_list(N_elif_exprs,&sub2(p));
    f_meta(f_elif_expr,sub2(p));
    f_mand(f_expr,sub3(p));
    break;
  default:  unexpected(p->type);
  }
}

/* .................................................................. */

static void f_elif_expr(p)
node *p;
{
  switch (p->type) {
  case N_elif_expr:
    f_mand(f_expr,sub0(p));
    f_mand(f_expr,sub1(p));
    break;
  default:  unexpected(p->type);
  }
}
