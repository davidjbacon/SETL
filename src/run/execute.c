/*  ===  The SETL virtual machine interpreter  =====================  */

/*  $Id: execute.c,v 1.105 2021/11/26 03:26:27 setlorg Exp $  */

/*  Free software (c) dB - see file COPYING for license (GPL).  */

/*
 *  The interpreter embodied in 'execute' iterates over high-level
 *  "pseudo-code" contained in setl_vm.  Recursive invocation of
 *  'execute' should be possible if the caller saves and restores
 *  setl_vm around the call, though this has not yet been tried.
 *
 *  All the pseudo-code and all the SETL call activation records are
 *  rooted in setl_vm (the "instance" record), and reside in the
 *  SETL "heap".  The pseudo-code is simply a tuple of instructions.
 *  The activation records are rooted at a "display" that is general
 *  enough to support lexically nested procedures, closures, and even
 *  Scheme-style continuations.  The SETL routine calling conventions
 *  implemented here reflect that full generality, but again this is
 *  just a matter of planning ahead, because neither the translator
 *  nor the external form of the pseudo-code currently even support
 *  lexical nesting.  Having the call "stack" in the heap obviously
 *  carries a bit of extra overhead, but I care not...
 */

#include "setlrun.h"
#include "custom.h"

/*
 *  Note - RET is used here to mean the last formal parameter of a
 *  proc or the last actual argument on a call.  It designates the
 *  function result and does not correspond to a formal declared at
 *  the SETL level nor to a user-provided arg at that level.  It is
 *  an implicit local variable of the proc, but is an explicit output
 *  arg (an operand such as ">U_result" in the textual form of the VM
 *  code) on the call.  Its frame slot is given in the last 'parmout'
 *  of the proc, and it has the highest arg index even in the internal
 *  representation of the call.
 */


/* Reference to the block ptr of the SETL variable identified by
 * the data_operand 'opd', which gives the lexical level of the
 * frame and the slot (index into 'locs') within that frame */
#define SETL_VARIABLE(opd)  framelt(setl_vm->display[(opd).data.level], \
                                                     (opd).data.slot)

/*
 *  These macros only have meaning within the execute() function below:
 */

/* Reference to operand k of the current instruction, counting from 1 */
#define opnd(k)  instrelt(ip,k)

/* The usual way we refer to SETL variables in instruction operands,
 * where var(1) is the first operand, etc.  */
#define var(k)  SETL_VARIABLE(instrelt(ip,k))

/* How we usually refer to SETL variables that are args on call
 * instructions, so arg(1) is the first actual, etc.  */
#define arg(k)  SETL_VARIABLE(callelt(xp,k))

/* The SETL variable at the arg index (parmnum) of an actual in the
 * list of args on a call, as given in a parmin selected by bow_index
 * from the 'bow' array in the proc at pp (the callee) */
#define actual_in(bow_index)  arg(pp->bow[bow_index].parmin.parmnum)

/* The SETL variable at the arg index (parmnum) of an actual in the
 * list of args on a call, as given in a parmout selected by bow_index
 * from the 'bow' array in the proc at pp (the callee) */
#define actual_out(bow_index)  arg(pp->bow[bow_index].parmout.parmnum)

/* The local frame slot number of a RD or RW formal, as given in a
 * parmin selected by bow_index, for frame ptr fp and proc ptr pp */
#define formal_in(bow_index)  framelt(fp, pp->bow[bow_index].parmin.slot)

/* The local frame slot number of a WR or RW formal, as given in a
 * parmout selected by bow_index, for frame ptr fp and proc ptr pp */
#define formal_out(bow_index)  framelt(fp, pp->bow[bow_index].parmout.slot)

/*
 *  This macro creates and populates a new activation record (frame) fp
 *  in preparation for a SETL-level call.
 *
 *  It requires pp to be pointing at the callee's proc or vproc instr.
 */
#define setup_frame(n_args, callee_pc) {\
  long callee_level = proc_level(pp);  /* callee lexical level */\
  long nloc = proc_nloc(pp);  /* number of local SETL vars in 'locs' */\
  fp = vblock(frame,nloc);\
  fp->nloc         = nloc;\
  fp->proc_pc      = (callee_pc);\
  fp->caller_pc    = setl_vm->pc;\
  fp->caller_level = setl_vm->level;\
  fp->nargs        = (n_args);\
  fp->link         = setl_vm->display[callee_level];\
  fp->combiter     = NULL;\
  if (fp->nloc > 0) {\
    memset (fp->locs, 0, fp->nloc * sizeof fp->locs[0]);  /* OM */\
  }\
}

/*
 *  This macro performs a SETL-level call.
 *
 *  It requires pp to point to the callee proc and fp to point to the
 *  frame that is to be pushed, and updates the VM accordingly.
 */
#define make_call {\
  long callee_level = proc_level(pp);  /* lexical nesting level of callee */\
  setl_vm->level = callee_level;\
  setl_vm->display[callee_level] = fp;  /* push frame (fp->link points here) */\
  /* pc is incr'd at end of interpreter loop, so this effectively */\
  /* jumps to 1 past the proc/vproc instr:  */\
  setl_vm->pc = fp->proc_pc;  /* jump to callee */\
}

/*
 *  Restore the display to point to the caller's frame, but leave fp
 *  alone so that it can continue to point to the callee's frame.
 *  This is useful for copying WR/RW formals out into actuals.
 *  Also pop (restore) the caller's lexical level (current
 *  display index) in the VM.
 */
#define pop_frame {\
  setl_vm->display[setl_vm->level] = fp->link;  /* pop frame */\
  setl_vm->level = fp->caller_level;  /* restore lexical level */\
}

/*
 *  Return to the instruction after the call.
 *
 *  Note that make_return is not symmetric with make_call, as pop_frame
 *  is a separate code sequence rather than being part of make_return.
 *  Thus we can go:  pop_frame; process parmouts; make_return.
 */
#define make_return {\
  setl_vm->pc = fp->caller_pc;  /* jump back to caller */\
}

/*
 *  With the formals set up for another execution of the binary operator
 *  in which the op_return was executed (in combining-operator settings
 *  x my_bop/ t and my_bop/ t), jump to its beginning.  Cf. make_return.
 */
#define repeat {\
  setl_vm->pc = xp->callee.pc;  /* jump to self (caller's callee) */\
}

/*
 *  These are the ptrs that are generally defined during the calling
 *  and returning phases of the various forms of call and return.
 */
#define clear_call_ptrs {\
  xp = NULL;  /* calling instruction (op_call, op_vcall, etc.) */\
  pp = NULL;  /* callee instruction (op_proc or op_vproc) */\
  fp = NULL;  /* activation record (frame) */\
}

/*
 *  These macros copy actual args on calls to formal parameters,
 *  where formals are locals of the frame at fp for the proc at pp...
 */

#define copy_args_in(begin, end) {\
  long bow_index;\
  for (bow_index=(begin); bow_index<(end); bow_index++) {\
    copy_arg_in(bow_index);\
  }\
}

/*
 *  The bow_index selects a parmin in the proc at pp for a RD/RW formal,
 *  which gives its slot number in the frame at fp and the corresponding
 *  parmnum index into the actuals on the call.
 *
 *  This is basically a trivial optimization of
 *
 *    let (formal_in(bow_index),               // the formal, a local
 *         copy_value(actual_in(bow_index)));  // copy of the actual
 */
#define copy_arg_in(bow_index) {\
  parm_operand  parmin = pp->bow[bow_index].parmin;\
  let (framelt(fp, parmin.slot),  /* the formal, a local */\
       copy_value(arg(parmin.parmnum)));  /* copy of the actual */\
}

/*
 *  Move block ptr p (e.g. for a just-created tuple) into the local
 *  slot of a formal, where bow_index has the same meaning as in
 *  copy_arg_in() above.  Then clear p, rendering it unavailable as
 *  an alias.
 */
#define move_ptr_in(bow_index, p) {\
  move_blockptr (formal_in(bow_index), p);  /* move p to formal */\
}

/*
 *  Check that pp points to an op_proc that could have been generated
 *  for a SETL binary operator (2 RD args and a result), and init
 *  the proc bow indices ibow,jbow,kbow from pp.
 */
#define see_binop {\
  taut (pp->op == op_proc);  /* expect non-variadic (not op_vproc) */\
  taut (proc_nformal(pp) == 2);  /* of valence 2 (binary op) */\
  ibow = proc_i_in(pp);  /* beginning of parmins */\
  jbow = proc_i_out(pp);  /* beginning of parmouts */\
  kbow = proc_i_back(pp);  /* first beyond parmouts */\
  taut (jbow - ibow == 2);  /* 2 parmins for 2 RD args */\
  taut (kbow - jbow == 1);  /* 1 parmout for RET */\
  /* The assembler has really gone astray if these fail: */\
  check_parmin(ibow,   1);  /* 1st parmin selects 1st arg */\
  check_parmin(ibow+1, 2);  /* 2nd parmin selects 2nd arg */\
  check_parmout(jbow,  3);  /* the parmout selects 3rd arg (RET) */\
}

/*
 *  Check (but just with taut) that a parmin or parmout meets
 *  expectations regarding its parmnum (actual arg index).
 */
#define check_parmin(bow_index,num) {\
  taut (pp->bow[bow_index].parmin.parmnum == (num));\
}
#define check_parmout(bow_index,num) {\
  taut (pp->bow[bow_index].parmout.parmnum == (num));\
}

/*
 *  Move a block ptr out to an arg from the local slot of a WR/RW formal,
 *  preparatory to a return.  The bow_index here addresses a parmout.
 *
 *  Since the formal's lifetime ends at this point, the move effects
 *  the required value semantics at the SETL level.  Nonetheless, once
 *  the formal ptr has been copied, its slot is defensively set to OM
 *  (by move_blockptr()) even though the frame is about to evanesce.
 */
#define move_parm_out(bow_index) {\
  move_blockptr (actual_out(bow_index),  /* output arg */\
                 formal_out(bow_index));  /* source formal */\
}

/*
 *  Same for a sequence of bow (parm descriptor) indices.
 *
 *  Formals are unique, but in the case of repeated actual args, it is
 *  unspecified which effective-assignment-from-formal is supposed to
 *  "win", though it is clear that in this impl the rightmost prevails.
 */
#define move_to_args(begin, end) {\
  long bow_index;\
  for (bow_index=(begin); bow_index<(end); bow_index++) {\
    move_parm_out(bow_index);\
  }\
}

/*
 *  Move RET to the last arg (the one at nactual+1), just before
 *  returning from a call to a possibly variadic function.
 *
 *  For a non-variadic call, RET is usually taken care of as if it
 *  were simply the last of the WR/RW formals, but for variadic,
 *  the parmout at kbow-1 gives only the formal slot number
 *  correctly, not the index (parmnum) of the corresponding arg,
 *  which is instead given by nactual+1 in this the variadic case.
 *  Compare move_parm_out().
 */
#define move_ret_out {\
  move_blockptr (arg(nactual+1), formal_out(kbow-1));\
}

/*
 *  Let p take ownership of the RET formal.
 */
#define move_from_ret_to(p) {\
  move_blockptr (p, formal_out(kbow-1));  /* move RET to p */\
}

/*
 *  Form a tuple in 'tup' of the optional args on an ordinary
 *  call to a vproc; nactual and nformal must already be defined
 *  as the number of user-supplied actual args and user-declared
 *  formal parameters respectively (which doesn't include RET).
 *
 *  Value semantics are strictly observed, as the same variable
 *  may appear repeatedly in the arg list.
 */
#define form_tup {\
  long j;\
  taut (nformal >= 1);  /* callee declared a (*) formal */\
  taut (nactual >= (nformal - 1));  /* for 0 or more actuals */\
  tup = new_tuple(nactual - (nformal - 1));\
  for (j = nformal; j <= nactual; j++) {\
    long i = j - (nformal - 1);  /* i starts at 1 */\
    let (tupelt(tup,i), copy_value(arg(j)));\
  }\
  tup_truncate (&tup);  /* "open" tuples are never user-visible */\
}

/*
 *  Inverse of form_tup():  move 'tup' out to the optional args.
 *
 *  Note that since the lifetime of tup ends with the move, it
 *  suffices to transfer ownership rather than copy values.
 *  In case of repeated args, it is unspecified which tup elmt
 *  is supposed to "win", though it is clear that in this impl
 *  the rightmost corresponding elmt of tup prevails.
 */
#define distribute_tup {\
  long j;\
  taut (nformal >= 1);  /* callee declared a (*) formal */\
  taut (nactual >= (nformal - 1));  /* for 0 or more actuals */\
  taut (is_tuple(tup));\
  for (j = nformal; j <= nactual; j++) {\
    long i = j - (nformal - 1);  /* i starts at 1 */\
    if (i <= tup->nelt) {\
      move_blockptr (arg(j), tupelt(tup,i));  /* move tup elmt to arg */\
    } else {\
      arg(j) = OM;\
    }\
  }\
  tup = NULL;  /* now dead to us */\
}

/*
 *  For use with vcalls, i.e., those with (*) after the last actual,
 *  which is the new notation a variadic caller can use to pass the
 *  variadic arg it was called with thru to a variadic callee...
 */

/*
 *  Form 'tup' from the "extra" args inserted by the caller and the
 *  tuple presumed to represent the (*) arg received by the caller
 *  (though the latter can actually be any transformation thereof).
 *
 *  As with form_tup, value semantics are strictly obsrved.
 */
#define v_form_tup {\
  long nextra, j;\
  taut (nformal >= 1);  /* callee declared a (*) formal */\
  taut (nactual >= nformal);  /* caller supplied a (*) actual */\
  nextra = nactual - nformal;  /* #args inserted by caller */\
  opt = (tuple *)arg(nactual);  /* alias for (*) arg */\
  if (!is_tuple(opt)) {\
    runerr("(*) arg must be TUPLE, not %s", TYPENAME(opt));\
  }\
  tup = new_tuple(nextra + opt->nelt);\
  /* Copy the 0 or more "extra" args after the fixed args and */\
  /* before the (*) arg into the initial elements of 'tup'.  */\
  for (j = 1; j <= nextra; j++) {\
    long i = (nformal - 1) + j;  /* just past the fixed args */\
    let (tupelt(tup,j), copy_value(arg(i)));\
  }\
  /* Copy the final arg, which is the inherited tuple of */\
  /* optional args or any reasonable facsimile thereof, */\
  /* into the remaining subtuple of 'tup'.  */\
  for (j = 1; j <= opt->nelt; j++) {\
    long i = nextra + j;\
    let (tupelt(tup,i), copy_value(tupelt(opt,j)));\
  }\
  opt = NULL;  /* done with this alias for now */\
  tup_truncate (&tup);  /* "open" tuples are never user-visible */\
}

/*
 *  Inverse of v_form_tup:  move the first part of 'tup' out to the
 *  "extra" args and the rest out to the caller's (*) arg as a tuple.
 *
 *  As with distribute_tup, tup's transience allows us to move ptrs
 *  rather than having to copy values.
 */
#define v_distribute_tup {\
  long nextra, j;\
  taut (nformal >= 1);  /* callee declared a (*) formal */\
  taut (nactual >= nformal);  /* caller supplied a (*) actual */\
  nextra = nactual - nformal;  /* not incl (*) arg */\
  assert (is_tuple(tup));\
  /* distribute first part to the "extra" args */\
  for (j = 1; j <= nextra; j++) {\
    long i = (nformal - 1) + j;  /* just past the fixed args */\
    /* truncation of tup could make tup->nelt < nextra:  */\
    if (j <= tup->nelt) {\
      move_blockptr (arg(i), tupelt(tup,j));  /* move tup elmt to arg */\
    } else {\
      arg(i) = OM;\
    }\
  }\
  opt = (tuple *)new_tuple(tup->nelt > nextra ? \
                           tup->nelt - nextra : 0);\
  /* form 'opt' from the remainder of tup after the "extra" args */\
  for (j = 1; j <= opt->nelt; j++) {\
    long i = nextra + j;\
    move_blockptr (tupelt(opt,j), tupelt(tup,i));  /* move tup element */\
  }\
  tup = NULL;  /* now finished mining this transient tuple */\
  tup_truncate (&opt);  /* "open" tuples are never user-visible */\
  move_blockptr (arg(nactual), opt);  /* move 'opt' to the (*) actual */\
}

#if 0 /* the above but with copying-out of values rather than "moving" */
/*
 *  Distribute 'tup' to "extra" args and outgoing (*) tuple.
 *  Although tup is not supposed to contain any aliases to
 *  SETL variables, we cautiously copy the values of its elements,
 *  not merely the block ptrs, just in case.
 */
#define v_distribute_tup {\
  long nextra, j;\
  taut (nformal >= 1);  /* callee declared a (*) formal */\
  taut (nactual >= nformal);  /* caller supplied a (*) actual */\
  nextra = nactual - nformal;  /* #args inserted by caller */\
  assert (is_tuple(tup));\
  /* distribute to the "extra" args */\
  for (j = 1; j <= nextra; j++) {\
    long i = (nformal - 1) + j;  /* just past the fixed args */\
    /* truncation of tup could make tup->nelt < nextra:  */\
    if (j <= tup->nelt) {\
      let (arg(i), copy_value(tupelt(tup,j)));\
    } else {\
      arg(i) = OM;\
    }\
  }\
  opt = (tuple *)new_tuple(tup->nelt > nextra ? \
                           tup->nelt - nextra : 0);\
  /* form 'opt' from the remainder of tup after the "extra" args */\
  for (j = 1; j <= opt->nelt; j++) {\
    long i = nextra + j;\
    let (tupelt(opt,j), copy_value(tupelt(tup,i)));\
  }\
  tup = NULL;  /* now finished mining this transient tuple */\
  tup_truncate (&opt);  /* "open" tuples are never user-visible */\
  move_blockptr (arg(nactual), opt);  /* move 'opt' to the (*) actual */\
}
#endif


/*
 *  Run the SETL Virtual Machine given in 'setl_vm'.
 */
void execute(void) {

  long callee_pc;  /* callee instruction index */
  long ibow;  /* index of first "parmin" in pp->bow, proc_i_in(pp) */
  long jbow;  /* index of first "parmout" in pp->bow, proc_i_out(pp) */
  long kbow;  /* index of first "backtrack" in pp->bow, proc_i_back(pp) */
  long nformal;  /* number of formal parameters, which doesn't incl RET */
  long nactual;  /* number of args on a call, again not counting RET */
  long nargs;  /* NARGS, which is nactual except under some (*) calls */
  long caller_nformal;
  long caller_nopt;

  instr    *ip = NULL;  HANDLE ih = ref(ip);  /* current instruction */
  call     *xp = NULL;  HANDLE xh = ref(xp);  /* calling instruction */
  proc     *pp = NULL;  HANDLE ph = ref(pp);  /* callee instruction */
  frame    *fp = NULL;  HANDLE fh = ref(fp);  /* activation record */
  iterator *jp = NULL;  HANDLE jh = ref(jp);  /* for binop/ combining ops */
  block    *tp = NULL;  HANDLE th = ref(tp);  /* scratch */
  block    *up = NULL;  HANDLE uh = ref(up);  /* where */
  block    *vp = NULL;  HANDLE vh = ref(vp);  /* it */
  block    *wp = NULL;  HANDLE wh = ref(wp);  /* itches */

  tuple *tup = NULL; HANDLE h_tup = ref(tup);  /* yet another scratch */
  tuple *opt = NULL; HANDLE h_opt = ref(opt);  /* one more, for opt args */

  /* Extra scratch block pointers and handles for customization to use: */
  block    *p1  = NULL; HANDLE h1  = ref(p1);
  block    *p2  = NULL; HANDLE h2  = ref(p2);
  block    *p3  = NULL; HANDLE h3  = ref(p3);
  block    *p4  = NULL; HANDLE h4  = ref(p4);
  block    *p5  = NULL; HANDLE h5  = ref(p5);
  block    *p6  = NULL; HANDLE h6  = ref(p6);
  block    *p7  = NULL; HANDLE h7  = ref(p7);
  block    *p8  = NULL; HANDLE h8  = ref(p8);
  block    *p9  = NULL; HANDLE h9  = ref(p9);
  block    *p10 = NULL; HANDLE h10 = ref(p10);
  block    *p11 = NULL; HANDLE h11 = ref(p11);
  block    *p12 = NULL; HANDLE h12 = ref(p12);
  block    *p13 = NULL; HANDLE h13 = ref(p13);
  block    *p14 = NULL; HANDLE h14 = ref(p14);
  block    *p15 = NULL; HANDLE h15 = ref(p15);
  block    *p16 = NULL; HANDLE h16 = ref(p16);
  block    *p17 = NULL; HANDLE h17 = ref(p17);
  block    *p18 = NULL; HANDLE h18 = ref(p18);
  block    *p19 = NULL; HANDLE h19 = ref(p19);
  block    *p20 = NULL; HANDLE h20 = ref(p20);

  bool running = true;

  while (running) {  /* main run-time interpreter execution loop */

    bool lifo_arena_use = true;

    arena_begin();  /* begin arena allocation scope */

#ifdef DEBUG_TRACE
    if (debug) print_stderr("%ld",setl_vm->pc);
#endif

    ip = (instr *)tupelt(setl_vm->code,setl_vm->pc);
    taut (is_instr(ip));

#ifdef DEBUG_TRACE
    if (debug) print_stderr(" %d\n",ip->op);
#endif

    switch (ip->op) {

    case op_assert: {
      boolean *b;
      taut (ip->nopnd == 1);
      b = (boolean *)var(1);
      if (!is_boolean(b)) {
        runerr("Operand of ASSERT must be BOOLEAN, not %s", TYPENAME(b));
      }
      if (!b->booval) runerr("Assertion failed");
    } break;

    case op_backtrack:  /* treated as an "assembler directive" */
      unexpected (ip->op);

    case op_call:  /* normal call to non-variadic user-defined callee */
      xp = (call *)ip;  /* the present call instr in all its glory */
      taut (xp->nopnd >= 2);  /* callee + 0 or more args + RET */
      callee_pc = xp->callee.pc;  /* code index of op_proc instr */
      pp = (proc *)tupelt(setl_vm->code, callee_pc);  /* proc descriptor */
      taut (pp->op == op_proc);  /* expect non-variadic callee */
      nformal = proc_nformal(pp);  /* not counting RET */
      nactual = xp->nopnd - 2;  /* user-provided args (not RET) */
      taut (nformal >= 0);  /* or assembler really botched things */
      taut (nactual == nformal);  /* this is the fixed-valence case */
      ibow = proc_i_in(pp);  /* beginning of parmins */
      jbow = proc_i_out(pp);  /* first beyond parmins */
      setup_frame (nactual, callee_pc);  /* fp := new frame */
      copy_args_in (ibow, jbow);  /* copy RD/RW actuals into formals */
      make_call;  /* push frame and jump to callee */
      clear_call_ptrs;  /* xp, pp, fp */
      break;

    case op_callv:  /* normal call to variadic user-defined callee */
      xp = (call *)ip;  /* the present call instr in all its glory */
      taut (xp->nopnd >= 2);  /* callee + 0 or more args + RET */
      callee_pc = xp->callee.pc;  /* code index of op_vproc instr */
      pp = (proc *)tupelt(setl_vm->code, callee_pc);  /* proc descriptor */
      taut (pp->op == op_vproc);  /* expect variadic callee */
      nformal = proc_nformal(pp);  /* not counting RET */
      nactual = xp->nopnd - 2;  /* user-provided args (not RET) */
      taut (nformal >= 1);  /* callee declared a (*) formal */
      taut (nactual >= nformal - 1);  /* for 0 or more actuals */
      ibow = proc_i_in(pp);  /* beginning of parmins */
      jbow = proc_i_out(pp);  /* first beyond parmins */
      setup_frame (nactual, callee_pc);  /* fp := new frame */
      if (jbow > ibow && pp->bow[jbow-1].parmin.parmnum == nformal) {
        /* last parmin refers to last formal, so (*) formal is RDable */
        copy_args_in (ibow, jbow-1);  /* copy fixed args in to formals */
        form_tup;  /* tup := a tuple of the optional args */
        move_ptr_in (jbow-1, tup);  /* move tup to the (*) formal */
      } else {
        /* variadic arg not RDable, so only have the fixed args to copy */
        copy_args_in (ibow, jbow);  /* copy RD/RW actuals into formals */
      }
      make_call;  /* push frame and jump to callee */
      clear_call_ptrs;  /* xp, pp, fp */
      break;

    case op_vcall:  /* call with (*) to necessarily variadic callee */
      xp = (call *)ip;  /* the present call instr in all its glory */
      taut (xp->nopnd >= 3);  /* callee + 0 or more args + (*) arg + RET */

      /* "borrow" fp (frame ptr) and pp (proc ptr) for the caller
       * context, to derive caller_nopt */
      fp = setl_vm->display[setl_vm->level];  /* current (caller) frame */
      pp = (proc *)tupelt(setl_vm->code, fp->proc_pc);  /* and instr */
      taut (pp->op == op_vproc);  /* vcalls only allowed from variadics */
      caller_nformal = proc_nformal(pp);  /* caller's own #formals */
      taut (caller_nformal >= 1);  /* caller declared a (*) formal */
      pp = NULL;
      /* how many actuals were passed to this caller in the (*) arg:  */
      caller_nopt = fp->nargs - (caller_nformal - 1);
      fp = NULL;

      /* now back to "normal" usage of pp and fp for callee context... */
      callee_pc = xp->callee.pc;  /* code index of op_vproc instr */
      pp = (proc *)tupelt(setl_vm->code, callee_pc);  /* proc descriptor */
      taut (pp->op == op_vproc);  /* vcalls only allowed to variadics */
      nformal = proc_nformal(pp);  /* not counting RET */
      nactual = xp->nopnd - 2;  /* user-provided args (not RET) */
      taut (nformal >= 1);  /* callee declared a (*) formal */
      taut (nactual >= nformal);  /* caller supplied a (*) actual */
      nargs = (nactual - 1) + caller_nopt;  /* SETL-level NARGS */
      ibow = proc_i_in(pp);  /* beginning of parmins */
      jbow = proc_i_out(pp);  /* first beyond parmins */
      setup_frame (nargs, callee_pc);  /* fp := new frame */
      if (jbow > ibow && pp->bow[jbow-1].parmin.parmnum == nformal) {
        /* last parmin refers to last formal, so (*) formal is RDable */
        copy_args_in (ibow, jbow-1);  /* copy fixed args in to formals */
        v_form_tup;  /* tup := ["extra" args] + starred arg */
        move_ptr_in (jbow-1, tup);  /* move tup to the (*) formal */
      } else {
        /* variadic arg not RDable, so only have the fixed args to copy */
        copy_args_in (ibow, jbow);  /* copy RD/RW actuals into formals */
      }
      make_call;  /* push frame and jump to callee */
      clear_call_ptrs;  /* xp, pp, fp */
      break;

    /* There are currently no known emitters of this op */
    case op_call_assigning:  /* lhs binop:= rhs */
      xp = (call *)ip;  /* the present call instr in all its glory */
      taut (xp->nopnd == 3);  /* binop + lhs + rhs */
      callee_pc = xp->callee.pc;  /* code index of op_proc instr */
      pp = (proc *)tupelt(setl_vm->code, callee_pc);  /* proc descriptor */
      see_binop;  /* make sure callee is a binop; init ibow,jbow,kbow */
      setup_frame (2, callee_pc);  /* fp := new frame */
      copy_arg_in (ibow);  /* copy lhs (1st actual) to 1st formal */
      copy_arg_in (ibow+1);  /* copy rhs (2nd actual) to 2nd formal */
      make_call;  /* push frame and jump to callee */
      clear_call_ptrs;  /* xp, pp, fp */
      break;

    case op_call_bincomb:  /* x binop/ t */
      xp = (call *)ip;  /* the present call instr in all its glory */
      taut (xp->nopnd == 4);  /* binop + x + t + RET */
      callee_pc = xp->callee.pc;  /* code index of op_proc instr */
      pp = (proc *)tupelt(setl_vm->code, callee_pc);  /* proc descriptor */
      see_binop;  /* make sure callee is a binop; init ibow,jbow,kbow */
      jp = init_iterator(arg(2));  /* make iterator from t */
      tp = NULL;
      if (step_iterator(&jp,&tp)) {  /* tp := copy of 1st element from t */
        setup_frame (2, callee_pc);  /* fp := new frame */
        fp->combiter = jp;  /* let callee frame own the iterator */
        jp = NULL;  /* ownership of iterator has been transferred */
        /* same as let (formal_in(ibow), copy_value(arg(1))):  */
        copy_arg_in (ibow);  /* copy x (1st actual) to 1st formal */
        move_ptr_in (ibow+1, tp);  /* copied t element is 2nd formal */
        make_call;  /* push frame and jump to binop */
      } else {  /* t is empty; will just return x */
        jp = NULL;  /* iterator exhausted */
        /* no need to set up a callee frame etc.; we can just copy
         * the 1st actual directly to the RET actual and we're done:  */
        let (arg(3), copy_value(arg(1)));  /* t empty; copy x to RET */
      }
      clear_call_ptrs;  /* xp, pp, fp */
      break;

    case op_call_uncomb:  /* binop/ t */
      xp = (call *)ip;  /* the present call instr in all its glory */
      taut (xp->nopnd == 3);  /* binop + t + RET */
      callee_pc = xp->callee.pc;  /* code index of op_proc instr */
      pp = (proc *)tupelt(setl_vm->code, callee_pc);  /* proc descriptor */
      see_binop;  /* make sure callee is a binop; init ibow,jbow,kbow */
      jp = init_iterator(arg(1));  /* make iterator from t */
      tp = NULL;
      if (step_iterator(&jp,&tp)) {  /* tp := copy of 1st element from t */
        up = NULL;
        if (step_iterator(&jp,&up)) {  /* up := copy of 2nd elmt from t */
          setup_frame (2, callee_pc);  /* fp := new frame */
          fp->combiter = jp;  /* let callee frame own the iterator */
          jp = NULL;  /* ownership of iterator has been transferred */
          move_ptr_in (ibow, tp);  /* copied 1st t elmt is 1st formal */
          move_ptr_in (ibow+1, up);  /* copied 2nd t elmt is 2nd formal */
          make_call;  /* push frame and jump to binop */
        } else {  /* no 2nd element in t */
          jp = NULL;  /* iterator exhausted */
          arg(2) = tp;  /* alias RET (uncomb result) to 1st element copy */
          tp = NULL;  /* ownership of block at tp has been transferred */
        }
      } else {  /* t empty */
        jp = NULL;  /* iterator exhausted */
        arg(2) = OM;  /* let RET (uncomb result) be OM */
      }
      clear_call_ptrs;  /* xp, pp, fp */
      break;

    case op_icall:  /* normal indirect call, callee variadicity unknown */
      xp = (call *)ip;  /* the present call instr in all its glory */
      taut (xp->nopnd >= 2);  /* callee + 0 or more args + RET */

      {
        routine *r = (routine *)var(1);  /* the callee */
        if (!is_routine(r)) {
          runerr("Only a ROUTINE (not %s) can be indirectly called",
                              TYPENAME(r));
        }
        callee_pc = r->proc_pc;
      }
      pp = (proc *)tupelt(setl_vm->code, callee_pc);  /* proc descriptor */
      nformal = proc_nformal(pp);  /* not counting RET */
      nactual = xp->nopnd - 2;  /* user-provided args (not RET) */
      taut (nformal >= 0);  /* or assembler really botched things */
      ibow = proc_i_in(pp);  /* beginning of parmins */
      jbow = proc_i_out(pp);  /* first beyond parmins */
      switch (pp->op) {
      case op_proc:  /* non-variadic callee */
        /* exact match required */
        if (nactual != nformal) {
          runerr("Need %ld arg(s) but have %ld on indirect call",
                   nformal,            nactual);
        }
        setup_frame (nactual, callee_pc);  /* fp := new frame */
        copy_args_in (ibow, jbow);  /* parmins */
        break;
      case op_vproc:  /* variadic callee */
        /* just need sufficient args */
        taut (nformal >= 1);  /* callee declared a (*) formal */
        if (nactual < nformal - 1) {
          runerr("Need at least %ld arg(s) but have %ld on indirect call",
                        nformal - 1,            nactual);
        }
        setup_frame (nactual, callee_pc);  /* fp := new frame */
        copy_args_in (ibow, jbow-1);  /* copy fixed args in to formals */
        form_tup;  /* tup := a tuple of the optional args */
        move_ptr_in (jbow-1, tup);  /* move tup to the (*) formal */
        break;
      default:
        unexpected (pp->op);
      }
      make_call;  /* push frame and jump to callee */
      clear_call_ptrs;  /* xp, pp, fp */
      break;

    case op_vicall: {  /* indirect (*) call to necessarily variadic callee */
      xp = (call *)ip;  /* the present call instr in all its glory */
      taut (xp->nopnd >= 3);  /* callee + 0 or more args + (*) arg + RET */

      /* "borrow" fp (frame ptr) and pp (proc ptr) for the caller
       * context, to derive caller_nopt */
      fp = setl_vm->display[setl_vm->level];  /* current (caller) frame */
      pp = (proc *)tupelt(setl_vm->code, fp->proc_pc);  /* and instr */
      taut (pp->op == op_vproc);  /* vcalls only allowed from variadics */
      caller_nformal = proc_nformal(pp);  /* caller's own #formals */
      taut (caller_nformal >= 1);  /* caller declared a (*) formal */
      pp = NULL;
      /* how many actuals were passed to this caller in the (*) arg:  */
      caller_nopt = fp->nargs - (caller_nformal - 1);
      fp = NULL;

      /* now back to operation similar to what op_icall does... */
      {
        routine *r = (routine *)var(1);  /* the callee */
        if (!is_routine(r)) {
          runerr("Only a ROUTINE (not %s) can be indirectly called",
                              TYPENAME(r));
        }
        callee_pc = r->proc_pc;
      }
      pp = (proc *)tupelt(setl_vm->code, callee_pc);  /* proc descriptor */
      if (pp->op != op_vproc) {
        runerr("Callee on indirect (*) call is not variadic");
      }
      nformal = proc_nformal(pp);  /* not counting RET */
      nactual = xp->nopnd - 2;  /* user-provided args (not RET) */
      if (nactual < nformal) {
        runerr("Need at least %ld arg(s) but have %ld on indirect (*) call",
                          nformal,            nactual);
      }
      taut (nformal >= 1);  /* callee declared a (*) formal */
      taut (nactual >= nformal);  /* as just checked above */
      nargs = (nactual - 1) + caller_nopt;  /* SETL-level NARGS */
      ibow = proc_i_in(pp);  /* beginning of parmins */
      jbow = proc_i_out(pp);  /* first beyond parmins */
      setup_frame (nargs, callee_pc);  /* fp := new frame */
      /* last parmin must point to last formal because all args are RD */
      copy_args_in (ibow, jbow-1);  /* copy fixed args in */
      v_form_tup;  /* tup := ["extra" args] + starred arg */
      move_ptr_in (jbow-1, tup);  /* move tup to the (*) formal */
      make_call;  /* push frame and jump to callee */
      clear_call_ptrs;  /* xp, pp, fp */
    } break;

    /*
     *  There currently isn't any SETL syntax that generates the
     *  following three cases, but they should be easy to implement
     *  after the patterns suggested by the direct-call assigning
     *  and combining forms and the plain indirect-call form op_icall.
     *  Three more cases under op_return would be needed then too.
     */

    case op_icall_assigning:
      taut (ip->nopnd == 4);
      runerr("Assigning form of indirect call not yet implemented");
      break;

    case op_icall_bincomb:
      taut (ip->nopnd == 5);
      runerr("Binary combining form of indirect call not yet implemented");
      break;

    case op_icall_uncomb:
      taut (ip->nopnd == 4);
      runerr("Unary combining form of indirect call not yet implemented");
      break;

    case op_scall:  /* call to built-in (library routine) */
      xp = (call *)ip;
      taut (xp->nopnd >= 2);
      nactual = xp->nopnd - 2;  /* user-provided args (not RET) */

#     define nullary(f) {\
        taut (nactual == 0);\
        let (arg(1), (block *)(f)());\
      }

#     define void_nullary(f) {\
        taut (nactual == 0);\
        (f)();\
      }

#     define unary(f,t1) {\
        taut (nactual == 1);\
        let (arg(2), (block *)(f)((t1)arg(1)));\
      }

#     define binary(f,t1,t2) {\
        taut (nactual == 2);\
        let (arg(3), (block *)(f)((t1)arg(1),\
                                  (t2)arg(2)));\
      }

#     define wr_unary(f,t1) {\
        taut (nactual == 1);\
        tp = NULL;\
        let (arg(2), (block *)(f)((t1)(void *)&tp));\
        arg(1) = tp; tp = NULL;\
      }

#     define void_wr_unary(f,t1) {\
        taut (nactual == 1);\
        tp = NULL;\
        (f)((t1)(void *)&tp);\
        arg(1) = tp; tp = NULL;\
      }

#     define void_wr(f,t1) {\
        taut (nactual == 1);\
        tp = NULL;\
        (f)((t1)(void *)&tp);\
        arg(1) = tp; tp = NULL;\
      }

#     define rw_unary(f,t1) {\
        taut (nactual == 1);\
        tp = arg(1);\
        let (arg(2), (block *)(f)((t1)(void *)&tp));\
        arg(1) = tp; tp = NULL;\
      }

#     define void_rw_unary(f,t1) {\
        taut (nactual == 1);\
        tp = arg(1);\
        (f)((t1)(void *)&tp);\
        arg(1) = tp; tp = NULL;\
      }

#     define void_rw(f,t1) {\
        taut (nactual == 1);\
        tp = arg(1);\
        (f)((t1)(void *)&tp);\
        arg(1) = tp; tp = NULL;\
      }

#     define void_rd_wr(f,t1,t2) {\
        taut (nactual == 2);\
        up = NULL;\
        (f)((t1)arg(1),\
            (t2)(void *)&up);\
        arg(2) = up; up = NULL;\
      }

#     define rw_rd(f,t1,t2) {\
        taut (nactual == 2);\
        tp = arg(1);\
        let (arg(3), (block *)(f)((t1)(void *)&tp,\
                                  (t2)arg(2)));\
        arg(1) = tp; tp = NULL;\
      }

#     define rw_rd_rd(f,t1,t2,t3) {\
        taut (nactual == 3);\
        tp = arg(1);\
        let (arg(4), (block *)(f)((t1)(void *)&tp,\
                                  (t2)arg(2),\
                                  (t3)arg(3)));\
        arg(1) = tp; tp = NULL;\
      }

#     define wr_wr(f,t1,t2) {\
        taut (nactual == 2);\
        tp = NULL;\
        up = NULL;\
        let (arg(3), (block *)(f)((t1)(void *)&tp,\
                                  (t2)(void *)&up));\
        arg(1) = tp; tp = NULL;\
        arg(2) = up; up = NULL;\
      }

#     define void_wr_wr(f,t1,t2) {\
        taut (nactual == 2);\
        tp = NULL;\
        up = NULL;\
        (f)((t1)(void *)&tp,(t2)(void *)&up);\
        arg(1) = tp; tp = NULL;\
        arg(2) = up; up = NULL;\
      }

#     define void_wr_wr_wr_wr(f,t1,t2,t3,t4) {\
        taut (nactual == 4);\
        tp = NULL;\
        up = NULL;\
        vp = NULL;\
        wp = NULL;\
        (f)((t1)(void *)&tp,\
            (t2)(void *)&up,\
            (t3)(void *)&vp,\
            (t4)(void *)&wp);\
        arg(1) = tp; tp = NULL;\
        arg(2) = up; up = NULL;\
        arg(3) = vp; vp = NULL;\
        arg(4) = wp; wp = NULL;\
      }

#     define rd_rd(f,t1,t2) {\
        taut (nactual == 2);\
        let (arg(3), (block *)(f)((t1)arg(1),\
                                  (t2)arg(2)));\
      }

#     define rd_rd_rd(f,t1,t2,t3) {\
        taut (nactual == 3);\
        let (arg(4), (block *)(f)((t1)arg(1),\
                                  (t2)arg(2),\
                                  (t3)arg(3)));\
      }

#     define void_rd(f,t1) {\
        taut (nactual == 1);\
        (f)((t1)arg(1));\
      }

#     define void_rd_rd(f,t1,t2) {\
        taut (nactual == 2);\
        (f)((t1)arg(1),\
            (t2)arg(2));\
      }

#     define void_rd_rd_rd(f,t1,t2,t3) {\
        taut (nactual == 3);\
        (f)((t1)arg(1),\
            (t2)arg(2),\
            (t3)arg(3));\
      }

#     define void_rd_rd_rd_rd(f,t1,t2,t3,t4) {\
        taut (nactual == 4);\
        (f)((t1)arg(1),\
            (t2)arg(2),\
            (t3)arg(3),\
            (t4)arg(4));\
      }

#     define void_rd_rd_rd_wr(f,t1,t2,t3,t4) {\
        taut (nactual == 4);\
        wp = NULL;\
        (f)((t1)arg(1),\
            (t2)arg(2),\
            (t3)arg(3),\
            (t4)(void *)&wp);\
        arg(4) = wp; wp = NULL;\
      }

#     define from(f,t1,t2) {\
        tp = NULL;\
        up = arg(2);\
        (f)((t1)(void *)&tp,\
            (t2)(void *)&up);\
        arg(1) = tp; tp = NULL;\
        arg(2) = up; up = NULL;\
      }

      switch (xp->callee.sysrot) {

      case S_card:
        unary (l_card, block *);
        break;

      case S_star:
        binary (l_star, block *, block *);
        break;

      case S_power:
        binary (l_power, block *, block *);
        break;

      case S_plus_1:
        unary (l_uplus, block *);
        break;

      case S_plus_2:
        binary (l_bplus, block *, block *);
        break;

      case S_minus_1:
        unary (l_uminus, block *);
        break;

      case S_minus_2:
        binary (l_bminus, block *, block *);
        break;

      case S_slash:
        binary (l_slash, block *, block *);
        break;

      case S_ne:
        binary (l_ne, block *, block *);
        break;

      case S_lt:
        binary (l_lt, block *, block *);
        break;

      case S_le:
        binary (l_le, block *, block *);
        break;

      case S_eq:
        binary (l_eq, block *, block *);
        break;

      case S_gt:
        binary (l_gt, block *, block *);
        break;

      case S_ge:
        binary (l_ge, block *, block *);
        break;

      case S_query:
        binary (l_query, block *, block *);
        break;

      case S_ABS:
        unary (l_abs, block *);
        break;

      case S_ACOS:
        unary (l_acos, real *);
        break;

      case S_ACCEPT:
        unary (l_accept, block *);
        break;

      case S_AND:
        binary (l_and, boolean *, boolean *);
        break;

      case S_ANY:
        rw_rd (l_any, string **, string *);
        break;

      case S_ARB:
        unary (l_arb, set *);
        break;

      case S_ARG:
        unary (l_arg, block *);
        break;

      case S_ASIN:
        unary (l_asin, real *);
        break;

      case S_ATAN:
        unary (l_atan, real *);
        break;

      case S_ATAN2:
        binary (l_atan2, real *, real *);
        break;

      case S_BIT_AND:
        binary (l_bit_and, integer *, integer *);
        break;

      case S_BIT_NOT:
        unary (l_bit_not, integer *);
        break;

      case S_BIT_OR:
        binary (l_bit_or, integer *, integer *);
        break;

      case S_BIT_XOR:
        binary (l_bit_xor, integer *, integer *);
        break;

      case S_BREAK:
        rw_rd (l_break, string **, string *);
        break;

      case S_CALLF:
        void_rd_rd_rd (l_callf, block *, block *, block *);
        break;

      case S_CALLOUT:
        rd_rd_rd (l_callout, integer *, block *, tuple *);
        break;

      case S_CALLOUT2:
        rd_rd_rd (l_callout2, integer *, block *, tuple *);
        break;

      case S_CEIL:
        unary (l_ceil, real *);
        break;

      case S_CHAR:
        unary (l_char, integer *);
        break;

      case S_CLEAR_ERROR:
        void_nullary (l_clear_error);
        break;

      case S_CLOCK:
        nullary (l_clock);
        break;

      case S_COMMAND_LINE:
        nullary (l_command_line);
        break;

      case S_COMMAND_NAME:
        nullary (l_command_name);
        break;

      case S_COMPILE:
        unary (l_compile, block *);
        break;

      case S_COMPLEX:
        unary (l_complex, block *);
        break;

      case S_COS:
        unary (l_cos, real *);
        break;

      case S_COSH:
        unary (l_cosh, real *);
        break;

      case S_DATE:
        nullary (l_date);
        break;

      case S_DENOTYPE:
        unary (l_denotype, string *);
        break;

      case S_DIV:
        binary (l_div, integer *, integer *);
        break;

      case S_DOMAIN:
        unary (l_domain, set *);
        break;

      case S_DOUBLE:
        unary (l_double, block *);
        break;

      case S_DUP:
        unary (l_dup, integer *);
        break;

      case S_DUP2:
        rd_rd (l_dup2, integer *, integer *);
        break;

      case S_EVEN:
        unary (l_even, integer *);
        break;

      case S_EXECUTE:
        unary (l_execute, block *);
        break;

      case S_EXP:
        unary (l_exp, real *);
        break;

      case S_FEXISTS:
        unary (l_fexists, string *);
        break;

      case S_FILENAME:
        unary (l_filename, block *);
        break;

      case S_FILENO:
        unary (l_fileno, block *);
        break;

      case S_FILEPOS:
        unary (l_filepos, block *);
        break;

      case S_FIX:
        unary (l_fix, real *);
        break;

      case S_FIXED:
        rd_rd_rd (l_fixed, real *, integer *, integer *);
        break;

      case S_FLOAT:
        unary (l_float, integer *);
        break;

      case S_FLOATING:
        rd_rd_rd (l_floating, real *, integer *, integer *);
        break;

      case S_FLOOR:
        unary (l_floor, real *);
        break;

      case S_FLUSH:
        void_rd (l_flush, block *);
        break;

      case S_FORK:
        nullary (l_fork);
        break;

      case S_FROM:
        from (l_from, block **, set **);
        break;

      case S_FROMB:
        from (l_fromb, block **, block **);
        break;

      case S_FROME:
        from (l_frome, block **, block **);
        break;

      case S_FSIZE:
        unary (l_fsize, block *);
        break;

      case S_FTRUNC:
        void_rd_rd (l_ftrunc, block *, integer *);
        break;

      case S_GETC:
        unary (l_getc, block *);
        break;

      case S_GETCHAR:
        nullary (l_getchar);
        break;

      case S_GETEGID:
        nullary (l_getegid);
        break;

      case S_GETEM:
        void_wr_wr (l_getem, block **, block **);
        break;

      case S_GETENV:
        unary (l_getenv, string *);
        break;

      case S_GETEUID:
        nullary (l_geteuid);
        break;

      case S_GETFILE:
        unary (l_getfile, block *);
        break;

      case S_GETGID:
        nullary (l_getgid);
        break;

      case S_GETIPP:
        unary (l_getipp, block *);
        break;

      case S_GETLINE:
        unary (l_getline, block *);
        break;

      case S_GETN:
        rd_rd (l_getn, block *, integer *);
        break;

      case S_GETPGID:
        unary (l_getpgid, integer *);
        break;

      case S_GETPGRP:
        nullary (l_getpgrp);
        break;

      case S_GETPID:
        nullary (l_getpid);
        break;

      case S_GETPPID:
        nullary (l_getppid);
        break;

      case S_GETS:
        void_rd_rd_rd_wr (l_gets, block *, integer *, integer *, string **);
        break;

      case S_GETSPP:
        unary (l_getspp, block *);
        break;

      case S_GETUID:
        nullary (l_getuid);
        break;

      case S_GETWD:
        nullary (l_getwd);
        break;

      case S_GLOB:
        unary (l_glob, string *);
        break;

      case S_GMARK:
        rd_rd (l_gmark, string *, block *);
        break;

      case S_HEX:
        unary (l_hex, string *);
        break;

      case S_HOSTADDR:
        nullary (l_hostaddr);
        break;

      case S_HOSTNAME:
        nullary (l_hostname);
        break;

      case S_ICHAR:
        unary (l_ichar, string *);
        break;

      case S_IMAG:
        unary (l_imag, block *);
        break;

      case S_IMPL:
        binary (l_impl, boolean *, boolean *);
        break;

      case S_IN:
        binary (l_in, block *, block *);
        break;

      case S_INCS:
        binary (l_incs, set *, set *);
        break;

      case S_INT:
        unary (l_int, block *);
        break;

      case S_INTSLASH:
        nullary (l_intslash);
        break;

      case S_IS_ATOM:
        unary (l_is_atom, block *);
        break;

      case S_IS_BOOLEAN:
        unary (l_is_boolean, block *);
        break;

      case S_IS_COMPLEX:
        unary (l_is_complex, block *);
        break;

      case S_IS_DOUBLE:
        unary (l_is_double, block *);
        break;

      case S_IS_FLOAT:
        unary (l_is_float, block *);
        break;

      case S_IS_INT:
        unary (l_is_integer, block *);
        break;

      case S_IS_INTEGER:
        unary (l_is_integer, block *);
        break;

      case S_IS_MAP:
        unary (l_is_map, block *);
        break;

      case S_IS_MMAP:
        unary (l_is_mmap, block *);
        break;

      case S_IS_NUMERIC:
        unary (l_is_numeric, block *);
        break;

      case S_IS_OM:
        unary (l_is_om, block *);
        break;

      case S_IS_OPEN:
        unary (l_is_open, block *);
        break;

      case S_IS_OP:
      case S_IS_OPERATOR:
        unary (l_is_op, block *);
        break;

      case S_IS_PROC:
      case S_IS_PROCEDURE:
        unary (l_is_proc, block *);
        break;

      case S_IS_REAL:
        unary (l_is_real, block *);
        break;

      case S_IS_ROUTINE:
        unary (l_is_routine, block *);
        break;

      case S_IS_SET:
        unary (l_is_set, block *);
        break;

      case S_IS_SMAP:
        unary (l_is_smap, block *);
        break;

      case S_IS_STRING:
        unary (l_is_string, block *);
        break;

      case S_IS_THERE:
        unary (l_is_there, block *);
        break;

      case S_IS_TUPLE:
        unary (l_is_tuple, block *);
        break;

      case S_JOIN:
        rd_rd (l_join, tuple *, string *);
        break;

      case S_LAST_ERRNO:
        nullary (l_last_errno);
        break;

      case S_LAST_ERROR:
        nullary (l_last_error);
        break;

      case S_LEN:
        rw_rd (l_len, string **, integer *);
        break;

      case S_LESS:
        binary (l_less, set *, block *);
        break;

      case S_LESSF:
        binary (l_lessf, set *, block *);
        break;

      case S_LEXISTS:
        unary (l_lexists, string *);
        break;

      case S_LEV:  /* current backtracking depth */
        runerr("LEV not yet implemented");
        break;

      case S_LINK:
        void_rd_rd (l_link, string *, string *);
        break;

      case S_LOG:
        unary (l_log, real *);
        break;

      case S_LPAD:
        rd_rd (l_lpad, string *, integer *);
        break;

      case S_MAGIC:
        nullary (l_magic);
        break;

      case S_MARK:
        rd_rd (l_mark, string *, block *);
        break;

      case S_MATCH:
        rw_rd (l_match, string **, string *);
        break;

      case S_MAX:
        binary (l_max, block *, block *);
        break;

      case S_MEM_ALLOC:
        unary (l_mem_alloc, integer *);
        break;

      case S_MEM_FREE:
        void_rd (l_mem_free, integer *);
        break;

      case S_MEM_REALLOC:
        rd_rd (l_mem_realloc, integer *, integer *);
        break;

      case S_MEM_COPY:
        void_rd_rd_rd (l_mem_copy, integer *, integer *, integer *);
        break;

      case S_MEM_FETCH_STRING:
        rd_rd (l_mem_fetch_string, integer *, integer *);
        break;

      case S_MEM_FETCH_C_STRING:
        unary (l_mem_fetch_c_string, integer *);
        break;

      case S_MEM_STORE_STRING:
        void_rd_rd (l_mem_store_string, string *, integer *);
        break;

      case S_MEM_STORE_C_STRING:
        void_rd_rd (l_mem_store_c_string, string *, integer *);
        break;

      case S_MIN:
        binary (l_min, block *, block *);
        break;

      case S_MKSTEMP:
        rw_unary (l_mkstemp, string **);
        break;

      case S_MOD:
        binary (l_mod, block *, block *);
        break;

      case S_NARGS:
        taut (nactual == 0);
        fp = setl_vm->display[setl_vm->level];
        taut (is_frame(fp));
        let (arg(1), (block *)new_integer(fp->nargs));
        fp = NULL;
        break;

      case S_NEWAT:
        nullary (l_newat);
        break;

      case S_NOT:
        unary (l_not, boolean *);
        break;

      case S_NOTANY:
        rw_rd (l_notany, string **, string *);
        break;

      case S_NOTIN:
        binary (l_notin, block *, block *);
        break;

      case S_NPOW:
        binary (l_npow, block *, block *);
        break;

      case S_ODD:
        unary (l_odd, integer *);
        break;

      case S_OK:
        /*
         *  Save context (including :BACK variables) and yield TRUE.
         *
         *  If FAIL is later called, restore all :BACK variables and
         *  yield FALSE.
         *
         *  This "blind backtracking" feature is poorly defined and
         *  would be at best marginally useful.  :BACK would seem a
         *  good rule for all locals, and for few if any globals, so
         *  getting rid of :BACK would help.  But then there is still
         *  the question of whether you want to be able to do a FAIL
         *  even after a routine that has called the preceding OK has
         *  returned, and thereby restore all the "popped-off" call
         *  context, or rather impose a restriction along the lines of
         *  the C setjmp/longjmp, where only the routine or something
         *  it has directly or indirectly called can do the FAIL
         *  (analogue of longjmp).
         */
        runerr("OK (nondeterministic BOOLEAN generator) not implemented");
        break;

      case S_OPEN:
        rd_rd (l_open, block *, string *);
        break;

      case S_OR:
        binary (l_or, boolean *, boolean *);
        break;

      case S_PACK_SHORT:
        unary (l_pack_short, integer *);
        break;

      case S_PACK_UNSIGNED_SHORT:
        unary (l_pack_unsigned_short, integer *);
        break;

      case S_PACK_INT:
        unary (l_pack_int, integer *);
        break;

      case S_PACK_UNSIGNED_INT:
        unary (l_pack_unsigned_int, integer *);
        break;

      case S_PACK_LONG:
        unary (l_pack_long, integer *);
        break;

      case S_PACK_UNSIGNED_LONG:
        unary (l_pack_unsigned_long, integer *);
        break;

      case S_PACK_LONG_LONG:
        unary (l_pack_long_long, integer *);
        break;

      case S_PACK_UNSIGNED_LONG_LONG:
        unary (l_pack_unsigned_long_long, integer *);
        break;

      case S_PACK_INTEGER:
        unary (l_pack_integer, integer *);
        break;

      case S_PACK_DOUBLE:
        unary (l_pack_double, real *);
        break;

      case S_PACK_FLOAT:
        unary (l_pack_float, real *);
        break;

      case S_PACK_REAL:
        unary (l_pack_real, real *);
        break;

      case S_PEEKC:
        unary (l_peekc, block *);
        break;

      case S_PEEKCHAR:
        nullary (l_peekchar);
        break;

      case S_PEER_ADDRESS:
        unary (l_peer_address, block *);
        break;

      case S_PEER_NAME:
        unary (l_peer_name, block *);
        break;

      case S_PEER_PORT:
        unary (l_peer_port, block *);
        break;

      case S_PEER_SOCKADDR:
        unary (l_peer_sockaddr, block *);
        break;

      case S_PEXISTS:
        unary (l_pexists, integer *);
        break;

      case S_PIPE:
        nullary (l_pipe);
        break;

      case S_PIPE_FROM_CHILD:
        nullary (l_pipe_from_child);
        break;

      case S_PIPE_TO_CHILD:
        nullary (l_pipe_to_child);
        break;

      case S_PORT:
        unary (l_port, block *);
        break;

      case S_POW:
        unary (l_pow, set *);
        break;

      case S_PRETTY:
        unary (l_pretty, block *);
        break;

      case S_PUMP:
        nullary (l_pump);
        break;

      case S_PUTC:
        void_rd_rd (l_putc, block *, string *);
        break;

      case S_PUTCHAR:
        void_rd (l_putchar, string *);
        break;

      case S_PUTENV:
        void_rd (l_putenv, string *);
        break;

      case S_PUTFILE:
        void_rd_rd (l_putfile, block *, string *);
        break;

      case S_PUTS:
        void_rd_rd_rd (l_puts, block *, integer *, string *);
        break;

      case S_RANDOM:
        unary (l_random, block *);
        break;

      case S_RANGE:
        unary (l_range, set *);
        break;

      case S_RANY:
        rw_rd (l_rany, string **, string *);
        break;

      case S_RBREAK:
        rw_rd (l_rbreak, string **, string *);
        break;

      case S_READLINK:
        unary (l_readlink, string *);
        break;

      case S_RECV:
        unary (l_recv, block *);
        break;

      case S_RECVFROM:
        unary (l_recvfrom, block *);
        break;

      case S_RECV_FD:
        unary (l_recv_fd, block *);
        break;

      case S_REM:
        binary (l_rem, integer *, integer *);
        break;

      case S_RENAME:
        void_rd_rd (l_rename, string *, string *);
        break;

      case S_REVERSE:
        unary (l_reverse, string *);
        break;

      case S_REWIND:
        void_rd (l_rewind, block *);
        break;

      case S_RLEN:
        rw_rd (l_rlen, string **, integer *);
        break;

      case S_RMATCH:
        rw_rd (l_rmatch, string **, string *);
        break;

      case S_RNOTANY:
        rw_rd (l_rnotany, string **, string *);
        break;

      case S_ROUND:
        unary (l_round, real *);
        break;

      case S_RPAD:
        rd_rd (l_rpad, string *, integer *);
        break;

      case S_RSPAN:
        rw_rd (l_rspan, string **, string *);
        break;

      case S_SEND:
        void_rd_rd (l_send, block *, string *);
        break;

      case S_SENDTO:
        void_rd_rd_rd (l_sendto, block *, block *, string *);
        break;

      case S_SEND_FD:
        void_rd_rd (l_send_fd, block *, integer *);
        break;

      case S_SETCTTY:
        void_rd (l_setctty, block *);
        break;

      case S_SETEGID:
        void_rd (l_setegid, integer *);
        break;

      case S_SETEM:
        void_rd_rd (l_setem, block *, block *);
        break;

      case S_SETEUID:
        void_rd (l_seteuid, integer *);
        break;

      case S_SETGID:
        void_rd (l_setgid, integer *);
        break;

      case S_SETPGID:
        void_rd_rd (l_setpgid, integer *, integer *);
        break;

      case S_SETPGRP:
        void_nullary (l_setpgrp);
        break;

      case S_SETRANDOM:
        void_rd (l_setrandom, integer *);
        /* gmp_randseed() indirectly calls internal GMP randseed_mt()
         * which does non-LIFO allocation/freeing of some big integers;
         * it does eventually give back everything that it takes */
        lifo_arena_use = false;
        break;

      case S_SETSID:
        void_nullary (l_setsid);
        break;

      case S_SETUID:
        void_rd (l_setuid, integer *);
        break;

      case S_SET_INTSLASH:
        unary (l_set_intslash, boolean *);
        break;

      case S_SET_MAGIC:
        unary (l_set_magic, boolean *);
        break;

      case S_SHUTDOWN:
        void_rd_rd (l_shutdown, block *, integer *);
        break;

      case S_SIGN:
        unary (l_sign, block *);
        break;

      case S_SIN:
        unary (l_sin, real *);
        break;

      case S_SINH:
        unary (l_sinh, real *);
        break;

      case S_SOCKADDR:
        unary (l_sockaddr, block *);
        break;

      case S_SOCKETPAIR:
        nullary (l_socketpair);
        break;

      case S_SPAN:
        rw_rd (l_span, string **, string *);
        break;

      case S_SQRT:
        unary (l_sqrt, real *);
        break;

      case S_STATUS:
        nullary (l_status);
        break;

      case S_STR:
        unary (l_str, block *);
        break;

      case S_STRAD:
        rd_rd (l_strad, integer *, integer *);
        break;

      case S_SUBSET:
        binary (l_subset, set *, set *);
        break;

      case S_SYMLINK:
        void_rd_rd (l_symlink, string *, string *);
        break;

      case S_SYSTEM:
        unary (l_system, string *);
        break;

      case S_SYS_READ:
        rd_rd (l_sys_read, integer *, integer *);
        break;

      case S_SYS_WRITE:
        rd_rd (l_sys_write, integer *, string *);
        break;

      case S_TAN:
        unary (l_tan, real *);
        break;

      case S_TANH:
        unary (l_tanh, real *);
        break;

      case S_TCGETPGRP:
        unary (l_tcgetpgrp, block *);
        break;

      case S_TCSETPGRP:
        void_rd_rd (l_tcsetpgrp, block *, integer *);
        break;

      case S_TIE:
        void_rd_rd (l_tie, block *, block *);
        break;

      case S_TIME:
        nullary (l_time);
        break;

      case S_TMPNAM:
        nullary (l_tmpnam);
        break;

      case S_TO_LOWER:
        unary (l_to_lower, string *);
        break;

      case S_TO_UPPER:
        unary (l_to_upper, string *);
        break;

      case S_TOD:
        nullary (l_tod);
        break;

      case S_TTY_PUMP:
        nullary (l_tty_pump);
        break;

      case S_TYPE:
        unary (l_type, block *);
        break;

      case S_UNGETC:
        void_rd_rd (l_ungetc, block *, string *);
        break;

      case S_UNGETCHAR:
        void_rd (l_ungetchar, string *);
        break;

      case S_UNHEX:
        unary (l_unhex, string *);
        break;

      case S_UNLINK:
        void_rd (l_unlink, string *);
        break;

      case S_UNPACK_SHORT:
        unary (l_unpack_short, string *);
        break;

      case S_UNPACK_UNSIGNED_SHORT:
        unary (l_unpack_unsigned_short, string *);
        break;

      case S_UNPACK_INT:
        unary (l_unpack_int, string *);
        break;

      case S_UNPACK_UNSIGNED_INT:
        unary (l_unpack_unsigned_int, string *);
        break;

      case S_UNPACK_LONG:
        unary (l_unpack_long, string *);
        break;

      case S_UNPACK_UNSIGNED_LONG:
        unary (l_unpack_unsigned_long, string *);
        break;

      case S_UNPACK_LONG_LONG:
        unary (l_unpack_long_long, string *);
        break;

      case S_UNPACK_UNSIGNED_LONG_LONG:
        unary (l_unpack_unsigned_long_long, string *);
        break;

      case S_UNPACK_INTEGER:
        unary (l_unpack_integer, string *);
        break;

      case S_UNPACK_DOUBLE:
        unary (l_unpack_double, string *);
        break;

      case S_UNPACK_FLOAT:
        unary (l_unpack_float, string *);
        break;

      case S_UNPACK_REAL:
        unary (l_unpack_real, string *);
        break;

      case S_UNPRETTY:
        unary (l_unpretty, string *);
        break;

      case S_UNSETCTTY:
        void_rd (l_unsetctty, block *);
        break;

      case S_UNSETENV:
        void_rd (l_unsetenv, string *);
        break;

      case S_UNSTR:
        unary (l_unstr, string *);
        break;

      case S_VAL:
        unary (l_val, string *);
        break;

      case S_WHOLE:
        rd_rd (l_whole, block *, integer *);
        break;

      case S_WITH:
        binary (l_with, block *, block *);
        break;

#include "custom.dispatch"

      default:
        runerr("Not yet implemented");

      } /* end switch (xp->callee.sysrot) under case op_scall */

      xp = NULL;
      break;

    case op_scallv:  /* call to variadic built-in library routine */
      xp = (call *)ip;
      taut (xp->nopnd >= 2);
      nactual = xp->nopnd - 2;  /* user-provided args (not RET) */

#     define unary_star(f,tt,tn) {\
        taut (nactual >= 0);\
        nformal = 1;\
        form_tup;  /* tup := a tuple of the optional args */\
        let (arg(nactual+1), (block *)(f)(tup, nactual));\
        tup = NULL;\
      }

#     define void_wr_star(f,tt,tn) {\
        taut (nactual >= 0);\
        nformal = 1;\
        (f)(&tup, nactual);\
        distribute_tup;  /* move tup elements to the optional args */\
      }

#     define void_rw_star(f,tt,tn) {\
        taut (nactual >= 0);\
        nformal = 1;\
        form_tup;  /* tup := a tuple of the optional args */\
        (f)(&tup, nactual);\
        distribute_tup;  /* move tup elements to the optional args */\
      }

#     define void_rd_wr_star(f,t1,tt,tn) {\
        taut (nactual >= 1);\
        nformal = 2;\
        (f)((t1)arg(1),\
            &tup, nactual);\
        distribute_tup;  /* move tup elements to the optional args */\
      }

#     define rw_rd_rd_star(f,t1,t2,tt,tn) {\
        taut (nactual >= 2);\
        nformal = 3;\
        tp = arg(1);\
        form_tup;  /* tup := a tuple of the optional args */\
        let (arg(nactual+1), (block *)(f)((t1)(void *)&tp,\
                                          (t2)arg(2),\
                                          tup, nactual));\
        tup = NULL;\
        arg(1) = tp; tp = NULL;\
      }

#     define rd_rd_star(f,t1,tt,tn) {\
        taut (nactual >= 1);\
        nformal = 2;\
        form_tup;  /* tup := a tuple of the optional args */\
        let (arg(nactual+1), (block *)(f)((t1)arg(1),\
                                          tup, nactual));\
        tup = NULL;\
      }

#     define rd_rd_rd_star(f,t1,t2,tt,tn) {\
        taut (nactual >= 2);\
        nformal = 3;\
        form_tup;  /* tup := a tuple of the optional args */\
        let (arg(nactual+1), (block *)(f)((t1)arg(1),\
                                          (t2)arg(2),\
                                          tup, nactual));\
        tup = NULL;\
      }

#     define void_rd_star(f,tt,tn) {\
        taut (nactual >= 0);\
        nformal = 1;\
        form_tup;  /* tup := a tuple of the optional args */\
        (f)(tup, nactual);\
        tup = NULL;\
      }

#     define void_rd_rd_star(f,t1,tt,tn) {\
        taut (nactual >= 1);\
        nformal = 2;\
        form_tup;  /* tup := a tuple of the optional args */\
        (f)((t1)arg(1),\
            tup, nactual);\
        tup = NULL;\
      }

      switch (xp->callee.sysrot) {

      case S_CHDIR:
        void_rd_star (l_chdir, tuple *, long);
        break;

      case S_CLOSE:
        void_rd_rd_star (l_close, block *, tuple *, long);
        break;

      case S_EJECT:
        void_rd_star (l_eject, tuple *, long);
        break;

      case S_EOF:
        unary_star (l_eof, tuple *, long);
        break;

      case S_EXEC:
        void_rd_rd_star (l_exec, string *, tuple *, long);
        break;

      case S_FDATE:
        rd_rd_star (l_fdate, integer *, tuple *, long);
        break;

      case S_FILTER:
        rd_rd_star (l_filter, string *, tuple *, long);
        break;

      case S_GET:
        void_wr_star (l_get, tuple **, long);
        break;

      case S_GETA:
        void_rd_wr_star (l_geta, block *, tuple **, long);
        break;

      case S_GETB:
        void_rd_wr_star (l_getb, block *, tuple **, long);
        break;

      case S_GETF:
        void_rd_wr_star (l_getf, block *, tuple **, long);
        break;

      case S_GETK:
        void_rw_star (l_getk, tuple **, long);
        break;

      case S_GETSID:
        unary_star (l_getsid, tuple *, long);
        break;

      case S_GSUB:
        rw_rd_rd_star (l_gsub, string **, block *, tuple *, long);
        break;

      case S_HOST:
        unary_star (l_host, tuple *, long);
        break;

      case S_IP_ADDRESSES:
        unary_star (l_ip_addresses, tuple *, long);
        break;

      case S_IP_NAMES:
        unary_star (l_ip_names, tuple *, long);
        break;

      case S_KILL:
        void_rd_rd_star (l_kill, integer *, tuple *, long);
        break;

      case S_NPRINT:
        void_rd_star (l_nprint, tuple *, long);
        break;

      case S_NPRINTA:
        void_rd_rd_star (l_nprinta, block *, tuple *, long);
        break;

      case S_PID:
        unary_star (l_pid, tuple *, long);
        break;

      case S_PRINT:
        void_rd_star (l_print, tuple *, long);
        break;

      case S_PRINTA:
        void_rd_rd_star (l_printa, block *, tuple *, long);
        break;

      case S_PUT:
        void_rd_star (l_put, tuple *, long);
        break;

      case S_PUTA:
        void_rd_rd_star (l_puta, block *, tuple *, long);
        break;

      case S_PUTB:
        void_rd_rd_star (l_putb, block *, tuple *, long);
        break;

      case S_PUTF:
        void_rd_rd_star (l_putf, block *, tuple *, long);
        break;

      case S_PUTK:
        void_rw_star (l_putk, tuple **, long);
        break;

      case S_PUTLINE:
        void_rd_rd_star (l_putline, block *, tuple *, long);
        break;

      case S_READ:
        void_wr_star (l_read, tuple **, long);
        break;

      case S_READA:
        void_rd_wr_star (l_reada, block *, tuple **, long);
        break;

      case S_READS:
        void_rd_wr_star (l_reads, string *, tuple **, long);
        break;

      case S_SEEK:
        rd_rd_rd_star (l_seek, block *, integer *, tuple *, long);
        break;

      case S_SELECT:
        rd_rd_star (l_select, tuple *, tuple *, long);
        break;

      case S_SETENV:
        void_rd_rd_star (l_setenv, string *, tuple *, long);
        break;

      case S_SPLIT:
        rd_rd_star (l_split, string *, tuple *, long);
        break;

      case S_SUB:
        rw_rd_rd_star (l_sub, string **, block *, tuple *, long);
        break;

      case S_TITLE:
        void_rd_star (l_title, tuple *, long);
        break;

      case S_UMASK:
        unary_star (l_umask, tuple *, long);
        break;

      case S_WAIT:
        unary_star (l_wait, tuple *, long);
        break;

      case S_WAITPID:
        rd_rd_star (l_waitpid, integer *, tuple *, long);
        break;

      case S_WRITE:
        void_rd_star (l_write, tuple *, long);
        break;

      case S_WRITEA:
        void_rd_rd_star (l_writea, block *, tuple *, long);
        break;

      default:
        runerr("Not yet implemented");

      } /* end switch (xp->callee.sysrot) under case op_scallv */

      xp = NULL;
      break;

    case op_vscall:  /* call with (*) to necessarily variadic built-in */
      xp = (call *)ip;  /* the present call instr in all its glory */
      taut (xp->nopnd >= 3);  /* callee + 0 or more args + (*) arg + RET */

      /* "borrow" fp (frame ptr) and pp (proc ptr) for the caller
       * context, to derive caller_nopt */
      fp = setl_vm->display[setl_vm->level];  /* current (caller) frame */
      pp = (proc *)tupelt(setl_vm->code, fp->proc_pc);  /* and instr */
      taut (pp->op == op_vproc);  /* vcalls only allowed from variadics */
      caller_nformal = proc_nformal(pp);  /* caller's own #formals */
      taut (caller_nformal >= 1);  /* caller declared a (*) formal */
      pp = NULL;
      /* how many actuals were passed to this caller in the (*) arg:  */
      caller_nopt = fp->nargs - (caller_nformal - 1);
      fp = NULL;

      nactual = xp->nopnd - 2;  /* user-provided args (not RET) */
      nargs = (nactual - 1) + caller_nopt;  /* SETL-level NARGS */

#     define v_unary_star(f,tt,tn) {\
        nformal = 1;\
        taut (nactual >= nformal);\
        v_form_tup;  /* tup := ["extra" args] + starred arg */\
        let (arg(nactual+1), (block *)(f)(tup, nargs));\
        tup = NULL;\
      }

#     define v_void_wr_star(f,tt,tn) {\
        nformal = 1;\
        taut (nactual >= nformal);\
        (f)(&tup, nargs);\
        v_distribute_tup;  /* move tup elements to "extras" and opt args */\
      }

#     define v_void_rw_star(f,tt,tn) {\
        nformal = 1;\
        taut (nactual >= nformal);\
        v_form_tup;  /* tup := ["extra" args] + starred arg */\
        (f)(&tup, nargs);\
        v_distribute_tup;  /* move tup elements to "extras" and opt args */\
      }

#     define v_void_rd_wr_star(f,t1,tt,tn) {\
        nformal = 2;\
        taut (nactual >= nformal);\
        (f)((t1)arg(1),\
            &tup, nargs);\
        v_distribute_tup;  /* move tup elements to "extras" and opt args */\
      }

#     define v_rw_rd_rd_star(f,t1,t2,tt,tn) {\
        nformal = 3;\
        taut (nactual >= nformal);\
        tp = arg(1);\
        v_form_tup;  /* tup := ["extra" args] + starred arg */\
        let (arg(nactual+1), (block *)(f)((t1)(void *)&tp,\
                                          (t2)arg(2),\
                                          tup, nargs));\
        tup = NULL;\
        arg(1) = tp; tp = NULL;\
      }

#     define v_rd_rd_star(f,t1,tt,tn) {\
        nformal = 2;\
        taut (nactual >= nformal);\
        v_form_tup;  /* tup := ["extra" args] + starred arg */\
        let (arg(nactual+1), (block *)(f)((t1)arg(1),\
                                        tup, nargs));\
        tup = NULL;\
      }

#     define v_rd_rd_rd_star(f,t1,t2,tt,tn) {\
        nformal = 3;\
        taut (nactual >= nformal);\
        v_form_tup;  /* tup := ["extra" args] + starred arg */\
        let (arg(nactual+1), (block *)(f)((t1)arg(1),\
                                          (t2)arg(2),\
                                          tup, nargs));\
        tup = NULL;\
      }

#     define v_void_rd_star(f,tt,tn) {\
        nformal = 1;\
        taut (nactual >= nformal);\
        v_form_tup;  /* tup := ["extra" args] + starred arg */\
        (f)(tup, nargs);\
        tup = NULL;\
      }

#     define v_void_rd_rd_star(f,t1,tt,tn) {\
        nformal = 2;\
        taut (nactual >= nformal);\
        v_form_tup;  /* tup := ["extra" args] + starred arg */\
        (f)((t1)arg(1),\
            tup, nargs);\
        tup = NULL;\
      }

      switch (xp->callee.sysrot) {

      case S_CHDIR:
        v_void_rd_star (l_chdir, tuple *, long);
        break;

      case S_CLOSE:
        v_void_rd_rd_star (l_close, block *, tuple *, long);
        break;

      case S_EJECT:
        v_void_rd_star (l_eject, tuple *, long);
        break;

      case S_EOF:
        v_unary_star (l_eof, tuple *, long);
        break;

      case S_EXEC:
        v_void_rd_rd_star (l_exec, string *, tuple *, long);
        break;

      case S_FDATE:
        v_rd_rd_star (l_fdate, integer *, tuple *, long);
        break;

      case S_FILTER:
        v_rd_rd_star (l_filter, string *, tuple *, long);
        break;

      case S_GET:
        v_void_wr_star (l_get, tuple **, long);
        break;

      case S_GETA:
        v_void_rd_wr_star (l_geta, block *, tuple **, long);
        break;

      case S_GETB:
        v_void_rd_wr_star (l_getb, block *, tuple **, long);
        break;

      case S_GETF:
        v_void_rd_wr_star (l_getf, block *, tuple **, long);
        break;

      case S_GETK:
        v_void_rw_star (l_getk, tuple **, long);
        break;

      case S_GETSID:
        v_unary_star (l_getsid, tuple *, long);
        break;

      case S_GSUB:
        v_rw_rd_rd_star (l_gsub, string **, block *, tuple *, long);
        break;

      case S_HOST:
        v_unary_star (l_host, tuple *, long);
        break;

      case S_IP_ADDRESSES:
        v_unary_star (l_ip_addresses, tuple *, long);
        break;

      case S_IP_NAMES:
        v_unary_star (l_ip_names, tuple *, long);
        break;

      case S_KILL:
        v_void_rd_rd_star (l_kill, integer *, tuple *, long);
        break;

      case S_NPRINT:
        v_void_rd_star (l_nprint, tuple *, long);
        break;

      case S_NPRINTA:
        v_void_rd_rd_star (l_nprinta, block *, tuple *, long);
        break;

      case S_PID:
        v_unary_star (l_pid, tuple *, long);
        break;

      case S_PRINT:
        v_void_rd_star (l_print, tuple *, long);
        break;

      case S_PRINTA:
        v_void_rd_rd_star (l_printa, block *, tuple *, long);
        break;

      case S_PUT:
        v_void_rd_star (l_put, tuple *, long);
        break;

      case S_PUTA:
        v_void_rd_rd_star (l_puta, block *, tuple *, long);
        break;

      case S_PUTB:
        v_void_rd_rd_star (l_putb, block *, tuple *, long);
        break;

      case S_PUTF:
        v_void_rd_rd_star (l_putf, block *, tuple *, long);
        break;

      case S_PUTK:
        v_void_rw_star (l_putk, tuple **, long);
        break;

      case S_PUTLINE:
        v_void_rd_rd_star (l_putline, block *, tuple *, long);
        break;

      case S_READ:
        v_void_wr_star (l_read, tuple **, long);
        break;

      case S_READA:
        v_void_rd_wr_star (l_reada, block *, tuple **, long);
        break;

      case S_READS:
        v_void_rd_wr_star (l_reads, string *, tuple **, long);
        break;

      case S_SEEK:
        v_rd_rd_rd_star (l_seek, block *, integer *, tuple *, long);
        break;

      case S_SELECT:
        v_rd_rd_star (l_select, tuple *, tuple *, long);
        break;

      case S_SETENV:
        v_void_rd_rd_star (l_setenv, string *, tuple *, long);
        break;

      case S_SPLIT:
        v_rd_rd_star (l_split, string *, tuple *, long);
        break;

      case S_SUB:
        v_rw_rd_rd_star (l_sub, string **, block *, tuple *, long);
        break;

      case S_TITLE:
        v_void_rd_star (l_title, tuple *, long);
        break;

      case S_UMASK:
        v_unary_star (l_umask, tuple *, long);
        break;

      case S_WAIT:
        v_unary_star (l_wait, tuple *, long);
        break;

      case S_WAITPID:
        v_rd_rd_star (l_waitpid, integer *, tuple *, long);
        break;

      case S_WRITE:
        v_void_rd_star (l_write, tuple *, long);
        break;

      case S_WRITEA:
        v_void_rd_rd_star (l_writea, block *, tuple *, long);
        break;

      default:
        runerr("Not yet implemented");

      } /* end switch (xp->callee.sysrot) under case op_vscall */

      xp = NULL;
      break;

    case op_scall_assigning:  /* lhs binop:= rhs for built-in binop */
      xp = (call *)ip;  /* the present call instr in all its glory */
      taut (xp->nopnd == 3);  /* binop + lhs + rhs */

#     define assigning(f,t1,t2) {\
        let (arg(1), (block *)(f)((t1)arg(1),\
                                  (t2)arg(2)));\
      }

#     define update(f,t1,t2) {\
        tp = arg(1);\
        (f)((t1 *)(void *)&tp,\
            (t2)arg(2));\
        arg(1) = tp;\
        tp = NULL;\
      }

      switch (xp->callee.sysrot) {

      case S_star:
        assigning (l_star, block *, block *);
        break;

      case S_power:
        assigning (l_power, block *, block *);
        break;

      case S_plus_1:
        update (l_aplus, block *, block *);
        break;

      case S_minus_1:
        update (l_aminus, block *, block *);
        break;

      case S_slash:
        assigning (l_slash, block *, block *);
        break;

      case S_ne:
        assigning (l_ne, block *, block *);
        break;

      case S_lt:
        assigning (l_lt, block *, block *);
        break;

      case S_le:
        assigning (l_le, block *, block *);
        break;

      case S_eq:
        assigning (l_eq, block *, block *);
        break;

      case S_gt:
        assigning (l_gt, block *, block *);
        break;

      case S_ge:
        assigning (l_ge, block *, block *);
        break;

      case S_query:
        assigning (l_query, block *, block *);
        break;

      case S_AND:
        assigning (l_and, boolean *, boolean *);
        break;

      case S_ATAN2:
        assigning (l_atan2, real *, real *);
        break;

      case S_BIT_AND:
        assigning (l_bit_and, integer *, integer *);
        break;

      case S_BIT_OR:
        assigning (l_bit_or, integer *, integer *);
        break;

      case S_BIT_XOR:
        assigning (l_bit_xor, integer *, integer *);
        break;

      case S_DIV:
        assigning (l_div, integer *, integer *);
        break;

      case S_IMPL:
        assigning (l_impl, boolean *, boolean *);
        break;

      case S_IN:
        assigning (l_in, block *, block *);
        break;

      case S_INCS:
        assigning (l_incs, set *, set *);
        break;

      case S_LESS:
        update (l_aless, set *, block *);
        break;

      case S_LESSF:
        update (l_alessf, set *, block *);
        break;

      case S_MAX:
        assigning (l_max, block *, block *);
        break;

      case S_MIN:
        assigning (l_min, block *, block *);
        break;

      case S_MOD:
        assigning (l_mod, block *, block *);
        break;

      case S_NOTIN:
        assigning (l_notin, block *, block *);
        break;

      case S_NPOW:
        assigning (l_npow, block *, block *);
        break;

      case S_OR:
        assigning (l_or, boolean *, boolean *);
        break;

      case S_REM:
        assigning (l_rem, integer *, integer *);
        break;

      case S_SUBSET:
        assigning (l_subset, set *, set *);
        break;

      case S_WITH:
        update (l_awith, block *, block *);
        break;

      default:
        bugerr("Assigning form not yet implemented for this operator");

      } /* end switch (xp->callee.sysrot) under op_scall_assigning */

      xp = NULL;
      break;

    case op_scall_bincomb:  /* x binop/ t for built-in binop */
      xp = (call *)ip;  /* the present call instr in all its glory */
      taut (xp->nopnd == 4);  /* binop + x + t + RET */

#     define bincomb(f,t1,t2) {\
        jp = init_iterator(arg(2));\
        tp = copy_value(arg(1));\
        up = NULL;\
        while (step_iterator(&jp,&up)) {\
          tp = (block *)(f)((t1)tp,\
                            (t2)up);\
        }\
        arg(3) = tp;\
        jp = NULL;\
        tp = NULL;\
        up = NULL;\
      }

#     define special_bincomb(f,t1,t2) {\
        jp = init_iterator(arg(2));\
        tp = copy_value(arg(1));\
        up = NULL;\
        while (step_iterator(&jp,&up)) {\
          (f)((t1 *)(void *)&tp,\
              (t2)up);\
        }\
        arg(3) = tp;\
        jp = NULL;\
        tp = NULL;\
        up = NULL;\
      }

      switch (xp->callee.sysrot) {

      case S_star:
        bincomb (l_star, block *, block *);
        break;

      case S_power:
        bincomb (l_power, block *, block *);
        break;

      case S_plus_2:
        special_bincomb (l_aplus, block *, block *);
        break;

      case S_minus_2:
        special_bincomb (l_aminus, block *, block *);
        break;

      case S_slash:
        bincomb (l_slash, block *, block *);
        break;

      case S_ne:
        bincomb (l_ne, block *, block *);
        break;

      case S_lt:
        bincomb (l_lt, block *, block *);
        break;

      case S_le:
        bincomb (l_le, block *, block *);
        break;

      case S_eq:
        bincomb (l_eq, block *, block *);
        break;

      case S_gt:
        bincomb (l_gt, block *, block *);
        break;

      case S_ge:
        bincomb (l_ge, block *, block *);
        break;

      case S_query:
        bincomb (l_query, block *, block *);
        break;

      case S_AND:
        bincomb (l_and, boolean *, boolean *);
        break;

      case S_ATAN2:
        bincomb (l_atan2, real *, real *);
        break;

      case S_BIT_AND:
        bincomb (l_bit_and, integer *, integer *);
        break;

      case S_BIT_OR:
        bincomb (l_bit_or, integer *, integer *);
        break;

      case S_BIT_XOR:
        bincomb (l_bit_xor, integer *, integer *);
        break;

      case S_DIV:
        bincomb (l_div, integer *, integer *);
        break;

      case S_IMPL:
        bincomb (l_impl, boolean *, boolean *);
        break;

      case S_IN:
        bincomb (l_in, block *, block *);
        break;

      case S_INCS:
        bincomb (l_incs, set *, set *);
        break;

      case S_LESS:
        special_bincomb (l_aless, set *, block *);
        break;

      case S_LESSF:
        special_bincomb (l_alessf, set *, block *);
        break;

      case S_MAX:
        bincomb (l_max, block *, block *);
        break;

      case S_MIN:
        bincomb (l_min, block *, block *);
        break;

      case S_MOD:
        bincomb (l_mod, block *, block *);
        break;

      case S_NOTIN:
        bincomb (l_notin, block *, block *);
        break;

      case S_NPOW:
        bincomb (l_npow, block *, block *);
        break;

      case S_OR:
        bincomb (l_or, boolean *, boolean *);
        break;

      case S_REM:
        bincomb (l_rem, integer *, integer *);
        break;

      case S_SUBSET:
        bincomb (l_subset, set *, set *);
        break;

      case S_WITH:
        special_bincomb (l_awith, block *, block *);
        break;

      default:
        bugerr("Binary combining form not yet implemented for this operator");

      } /* end switch (xp->callee.sysrot) under op_scall_bincomb */

      xp = NULL;
      break;

    case op_scall_uncomb:  /* binop/ t for built-in binop */
      xp = (call *)ip;  /* the present call instr in all its glory */
      taut (xp->nopnd == 3);  /* binop + t + RET */

#     define uncomb(f,t1,t2) {\
        jp = init_iterator(arg(1));\
        tp = NULL;\
        if (step_iterator(&jp,&tp)) {\
          up = NULL;\
          while (step_iterator(&jp,&up)) {\
            tp = (block *)(f)((t1)tp,\
                              (t2)up);\
          }\
          arg(2) = tp;\
          up = NULL;\
        } else {\
          arg(2) = NULL;\
        }\
        jp = NULL;\
        tp = NULL;\
      }

#     define special_uncomb(f,t1,t2) {\
        jp = init_iterator(arg(1));\
        tp = NULL;\
        if (step_iterator(&jp,&tp)) {\
          up = NULL;\
          while (step_iterator(&jp,&up)) {\
            (f)((t1 *)(void *)&tp,\
                (t2)up);\
          }\
          arg(2) = tp;\
          up = NULL;\
        } else {\
          arg(2) = NULL;\
        }\
        jp = NULL;\
        tp = NULL;\
      }

      switch (xp->callee.sysrot) {

      case S_star:
        uncomb (l_star, block *, block *);
        break;

      case S_power:
        uncomb (l_power, block *, block *);
        break;

      case S_plus_1:
        special_uncomb (l_aplus, block *, block *);
        break;

      case S_minus_1:
        special_uncomb (l_aminus, block *, block *);
        break;

      case S_slash:
        uncomb (l_slash, block *, block *);
        break;

      case S_ne:
        uncomb (l_ne, block *, block *);
        break;

      case S_lt:
        uncomb (l_lt, block *, block *);
        break;

      case S_le:
        uncomb (l_le, block *, block *);
        break;

      case S_eq:
        uncomb (l_eq, block *, block *);
        break;

      case S_gt:
        uncomb (l_gt, block *, block *);
        break;

      case S_ge:
        uncomb (l_ge, block *, block *);
        break;

      case S_query:
        uncomb (l_query, block *, block *);
        break;

      case S_AND:
        uncomb (l_and, boolean *, boolean *);
        break;

      case S_ATAN2:
        uncomb (l_atan2, real *, real *);
        break;

      case S_BIT_AND:
        uncomb (l_bit_and, integer *, integer *);
        break;

      case S_BIT_OR:
        uncomb (l_bit_or, integer *, integer *);
        break;

      case S_BIT_XOR:
        uncomb (l_bit_xor, integer *, integer *);
        break;

      case S_DIV:
        uncomb (l_div, integer *, integer *);
        break;

      case S_IMPL:
        uncomb (l_impl, boolean *, boolean *);
        break;

      case S_IN:
        uncomb (l_in, block *, block *);
        break;

      case S_INCS:
        uncomb (l_incs, set *, set *);
        break;

      case S_LESS:
        special_uncomb (l_aless, set *, block *);
        break;

      case S_LESSF:
        special_uncomb (l_alessf, set *, block *);
        break;

      case S_MAX:
        uncomb (l_max, block *, block *);
        break;

      case S_MIN:
        uncomb (l_min, block *, block *);
        break;

      case S_MOD:
        uncomb (l_mod, block *, block *);
        break;

      case S_NOTIN:
        uncomb (l_notin, block *, block *);
        break;

      case S_NPOW:
        uncomb (l_npow, block *, block *);
        break;

      case S_OR:
        uncomb (l_or, boolean *, boolean *);
        break;

      case S_REM:
        uncomb (l_rem, integer *, integer *);
        break;

      case S_SUBSET:
        uncomb (l_subset, set *, set *);
        break;

      case S_WITH:
        special_uncomb (l_awith, block *, block *);
        break;

      default:
        bugerr("Unary combining form not yet implemented for this operator");

      } /* end switch (xp->callee.sysrot) under op_scall_uncomb */

      xp = NULL;
      break;

    case op_copy:
      taut (ip->nopnd == 2);
      let (var(2), copy_value(var(1)));
      break;

    case op_copy_from_mmap:
      taut (ip->nopnd == 3);
      let (var(3), (block *)mmap_fetch((set *)var(1),var(2)));
      break;

    case op_copy_from_slice:
      taut (ip->nopnd == 4);
      let (var(4), slice_fetch(var(1),var(2),var(3)));
      break;

    case op_copy_from_smap:
      taut (ip->nopnd == 3);
      let (var(3), smap_fetch(var(1),var(2)));
      break;

    case op_copy_indexed:
      taut (ip->nopnd == 3);
      tup = (tuple *)var(1);
      if (tup == NULL) {
        var(3) = OM;  /* NULL */
      } else if (is_tuple(tup)) {
        long indx = opnd(2).longval;
        taut (indx >= 1);
        if (indx > tup->nelt) {
          var(3) = OM;  /* NULL */
        } else {
          let (var(3), copy_value(tupelt(tup,indx)));
        }
        tup = NULL;
      } else {
        runerr("Cannot tuple-assign from non-TUPLE");
      }
      break;

    case op_copy_into_mmap:
      taut (ip->nopnd == 3);
      up = var(2);
      mmap_insert((set **)(void *)&up, var(3), (set *)var(1));
      var(2) = up;
      up = NULL;
      break;

    case op_copy_into_slice:
      taut (ip->nopnd == 4);
      up = var(2);
      slice_insert(&up, var(3), var(4), var(1));
      var(2) = up;
      up = NULL;
      break;

    case op_copy_into_smap:
      taut (ip->nopnd == 3);
      up = var(2);
      smap_insert(&up, var(3), var(1));
      var(2) = up;
      up = NULL;
      break;

    case op_cplusplus_code:
    case op_cplusplus_include:
      taut (ip->nopnd == 1);
      /*
       *  Embedded C/C++ code is not supported by this interpreter
       *  (even the concept is suspect), but we might as well just
       *  report it rather than choking on it...
       */
      {
        string *c = (string *)var(1);
        assert (is_string(c));  /* in "pretty" form per assembler */
        if (restricted) {
          runerr("Executing embedded C++ code is restricted");
        }
        print_stderr("SETL VM pretending to execute embedded C++ code %s\n",
                                                        tame(&strelt(c,1)));
      }
      break;

    case op_end:  /* should never be flowed into, per assemble() check */
      unexpected (ip->op);

    case op_extend_set:
      taut (ip->nopnd == 2);
      up = var(2);
      set_insert((set **)(void *)&up, var(1));
      var(2) = up;
      up = NULL;
      break;

    case op_extend_tuple:
      taut (ip->nopnd == 2);
      up = var(2);
      tup_tackon((tuple **)(void *)&up, var(1));
      var(2) = up;
      up = NULL;
      break;

    case op_fail:
      taut (ip->nopnd == 0);
      /*
       *  Leap back to where OK was last called, restore context
       *  including the :BACK variables that were saved at that time,
       *  and make OK yield FALSE.
       */
      runerr("FAIL (make OK return FALSE) not implemented");
      break;

    case op_global:  /* treated as an "assembler directive" */
      unexpected (ip->op);

    case op_init_counter_fl:
      taut (ip->nopnd == 3);
      let (var(3), (block *)init_counter_fl((integer *)var(1),
                                            (integer *)var(2)));
      break;

    case op_init_counter_fnl:
      taut (ip->nopnd == 4);
      let (var(4), (block *)init_counter_fnl((integer *)var(1),
                                             (integer *)var(2),
                                             (integer *)var(3)));
      break;

    case op_init_iterator:
      taut (ip->nopnd == 2);
      let (var(2), (block *)init_iterator(var(1)));
      break;

    case op_init_itermmap:
      taut (ip->nopnd == 2);
      let (var(2), (block *)init_itermmap((set *)var(1)));
      break;

    case op_init_itersmap:
      taut (ip->nopnd == 2);
      let (var(2), (block *)init_itersmap(var(1)));
      break;

    case op_jmp:
      taut (ip->nopnd == 1);
      setl_vm->pc = opnd(1).pc - 1;
      break;

    case op_jmpeq:
      taut (ip->nopnd == 3);
      if (equal_value(var(1),var(2))) setl_vm->pc = opnd(3).pc - 1;
      break;

    case op_jmpne:
      taut (ip->nopnd == 3);
      if (!equal_value(var(1),var(2))) setl_vm->pc = opnd(3).pc - 1;
      break;

    case op_jmpfalse: {
      boolean *b;
      taut (ip->nopnd == 2);
      b = (boolean *)var(1);
      if (!is_boolean(b)) {
        runerr("Subject of test must be BOOLEAN, not %s", TYPENAME(b));
      }
      if (!b->booval) setl_vm->pc = opnd(2).pc - 1;
      b = NULL;
    } break;

    case op_jmptrue: {
      boolean *b;
      taut (ip->nopnd == 2);
      b = (boolean *)var(1);
      if (!is_boolean(b)) {
        runerr("Subject of test must be BOOLEAN, not %s", TYPENAME(b));
      }
      if (b->booval) setl_vm->pc = opnd(2).pc - 1;
      b = NULL;
    } break;

    case op_local:  /* treated as an "assembler directive" */
      unexpected (ip->op);

    case op_machine_code:
      taut (ip->nopnd == 1);
      assert (opnd(1).data.level == GLOBAL_LEVEL);
      /*
       *  A non-dummy implementation might use opnd(1).data.slot to
       *  find the precompiled machine code, but here it just points
       *  to a string containing the original source text for the
       *  code.  We get that string as var(1) and print it on stderr.
       */
      {
        string *c = (string *)var(1);
        assert (is_string(c));
        if (restricted) runerr("Executing \"machine code\" is restricted");
        print_stderr("SETL VM pretending to execute %s\n",
                                        tame(&strelt(c,1)));
      }
      break;

    /*
     *  This is the instruction at which VM execution begins.
     *
     *  Unlike the other "proc" opcodes, it is actually "executed";
     *  the others are retained as descriptors that are used by call
     *  instrs but never flowed into directly (execution on a call
     *  goes to the instr following the proc, after the callee frame
     *  is set up and actual args are copied in to formal parms).
     */
    case op_mainproc:
      taut (ip->nopnd == proc_n_fixed_opnds);  /* 'bow' is empty */
      pp = (proc *)ip;
      /* Use the macro intended for "call" ops, to set up init frame: */
      setup_frame (0, setl_vm->pc);  /* fp := new frame */
      /* That will have left unintended meanings in these 2 fields, so
       * we amend them: */
      fp->caller_pc = -1;  /* we have no caller */
      fp->caller_level = NO_LEVEL;  /* our caller has no level */
      /* This macro used for making a call will do for initting the
       * level and display entry in setl_vm.  The macro's setting of
       * setl_vm->pc to itself (via the 2nd arg to setup_frame just
       * above, which gets put in fp->proc_pc and read by make_call)
       * then comes out as merely redundant: */
      make_call;  /* push initial frame and continue to next instr */
      clear_call_ptrs;  /* clear xp, pp, fp */
      /* Now that a plausible activation record has been built as if by
       * a call instr but without a valid return address, carry on like
       * in a regular proc. */
      break;

    case op_proc:  /* should never be flowed into, per assemble() check */
      unexpected (ip->op);
      break;

    case op_return:  /* return from call to user-defined callee */
      taut (ip->nopnd == 0);
      fp = setl_vm->display[setl_vm->level];  /* callee frame */
      taut (is_frame(fp));
      xp = (call *)tupelt(setl_vm->code,fp->caller_pc);  /* call instr */
      taut (is_instr(xp));
      pp = (proc *)tupelt(setl_vm->code,fp->proc_pc);  /* callee proc */
      taut (is_instr(pp));
#ifdef DEBUG_TRACE
      if (debug) print_stderr("xp->op == %d\n",xp->op);
#endif

      switch (xp->op) {  /* what kind of call are we returning from */

      case op_call:  /* normal call to fixed-valence user-defined callee */
        taut (xp->nopnd >= 2);  /* callee + 0 or more args + RET */
        taut (fp->combiter == NULL);
        taut (pp->op == op_proc);  /* returning from non-variadic callee */
        pop_frame;  /* restore caller context in display but leave fp */
        jbow = proc_i_out(pp);  /* beginning of parmouts */
        kbow = proc_i_back(pp);  /* first beyond parmouts */
        taut (kbow > jbow);  /* there is at least a RET parmout */
        move_to_args (jbow, kbow);  /* from WR/RW formals and RET */
        make_return;  /* jump back to caller */
        clear_call_ptrs;  /* xp, pp, fp */
        break;

      case op_callv:  /* normal call to variadic user-defined callee */
        taut (xp->nopnd >= 2);  /* callee + 0 or more args + RET */
        taut (fp->combiter == NULL);
        taut (pp->op == op_vproc);  /* returning from variadic callee */
        pop_frame;  /* restore caller context in display but leave fp */
        nformal = proc_nformal(pp);  /* not counting RET */
        nactual = xp->nopnd - 2;  /* user-provided args (not RET) */
        taut (nformal >= 1);  /* callee declared a (*) formal */
        taut (nactual >= nformal - 1);  /* for 0 or more actuals */
        jbow = proc_i_out(pp);  /* beginning of parmouts */
        kbow = proc_i_back(pp);  /* first beyond parmouts */
        taut (kbow > jbow);  /* there is at least a RET parmout */
        if (kbow >= jbow+2 && pp->bow[kbow-2].parmout.parmnum == nformal) {
          /* last non-RET parmout is for a WR/RW (*) formal */
          move_to_args (jbow, kbow-2);  /* from fixed WR/RW formals */
          tup = (tuple *)formal_out(kbow-2);  /* the (*) formal */
          if (!is_tuple(tup)) {
            runerr("WR/RW (*) arg to return must be TUPLE, not %s",
                    TYPENAME(tup));
          }
          distribute_tup;  /* move tup elements to the optional args */
        } else {
          /* (*) formal not WR/RW => have only fixed formals to move out */
          move_to_args (jbow, kbow-1);  /* not including RET */
        }
        move_ret_out;  /* move RET out to the arg at nactual+1 */
        make_return;  /* jump back to caller */
        clear_call_ptrs;  /* xp, pp, fp */
        break;

      case op_vcall:  /* call with (*) to necessarily variadic callee */
        taut (xp->nopnd >= 3);  /* callee + 0 or more args + (*) arg + RET */
        taut (fp->combiter == NULL);
        taut (pp->op == op_vproc);  /* returning from variadic callee */
        pop_frame;  /* restore caller context in display but leave fp */
        nformal = proc_nformal(pp);  /* not counting RET */
        nactual = xp->nopnd - 2;  /* user-provided args (not RET) */
        taut (nformal >= 1);  /* callee declared a (*) formal */
        taut (nactual >= nformal);  /* caller supplied a (*) actual */
        jbow = proc_i_out(pp);  /* beginning of parmouts */
        kbow = proc_i_back(pp);  /* first beyond parmouts */
        taut (kbow > jbow);  /* there is at least a RET parmout */
        if (kbow >= jbow+2 && pp->bow[kbow-2].parmout.parmnum == nformal) {
          /* last non-RET parmout is for a WR/RW (*) formal */
          move_to_args (jbow, kbow-2);  /* from fixed WR/RW formals */
          tup = (tuple *)formal_out(kbow-2);  /* the (*) formal */
          if (!is_tuple(tup)) {
            runerr("WR/RW (*) arg to return must be TUPLE, not %s",
                    TYPENAME(tup));
          }
          v_distribute_tup;  /* move tup members to "extras" and opt args */
        } else {
          /* (*) formal not WR/RW => have only fixed formals to move out */
          move_to_args (jbow, kbow-1);  /* not including RET */
        }
        move_ret_out;  /* move RET out to the arg at nactual+1 */
        make_return;  /* jump back to caller */
        clear_call_ptrs;  /* xp, pp, fp */
        break;

      case op_call_assigning:  /* lhs binop:= rhs */
        taut (xp->nopnd == 3);  /* binop + lhs + rhs */
        taut (fp->combiter == NULL);
        see_binop;  /* make sure callee is a binop; init ibow,jbow,kbow */
        pop_frame;  /* restore caller context in display but leave fp */
        move_from_ret_to (arg(1));  /* move binop RET to the lhs arg */
        make_return;  /* jump back to caller */
        clear_call_ptrs;  /* xp, pp, fp */
        break;

      case op_call_bincomb:  /* x binop/ t */
      case op_call_uncomb:  /* binop/ t */
        taut (xp->nopnd == (xp->op == op_call_bincomb
          ? 4  /* binop + x + t + RET for bincomb */
          : 3  /* binop + t + RET for uncomb */
        ));
        jp = fp->combiter;  /* iterator over set, string, or tuple t */
        taut (is_iterator(jp));
        see_binop;  /* make sure callee is a binop; init ibow,jbow,kbow */
        move_from_ret_to (tp);  /* move RET as returned by binop to tp */
        up = NULL;
        if (step_iterator(&jp,&up)) {  /* up := copy of next elmt from t */
          fp->combiter = jp;  /* resync iterator ptr in callee frame */
          jp = NULL;  /* callee frame officially owns the iterator */
          /*
           *  Set things up like on proc entry, with args representing
           *  "accumulated" and "next from iterator"
           */
          /* clear all SETL vars in the frame like setup_frame() does:  */
          if (fp->nloc > 0) {
            memset (fp->locs, 0, fp->nloc * sizeof fp->locs[0]);  /* OM */
          }
          move_ptr_in (ibow, tp);  /* 1st formal is last binop RET */ 
          move_ptr_in (ibow+1, up);  /* 2nd formal is copied t elmt */
          repeat;  /* go back to top of binop, with formals now set up */
        } else {  /* iterator exhausted; finally we get to return */
          taut (jp == NULL);
          fp->combiter = NULL;
          pop_frame;  /* restore caller context in display but leave fp */
          /* let last binop RET be the result of the combining op */
          move_blockptr (arg(xp->nopnd-1), tp);  /* move RET to out arg */
          make_return;  /* jump back to caller */
        }
        clear_call_ptrs;  /* xp, pp, fp */
        break;

      case op_icall:  /* normal indirect call, callee variadicity unknown */
      case op_vicall:  /* indirect (*) call to necessarily variadic callee */
        taut (xp->nopnd >= (xp->op == op_icall
          ? 2  /* callee + 0 or more args + RET for icall */
          : 3  /* callee + 0 or more args + (*) arg + RET for vicall */
        ));
        taut (fp->combiter == NULL);
        taut ((xp->op == op_icall && (pp->op == op_proc ||
                                      pp->op == op_vproc))  ||
              (xp->op == op_vicall && pp->op == op_vproc));
        pop_frame;  /* restore caller context in display but leave fp */
        jbow = proc_i_out(pp);  /* beginning of parmouts */
        kbow = proc_i_back(pp);  /* first beyond parmouts */
        /* Only RD args on an indirect call, so only RET to copy out */
        taut (kbow == jbow + 1);  /* the parmout for RET */
        nactual = xp->nopnd - 2;  /* user-provided args (not RET) */
        move_ret_out;  /* move RET out to the arg at nactual+1 */
        make_return;  /* jump back to caller */
        clear_call_ptrs;  /* xp, pp, fp */
        break;

      case op_icall_assigning:  /* not yet used */

      case op_icall_bincomb:  /* not yet used */

      case op_icall_uncomb:  /* not yet used */

      default:
        unexpected (xp->op);

      } /* end switch (xp->op) under op_return */

      xp = NULL;
      break;

    case op_routine:
      taut (ip->nopnd == 2);
      pp = (proc *)tupelt(setl_vm->code, opnd(1).pc);
      taut (pp->op == op_proc ||
            pp->op == op_vproc);  /* per assemble() */
      ibow = proc_i_in(pp);  /* beginning of parmins */
      jbow = proc_i_out(pp);  /* first beyond parmins */
      nformal = proc_nformal(pp);  /* not counting RET */
      taut (nformal == jbow - ibow);  /* all formals are RD, per assemble() */
      let (var(2), (block *)new_routine(opnd(1).pc));
      break;

    case op_set_fl:
      taut (ip->nopnd == 3);
      let (var(3), (block *)set_fl((integer *)var(1),
                                   (integer *)var(2)));
      break;

    case op_set_fnl:
      taut (ip->nopnd == 4);
      let (var(4), (block *)set_fnl((integer *)var(1),
                                    (integer *)var(2),
                                    (integer *)var(3)));
      break;

    case op_step_counter:
      taut (ip->nopnd == 3);
      tp = var(1);
      up = NULL;
      if (step_counter((iterator **)(void *)&tp,
                       (integer **)(void *)&up)) {
        var(1) = tp;
        var(2) = up;
        setl_vm->pc = opnd(3).pc - 1;
      }
      tp = NULL;
      up = NULL;
      break;

    case op_step_iterator:
      taut (ip->nopnd == 3);
      tp = var(1);
      up = NULL;
      if (step_iterator((iterator **)(void *)&tp, &up)) {
        var(1) = tp;
        var(2) = up;
        setl_vm->pc = opnd(3).pc - 1;
      }
      tp = NULL;
      up = NULL;
      break;

    case op_step_itermmap:
      taut (ip->nopnd == 4);
      tp = var(1);
      up = NULL;
      vp = NULL;
      if (step_itermmap((iterator **)(void *)&tp, &up,
                        (set **)(void *)&vp)) {
        var(1) = tp;
        var(2) = up;
        var(3) = vp;
        setl_vm->pc = opnd(4).pc - 1;
      }
      tp = NULL;
      up = NULL;
      vp = NULL;
      break;

    case op_step_itersmap:
      taut (ip->nopnd == 4);
      tp = var(1);
      up = NULL;
      vp = NULL;
      if (step_itersmap((iterator **)(void *)&tp, &up, &vp)) {
        var(1) = tp;
        var(2) = up;
        var(3) = vp;
        setl_vm->pc = opnd(4).pc - 1;
      }
      tp = NULL;
      up = NULL;
      vp = NULL;
      break;

    case op_stop: {
      integer *s;
      taut (ip->nopnd == 1);
      s = (integer *)var(1);
      setl_vm->exit_status = get_int(s, "STOP operand");
      running = false;  /* exit the interpreter loop */
    } break;

    case op_succeed:
      taut (ip->nopnd == 0);
      /*
       *  Previous OK really was okay!  Pop off the context that would
       *  otherwise allow us to leap back and make it yield FALSE on
       *  the next FAIL.
       *
       *  I'm not sure if this has been stated elsewhere, but it seems
       *  clear that SUCCEED should remove just one level of saved
       *  :BACK variables.
       */
      runerr("SUCCEED (confirm last TRUE from OK) not implemented");
      break;

    case op_truncate:
      taut (ip->nopnd == 1);
      tp = var(1);
      tup_truncate((tuple **)(void *)&tp);
      var(1) = tp;
      tp = NULL;
      break;

    case op_tuple_fl:
      taut (ip->nopnd == 3);
      let (var(3), (block *)tup_fl((integer *)var(1),
                                   (integer *)var(2)));
      break;

    case op_tuple_fnl:
      taut (ip->nopnd == 4);
      let (var(4), (block *)tup_fnl((integer *)var(1),
                                    (integer *)var(2),
                                    (integer *)var(3)));
      break;

    case op_vproc:  /* should never be flowed into, per assemble() check */
      unexpected (ip->op);
      break;

    default:
      unexpected (ip->op);    /* no such opcode */

    } /* end switch (ip->op) */

    ip = NULL;

    setl_vm->pc++;

    if (lifo_arena_use) {  /* the instr's arena use was strictly LIFO */
      /*
       *  XXX This is nice for testing, as we want to know about any
       *  non-LIFO arena use, and works well with our current use of GMP.
       *  But it comes at the cost of making implementation-dependent
       *  assumptions about GMP's allocation patterns.
       *
       *  So for production, it is probably best to fall back to
       *  arena_end_alt(), as in the else clause.
       */
      arena_end();  /* end LIFO arena allocation scope */
    } else {
      arena_end_alt();  /* end non-leaking but maybe non-LIFO scope */
    }

  } /* end while (running) */

  retire(h20);
  retire(h19);
  retire(h18);
  retire(h17);
  retire(h16);
  retire(h15);
  retire(h14);
  retire(h13);
  retire(h12);
  retire(h11);
  retire(h10);
  retire(h9);
  retire(h8);
  retire(h7);
  retire(h6);
  retire(h5);
  retire(h4);
  retire(h3);
  retire(h2);
  retire(h1);

  retire(h_opt);
  retire(h_tup);

  retire(wh);
  retire(vh);
  retire(uh);
  retire(th);
  retire(jh);
  retire(fh);
  retire(ph);
  retire(xh);
  retire(ih);

} /* end execute */


long newat(void) {
  return ++atom_counter;
}

bool get_magic(void) {
  return setl_vm ? setl_vm->magic : true;
}

bool set_magic(bool x) {
  if (setl_vm) {
    bool r = setl_vm->magic;
    setl_vm->magic = x;
    return r;
  } else {
    return true;
  }
}

bool get_intslash(void) {
  return setl_vm && setl_vm->intslash;
}

bool set_intslash(bool x) {
  if (setl_vm) {
    bool r = setl_vm->intslash;
    setl_vm->intslash = x;
    return r;
  } else {
    return false;
  }
}

bool get_eof(void) {
  return setl_vm && setl_vm->eof;
}

bool set_eof(bool x) {
  if (setl_vm) {
    bool r = setl_vm->eof;
    setl_vm->eof = x;
    return r;
  } else {
    return false;
  }
}

int get_raw_status(void) {
  return setl_vm ? setl_vm->raw_status : no_status;
}

int set_raw_status(int new_status) {
  if (setl_vm) {
    int old_status = setl_vm->raw_status;
    setl_vm->raw_status = new_status;
    return old_status;
  } else {
    return no_status;
  }
}
