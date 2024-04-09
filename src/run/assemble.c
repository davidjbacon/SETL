/*  ===  The SETL assembler/linker/loader  =========================  */

/*  $Id: assemble.c,v 1.71 2024/04/08 18:57:22 setlorg Exp $  */

/*  Free software (c) dB - see file COPYING for license (GPL).  */

/*
 *  The 'assemble' routine here takes the external, textual form of
 *  SETL virtual machine code and returns a new interpreter context
 *  (virtual machine instance) ready to begin execution.
 *
 *  The textual format is best understood by applying the SETL
 *  Translator to some sample programs (e.g., "setl -c ...").  The
 *  full instruction set (with operands) is listed in "../opcodes".
 */

#include "setlrun.h"

#define rd 0x1
#define wr 0x2
#define rw (rd|wr)

static tuple *chopline(string *s);
static long str_to_long(const char *s);
static int str_to_int(const char *s);
static void numfields_check(tuple *fields, long expected);
static void varfields_check(tuple *fields, long expected);
static void nopnd_check(tuple *fields, opcode op);
static void prefix_checks(tuple *fields, opcode op);
static void prefix_check(tuple *fields, opcode op, long iopnd);
static void null_prefix_check(tuple *fields, long iopnd);
static void in_prefix_check(tuple *fields, long iopnd);
static void out_prefix_check(tuple *fields, long iopnd);
static void inout_prefix_check(tuple *fields, long iopnd);
static bool is_unprefixed_opnd(string *field);
static bool is_in_opnd(string *field);
static bool is_out_opnd(string *field);
static bool is_inout_opnd(string *field);
static int opnd_rw_bits(tuple *fields, long iopnd);
static const char *rw_prefix(int rw_bits);
static const char *get_unprefixed_opnd(tuple *fields, long iopnd);
static const char *get_in_opnd(tuple *fields, long iopnd);
static const char *get_out_opnd(tuple *fields, long iopnd);
static const char *get_inout_opnd(tuple *fields, long iopnd);
static string *get_u_opnd(tuple *fields, long iopnd);
static bool is_sysrot_opnd(tuple *fields, long iopnd, long *sysrot);
static void call_proc_match (tuple *actuals, tuple *formals);
static void call_vproc_match (tuple *actuals, tuple *formals);
static void vcall_vproc_match (tuple *actuals, tuple *formals);
static void arg_match (tuple *actuals, long iactual,
                       tuple *formals, long iformal);
static void opnd_is (tuple *fields, long iopnd, int rw_bits, bool call_op);
static void append_n (string **name, long n);

/*
 *  The first parameter, 'vcode', contains the code to be "assembled",
 *  and the second parameter, 'sources', contains the corresponding
 *  SETL source code.  These two pointers are also copied into the
 *  returned VM instance's symbol table for debugging and run-time
 *  error reporting:
 */
machine *assemble(tuple *vcode, tuple *sources) {

  HANDLE h_vcode = ref(vcode);
  HANDLE h_sources = ref(sources);

  tuple *procnames = NULL;      HANDLE h_procnames = ref(procnames);
  table *procnums = NULL;       HANDLE h_procnums = ref(procnums);
  table *procaddrs = NULL;      HANDLE h_procaddrs = ref(procaddrs);
  table *procsigs = NULL;       HANDLE h_procsigs = ref(procsigs);
  table *globals = NULL;        HANDLE h_globals = ref(globals);
  table *gbl_backtrax = NULL;   HANDLE h_gbl_backtrax = ref(gbl_backtrax);
  table *labels = NULL;         HANDLE h_labels = ref(labels);
  table *curlabels = NULL;      HANDLE h_curlabels = ref(curlabels);
  table *locals = NULL;         HANDLE h_locals = ref(locals);
  table *curlocals = NULL;      HANDLE h_curlocals = ref(curlocals);
  table *formals = NULL;        HANDLE h_formals = ref(formals);
  table *lcl_backtrax = NULL;   HANDLE h_lcl_backtrax = ref(lcl_backtrax);
  table *curlcl_backtrax = NULL; HANDLE h_curlcl_backtrax = ref(curlcl_backtrax);
  codeline *rawline = NULL;     HANDLE h_rawline = ref(rawline);
  tuple *fields = NULL;         HANDLE h_fields = ref(fields);
  string *first_field = NULL;   HANDLE h_first_field = ref(first_field);
  string *field = NULL;         HANDLE h_field = ref(field);
  string *label = NULL;         HANDLE h_label = ref(label);
  tuple *procsig = NULL;        HANDLE h_procsig = ref(procsig);
  key *opkey = NULL;            HANDLE h_opkey = ref(opkey);
  key *opndkey = NULL;          HANDLE h_opndkey = ref(opndkey);
  string *procname = NULL;      HANDLE h_procname = ref(procname);
  string *curprocname = NULL;   HANDLE h_curprocname = ref(curprocname);
  key *prockey = NULL;          HANDLE h_prockey = ref(prockey);
  string *scrstr = NULL;        HANDLE h_scrstr = ref(scrstr);
  key *scrkey = NULL;           HANDLE h_scrkey = ref(scrkey);
  small *scrint = NULL;         HANDLE h_scrint = ref(scrint);
  subnode *scrsub = NULL;       HANDLE h_scrsub = ref(scrsub);
  parm *scrparm = NULL;         HANDLE h_scrparm = ref(scrparm);
  opcode op, prev_op;
  long pc;
  long pc_end;
  long curprocpc;
  long lexical_level;
  long iv;
  char *gblid;
  bool last_line_was_label;

  tuple *code = NULL;           HANDLE h_code = ref(code);
  instr *inst = NULL;           HANDLE h_inst = ref(inst);
  proc *capo = NULL;            HANDLE h_capo = ref(capo);
  long iopnd,nopnd;  /* opnd indices are 'fields' indices less 1 */
  long ibow,nbow;
  bool call_op, s_call, i_call, v_call;
  long procnum;
  long iparmin,nparmin,iparmout,nparmout;
  long ibacktrax,nbacktrax;
  tuple *parmins = NULL;        HANDLE h_parmins = ref(parmins);
  tuple *parmouts = NULL;       HANDLE h_parmouts = ref(parmouts);
  tuple *backtrax = NULL;       HANDLE h_backtrax = ref(backtrax);
  tuple *backs = NULL;          HANDLE h_backs = ref(backs);

  tuple *global_vec = NULL;     HANDLE h_global_vec = ref(global_vec);
  long iglobal,nglobal;
  long slotnum;
  long sysrot;

  symtab *symbol_table = NULL;  HANDLE h_symbol_table = ref(symbol_table);

  frame *fp = NULL;             HANDLE h_fp = ref(fp);

  long starting_pc;
  machine *vm = NULL;           HANDLE h_vm = ref(vm);

  procnames = null_tuple();
  procnums = empty_table();
  procaddrs = empty_table();
  procsigs = empty_table();
  /* init the 'globals' map (global identifier -> frame slot#)
   * with the predefined entities in sysdats:  */
  globals = copy_table(sysdats);
  locals = empty_table();
  labels = empty_table();
  gbl_backtrax = empty_table();
  lcl_backtrax = empty_table();

  /*
   *  First pass.  Build the above tuple and tables from labels and
   *  proc/vproc/mainproc, global/local, and backtrack directives.
   */
  pc = 0;  /* as in program counter */
  procnum = 0;  /* out of bounds, here for definite init */
  /* This init not only avoids a germane compiler warning, but allows
   * a certain check on the calling context of a v-call to remain
   * non-embarrassing even in the face of rogue translator output:  */
  curprocpc = 1;
  lexical_level = 0;
  last_line_was_label = false;
  prev_op = op_end;  /* effective initial state */

  for (iv=1; iv<=vcode->nelt; iv++) {

    rawline = (codeline *)tupelt(vcode,iv);
    fields = chopline(rawline->text);
    first_field = (string *)tupelt(fields,1);

    if (last_char(first_field) == ':') {  /* label */

      /* remove trailing colon */
      label = copy_substring(first_field,1,first_field->nchar-1);
      scrkey = string_to_key(label);
      if (insert_num (curlabels, scrkey, pc+1)) {
        tranerr("Repeated label '%s'", tame(&strelt(label,1)));
      }
      scrkey = NULL;
      label = NULL;
      last_line_was_label = true;

    } else {  /* not label */

      opkey = string_to_key(first_field);
      if (!has_key (optab, opkey)) {
        tranerr("Unrecognized opcode '%s'", tame(&strelt(first_field,1)));
      }
      op = (opcode) lookup_num (optab, opkey);
      switch (op) {

      case op_mainproc:
      case op_proc:
      case op_vproc:

        if (last_line_was_label) {
          tranerr("Procs must not be labelled");
        }

        if (op == op_mainproc) {
          numfields_check(fields, 2);
        } else {
          varfields_check(fields, op == op_vproc ? 4 : 3);
        }

        curprocname = copy_string( get_u_opnd(fields, 1) );
        prockey = string_to_key(curprocname);

        if (++lexical_level != 1) {
          tranerr("Proc nesting not yet supported");
        }

        /*
         *  Comments in the translator advertise that redeclarations
         *  of globals are normal, and supposed to be tolerated here.
         *  This policy is extended to routines on the premise that
         *  someday code might get concatenated into "object libraries",
         *  and to locals for consistency of policy.
         *
         *  Speaking of the future, if SETL ever gains typed formals
         *  and thus function overloading, the proc key will be based
         *  on the whole signature (as in 'fields'), not just on the
         *  proc name.
         */

        if (has_key (procnums, prockey)) {
          procnum = lookup_num (procnums, prockey);
        } else {  /* new procname */
          procnum = patsiz(procnums) + 1;
          insert_num (procnums, prockey, procnum);
          assert ((long)patsiz(procnums) == procnum);
          tup_tackon (&procnames,(block *)curprocname);
          assert (procnames->nelt == procnum);
        }

        insert_num (procaddrs, prockey, pc+1);  /* add or replace */

        insert_tuple (procsigs, prockey, fields);

        curlabels = empty_table();  /* map local label -> code index */
        insert_subtable (labels, prockey, curlabels);

        curlocals = empty_table();  /* map local identifier -> frame slot# */
        insert_num (curlocals, string_to_key(new_string("RET")), 1);
        insert_subtable (locals, prockey, curlocals);

        curlcl_backtrax = empty_table();  /* backtrackable local identifiers */
        insert_subtable (lcl_backtrax, prockey, curlcl_backtrax);

        prockey = NULL;

        /*
         *  At this point, the following are all true, and are logically
         *  equivalent ways of saying we are now in the context of a
         *  proc, also known as a "local" context:
         *
         *   lexical_level > 0
         *
         *   curprocname, curlabels, curlocals, or curlcl_backtrax is
         *   non-NULL
         *
         *  Conversely, if any of those were false, they would all be,
         *  meaning we were outside of a proc context.
         */

        pc++;
        break;

      case op_global:
        numfields_check(fields,2);
        field = get_u_opnd(fields,1);  /* global identifier */
        scrkey = string_to_key(field);
        cond_insert_num (globals, scrkey, patsiz(globals) + 1);
        scrkey = NULL;
        field = NULL;
        break;

      case op_local:
        numfields_check(fields,2);
        field = get_u_opnd(fields,1);  /* local identifier */
        scrkey = string_to_key(field);
        if (!curlocals) {
          tranerr("Cannot declare a local outside of a proc");
        }
        cond_insert_num (curlocals, scrkey, patsiz(curlocals) + 1);
        scrkey = NULL;
        field = NULL;
        break;

      case op_backtrack:
        numfields_check(fields,2);
        field = get_u_opnd(fields,1);  /* identifier */
        scrkey = string_to_key(field);
        if (curlocals) {  /* we are in a proc */
          if (!has_key (curlocals, scrkey)) {
            tranerr("Prematurely placed local 'backtrack' instruction");
          }
          insert_block (curlcl_backtrax, scrkey, NULL);
        } else {  /* we are at global scope */
          if (!has_key (globals, scrkey)) {
            tranerr("Prematurely placed global 'backtrack' instruction");
          }
          insert_block (gbl_backtrax, scrkey, NULL);
        }
        scrkey = NULL;
        field = NULL;
        break;

      case op_end:
        if (last_line_was_label) {
          tranerr("Label not allowed on 'end' instruction");
        }
        numfields_check(fields,2);
        field = get_u_opnd(fields,1);
        if (--lexical_level != 0 || !equal_string(field, curprocname)) {
          tranerr("Misplaced 'end %s'", tame(&strelt(field,1)));
        }
        field = NULL;
        switch (prev_op) {
        case op_return:
        case op_stop:
        case op_jmp:
        case op_fail:
          break;  /* good */
        default:
          tranerr("Control must not flow into 'end'");
        }
        curlcl_backtrax = NULL;
        curlocals = NULL;
        curlabels = NULL;
        curprocname = NULL;
        pc++;
        break;

      default:  /* "executable" instruction */
        if (lexical_level == 0) {
          tranerr("Cannot have executable instruction at global scope");
        }
        pc++;
        break;

      } /* end switch (op) */

      prev_op = op;
      last_line_was_label = false;
      opkey = NULL;

    } /* end if not label */

    first_field = NULL;
    fields = NULL;
    rawline = NULL;

  } /* end for (iv...) */  /* end of 1st pass */

  if (lexical_level != 0) {
    tranerr("Missing 'end' instruction");
  }
  pc_end = pc;

  /*
   *  Second pass.  Build the code tuple (what execute() will interpret).
   */
  code = null_tuple();

  for (iv=1; iv<=vcode->nelt; iv++) {

    rawline = (codeline *)tupelt(vcode,iv);
    fields = chopline(rawline->text);
    first_field = (string *)tupelt(fields,1);

    if (last_char(first_field) != ':') {  /* not label */

      nopnd = fields->nelt - 1;  /* opnd nums are field nums less 1 */

      opkey = string_to_key(first_field);
      op = (opcode) lookup_num(optab, opkey);
      switch (op) {

      case op_global:
      case op_local:
      case op_backtrack:
        /*
         *  Not insisting on any particular predecessor for these decls
         *  here, as they were already processed on the first pass, and
         *  don't become instrs anyway.
         */
        break;

      case op_mainproc:
      case op_proc:
      case op_vproc:
        /* number of fields was checked on first pass */
        curprocname = copy_string( get_u_opnd(fields, 1) );
        prockey = string_to_key(curprocname);
        procnum = lookup_num (procnums, prockey);
        curprocpc = lookup_num (procaddrs, prockey);
        curlabels = lookup_subtable (labels, prockey);
        curlocals = lookup_subtable (locals, prockey);
        curlcl_backtrax = lookup_subtable (lcl_backtrax, prockey);
        prockey = NULL;
        parmins = null_tuple();
        parmouts = null_tuple();
        backtrax = null_tuple();  /* local backtrackable variables */
        nbow = 0;  /* will be #parmins + #parmouts + #backtrax */

        formals = empty_table();  /* temp set of formal parm names */
        for (iopnd=2; iopnd<=nopnd; iopnd++) {
          int rw_bits = opnd_rw_bits(fields, iopnd);
          field = (string *)tupelt(fields, iopnd+1);
          scrstr = copy_substring(field, (rw_bits == rw) ? 3 : 2,
                                                    field->nchar);
          scrkey = string_to_key(scrstr);
          if (!has_key (curlocals, scrkey)) {
            tranerr("Undeclared parameter '%s'", tame(&strelt(scrstr,1)));
          }
          if (has_key (formals, scrkey)) {
            tranerr("Repeated parameter '%s'", tame(&strelt(scrstr,1)));
          }
          insert_block (formals, scrkey, NULL);
          scrparm = fblock(parm);
          scrparm->parmnum = iopnd - 1;  /* formal parm number */
          scrparm->slot = lookup_num (curlocals, scrkey);
          if (rw_bits & rd) {
            tup_tackon (&parmins, (block *)scrparm);
            nbow++;
          }
          if (rw_bits & wr) {
            tup_tackon (&parmouts, (block *)scrparm);
            nbow++;
          }
          scrparm = NULL;
          scrkey = NULL;
          scrstr = NULL;
          field = NULL;
        }
        formals = NULL;

        nbacktrax = patsiz(curlcl_backtrax);
        for (ibacktrax=1; ibacktrax<=nbacktrax; ibacktrax++) {
          check (ordsee(curlcl_backtrax,ibacktrax,&scrsub));
          scrkey = scrsub->k;  /* local identifier, as a key */
          /* its local slot# in 'curlocals' */
          tup_tackon (&backtrax, lookup_block (curlocals, scrkey));
          scrkey = NULL;
          scrsub = NULL;
        }
        nbow += nbacktrax;

        /*
         *  The 'nopnd' we store in a 'proc' instr is not simply
         *  fields->nelt - 1 (which is in the 'nopnd' local variable)
         *  as it is in other instrs:
         */
        capo = (proc *) alloc_block(instr_type, vbytes(proc,nbow));
        capo->op = op;
        capo->nopnd = proc_n_fixed_opnds + nbow;  /* NOT local var nopnd */
        capo->vindex = iv;
        capo->procnum.longval = procnum;
        capo->level.longval = LOCAL_LEVEL;  /* pending nesting support */
        /* capo->nloc is filled in after temps go into curlocals */
        capo->nformal.longval = nopnd - 2;  /* all but RET */
        capo->i_in.longval = 0;
        capo->i_out.longval = capo->i_in.longval + parmins->nelt;
        capo->i_back.longval = capo->i_out.longval + parmouts->nelt;
        ibow = 0;
        /* end of the backtrax is nbow, which can be reckoned at the
         * point of call as  capo->nopnd - proc_n_fixed_opnds  */

        nparmin = parmins->nelt;
        for (iparmin=1; iparmin<=nparmin; iparmin++) {
          parm *p = (parm *)tupelt(parmins, iparmin);
          operand o;
          o.parmin.parmnum = p->parmnum;
          o.parmin.slot = p->slot;
          capo->bow[ibow++] = o;
        }

        nparmout = parmouts->nelt;
        for (iparmout=1; iparmout<=nparmout; iparmout++) {
          parm *p = (parm *)tupelt(parmouts, iparmout);
          operand o;
          o.parmout.parmnum = p->parmnum;
          o.parmout.slot = p->slot;
          capo->bow[ibow++] = o;
        }

        nbacktrax = backtrax->nelt;
        for (ibacktrax=1; ibacktrax<=nbacktrax; ibacktrax++) {
          small *p = (small *)tupelt(backtrax, ibacktrax);
          operand o;
          o.data.level = capo->level.longval;
          o.data.slot = p->weeval;
          capo->bow[ibow++] = o;
        }

        assert (ibow == nbow);
        assert (ibow == nparmin + nparmout + nbacktrax);

        tup_tackon (&code,(block *)capo);
        assert (code->nelt == curprocpc);
        capo = NULL;
        backtrax = NULL;
        parmouts = NULL;
        parmins = NULL;
        break;

      default:

        nopnd_check(fields,op);

        if (op == op_scall &&
            lpfx(&strelt((string *)tupelt(fields, 2), 1), "S_FROM")) {
          /*
           *  The FROM* ops are special in having a signature that does
           *  not end in a > arg.  They take > and <>.
           */
          out_prefix_check(fields,2);
          inout_prefix_check(fields,3);
        } else {
          /*
           *  Check operand prefixes the usual way.
           */
          prefix_checks(fields,op);
        }

        call_op = true;
        switch (op) {
        case op_call:
          s_call = false;  /* system */
          i_call = false;  /* indirect */
          v_call = false;  /* variadic */
          break;
        case op_scall:
          s_call = true;
          i_call = false;
          v_call = false;
          break;
        case op_vcall:
          s_call = false;
          i_call = false;
          v_call = true;
          break;
        case op_vscall:
          s_call = true;
          i_call = false;
          v_call = true;
          break;
        case op_icall:
          s_call = false;
          i_call = true;
          v_call = false;
          break;
        case op_vicall:
          s_call = false;
          i_call = true;
          v_call = true;
          break;
        default:
          call_op = false;
          break;
        }

        if (call_op) {

          /* Check that the translator hasn't tried to get us to
           * make a (*) call from a non-variadic caller.  */
          if (v_call) {  /* trailing (*) on call */
            capo = (proc *)tupelt(code, curprocpc);  /* caller */
            if (capo->op != op_vproc) {
              tranerr("Only a vproc can make a v-call");
            }
            capo = NULL;
          }

          if (!i_call) {  /* direct call */

            const char *callee_opcode;
            bool v_proc;
            procname = copy_string( (string *)tupelt(fields,2) );
            prockey = string_to_key(procname);

            if (!s_call) {  /* call to user-defined routine */

              if (!has_key(procnums, prockey)) {
                linkerr("Proc '%s' not found for call",
                         tame(&strelt(procname,1)));
              }
              procsig = lookup_tuple (procsigs, prockey);  /* its fields */
              opnd_is (procsig, 1, 0, false);  /* no pfx on proc name */
              scrstr = (string *)tupelt(procsig,1);
              callee_opcode = &strelt(scrstr,1);
              if (leq(callee_opcode,"proc")) {
                v_proc = false;
              } else if (leq(callee_opcode,"vproc")) {
                v_proc = true;
                if (!v_call) {  /* no trailing (*) on call */
                  op = op_callv;  /* call to variadic user-def'd callee */
                }
              } else {
                unexpected_str(callee_opcode);
              }

            } else {  /* call to built-in routine */

              if (!has_key(sysrots, prockey)) {
                /* try with _n according to #fields */
                const long procnamelen = procname->nchar;  /* w/o suffix */
                const long n = fields->nelt - 3;
                append_n (&procname, n);  /* add _n */
                prockey = string_to_key(procname);
                if (!has_key(sysrots, prockey)) {
                  str_resize(&procname, procnamelen);  /* strip suffix */
                  linkerr("Built-in '%s' not found",
                           tame(&strelt(procname,1)));
                }
              }
              procsig = lookup_tuple (sysrotsigs, prockey);
              scrstr = (string *)tupelt(procsig,1);
              callee_opcode = &strelt(scrstr,1);
              /* These opcodes are a convention used by oprotsigs.awk
               * and sysrotsigs.awk for built-in ("system") routines;
               * they do not exist as SETL VM opcodes: */
              if (leq(callee_opcode,"sproc")) {
                v_proc = false;
              } else if (leq(callee_opcode,"vsproc")) {
                v_proc = true;
                if (!v_call) {  /* no trailing (*) on call */
                  op = op_scallv;  /* call to variadic system routine */
                }
              } else {
                unexpected_str(callee_opcode);
              }

            } /* end if scall */

            if (v_call && !v_proc) {
              tranerr("V-call of non-variadic '%s'",
                       tame(&strelt(procname,1)));
            }

            if (!v_proc) {  /* non-variadic */
              assert (!v_call);  /* by above logic */
              call_proc_match (fields, procsig);
            } else {
              if (!v_call) {  /* plain call to variadic callee */
                call_vproc_match (fields, procsig);
              } else {  /* v-call to variadic callee */
                vcall_vproc_match (fields, procsig);
              }
            }

            scrstr = NULL;
            procsig = NULL;
            prockey = NULL;
            procname = NULL;

          } else {  /* indirect call */
            /* Don't know anything about callee.  All operands but last
             * must be "<"; last is ">".  */
            opnd_is (fields, 1, rd, false);  /* <routine (1st opnd) */
            for (iopnd=2; iopnd<nopnd; iopnd++) {
              opnd_is (fields, iopnd, rd, true);  /* <arg */
            }
            opnd_is (fields, nopnd, wr, true);  /* >result */
          }

        } /* if call_op */

        inst = vblock(instr,nopnd);
        inst->op = op;  /* opcode is encoded in op */
        inst->nopnd = nopnd;  /* number of operands */
        inst->vindex = iv;

        for (iopnd=1; iopnd<=nopnd; iopnd++) {

          /* Make 'field' a copy of fields(iopnd+1) without the r/w
           * prefix(es), and make sure that doesn't leave it empty.  */
          field = copy_string( (string *)tupelt(fields,iopnd+1) );
          while (prefix[(uchar)strelt(field,1)]) {
            field->nbefore++;
            field->nchar--;
            if (field->nchar <= 0) {
              const char *opcode_name = &strelt(first_field,1);
              tranerr("Operand %ld of '%s' instruction is content-free!",
                               iopnd, opcode_name);
            }
          }
          opndkey = string_to_key(field);  /* 'field' as a key */

#         define misplaced_operand(x) do { \
            const char *opcode_name = &strelt(first_field,1); \
            tranerr("%s operand %ld (%s) of '%s' instruction", \
                      x, iopnd, tame(&strelt(field,1)), opcode_name); \
          } while (0)

          /*
           *  "Immediate" operand, given as an unadorned decimal string.
           *  No instrs in the current opcodes file use this form of
           *  operand anymore, but could again in the future.
           */
          if (digital(&strelt(field,1))) {
            opnd_is (fields, iopnd, 0, call_op);  /* unprefixed opnd */
            switch (find_opndtype(inst,iopnd)) {
            case int_opnd:
              instrelt(inst,iopnd).intval = str_to_int(&strelt(field,1));
              break;
            case long_opnd:
              instrelt(inst,iopnd).longval = str_to_long(&strelt(field,1));
              break;
            default:
              misplaced_operand("Numeric");
            }

          } else if (leqn(&strelt(field,1),"T_",2) ||
                     leq(&strelt(field,1),"RET")) {
            if (find_opndtype(inst,iopnd) != data_opnd) {
              misplaced_operand("Data");
            }
            slotnum = patsiz(curlocals) + 1;
            insert_or_get_num (curlocals, opndkey, &slotnum);
            instrelt(inst,iopnd).data.level = LOCAL_LEVEL;
            instrelt(inst,iopnd).data.slot  = slotnum;
            insert_block (curlcl_backtrax, opndkey, NULL);

          } else if (leqn(&strelt(field,1),"U_",2)) {
            if (see_num (curlocals, opndkey, &slotnum)) {
              if (find_opndtype(inst,iopnd) != data_opnd) {
                misplaced_operand("User data");
              }
              instrelt(inst,iopnd).data.level = LOCAL_LEVEL;
              instrelt(inst,iopnd).data.slot  = slotnum;
            } else if (see_num (globals, opndkey, &slotnum)) {
              if (find_opndtype(inst,iopnd) != data_opnd) {
                misplaced_operand("User data");
              }
              instrelt(inst,iopnd).data.level = GLOBAL_LEVEL;
              instrelt(inst,iopnd).data.slot  = slotnum;
            } else if (see_num (procaddrs, opndkey, &pc)) {
              opnd_is (fields, iopnd, 0, call_op);  /* unprefixed opnd */
              if (find_opndtype(inst,iopnd) != pc_opnd) {
                misplaced_operand("User proc");
              }
              instrelt(inst,iopnd).pc = pc;
            } else {
              tranerr("Undeclared name '%s'", tame(&strelt(field,1)));
            }

          } else if (leqn(&strelt(field,1),"S_",2)) {
            if (see_num (globals, opndkey, &slotnum)) {
              if (find_opndtype(inst,iopnd) != data_opnd) {
                misplaced_operand("System data");
              }
              instrelt(inst,iopnd).data.level = GLOBAL_LEVEL;
              instrelt(inst,iopnd).data.slot  = slotnum;
            } else if (is_sysrot_opnd(fields, iopnd, &sysrot)) {
              opnd_is (fields, iopnd, 0, call_op);  /* unprefixed opnd */
              if (find_opndtype(inst,iopnd) != sysrot_opnd) {
                misplaced_operand("System routine");
              }
              instrelt(inst,iopnd).sysrot = sysrot;
            } else {
              tranerr("Unknown name '%s'", tame(&strelt(field,1)));
            }

          } else if (see_num (sysdats, opndkey, &slotnum)) {
            if (find_opndtype(inst,iopnd) != data_opnd) {
              misplaced_operand("System data");
            }
            instrelt(inst,iopnd).data.level = GLOBAL_LEVEL;
            instrelt(inst,iopnd).data.slot  = slotnum;

          } else if (leqn(&strelt(field,1),"C_",2)  ||
                     leqn(&strelt(field,1),"L_",2)  ||
                     leqn(&strelt(field,1),"RL_",3) ||
                     leqn(&strelt(field,1),"RR_",3)) {
            opnd_is (fields, iopnd, 0, call_op);  /* unprefixed opnd */
            if (find_opndtype(inst,iopnd) != pc_opnd) {
              misplaced_operand("Label");
            }
            if (!see_num (curlabels, opndkey, &pc)) {
              tranerr("Unknown label '%s'", tame(&strelt(field,1)));
            }
            instrelt(inst,iopnd).pc  = pc;

          } else if (leqn(&strelt(field,1),"I_",2)  ||
                     leqn(&strelt(field,1),"R_",2)  ||
                     strelt(field,1) == '\''    ||
                     strelt(field,1) == '\"') {
            if (op == op_cplusplus_code ||
                op == op_cplusplus_include) {
              /* the prettified "C++ code" arg looks like a
               * SETL string literal, but with no "<" prefix  */
              if (strelt(field,1) == '\''    ||
                  strelt(field,1) == '\"') {
                opnd_is (fields, iopnd, 0, false);  /* unprefixed opnd */
              } else {
                tranerr("Operand of op_cplusplus_{code,include} must"
                        " look like a SETL string literal");
              }
            } else {
              opnd_is (fields, iopnd, rd, call_op);  /* <literal */
              if (find_opndtype(inst,iopnd) != data_opnd) {
                misplaced_operand("Literal");
              }
            }
            slotnum = patsiz(globals) + 1;
            insert_or_get_num (globals, opndkey, &slotnum);
            instrelt(inst,iopnd).data.level = GLOBAL_LEVEL;
            instrelt(inst,iopnd).data.slot = slotnum;

          } else {
            misplaced_operand("Unrecognized");
          }

          field = NULL;

#         undef misplaced_operand

        } /* end for */

        if (op == op_routine) {
          long procsig_nopnd;
          procname = copy_string( (string *)tupelt(fields,2) );
          prockey = string_to_key(procname);
          if (!has_key(procnums, prockey)) {
            linkerr("Routine '%s' not found",
                     tame(&strelt(procname,1)));
          }
          procsig = lookup_tuple (procsigs, prockey);  /* its fields */
          procsig_nopnd = procsig->nelt - 1;
          opnd_is (procsig, 1, 0, false);  /* no pfx on proc name */
          for (iopnd=2; iopnd < procsig_nopnd; iopnd++) {
            if (opnd_rw_bits (procsig, iopnd) != rd) {
              tranerr("Proc '%s' must have only RD formals before RET",
                       tame(&strelt(procname,1)));
            }
          }
          if (opnd_rw_bits (procsig, procsig_nopnd) != wr) {
            tranerr("Proc '%s' must have a final >RET formal",
                     tame(&strelt(procname,1)));
          }
          procsig = NULL;
          prockey = NULL;
          procname = NULL;
        }

        if (op == op_end) {
          capo = (proc *)tupelt(code, curprocpc);
          capo->nloc.longval = patsiz(curlocals);  /* including T_* */
          capo = NULL;
          curlcl_backtrax = NULL;
          curlocals = NULL;
          curlabels = NULL;
          curprocname = NULL;
        }

        tup_tackon (&code,(block *)inst);
        inst = NULL;

        break;

      } /* end switch (op) */

      opkey = NULL;

    } /* end if not label */

    first_field = NULL;
    fields = NULL;
    rawline = NULL;

  } /* end for (iv...) */  /* end of 2nd pass */

  assert (code->nelt == pc_end);

  assert (patsiz(procnums) == (ulong)procnames->nelt);

  /*
   *  Make a tuple 'backs' from the global slot numbers of the
   *  identifiers in gbl_backtrax.
   *
   *  Since the blind backtracking "feature" is probably DOA, it
   *  doesn't really matter, but on a technical point this 'backs'
   *  tuple for globals is a bit retro and now clashes with the
   *  style of the "backtracks" part of the 'bow' array in the
   *  proc struct, which reflects local :BACK vars.  The equiv of
   *  that array used to be a tuple.  Both the global tuple and
   *  proc-local array are populated according to the op_backtrack
   *  instrs that ensue from the :BACK decorations on VAR decls at
   *  global and routine-local levels respectively, and neither is
   *  used for anything at run-time currently.
   */
  backs = null_tuple();
  nbacktrax = patsiz(gbl_backtrax);
  for (ibacktrax=1; ibacktrax<=nbacktrax; ibacktrax++) {
    check (ordsee(gbl_backtrax,ibacktrax,&scrsub));
    scrkey = scrsub->k;  /* global identifier, as key */
    tup_tackon (&backs, lookup_block (globals, scrkey));  /* slot num */
    scrkey = NULL;
    scrsub = NULL;
  }

  /*
   *  Create global_vec with what will be the initial contents of the
   *  stack frame that represents the global scope.
   */
  nglobal = patsiz(globals);
  global_vec = new_tuple(nglobal);

  for (iglobal=1; iglobal<=nglobal; iglobal++) {

    check (ordsee(globals,iglobal,&scrsub));
    scrkey = scrsub->k;  /* global identifier, as key */
    slotnum = lookup_num (globals, scrkey);
    assert (1 <= slotnum && slotnum <= global_vec->nelt);
    assert (slotnum == ((small *)scrsub->d)->weeval);
    scrstr = (string *)unkey(scrkey);
    gblid = &strelt(scrstr,1);  /* transient w.r.t. heap allocation! */

    if (leqn(gblid,"I_",2)) {

      char *begin = &gblid[2];  /* where the number itself begins */
      char *sharp = strchr(begin,'#');  /* end of radix indicator */
      integer *t = NULL;
      char first_nonblank;

      /* skip over any leading spaces and + sign */
      while (*begin == ' ') ++begin;
      first_nonblank = *begin;
      if (*begin == '+') ++begin;

      if (sharp == NULL) {  /* no radix indicator, radix is 10 */
        size_t i = first_nonblank == '-' ? 1 : 0;
        while (isdigit((uchar)begin[i])) ++i;
        if (begin[i] != '\0') {
          tranerr("Bad integer literal in '%s'", tame(gblid));
        }
        t = charstr_to_integer(begin, 10);

      } else {  /* expect radix#digits or radix#digits# */

        char *digits;
        char *hend;
        size_t ndig_radix;
        int radix;
        size_t ndig, i;
        int saved_errno = errno;
        begin += first_nonblank == '-' ? 1 : 0;
        while (begin[0] == '0') ++begin;  /* skip leading 0's in radix */
        ndig_radix = sharp - begin;
        if (ndig_radix != 1 &&
            ndig_radix != 2) {
          tranerr("Radix indicator in '%s' has too many sig digs",
                                 tame(gblid));
        }
        if (!isdigit((uchar)begin[0]) ||
            (ndig_radix == 2 && !isdigit((uchar)begin[1]))) {
          tranerr("Malformed radix indicator in '%s'",
                                           tame(gblid));
        }
        errno = 0;
        radix = strtol(begin, NULL, 10);
        assert (errno == 0);
        errno = saved_errno;
        if (radix < 2 || radix > 36) {
          tranerr("Radix indicator in '%s' is out of range 2..36",
                                 tame(gblid));
        }
        digits = sharp + 1;
        hend = strchr(digits,'#');
        if (hend != NULL) {
          if (hend[1] != '\0') {
            tranerr("Trailing junk after integer literal in '%s'",
                                                       tame(gblid));
          }
          ndig = hend - digits;
          hend[0] = '\0';  /* clobber trailing sharp in scrstr */
        } else {
          ndig = strlen(digits);
        }
        for (i=0; i<ndig; i++) {
          char c = digits[i];
          int v = setl2digval[(uchar)c];
          if (v < 0 || v >= radix) {
            tranerr("Character '%c' in '%s' is not in base %d",
             isprint((uchar)c) ? c : '?', tame(gblid), radix);
          }
        }
        if (first_nonblank == '-') {
          /* clobber first sharp with minus, and point 'digits' at it */
          *(--digits) = '-';
        }
        t = charstr_to_integer(digits, radix);

      } /* end if */

      tupelt(global_vec,slotnum) = (block *)t;

    } else if (leqn(gblid,"R_",2)) {

      char *begin = &gblid[2];
      char *end = NULL;
      double lit_val;
      int saved_errno = errno;
      errno = 0;
      lit_val = strtod(begin, &end);
      if (errno != 0 || *end != '\0') {
        /* I say "literal" here but "denotation" in the context of
         * getval() and unstr() (see sys.c).  We are here processing
         * what was a piece of SETL program text, but over there an
         * input token.  Perhaps a good convention to adopt everywhere,
         * arbitrary though it is?  */
        if (errno == ERANGE) {
          /* Co-opt linkerr() so that we give a reasonable message and
           * no abort() for this case where the literal was well enough
           * formed but was out of range for this interpreter.  */
          linkerr("Floating-point literal out of range in '%s'",
                                          tame(gblid));
        } else {  /* non-ERANGE error, or junk after a good literal */
          /* Trailing junk after a good literal will give the likes of
           * "No error" or "Success" in this error message.  Oh well:  */
          tranerr("Bad floating-point literal in '%s': %s",
                                          tame(gblid), os_strerror(errno));
        }
      }
      errno = saved_errno;
      let (tupelt(global_vec,slotnum), (block *)new_real(lit_val));

    } else if (gblid[0] == '\'' || gblid[0] == '\"') {
      let (tupelt(global_vec,slotnum), (block *)str_undress(scrstr,tranerr));

    } else if (leq(gblid,"{}")) {
      let (tupelt(global_vec,slotnum), (block *)null_set());

    } else if (leq(gblid,"[]")) {
      let (tupelt(global_vec,slotnum), (block *)null_tuple());

    } else if (leq(gblid,"S_CLOSE_AWAIT")) {
      let (tupelt(global_vec,slotnum), (block *)new_integer(close_await));

    } else if (leq(gblid,"S_CLOSE_AUTOREAP")) {
      let (tupelt(global_vec,slotnum), (block *)new_integer(close_autoreap));

    } else if (leq(gblid,"S_CLOSE_ZOMBIE")) {
      let (tupelt(global_vec,slotnum), (block *)new_integer(close_zombie));

    } else if (leq(gblid,"S_FALSE")) {
      let (tupelt(global_vec,slotnum), (block *)new_boolean(false));

    } else if (leq(gblid,"S_NO_ERROR")) {
      const char *msg = os_strerror(0);
      let (tupelt(global_vec,slotnum), (block *)new_string(msg));

    } else if (leq(gblid,"S_OM")) {
      tupelt(global_vec,slotnum) = OM;  /* NULL */

    } else if (leq(gblid,"S_SEEK_SET")) {
      let (tupelt(global_vec,slotnum), (block *)new_integer(SEEK_SET));

    } else if (leq(gblid,"S_SEEK_CUR")) {
      let (tupelt(global_vec,slotnum), (block *)new_integer(SEEK_CUR));

    } else if (leq(gblid,"S_SEEK_END")) {
      let (tupelt(global_vec,slotnum), (block *)new_integer(SEEK_END));

    } else if (leq(gblid,"S_SHUT_RD")) {
      let (tupelt(global_vec,slotnum), (block *)new_integer(SHUT_RD));

    } else if (leq(gblid,"S_SHUT_WR")) {
      let (tupelt(global_vec,slotnum), (block *)new_integer(SHUT_WR));

    } else if (leq(gblid,"S_SHUT_RDWR")) {
      let (tupelt(global_vec,slotnum), (block *)new_integer(SHUT_RDWR));

    } else if (leq(gblid,"S_STDERR")) {
      let (tupelt(global_vec,slotnum), (block *)new_integer(fd_stderr));

    } else if (leq(gblid,"S_STDIN")) {
      let (tupelt(global_vec,slotnum), (block *)new_integer(fd_stdin));

    } else if (leq(gblid,"S_STDOUT")) {
      let (tupelt(global_vec,slotnum), (block *)new_integer(fd_stdout));

    } else if (leq(gblid,"S_TRUE")) {
      let (tupelt(global_vec,slotnum), (block *)new_boolean(true));

    } else {
      tupelt(global_vec,slotnum) = OM;  /* NULL */
    }

    scrstr = NULL;
    scrkey = NULL;
    scrsub = NULL;

  } /* end for (iglobal...) */

  symbol_table = fblock(symtab);
  symbol_table->procnames = procnames;
  symbol_table->procnums = procnums;
  symbol_table->procaddrs = procaddrs;
  symbol_table->globals = globals;
  symbol_table->locals = locals;
  symbol_table->labels = labels;
  symbol_table->gbl_backtrax = gbl_backtrax;
  symbol_table->lcl_backtrax = lcl_backtrax;
  symbol_table->vcode = vcode;  /* caller beware - pointer copy only! */
  symbol_table->sources = sources;  /* likewise */

  scrstr = new_string("U__MAIN");
  scrkey = string_to_key(scrstr);
  if (!see_num (procaddrs, scrkey, &starting_pc)) {
    tranerr("No 'mainproc' found");
  }
  scrkey = NULL;
  scrstr = NULL;
  fp = vblock(frame,nglobal);  /* a starting frame */
  fp->nloc         = nglobal;
  fp->proc_pc      = starting_pc;
  fp->caller_pc    = -1;
  fp->caller_level = NO_LEVEL;
  fp->nargs        = 0;
  fp->link         = NULL;
  fp->combiter     = NULL;
  for (iglobal=1; iglobal<=nglobal; iglobal++) {
    framelt(fp,iglobal) = tupelt(global_vec,iglobal);
  }
  global_vec = NULL;
  vm = fblock(machine);
  vm->pc = starting_pc;
  vm->level = NO_LEVEL;
  vm->exit_status = 0;
  vm->raw_status = no_status;
  vm->eof = false;
  vm->magic = true;
  vm->intslash = false;
  vm->code = code;
  vm->backs = backs;
  vm->sym = symbol_table;
  vm->ready_maps = NULL;
  vm->display[GLOBAL_LEVEL] = fp;
  vm->display[LOCAL_LEVEL] = NULL;

  retire(h_vm);
  retire(h_fp);
  retire(h_symbol_table);
  retire(h_global_vec);
  retire(h_backs);
  retire(h_backtrax);
  retire(h_parmouts);
  retire(h_parmins);
  retire(h_capo);
  retire(h_inst);
  retire(h_code);
  retire(h_scrparm);
  retire(h_scrsub);
  retire(h_scrint);
  retire(h_scrkey);
  retire(h_scrstr);
  retire(h_prockey);
  retire(h_curprocname);
  retire(h_procname);
  retire(h_opndkey);
  retire(h_opkey);
  retire(h_procsig);
  retire(h_label);
  retire(h_field);
  retire(h_first_field);
  retire(h_fields);
  retire(h_rawline);
  retire(h_curlcl_backtrax);
  retire(h_lcl_backtrax);
  retire(h_formals);
  retire(h_curlocals);
  retire(h_locals);
  retire(h_curlabels);
  retire(h_labels);
  retire(h_gbl_backtrax);
  retire(h_globals);
  retire(h_procsigs);
  retire(h_procaddrs);
  retire(h_procnums);
  retire(h_procnames);

  retire(h_sources);
  retire(h_vcode);

  return vm;

} /* end assemble */


static tuple *chopline(string *s) {
  HANDLE hs = ref(s);
  long i, j, n;
  tuple *fields = null_tuple();  HANDLE hf = ref(fields);
  char *p;
  string *t;
  n = s->nchar;
  for (i=0; i<n; i=j) {
    p = &strelt(s,1);  /* keep re-establishing in case of g.c. */
    for (   ; i<n &&  separator[(uchar)(p[i])]; i++) ;  /* span(sep) */
    for (j=i; j<n && !separator[(uchar)(p[j])]; j++) ;  /* break(sep) */
    t = new_estring(j-i);
    mvmem (&strelt(t,1), &strelt(s,i+1), j-i);
    tup_tackon (&fields, (block *)t);
  }
  retire(hf);
  retire(hs);
  return fields;
}

/*
 *  Convert from decimal string (with optional sign) to long int,
 *  allowing leading and trailing whitespace but no other junk.
 */
static long str_to_long(const char *s) {
  size_t i, j, k;
  i = 0; while (isspace((uchar)s[i])) i++;
  j = i; if (s[j]=='+' || s[j]=='-') j++;
  if (isdigit((uchar)s[j])) {
    while (isdigit((uchar)s[j])) j++;
    k = j; while (isspace((uchar)s[k])) k++;
    if (k == strlen(s)) {
      long r;
      int saved_errno = errno;
      errno = 0;
      r = strtol(&s[i],NULL,10);
      if (errno == ERANGE) {
        tranerr("Value of '%s' too wide for C long int", tame(s));
      }
      assert (errno == 0);  /* EINVAL ruled out by above checks */
      errno = saved_errno;
      return r;
    }
    tranerr("Junk after digits in '%s'", tame(s));
  }
  tranerr("Malformed integer '%s'", tame(s));
}

static int str_to_int(const char *s) {
  long r = str_to_long(s);
  if (INT_MIN <= r && r <= INT_MAX) {
    return r;
  } else {
    tranerr("Value of '%s' too wide for C int", tame(s));
  }
}

static void numfields_check(tuple *fields, long expected) {
  long numfields = fields->nelt;
  if (numfields != expected) {
    string *first_field = (string *)tupelt(fields,1);
    const char *opcode_name = &strelt(first_field,1);
    if (expected == 2) {
      tranerr("Expected 1 operand after '%s' opcode, not %ld",
                                         opcode_name,    numfields-1);
    } else {
      tranerr("Expected %ld operands after '%s' opcode, not %ld",
                        expected-1,      opcode_name,    numfields-1);
    }
  }
}

static void varfields_check(tuple *fields, long expected) {
  long numfields = fields->nelt;
  if (numfields < expected) {
    string *first_field = (string *)tupelt(fields,1);
    const char *opcode_name = &strelt(first_field,1);
    tranerr("Expected at least %ld operands after '%s' opcode, not %ld",
                      expected-1,         opcode_name,     numfields-1);
  }
}

static void nopnd_check(tuple *fields, opcode op) {
  switch (op) {
  /* This file is generated by nopnds.awk:  */
#include "nopnds.c"
  default:
    unexpected (op);
  }
}

static void prefix_checks(tuple *fields, opcode op) {
  long nopnd = fields->nelt - 1;
  long iopnd;
  for (iopnd=1; iopnd<=nopnd; iopnd++) {
    prefix_check(fields, op, iopnd);
  }
}

static void prefix_check(tuple *fields, opcode op, long iopnd) {
  long nopnd = fields->nelt - 1;
  switch (op) {
  /* This file is generated by opndpfxs.awk:  */
#include "opndpfxs.c"
  default:
    unexpected (op);
  }
}

static void null_prefix_check(tuple *fields, long iopnd) {
  (void) get_unprefixed_opnd(fields, iopnd);
}

static void in_prefix_check(tuple *fields, long iopnd) {
  (void) get_in_opnd(fields, iopnd);
}

static void out_prefix_check(tuple *fields, long iopnd) {
  (void) get_out_opnd(fields, iopnd);
}

static void inout_prefix_check(tuple *fields, long iopnd) {
  (void) get_inout_opnd(fields, iopnd);
}

static bool is_unprefixed_opnd(string *field) {
  const char *opnd;
  assert (is_string(field));
  opnd = &strelt(field,1);
  return opnd[0] != '<' && opnd[0] != '>' && opnd[0] != '\0';
}

static bool is_in_opnd(string *field) {
  const char *opnd;
  assert (is_string(field));
  opnd = &strelt(field,1);
  return strlen(opnd) > 1 && opnd[0] == '<' && opnd[1] != '>';
}

static bool is_out_opnd(string *field) {
  const char *opnd;
  assert (is_string(field));
  opnd = &strelt(field,1);
  return strlen(opnd) > 1 && opnd[0] == '>' /* && opnd[1] != '<' */;
}

static bool is_inout_opnd(string *field) {
  const char *opnd;
  assert (is_string(field));
  opnd = &strelt(field,1);
  return strlen(opnd) > 2 && opnd[0] == '<' && opnd[1] == '>';
}

static int opnd_rw_bits(tuple *fields, long iopnd) {
  string *field = (string *)tupelt(fields,iopnd+1);
  const char *opnd;
  assert (is_string(field));
  opnd = &strelt(field,1);
  if (opnd[0] == '<') {
    if (opnd[1] == '>') {
      return rw;
    } else {
      return rd;
    }
  } else if (opnd[0] == '>') {
    return wr;
  } else {
    return 0;  /* no direction prefix (<, >, or <>) */
  }
}

static const char *rw_prefix(int rw_bits) {
  switch (rw_bits) {
  case 0:   return "";
  case rd:  return "<";
  case wr:  return ">";
  case rw:  return "<>";
  default:  unexpected (rw_bits);
  }
}

#define bad_operand(msg) do { \
  string *first_field = (string *)tupelt(fields,1); \
  const char *opcode_name = &strelt(first_field,1); \
  tranerr("Operand %ld (%s) of '%s' instruction %s", \
           iopnd, tame(&strelt(field,1)), opcode_name, msg); \
} while (0)

static const char *get_unprefixed_opnd(tuple *fields, long iopnd) {
  string *field = (string *)tupelt(fields,iopnd+1);
  assert (is_string(field));
  if (is_unprefixed_opnd(field)) {
    return &strelt(field,1);
  } else {
    bad_operand("must not have prefix ('<' etc.)");
  }
}

static const char *get_in_opnd(tuple *fields, long iopnd) {
  string *field = (string *)tupelt(fields,iopnd+1);
  assert (is_string(field));
  if (is_in_opnd(field)) {
    return &strelt(field,2);
  } else {
    bad_operand("is not in-only ('<')");
  }
}

static const char *get_out_opnd(tuple *fields, long iopnd) {
  string *field = (string *)tupelt(fields,iopnd+1);
  assert (is_string(field));
  if (is_out_opnd(field)) {
    return &strelt(field,2);
  } else {
    bad_operand("is not out-only ('>')");
  }
}

static const char *get_inout_opnd(tuple *fields, long iopnd) {
  string *field = (string *)tupelt(fields,iopnd+1);
  assert (is_string(field));
  if (is_inout_opnd(field)) {
    return &strelt(field,3);
  } else {
    bad_operand("is not in-out ('<>')");
  }
}

static string *get_u_opnd(tuple *fields, long iopnd) {
  string *field = (string *)tupelt(fields,iopnd+1);
  const char *opnd = &strelt(field,1);
  if (leqn(opnd,"U_",2)) {  /* opnd starts with U_ and is unprefixed */
    return field;
  } else {
    bad_operand("does not begin with 'U_'");
  }
}

static bool is_sysrot_opnd(tuple *fields, long iopnd, long *sysrot) {
  HANDLE hf = ref(fields);
  string *procname = copy_string((string *)tupelt(fields, iopnd+1));
  HANDLE hp = ref(procname);
  key *k = string_to_key(procname);
  HANDLE hk = ref(k);
  bool ret = false;
  if (see_num (sysrots, k, sysrot)) {
    ret = true;
  } else {
    /* try with _n according to #fields */
    const long n = fields->nelt - 3;
    append_n (&procname, n);  /* add _n */
    k = string_to_key(procname);
    ret = see_num (sysrots, k, sysrot);
  }
  retire(hk);
  retire(hp);
  retire(hf);
  return ret;
}

/*
 *  In the following *call-*proc agreement checkers, the conventions
 *  are that element 1 of the tuples is an op code in string form,
 *  element 2 is the routine or callee name, and elements 3 on are the
 *  args.  The last actual arg has ">", to catch the function result,
 *  so the routine's last formal parameter is WR.  If the routine is
 *  variadic, the variable-sized tuple is the second-last formal.
 *
 *  Note that in the tranerr() messages, the cited arg and parm numbers
 *  are 2 less than for the field tuples in 'actuals' and 'formals',
 *  thus reflecting the SETL user point of view of calls and procs
 *  (+1 if the message is about the function result).
 */

/* Straight call to non-variadic routine.  Exact R/W match.  */
static void call_proc_match (tuple *actuals, tuple *formals) {
  HANDLE ha = ref(actuals);
  HANDLE hf = ref(formals);
  long nactual, nformal;
  long iactual;
  string *procname = copy_string((string *)tupelt(actuals,2));
  if (!equal_string(procname, (string *)tupelt(formals,2))) {
    /* procname + _n according to #actuals must succeed */
    const long procnamelen = procname->nchar;  /* w/o suffix */
    const long n = actuals->nelt - 3;
    append_n (&procname, n);  /* add _n */
    assert (equal_string(procname, (string *)tupelt(formals,2)));
    str_resize(&procname, procnamelen);  /* lest needed by tranerr() */
  }
  nactual = actuals->nelt;
  nformal = formals->nelt;
  if (nactual != nformal) {
    tranerr("Call of %ld-ary %s with %ld args",
             nformal-2, tame(&strelt(procname,1)), nactual-2);
  }
  for (iactual=3; iactual<=nactual; iactual++) {
    arg_match (actuals, iactual, formals, iactual);
  }
  retire(hf);
  retire(ha);
}

/* Straight call to variadic routine.  */
static void call_vproc_match (tuple *actuals, tuple *formals) {
  long nactual, nformal;
  long iactual;
  string *procname = (string *)tupelt(actuals,2);
  assert (equal_string(procname, (string *)tupelt(formals,2)));
  nactual = actuals->nelt;
  nformal = formals->nelt;
  if (nactual < nformal-1) {
    tranerr("Call of variadic %ld-ary %s with only %ld args",
             nformal-2, tame(&strelt(procname,1)), nactual-2);
  }
  for (iactual=3; iactual<=nformal-2/*sic*/; iactual++) {
    arg_match (actuals, iactual, formals, iactual);
  }
  for (; iactual<=nactual-1; iactual++) {
    arg_match (actuals, iactual, formals, nformal-1);
  }
  arg_match (actuals, nactual, formals, nformal);
}

/* V-call to variadic routine.  */
static void vcall_vproc_match (tuple *actuals, tuple *formals) {
  long nactual, nformal;
  long iactual;
  string *procname = (string *)tupelt(actuals,2);
  assert (equal_string(procname, (string *)tupelt(formals,2)));
  nactual = actuals->nelt;
  nformal = formals->nelt;
  if (nactual < nformal) {
    tranerr("V-call of %ld-ary %s with only %ld args",
             nformal-2, tame(&strelt(procname,1)), nactual-2);
  }
  for (iactual=3; iactual<=nformal-2/*sic*/; iactual++) {
    arg_match (actuals, iactual, formals, iactual);
  }
  for (; iactual<=nactual-1; iactual++) {
    arg_match (actuals, iactual, formals, nformal-1);
  }
  arg_match (actuals, nactual, formals, nformal);
}

/* Check call arg prefix against r/w bits expected for a proc formal.  */
static void arg_match (tuple *actuals, long iactual,    /* call */
                       tuple *formals, long iformal) {  /* proc */
  /* iactual and iformal are field numbers */
  opnd_is (actuals, iactual-1, opnd_rw_bits (formals, iformal-1), true);
}

/* Check operand prefix against given r/w bits.  */
static void opnd_is (tuple *fields, long iopnd, int rw_bits, bool call_op) {
  int opnd_bits = opnd_rw_bits (fields, iopnd);
  if (opnd_bits != rw_bits) {
    const char *opnd_pfx = rw_prefix(opnd_bits);
    const char *needed_pfx = rw_prefix(rw_bits);
    string *opcode_name = (string *)tupelt(fields, 1);
    string *field = (string *)tupelt(fields, iopnd+1);
    char tamed_arg[TAME_SIZE];  /* can only use tame() once in an expr */
    assert (is_string(opcode_name));
    assert (is_string(field));
    finite_strcpy (tamed_arg,  /* arg sans pfx, trunc'd if nec */
                   &strelt(field,1+strlen(opnd_pfx)));
    if (call_op) {
      /* make the message suit a call instr */
      string *procname = (string *)tupelt(fields, 2);
      assert (is_string(procname));
      tranerr("Arg %ld \"%s%s\" should be \"%s%s\" on %s to %s",
               iopnd-1, opnd_pfx, tamed_arg, needed_pfx, tamed_arg,
               &strelt(opcode_name,1), tame(&strelt(procname,1)));
    } else {
      /* make the message suit a non-call instr, or the first opnd of
       * an icall instr */
      tranerr("Operand %ld \"%s%s\" should be \"%s%s\" on %s instr",
               iopnd, opnd_pfx, tamed_arg, needed_pfx, tamed_arg,
                                          &strelt(opcode_name,1));
    }
  }
}

static void append_n (string **name, long n) {
  char ming[25];
  sprintf(ming, "_%ld", n );
  str_append(name, ming);
}

#undef bad_operand
