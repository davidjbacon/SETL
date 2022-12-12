/*  ===  Emit a compact form of the "assembled" input code  ========  */

/*  $Id: spewcode.c,v 1.16 2020/12/12 01:19:00 setlorg Exp $  */

/*  Free software (c) dB - see file COPYING for license (GPL).  */

/*
 *  The principal routine of this module, 'spewcode', emits a compact
 *  representation of the SETL virtual machine setl_vm's symbol tables
 *  and code (source and object) on its standard output, for the use of
 *  other interpreters.
 *
 *  [The original version was a hack for the "setlterp" that was to
 *  use the SETL/C++ library.  Now it is that program which will have
 *  to adapt, probably with the aid of some *.defns files here.]
 *
 *  In the output, all tokens are integers (digit strings) or strings.
 *  Each integer may be preceded by one or more whitespace characters.
 *  Each string is preceded by its length (as an integer) and a single
 *  whitespace character.
 *
 *  Here is a reference summary of the format:
 *
 *    nsource
 *    nsource * details
 *     where the details consist of string pairs:
 *    name  (an arbitrary name for this source fragment)
 *    text  (some SETL source text)
 *
 *    Tree of "lexical scope" nodes, starting at the single root:
 *      lexical level  (level 0 is the root)
 *      name  (placeholder for level 0, routine name otherwise)
 *            [could be a full signature in some future of overloading]
 *      srcnum, offset  (index into the sources, and byte offset)
 *      ndata
 *      ndata * name  (naming an identifier, literal, etc.)
 *      nroutines
 *      nroutines * child lexical scope node
 *
 *    DISPLAY_LEVELS
 *    ninstr
 *    ninstr * instruction
 *     where each instruction is
 *    srcnum, offset, opcode, nopnd, nopnd * operand
 *     and each operand is an opndtype code followed by one or two
 *     numbers (a lexical level followed by a slot number, or a
 *     single integer; scan for 'opndtype' in this file)
 *
 *  Try a command like "setl -spew 'print (3 - 5);'" to generate an
 *  example of the above.
 *
 *  Note that filenames and line numbers in error messages should be
 *  quoted relative to the cpp annotations if present in the source.
 *
 *  For us, each child scope node will just have level 1, and its own
 *  nroutines will be 0 (no further nested scopes).  "Globals" are the
 *  data declared at level 0 and "locals" are the data at each level 1.
 */

#include "setlrun.h"
#include "custom.h"

void spewcode(void) {

  symtab   *sym       = NULL;   HANDLE h_sym       = ref(sym);      
  tuple    *sources   = NULL;   HANDLE h_sources   = ref(sources);  
  tuple    *vcode     = NULL;   HANDLE h_vcode     = ref(vcode);    
  tuple    *code      = NULL;   HANDLE h_code      = ref(code);     
  codeline *line      = NULL;   HANDLE h_line      = ref(line);     
  source   *src       = NULL;   HANDLE h_src       = ref(src);      
  string   *filename  = NULL;   HANDLE h_filename  = ref(filename); 
  string   *srctext   = NULL;   HANDLE h_srctext   = ref(srctext);  
  table    *globals   = NULL;   HANDLE h_globals   = ref(globals);  
  table    *locals    = NULL;   HANDLE h_locals    = ref(locals);   
  table    *curlocals = NULL;   HANDLE h_curlocals = ref(curlocals);
  string   *curproc   = NULL;   HANDLE h_curproc   = ref(curproc);  
  key      *kurprok   = NULL;   HANDLE h_kurprok   = ref(kurprok);  
  string   *scrstr    = NULL;   HANDLE h_scrstr    = ref(scrstr);   
  key      *scrkey    = NULL;   HANDLE h_scrkey    = ref(scrkey);   
  tuple    *scrtup    = NULL;   HANDLE h_scrtup    = ref(scrtup);   
  tuple    *procnames = NULL;   HANDLE h_procnames = ref(procnames);
  table    *procaddrs = NULL;   HANDLE h_procaddrs = ref(procaddrs);
  instr    *ip        = NULL;   HANDLE h_ip        = ref(ip);       
  const char *s;
  long iglobal,nglobal,slotnum;
  long ilocal,nlocal;
  long iproc,nproc;
  long srcnum,offset;
  long iv,pc,ninstr,iopnd;

  sym = setl_vm->sym;
  assert (is_symtab(sym));
  sources = sym->sources;
  assert (is_tuple(sources));
  vcode = sym->vcode;
  assert (is_tuple(vcode));
  os_printf ("%ld\n", sources->nelt);
  for (srcnum=1; srcnum<=sources->nelt; srcnum++) {
    src = (source *)tupelt(sources,srcnum);
    assert (is_source(src));
    filename = src->filename;
    if (filename) {
      assert (is_string(filename));
      s = &strelt(filename,1);
      os_printf ("%ld %s\n", filename->nchar, s);
      filename = NULL;
    } else {
      s = "<of unknown origin>";
      os_printf ("%ld %s\n", (long)strlen(s), s);
    }
    srctext = src->srctext;
    if (srctext) {
      assert (is_string(srctext));
      s = &strelt(srctext,1);
      os_printf ("%ld\n%s", srctext->nchar, s);
      srctext = NULL;
    } else {
      os_printf ("0\n");
    }
    src = NULL;
  }
  os_printf ("\n");

  procnames = sym->procnames;
  assert (is_tuple(procnames));
  procaddrs = sym->procaddrs;
  assert (is_table(procaddrs));
  nproc = procnames->nelt;
  os_printf ("0\n");  /* level */
  s = "SETL program";
  os_printf ("%ld %s\n", (long)strlen(s), s);
  /* This is highly meaningless given that the level 0 symbols could be
   * scattered among program and package units: */
  os_printf ("1 0\n");  /* stand-in srcnum and offset for top level */
  globals = sym->globals;
  assert (is_table(globals));
  nglobal = patsiz(globals);
  os_printf ("%ld\n", nglobal);
  scrtup = new_tuple(nglobal);
  for (iglobal=1; iglobal<=nglobal; iglobal++) {
    scrkey = ith_key(globals,iglobal);
    scrstr = key_to_string(scrkey);
    slotnum = lookup_num(globals,scrkey);
    assert (slotnum >= 1);
    assert (slotnum <= nglobal);
    assert (tupelt(scrtup,slotnum) == OM);
    tupelt(scrtup,slotnum) = (block *)scrstr;  /* gblid */
    scrstr = NULL;
    scrkey = NULL;
  }
  for (iglobal=1; iglobal<=nglobal; iglobal++) {
    string *gblid = (string *)tupelt(scrtup,iglobal);
    assert (is_string(gblid));
    s = &strelt(gblid,1);
    os_printf ("%ld %s\n", gblid->nchar, s);
  }
  scrtup = NULL;
  locals = sym->locals;
  assert (is_table(locals));
  code = setl_vm->code;
  assert (is_tuple(code));
  os_printf ("%ld\n", nproc);
  for (iproc=1; iproc<=nproc; iproc++) {
    os_printf ("1\n");     /* level */
    curproc = (string *)tupelt(procnames,iproc);
    assert (is_string(curproc));
    s = &strelt(curproc,1);
    os_printf ("%ld %s\n", curproc->nchar, s);
    kurprok = string_to_key(curproc);
    pc = lookup_num(procaddrs,kurprok);
    ip = (instr *)tupelt(code,pc);
    assert (is_instr(ip));
    iv = ip->vindex;
    assert (iv >= 1);
    assert (iv <= vcode->nelt);
    line = (codeline *)tupelt(vcode,iv);
    assert (is_codeline(line));
    srcnum = line->srcnum;
    offset = line->offset;
    os_printf ("%ld %ld\n", srcnum, offset);
    curlocals = lookup_subtable(locals,kurprok);
    assert (is_table(curlocals));
    nlocal = patsiz(curlocals);
    os_printf ("%ld\n", nlocal);
    scrtup = new_tuple(nlocal);
    for (ilocal=1; ilocal<=nlocal; ilocal++) {
      scrkey = ith_key(curlocals,ilocal);
      scrstr = key_to_string(scrkey);
      slotnum = lookup_num(curlocals,scrkey);
      assert (slotnum >= 1);
      assert (slotnum <= nlocal);
      assert (tupelt(scrtup,slotnum) == OM);
      tupelt(scrtup,slotnum) = (block *)scrstr;  /* lclid */
      scrstr = NULL;
      scrkey = NULL;
    }
    for (ilocal=1; ilocal<=nlocal; ilocal++) {
      string *lclid = (string *)tupelt(scrtup,ilocal);
      assert (is_string(lclid));
      s = &strelt(lclid,1);
      os_printf ("%ld %s\n", lclid->nchar, s);
    }
    os_printf ("0\n");     /* our routines have no children */
    scrtup = NULL;
    curlocals = NULL;
    line = NULL;
    ip = NULL;
    kurprok = NULL;
    curproc = NULL;
  }
  os_printf ("\n");
  locals = NULL;
  globals = NULL;
  procaddrs = NULL;
  procnames = NULL;

  os_printf ("%ld\n", DISPLAY_LEVELS);

  ninstr = code->nelt;
  os_printf ("%ld\n", ninstr);
  for (pc=1; pc<=ninstr; pc++) {
    ip = (instr *)tupelt(code,pc);
    assert (is_instr(ip));
    iv = ip->vindex;
    assert (iv >= 1);
    assert (iv <= vcode->nelt);
    line = (codeline *)tupelt(vcode,iv);
    assert (is_codeline(line));
    srcnum = line->srcnum;
    offset = line->offset;
    os_printf ("%ld %ld %d %ld", srcnum, offset, ip->op, ip->nopnd);
    for (iopnd=1; iopnd<=ip->nopnd; iopnd++) {
      opndtype type = find_opndtype(ip,iopnd);
      switch (type) {
      case data_opnd:
        os_printf(" %d %ld %ld", type, instrelt(ip,iopnd).data.level,
                                       instrelt(ip,iopnd).data.slot);
        break;
      case parm_opnd:
        /*
         *  This covers both parmins and parmouts; and because we
         *  haven't taken the trouble in opndtypes.awk -> opndtypes.c
         *  (the guts of find_opndtype()) to classify backtracks
         *  properly, it adventitiously covers them too:
         */
        os_printf(" %d %ld %ld", type, instrelt(ip,iopnd).parmin.parmnum,
                                       instrelt(ip,iopnd).parmin.slot);
        break;
      case pc_opnd:
        os_printf(" %d %ld", type, instrelt(ip,iopnd).pc);
        break;
      case sysrot_opnd:
        os_printf(" %d %ld", type, instrelt(ip,iopnd).sysrot);
        break;
      case int_opnd:
        os_printf(" %d %d", type, instrelt(ip,iopnd).intval);
        break;
      case long_opnd:
        os_printf(" %d %ld", type, instrelt(ip,iopnd).longval);
        break;
      default:
        unexpected (type);
      }
    }
    os_printf ("\n");
    line = NULL;
    ip = NULL;
  }

  code = NULL;
  vcode = NULL;
  sources = NULL;
  sym = NULL;

  retire(h_ip);
  retire(h_procaddrs);
  retire(h_procnames);
  retire(h_scrtup);
  retire(h_scrkey);
  retire(h_scrstr);
  retire(h_kurprok);
  retire(h_curproc);
  retire(h_curlocals);
  retire(h_locals);
  retire(h_globals);
  retire(h_srctext);
  retire(h_filename);
  retire(h_src);
  retire(h_line);
  retire(h_code);
  retire(h_vcode);
  retire(h_sources);
  retire(h_sym);

}
