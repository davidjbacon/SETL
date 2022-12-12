/*  ===  Assemble code for and launch the SETL virtual machine  ====  */

/*  $Id: go.c,v 1.30 2021/02/13 16:01:22 setlorg Exp $  */

/*  Free software (c) dB - see file COPYING for license (GPL).  */

/*
 *  Input is in sections headed by %SOURCE, %CODE, and %EXECUTE
 *  lines.  Any line beginning with "#" is a comment.
 *
 *  After %SOURCE should appear a line identifying the origin of
 *  the SETL source code (e.g. a filename or other designation),
 *  followed by the SETL source code (text) itself.  Everything up
 *  to and including the first tab character is ignored in the SETL
 *  source lines.  That part of each line happens to be spewed as
 *  line numbers by the SETL Translator.
 *
 *  After %CODE should appear the SETL virtual machine code in its
 *  strange assembler-like textual form; try the "-c" option of the
 *  standard "setl" command to get an example.
 *
 *  %SOURCE and %CODE sections are cumulative.
 *
 *  The %EXECUTE line triggers "assembly" of the SETL virtual machine
 *  code into its internal form, and submission of this to execute().
 *
 *  This module will repeat the input, assembly, and execution cycle
 *  as long as %SOURCE, %CODE, and %EXECUTE sections appear.  Multiple
 *  %EXECUTE sections may be supplied, in which case the previously
 *  assembled code is executed.  Any subsequent %SOURCE or %CODE
 *  directive causes all source and SETL VM code accumulated before
 *  the last %EXECUTE-triggered execution to be forgotten.
 *
 *  If the undocumented "spew" command-line option is present,
 *  %EXECUTE causes emission of a machine-friendly representation
 *  of the assembled SETL VM code on standard output, instead of
 *  actual execution.
 */

#include "setlrun.h"

bool exit_please;  /* set to make me stop reading SETL VM input lines */

int go(void) {

  tuple *sources;    HANDLE h_sources;    /* filename(s), text(s) */
  tuple *vcode;      HANDLE h_vcode;      /* as presented to assemble() */
  codeline *line;    HANDLE h_line;       /* a "line" thereof */
  source *in;        HANDLE h_in;         /* current source */
  string *filename;  HANDLE h_filename;   /* current filename */
  string *srctext;   HANDLE h_srctext;    /* current source text */
  string *rawline;   HANDLE h_rawline;    /* line from get_line() */
  enum {nowhere,insource,incode,executed} context;  /* processing state */
  long srcnum;
  int exit_status;

  sources  = null_tuple();  h_sources  = ref(sources);
  vcode    = null_tuple();  h_vcode    = ref(vcode);
  line     = NULL;          h_line     = ref(line);
  in       = NULL;          h_in       = ref(in);
  filename = NULL;          h_filename = ref(filename);
  srctext  = NULL;          h_srctext  = ref(srctext);
  rawline  = NULL;          h_rawline  = ref(rawline);
  context = nowhere;
  srcnum = 0;
  exit_status = 0;
  exit_please = false;
  while ( !exit_please && (rawline = get_line()) != OM) {
    long i,n;
    const char *p = &strelt(rawline,1);
    switch (p[0]) {
    case '#':
      break;
    case '%':
      if (context == insource) {
        /* tie off this source fragment, and append it to 'sources' */
        in = fblock(source);
        in->filename = filename;
        in->srctext = srctext;
        filename = NULL;
        srctext = NULL;
        tup_tackon(&sources,(block *)in);
        in = NULL;
        srcnum = sources->nelt;
      }
      p = &strelt(rawline,1);  /* renew in case rawline has moved */
      /*
       *  Multiple executions via %EXECUTE are permitted without new
       *  source (%SOURCE) or vcode (%CODE) needing to be supplied, but
       *  new source or vcode after an execution causes all source and
       *  vcode acquired before that execution to be forgotten.
       */
      if (leq(p,"%SOURCE")) {
        if (context == executed) {
          sources = null_tuple();
          vcode = null_tuple();
        }
        filename = get_line();
        srctext = null_string();
        context = insource;
      } else if (leq(p,"%CODE")) {
        if (context == executed) {
          sources = null_tuple();
          vcode = null_tuple();
        }
        context = incode;
      } else if (leq(p,"%EXECUTE")) {
        if (verbose) print_stderr ("%s: beginning \"assembly\"\n",
                                    setlprog);
        /*
         *  Reassembling after each %EXECUTE is inefficient (when the
         *  sources and vcode haven't changed since the last %EXECUTE),
         *  but safe.  If you decide that consecutive %EXECUTE lines
         *  are in fact useful enough that you want to optimize this by
         *  skipping the following assemble() call when possible, please
         *  make sure you at least reinitialize the appropriate fields
         *  of setl_vm (pc, level, exit_status, raw_status, eof, magic,
         *  intslash, and display) like assemble() would, before calling
         *  execute() again.
         */
        setl_vm = assemble(vcode,sources);  /* assemble and "link" */
        if (spew) {
          if (verbose) print_stderr ("%s: beginning to spew\n",
                                      setlprog);
          spewcode();  /* emit a representation of the assembled code */
          if (verbose) print_stderr ("%s: finished spewing\n",
                                      setlprog);
        } else {
          if (verbose) print_stderr ("%s: beginning execution\n",
                                      setlprog);
          os_set_time_base();  /* elapsed time officially begins now */
          errno = 0;  /* let LAST_ERROR initially be NO_ERROR */
          execute();  /* execute the assembled program */
          file_rites();  /* flush all streams; close non-'can_persist' */
          if (verbose) print_stderr ("%s: execution finished\n",
                                      setlprog);
          exit_status = setl_vm->exit_status;
        }
        setl_vm = NULL;
        context = executed;
      } /* else ignore unrecognized directive */
      break;
    default:
      n = strlen(p);
      switch (context) {
      case nowhere:
        /* ignore line out of sequence */
        break;
      case insource:
        /* advance i over the line number */
        for (i=0; i<n && !separator[(uchar)p[i]]; i++) ;
        /* i points at the separator in C index conventions, so i+2 is
         * the first thing after the separator in SETL conventions;
         * but if the separator is really the line-terminating NUL,
         * then the line is ill-formed and we just skip it: */
        if (i != n) {
          str_concat_substring(&srctext, rawline, i+2, rawline->nchar);
          str_tackon(&srctext, '\n');
        }
        break;
      case incode:
        /* advance i over the offset field */
        for (i=0; i<n && !separator[(uchar)p[i]]; i++) ;
        if (!digital(p)) {
          /* Since the field in p is terminated with a separator,
           * usually a tab, make a nul-terminated copy of that in q,
           * limited in size to just above tame()'s threshold for
           * truncating and appending three dots.  */
          char q[TAME_SIZE + 1];
          strncpy_plus_nul(q, p, MIN(i,TAME_SIZE));
          tranerr("Non-digital offset field (%s)", tame(q));
        }
        line = fblock(codeline);
        line->srcnum = srcnum;
        p = &strelt(rawline,1);  /* renew in case rawline has moved */
        errno = 0;
        line->offset = strtol(p,NULL,10);
        if (errno == ERANGE) {
          /* Same deal as for non-digital p above.  */
          char q[TAME_SIZE + 1];
          strncpy_plus_nul(q, p, MIN(i,TAME_SIZE));
          tranerr("Offset field (%s) too big for C long int", tame(q));
        }
        assert (errno == 0);  /* EINVAL ruled out by checks above */
        for (; i<n && separator[(uchar)p[i]]; i++) ;
        line->text = NULL;  /* in case copy_substring garbage-collects */
        let (line->text, copy_substring(rawline, i+1, rawline->nchar));
        tup_tackon(&vcode, (block *)line);
        line = NULL;
        break;
      case executed:
        /* ignore unused line after %EXECUTE */
        break;
      default:
        unexpected (context);
      } /* end switch (context) */
      break;
    } /* end switch (p[0]) */
  } /* end while */
  sources  = NULL;
  vcode    = NULL;
  filename = NULL;
  srctext  = NULL;
  retire(h_rawline);
  retire(h_srctext);
  retire(h_filename);
  retire(h_in);
  retire(h_line);
  retire(h_vcode);
  retire(h_sources);

  return exit_status;

} /* end go */
