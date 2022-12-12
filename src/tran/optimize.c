/*  ===  Code optimization  ========================================  */

/*  $Id: optimize.c,v 1.7 2020/12/20 00:34:01 setlorg Exp $  */

/*  Free software (c) dB - see file COPYING for license (GPL).  */

/*
 *  So far, this just means short-circuiting jump-to-jump sequences,
 *  removing dead code, and removing redundant labels.
 */

/* ------------------------------------------------------------------ */

#include "setltran.h"

/* ------------------------------------------------------------------ */

#define cscan  for (r=virtcode; r!=(csect*)NULL; r=r->next)
#define bscan  for (b=r->blocks; b!=(block*)NULL; b=b->next)
#define lscan  for (l=b->labels; l!=(label*)NULL; l=l->next)
#define iscan  for (i=b->instrs; i!=(instr*)NULL; i=i->next)
#define oscan  for (o=i->operas; o!=(opera*)NULL; o=o->next)

void optimize(void)
{
  table lb,ref;
  csect *r;
  block *b,*c;
  label *l;
  instr *i,*j;
  opera *o,*p;
  datum *d;
  int spam;
  bool removing;
  /*
   *  Process each routine in turn
   */
  cscan {
    /*
     *  Make table from labels to code block addresses
     */
    patcre(&lb);
    bscan {
      lscan {
        check (!keysub(lb,l->lab,&d));  /* insert */
        d->blkptr = b;
      }
    }
    /*
     *  Forget all but the first label of each block
     */
    bscan {
      if (b->labels != (label *)NULL) b->labels->next = (label *)NULL;
    }
    /*
     *  Make all label references be to the first label of a block
     */
    bscan {
      iscan {
        oscan {
          if (o->type == label_opera) {
            check (lookup(lb,o->libretto,&d));
            o->libretto = d->blkptr->labels->lab;
          }
        }
      }
    }
    /*
     *  Short-circuit all jumps-to-jumps
     */
    bscan {
      iscan {
        oscan {
          if (o->type == label_opera) {
            const char *s = NULL;  /* init. to avoid C optimizer warning */
            p = o;
            /*
             *  The meaning of this late change from "while (true)" (to
             *  stop the translator from going into an infinite loop on
             *  a trivial infinite loop in the SETL program!), is that
             *  if the jump-to-jump sequence reaches back to its
             *  beginning, it is left alone (and o->libretto is set to
             *  point to some arbitrary point in it).  We could report
             *  that code flow analysis has detected an infinite loop,
             *  but if users code trivial infinite loops they probably
             *  really want them!  De-optimizing this loop entirely is
             *  (strictly speaking) a little extreme, but who cares.
             *  And really strictly speaking, the limit of 1000 on the
             *  number of jumps to jumps before "shorting out" the chain
             *  could miss some finite (but very big) cases, so it would
             *  be incorrect to flag down falling out of this loop(!):
             */
            for (spam=0; spam<1000; spam++) {  /* was "while (true)" */
              s = p->libretto;
              check (lookup(lb,s,&d));
              c = d->blkptr;
              j = c->instrs;
              if (j == (instr *)NULL) break;
              if (j->opcode != jmp_op) break;
              assert (j->next == (instr *)NULL);
              p = j->operas;
              assert (p->type == label_opera);
            }
            o->libretto = s;
          }
        }
      }
    }
    /*
     *  Remove dead code until there's no more to remove
     */
    do {
      removing = false;
      /*
       *  Remove unconditional jumps from one block to the next
       */
      bscan {
        iscan {
          if (i->opcode == jmp_op) {
            assert (i->next == (instr *)NULL);
            o = i->operas;
            assert (o->type == label_opera);
            check (lookup(lb,o->libretto,&d));
            if (d->blkptr == b->next) {
              if (i == b->instrs) {
                b->instrs = (instr *)NULL;
              } else {
                for (j = b->instrs; j->next != i; j = j->next) ;
                j->next = (instr *)NULL;
              }
            }
          }
        }
      }
      /*
       *  Make table of reference counts
       */
      patcre(&ref);
      bscan {
        iscan {
          oscan {
            if (o->type == label_opera) {
              keysub(ref,o->libretto,&d);
              d->count = 1;  /* don't care about the actual number */
            }
          }
        }
      }
      /*
       *  Remove all block labels with reference counts of 0
       */
      bscan {
        l = b->labels;
        if (l != (label *)NULL) {
          if (!lookup(ref,l->lab,&d)) b->labels = (label *)NULL;
        }
      }
      /*
       *  Remove all unlabelled blocks that cannot be flowed into
       *  (except for the first block)
       */
      bscan {
        c = b->next;
        if (c != (block *)NULL &&
            c->labels == (label *)NULL) {
          iscan {
            if (i->opcode == jmp_op    ||
                i->opcode == return_op ||
                i->opcode == stop_op   ||
                i->opcode == fail_op    ) {
              assert (i->next == (instr *)NULL);
              /*
               *  To avoid quadratic behaviour in the optimizer in
               *  pathological cases, remove not just the unreachable
               *  block c but also all unlabelled blocks immediately
               *  after it in the csect, up to but not including the
               *  end_op block
               */
              while (c != (block *)NULL &&
                     c->labels == (label *)NULL &&
                     (c->instrs == (instr *)NULL ||
                      c->instrs->opcode != end_op)) {
                c = c->next;
                b->next = c;
                removing = true;
              }
            }
          }
        }
      }
    } while (removing);
  }
}
