## Process this file with automake to generate Makefile.in
#
# $Id: Makefile.am,v 1.25 2022/12/11 17:09:27 setlorg Exp $
#
# Free software (c) dB - see file COPYING for license (GPL).
#
# Makefile.in -> Makefile via "configure" or "config.status" script.


######################################################################
#
#  Please see the README file on how to update Makefile.am with
#  new tests.
#
######################################################################


# Suppress complaints about use of GNU Make extensions:
AUTOMAKE_OPTIONS = -Wno-portability

# srcdir is already exported to the scripts we run, but we want these
# as well:
export top_builddir
export top_srcdir

# All the test scripts:
TESTS = $(dist_check_SCRIPTS)

# Test(s) we expect to fail in particular environment(s):
XFAIL_TESTS = $(if $(findstring cygwin,$(host_os)),t-fd-pass)

dist_check_SCRIPTS = $(test_scripts)

EXTRA_DIST =      \
 $(test_programs) \
 $(test_subprogs) \
 $(test_sh)       \
 $(test_aux)      \
 $(test_data)     \
 $(test_outputs)

test_scripts = \
 t-any         \
 t-assign      \
 t-b           \
 t-calls       \
 t-comb        \
 t-compound    \
 t-connie      \
 t-count       \
 t-dike        \
 t-div         \
 t-dumdum      \
 t-eof         \
 t-euler       \
 t-fd-pass     \
 t-filter      \
 t-from        \
 t-geniters    \
 t-get         \
 t-group       \
 t-hello       \
 t-holes       \
 t-integrate   \
 t-iter        \
 t-magic       \
 t-matvec      \
 t-mmap        \
 t-npow        \
 t-patty       \
 t-pax         \
 t-perm        \
 t-pi          \
 t-pipe        \
 t-pretty      \
 t-primes      \
 t-primes100   \
 t-procval     \
 t-procval3    \
 t-read        \
 t-reverse     \
 t-rex         \
 t-seek        \
 t-sel         \
 t-select      \
 t-setperm     \
 t-sets        \
 t-slice       \
 t-smap        \
 t-sort        \
 t-spew        \
 t-split       \
 t-suck        \
 t-system      \
 t-t           \
 t-toto        \
 t-tty-pump    \
 t-tuple       \
 t-twinx       \
 t-unnamed     \
 t-unstr       \
 t-valstr

test_programs = $(test_scripts:t-%=%.setl)

# Sub-programs invoked by certain test program:
test_subprogs =    \
 fd-pass-main.setl \
 fd-pass-pump.setl

# Scripts to produce outputs for comparison with SETL program outputs:
test_sh =      \
 compound.sh   \
 pipe.sh

# Supportive bits that don't really fit in elsewhere:
test_aux =      \
 simple-test    \
 diff-read      \
 diff-read.setl \
 basename.setl  \
 update-out

# Inputs, for those tests that need them:
test_data =    \
 eof.data      \
 get.data      \
 primes.data   \
 read.data     \
 reverse.data  \
 tty-pump.data \
 twinx.data    \
 unstr.data

# Static outputs for comparison with SETL program outputs:
test_outputs = \
 any.out       \
 assign.out    \
 b.out         \
 calls.out     \
 connie.out    \
 count.out     \
 dike.out      \
 div.out       \
 dumdum.out    \
 eof.out       \
 euler.out     \
 fd-pass.out   \
 filter.out    \
 geniters.out  \
 get.out       \
 group.out     \
 hello.out     \
 holes.out     \
 integrate.out \
 iter.out      \
 magic.out     \
 matvec.out    \
 mmap.out      \
 npow.out      \
 pax.out       \
 patty.out     \
 perm.out      \
 pi.out        \
 pretty.out    \
 primes.out    \
 primes100.out \
 procval.out   \
 procval3.out  \
 read.out      \
 reverse.out   \
 rex.out       \
 seek.out      \
 sel.out       \
 select.out    \
 setperm.out   \
 sets.out      \
 slice.out     \
 smap.out      \
 sort.out      \
 split.out     \
 t.out         \
 toto.out      \
 tty-pump.out  \
 twinx.out     \
 unnamed.out   \
 unstr.out     \
 valstr.out

# A convenience rule e.g. "make update-out test=unstr" to update
# unstr.out when you are in the build dir:
update-out:
	srcdir=$(srcdir) $(srcdir)/update-out $(test)
.PHONY:  update-out

## Makefile.am ends here
