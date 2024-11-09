# Used by a makefile to build, check, install, and distribute GNU SETL
#
# $Id: main.mk,v 1.53 2022/12/11 17:09:26 setlorg Exp $
#
# Free software (c) dB - see file COPYING for license (GPL).
#
#
# This main.mk is the workhorse for the main makefile (GNUmakefile)
# in the build dir.  It is designed to be used indirectly, via
# forwarding of requests that go to that default makefile.
#
# The following help applies to GNUmakefile users, even though the
# targets are defined here in main.mk.
#
#HELPBEGIN
#
# These are the main "make" targets:
#
#  [all]      -  set up build envt, configure, build
#  check      -  run some tests before installing
#  install    -  install GNU SETL executables and documentation
#  uninstall  -  remove installed files
#  clean      -  remove easily recreated files from build tree
#  help       -  show this help
#
# These are of more occasional interest:
#
#  setup      -  renew configuration choices (interactive at tty)
#  distclean  -  clean back to freshly unpacked state
#
# These may be of use to maintainers and distributors:
#
#  bin        -  build just executables
#  doc        -  build just .info, .html, .pdf files and man pages
#  web-doc    -  bundle doc files in a tarball for web installation
#  distcheck  -  rigorously check a distribution (recommended!)
#  dist       -  prepare a SETL distribution for export
#  maintainer-clean  -  cleaner than distclean; see GNU docs
#  superclean -  remove everything regenerable (see also preconfig)
#  preconfig  -  gen "configure" scripts, .in files, etc.
#  config     -  gen Makefile and config.h, config.status, etc.
#
# See within main.mk for many more targets.
#
#HELPEND
#
# The "make" command (usually make or gmake, as given by $(MAKE)) is
# assumed to support GNU Make extensions.
#
# First-time builders/installers should normally be able to take
# defaults on all prompts after a no-args "make", and then acquire
# administrative privileges if needed and "make install" from the
# build dir.
#
# You should always be able to "make distclean" and then "make"
# without any maintainer-oriented tools such as automake, autoconf,
# and Texinfo (unless you touch their inputs such as Makefile.am and
# configure.ac).
#
# Builder-oriented tools such as a C compiler and linker, bison/yacc,
# m4, awk, and basic shell tools are required for a default "make".
# GNU tools are a good choice.  There is pretty good conformance to
# POSIX strictures here.
#
# In order to build some of the optional ("customization") packages
# that are under $(sopt), you need to have a working GNU SETL
# implementation already in place (with a findable 'setl' command).
#
# TODO:  Have a target to build a vanilla, uncustomized, default impl.
# To gild the lily outrageously, invoke that target automatically to
# make a temp aux "boot" impl when asked to build a custom impl from
# scratch.
#
# Maintainers may wish to note that if you have the maintainer tools
# and a GNU SETL implementation installed, you should be able to
# make any goal here after a superclean, even distcheck.
#
#
# VPATH-type builds
# -----------------
#
# Building GNU SETL from a separate build directory is easy, allowing
# the distributed source to reside on a read-only medium.
#
# In fact, the suitably impatient builder who starts with a no-args
# "make" command at the top of the distributed source tree, and takes
# defaults at all prompts, will end up with GNU SETL built in a
# separate, freshly-created build dir.
#
# That should be everything you need to know in order to do it.  If
# you want to know how it works, read on...
#
# In more detail, this makefile (main.mk) supports a "loc" variable
# specifying the root of the source tree, which is also the dir
# containing this makefile.  For example, the little proxy GNUmakefile
# that is usually placed in the build directory does this:
#
#  $(MAKE) loc=$(srcdir) -f $(srcdir)/main.mk $@
#
# with $(srcdir) indicating the source dir from the build dir's point
# of view.  On the other hand, the manually maintained (not generated)
# GNUmakefile in the source dir, if the user insists on building in the
# source dir, does this:
#
#  $(MAKE) loc=. -f main.mk $@
#
# Incidentally, main.mk does not itself use the VPATH mechanism (all
# its source tree references are explicit, with $(loc) as their stem,
# which is why we only refer to VPATH-"type" builds in comments here),
# but the generated Makefiles in the build tree use VPATH extensively
# and expect the GNU Make implementation to honour it.
#
# Deep maintenance operations such as changing a Makefile.am file can
# require regeneration of some distributable files, which are in the
# source tree.  This makefile makes every effort to track and respond
# to such dependencies automatically.
#
# All operations, even those that may affect the source tree, can be
# driven from the GNUmakefile at the top of the build tree, whether
# that be also the source tree or not.  Please "make distcheck" in the
# build tree occasionally if you are a maintainer.


SHELL = /bin/sh

# Set default level of build verbosity to relatively quiet.
#
# You can get full compilation command lines spewed by putting
# V=1 on the "make" command, e.g.:
#
#  make V=1 {goals}
#
export V = 0

# A "safe" shell echo (though multiple args will give multiple lines).
#
# Note that stdout is used for all normal user-directed output by this
# makefile and the commands it runs, including prompts in interactive
# modes.  Errors may occasionally produce output on stderr.
#
prt = printf '%s\n'

# A variant using a cheap trick to highlight the text:
pt = printf '%s %s\n' '--->'

# Some external commands:
AUTOMAKE   = automake
AUTOCONF   = autoconf
ACLOCAL    = aclocal
AUTORECONF = autoreconf
AUTOHEADER = autoheader
FIND       = find
GREP       = grep
SED        = sed
SETL       = setl
PATCH      = patch
TAR        = tar
MKDIR_P    = mkdir -p --
GZCAT      = gzip -cd
GZIP_HARD  = gzip -9

# As given by the -f arg on the "make" invocation:
this_makefile = $(lastword $(MAKEFILE_LIST))

# Default source tree root, from the build dir's point of view, in case
# loc={source dir} isn't given on the command line:
loc = .

# Redundant build tree root name from the build dir's point of view.
# Unlike loc, this isn't expected on the command line:
override bld = .

# Key parts of autoconf/automake-managed tree:
src = src
fgt = src/run/freeglut-setl
opt = src/opt
doc = doc

# Top of the overall source tree (not to be confused with src):
sdir = $(loc)

# Top of the overall build tree:
bdir = $(bld)

# Source subdirs:
ssrc = $(sdir)/$(src)
sfgt = $(sdir)/$(fgt)
sopt = $(sdir)/$(opt)
# The bundled copy of the GNU MP library is unpacked in $(bdir), not
# $(sdir):
sgmp = $(bdir)/gmp-source
sdoc = $(sdir)/$(doc)

# Build subdirs in (mostly) the same layout as in the $(sdir)-headed
# source tree above:
bsrc = $(bdir)/$(src)
bfgt = $(bdir)/$(fgt)
bopt = $(bdir)/$(opt)
bgmp = $(bdir)/gmp-build
bdoc = $(bdir)/$(doc)

# A command to give a relative pathname from one dir to another:
path_from := $(sdir)/path-from

# Principal source dirs (including the doc dir) from the corresponding
# build dirs' point of view.  Each path is either absolute or relative
# according to how the source dir is expressed:
define echo_path_from
  case $(2) in \
    [\\/]* | ?:[\\/]* ) \
      $(prt) $(2); \
      ;; \
    * ) \
      $(path_from) $(1) $(2); \
      ;; \
  esac
endef
# Note that path_to_source must remain unevaluated until the point of
# call (whence "=" not ":=" in its defn):
path_to_source = $(shell $(echo_path_from))
bsrc_to_ssrc := $(call path_to_source,$(bsrc),$(ssrc))
bfgt_to_sfgt := $(call path_to_source,$(bfgt),$(sfgt))
bopt_to_sopt := $(call path_to_source,$(bopt),$(sopt))
bgmp_to_sgmp := $(call path_to_source,$(bgmp),$(sgmp))
bdoc_to_sdoc := $(call path_to_source,$(bdoc),$(sdoc))

# Generic "cross product" (or outer or Cartesian product) macro.
#
# For lists A and C, and connective B, $(call cross,$(A),$(B),$(C)) is
# all pairs of A and C members, each connected by B.  For example,
#
#  $(call cross,d1 d2,/,f1 f2 f3)
#
# is
#
#  d1/f1 d1/f2 d1/f3 d2/f1 d2/f2 d2/f3
#
cross = $(foreach a_item,$(1),$(addprefix $(a_item)$(2),$(3)))

# $E is for recipe code in a variable (such as rm_rf below) to use in
# place of backslash at the ends of lines, so that GNU Make's echoing
# of the code will look just as it would if that code appeared directly
# in a rule with backslash-newlines.  For code that isn't meant to be
# echoed (as in recover_removed_target below), you might as well use an
# actual backslash, which gets eaten earlier (when the var is defined).
#
E = \$(nothing_but_nothingness_in_this_undefined_variable)

# Recipe code to recover from the removal of $@ (a target) in a rule,
# like this:
#
# targets:  witness
#	@$(recover_removed_target)
#
# The normal way to request regeneration of a target that depends on a
# witness (timestamp) file, as $< is assumed to be, is to remove the
# witness (preferably via a makefile rule), and then remake the target;
# but a naive user might instead just remove the target and then try to
# remake it directly.  This seems forgivable.
#
define recover_removed_target
if test -f $@; then : OK; else \
  $(prt) 'Regenerating removed file $@'; \
  rm -f $<; \
  $(loc_MAKE) $<; \
fi
endef

# Macro to echo a command on stdout and then execute it, in a recipe:
cmd = { $(prt) "$(1)"; $(1); }


#####################################################################
#
#  Please update the VERSION_NUMBER file before doing the "make dist"
#  of each new GNU SETL release.
#
#  No rules are enforced on the form of the version number, though
#  the intention is to feign a conventional "semantic" scheme of
#  major.minor.patch.  The first public source release will be 3.5.8.
#
VERSION_NUMBER := $(shell cat $(sdir)/VERSION_NUMBER)
#
#####################################################################

# Stem of the dist archive name (and of transient src and doc
# tarballs that are used to create the overall dist archive);
# see also the 4th arg to AC_INIT in {src,doc}/configure.ac:
pkgname = setl

# Versioned stem of the dist archive name:
pkg = $(pkgname)-$(VERSION_NUMBER)

# The dir into which "make dist" puts the dist archive:
dist = $(bdir)

# The transient dir in which a dist is built (which is also the stem of
# the dist archive pathname), and the same as an absolute pathname for
# specificity in certain reporting:
dist_pkg = $(dist)/$(pkg)
#abs_dist_pkg = $(abspath $(dist_pkg))  <-- requires GNU Make 3.81+
abs_dist_pkg := $(shell (cd $(dist); pwd))/$(pkg)

# Name of the ultimate dist archive as viewed resp from within $(dist),
# from the dir containing $(dist), and from anywhere:
pkg_tgz = $(pkg).tgz
dist_tgz = $(dist_pkg).tgz
abs_dist_tgz = $(abs_dist_pkg).tgz

# Other transient dirs that are also transient tarball stems:
src_pkg = $(bsrc)/$(pkg)
doc_pkg = $(bdoc)/$(pkg)

# Transient tarballs used in making the overall dist archive:
src_tgz = $(src_pkg).tar.gz
doc_tgz = $(doc_pkg).tar.gz

# The GMP lib source is only unpacked if needed.  This is the name
# of the tarball and the name of the dir that can be expected to be
# unpacked from it.  There should be only one file matching gmp-*.tgz
# in $(sdir), but we use wildcard and lastword here in case there is
# more than one when a new version is being tested or something.  Note
# that lexical sort order fails on transitions like .9 -> .10, though:
gmp_tgz := $(lastword $(sort $(wildcard $(sdir)/gmp-*.tgz)))
gmp_dir := $(notdir $(gmp_tgz:%.tgz=%))

# Libtool creates this file (a human-readable text file describing the
# static and dynamic forms of the GMP lib) when the bundled GMP is used:
gmplib = $(bgmp)/libgmp.la

# Distributed files in $(sdir) beyond those made in src and doc dirs:
EXTRA_DIST = VERSION_NUMBER \
 AUTHORS BUGS ChangeLog COPYING INSTALL NEWS README \
 GNUmakefile Makefile main.mk \
 mkbuild setup-parms ask-yes path-from doc-dist \
 configure stamp-auto \
 doc-legacy/setlprog.pdf doc-legacy/lib.html \
 gmp-*.tgz \
 cpp-2.7.2.1.tgz freeglut-2.0.1.tgz \
 gears.tgz

# Optional customization packages currently available:
opt_subdirs := $(shell $(ssrc)/pax-avail $(sopt))

# We manage the src, fgt (freeglut), opt, and doc "projects" from here.
#
# Each such project has its own configure.ac; we manage from here the
# few inter-project build dependencies that exist, and provide rules
# to rebuild configure scripts and Makefile.in files when their
# prerequisites (configure.ac and Makefile.am files) change.

src_build_dirs = $(bsrc)       \
                 $(bsrc)/cpp   \
                 $(bsrc)/tran  \
                 $(bsrc)/run   \
                 $(bsrc)/run/w \
                 $(bsrc)/tests

fgt_build_dirs = $(bfgt)

opt_build_dirs = $(bopt) $(addprefix $(bopt)/,$(opt_subdirs))

doc_build_dirs = $(bdoc)

# All source dirs from the top-level build dir's point of view:
src_source_dirs  = $(addprefix $(sdir)/,$(src_build_dirs))
fgt_source_dirs  = $(addprefix $(sdir)/,$(fgt_build_dirs))
opt_source_dirs  = $(addprefix $(sdir)/,$(opt_build_dirs))
doc_source_dirs  = $(addprefix $(sdir)/,$(doc_build_dirs))

# All Makefile.am files:
src_Makefile_ams = $(addsuffix /Makefile.am,$(src_source_dirs))
fgt_Makefile_ams := $(shell $(FIND) $(fgt_source_dirs) -name Makefile.am)
opt_Makefile_ams = $(addsuffix /Makefile.am,$(opt_source_dirs))
doc_Makefile_ams = $(addsuffix /Makefile.am,$(doc_source_dirs))

# All Makefile.in files:
src_Makefile_ins = $(addsuffix /Makefile.in,$(src_source_dirs))
fgt_Makefile_ins = $(fgt_Makefile_ams:.am=.in)
opt_Makefile_ins = $(addsuffix /Makefile.in,$(opt_source_dirs))
doc_Makefile_ins = $(addsuffix /Makefile.in,$(doc_source_dirs))

# All Makefiles:
bsrc_Makefiles   = $(addsuffix /Makefile,$(src_build_dirs))
bfgt_Makefiles   = $(patsubst $(sdir)%,$(bdir)%,$(fgt_Makefile_ins:.in=))
bopt_Makefiles   = $(addsuffix /Makefile,$(opt_build_dirs))
bdoc_Makefiles   = $(addsuffix /Makefile,$(doc_build_dirs))

all_src_Makefiles = $(bsrc_Makefiles) $(bfgt_Makefiles) $(bopt_Makefiles)
all_doc_Makefiles = $(bdoc_Makefiles)

# Where configure scripts are generated by autoreconf:
all_configure_dirs = $(ssrc) $(sfgt) $(sopt) $(sdoc)

# All our configure.ac files (configure script templates):
all_configure_acs = $(addsuffix /configure.ac,$(all_configure_dirs))

# All the configure scripts we generate:
all_configures    = $(addsuffix /configure,$(all_configure_dirs))

# Where config.status scripts are created by configure scripts:
src_config_status_dirs = $(bsrc) $(bfgt) $(bopt)
doc_config_status_dirs = $(bdoc)

# The custom.* ("optional customization") files:
opt_files = $(call cross,$(opt_build_dirs),/custom., \
                         h lexicon sysrots dispatch setl)

# Input files for 'autoreconf'.  Among these, only $(ssrc)/version.m4
# and $(sdoc)/version.m4 are generated by rules in this makefile.  This
# list also includes $(sdoc)/texinfo.tex.patch but only because it is
# used in the rule where autoreconf is run:
autoreconf_inputs  = $(ssrc)/version.m4   \
                     $(sdoc)/version.m4   \
                     $(src_Makefile_ams)  \
                     $(fgt_Makefile_ams)  \
                     $(opt_Makefile_ams)  \
                     $(doc_Makefile_ams)  \
                     $(all_configure_acs) \
                     $(sdoc)/texinfo.tex.patch

# The two kinds of autoreconf output don't actually need to be
# distinguished from each other as they are in the following, but
# there is no harm (and perhaps some documentary value) in doing so.

# Input-sensitive autoreconf outputs:
generated_autobits = $(ssrc)/aclocal.m4   \
                     $(sfgt)/aclocal.m4   \
                     $(sopt)/aclocal.m4   \
                     $(sdoc)/aclocal.m4   \
                     $(ssrc)/config.h.in  \
                     $(sfgt)/config.h.in  \
                     $(src_Makefile_ins)  \
                     $(opt_Makefile_ins)  \
                     $(fgt_Makefile_ins)  \
                     $(doc_Makefile_ins)  \
                     $(all_configures)

# Input-insensitive autoreconf outputs.  Note that this list no longer
# contains $(sdoc)/texinfo.tex, because although autoreconf will
# recreate it if it doesn't exist, we don't particularly want to lose it
# in the first place.  If you want to upgrade it, though, it may be best
# to grab the latest version directly from the Texinfo web site at
# www.gnu.org/software/texinfo/
# and then see the texinfo-patch rule below:
standard_creations = $(ssrc)/ltmain.sh    \
                     $(sfgt)/ltmain.sh    \
                     $(sopt)/ltmain.sh    \
                     $(ssrc)/config.guess \
                     $(sfgt)/config.guess \
                     $(sopt)/config.guess \
                     $(ssrc)/config.sub   \
                     $(sfgt)/config.sub   \
                     $(sopt)/config.sub   \
                     $(ssrc)/compile      \
                     $(sfgt)/compile      \
                     $(sopt)/compile      \
                     $(ssrc)/install-sh   \
                     $(sfgt)/install-sh   \
                     $(sopt)/install-sh   \
                     $(sdoc)/install-sh   \
                     $(ssrc)/missing      \
                     $(sfgt)/missing      \
                     $(sopt)/missing      \
                     $(sdoc)/missing      \
                     $(ssrc)/depcomp      \
                     $(sfgt)/depcomp      \
                     $(sdoc)/mdate-sh     \
                     $(ssrc)/INSTALL      \
                     $(sdoc)/INSTALL      \
                     $(ssrc)/ar-lib       \
                     $(sfgt)/ar-lib       \
                     $(ssrc)/test-driver

# Basenames of some libtoolize-generated m4 scripts:
m4_basenames = libtool.m4   \
               ltoptions.m4 \
               ltsugar.m4   \
               ltversion.m4 \
               lt~obsolete.m4

# These used to be among the $(standard_creations) above, but are no
# longer created by autoreconf as of some version or other, so it is
# now unhelpful to have them as prerequisites to anything (but we
# still want this list, because they _may_ get created, and we want
# to make sure they aren't present after a superclean):
old_spam_creations = $(call cross,$(ssrc)/run,/,$(m4_basenames))

# These are now expected to be autogenerated:
standard_creations += $(call cross,$(ssrc)/m4,/,$(m4_basenames))

autoreconf_outputs = $(generated_autobits) $(standard_creations)


# Name of config parms file and its timestamp ("witness") file.
#
# The file $(config_file) records the current configuration parameters
# including customization choices and installation prefix(es), in the
# form of a list of command-line arguments to the standard GNU-style
# configure scripts we generate and manage (see create_config_status).
#
# It is normally initialized either with the command-line args passed
# explicitly to the top-level configure script (a batch-mode user might
# wish to do this) or with args assembled by $(setup_parms).
#
# The witness (timestamp file) for $(config_file) is $(parms_stamp).
#
# The file and witness are kept up to date automatically for changes to
# anything known to affect the config.  To change the config just
# because you want to (and have it witnessed), you can "make setup",
# whereupon the next "make" will run the configure scripts again.
#
config_file = $(bdir)/config.parms
parms_stamp = $(bdir)/stamp-parms

# Witnesses for configuration in the src and doc build subdirs:
config_src_stamp = $(bsrc)/stamp-config-src
config_doc_stamp = $(bdoc)/stamp-config-doc

# $(MAKE) is typically make or gmake:
loc_MAKE   = $(MAKE) -f $(this_makefile)
bsrc_MAKE  = $(MAKE) -C $(bsrc)
bgmp_MAKE  = $(MAKE) -C $(bgmp)
bopt_MAKE  = $(MAKE) -C $(bopt)
bdoc_MAKE  = $(MAKE) -C $(bdoc)

# In an apparent effort to promote universal POSIX compliance,
# Automake 1.14+ rejects rm commands that fail when passed -f without
# any following args (such as the rm on QNX 6.3.2 and 6.5.0) unless
# this ACCEPT_INFERIOR_RM_PROGRAM envt var is set to yes.  Even then,
# a warning will be issued by the configure script if such a "bad" rm
# command is detected.  (If ever you need to look deeper into that,
# the configure script code is emitted by the AM_INIT_AUTOMAKE macro,
# which is called in configure.ac files and defined in the aclocal.m4
# scripts generated by aclocal, a command in Automake.)
ifneq ($(shell rm -f 2>/dev/null && echo ok),ok)  # "rm -f" fails?
  export ACCEPT_INFERIOR_RM_PROGRAM = yes
endif

# $(call rm_rf,<d>) tries to make sure all dirs under <d> are writable
# (which they already are in virtually all the expected use cases) and
# then recursively removes <d> and everything under it:
define rm_rf
  if test -d $(1); then $E
    : Since writable is the norm, no need to optimize using xargs: ; $E
    $(FIND) $(1) -type d ! -perm 200 -exec chmod u+w {} \; ; $E
    rm -rf $(1); $E
  fi
endef

# $(call Rm_archive,<d>) tries to rm all <d>.tgz, <d>.tar.gz, and <d>/:
define rm_archive
  -rm -f $(1).tgz $(1).tar.gz
  $(call rm_rf,$(1))
endef

# A command to present a prompt and [Y/n], and succeed if appropriate:
ask_yes = $(sdir)/ask-yes

# A command to collect config parms into the $(config_file):
setup_parms = $(sdir)/setup-parms

# A GNU Make way of saying that no-args, not "all", is the default
# target to be made:
.DEFAULT_GOAL := no-args

no-args:
	@$(prt) '*** No default target in $(this_makefile), which is'
	@$(prt) '*** intended only for use by a GNUmakefile.'
	exit 1

# This help is misleading as applied to main.mk (this help is meant for
# the eyes of GNUmakefile invokers, who get here indirectly through
# forwarding) in that "all" is not the default target for main.mk:
#
help:
	$(SED) -n '/^#HELPBEGIN/,/^#HELPEND/p' <$(this_makefile) | \
	  $(SED) 's#make#$(MAKE)#g'


# The "all" target builds the executables and documentation.

all:
# Do bin and doc in series rather than simply listing them as prereqs of
# all, to avoid any possible race in setting up the common build dir and
# config parms.
	$(loc_MAKE) bin
	$(loc_MAKE) doc
	@$(pt) ''
	@$(pt) ' Possible next step (as admin):  $(MAKE) install'
	@$(pt) ' Or, for a normal native build:  $(MAKE) check'
	@$(pt) ''

bin:  $(all_src_Makefiles)
	@if $(GREP) -q '^#define HAVE_GMP_H 1' \
	   $(bsrc)/config.h 2>/dev/null; then : OK; else \
	  $(prt) 'Building the bundled GMP lib...'; \
	  $(call cmd,$(loc_MAKE) $(gmplib)); \
	fi
# Use a rather backhanded way of discovering what customization
# packages (CUSTOM_PAX) were selected, and then do their
# preprocessing in subdirs of $(bopt):
	@CUSTOM_PAX=`$(SED) -n 's/^CUSTOM_PAX *= *//p' $(bsrc)/Makefile`; \
	$(prt) "CUSTOM_PAX = $$CUSTOM_PAX"; \
	for pak in $$CUSTOM_PAX; do \
	  $(call cmd,$(MAKE) -C $(bopt)/$$pak); \
	done
	# Build the binaries:
	$(bsrc_MAKE)

doc:  $(all_doc_Makefiles)
	$(bdoc_MAKE)

# Create web-doc.tar but use phony target web-doc in case there are
# more files we'd like to produce under that target someday:
web-doc:  doc $(sdir)/doc-dist
	cp -p $(sdoc)/index.html $(bdoc)/
	$(sdir)/doc-dist $(bdir)
	#rm $(bdoc)/index.html
	test -e $(bdoc)/web-doc.tar && mv -v $(bdoc)/web-doc.tar $(bdir)/

# "make check" runs some tests, usually before installation.

check:
	$(loc_MAKE) check-src
	$(loc_MAKE) check-doc
	@$(pt) ''
	@$(pt) ' Possible next step (as admin):  $(MAKE) install'
	@$(pt) ''

check-src:  bin
	$(bsrc_MAKE) check

check-doc:  doc
	$(bdoc_MAKE) check


# "make install" installs the executables and documentation.
#
# The plus signs (+) at the beginnings of the $(..._MAKE) lines in
# these install and uninstall rules are so that we recurse into the
# subdirs even when the -n flag is passed to the top-level make.
#
# Without them, the output from -n at this level would not be very
# informative as to what would be executed without -n.

install:
	+$(loc_MAKE) installation_blurb=no install-bin
	+$(loc_MAKE) installation_blurb=no install-doc
	@$(loc_MAKE) history-bin
	@$(loc_MAKE) history-doc
	@$(loc_MAKE) advertise-bin
	@$(loc_MAKE) advertise-doc

install-bin:  bin
	+$(bsrc_MAKE) install
	@if test x$(installation_blurb) = xno; then : OK; else \
	  $(loc_MAKE) history-bin; \
	  $(loc_MAKE) advertise-bin; \
	fi

install-doc:  doc
	+$(bdoc_MAKE) install
	@if test x$(installation_blurb) = xno; then : OK; else \
	  $(loc_MAKE) history-doc; \
	  $(loc_MAKE) advertise-doc; \
	fi


# "make uninstall" removes the installed documentation and executables,
# provided the configuration sufficiently resembles that of the
# original install.

uninstall:
	+@$(loc_MAKE) installation_blurb=no uninstall-doc
	+@$(loc_MAKE) installation_blurb=no uninstall-bin
	@$(loc_MAKE) history-doc
	@$(loc_MAKE) history-bin

uninstall-bin:  $(bsrc)/Makefile
	+$(bsrc_MAKE) uninstall
	@test x$(installation_blurb) = xno || $(loc_MAKE) history-bin

uninstall-doc:  $(bdoc)/Makefile
	+$(bdoc_MAKE) uninstall
	@test x$(installation_blurb) = xno || $(loc_MAKE) history-doc


# For a sort of benign symmetry with "make history":

advertise:
	@$(loc_MAKE) advertise-bin
	@$(loc_MAKE) advertise-doc

advertise-bin:
	@$(pt) ''
	@$(pt) " `date` - GNU SETL executables have been installed."
	@$(pt) ''

advertise-doc:
	@$(pt) ''
	@$(pt) " `date` - GNU SETL documentation has been installed."
	@$(pt) ''


# "make history" cites the tails of the src and doc installation logs:

history:
	@$(loc_MAKE) history-bin
	@$(loc_MAKE) history-doc

history-bin:
	@$(pt) ''
	@$(pt) ' Latest executable installation history:'
	@$(pt) ''
	tail -3 $(bsrc)/install.log
	@$(pt) ''

history-doc:
	@$(pt) ''
	@$(pt) ' Latest documentation installation history:'
	@$(pt) ''
	tail -12 $(bdoc)/install.log
	@$(pt) ''


# "make clean" removes generated executables, object files, and
# libraries, but lets configuration and some other data remain.
#
# "Clean" implies clean-src and clean-gmp; and for a VPATH-type build,
# clean-doc.  But if you are building directly into the source tree,
# clean does not imply clean-doc:  you have to go "make clean-doc" if
# you really want to expunge the .info, .html, and .pdf files.
#
# This special-case treatment is because those files are included in
# distributions.  Regenerating them requires Texinfo and TeX support,
# which is rather heavy apparatus that not every user is likely to
# have installed.
#
# In the VPATH-type case, a clean-doc (explicit or implied) only
# removes files from the build tree, leaving the distributed .info,
# .html, and .pdf files untouched.

clean:
	@$(loc_MAKE) clean-src
	@$(loc_MAKE) clean-gmp
ifneq ($(sdir),$(bdir))
	# Good, a VPATH-type build.
	@$(loc_MAKE) clean-doc
else
	@$(pt) ''
	@$(pt) ' Note that "clean" only made "clean-src".'
	@$(pt) ''
	@$(pt) ' You must $(MAKE) "clean-doc" separately, because'
	@$(pt) ' you have chosen to build in the source tree, and'
	@$(pt) ' "clean-doc" clobbers files there that require'
	@$(pt) ' specialized tools such as Texinfo to regenerate.'
	@$(pt) ''
endif

# In the *clean-src and *clean-doc rules below, we may be creating
# Makefiles only to delete them again at levels of cleanliness higher
# than plain "clean".  But that seems less onerous than maintaining
# separate lists up here of all the things that could be created in
# subdirs, grouped by what cleanliness levels should destroy them.
# Doing it this way, we get the benefit of the cleaning rules (clean,
# distclean, etc.) that are in the generated Makefiles.

clean-src:  $(all_src_Makefiles)
	$(bsrc_MAKE) clean

clean-doc:  $(all_doc_Makefiles)
	rm -f $(bdir)/web-doc.tar
	rm -f $(bdoc)/index.html
	$(bdoc_MAKE) clean


# "make distclean" cleans everything in src back to the state it was in
# when the distributed package was first unpacked (unless you have
# touched a distributed file, of course).
#
# Analogously with the "clean" rule (please see the comments above it),
# distclean implies distclean-src and distclean-gmp always, but only
# implies distclean-doc for a VPATH-type build (where the build tree is
# separate from the source tree).

distclean:
	@$(loc_MAKE) distclean-src
	@$(loc_MAKE) distclean-gmp
ifneq ($(sdir),$(bdir))
	# Good, a VPATH-type build.
	@$(loc_MAKE) distclean-doc
else
	@$(pt) ''
	@$(pt) ' Note that "distclean" only made "distclean-src".'
	@$(pt) ''
	@$(pt) ' You must $(MAKE) "distclean-doc" separately, because'
	@$(pt) ' you have chosen to build in the source tree, and'
	@$(pt) ' "distclean-doc" clobbers files there that require'
	@$(pt) ' specialized tools such as Texinfo to regenerate.'
	@$(pt) ''
endif
# Clean out things that might have been created by dist or distcheck:
	@$(loc_MAKE) clean-dist
# This goes after operations like distclean-src (above) so that
# needed Makefiles will not appear out of date with the config and
# cause the just-deleted config parms to be requested again, etc.:
	@$(loc_MAKE) deconfig

distclean-src:  $(all_src_Makefiles)
	$(bsrc_MAKE) distclean

distclean-doc:  $(all_doc_Makefiles)
	$(bdoc_MAKE) distclean


# At maintainer-clean and cleaner levels, doc is cleaned
# unconditionally along with src.  (At lower levels of cleanliness,
# doc is only cleaned automatically if the build dir is different
# from the source dir (a VPATH-type build), as detailed above.)


# "make maintainer-clean" gives Dr. Bacon a well-deserved bl

maintainer-clean:
	@$(loc_MAKE) maintainer-clean-src
	@$(loc_MAKE) distclean-gmp
	@$(loc_MAKE) maintainer-clean-doc
# Clean out things that might have been created by dist or distcheck:
	@$(loc_MAKE) clean-dist
# This goes last for similar reasons as in distclean:
	@$(loc_MAKE) deconfig

maintainer-clean-src:  $(all_src_Makefiles)
	$(bsrc_MAKE) maintainer-clean

maintainer-clean-doc:  $(all_doc_Makefiles)
	$(bdoc_MAKE) maintainer-clean


# "make superclean" destroys everything that can be regenerated,
# except for $(sdoc)/texinfo.tex, which we'd rather not lose,
# though it should be carefully updated from time to time from
# public sources (see the "texinfo-patch" rule below for more on
# that subject).

superclean:
	$(loc_MAKE) maintainer-clean
	-rm -f $(sdir)/stamp-auto
	-rm -f $(autoreconf_outputs)
	-rm -rf `$(FIND) $(sdir) -type d -name autom4te.cache`
	-rm -f $(old_spam_creations)
	-rm -f $(ssrc)/version.m4
	-rm -f $(sdoc)/version.m4
	-rm -f $(sdoc)/texinfo.tex~
# Clean out even these distributed files if present:
	-rm -f $(sdoc)/setl*.html $(sdoc)/setl*.info $(sdoc)/setl*.pdf


# "make distcheck" makes a "distribution" from $(ssrc) and from $(sdoc).
# For each, it unpacks it, configures it, builds it, checks it, does a
# test installation of it, uninstalls that, and distcleans the package
# (doing distuninstallcheck and distcleancheck steps on the way by).
# Finally, it removes the resulting .tar.gz file, because in the "dist"
# rule, that archive file is just an intermediary in creating the full
# distribution that contains both src and doc.
#
# The use of "make distcheck" is recommended at any time, as an easy
# end-to-end check that the GNU SETL package (as freshly unpacked from
# the distribution, for example) builds sanely, runs a miscellaneous
# little bunch of regression tests correctly, and reproduces itself
# as a new distribution package faithfully.

distcheck:  distcheck-src distcheck-doc
# If we get here, the GNU-style distchecks in distcheck-{src,doc}
# succeeded, and there is nothing left to do but clean up after them:
	@$(loc_MAKE) clean-dist
	@$(pt) ''
	@$(pt) 'The "$(MAKE) distcheck" appears to have been successful.'
	@$(pt) 'Use "$(MAKE) dist" to make a GNU SETL distribution archive.'
	@$(pt) ''

# First phase of a distcheck, which you can use directly if you like;
# note that it does not attempt to clear out whatever the distcheck in
# the src dir leaves behind (unlike the full distcheck at this level,
# which does):
distcheck-src:  $(all_src_Makefiles)
	# Make sure the bundled GMP lib is available in case needed:
	$(loc_MAKE) $(gmplib)
	# Do the GNU-style distcheck in src, suppressing the use of any
	# customization packages that might currently be configured:
# Could use GMP='$(abspath $(bgmp))' in GNU Make 3.81+:
	$(bsrc_MAKE) GMP=`(cd $(bgmp); pwd)` distcheck CUSTOM_PAX=''
# Even though we populated $(sgmp) and $(bgmp) by making $(gmplib)
# above, we leave the cleaning and removal to the clean and distclean
# rules, as those dirs and lib could just as easily have been built by
# the bin rule and it doesn't seem worth distinguishing that case by
# tracking here who made what.  We could clobber $(sgmp) and $(bgmp)
# unconditionally, of course, but that seems unhelpful given that they
# are easy to clean and remove with clean-gmp and distclean-gmp, and
# are cleaned/removed automatically with plain clean and distclean.

# Second phase of a distcheck:
distcheck-doc:  $(all_doc_Makefiles)
	# Do the GNU-style distcheck in doc:
	$(bdoc_MAKE) distcheck


# "make dist" makes a distribution tarball (compressed tar archive).
#
# It does a "make dist" in $(bsrc) and $(bdoc) and then unpacks the
# result under $(dist).  The $(EXTRA_DIST) files are then also copied
# into $(dist), and the whole schmozzle is tarballed as $(dist_tgz).
#
# By default $(dist) is the top build dir, but you can override that
# on the command line if you have some strange need to, like this:
#
#  make dist=<yourdistdir> dist

dist:  $(ask_yes) $(all_src_Makefiles) $(all_doc_Makefiles)
	# Do the GNU-style dist, creating $(src_tgz):
	$(bsrc_MAKE) dist CUSTOM_PAX=''
	# GNU-style, create $(doc_tgz):
	$(bdoc_MAKE) dist
	# Make sure we have a writable $(dist_pkg) directory:
	@if test -d $(dist_pkg); then \
	  $(prt) '*** "$(abs_dist_pkg)/" already exists:'; \
	  $(prt) "`ls -ldF $(dist_pkg)`"; \
	  if $(ask_yes) 'May I remove it?'; then \
	    $(call rm_rf,$(dist_pkg)); \
	    $(prt) '"$(abs_dist_pkg)/" removed.'; \
	  else \
	    $(prt) '"$(abs_dist_pkg)/" NOT removed.  Dist aborted, but'; \
	    $(prt) '$(src_tgz) and $(doc_tgz) remain.'; \
	    exit 1; \
	  fi; \
	fi
	$(MKDIR_P) $(dist_pkg)
	# Unroll $(src_tgz) in $(dist_pkg)/, thereby
	# populating $(dist_pkg)/$(pkg)/, and then move that
	# to $(dist_pkg)/src/:
	$(GZCAT) $(src_tgz) | (cd $(dist_pkg) && $(TAR) xf -)
	cd $(dist_pkg) && mv $(pkg) src
	rm $(src_tgz)
	# Similarly get $(dist_pkg)/doc/ from $(doc_tgz):
	$(GZCAT) $(doc_tgz) | (cd $(dist_pkg) && $(TAR) xf -)
	cd $(dist_pkg) && mv $(pkg) doc
	rm $(doc_tgz)
	# Copy extra files from $(sdir) into $(dist_pkg):
	(cd $(sdir) && $(TAR) cf - $(EXTRA_DIST)) | \
	 (cd $(dist_pkg) && $(TAR) xf -)
	@if test -f $(dist_tgz); then \
	  $(prt) 'Old "$(abs_dist_tgz)" found:'; \
	  $(prt) "`ls -l $(dist_tgz)`"; \
	  if $(ask_yes) 'Clobber it?'; then \
	    rm -f $(dist_tgz); \
	  else \
	    $(prt) '"$(abs_dist_tgz)" NOT clobbered;'; \
	    $(prt) '"$(abs_dist_pkg)/" contains unpacked distribution.'; \
	    exit 1; \
	  fi; \
	fi
	# Prepare the final tarball, $(dist_tgz):
	cd $(dist) && $(TAR) cf - $(pkg) | $(GZIP_HARD) >$(pkg_tgz)
	$(call rm_rf,$(dist_pkg))
	# Distribution ready in $(abs_dist_tgz):
	@$(prt) "`ls -l $(dist_tgz)`"


# "make clean-dist" (not to be confused with distclean) removes anything
# created by a "make dist" or "make distclean" (even an interrupted one),
# including any transient intermediate dirs and archives.

clean-dist:
	# Clean up any "dist"-prepared archive and associated detritus:
	$(call rm_archive,$(dist_pkg))
	# Clean up anything left behind by a "distcheck":
	$(call rm_archive,$(src_pkg))
	$(call rm_archive,$(doc_pkg))


# "make preopt" gets the optional customization packages ready for
# distribution or use.  This rule is just for convenience in case
# you want to want to generate (or update) all the custom.* files
# without doing anything else; normally those files are generated
# only for the packages you have selected.
#
# Note that some customization packages (such as mesa) can only be
# prepped in this way (endowed with custom.* files) if you already
# have a 'setl' command available to help with the preprocessing:

preopt:  $(opt_files)

$(opt_files):  $(bopt_Makefiles)
	$(bopt_MAKE)


# "make setup" obtains config parms and stores them in $(config_file).

setup:  $(ssrc)/configure $(ask_yes)
	@parms=`cat $(config_file) 2>/dev/null`; \
	if test -n "$$parms"; then \
	  $(prt) '$(config_file) contains:'; \
	  cat $(config_file); \
	  if ! $(ask_yes) 'Update it?'; then \
	    $(prt) '$(config_file) not changed.'; \
	    exit 1; \
	  fi; \
	else \
	  $(prt) 'Creating $(config_file)...'; \
	fi
	rm -f $(parms_stamp)
	$(loc_MAKE) $(config_file)
reconfig:  setup


# "make upset" is just a careful way of removing the config parms file
# and witness, causing the next "make build" to do a "make setup" to
# regenerate it.  Probably not all that useful except perhaps in some
# unwritten script that likes the prompt-before-delete.  Note that
# "make setup" similarly begins by citing the config parms in the file
# and then prompting before blowing it away.

upset:  $(ask_yes)
	@parms=`cat $(config_file) 2>/dev/null`; \
	if test -n "$$parms"; then \
	  $(prt) '$(config_file) contains:'; \
	  cat $(config_file); \
	  if $(ask_yes) 'Destroy it?'; then \
	    $(loc_MAKE) deconfig; \
	    $(prt) '$(config_file) and $(parms_stamp) removed.'; \
	  else \
	    $(prt) '$(config_file) not removed (nor $(parms_stamp)).'; \
	    exit 1; \
	  fi; \
	else \
	  : The config parms file is empty or nonexistent; \
	  : make sure it and the witness are gone; \
	  $(loc_MAKE) deconfig; \
	fi


# A less careful way of removing the config parms file and its witness.

deconfig:
	-rm -f $(parms_stamp)
	-rm -f $(config_file)


# Get config parms directly from the envt var configure_args if it
# is non-null, else indirectly from the file named by the envt var
# default_config_file if any, else painfully from the user.
#
# That delicate bit of shellage is in the $(setup_parms) script.
#
# Store those config parms in $(config_file) (e.g. config.parms) if
# new or changed, and witness with $(parms_stamp) (e.g. stamp-parms):
#
$(config_file):  $(parms_stamp)
	@$(recover_removed_target)
$(parms_stamp):  $(setup_parms) $(ssrc)/configure
	@-$(MKDIR_P) $(dir $(config_file))
	@if test -f $@; then \
	  $(prt) '$@ is out of date with $? - must update $(config_file):'; \
	fi
# The $(setup_parms) script prompts on stderr, but for consistency
# with how mkbuild is used, we redirect that output to stdout here:
	$(setup_parms) $(config_file) $(sopt) 2>&1
	rm -f $@
	$(prt) "$(config_file) witnessed at `date`" >$@


# "make preconfig" gets everything (such as "configure" scripts) ready
# for distribution, as witnessed by $(sdir)/stamp-auto.

preconfig:  $(autoreconf_outputs)
	@$(pt) ''
	@$(pt) ' To regenerate configure scripts and .in files,'
	@$(pt) ' you can $(MAKE) force-preconfig.'
	@$(pt) ''


# "make force-preconfig" can be useful after a change to one of
# autoreconf's untracked dependencies (such as host system auto-tool
# reconfig), or after an unintended direct change to one of its outputs
# (which would not have been reflected in an update to the witness).

force-preconfig:
	# Remove old preconfig witness:
	rm -f $(sdir)/stamp-auto
	# Do a normal preconfig:
	$(loc_MAKE) preconfig


# Among the $(standard_creations) and $(generated_autobits) produced
# by autoreconf, most will only be rewritten if their contents would
# change, so we use stamp-auto to witness their up-to-dateness, and a
# null rule to let clients depend selectively on $(autoreconf_outputs).
#
# We didn't actually need to squeeze both src and doc through the same
# tube by having them share stamp-auto like this, but the bit of extra
# simplicity is worth the occasional redundant autoreconfing.

$(autoreconf_outputs) $(sdoc)/texinfo.tex:  $(sdir)/stamp-auto
	@$(recover_removed_target)
$(sdir)/stamp-auto:  $(autoreconf_inputs)
	@if test -f $@; then \
	  $(prt) '$@ is out of date with $? - must autoreconf:'; \
	fi
# The autoreconf in $(ssrc) recurses into $(sfgt) and $(sopt) too:
	cd $(ssrc) && $(AUTORECONF) --install --verbose
	cd $(sdoc) && $(AUTORECONF) --install --verbose
	# If texinfo.tex has not already been patched with our special
	# little hacks, do so now.  Using texinfo-patch is a bit heavy
	# for the purpose since we're only interested in texinfo.tex
	# here, not the patchfile, but it should be rarely invoked.
	# We don't even insist it succeed; if something really bad
	# happens you could conceivably have to repair or revert
	# $(sdoc)/texinfo.tex.
	-$(GREP) -q 'naIII' $(sdoc)/texinfo.tex || \
	 $(loc_MAKE) texinfo-patch
	rm -f $@
	$(prt) '"$(AUTORECONF) --install" outputs witnessed at '"`date`" >$@


# "make texinfo-patch" is for use when you suspect that the
# doc/texinfo.tex file is out of date or at any rate not compatible
# with your current Texinfo toolset.  Here's how to use this rule:
#
# First move doc/texinfo.tex to texinfo.tex.old for backup.
#
# Then establish the new texinfo.tex thus:
#
#  - by a "make force-preconfig" to fetch the latest version provided
#    by your local autoreconf, in which case texinfo.tex ends up
#    automatically being patched by "make texinfo-patch"; or
#
#  - by browsing to http://www.gnu.org/software/texinfo/ and chasing
#    a link or two to get the latest "official" texinfo.tex into
#    doc/texinfo.tex (of course any other reliable source from the
#    Internet which never lies would be fine too), in which case you
#    have to "make texinfo-patch" yourself after fetching the new
#    base version in order to apply the patch.
#
# "make texinfo-patch" first tries applying the existing patch to
# doc/texinfo.tex, leaving the original unpatched version in
# doc/texinfo.tex.orig.  If doc/texinfo.tex was already patched, the
# patch is used in reverse to obtain doc/texinfo.tex.orig from it.
#
# Given those two versions of texinfo.tex, this rule (courtesy of the
# make-texinfo-patch script) then takes the diff between them as the
# new patch and presents that to the user for approval.
#
# Besides being oh so user friendly, this rule is also invoked
# automatically by the recipe above (indirectly called from
# force-preconfig) when the existing doc/texinfo.tex appears not to
# have been patched.
#
# It is boring but worth noting that the make-texinfo-patch script
# called by "make texinfo-patch" freely clobbers anything in doc/ named:
#
#  texinfo.tex            - an intended product of the script
#  texinfo.tex.patch      - the main product
#  texinfo.tex.orig       - a consistency-check byproduct
#  texinfo.tex.patch.new  - a user-interaction temporary
#  texinfo.tex.patch.bad  - where a user-rejected "new" patch goes
#
# New feature: since it is sort of rude of me to clobber your
# texinfo.tex.orig file that you wisely left around as the old
# committee version before you hacked texinfo.tex, and you might
# now wish to merge diffs first for your hacks and then for theirs,
# "make texinfo-patch" now behaves more simply and sanely when
# both texinfo.tex.orig and texinfo.tex already exist, and offers
# their context diff as the patch.  That merges your changes in.
# If that is satisfactory, you can then add their changes by moving
# texinfo.tex.orig aside, replacing texinfo.tex with their new
# version, and running "make texinfo-patch" a second time.  It begins
# by moving texinfo.tex to texinfo.tex.orig and applying the patch
# to generate a new texinfo.tex.  It may need to use some fuzz,
# or it could even fail, in which case you may have to complete the
# updating of texinfo.tex more manually.  Once that succeeds, this
# second "make texinfo-patch" goes on to offer an updated patch as
# the one applicable to the new vanilla version.

texinfo-patch:
	loc=$(sdir) doc=$(sdoc) ask_yes=$(ask_yes) make=$(MAKE) \
	 $(sdir)/make-texinfo-patch


# What you need to update "manually" on each new release is the file
# VERSION_NUMBER, from which $(ssrc)/version.m4 and $(sdoc)/version.m4
# are derived.

$(ssrc)/version.m4:  $(sdir)/VERSION_NUMBER
	$(prt) "m4_define([VERSION_NUMBER], [$(VERSION_NUMBER)])" >$@

$(sdoc)/version.m4:  $(sdir)/VERSION_NUMBER
	$(prt) "m4_define([VERSION_NUMBER], [$(VERSION_NUMBER)])" >$@


# "make config" creates Makefiles, guided by config parms and
# witnessed by $(config_src_stamp) and $(config_doc_stamp).

config:  config-src config-doc
	@$(pt) ''
	@$(pt) ' To regenerate Makefile and config.h files,'
	@$(pt) ' you can $(MAKE) force-config.'
	@$(pt) ''
	@$(pt) ' To change config parms and reconfigure,'
	@$(pt) ' you can $(MAKE) setup.'
	@$(pt) ''
config-src:  $(all_src_Makefiles)
config-doc:  $(all_doc_Makefiles)


# "make force-config" (updates and) (re)runs config.status scripts,
# thus creating Makefiles and updating (if changed) config.h files.

force-config:  force-config-src force-config-doc
force-config-src:
	-rm $(config_src_stamp)
	$(loc_MAKE) config-src
force-config-doc:
	-rm $(config_doc_stamp)
	$(loc_MAKE) config-doc


# Common recipe code for rules that create makefiles using config.status
# scripts:
define make_makefiles
	@if test -f $@; then \
	  $(prt) '$@ is out of date with $? -'; \
	  $(prt) "Re-creating and running config.status scripts:"; \
	fi; \
	$(foreach d,$(1),$(call do_make_makefiles,$(d))) \
	rm -f $@;
endef

# Helper for the above:
define do_make_makefiles
	$(call cmd,$(loc_MAKE) $(1)/config.status) && \
	$(call cmd,(cd $(1) && ./config.status));
endef

# $(config_src_stamp) witnesses the creation of all the Makefiles under
# $(bsrc) by config.status scripts.  $(config_doc_stamp) is analogous.
# Note that those scripts also create config.h files where needed, and
# update them when they need to change.  (Makefiles are updated
# unconditionally.)

$(all_src_Makefiles):  $(config_src_stamp)
	@$(recover_removed_target)
$(config_src_stamp):  $(config_file) $(autoreconf_outputs)
# Incredible as it may seem, this rule was getting triggered in
# GNU Make 3.81 even when '$?' was empty.  Turned out this is a
# cherished bug (#16051), tickled when $(autoreconf_outputs)
# contained names of files that were no longer being created by
# autoreconf.  The GNU Make bug is that '$?' only contains the
# names of out-of-date prerequisites that actually exist,
# contrary to documentation, common sense, and convenience.  So,
# why should you care about this history?  Well, if you ever see
# the meaningless message "... out of date with  -" (out of date
# with nothing), which will occur in conjunction with massive
# unnecessary recompilation, it means that $(autoreconf_outputs)
# needs revising again while the GNU Make bug is still being
# cherished (ostensibly to avoid breaking someone's much more
# important build system).  If that happens, I suggest temporarily
# uncommenting this "ls" command to find out which prerequisites
# are failing to be created - it will name names and error you out,
# so that you can remove the retirees from autoreconf_outputs, try
# again, and bless the GNU Make bug for helping you tidy up that list:
#	ls -alF $^
	$(call make_makefiles,$(src_config_status_dirs))
	$(prt) "$(all_src_Makefiles) witnessed at `date`" >$@


# stamp-config-doc witnesses the creation/updating of the
# $(bdoc) Makefiles by a config.status script.

$(all_doc_Makefiles):  $(config_doc_stamp)
	@$(recover_removed_target)
$(config_doc_stamp):  $(config_file) $(autoreconf_outputs)
# See discussion above about when to uncomment this ls:
#	ls -alF $^
	$(call make_makefiles,$(doc_config_status_dirs))
	$(prt) "$(all_doc_Makefiles) witnessed at `date`" >$@


# This macro creates or updates $(1)/config.status; --no-create
# only suppresses writing of the other files besides config.status.
#
# We want the sensitivity to $(config_file) changes when updating
# a config.status file, so we do a full configure in this macro
# instead of just a config.status --recheck.
#
# A default --build arg is passed to the configure script to encourage
# it to decide early whether it is setting up for cross-compiling.
# That default is the output of src/config.guess.  If for some
# perverse reason the user really wants the script's cross_compiling
# variable to be set to "maybe" in the early stages, which is the
# normal default for a GNU configure script when --host is specified
# and --build is not (though that strange default is expected to
# change in the future), then passing a --host arg and an explicitly
# null --build arg (as in "build=") is how to get that.
#
define create_config_status
@test -d $(1) || $(call cmd,$(MKDIR_P) $(1))
@parms=`cat $(config_file) 2>/dev/null`; \
default_build=`$(ssrc)/config.guess`; \
: Cannot use the cmd macro here or $$parms would be unquoted:; \
config_cmd="$(2)/configure --no-recursion --no-create \
   --srcdir=$(2) --build=$$default_build $$parms $(3)"; \
$(prt) "cd $(1) && eval $$config_cmd"; \
        cd $(1) && eval "$$config_cmd"
endef

$(bsrc)/config.status:  $(ssrc)/configure $(config_file) $(ssrc)/config.guess
	$(call create_config_status,$(bsrc),$(bsrc_to_ssrc), \
	                            --enable-option-checking)

# We --disable-option-checking in the rest of these because they do not
# grok the --with-* args produced by customization.  This is similar to
# what GNU-type configure scripts do when calling other configure
# scripts recursively.

$(bfgt)/config.status:  $(sfgt)/configure $(config_file) $(ssrc)/config.guess
	$(call create_config_status,$(bfgt),$(bfgt_to_sfgt), \
	                            --disable-option-checking)

$(bopt)/config.status:  $(sopt)/configure $(config_file) $(ssrc)/config.guess
	$(call create_config_status,$(bopt),$(bopt_to_sopt), \
	                            --disable-option-checking)

$(bdoc)/config.status:  $(sdoc)/configure $(config_file) $(ssrc)/config.guess
	$(call create_config_status,$(bdoc),$(bdoc_to_sdoc), \
	                            --disable-option-checking)


# The GMP lib depends on much more than its main Makefile, so this is
# just to trigger a build from fresh; you aren't meant to modify GMP:
$(gmplib):  $(bgmp)/Makefile
	$(bgmp_MAKE)

# Attempt a clean in $(bgmp) if it exists.  Forgive all failures, and
# don't insist on creating a Makefile just to run the clean rule:
clean-gmp:
	-test -d $(bgmp) && $(bgmp_MAKE) clean

# distclean-gmp removes the GMP build and source dirs, as they are not
# distributed (only the GMP source tarball is):
distclean-gmp:
	$(call rm_rf,$(bgmp))
	$(call rm_rf,$(sgmp))

$(bgmp)/Makefile:  $(bgmp)/config.status
	# Run config.status in $(bgmp) -> config.h and Makefiles:
	cd $(bgmp) && ./config.status

$(bgmp)/config.status:  $(sgmp)/configure $(config_file) $(ssrc)/config.guess
	$(call create_config_status,$(bgmp),$(bgmp_to_sgmp), \
	                            --disable-option-checking)

# This rule creates a configure script by unpacking it from the vanilla
# GMP distribution.  We generate other configure scripts when needed,
# using Autoconf (see autoreconf_outputs), but do not presume to do that
# for unmodified gmp archives:
$(sgmp)/configure:
	# Unroll $(gmp_tgz) into $(gmp_dir):
	$(GZCAT) $(gmp_tgz) | $(TAR) xf -
	# Remove old $(sgmp) if any and rename $(gmp_dir) as $(sgmp):
	$(call rm_rf,$(sgmp))
	mv $(gmp_dir) $(sgmp)


.PHONY:  \
  no-args help \
  all bin doc \
  web-doc \
  preopt \
  preconfig    force-preconfig \
  texinfo-patch \
  config       force-config \
    config-src   force-config-src \
    config-doc   force-config-doc \
  setup reconfig \
  upset deconfig \
  check \
    check-src \
    check-doc \
  install \
    install-bin \
    install-doc \
  uninstall \
    uninstall-bin \
    uninstall-doc \
  advertise \
    advertise-bin \
    advertise-doc \
  history \
    history-bin \
    history-doc \
  distcheck \
    distcheck-src \
    distcheck-doc \
  dist \
  clean-dist \
  clean \
    clean-src \
    clean-doc \
    clean-gmp \
  distclean \
    distclean-src \
    distclean-doc \
    distclean-gmp \
  maintainer-clean \
    maintainer-clean-src \
    maintainer-clean-doc \
  superclean

.DELETE_ON_ERROR:  # tell GNU make to rm possibly incomplete targets
