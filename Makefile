# $Id: Makefile,v 1.248 2018/01/20 04:42:04 setlorg Exp $
#
# Free software (c) dB - see file COPYING for license (GPL).
#
# Building GNU SETL from source requires GNU Make, which looks for
# GNUmakefile before it looks for Makefile.  If your make command
# sees this Makefile by default, it is presumed not to support the
# GNU Make extensions.
#
all .DEFAULT:
	@if echo 'all:' | gmake -f - >/dev/null 2>&1; then \
	  echo "gmake $*"; gmake $*; \
	else \
	  echo 'Please install GNU Make as gmake (see INSTALL) and try again.'; \
	fi
