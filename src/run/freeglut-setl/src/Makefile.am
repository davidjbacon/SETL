## Process this file with automake to produce Makefile.im

# $Id: Makefile.am,v 1.8 2017/09/22 07:58:16 setlorg Exp $

noinst_LTLIBRARIES = libglut.la

libglut_la_SOURCES = freeglut_internal.h \
			 freeglut_callbacks.c \
			 freeglut_cursor.c \
			 freeglut_display.c \
			 freeglut_ext.c \
			 freeglut_font.c \
			 freeglut_glutfont_definitions.c \
			 freeglut_font_data.c \
			 freeglut_stroke_roman.c \
			 freeglut_stroke_mono_roman.c \
			 freeglut_gamemode.c \
			 freeglut_geometry.c \
			 freeglut_init.c \
			 freeglut_joystick.c \
			 freeglut_main.c \
			 freeglut_menu.c \
			 freeglut_misc.c \
			 freeglut_overlay.c \
			 freeglut_state.c \
			 freeglut_structure.c \
			 freeglut_teapot.c \
			 freeglut_videoresize.c \
			 freeglut_window.c


# These were in the original freeglut-2.0.1 Makefile.am:

#libglut_la_LIBADD = $(LIBM) $(X_LIBS) -lGL -lGLU -lXext -lX11 $(LIBXXF86VM)
#libglut_la_LDFLAGS = -version-info 11:0:8
#libglut_la_CFLAGS = $(X_CFLAGS)


# New defs:

# No -I$(top_srcdir)/include needed, as the sources here all refer
# explicitly to "../include/GL/freeglut.h" when they need it:
libglut_la_CPPFLAGS = $(AM_CFLAGS) $(X_CFLAGS)

# Our only important product is the static convenience library for the
# GNU SETL interpreter to link to, but we go ahead and build a shared
# lib too since it's actually a general-purpose lib (and the local demos
# stay quite tiny by linking to that lib):
libglut_la_LDFLAGS = $(AM_LDFLAGS) \
                     $(X_LIBS) \
                     -rpath $(abs_builddir)/.libs \
                     -version-info 11:0:8

# This ordering in libglut_la_LIBADD agrees with what is in the def of
# OPT_MESA_LIBS in the relevant Makefile.am in GNU SETL:
libglut_la_LIBADD = -lGLU -lGL $(X_PRE_LIBS) -lXext -lXt -lX11 $(LIBXXF86VM) $(X_EXTRA_LIBS)


## Makefile.am ends here
