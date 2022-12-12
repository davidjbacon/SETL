/* I think this code was mostly written by David Fox, ca. 1994. */

#include "config.h"

#include "W.h"

#include <assert.h>

#ifdef HAVE_MALLOC_H
#include <malloc.h>
#endif

#include <stdlib.h>

#if HAVE_UNISTD_H
#include <sys/types.h>
#include <unistd.h>
#endif

#ifdef TIME_WITH_SYS_TIME
#include <sys/time.h>
#include <time.h>
#else
#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#else
#include <time.h>
#endif
#endif

#if HAVE_SYS_SELECT_H
#include <sys/select.h>
#endif

#include <X11/Xlib.h>
#include <X11/Xutil.h>

typedef int boolean;
#define true 1
#define false 0

typedef struct {
  Display *display_;
  int screen_;
  Window window_;
  Drawable drawbuffer_;	/* For double buffering */
  Drawable stipple_;
  unsigned long foreground_;
  unsigned long background_;
  GC gc_;			/* Current gc */
  GC fg_;
  GC bg_;
  GC grayfg_;
  GC graybg_;
  XPoint *points_;  /* was XPoint points_[256]; now malloc/reallocing */
  int point_count_;
  int point_max_;
  KeySym key_;
  int width_;
  int height_;
  boolean dirty_;
  boolean opaque_;
  unsigned long color_;
} WWindow;

static WWindow *WWindow__WWindow(int w, int h);
#if 0
static void WWindow__delete_WWindow(WWindow *);
#endif

static void WWindow__box(WWindow *, long ax, long ay, long bx, long by);
static void WWindow__vector(WWindow *, long ax, long ay, long bx, long by);
static void WWindow__color(WWindow *, unsigned long color);
static void WWindow__opacity(WWindow *, long opacity);
static void WWindow__sync(WWindow *);
static void WWindow__event(WWindow *, long *key, long *x, long *y, long *z);

static void WWindow__new_path(WWindow *);
static void WWindow__add_vertex(WWindow *, int x, int y);
static void WWindow__polygon(WWindow *);

static XEvent *WWindow__NextEvent(WWindow *, int *x, int *y,
				  unsigned int *state);
static Display *WWindow__Pending(WWindow *);
static void WWindow__Read(WWindow *, XEvent*, int *x, int *y,
			  unsigned int *state);

static WWindow *default_window = 0;

/* INITIALIZATION */
void W_init(long width, long height) {
  default_window = WWindow__WWindow(width, height);}

/* GET THE NEXT INPUT EVENT */
void W_event(long *k, long *x, long *y, long *z) {
  WWindow__event(default_window, k, x, y, z);}

/* DRAW BOX ONTO INTERNAL IMAGE */
void W_box(long ax, long ay, long bx, long by) {
  WWindow__box(default_window, ax, ay, bx, by);}

/* DRAW VECTOR ONTO INTERNAL IMAGE */
void W_vector(long ax, long ay, long bx, long by) {
  WWindow__vector(default_window, ax, ay, bx, by);}

void W_new_path(void) {
  WWindow__new_path(default_window);}
void W_add_vertex(int x, int y) {
  WWindow__add_vertex(default_window, x, y);}
void W_polygon(void) {
  WWindow__polygon(default_window);}

/* SET THE DRAWING COLOR */
void W_color(unsigned long color) {
  WWindow__color(default_window, color);}

/* SET THE DRAWING OPACITY */
void W_opacity(long opacity) {
  WWindow__opacity(default_window, opacity);}

/* UPDATE SCREEN FROM INTERNAL IMAGE */
void W_sync(void) {
  WWindow__sync(default_window);}


/* Avoid spurious warnings that will arise if we compile with
 * GCC's -Wc++-compat flag:  */
#define this that

static WWindow *WWindow__WWindow(int w, int h)
{
  XEvent *event;
  XSizeHints hint;
  WWindow *this = (WWindow*)malloc(sizeof(WWindow));
  GC tmp_bg;			/* For drawing on stipple_, 1 plane deep */
  GC tmp_fg;			/* For drawing on stipple_, 1 plane deep */
  
  this->dirty_ = true;
  this->opaque_ = true;

  this->display_ = XOpenDisplay("");
  this->screen_ = DefaultScreen(this->display_);
  this->background_ = WhitePixel(this->display_, this->screen_);
  this->foreground_ = BlackPixel(this->display_, this->screen_);
  this->color_ = this->foreground_;
  hint.width = w; hint.height = h;
  hint.flags = PSize;
  this->window_ = XCreateSimpleWindow(this->display_,
				      DefaultRootWindow(this->display_),
				      hint.x, hint.y, hint.width, hint.height,
				      5, this->foreground_, this->background_);
  XSetStandardProperties(this->display_, this->window_, "W", "W",
			 None, 0, 0, &hint);

  this->gc_ = this->fg_ = XCreateGC(this->display_, this->window_, 0, 0);
  XSetBackground(this->display_, this->fg_, this->background_);
  XSetForeground(this->display_, this->fg_, this->foreground_);

  this->bg_ = XCreateGC(this->display_, this->window_, 0, 0);
  XSetBackground(this->display_, this->bg_, this->foreground_);
  XSetForeground(this->display_, this->bg_, this->background_);

  this->stipple_ = XCreatePixmap(this->display_,this->window_, 2, 2, 1);
  tmp_fg = XCreateGC(this->display_, this->stipple_, 0, 0);
  XSetBackground(this->display_, tmp_fg, this->background_);
  XSetForeground(this->display_, tmp_fg, this->foreground_);
  tmp_bg = XCreateGC(this->display_, this->stipple_, 0, 0);
  XSetBackground(this->display_, tmp_bg, this->foreground_);
  XSetForeground(this->display_, tmp_bg, this->background_);
  
  XDrawPoint(this->display_, this->stipple_, tmp_fg, 0, 0);
  XDrawPoint(this->display_, this->stipple_, tmp_fg, 1, 1);
  XDrawPoint(this->display_, this->stipple_, tmp_bg, 0, 1);
  XDrawPoint(this->display_, this->stipple_, tmp_bg, 1, 0);
  this->grayfg_ = XCreateGC(this->display_, this->window_, 0, 0);
  this->graybg_ = XCreateGC(this->display_, this->window_, 0, 0);
  XSetBackground(this->display_, this->grayfg_, this->background_);
  XSetForeground(this->display_, this->grayfg_, this->foreground_);
  XSetBackground(this->display_, this->graybg_, this->foreground_);
  XSetForeground(this->display_, this->graybg_, this->background_);
  XSetFillStyle(this->display_, this->grayfg_, FillStippled);
  XSetStipple(this->display_,   this->grayfg_, this->stipple_);
  XSetFillStyle(this->display_, this->graybg_, FillStippled);
  XSetStipple(this->display_,   this->graybg_, this->stipple_);
  
  XSelectInput(this->display_, this->window_, 
	       PointerMotionMask |
	       ButtonPressMask | 
	       ButtonReleaseMask | 
	       KeyPressMask | 
	       ExposureMask);
  XMapRaised(this->display_, this->window_);
  
  this->point_count_ = 0;
  this->point_max_ = 1000;  /* doubled as necessary */
  this->points_ = (XPoint *) malloc (this->point_max_ * sizeof(XPoint));

  do {
    int x, y;
    unsigned int state;
    event = WWindow__NextEvent(this, &x, &y, &state);
  } while (!event || event->type != Expose);

  /* The first expose event gives us the real size, maybe?  Not really...*/
  this->width_ = event->xexpose.width;
  this->height_ = event->xexpose.height;
  this->drawbuffer_ = 
    XCreatePixmap(this->display_, this->window_, this->width_, this->height_,
		  DefaultDepth(this->display_, DefaultScreen(this->display_)));
  XFillRectangle(this->display_, this->drawbuffer_, this->bg_, 0, 0,
		 this->width_, this->height_);
  return this;
}  

#if 0
static void WWindow__delete_WWindow(WWindow *this)
{
  XFreeGC(this->display_, this->gc_);
  XDestroyWindow(this->display_, this->window_);
  /* XCloseDisplay(display_); */
}  
#endif


static int tmp;

#define Swap(a,b) (tmp=a,a=b,b=tmp)

static void WWindow__box(WWindow *this, long ax, long ay, long bx, long by)
{
  if (ax > bx) Swap(ax,bx);
  if (ay > by) Swap(ay,by);
  XFillRectangle(this->display_, this->drawbuffer_, this->gc_, ax, ay, bx - ax, by - ay);
  this->dirty_ = true;
}


static void WWindow__vector(WWindow *this, long ax, long ay, long bx, long by)
{
  if (ax == bx && ay > by) Swap(ay,by);
  if (ay == by && ax > bx) Swap(ax,bx);
  XDrawLine(this->display_, this->drawbuffer_, this->gc_, ax, ay, bx, by);
  this->dirty_ = true;
}

static void WWindow__opacity(WWindow *this, long opacity)
{
	this->opaque_ = opacity;
	WWindow__color(this, this->color_);
}

static void WWindow__color(WWindow *this, unsigned long color)
{
  this->color_ = color ? this->foreground_ : this->background_;
  if (this->opaque_)
    if (this->color_ == this->background_) this->gc_ = this->bg_;
    else this->gc_ = this->fg_;
  else
    if (this->color_ == this->background_) this->gc_ = this->graybg_;
    else this->gc_ = this->grayfg_;
}

static void WWindow__sync(WWindow *this)
{
  if (this->dirty_) {
    XCopyArea(this->display_, this->drawbuffer_, this->window_, this->gc_,
	      0, 0, this->width_, this->height_, 0, 0);	    
    XFlush(this->display_);
    this->dirty_ = false;
  }
}

static void WWindow__new_path(WWindow *this)
{
  this->point_count_ = 0;
}

static void WWindow__add_vertex(WWindow *this, int x, int y)
{
  if (this->point_count_ == this->point_max_) {
    this->point_max_ *= 2;
    this->points_ = (XPoint *) realloc (this->points_,
                                this->point_max_ * sizeof(XPoint));
  }
  this->points_[this->point_count_].x = x;
  this->points_[this->point_count_].y = y;
  this->point_count_ += 1;
}

static void WWindow__polygon(WWindow *this)
{
  XFillPolygon(this->display_, this->drawbuffer_, this->gc_,
	       &this->points_[0], this->point_count_,
	       Complex, CoordModeOrigin);
  this->dirty_ = true;
}
 

static int time_click = 0;

static void WWindow__event(WWindow *this, long *key, long *x, long *y, long *z)
{
  unsigned char button;

  while (1) {
    int xi, yi;
    unsigned int state;
    XEvent *event = WWindow__NextEvent(this, &xi, &yi, &state);

    *x = xi; *y = yi; *z = 0;
    if (!event) {
      *key = TIME_CLICK;
      *z = time_click++;
      return;
    }
    else switch (event->type) {

    case MappingNotify:
/*** This looks like an unnoticed bug of long standing (the
 *** extra level of indirection is surely wrong; it was a
 *** "type pun" warning that alerted me to this):
      XRefreshKeyboardMapping((XMappingEvent*)&event);
 ***/
/*** So here is what I take to be the intention:  ***/
      XRefreshKeyboardMapping(&event->xmapping);
      break;

    case ButtonPress:
      button = event->xbutton.button;
      *key = ((button == 1) ? LEFT_MOUSE_BUTTON :
	      (button == 2) ? MIDDLE_MOUSE_BUTTON :
	      (button == 3) ? RIGHT_MOUSE_BUTTON : -1);
      *z = 255;
      return;
      
    case ButtonRelease:
      button = event->xbutton.button;
      *key = ((button == 1) ? LEFT_MOUSE_BUTTON :
	      (button == 2) ? MIDDLE_MOUSE_BUTTON :
	      (button == 3) ? RIGHT_MOUSE_BUTTON : -1);
      *z = 0;
      return;

    case MotionNotify:
      state = event->xmotion.state;
      *key = MOUSE_MOTION;
      if (state & Button1Mask) {*z = 255; *key = LEFT_MOUSE_BUTTON;}
      else if (state & Button2Mask) {*z = 255; *key = MIDDLE_MOUSE_BUTTON;}
      else if (state & Button3Mask) {*z = 255; *key = RIGHT_MOUSE_BUTTON;}
      return;
      
    case KeyPress: {
      XKeyEvent *k = &event->xkey;
      char buf[4096];
      int len = XLookupString(k, buf, sizeof(buf), 0, 0);
      if (len != 0) {
	*key = buf[0];
	if (state & Mod1Mask) *key |= 0x80; /* Set the meta bit. */
	return;
      }
    }

#if 0
    case Expose:
      if (event->xexpose.count == 0)
	XDrawImageString(event->xexpose.display,
			 event->xexpose.window,
			 w.gc_, 50, 50, hello, strlen(hello));
      break;
#endif
    }
  }
}

static XEvent *WWindow__NextEvent(WWindow *this, int *x, int *y,
				  unsigned int *state)
{
  static XEvent event;
  Display *display = WWindow__Pending(this);
  if (display) {
    WWindow__Read(this, &event, x, y, state);
    return &event;
  }
  else {
    fd_set channels;  /* oops - powder keg! */
    struct timeval timer;
    int fd;
    int r;
    FD_ZERO(&channels);
    fd = XConnectionNumber(this->display_);
    assert (fd >= 0);
assert (fd < FD_SETSIZE);  /* hack against the powder keg - FIXME */
    FD_SET(fd, &channels);
    timer.tv_sec = 0;
    timer.tv_usec = 1000030 / 60;
    r = select (fd+1, &channels, 0, 0, &timer);
    if (r == 0)
      return 0;
    else {
      WWindow__Read(this, &event, x, y, state);
      return &event;
    }
  }
}

static void WWindow__Read(WWindow *this, XEvent *event, int *x, int *y,
			  unsigned int *state)
{
  Window root, child;
  int root_x, root_y;
  XNextEvent(this->display_, event);
  if (event->type == MapNotify)
    while (XCheckTypedEvent(this->display_, MotionNotify, event));
  if (event->type == MotionNotify)
    while (XCheckTypedEvent(this->display_, MotionNotify, event));
  XQueryPointer(this->display_, this->window_, &root, &child,
		&root_x, &root_y, x, y, state);
}

static Display *WWindow__Pending(WWindow *this)
{
  XFlush(this->display_);
  if (QLength(this->display_) != 0)
    return this->display_;
  return 0;
}

