/* $Id: w-custom.c,v 1.4 2022/12/11 17:09:27 setlorg Exp $ */

/* Support for Ken Perlin's minimalist "W" graphics library  */

#include "setlrun.h"
#include "custom.h"
#include "W.h"

#define public /* */

public void l_w_add_vertex(integer *x, integer *y) {
  long lx, ly;
  lx = get_long(x, "first (x) arg to W_ADD_VERTEX");
  ly = get_long(y, "first (y) arg to W_ADD_VERTEX");
  W_add_vertex(lx,ly);
}

public void l_w_box(integer *ax, integer *ay, integer *bx, integer *by) {
  long lax, lay, lbx, lby;
  lax = get_long(ax, "first (x1) arg to W_ADD_VERTEX");
  lay = get_long(ay, "second (y1) arg to W_ADD_VERTEX");
  lbx = get_long(bx, "third (x2) arg to W_ADD_VERTEX");
  lby = get_long(by, "fourth (y2) arg to W_ADD_VERTEX");
  W_box(lax,lay,lbx,lby);
}

public void l_w_color(integer *c) {
  unsigned long color;
  color = get_ulong(c, "W_COLOR arg");
  W_color(color);
}

public void l_w_event(integer **k, integer **x, integer **y, integer **z) {
  integer *ik = NULL;	HANDLE hk = ref(ik);
  integer *ix = NULL;	HANDLE hx = ref(ix);
  integer *iy = NULL;	HANDLE hy = ref(iy);
  integer *iz = NULL;	HANDLE h_z = ref(iz);
  long lk, lx, ly, lz;
  W_event(&lk,&lx,&ly,&lz);
  ik = new_integer(lk);
  ix = new_integer(lx);
  iy = new_integer(ly);
  iz = new_integer(lz);
  *k = ik;
  *x = ix;
  *y = iy;
  *z = iz;
  retire(h_z);  /* hz is in a standard Berkeley #include */
  retire(hy);
  retire(hx);
  retire(hk);
}

public void l_w_init(integer *x, integer *y) {
  long lx, ly;
  lx = get_long(x, "first (x) arg to W_INIT");
  ly = get_long(y, "second (y) arg to W_INIT");
  W_init(lx,ly);
}

public void l_w_new_path(void) {
  W_new_path();
}

public void l_w_opacity(integer *c) {
  long opacity;
  opacity = get_long(c, "W_OPACITY arg");
  W_opacity(opacity);
}

public void l_w_polygon(void) {
  W_polygon();
}

public void l_w_sync(void) {
  W_sync();
}

public void l_w_vector(integer *ax, integer *ay, integer *bx, integer *by) {
  long lax, lay, lbx, lby;
  lax = get_long(ax, "first (x1) arg to W_VECTOR");
  lay = get_long(ay, "second (y1) arg to W_VECTOR");
  lbx = get_long(bx, "third (x2) arg to W_VECTOR");
  lby = get_long(by, "fourth (y2) arg to W_VECTOR");
  W_vector(lax,lay,lbx,lby);
}
