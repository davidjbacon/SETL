
#ifndef TRUE
#define TRUE 1
#endif

#ifndef FALSE
#define FALSE 0
#endif

#define TIME_CLICK 256
#define TABLET_PEN 257
#define LEFT_MOUSE_BUTTON (TABLET_PEN + 1)
#define MIDDLE_MOUSE_BUTTON (TABLET_PEN + 2)
#define RIGHT_MOUSE_BUTTON (TABLET_PEN + 3)
#define MOUSE_MOTION (TABLET_PEN + 4)

#ifdef __cplusplus
extern "C" {
#endif

    /* GET THE NEXT INPUT EVENT */
extern void W_event(long *k, long *x, long *y, long *z);

    /* DRAW BOX ONTO INTERNAL IMAGE */
extern void W_box(long ax, long ay, long bx, long by);

    /* DRAW VECTOR ONTO INTERNAL IMAGE */
extern void W_vector(long ax, long ay, long bx, long by);

    /* INITIALIZATION */
extern void W_init(long width, long height);

    /* SET THE DRAWING COLOR */
extern void W_color(unsigned long color);
extern void W_opacity(long opacity);

    /* UPDATE SCREEN FROM INTERNAL IMAGE */
extern void W_sync(void);

extern void W_new_path(void);
extern void W_add_vertex(int x, int y);
extern void W_polygon(void);

#ifdef __cplusplus
};
#endif

