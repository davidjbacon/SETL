/*  --  Auxiliary support for the GNU SETL GLUT/Mesa interface  --  */

/*  $Id: auxi.c,v 1.15 2022/12/11 17:09:27 setlorg Exp $  */


/*
 *  This is the maximum number of elements a GL routine with the pair
 *  "GLenum pname, GL... *params" in its parameter list will return
 *  through the 'params' pointer:
 */
#define MAX_PARAMS 16

static long n_params(GLenum pname) {
  long r;
  /*
   *  This switch was generated with the help of sniff-params.setl
   *  and some snarfs from "man" pages.
   *
   *  It was supposed have been current as of GLUT 3.6.  A few cases
   *  were added subsequently.
   */
  switch (pname) {
  case GL_ACCUM_ALPHA_BITS:                r = 1; break;
  case GL_ACCUM_BLUE_BITS:                 r = 1; break;
  case GL_ACCUM_CLEAR_VALUE:               r = 4; break;
  case GL_ACCUM_GREEN_BITS:                r = 1; break;
  case GL_ACCUM_RED_BITS:                  r = 1; break;
  case GL_ALPHA_BIAS:                      r = 1; break;
  case GL_ALPHA_BITS:                      r = 1; break;
  case GL_ALPHA_SCALE:                     r = 1; break;
  case GL_ALPHA_TEST:                      r = 1; break;
  case GL_ALPHA_TEST_REF:                  r = 1; break;
  case GL_ATTRIB_STACK_DEPTH:              r = 1; break;
  case GL_AUTO_NORMAL:                     r = 1; break;
  case GL_AUX_BUFFERS:                     r = 1; break;
  case GL_BLEND:                           r = 1; break;
  case GL_BLEND_DST:                       r = 1; break;
  case GL_BLEND_SRC:                       r = 1; break;
  case GL_BLUE_BIAS:                       r = 1; break;
  case GL_BLUE_BITS:                       r = 1; break;
  case GL_BLUE_SCALE:                      r = 1; break;
  case GL_CLIP_PLANE0:                     r = 1; break;
  case GL_CLIP_PLANE1:                     r = 1; break;
  case GL_CLIP_PLANE2:                     r = 1; break;
  case GL_CLIP_PLANE3:                     r = 1; break;
  case GL_CLIP_PLANE4:                     r = 1; break;
  case GL_CLIP_PLANE5:                     r = 1; break;
  case GL_COLOR_CLEAR_VALUE:               r = 4; break;
  case GL_COLOR_MATERIAL:                  r = 1; break;
  case GL_COLOR_MATERIAL_FACE:             r = 1; break;
  case GL_COLOR_MATERIAL_PARAMETER:        r = 1; break;
  case GL_COLOR_WRITEMASK:                 r = 4; break;
  case GL_CULL_FACE:                       r = 1; break;
  case GL_CULL_FACE_MODE:                  r = 1; break;
  case GL_CURRENT_COLOR:                   r = 4; break;
  case GL_CURRENT_INDEX:                   r = 1; break;
  case GL_CURRENT_NORMAL:                  r = 3; break;
  case GL_CURRENT_RASTER_COLOR:            r = 4; break;
  case GL_CURRENT_RASTER_INDEX:            r = 1; break;
  case GL_CURRENT_RASTER_POSITION:         r = 4; break;
  case GL_CURRENT_RASTER_TEXTURE_COORDS:   r = 4; break;
  case GL_CURRENT_RASTER_POSITION_VALID:   r = 1; break;
  case GL_CURRENT_TEXTURE_COORDS:          r = 4; break;
  case GL_DEPTH_BITS:                      r = 1; break;
  case GL_DEPTH_CLEAR_VALUE:               r = 1; break;
  case GL_DEPTH_FUNC:                      r = 1; break;
  case GL_DEPTH_RANGE:                     r = 2; break;
  case GL_DEPTH_WRITEMASK:                 r = 1; break;
  case GL_DOUBLEBUFFER:                    r = 1; break;
  case GL_DRAW_BUFFER:                     r = 1; break;
  case GL_EDGE_FLAG:                       r = 1; break;
  case GL_FOG:                             r = 1; break;
  case GL_FOG_COLOR:                       r = 4; break;
  case GL_FOG_DENSITY:                     r = 1; break;
  case GL_FOG_END:                         r = 1; break;
  case GL_FOG_HINT:                        r = 1; break;
  case GL_FOG_INDEX:                       r = 1; break;
  case GL_FOG_MODE:                        r = 1; break;
  case GL_FOG_START:                       r = 1; break;
  case GL_FRONT_FACE:                      r = 1; break;
  case GL_GREEN_BIAS:                      r = 1; break;
  case GL_GREEN_BITS:                      r = 1; break;
  case GL_GREEN_SCALE:                     r = 1; break;
  case GL_INDEX_BITS:                      r = 1; break;
  case GL_INDEX_CLEAR_VALUE:               r = 1; break;
  case GL_INDEX_MODE:                      r = 1; break;
  case GL_INDEX_OFFSET:                    r = 1; break;
  case GL_INDEX_SHIFT:                     r = 1; break;
  case GL_INDEX_WRITEMASK:                 r = 1; break;
  case GL_LIGHT0:                          r = 1; break;
  case GL_LIGHT1:                          r = 1; break;
  case GL_LIGHT2:                          r = 1; break;
  case GL_LIGHT3:                          r = 1; break;
  case GL_LIGHT4:                          r = 1; break;
  case GL_LIGHT5:                          r = 1; break;
  case GL_LIGHT6:                          r = 1; break;
  case GL_LIGHT7:                          r = 1; break;
  case GL_LIGHTING:                        r = 1; break;
  case GL_LIGHT_MODEL_AMBIENT:             r = 4; break;
  case GL_LIGHT_MODEL_LOCAL_VIEWER:        r = 1; break;
  case GL_LIGHT_MODEL_TWO_SIDE:            r = 1; break;
  case GL_AMBIENT:                         r = 4; break;
  case GL_DIFFUSE:                         r = 4; break;
  case GL_AMBIENT_AND_DIFFUSE:             r = 4; break;
  case GL_SPECULAR:                        r = 4; break;
  case GL_EMISSION:                        r = 4; break;
  case GL_SHININESS:                       r = 1; break;
  case GL_COLOR_INDEXES:                   r = 3; break;
  case GL_POSITION:                        r = 4; break;
  case GL_SPOT_DIRECTION:                  r = 3; break;
  case GL_SPOT_EXPONENT:                   r = 1; break;
  case GL_SPOT_CUTOFF:                     r = 1; break;
  case GL_CONSTANT_ATTENUATION:            r = 1; break;
  case GL_LINEAR_ATTENUATION:              r = 1; break;
  case GL_QUADRATIC_ATTENUATION:           r = 1; break;
  case GL_LINE_SMOOTH:                     r = 1; break;
  case GL_LINE_STIPPLE:                    r = 1; break;
  case GL_LINE_STIPPLE_PATTERN:            r = 1; break;
  case GL_LINE_STIPPLE_REPEAT:             r = 1; break;
  case GL_LINE_WIDTH:                      r = 1; break;
  case GL_LINE_WIDTH_GRANULARITY:          r = 1; break;
  case GL_LINE_WIDTH_RANGE:                r = 2; break;
  case GL_LIST_BASE:                       r = 1; break;
  case GL_LIST_INDEX:                      r = 1; break;
  case GL_LIST_MODE:                       r = 1; break;
  case GL_LOGIC_OP:                        r = 1; break;
  case GL_LOGIC_OP_MODE:                   r = 1; break;
  case GL_MAP1_COLOR_4:                    r = 1; break;
  case GL_MAP1_GRID_DOMAIN:                r = 2; break;
  case GL_MAP1_GRID_SEGMENTS:              r = 1; break;
  case GL_MAP1_INDEX:                      r = 1; break;
  case GL_MAP1_NORMAL:                     r = 1; break;
  case GL_MAP1_TEXTURE_COORD_1:            r = 1; break;
  case GL_MAP1_TEXTURE_COORD_2:            r = 1; break;
  case GL_MAP1_TEXTURE_COORD_3:            r = 1; break;
  case GL_MAP1_TEXTURE_COORD_4:            r = 1; break;
  case GL_MAP1_VERTEX_3:                   r = 1; break;
  case GL_MAP1_VERTEX_4:                   r = 1; break;
  case GL_MAP2_COLOR_4:                    r = 1; break;
  case GL_MAP2_GRID_DOMAIN:                r = 4; break;
  case GL_MAP2_GRID_SEGMENTS:              r = 2; break;
  case GL_MAP2_INDEX:                      r = 1; break;
  case GL_MAP2_NORMAL:                     r = 1; break;
  case GL_MAP2_TEXTURE_COORD_1:            r = 1; break;
  case GL_MAP2_TEXTURE_COORD_2:            r = 1; break;
  case GL_MAP2_TEXTURE_COORD_3:            r = 1; break;
  case GL_MAP2_TEXTURE_COORD_4:            r = 1; break;
  case GL_MAP2_VERTEX_3:                   r = 1; break;
  case GL_MAP2_VERTEX_4:                   r = 1; break;
  case GL_MAP_COLOR:                       r = 1; break;
  case GL_MAP_STENCIL:                     r = 1; break;
  case GL_MATRIX_MODE:                     r = 1; break;
  case GL_MAX_ATTRIB_STACK_DEPTH:          r = 1; break;
  case GL_MAX_CLIP_PLANES:                 r = 1; break;
  case GL_MAX_EVAL_ORDER:                  r = 1; break;
  case GL_MAX_LIGHTS:                      r = 1; break;
  case GL_MAX_LIST_NESTING:                r = 1; break;
  case GL_MAX_MODELVIEW_STACK_DEPTH:       r = 1; break;
  case GL_MAX_NAME_STACK_DEPTH:            r = 1; break;
  case GL_MAX_PIXEL_MAP_TABLE:             r = 1; break;
  case GL_MAX_PROJECTION_STACK_DEPTH:      r = 1; break;
  case GL_MAX_TEXTURE_SIZE:                r = 1; break;
  case GL_MAX_TEXTURE_STACK_DEPTH:         r = 1; break;
  case GL_MAX_VIEWPORT_DIMS:               r = 2; break;
  case GL_MODELVIEW_MATRIX:                r = 16; break;
  case GL_MODELVIEW_STACK_DEPTH:           r = 1; break;
  case GL_NORMALIZE:                       r = 1; break;
  case GL_PACK_ALIGNMENT:                  r = 1; break;
  case GL_PACK_LSB_FIRST:                  r = 1; break;
  case GL_PACK_ROW_LENGTH:                 r = 1; break;
  case GL_PACK_SKIP_PIXELS:                r = 1; break;
  case GL_PACK_SKIP_ROWS:                  r = 1; break;
  case GL_PACK_SWAP_BYTES:                 r = 1; break;
  case GL_PIXEL_MAP_A_TO_A_SIZE:           r = 1; break;
  case GL_PIXEL_MAP_B_TO_B_SIZE:           r = 1; break;
  case GL_PIXEL_MAP_G_TO_G_SIZE:           r = 1; break;
  case GL_PIXEL_MAP_I_TO_A_SIZE:           r = 1; break;
  case GL_PIXEL_MAP_I_TO_B_SIZE:           r = 1; break;
  case GL_PIXEL_MAP_I_TO_G_SIZE:           r = 1; break;
  case GL_PIXEL_MAP_I_TO_I_SIZE:           r = 1; break;
  case GL_PIXEL_MAP_I_TO_R_SIZE:           r = 1; break;
  case GL_PIXEL_MAP_R_TO_R_SIZE:           r = 1; break;
  case GL_PIXEL_MAP_S_TO_S_SIZE:           r = 1; break;
  case GL_POINT_SIZE:                      r = 1; break;
  case GL_POINT_SIZE_GRANULARITY:          r = 1; break;
  case GL_POINT_SIZE_RANGE:                r = 2; break;
  case GL_POINT_SMOOTH:                    r = 1; break;
  case GL_POLYGON_MODE:                    r = 2; break;
  case GL_POLYGON_SMOOTH:                  r = 1; break;
  case GL_POLYGON_STIPPLE:                 r = 1; break;
  case GL_PROJECTION_MATRIX:               r = 16; break;
  case GL_PROJECTION_STACK_DEPTH:          r = 1; break;
  case GL_READ_BUFFER:                     r = 1; break;
  case GL_RED_BIAS:                        r = 1; break;
  case GL_RED_BITS:                        r = 1; break;
  case GL_RED_SCALE:                       r = 1; break;
  case GL_RENDER_MODE:                     r = 1; break;
  case GL_RGBA_MODE:                       r = 1; break;
  case GL_SCISSOR_BOX:                     r = 4; break;
  case GL_SCISSOR_TEST:                    r = 1; break;
  case GL_SHADE_MODEL:                     r = 1; break;
  case GL_STENCIL_BITS:                    r = 1; break;
  case GL_STENCIL_CLEAR_VALUE:             r = 1; break;
  case GL_STENCIL_FAIL:                    r = 1; break;
  case GL_STENCIL_FUNC:                    r = 1; break;
  case GL_STENCIL_PASS_DEPTH_FAIL:         r = 1; break;
  case GL_STENCIL_PASS_DEPTH_PASS:         r = 1; break;
  case GL_STENCIL_REF:                     r = 1; break;
  case GL_STENCIL_TEST:                    r = 1; break;
  case GL_STENCIL_VALUE_MASK:              r = 1; break;
  case GL_STENCIL_WRITEMASK:               r = 1; break;
  case GL_STEREO:                          r = 1; break;
  case GL_SUBPIXEL_BITS:                   r = 1; break;
  case GL_TEXTURE_1D:                      r = 1; break;
  case GL_TEXTURE_2D:                      r = 1; break;
  case GL_TEXTURE_GEN_S:                   r = 1; break;
  case GL_TEXTURE_GEN_T:                   r = 1; break;
  case GL_TEXTURE_GEN_R:                   r = 1; break;
  case GL_TEXTURE_GEN_Q:                   r = 1; break;
  case GL_TEXTURE_MATRIX:                  r = 16; break;
  case GL_TEXTURE_STACK_DEPTH:             r = 1; break;
  case GL_TEXTURE_MAG_FILTER:              r = 1; break;
  case GL_TEXTURE_MIN_FILTER:              r = 1; break;
  case GL_TEXTURE_WRAP_S:                  r = 1; break;
  case GL_TEXTURE_WRAP_T:                  r = 1; break;
  case GL_TEXTURE_BORDER_COLOR:            r = 4; break;
  case GL_TEXTURE_WIDTH:                   r = 1; break;
  case GL_TEXTURE_HEIGHT:                  r = 1; break;
  case GL_TEXTURE_COMPONENTS:              r = 1; break;
  case GL_TEXTURE_BORDER:                  r = 1; break;
  case GL_TEXTURE_GEN_MODE:                r = 1; break;
  case GL_OBJECT_PLANE:                    r = 4; break;
  case GL_EYE_PLANE:                       r = 4; break;
  case GL_TEXTURE_ENV_MODE:                r = 1; break;
  case GL_TEXTURE_ENV_COLOR:               r = 4; break;
  case GL_UNPACK_ALIGNMENT:                r = 1; break;
  case GL_UNPACK_LSB_FIRST:                r = 1; break;
  case GL_UNPACK_ROW_LENGTH:               r = 1; break;
  case GL_UNPACK_SKIP_PIXELS:              r = 1; break;
  case GL_UNPACK_SKIP_ROWS:                r = 1; break;
  case GL_UNPACK_SWAP_BYTES:               r = 1; break;
  case GL_VIEWPORT:                        r = 4; break;
  case GL_ZOOM_X:                          r = 1; break;
  case GL_ZOOM_Y:                          r = 1; break;
  case GL_ELEMENT_ARRAY_BUFFER:            r = 1; break;
  case GL_BINORMAL_ARRAY_POINTER_EXT:      r = 1; break;
  case GL_COLOR_ARRAY_POINTER:             r = 1; break;
  case GL_EDGE_FLAG_ARRAY_POINTER:         r = 1; break;
  case GL_FOG_COORDINATE_ARRAY_POINTER:    r = 1; break;
  case GL_INDEX_ARRAY_POINTER:             r = 1; break;
  case GL_MATRIX_INDEX_ARRAY_POINTER_ARB:  r = 1; break;
  case GL_NORMAL_ARRAY_POINTER:            r = 1; break;
  case GL_SECONDARY_COLOR_ARRAY_POINTER:   r = 1; break;
  case GL_TANGENT_ARRAY_POINTER_EXT:       r = 1; break;
  case GL_TEXTURE_COORD_ARRAY_POINTER:     r = 1; break;
  case GL_VARIANT_ARRAY_POINTER_EXT:       r = 1; break;
  case GL_VERTEX_ARRAY_POINTER:            r = 1; break;
  case GL_VERTEX_ATTRIB_ARRAY_POINTER:     r = 1; break;
  case GL_VERTEX_WEIGHT_ARRAY_POINTER_EXT: r = 1; break;
  case GL_WEIGHT_ARRAY_POINTER_ARB:        r = 1; break;
  /*
   *  Perhaps instead of treating this as an error, we should just
   *  assume 1 parameter:
   */
  default:  runerr("Number of parameters not known for GL_ constant "
                   "having value %d", pname);
  }
  assert (r <= MAX_PARAMS);
  return r;
}

/*
 *  Conversion utilities
 */

static void check_tuple(long n, tuple *t, const char *what) {
  if (!is_tuple(t)) {
    runerr("Type of %s must be TUPLE, not %s",
                  what,           TYPENAME(t));
  }
  if (t->nelt < n) {
    runerr("Need at least %ld elements in %s, but only %ld found",
                            n,          what,      t->nelt);
  }
}

static bool get_boolean_element(tuple *t, long i, const char *what) {
  boolean *x = (boolean *)tupelt(t,i);
  if (!is_boolean(x)) {
    runerr("Element %ld of %s must be BOOLEAN, not %s",
                      i, what,             TYPENAME(x));
  }
  return x->booval;
}

static long get_long_element(tuple *t, long i, long lwb, long upb, const char *what) {
  integer *x = (integer *)tupelt(t,i);
  char *swat = (char *)alloca(strlen(what)+50);
  sprintf(swat,"element %ld of %s",i,what);
  return get_long_in(x,lwb,upb,swat);
}

static ulong get_ulong_element(tuple *t, long i, ulong upb, const char *what) {
  integer *x = (integer *)tupelt(t,i);
  char *swat = (char *)alloca(strlen(what)+50);
  sprintf(swat,"element %ld of %s",i,what);
  ulong r = get_ulong(x,swat);
  if (r > upb) {
    runerr("Value of %s (%lu) is out of range 0..%lu",
                   swat,   r,                    upb);
  }
  return r;
}

static double get_double_element(tuple *t, long i, const char *what) {
  real *x = (real *)tupelt(t,i);
  char *swat = (char *)alloca(strlen(what)+50);
  sprintf(swat,"Element %ld of %s",i,what);
  return get_double(x,swat);
}

static const char *see_strelt(string *s, const char *what) {
  if (!is_string(s)) {
    runerr("Type of %s must be STRING, not %s",
                  what,            TYPENAME(s));
  }
  return (const char *)&strelt(s,1);
}

static void get_booleans(GLboolean *dst, long n, tuple *src, const char *what) {
  long i;
  check_tuple(n, src, what);
  for (i=0; i<n; i++) {
    dst[i] = get_boolean_element(src, i+1, what);
  }
}

static void get_bytes(GLbyte *dst, long n, tuple *src, const char *what) {
  long i;
  check_tuple(n, src, what);
  for (i=0; i<n; i++) {
    dst[i] = get_long_element(src, i+1, SCHAR_MIN,SCHAR_MAX, what);
  }
}

static void get_ubytes(GLubyte *dst, long n, tuple *src, const char *what) {
  long i;
  check_tuple(n, src, what);
  for (i=0; i<n; i++) {
    dst[i] = get_ulong_element(src, i+1, UCHAR_MAX, what);
  }
}

static void get_shorts(GLshort *dst, long n, tuple *src, const char *what) {
  long i;
  check_tuple(n, src, what);
  for (i=0; i<n; i++) {
    dst[i] = get_long_element(src, i+1, SHRT_MIN,SHRT_MAX, what);
  }
}

static void get_ushorts(GLushort *dst, long n, tuple *src, const char *what) {
  long i;
  check_tuple(n, src, what);
  for (i=0; i<n; i++) {
    dst[i] = get_ulong_element(src, i+1, USHRT_MAX, what);
  }
}

static void get_ints(GLint *dst, long n, tuple *src, const char *what) {
  long i;
  check_tuple(n, src, what);
  for (i=0; i<n; i++) {
    dst[i] = get_long_element(src, i+1, INT_MIN,INT_MAX, what);
  }
}

static void get_uints(GLuint *dst, long n, tuple *src, const char *what) {
  long i;
  check_tuple(n, src, what);
  for (i=0; i<n; i++) {
    dst[i] = get_ulong_element(src, i+1, UINT_MAX, what);
  }
}

static void get_floats(GLfloat *dst, long n, tuple *src, const char *what) {
  long i;
  check_tuple(n, src, what);
  for (i=0; i<n; i++) {
    dst[i] = get_double_element(src, i+1, what);
  }
}

static void get_doubles(GLdouble *dst, long n, tuple *src, const char *what) {
  long i;
  check_tuple(n, src, what);
  for (i=0; i<n; i++) {
    dst[i] = get_double_element(src, i+1, what);
  }
}

/* This one is really bogus, but avoids provoking a warning when
 * casting the result of get_ulong() to a pointer in the presence
 * of GCC's -Wbad-function-cast warning flag:  */
static void *get_ptr(integer *i, const char *what) {
  ulong a = get_ulong(i, what);
  return (void *)a;
}

static tuple *new_boolean_tuple(long n, const GLboolean *src) {
  tuple *r = new_tuple(n);  HANDLE hr = ref(r);
  long i;
  for (i=0; i<n; i++) {
    let (tupelt(r,i+1), (block *)new_boolean(!!src[i]));
  }
  retire(hr);
  return r;
}

static tuple *new_int_tuple(long n, const GLint *src) {
  tuple *r = new_tuple(n);  HANDLE hr = ref(r);
  long i;
  for (i=0; i<n; i++) {
    let (tupelt(r,i+1), (block *)new_integer(src[i]));
  }
  retire(hr);
  return r;
}

static tuple *new_float_tuple(long n, const GLfloat *src) {
  tuple *r = new_tuple(n);  HANDLE hr = ref(r);
  long i;
  for (i=0; i<n; i++) {
    let (tupelt(r,i+1), (block *)new_real(src[i]));
  }
  retire(hr);
  return r;
}

static tuple *new_double_tuple(long n, const GLdouble *src) {
  tuple *r = new_tuple(n);  HANDLE hr = ref(r);
  long i;
  for (i=0; i<n; i++) {
    let (tupelt(r,i+1), (block *)new_real(src[i]));
  }
  retire(hr);
  return r;
}

static tuple *new_pointer_tuple(long n, void **src) {
  tuple *r = new_tuple(n);  HANDLE hr = ref(r);
  long i;
  for (i=0; i<n; i++) {
    let (tupelt(r,i+1), (block *)ulong_integer((ulong)src[i]));
  }
  retire(hr);
  return r;
}


void glutSETLInit(void) {
  int argc = (int) command_line->nelt + 1;
  int argi;
  char **argv = (char **) malloc(argc * sizeof(char *));
  argv[0] = strcpy ((char *) malloc(command_name->nchar + 1),
                              &strelt(command_name,1));
  for (argi=1; argi<argc; argi++) {
    string *s = (string *) tupelt(command_line, argi);
    argv[argi] = strcpy ((char *) malloc(s->nchar + 1), &strelt(s,1));
  }
  glutInit(&argc, argv);
  command_line = new_tuple(argc-1);
  for (argi=1; argi<argc; argi++) {
    let (tupelt(command_line,argi), (block *)new_string(argv[argi]));
  }
}

/*
 *  Here is a bogus little data structure to support the reflection
 *  of events:
 */

typedef struct {
  int *array;     /* grows as necessary */
  int arrayMax;   /* upper bound of space */
  int arraySize;  /* upper bound of defined members */
  int arrayCur;   /* current index for taking from front */
} intArray;

#define emptyIntArray {NULL, 0, 0, 0}

static void appendInt(intArray *a, int k) {
  if (a->arraySize >= a->arrayMax) {
    if (a->arrayMax == 0) {
      a->arrayMax = 100;
      a->array = (int *) malloc(a->arrayMax * sizeof(int));
    } else {
      a->arrayMax = 2 * a->arraySize;
      a->array = (int *) realloc(a->array, a->arrayMax * sizeof(int));
    }
    assert (a->array);  /* else quite tiny malloc or realloc failed! */
  }
  a->array[a->arraySize++] = k;
}

static int takeInt(intArray *a) {
  assert (a->arrayCur < a->arraySize);
  return a->array[a->arrayCur++];
}

static void resetIntArray(intArray *a) {
  a->arraySize = 0;
  a->arrayCur = 0;
}

static intArray events = emptyIntArray;

tuple *glutSETLGetOneEvent(void) {
  int i, n;
  tuple *r;  HANDLE hr;
  if (events.arrayCur == events.arraySize) {
    /* event array was consumed; try to add to it again */
    resetIntArray(&events);  /* make way for loading */
    /* The dB GLUT 3.6 extension would want glutEventWait() here
     * instead of this freeglut call:  */
    glutMainLoopEvent();  /* grab all currently queued events */
  }
  if (events.arrayCur == events.arraySize) {
    /* nothing new */
    return OM;
  }
  /* Return the first unconsumed event from the event array */
  n = takeInt(&events);
  r = new_tuple(n);  hr = ref(r);
  for (i=1; i<=n; i++) {
    int k = takeInt(&events);
    let (tupelt(r,i), (block *)new_integer(k));
  }
  retire(hr);
  return r;
}

static void recordEvent(int n, ...) {
  va_list ap;
  int i;
  va_start(ap, n);
  appendInt(&events, n);
  for (i=0; i<n; i++) {
    int k = va_arg(ap, int);
    appendInt(&events, k);
  }
  va_end(ap);
}


/*
 *  The following repetitive code was generated in part by applying
 *  glutSETL-gen.setl to the gluta...Func prototypes in auxi.setl,
 *  which at the time of this writing was defined as:
 *
 *  while (s := getline stdin) /= om loop
 *    s('proc gluta') := '';
 *    s('Func') := '';
 *    p := s('\\(.*\\)');
 *    s('\\(.*$') := '';
 *    t := s;
 *    t(1) := to_lower t(1);
 *    print;
 *    print('static int '+t+'EventType = 0;');
 *    print;
 *    print('static void '+t+'Handler(<<void, or expected parms>>) {');
 *    print('  recordEvent(<<how many parms follow>>, '+t+
 *                                       'EventType <<, parms...>>);');
 *    print('}');
 *    print;
 *    a := split(p(2..#p-1),',');
 *    a := [if a = 'p' then 'eventType' else a end : a in a];
 *    r := +/['int '+r+', ' : r in a];
 *    r(#r-1..) := '';
 *    print('void glutSETL'+s+'Func('+r+') {');
 *    print('  '+t+'EventType = eventType;');
 *    print('  if (eventType == 0) {');
 *    b := [if b = 'eventType' then 'NULL' else b end : b in a];
 *    r := +/[r+', ' : r in b];
 *    r(#r-1..) := '';
 *    print('    glut'+s+'Func('+r+');');
 *    print('  } else {');
 *    r('NULL') := t+'Handler';
 *    print('    glut'+s+'Func('+r+');');
 *    print('  }');
 *    print('}');
 *  end loop;
 *
 *  The things in angle brackets can be filled in pretty easily "by
 *  hand" using glutaDispatch in auxi.setl as a guide.  For example,
 *  you would put:
 *
 *  static void reshapeHandler(int width, int height) {
 *    recordEvent(3, reshapeEventType, width, height);
 *  }
 *
 *  in place of the skeletal:
 *
 *  static void reshapeHandler(<<void, or expected parms>>) {
 *    recordEvent(<<how many parms follow>>, reshapeEventType <<, parms...>>);
 *  }
 *
 *  However, if I have to do it again, I think I will extend
 *  glutSETL-gen.setl to grep out the appropriate glut...Func lines
 *  from <GL/glut.h> and digest them to create these local handlers.
 */

static int idleEventType = 0;

static void idleHandler(void) {
  recordEvent(1, idleEventType);
}

void glutSETLIdleFunc(int eventType) {
  idleEventType = eventType;
  if (eventType == 0) {
    glutIdleFunc(NULL);
  } else {
    glutIdleFunc(idleHandler);
  }
}

static int displayEventType = 0;

static void displayHandler(void) {
  recordEvent(1, displayEventType);
}

void glutSETLDisplayFunc(int eventType) {
  displayEventType = eventType;
  if (eventType == 0) {
    glutDisplayFunc(NULL);
  } else {
    glutDisplayFunc(displayHandler);
  }
}

static int overlayDisplayEventType = 0;

static void overlayDisplayHandler(void) {
  recordEvent(1, overlayDisplayEventType);
}

void glutSETLOverlayDisplayFunc(int eventType) {
  overlayDisplayEventType = eventType;
  if (eventType == 0) {
    glutOverlayDisplayFunc(NULL);
  } else {
    glutOverlayDisplayFunc(overlayDisplayHandler);
  }
}

static int reshapeEventType = 0;

static void reshapeHandler(int width, int height) {
  recordEvent(3, reshapeEventType, width, height);
}

void glutSETLReshapeFunc(int eventType) {
  reshapeEventType = eventType;
  if (eventType == 0) {
    glutReshapeFunc(NULL);
  } else {
    glutReshapeFunc(reshapeHandler);
  }
}

static int keyboardEventType = 0;

static void keyboardHandler(unsigned char k, int x, int y) {
  recordEvent(4, keyboardEventType, (int)k, x, y);
}

void glutSETLKeyboardFunc(int eventType) {
  keyboardEventType = eventType;
  if (eventType == 0) {
    glutKeyboardFunc(NULL);
  } else {
    glutKeyboardFunc(keyboardHandler);
  }
}

static int mouseEventType = 0;

static void mouseHandler(int button, int state, int x, int y) {
  recordEvent(5, mouseEventType, button, state, x, y);
}

void glutSETLMouseFunc(int eventType) {
  mouseEventType = eventType;
  if (eventType == 0) {
    glutMouseFunc(NULL);
  } else {
    glutMouseFunc(mouseHandler);
  }
}

static int motionEventType = 0;

static void motionHandler(int x, int y) {
  recordEvent(3, motionEventType, x, y);
}

void glutSETLMotionFunc(int eventType) {
  motionEventType = eventType;
  if (eventType == 0) {
    glutMotionFunc(NULL);
  } else {
    glutMotionFunc(motionHandler);
  }
}

static int passiveMotionEventType = 0;

static void passiveMotionHandler(int x, int y) {
  recordEvent(3, passiveMotionEventType, x, y);
}

void glutSETLPassiveMotionFunc(int eventType) {
  passiveMotionEventType = eventType;
  if (eventType == 0) {
    glutPassiveMotionFunc(NULL);
  } else {
    glutPassiveMotionFunc(passiveMotionHandler);
  }
}

static int visibilityEventType = 0;

static void visibilityHandler(int state) {
  recordEvent(2, visibilityEventType, state);
}

void glutSETLVisibilityFunc(int eventType) {
  visibilityEventType = eventType;
  if (eventType == 0) {
    glutVisibilityFunc(NULL);
  } else {
    glutVisibilityFunc(visibilityHandler);
  }
}

static int entryEventType = 0;

static void entryHandler(int state) {
  recordEvent(2, entryEventType, state);
}

void glutSETLEntryFunc(int eventType) {
  entryEventType = eventType;
  if (eventType == 0) {
    glutEntryFunc(NULL);
  } else {
    glutEntryFunc(entryHandler);
  }
}

static int specialEventType = 0;

static void specialHandler(int k, int x, int y) {
  recordEvent(4, specialEventType, k, x, y);
}

void glutSETLSpecialFunc(int eventType) {
  specialEventType = eventType;
  if (eventType == 0) {
    glutSpecialFunc(NULL);
  } else {
    glutSpecialFunc(specialHandler);
  }
}

static int spaceballMotionEventType = 0;

static void spaceballMotionHandler(int x, int y, int z) {
  recordEvent(4, spaceballMotionEventType, x, y, z);
}

void glutSETLSpaceballMotionFunc(int eventType) {
  spaceballMotionEventType = eventType;
  if (eventType == 0) {
    glutSpaceballMotionFunc(NULL);
  } else {
    glutSpaceballMotionFunc(spaceballMotionHandler);
  }
}

static int spaceballRotateEventType = 0;

static void spaceballRotateHandler(int x, int y, int z) {
  recordEvent(4, spaceballRotateEventType, x, y, z);
}

void glutSETLSpaceballRotateFunc(int eventType) {
  spaceballRotateEventType = eventType;
  if (eventType == 0) {
    glutSpaceballRotateFunc(NULL);
  } else {
    glutSpaceballRotateFunc(spaceballRotateHandler);
  }
}

static int spaceballButtonEventType = 0;

static void spaceballButtonHandler(int button, int state) {
  recordEvent(3, spaceballButtonEventType, button, state);
}

void glutSETLSpaceballButtonFunc(int eventType) {
  spaceballButtonEventType = eventType;
  if (eventType == 0) {
    glutSpaceballButtonFunc(NULL);
  } else {
    glutSpaceballButtonFunc(spaceballButtonHandler);
  }
}

static int buttonBoxEventType = 0;

static void buttonBoxHandler(int button, int state) {
  recordEvent(3, buttonBoxEventType, button, state);
}

void glutSETLButtonBoxFunc(int eventType) {
  buttonBoxEventType = eventType;
  if (eventType == 0) {
    glutButtonBoxFunc(NULL);
  } else {
    glutButtonBoxFunc(buttonBoxHandler);
  }
}

static int dialsEventType = 0;

static void dialsHandler(int dial, int value) {
  recordEvent(3, dialsEventType, dial, value);
}

void glutSETLDialsFunc(int eventType) {
  dialsEventType = eventType;
  if (eventType == 0) {
    glutDialsFunc(NULL);
  } else {
    glutDialsFunc(dialsHandler);
  }
}

static int tabletMotionEventType = 0;

static void tabletMotionHandler(int x, int y) {
  recordEvent(3, tabletMotionEventType, x, y);
}

void glutSETLTabletMotionFunc(int eventType) {
  tabletMotionEventType = eventType;
  if (eventType == 0) {
    glutTabletMotionFunc(NULL);
  } else {
    glutTabletMotionFunc(tabletMotionHandler);
  }
}

static int tabletButtonEventType = 0;

static void tabletButtonHandler(int button, int state, int x, int y) {
  recordEvent(5, tabletButtonEventType, button, state, x, y);
}

void glutSETLTabletButtonFunc(int eventType) {
  tabletButtonEventType = eventType;
  if (eventType == 0) {
    glutTabletButtonFunc(NULL);
  } else {
    glutTabletButtonFunc(tabletButtonHandler);
  }
}

static int menuStatusEventType = 0;

static void menuStatusHandler(int state, int x, int y) {
  recordEvent(4, menuStatusEventType, state, x, y);
}

void glutSETLMenuStatusFunc(int eventType) {
  menuStatusEventType = eventType;
  if (eventType == 0) {
    glutMenuStatusFunc(NULL);
  } else {
    glutMenuStatusFunc(menuStatusHandler);
  }
}

static int timerEventType = 0;

static void timerHandler(int id) {
  recordEvent(2, timerEventType, id);
}

void glutSETLTimerFunc(int ms, int eventType, int id) {
  timerEventType = eventType;
  if (eventType == 0) {
    glutTimerFunc(ms, NULL, id);
  } else {
    glutTimerFunc(ms, timerHandler, id);
  }
}

/*
 *  I had extended Kilgard's GLUT 3.6 with "file descriptor events"
 *  (and distributed the patches to the mesa and glut mailing lists
 *  and the comp.graphics.api.opengl newsgroup, on 26 Jan 1998), but
 *  according to some commentary in the newer freeglut documentation,
 *  Kilgard's license did not allow for source modifications.  (The
 *  patches, contained in 'glut-patch', appear to be applicable with
 *  very little change to GLUT 3.7, but even that, dating as it does
 *  from August 1998, is now [2009] essentially obsolete, while
 *  freeglut is being actively maintained.  The patches do not
 *  appear to have been adopted by the community or by Mark Kilgard.)
 *
 *  It would be nice to have support for these fd events in freeglut,
 *  though, either in a future release or as code that can be placed
 *  in front of the precompiled "vanilla" libglut.  Meanwhile, the
 *  calls to the desired but nonexistent glutFile*Func functions are
 *  commented out below.  The "gears.setl" program illustrates how to
 *  check in a timer callback for the readiness of an fd for reading,
 *  such polling being better than nothing at all.
 */

static int fileInputEventType = 0;

#if 0
static void fileInputHandler(int fd) {
  recordEvent(2, fileInputEventType, fd);
}
#endif

void glutSETLFileInputFunc(int fd, int eventType) {
  fileInputEventType = eventType;
  if (eventType == 0) {
    /* glutFileInputFunc(fd, NULL); */
  } else {
    /* glutFileInputFunc(fd, fileInputHandler); */
  }
}

static int fileOutputEventType = 0;

#if 0
static void fileOutputHandler(int fd) {
  recordEvent(2, fileOutputEventType, fd);
}
#endif

void glutSETLFileOutputFunc(int fd, int eventType) {
  fileOutputEventType = eventType;
  if (eventType == 0) {
    /* glutFileOutputFunc(fd, NULL); */
  } else {
    /* glutFileOutputFunc(fd, fileOutputHandler); */
  }
}

static int fileExceptionEventType = 0;

#if 0
static void fileExceptionHandler(int fd) {
  recordEvent(2, fileExceptionEventType, fd);
}
#endif

void glutSETLFileExceptionFunc(int fd, int eventType) {
  fileExceptionEventType = eventType;
  if (eventType == 0) {
    /* glutFileExceptionFunc(fd, NULL); */
  } else {
    /* glutFileExceptionFunc(fd, fileExceptionHandler); */
  }
}

static int closeEventType = 0;

static void closeHandler(void) {
  recordEvent(1, closeEventType);
}

void glutSETLCloseFunc(int eventType) {
  closeEventType = eventType;
  if (eventType == 0) {
    glutCloseFunc(NULL);
  } else {
    glutCloseFunc(closeHandler);
  }
}

static int menuSelectionEventType = 0;
static int howManyMenus = 0;

static void menuSelectionHandler(int value) {
  int menu = glutGetMenu();
  recordEvent(3, menuSelectionEventType, menu, value);
}

int glutSETLCreateMenu(int eventType) {
  menuSelectionEventType = eventType;
  howManyMenus++;
  return glutCreateMenu(menuSelectionHandler);
}

void glutSETLDestroyMenu(int menu) {
  glutDestroyMenu(menu);
  if (--howManyMenus == 0) menuSelectionEventType = 0;
}

void *glutSETLBitmap8By13(void) {
  return GLUT_BITMAP_8_BY_13;
}

void *glutSETLBitmap9By15(void) {
  return GLUT_BITMAP_9_BY_15;
}

void *glutSETLBitmapHelvetica10(void) {
  return GLUT_BITMAP_HELVETICA_10;
}

void *glutSETLBitmapHelvetica12(void) {
  return GLUT_BITMAP_HELVETICA_12;
}

void *glutSETLBitmapHelvetica18(void) {
  return GLUT_BITMAP_HELVETICA_18;
}

void *glutSETLBitmapTimesRoman10(void) {
  return GLUT_BITMAP_TIMES_ROMAN_10;
}

void *glutSETLBitmapTimesRoman24(void) {
  return GLUT_BITMAP_TIMES_ROMAN_24;
}

void *glutSETLStrokeMonoRoman(void) {
  return GLUT_STROKE_MONO_ROMAN;
}

void *glutSETLStrokeRoman(void) {
  return GLUT_STROKE_ROMAN;
}
