/*  --  Auxiliary support for the GNU SETL GLUT/Mesa interface  --  */

/*  $Id: auxi.h,v 1.5 2022/12/11 17:09:27 setlorg Exp $  */


extern void glutSETLInit(void);
extern tuple *glutSETLGetOneEvent(void);
extern void glutSETLIdleFunc(int eventType);
extern void glutSETLDisplayFunc(int eventType);
extern void glutSETLOverlayDisplayFunc(int eventType);
extern void glutSETLReshapeFunc(int eventType);
extern void glutSETLKeyboardFunc(int eventType);
extern void glutSETLMouseFunc(int eventType);
extern void glutSETLMotionFunc(int eventType);
extern void glutSETLPassiveMotionFunc(int eventType);
extern void glutSETLVisibilityFunc(int eventType);
extern void glutSETLEntryFunc(int eventType);
extern void glutSETLSpecialFunc(int eventType);
extern void glutSETLSpaceballMotionFunc(int eventType);
extern void glutSETLSpaceballRotateFunc(int eventType);
extern void glutSETLSpaceballButtonFunc(int eventType);
extern void glutSETLButtonBoxFunc(int eventType);
extern void glutSETLDialsFunc(int eventType);
extern void glutSETLTabletMotionFunc(int eventType);
extern void glutSETLTabletButtonFunc(int eventType);
extern void glutSETLMenuStatusFunc(int eventType);
extern void glutSETLTimerFunc(int ms, int eventType, int id);
extern void glutSETLFileInputFunc(int fd, int eventType);
extern void glutSETLFileOutputFunc(int fd, int eventType);
extern void glutSETLFileExceptionFunc(int fd, int eventType);
extern void glutSETLCloseFunc(int eventType);
extern int glutSETLCreateMenu(int eventType);
extern void glutSETLDestroyMenu(int menu);
extern void *glutSETLBitmap8By13(void);
extern void *glutSETLBitmap9By15(void);
extern void *glutSETLBitmapHelvetica10(void);
extern void *glutSETLBitmapHelvetica12(void);
extern void *glutSETLBitmapHelvetica18(void);
extern void *glutSETLBitmapTimesRoman10(void);
extern void *glutSETLBitmapTimesRoman24(void);
extern void *glutSETLStrokeMonoRoman(void);
extern void *glutSETLStrokeRoman(void);
