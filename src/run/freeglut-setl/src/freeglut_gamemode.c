/*
 * freeglut_gamemode.c
 *
 * The game mode handling code.
 *
 * Copyright (c) 1999-2000 Pawel W. Olszta. All Rights Reserved.
 * Written by Pawel W. Olszta, <olszta@sourceforge.net>
 * Creation date: Thu Dec 16 1999
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included
 * in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
 * OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL
 * PAWEL W. OLSZTA BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
 * IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <unistd.h>

#define  G_LOG_DOMAIN  "freeglut-gamemode"

#include "../include/GL/freeglut.h"
#include "freeglut_internal.h"

/*
 * TODO BEFORE THE STABLE RELEASE:
 *
 *  glutGameModeString()    -- missing
 *  glutEnterGameMode()     -- X11 version
 *  glutLeaveGameMode()     -- is that correct?
 *  glutGameModeGet()       -- is that correct?
 */


/* -- PRIVATE FUNCTIONS ---------------------------------------------------- */

/*
 * Remembers the current visual settings, so that
 * we can change them and restore later...
 */
void fghRememberState( void )
{
#if TARGET_HOST_UNIX_X11

    /*
     * This highly depends on the XFree86 extensions, not approved as X Consortium standards
     */
#   ifdef X_XF86VidModeGetModeLine


    /*
     * Remember the current ViewPort location of the screen to be able to
     * restore the ViewPort on LeaveGameMode():
     */
    XF86VidModeGetViewPort(
	fgDisplay.Display,
	fgDisplay.Screen,
	&fgDisplay.DisplayViewPortX,
	&fgDisplay.DisplayViewPortY
    );

    /*
     * Remember the current pointer location before going fullscreen
     * for restoring it later:
     */
    {
	Window junk_window;
	unsigned int mask;

	XQueryPointer(
	    fgDisplay.Display, fgDisplay.RootWindow,
	    &junk_window, &junk_window,
	    &fgDisplay.DisplayPointerX, &fgDisplay.DisplayPointerY,
	    &fgDisplay.DisplayPointerX, &fgDisplay.DisplayPointerY, &mask
	);
    }

    /*
     * Query the current display settings:
     */
    fgDisplay.DisplayModeValid = 
      XF86VidModeGetModeLine(
        fgDisplay.Display,
        fgDisplay.Screen,
        &fgDisplay.DisplayModeClock,
        &fgDisplay.DisplayMode
    );

    if (!fgDisplay.DisplayModeValid)
    	fgWarning( "Runtime use of XF86VidModeGetModeLine failed.\n" );

#   else
#       warning fghRememberState: missing XFree86 video mode extensions, game mode will not change screen resolution when activated
#   endif

#elif TARGET_HOST_WIN32

/*    DEVMODE devMode; */

    /*
     * Grab the current desktop settings...
     */

/* hack to get around my stupid cross-gcc headers */
#define FREEGLUT_ENUM_CURRENT_SETTINGS -1

    EnumDisplaySettings( NULL, FREEGLUT_ENUM_CURRENT_SETTINGS, &fgDisplay.DisplayMode );

    /*
     * Make sure we will be restoring all settings needed
     */
    fgDisplay.DisplayMode.dmFields |= DM_PELSWIDTH | DM_PELSHEIGHT | DM_BITSPERPEL | DM_DISPLAYFREQUENCY;

#endif
}

/*
 * Restores the previously remembered visual settings
 */
void fghRestoreState( void )
{
#if TARGET_HOST_UNIX_X11

#   ifdef X_XF86VidModeGetAllModeLines
    /*
     * Restore the remembered pointer position:
     */
    XWarpPointer(
	fgDisplay.Display, None, fgDisplay.RootWindow, 0, 0, 0, 0,
	fgDisplay.DisplayPointerX, fgDisplay.DisplayPointerY
    );

    /*
     * This highly depends on the XFree86 extensions, not approved as X Consortium standards
     */

    if (fgDisplay.DisplayModeValid)
    {
        XF86VidModeModeInfo** displayModes;
        int i, displayModesCount;

        /*
         * Query for all the display available...
         */
        XF86VidModeGetAllModeLines(
            fgDisplay.Display,
            fgDisplay.Screen,
            &displayModesCount,
            &displayModes
        );

        /*
         * Check every of the modes looking for one that matches our demands
         */
        for( i=0; i<displayModesCount; i++ )
        {
            if( displayModes[ i ]->hdisplay == fgDisplay.DisplayMode.hdisplay &&
                displayModes[ i ]->vdisplay == fgDisplay.DisplayMode.vdisplay &&
                displayModes[ i ]->dotclock == fgDisplay.DisplayModeClock )
            {
                /*
                 * OK, this is the display mode we have been looking for...
                 */
                XF86VidModeSwitchToMode(
                    fgDisplay.Display,
                    fgDisplay.Screen,
                    displayModes[ i ]
                );

		/*
		 * Now we can restore the remembered ViewPort:
		 */
		XF86VidModeSetViewPort(
		     fgDisplay.Display,
		     fgDisplay.Screen,
		     fgDisplay.DisplayViewPortX,
		     fgDisplay.DisplayViewPortY
		);

	  	/*
	  	 * For the case this would be the last X11 call the application
	  	 * calls exit() we've to flush the X11 output queue to have the
	  	 * commands sent to the X server before the application exists.
	  	 */
	  	XFlush(fgDisplay.Display);

                return;
            }
        }
    }

#   else
#       warning fghRestoreState: missing XFree86 video mode extensions, game mode will not change screen resolution when activated
#   endif

#elif TARGET_HOST_WIN32

    /*
     * Restore the previously rememebered desktop display settings
     */
    ChangeDisplaySettings( &fgDisplay.DisplayMode, 0 );

#endif
}

/*
 * Checks the display mode settings against user's preferences
 */
GLboolean fghCheckDisplayMode( int width, int height, int depth, int refresh )
{
    /*
     * The desired values should be stored in fgState structure...
     */
    return( (width == fgState.GameModeSize.X) && (height == fgState.GameModeSize.Y) &&
            (depth == fgState.GameModeDepth)  && (refresh == fgState.GameModeRefresh) );
}

/*
 * Changes the current display mode to match user's settings
 */
GLboolean fghChangeDisplayMode( GLboolean haveToTest )
{
#if TARGET_HOST_UNIX_X11

    /*
     * This highly depends on the XFree86 extensions, not approved as X Consortium standards
     */
#   ifdef X_XF86VidModeGetAllModeLines

    /*
     * This is also used by applcations which check modes by calling
     * glutGameModeGet(GLUT_GAME_MODE_POSSIBLE), so allow the check:
     */
    if (haveToTest || fgDisplay.DisplayModeValid)
    {
        XF86VidModeModeInfo** displayModes;
        int i, displayModesCount;

        /*
         * Query for all the display available...
         */
        XF86VidModeGetAllModeLines(
            fgDisplay.Display,
            fgDisplay.Screen,
            &displayModesCount,
            &displayModes
        );

        /*
         * Check every of the modes looking for one that matches our demands
         */
        for( i=0; i<displayModesCount; i++ )
        {
            if( fghCheckDisplayMode( displayModes[ i ]->hdisplay, displayModes[ i ]->vdisplay,
                                     fgState.GameModeDepth, fgState.GameModeRefresh ) )
            {
	  	    if( haveToTest )
				return( TRUE );
                /*
                 * OKi, this is the display mode we have been looking for...
                 */
                XF86VidModeSwitchToMode(
                    fgDisplay.Display,
                    fgDisplay.Screen,
                    displayModes[ i ]
                );
                /*
                 * Return successfull...
                 */
                return( TRUE );
            }
        }
    }

    /*
     * Something must have went wrong
     */
    return( FALSE );

#   else
#       warning fghChangeDisplayMode: missing XFree86 video mode extensions, game mode will not change screen resolution when activated
#   endif

#elif TARGET_HOST_WIN32

    unsigned int    displayModes = 0, mode = 0xffffffff;
    GLboolean success = FALSE;
/*    HDC      desktopDC; */
    DEVMODE  devMode;

    /*
     * Enumerate the available display modes
     * Try to get a complete match
     */
    while( EnumDisplaySettings( NULL, displayModes, &devMode ) == TRUE )
    {
        /*
         * Does the enumerated display mode match the user's preferences?
         */
        if( fghCheckDisplayMode( devMode.dmPelsWidth,  devMode.dmPelsHeight,
                                 devMode.dmBitsPerPel, devMode.dmDisplayFrequency ) )
        {
            /*
             * OKi, we've found a matching display mode, remember its number and break
             */
            mode = displayModes;
            break;
        }

        /*
         * Switch to the next display mode, if any
         */
        displayModes++;
    }

    if ( mode == 0xffffffff )
    {
      /* then try without Display Frequency */
      displayModes = 0;

      /*
       * Enumerate the available display modes
       */
      while( EnumDisplaySettings( NULL, displayModes, &devMode ) == TRUE )
      {
        /* then try without Display Frequency */

        if( fghCheckDisplayMode( devMode.dmPelsWidth,  devMode.dmPelsHeight,
                                 devMode.dmBitsPerPel, fgState.GameModeRefresh))
        {
          /*
           * OKi, we've found a matching display mode, remember its number and break
           */
          mode = displayModes;
          break;
        }
	
        /*
         * Switch to the next display mode, if any
         */
        displayModes++;
      }
    }

    /*
     * Did we find a matching display mode?
     */
    if( mode != 0xffffffff )
    {
        int retVal = DISP_CHANGE_SUCCESSFUL;

        /*
         * Mark the values we want to modify in the display change call
         */
        devMode.dmFields |= DM_PELSWIDTH | DM_PELSHEIGHT | DM_BITSPERPEL | DM_DISPLAYFREQUENCY;

        /*
         * Change the current display mode (possibly in test mode only)
         */
        retVal = ChangeDisplaySettings( &devMode, haveToTest ? CDS_TEST : 0 );

        /*
         * I don't know if it's really needed, but looks nice:
         */
        success = (retVal == DISP_CHANGE_SUCCESSFUL) || (retVal == DISP_CHANGE_NOTUPDATED);

        /*
         * If it was not a test, remember the current screen settings
         */
        if( !haveToTest && success )
        {
            fgState.GameModeSize.X  = devMode.dmPelsWidth;
            fgState.GameModeSize.Y  = devMode.dmPelsHeight;
            fgState.GameModeDepth   = devMode.dmBitsPerPel;
            fgState.GameModeRefresh = devMode.dmDisplayFrequency;
        }
    }

    /*
     * Otherwise we must have failed somewhere
     */
    return( success );

#endif
}


/* -- INTERFACE FUNCTIONS -------------------------------------------------- */

/*
 * Sets the game mode display string
 */
void FGAPIENTRY glutGameModeString( const char* string )
{
    int width = 640, height = 480, depth = 16, refresh = 72;

    /*
     * This one seems a bit easier than glutInitDisplayString. The bad thing
     * about it that I was unable to find the game mode string definition, so
     * that I assumed it is: "[width]x[height]:[depth]@[refresh rate]", which
     * appears in all GLUT game mode programs I have seen to date.
     */
    if( sscanf( string, "%ix%i:%i@%i", &width, &height, &depth, &refresh ) != 4 )
        if( sscanf( string, "%ix%i:%i", &width, &height, &depth ) != 3 )
            if( sscanf( string, "%ix%i@%i", &width, &height, &refresh ) != 3 )
                if( sscanf( string, "%ix%i", &width, &height ) != 2 )
                    if( sscanf( string, ":%i@%i", &depth, &refresh ) != 2 )
                        if( sscanf( string, ":%i", &depth ) != 1 )
                            if( sscanf( string, "@%i", &refresh ) != 1 )
                                fgWarning( "unable to parse game mode string `%s'", string );

    /*
     * Hopefully it worked, and if not, we still have the default values
     */
    fgState.GameModeSize.X  = width;
    fgState.GameModeSize.Y  = height;
    fgState.GameModeDepth   = depth;
    fgState.GameModeRefresh = refresh;
}

/*
 * Enters the game mode
 */
int FGAPIENTRY glutEnterGameMode( void )
{
    /*
     * Check if a game mode window already exists...
     */
    if( fgStructure.GameMode != NULL )
    {
        /*
         * ...if so, delete it before proceeding...
         */
        fgAddToWindowDestroyList( fgStructure.GameMode, TRUE );
    }
    else
    {
        /*
         * ...otherwise remember the current resolution, etc.
         */
        fghRememberState();
    }

    /*
     * We are ready to change the current screen's resolution now
     */
    if( fghChangeDisplayMode( FALSE ) == FALSE )
    {
	      fgWarning( "failed to change screen settings" );
        return( FALSE );
    }

    /*
     * Finally, have the game mode window created
     */
    fgStructure.GameMode = fgCreateWindow( 
        NULL, "FREEGLUT", 0, 0, fgState.GameModeSize.X, fgState.GameModeSize.Y, TRUE 
    );

#if TARGET_HOST_UNIX_X11

    /* Move the window up to the topleft corner */
    XMoveWindow(fgDisplay.Display, fgStructure.Window->Window.Handle, 0, 0);

    /*
     * Sync needed to avoid a real race, the Xserver must have really created
     * the window before we can grab the pointer into it:
     */
    XSync(fgDisplay.Display, False);

    /* Move the Pointer to the middle of the fullscreen window */
    XWarpPointer(
	fgDisplay.Display,
	None, 
	fgDisplay.RootWindow,
	0, 0, 0, 0,
	fgState.GameModeSize.X/2, fgState.GameModeSize.Y/2
    );

    /*
     * Grab the pointer to confine it into the window after the calls to
     * XWrapPointer() which ensure that the pointer really enters the window.
     *
     * We also need to wait here until XGrabPointer() returns GrabSuccess,
     * otherwise the new window is not viewable yet and if the next function
     * (XSetInputFocus) is called with a not yet viewable window, it will exit
     * the application which we have to aviod, so wait until it's viewable:
     */
    while (GrabSuccess != XGrabPointer(
	    fgDisplay.Display, fgStructure.GameMode->Window.Handle,
	    TRUE, ButtonPressMask|ButtonReleaseMask|ButtonMotionMask
		|PointerMotionMask,
	    GrabModeAsync, GrabModeAsync,
	    fgStructure.GameMode->Window.Handle, None, CurrentTime)) {
	usleep (100);
    }

    /*
     * Change input focus to the new window. This will exit the application
     * if the new window is not viewable yet, see the XGrabPointer loop above.
     */
    XSetInputFocus(
        fgDisplay.Display,
        fgStructure.GameMode->Window.Handle,
        RevertToNone,
        CurrentTime
    );

#   ifdef X_XF86VidModeSetViewPort

    if (fgDisplay.DisplayModeValid) {
	int x, y;
	Window child;

	/*
	 * Change to viewport to the window topleft edge:
	 */
	XF86VidModeSetViewPort(fgDisplay.Display, fgDisplay.Screen, 0, 0);

	/*
	 * Final window repositioning: It could be avoided using an undecorated
	 * window using override_redirect, but this * would possily require more
	 * changes and investigation.
	 */

        /* Get the current postion of the drawable area on screen */
	XTranslateCoordinates(
	    fgDisplay.Display,
	    fgStructure.Window->Window.Handle,
	    fgDisplay.RootWindow,
	    0, 0, &x, &y,
	    &child
	);

	/* Move the decorataions out of the topleft corner of the display */
	XMoveWindow(fgDisplay.Display, fgStructure.Window->Window.Handle, -x, -y);
    }

#endif

    /*
     * Grab the keyboard, too
     */
    XGrabKeyboard(
        fgDisplay.Display,
        fgStructure.GameMode->Window.Handle,
        FALSE,
        GrabModeAsync, GrabModeAsync,
        CurrentTime
    );

#endif

    /*
     * Return successfull
     */
    return( TRUE );
}

/*
 * Leaves the game mode
 */
void FGAPIENTRY glutLeaveGameMode( void )
{
    freeglut_return_if_fail( fgStructure.GameMode != NULL );

    /*
     * First of all, have the game mode window destroyed
     */
    fgAddToWindowDestroyList( fgStructure.GameMode, TRUE );

#if TARGET_HOST_UNIX_X11

    /*
     * Ungrab the mouse and keyboard
     */
    XUngrabPointer( fgDisplay.Display, CurrentTime );
    XUngrabKeyboard( fgDisplay.Display, CurrentTime );

#endif

    /*
     * Then, have the desktop visual settings restored
     */
    fghRestoreState();
}

/*
 * Returns information concerning the freeglut game mode
 */
int FGAPIENTRY glutGameModeGet( GLenum eWhat )
{
    /*
     * See why are we bothered
     */
    switch( eWhat )
    {
    case GLUT_GAME_MODE_ACTIVE:
        /*
         * Check if the game mode is currently active
         */
        return( fgStructure.GameMode != NULL );

    case GLUT_GAME_MODE_POSSIBLE:
        /*
         * Check if the current game mode settings are valid
         */
        return( fghChangeDisplayMode( TRUE ) );

    case GLUT_GAME_MODE_WIDTH:
        /*
         * The game mode screen width
         */
        return( fgState.GameModeSize.X );

    case GLUT_GAME_MODE_HEIGHT:
        /*
         * The game mode screen height
         */
        return( fgState.GameModeSize.Y );

    case GLUT_GAME_MODE_PIXEL_DEPTH:
        /*
         * The game mode pixel depth
         */
        return( fgState.GameModeDepth );

    case GLUT_GAME_MODE_REFRESH_RATE:
        /*
         * The game mode refresh rate
         */
        return( fgState.GameModeRefresh );

    case GLUT_GAME_MODE_DISPLAY_CHANGED:
        /*
         * This is true if the game mode has been activated successfully..
         */
        return( fgStructure.GameMode != NULL );
    }

    return( -1 );
}

/*** END OF FILE ***/




