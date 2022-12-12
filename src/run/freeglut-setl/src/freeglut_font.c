/*
 * freeglut_font.c
 *
 * Bitmap and stroke fonts displaying.
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

#define  G_LOG_DOMAIN  "freeglut-font"

#include "../include/GL/freeglut.h"
#include "freeglut_internal.h"

/*
 * TODO BEFORE THE STABLE RELEASE:
 *
 *  Test things out ...
 */

/* -- IMPORT DECLARATIONS -------------------------------------------------- */

/*
 * These are the font faces defined in freeglut_font_data.c file:
 */
extern SFG_Font fgFontFixed8x13;
extern SFG_Font fgFontFixed9x15;
extern SFG_Font fgFontHelvetica10;
extern SFG_Font fgFontHelvetica12;
extern SFG_Font fgFontHelvetica18;
extern SFG_Font fgFontTimesRoman10;
extern SFG_Font fgFontTimesRoman24;
extern SFG_StrokeFont fgStrokeRoman;
extern SFG_StrokeFont fgStrokeMonoRoman;


/* -- PRIVATE FUNCTIONS ---------------------------------------------------- */

/*
 * Matches a font ID with a SFG_Font structure pointer.
 * This was changed to match the GLUT header style.
 */
static SFG_Font* fghFontByID( void* font )
{
  if( font == GLUT_BITMAP_8_BY_13        ) return( &fgFontFixed8x13    );
  if( font == GLUT_BITMAP_9_BY_15        ) return( &fgFontFixed9x15    );
  if( font == GLUT_BITMAP_HELVETICA_10   ) return( &fgFontHelvetica10  );
  if( font == GLUT_BITMAP_HELVETICA_12   ) return( &fgFontHelvetica12  );
  if( font == GLUT_BITMAP_HELVETICA_18   ) return( &fgFontHelvetica18  );
  if( font == GLUT_BITMAP_TIMES_ROMAN_10 ) return( &fgFontTimesRoman10 );
  if( font == GLUT_BITMAP_TIMES_ROMAN_24 ) return( &fgFontTimesRoman24 );
  fgError( "font 0x%08x not found", font );
  return 0; /*** XXX NOT REACHED ***/
}

/*
 * Matches a font ID with a SFG_StrokeFont structure pointer.
 * This was changed to match the GLUT header style.
 */
static SFG_StrokeFont* fghStrokeByID( void* font )
{
  if( font == GLUT_STROKE_ROMAN      ) return( &fgStrokeRoman     );
  if( font == GLUT_STROKE_MONO_ROMAN ) return( &fgStrokeMonoRoman );
  fgError( "stroke font 0x%08x not found", font );
  return 0; /*** XXX NOT REACHED ***/
}


/* -- INTERFACE FUNCTIONS -------------------------------------------------- */

/*
 * Draw a bitmap character
 */
void FGAPIENTRY glutBitmapCharacter( void* fontID, int character )
{
  const GLubyte* face;
  SFG_Font* font = fghFontByID( fontID );

  freeglut_return_if_fail( character >= 1 && character < 256 );

  /*
   * Find the character we want to draw (???)
   */
  face = font->Characters[ character - 1 ];

  glPushClientAttrib( GL_CLIENT_PIXEL_STORE_BIT );
  glPixelStorei( GL_UNPACK_SWAP_BYTES,  GL_FALSE );
  glPixelStorei( GL_UNPACK_LSB_FIRST,   GL_FALSE );
  glPixelStorei( GL_UNPACK_ROW_LENGTH,  0        );
  glPixelStorei( GL_UNPACK_SKIP_ROWS,   0        );
  glPixelStorei( GL_UNPACK_SKIP_PIXELS, 0        );
  glPixelStorei( GL_UNPACK_ALIGNMENT,   1        );
  glBitmap(
      face[ 0 ], font->Height,      /* The bitmap's width and height  */
      font->xorig, font->yorig,     /* The origin in the font glyph   */
      (float)(face[ 0 ]), 0.0,      /* The raster advance -- inc. x,y */
      (face + 1)                    /* The packed bitmap data...      */
  );
  glPopClientAttrib();
}

void FGAPIENTRY glutBitmapString( void* fontID, const unsigned char *string )
{
  int c;
  int numchar = strlen ( string ) ;
  SFG_Font* font = fghFontByID( fontID );
  float raster_position[4] ;

  glGetFloatv ( GL_CURRENT_RASTER_POSITION, raster_position ) ;
  glPushClientAttrib( GL_CLIENT_PIXEL_STORE_BIT );
  glPixelStorei( GL_UNPACK_SWAP_BYTES,  GL_FALSE );
  glPixelStorei( GL_UNPACK_LSB_FIRST,   GL_FALSE );
  glPixelStorei( GL_UNPACK_ROW_LENGTH,  0        );
  glPixelStorei( GL_UNPACK_SKIP_ROWS,   0        );
  glPixelStorei( GL_UNPACK_SKIP_PIXELS, 0        );
  glPixelStorei( GL_UNPACK_ALIGNMENT,   1        );

  /*
   * Step through the string, drawing each character.
   * A carriage return will simply translate the next character's insertion
   * point back to the start of the line and down one line.
   */
  for( c = 0; c < numchar; c++ )
  {
    if ( string[c] == '\n' )
    {
      raster_position[1] -= (float)font->Height ;
      glRasterPos4fv ( raster_position ) ;
    }
    else  /* Not an EOL, draw the bitmap character */
    {
      const GLubyte* face = font->Characters[ string[ c ] - 1 ] ;

      glBitmap(
          face[ 0 ], font->Height,      /* The bitmap's width and height  */
          font->xorig, font->yorig,     /* The origin in the font glyph   */
          (float)(face[ 0 ]), 0.0,      /* The raster advance -- inc. x,y */
          (face + 1)                    /* The packed bitmap data...      */
      ) ;
    }
  }
  glPopClientAttrib();
}

/*
 * Returns the width in pixels of a font's character
 */
int FGAPIENTRY glutBitmapWidth( void* fontID, int character )
{
  SFG_Font* font = fghFontByID( fontID );

  freeglut_return_val_if_fail( character > 0 && character < 256, 0 );
  return( *(font->Characters[ character - 1 ]) );
}

/*
 * Return the width of a string drawn using a bitmap font
 */
int FGAPIENTRY glutBitmapLength( void* fontID, const unsigned char* string )
{
  int c, length = 0, this_line_length = 0;
  SFG_Font* font = fghFontByID( fontID );
  int numchar = strlen ( string ) ;

  for( c = 0; c < numchar; c++ )
  {
    if ( string[ c ] == '\n' )  /* EOL; reset the length of this line */
    {
      if ( length < this_line_length ) length = this_line_length ;
      this_line_length = 0 ;
    }
    else  /* Not an EOL, increment the length of this line */
      this_line_length += *(font->Characters[ string[ c ] - 1 ]) ;
  }
  if ( length < this_line_length )
    length = this_line_length ;

  return( length );
}

/*
 * Returns the height of a bitmap font
 */
int FGAPIENTRY glutBitmapHeight( void* fontID )
{
  SFG_Font* font = fghFontByID( fontID );
  return( font->Height );
}

/*
 * Draw a stroke character
 */
void FGAPIENTRY glutStrokeCharacter( void* fontID, int character )
{
  const SFG_StrokeChar *schar;
  const SFG_StrokeStrip *strip;
  int i, j;
  SFG_StrokeFont* font = fghStrokeByID( fontID );

  freeglut_return_if_fail( character >= 0 && character < font->Quantity );

  schar = font->Characters[character];
  freeglut_return_if_fail( schar );
  strip = schar->Strips;

  for (i = 0; i < schar->Number; i++, strip++)
  {
    glBegin(GL_LINE_STRIP);
    for(j = 0; j < strip->Number; j++)
    {
      glVertex2f(strip->Vertices[j].X, strip->Vertices[j].Y);
    }
    glEnd();
  }
  glTranslatef(schar->Right, 0.0, 0.0);
}

void FGAPIENTRY glutStrokeString( void* fontID, const unsigned char *string )
{
  int c, i, j;
  int numchar = strlen ( string ) ;
  float length = 0.0 ;
  SFG_StrokeFont* font = fghStrokeByID( fontID );

  /*
   * Step through the string, drawing each character.
   * A carriage return will simply translate the next character's insertion
   * point back to the start of the line and down one line.
   */
  for( c = 0; c < numchar; c++ )
  {
    if ( string[ c ] < font->Quantity )
    {
      if ( string[c] == '\n' )
      {
        glTranslatef ( -length, -(float)(font->Height), 0.0 ) ;
        length = 0.0 ;
      }
      else  /* Not an EOL, draw the bitmap character */
      {
        const SFG_StrokeChar *schar = font->Characters[string[c]];
        if ( schar != NULL )
        {
          const SFG_StrokeStrip *strip = schar->Strips;

          for (i = 0; i < schar->Number; i++, strip++)
          {
            glBegin(GL_LINE_STRIP);
            for(j = 0; j < strip->Number; j++)
              glVertex2f(strip->Vertices[j].X, strip->Vertices[j].Y);

            glEnd();
          }

          length += schar->Right ;
          glTranslatef(schar->Right, 0.0, 0.0);
        }
      }
    }
  }
}

/*
 * Return the width in pixels of a stroke character
 */
int FGAPIENTRY glutStrokeWidth( void* fontID, int character )
{
  const SFG_StrokeChar *schar;
  SFG_StrokeFont* font = fghStrokeByID( fontID );

  freeglut_return_val_if_fail( character >= 0 && character < font->Quantity, 0 );
  schar = font->Characters[character];
  freeglut_return_val_if_fail( schar, 0 );

  return ((int)(schar->Right + 0.5));
}

/*
 * Return the width of a string drawn using a stroke font
 */
int FGAPIENTRY glutStrokeLength( void* fontID, const unsigned char* string )
{
  int c;
  float length = 0.0;
  float this_line_length = 0.0 ;
  SFG_StrokeFont* font = fghStrokeByID( fontID );
  int numchar = strlen ( string ) ;

  for( c = 0; c < numchar; c++ )
  {
    if ( string[ c ] < font->Quantity )
    {
      if ( string[ c ] == '\n' )  /* EOL; reset the length of this line */
      {
        if ( length < this_line_length ) length = this_line_length ;
        this_line_length = 0.0 ;
      }
      else  /* Not an EOL, increment the length of this line */
      {
        const SFG_StrokeChar *schar = font->Characters[string[c]];
        if ( schar != NULL )
          this_line_length += schar->Right ;
      }
    }
  }
  if ( length < this_line_length )
    length = this_line_length ;
  return( (int)(length+0.5) );
}

/*
 * Returns the height of a stroke font
 */
GLfloat FGAPIENTRY glutStrokeHeight( void* fontID )
{
    SFG_StrokeFont* font = fghStrokeByID( fontID );
    return( font->Height );
}

/*** END OF FILE ***/
