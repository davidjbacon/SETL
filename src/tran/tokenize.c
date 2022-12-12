/*  ===  Break input into tokens  ==================================  */

/*  $Id: tokenize.c,v 1.29 2022/12/10 23:35:26 setlorg Exp $  */

/*  Free software (c) dB - see file COPYING for license (GPL).  */


/*
 *  This is the lexical analysis module ("lexer") of the SETL translator.
 *
 *  The file 'lexicon' lists all the SETL tokens.  It is preprocessed
 *  at build time to form the files 'Tokens' (full of %token codes
 *  for the 'grammar' file to %include), and 'Tinits', which toksym.c
 *  #includes to populate the map of textual form to token code;
 *  calls from here to get_tcode() and toksymname() use that map.
 *
 *   -  tokenize() is the principal routine here.  It creates an array
 *      'tokens' by scanning the input with scan().  Each token in that
 *      array has several pieces of information in it, such as its
 *      location in the source text, and its lexical category (code).
 *
 *  The lexical categories in SETL (including extensions) are as
 *  follows.  All but the first two definitions here are for tokens
 *  in the 'tokens' array:
 *
 *   -  Whitespace is any sequence of characters from the set
 *      {blank, newline, carriage return, horizontal tab, vertical
 *      tab, form feed}.
 *
 *   -  A comment is any sequence of characters beginning with a
 *      dollar sign ('$') or a pair of hyphens ('--') and ending
 *      with a newline character.  It is lexically equivalent to
 *      whitespace.  C-style comments are now also allowed.
 *
 *   -  A name is any letter followed by any combination of letters,
 *      digits, and underscores.  Nominally it is a keyword or a Name,
 *      but its code in a token in the tokens array is changed to
 *      Ref_name ("refinement name") if a '::' token precedes it.
 *      Also, preparse() scans the tokens array shallowly for COMMAND
 *      and OP (OPERATOR) headers, and changes Name to Bop_name,
 *      Uop_name, Command_name, or Accum (which is the code for a
 *      binary operator that is just before ':=' in an assigning
 *      form) wherever it thinks it should in the tokens array.
 *
 *   -  A real literal (Real_) is as described before the spanreal()
 *      impl below.
 *
 *   -  An integer literal (Integer) is any string of digits that is
 *      not part of a real literal, or (thanks to the SETL2 part of
 *      the heritage) a string of digits indicating a radix r in the
 *      range 2 to 36 followed by a sharp sign ('#') and digits from
 *      the set {c : c in '0123456789'(1..r min 10)} +
 *      {c : c in 'abcdefghijklmnopqrstuvwxyz'(1..(r-10) max 0)} +
 *      {c : c in 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'(1..(r-10) max 0)},
 *      optionally followed by another sharp sign.
 *
 *   -  A string literal (String) is an opening apostrophe ("'")
 *      followed by an arbitrary run of "printable" characters,
 *      followed by a closing apostrophe (also "'").  Certain
 *      "backslash escapes" are allowed between the opening and
 *      closing apostrophes (see below).  A consecutive pair of
 *      apostrophes within a string literal stands for the apostrophe
 *      character itself.  For SETL2 compatibility and because it
 *      seems righteous, strings may be denoted using double quotes
 *      ('"'), and two of those in a row then stand for one (and the
 *      apostrophe is treated as a regular character).
 *
 *   -  "Machine code" (Machine) is anything between '[:' and ':]', and
 *      is currently meaningless.  In fact it isn't even recognized yet.
 *
 *   -  A keyword is any name that is recognized as being a SETL
 *      reserved word.  The file 'lexicon' plus any 'custom.lexicon'
 *      files in use list these.  Keyword recognition is subject to
 *      the setting of the 'keycase' option (see below).
 *
 *   -  A user tag is any non-keyword name or op name (identifier).
 *      This usage of "tag" is an ancient Algol 68 practice, not to be
 *      confused with more modern meanings of tag such as dynamic
 *      type code.
 *
 *   -  A special symbol is a sequence of characters from the set
 *      {c : c in '.:;,#=!?|/+-*(){}[]<>^'} that forms a valid operator
 *      or punctuation symbol of SETL.  (See the 'lexicon' file.)
 *      Whitespace can be used to make the boundaries of special
 *      symbols unambiguous.  Where more than one interpretation
 *      of a sequence of special symbols is possible, the longest
 *      such sequence will be taken to represent the token, even
 *      if this leads to erroneous subsequent interpretation.
 *
 *   -  "Embedded C++ code" (C_code) is anything between backslash-c
 *      ('\c') and backslash-s ('\s') that is not part of a C++ comment
 *      or C++ string or character literal.  Like "machine code", it
 *      seems a horrible convention for embedding other-language code,
 *      and has no coherent definition yet.  It is only recognized
 *      by this lexer if EMBEDDED_C is #defined.
 *
 *   -  Any line that begins with '#!' or appears to be produced by
 *      the C preprocessor (the line begins with a '#' token
 *      followed by a line-number token) is not considered part of
 *      the SETL source text.
 *
 *  The characters that may appear in SETL string literals are the
 *  letters, digits, blank, horizontal tab, and the 32 "glyphs":
 *
 *    `~!@#$%^&*()-_=+[{]}\|;:'",<.>/?
 *
 *  Other codes may be specified inside string literals by using the
 *  backslash character as an "escape" prefix.  Allowed after the
 *  backslash are:
 *
 *   -  up to three octal digits in the range 0 to 377,
 *
 *   -  an x followed by 1 or 2 hexadecimal digits,
 *
 *   -  any of the lowercase letters {a,b,f,n,r,t,v},
 *
 *   -  any of the 32 glyphs, or
 *
 *   -  blank.
 *
 *  A backslash before a glyph or blank stands for that glyph or blank.
 *  It is an error to put anything else after the backslash, to leave
 *  room for future expansion (new escape sequences).
 *
 *  Currently there is no support for internationalization nor for
 *  "wide" characters.  The locale is treated as if it is "C".
 *
 *  Interpretation of names as keywords versus user-defined tags
 *  (identifiers) is determined by the so-called "stropping regime"
 *  (again with the ancient Algol 68 terminology, eh).  By default,
 *  all names are recognized case-insensitively, but:
 *
 *   -  The 'keycase' option specifies whether keywords are
 *      recognized only in uppercase, only in lowercase, or
 *      case-insensitively (anycase).
 *
 *   -  The 'tagcase' option allows user tags to be recognized
 *      only in uppercase, only in lowercase, case-sensitively
 *      (mixedcase), or case-insensitively (anycase).
 */


/* ------------------------------------------------------------------ */

#include "setltran.h"
#include "y.tab.h"

/* #define EMBEDDED_C 1 */  /* enable \c...\s recognition (see above) */
/* #undef EMBEDDED_C */  /* no \c...\s recognition */

/* Form of a numeric literal */
enum numform {intnum, radnum, realnum};  /* e.g. 55, 16#5a, 1.5 & 1e9 */

/* Local routines */
static void scan(void);
static long spannewline(long m);
static long spancomment(long m);
static long spanalphanum(long m);
static bool unary_ok(void);
static enum numform classify_numlit(long m);
static long spanreal(long m);
static long spanint(long m);
static long spanrad(long m);
static long spanstring(long m);
static long spandqstring(long m);
static long spanvariadic(long m);
#ifdef EMBEDDED_C
static long spanc(long m);
#endif


/* ------------------------------------------------------------------ */

/* Public data */

extern bool pre_parsing;        /* defined in parse.c */


/* Local data */

#define TOKCHUNK 1024   /* #tokens to allocate for tokens initially */

/* Top-level character classification */
enum chartype1 {bad,white,letter,digit,bar,unde,dollar,dot,dquote,
 apostrophe,colon,semicolon,comma,sharp,equal,query,slash,star,plus,
 minus,lparen,rparen,lbrace,rbrace,lbracket,rbracket,langle,rangle,
 caret,backslash};
static enum chartype1 ttype[CHARSETSIZE];
#define chartype(k) ttype[(unsigned char)(src[k])]
static char whitetrash[] = " \f\n\r\t\v";
static char letters[] =
 "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz";
static char digits[] = "0123456789";
static char bars[] = "|!";
static char blanks[] = " ";  /* the blank character */

/* For tag scanning */
enum chartype2 {tagbreak,tagtail};
static enum chartype2 alphanum[CHARSETSIZE];
static char tagtails[] =
 "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz_0123456789";

/* For string literal processing */
enum chartype3 {unescapable,repself,control,hexlead,octlead};
static enum chartype3 escapee[CHARSETSIZE];
static bool strok[CHARSETSIZE];
static bool hexdigit[CHARSETSIZE];
static bool octdigit[CHARSETSIZE];
static char glyphs[] = "`~!@#$%^&*()-_=+[{]}\\|;:\'\",<.>/?";
static char controls[] = "abfnrtv";
static char hexleads[] = "x";
static char hexes[] = "0123456789abcdefABCDEF";
static char octleads[] = "0123";
static char octals[] = "01234567";

#ifdef EMBEDDED_C
/* For embedded C/C++ code processing */
enum chartype4 {octalize,musttwin,ctrlchar,already_pretty};
static enum chartype4 cppchar[CHARSETSIZE];
static char twinners[] = "\'\\";
static char raw_ctrl[] = "\a\b\f\n\r\t\v";
#endif

/* For quick on-the-fly conversions */
static int digval[CHARSETSIZE];

/* For checking radix-prefixed integer literals */
static int raddigval[CHARSETSIZE];
static char lc_digs[] = "abcdefghijklmnopqrstuvwxyz";
static char uc_digs[] = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";

static char *strbuf;
#define STRCHUNK 64     /* #chars to allocate for strbuf initially */
long curstrchunk;


/* ------------------------------------------------------------------ */

/* Initialize character tables */
void charinit(void)
{
  int i,j,n;

# define deftab(t,c) {n=CHARSETSIZE; for(i=0;i<n;i++)t[i]=c;}
# define cchar(t,s,c) {for(i=0;s[i]!='\0';i++)t[(unsigned char)(s[i])]=c;}

  deftab (ttype,bad);
  cchar  (ttype,whitetrash,white);
  cchar  (ttype,letters,letter);
  cchar  (ttype,digits,digit);
  cchar  (ttype,bars,bar);
  cchar  (ttype,"_",unde);
  cchar  (ttype,"\"",dquote);
  cchar  (ttype,"$",dollar);
  cchar  (ttype,".",dot);
  cchar  (ttype,"\'",apostrophe);
  cchar  (ttype,":",colon);
  cchar  (ttype,";",semicolon);
  cchar  (ttype,",",comma);
  cchar  (ttype,"#",sharp);
  cchar  (ttype,"=",equal);
  cchar  (ttype,"?",query);
  cchar  (ttype,"/",slash);
  cchar  (ttype,"*",star);
  cchar  (ttype,"+",plus);
  cchar  (ttype,"-",minus);
  cchar  (ttype,"(",lparen);
  cchar  (ttype,")",rparen);
  cchar  (ttype,"{",lbrace);
  cchar  (ttype,"}",rbrace);
  cchar  (ttype,"[",lbracket);
  cchar  (ttype,"]",rbracket);
  cchar  (ttype,"<",langle);
  cchar  (ttype,">",rangle);
  cchar  (ttype,"^",caret);
#ifdef EMBEDDED_C
  cchar  (ttype,"\\",backslash);
#endif

  deftab (alphanum,tagbreak);
  cchar  (alphanum,tagtails,tagtail);

  deftab (strok,false);
  cchar  (strok,letters,true);
  cchar  (strok,digits,true);
  cchar  (strok,blanks,true);
  cchar  (strok,glyphs,true);
  /*
   *  I deprecate the use of raw tab characters in string literals,
   *  but allow it because it occurs so much in the Ada/Ed source:
   */
  cchar  (strok,"\t",true);

  deftab (escapee,unescapable);
  cchar  (escapee,glyphs,repself);
  cchar  (escapee,blanks,repself);
  cchar  (escapee,controls,control);
  cchar  (escapee,hexleads,hexlead);
  cchar  (escapee,octleads,octlead);

  deftab (hexdigit,false);
  cchar  (hexdigit,hexes,true);

  deftab (octdigit,false);
  cchar  (octdigit,octals,true);

#ifdef EMBEDDED_C
  deftab (cppchar,octalize);
  /* The order of initializations here is significant:  */
  cchar  (cppchar,letters,already_pretty);
  cchar  (cppchar,digits,already_pretty);
  cchar  (cppchar,blanks,already_pretty);
  cchar  (cppchar,glyphs,already_pretty);
  cchar  (cppchar,twinners,musttwin);  /* overrides a coupla pretties */
  cchar  (cppchar,raw_ctrl,ctrlchar);
#endif

  deftab (digval,-1);
  j = 0;  /* incremented each time round the implied loop in this: */
  cchar  (digval,digits,j++);  /* digits assumed to be in order 0..9 */

  deftab (raddigval,-1);  /* theory like that of digval above */
  j = 0;  cchar (raddigval,digits,j++);
  j = 10; cchar (raddigval,lc_digs,j++);
  j = 10; cchar (raddigval,uc_digs,j++);

} /* end charinit */

/* .................................................................. */

/* Tokenize input - called by main routine */
void tokenize(void)
{
  curstrchunk = STRCHUNK;  /* allocate initial flexible string buffer */
  getmem(strbuf, curstrchunk, char);
  scan();               /* basic token determination */
} /* end tokenize */

/* .................................................................. */

/*
 *  Define a bunch of macros for scan(), and then scan() itself.
 */

/* Create list of tokens */
# define addtoken(t)\
  {\
    if (ntokens >= curchunk) {\
      resize(tokens, 2*curchunk, token);\
      curchunk *= 2;\
    }\
    t.tn = ntokens;\
    tokens[ntokens++] = t;\
  }
# define addname\
  {\
    j = spanalphanum(i+1);\
    t.srcloc = i;\
    t.srcend = j;\
    t.graphic = strnmake(&src[i],j-i);\
    toksymname(&t);\
    addtoken(t);\
    i = j;\
  }
# define addid\
  {\
    j = spanalphanum(i+1);\
    t.srcloc = i;\
    t.srcend = j;\
    t.graphic = strnmake(&src[i],j-i);\
    toksymid(&t);\
    addtoken(t);\
    i = j;\
  }
# define addstring\
  {\
    j = spanstring(i);\
    t.srcloc = i;\
    t.srcend = j;\
    t.graphic = strmake(strbuf);\
    t.code = String;\
    addtoken(t);\
    i = j;\
  }
# define adddqstring\
  {\
    j = spandqstring(i);\
    t.srcloc = i;\
    t.srcend = j;\
    t.graphic = strmake(strbuf);\
    t.code = String;\
    addtoken(t);\
    i = j;\
  }
# define addreal\
  {\
    j = spanreal(i);\
    t.srcloc = i;\
    t.srcend = j;\
    t.graphic = strnmake(&src[i],j-i);\
    t.code = Real_;\
    addtoken(t);\
    i = j;\
  }
# define addint\
  {\
    j = spanint(i);\
    t.srcloc = i;\
    t.srcend = j;\
    t.graphic = strnmake(&src[i],j-i);\
    t.code = Integer;\
    addtoken(t);\
    i = j;\
  }
# define addrad\
  {\
    j = spanrad(i);\
    t.srcloc = i;\
    t.srcend = j;\
    t.graphic = strnmake(&src[i],j-i);\
    t.code = Integer;\
    addtoken(t);\
    i = j;\
  }
# define addmach\
  {\
    /* j assumed to be defined already */\
    t.srcloc = i;\
    t.srcend = j;\
    t.graphic = strnmake(&src[i],j-i);\
    t.code = Machine;\
    addtoken(t);\
    i = j;\
  }
# define addlit(s,k)\
  {\
    j = i+(k);\
    t.srcloc = i;\
    t.srcend = j;\
    t.graphic = s;\
    t.code = get_tcode(s);\
    addtoken(t);\
    i = j;\
  }
# define add_plus_or_minus\
  {\
    switch (chartype(i)) {\
    case plus:   addlit("+",1); break;\
    case minus:  addlit("-",1); break;\
    default:  unexpected(chartype(i));\
    }\
  }
#ifdef EMBEDDED_C
#define addc\
  {\
    j = spanc(i);\
    t.srcloc = i;\
    t.srcend = j;\
    t.graphic = strmake(strbuf);\
    t.code = C_code;\
    addtoken(t);\
    i = j;\
  }
#endif
static void scan(void)
{
  long i,j,curchunk;
  token t;
  t.type = N_Token;
  curchunk = TOKCHUNK;
  getmem(tokens, curchunk, token);
  ntokens = 0;
  for (i=0; i<srclen; ) {
    switch (chartype(i)) {
    case bad:  tranerr(i,"not in SETL character set");
    case white:  i++; continue;
    case letter:
      if (ntokens==0 || tokens[ntokens-1].code!='.') {
        addname; continue;  /* kwd or user tag */
      } else {  /* after dot */
        addid; continue;  /* treat as user tag */
      }
    case digit:  {  /* unsigned numeric literal */
      enum numform num = classify_numlit(i);
      switch (num) {
      case intnum:   addint;  continue;
      case radnum:   addrad;  continue;
      case realnum:  addreal; continue;
      default:  unexpected(num);  /* bad enum value */
      }
    }
    case bar:  addlit("|",1); continue;
    case unde:  tranerr(i,"no SETL token begins with an underscore");
    case dollar:  i = spannewline(i+1); continue;
    case dot:
      switch (chartype(i+1)) {
      case digit:  addreal; continue;
      case dot:
        if (chartype(i+2)==dot) { addlit("..",3); }
        else                    { addlit("..",2); }
        continue;
      default:  addlit(".",1); continue;
      }
    case dquote:  adddqstring; continue;
    case apostrophe:  addstring; continue;
    case colon:
      switch (chartype(i+1)) {
      case colon:
        if (ntokens > 0) {
          switch (tokens[ntokens-1].code) {
          case Name:
            tokens[ntokens-1].code = Ref_name;
            addlit("::",2); continue;
          default:
            tranerr(i,"\"::\" must be preceded by refinement name");
          }
        } else tranerr(i,"program cannot begin with double colon");
      case equal:
        if (ntokens > 0) {
          switch (tokens[ntokens-1].code) {
          case '+':   case '-':   case '*':   case '/':
          case Bop1:  case Bop2:  case Bop3:  case Bop4:  case Bop5:
          case Bop6:  case Bop7:  case Bop8:  case Bop9:  case Bop10:
          case Bop11: case Bop12: case Bop13: case Bop14: case Bop15:
          case Bop16:
            tokens[ntokens-1].code = Accum;
            addlit(":=",2); continue;
          default:  addlit(":=",2); continue;
          }
        } else tranerr(i,"program cannot begin with a \":=\" sign");
      default:  addlit(":",1); continue;
      }
    case semicolon:  addlit(";",1); continue;
    case comma:  addlit(",",1); continue;
    case sharp:
      if (i==0 || src[i-1]=='\n') {  /* '#' in first column */
        if (i+2<srclen && src[i+1]=='!') {  /* #! */
          i = spannewline(i+2);  /* advance past newline */
          continue;
        }
        {
          long n,f,g;
          if (cpp_emission(&i,&n,&f,&g)) {  /* # or #line directive */
            assert (src[i] == '\n');
            ++i;  /* advance i past newline */
            continue;
          }
        }
      }
      addlit("#",1); continue;
    case equal:
      switch (chartype(i+1)) {
      case rangle:  addlit("=>",2); continue;
      default:  addlit("=",1); continue;
      }
    case query:  addlit("?",1); continue;
    case slash:
      switch (chartype(i+1)) {
      case equal:  addlit("/=",2); continue;
      case rparen:  addlit("]",2); continue;
      case star:
        i = spancomment(i);
        continue;
      default:  addlit("/",1); continue;
      }
    case star:
      switch (chartype(i+1)) {
      case star:  addlit("**",2); continue;
      default:  addlit("*",1); continue;
      }
    case plus:
    case minus:
      /* minor optimization - recognize signed numeric literals */
      switch (chartype(i+1)) {
      case digit:
        if (unary_ok()) {  /* treat as a signed numeric literal */
          enum numform num = classify_numlit(i+1);
          switch (num) {
          case intnum:   addint;  continue;
          case radnum:   addrad;  continue;
          case realnum:  addreal; continue;
          default:  unexpected(num);  /* bad enum value */
          }
        } else {  /* treat the + or - as a binary operator */
          add_plus_or_minus; continue;
        }
      case dot:
        switch (chartype(i+2)) {
        case digit:
          if (unary_ok()) {  /* treat as a signed real literal */
            addreal; continue;
          } else {  /* treat the + or - as a binary operator */
            add_plus_or_minus; continue;
          }
        default:
          /* "+." and "-." can only be followed by a digit, but we
           * deliberately leave diagnosis of that to downstream  */
          add_plus_or_minus; continue;
        }
      case minus:
        switch (chartype(i)) {
        case plus:   addlit("+",1); continue;
        case minus:  /* "--" begins a comment */
          i+=2;
          while (src[i]!='\n') i++;  /* unbounded search for \n OK */
          continue;
        default:  unexpected(chartype(i));
        }
      default:  add_plus_or_minus; continue;
      }
    case lparen:
      if ((j=spanvariadic(i)) != 0) {  /* (*) and interlarded whitespace */
        addlit("(*)",j-i); continue;
      }
      switch (chartype(i+1)) {
      case slash:  addlit("[",2); continue;
      default:  addlit("(",1); continue;
      }
    case rparen:  addlit(")",1); continue;
    case lbrace:  addlit("{",1); continue;
    case rbrace:  addlit("}",1); continue;
    /*
     *  There is currently no lexical convention for embedded
     *  "machine code", whatever that might be (Trojan VM code?),
     *  but one proposal was that it be bracketed by [: and :].
     *
     *  If such a thing is added under 'lbracket' below, it should
     *  put everything between those diglyphs (assumed to be in
     *  "pretty" form already) into a new Machine token, perhaps
     *  using the (currently unused) 'addmach' macro after doing
     *  the recognizing.
     */
    case lbracket:  addlit("[",1); continue;
    case rbracket:  addlit("]",1); continue;
    case langle:
      switch (chartype(i+1)) {
      case equal:  addlit("<=",2); continue;
      case langle:  addlit("{",2); continue;
      default:  addlit("<",1); continue;
      }
    case rangle:
      switch (chartype(i+1)) {
      case equal:  addlit(">=",2); continue;
      case rangle:  addlit("}",2); continue;
      default:  addlit(">",1); continue;
      }
    case caret:  addlit("^",1); continue;
#ifdef EMBEDDED_C
    case backslash:
      switch (chartype(i+1)) {
      case white:
        for (i++; src[i++]!='\n'; ) {
          switch (chartype(i-1)) {
            case white:  break;
            case dollar:  while (src[i]!='\n') i++; break;
            case minus:
              switch (chartype(i)) {
              case minus:  while (src[++i]!='\n') ; break;
              default:  tranerr(i,"silly characters after backslash");
              }
              break;
            default:  tranerr(i-1,"invalid use of backslash");
          }
        }
        continue;
      case letter:
        switch (src[++i]) {
        case 'c':  addc; continue;
        case 'a':  tranerr(i,"embedded Ada source code not yet supported");
        case 'f':  tranerr(i,"embedded Fortran not yet supported");
        default:  tranerr(i,"embedded language request not recognized");
        }
      case dollar:  i+=2; while (src[i++]!='\n') ; continue;
      case minus:
        switch (chartype(i+2)) {
        case minus:  i+=3; while (src[i++]!='\n') ; continue;
        default:  tranerr(i,"\"\\-\" is meaningful in APL, not in SETL");
        }
      default:  tranerr(i+1,"unintelligible spam after backslash");
      }
#endif /* EMBEDDED_C */
    default:  unexpected(chartype(i));
    }
  }
  if (ntokens > 0) {
    resize(tokens, ntokens, token);
  } else {
    release(tokens);
    tokens = NULL;
  }
} /* end scan */

/* .................................................................. */

/* Find the first char past the next newline */
static long spannewline(m)
long m;
{
  /* This unbounded search for a newline is OK because getsrc() in
   * tran.c makes sure src has a newline at the end:  */
  while (src[m]!='\n') m++;
  return(m+1);
} /* end spanalphanum */

/* .................................................................. */

/* Find the end of a C-style comment */
static long spancomment(m)
long m;
{
  long m_init = m;  /* assumed to point to the opening slash, star */
  for (m+=2; m+1<srclen; m++) {
    if (src[m]=='*' && src[m+1]=='/') return m+2;
  }
  tranerr(m_init,"unterminated comment");
} /* end spancomment */

/* .................................................................. */

/* Find the end of a tag or keyword tail */
static long spanalphanum(m)
long m;
{
  while (alphanum[(unsigned char)(src[m])]==tagtail) m++;
  return(m);
} /* end spanalphanum */

/* .................................................................. */

/* Recognize (*) with possible embedded whitespace, and give the loc
 * just past the rparen or 0 to say not recog  */
static long spanvariadic(m)
long m;
{
  assert (chartype(m) == lparen);
  ++m;
  while (m < srclen-2 && chartype(m) == white) ++m;
  if (chartype(m) != star) return 0;
  ++m;
  while (m < srclen-1 && chartype(m) == white) ++m;
  if (chartype(m) != rparen) return 0;
  return m+1;
} /* end spanvariadic */

/* .................................................................. */

/*
 *  If the token(s) just before the current + or - definitely make a
 *  unary interpretation of it as a sign rather than a binary operator
 *  valid, then return true.
 *
 *  It is OK for this to conservatively produce some false negatives
 *  (falsely giving false), such as for a user-defined op:  we only
 *  know it as a Name at this stage, whereas if it was already
 *  classified as a Bop_name or Uop_name, the + or - could definitely
 *  be taken as a sign.
 *
 *  But it would not be OK for this to miss giving false for something
 *  else before the + or - that actually requires it.
 */
static bool unary_ok(void)
{
  switch (ntokens > 0 ? tokens[ntokens-1].code : ';') {
  case Integer:  case Real_:  case String:  case Machine:
  case Name:  case Syscon:  case Sysval:  case Sysvar:
  case ')':  case ']':  case '}':  case END:
    return false;  /* can't safely say it's unary yet */
  case IF:  case CASE:  case EXPR:
    /* END IF, END CASE, and END EXPR cannot precede a unary op.
     * (Neither can EXPR itself, for that matter, but that will be
     * treated as a syntax error at a later stage, like EXPR -x.)  */
    return ntokens < 2 || tokens[ntokens-2].code != END;
  default:
    return true;
  }
} /* end unary_ok */

/* .................................................................. */

/* Determine what kind of numeric literal starts at m */
static enum numform classify_numlit(m)
long m;
{
  m = spanint(m);  /* span initial digits */
  switch (chartype(m)) {  /* first char after digits */
  case sharp:  return radnum;  /* # => must be a radix-prefixed num */
  case dot:
    switch (chartype(m+1)) {
    case dot:    return intnum;  /* this int ends at ".." */
    case digit:  return realnum;  /* digit(s) dot digit => real num */
    default:  tranerr(m+1,"need digit after single dot");
    }
  case letter:
    if (src[m]=='E' || src[m]=='e') return realnum;  /* e.g. 1e9 */
    tranerr(m,"no letter (except e or E) may go right after a digit");
  default:  return intnum;  /* anything else ends a plain integer */
  }
} /* end classify_numlit */

/* .................................................................. */

/* Find the end of a "real" literal given a sure start but an uncertain
 * finish */
static long spanreal(m)
long m;
{
  /*
   *  With an initial REAL-establishing sequence of chars already
   *  recognized, namely an optional sign and optional digits followed
   *  by a dot and a digit, or an optional sign and some digits
   *  followed by an e or E, carry on to recogize the whole literal.
   *  Note that this excludes integer literals.
   *
   *  For the literal to be complete, if there is a dot, it must be
   *  followed by some digits and an optional exponent.
   *
   *  An exponent is e or E followed by an optional sign and mandatory
   *  digits.
   *
   *  This can be summarized in this ERE pattern:
   *
   *    [+-]?([0-9]*\.[0-9]+([Ee][+-]?[0-9]+)?|[0-9]+[Ee][+-]?[0-9]+)
   *
   *  Anything after the trailing digits in each of the alternatives
   *  that pattern matches terminates the span successfully.
   */
  if (src[m]=='+' || src[m]=='-') ++m;  /* leading sign if any */
  while (chartype(m) == digit) ++m;  /* 0 or more initial digits */
  switch (src[m]) {  /* 1st char after initial digits */
  case '.':  /* dot; expect 1 or more digits */
    ++m;
    assert (chartype(m) == digit);  /* cannot have 1. or 1.e1 */
    while (chartype(m) == digit) ++m;  /* digits after dot */
    if (src[m]=='e' || src[m]=='E') {  /* exponent */
      ++m;
      if (src[m]=='+' || src[m]=='-') ++m;  /* optional sign */
      if (chartype(m) != digit) tranerr(m,"need digit(s) in exponent");
      while (chartype(m) == digit) ++m;  /* digits in exponent */
    }
    break;
  case 'e':  case 'E':  /* exponent */
    ++m;
    if (src[m]=='+' || src[m]=='-') ++m;  /* optional sign */
    if (chartype(m) != digit) tranerr(m,"need digit(s) in exponent");
    while (chartype(m) == digit) ++m;  /* digits in exponent */
    break;
  default:
    unexpected(src[m]);  /* didn't even see dot or e where expected */
  }
  return m;
} /* end spanreal */

/* .................................................................. */

/* Find the end of a run of decimal digits */
static long spanint(m)
long m;
{
  if (src[m]=='+' || src[m]=='-') ++m;  /* span leading sign if any */
  assert (chartype(m)==digit);
  while (chartype(++m)==digit) ;
  return m;
} /* end spanint */

/* .................................................................. */

/* Find the end of a radix-prefixed integer literal */
static long spanrad(m)
long m;
{
  int v;
  int prefix = 0;
  long m_init;
  if (src[m]=='+' || src[m]=='-') ++m;  /* span leading sign if any */
  m_init = m;
  while ((v = digval[(unsigned char)(src[m])]) != -1) {
    prefix *= 10;
    prefix += v;
    if (prefix > 36) tranerr(m_init,"radix must be in range 2..36");
    m++;
  }
  assert (chartype(m)==sharp); m++;
  if (prefix < 2) tranerr(m_init,"radix must be in range 2..36");
  if ((v = raddigval[(unsigned char)(src[m])]) == -1 || v >= prefix)
   tranerr(m,"need at least one digit in appropriate radix after \"#\"");
  while ((v = raddigval[(unsigned char)(src[m])]) != -1 && v < prefix) m++;
  if (chartype(m) == sharp) m++;
  return(m);
} /* end spanrad */

/* .................................................................. */

/*
 *  Find the end of a SETL (single-quoted) string literal.
 *  Start at the opening apostrophe that is at src[m].
 *  Return the src index of the char past the closing apostrophe.
 *
 *  Note that src is assumed to end with a newline (\n) and nul (\0),
 *  with the nul at src[srclen].  The string is considered unterminated
 *  if a newline occurs before the closing apostrophe, an error.
 *
 *  As a side-effect, store a "pretty" form of the string in 'strbuf'.
 *
 *  TODO:  merge this function with spandqstring().
 */
static long spanstring(m)
long m;
{
  long mark = 0;
  enum stateval {within,quoteseen};
  enum stateval state;
  long n;
# define strextend(c)\
  {\
    if (n >= curstrchunk) {\
      resize(strbuf, 2*curstrchunk, char);\
      curstrchunk *= 2;\
    }\
    strbuf[n++] = (c);\
  }
  assert (chartype(m)==apostrophe);
  assert (src[m]=='\'');
  strbuf[0] = '\'';
  n = 1;
  state = within;
  for (m++; m<srclen; m++) {
    switch (state) {
    case within:
      switch (chartype(m)) {
      case apostrophe:
        mark = m;
        state = quoteseen;
        continue;
      default:
        if (src[m]=='\n') {
          tranerr(m-1,"unterminated string");
        }
        if (!strok[(unsigned char)(src[m])]) {
          tranerr(m,"disallowed character in string");
        }
        if (src[m]!='\\') {
          if (src[m]=='\t') {
            /*
             * As a special case to accommodate Ada/Ed, we allow
             * raw tabs in literal strings, translating them here
             * to \t.
             */
            strextend('\\');
            strextend('t');
          } else {
            strextend(src[m]);
          }
        } else { /* src[m] is backslash */
          /*
           *  The designation "unexpected" here and in similar cases
           *  below is because we expect a newline at the end of src,
           *  but have just seen something else there instead.
           */
          if (++m >= srclen) unexpected(m);  /* src[srclen-1] != '\n' */
          switch (escapee[(unsigned char)(src[m])]) {
          case unescapable:
            tranerr(m,"disallowed character after backslash");
            break;
          case repself:  /* glyph or blank */
            strextend('\\');
            strextend(src[m]);
            break;
          case control:  /* a,b,f,n,r,t,v */
            strextend('\\');
            strextend(src[m]);
            break;
          case hexlead:  /* x */
            strextend('\\');
            strextend(src[m]);
            if (++m >= srclen) unexpected(m);
            if (!hexdigit[(unsigned char)(src[m])]) {
              tranerr(m,"invalid hexadecimal escape");
            }
            strextend(src[m]);
            if (++m >= srclen) unexpected(m);
            if (!hexdigit[(unsigned char)(src[m])]) {
              m--;
              break;
            }
            strextend(src[m]);
            break;
          case octlead:  /* 0,1,2,3 */
            strextend('\\');
            strextend(src[m]);
            if (++m >= srclen) unexpected(m);
            if (!octdigit[(unsigned char)(src[m])]) {
              m--;
              break;
            }
            strextend(src[m]);
            if (++m >= srclen) unexpected(m);
            if (!octdigit[(unsigned char)(src[m])]) {
              m--;
              break;
            }
            strextend(src[m]);
            break;
          default:  unexpected(escapee[(unsigned char)(src[m])]);
          }
        }
        continue;
      }
    case quoteseen:
      switch (chartype(m)) {
      case apostrophe:  /* the 2nd in a row */
        strextend('\'');
        strextend('\'');
        state = within;
        continue;
      default:
        strextend('\'');
        strextend('\0');
        return(mark+1);  /* 1 char past the closing apostrophe */
      }
    default:  unexpected(state);
    }
  }
  unexpected(m);  /* reached srclen without seeing \n */
} /* end spanstring */

/* .................................................................. */

/*
 *  Find the end of a SETL2 (double-quoted) string literal.
 *  Start at the opening dquote.
 *
 *  TODO:  merge this function with spanstring(), whose other
 *  leading comments apply here too.
 */
static long spandqstring(m)
long m;
{
  long mark = 0;
  enum stateval {within,quoteseen};
  enum stateval state;
  long n;
  assert (chartype(m)==dquote);
  assert (src[m]=='\"');
  strbuf[0] = '\'';
  n = 1;
  state = within;
  for (m++; m<srclen; m++) {
    switch (state) {
    case within:
      switch (chartype(m)) {
      case dquote:
        mark = m;
        state = quoteseen;
        continue;
      default:
        if (src[m]=='\n') {
          tranerr(m-1,"unterminated string");
        }
        if (!strok[(unsigned char)(src[m])]) {
          tranerr(m,"disallowed character in string");
        }
        if (src[m]!='\\') {
          if (src[m]=='\t') {
            /*
             * As a special case to accommodate Ada/Ed, we allow
             * raw tabs in literal strings, translating them here
             * to \t.
             */
            strextend('\\');
            strextend('t');
          } else {
            strextend(src[m]);
            /*
             * Since we are using the SETL (single-quoted) form
             * for the pretty string, twin any unescaped single
             * quote within the double-quoted literal string we
             * are processing.
             */
            if (src[m]=='\'') {
              strextend('\'');
            }
          }
        } else { /* src[m] is backslash */
          /*
           *  The designation "unexpected" here and in similar cases
           *  below is because we expect a newline at the end of src,
           *  but have just seen something else there instead.
           */
          if (++m >= srclen) unexpected(m);  /* src[srclen-1] != '\n' */
          switch (escapee[(unsigned char)(src[m])]) {
          case unescapable:
            tranerr(m,"disallowed character after backslash");
            break;
          case repself:  /* glyph or blank */
            strextend('\\');
            strextend(src[m]);
            break;
          case control:  /* a,b,f,n,r,t,v */
            strextend('\\');
            strextend(src[m]);
            break;
          case hexlead:  /* x */
            strextend('\\');
            strextend(src[m]);
            if (++m >= srclen) unexpected(m);
            if (!hexdigit[(unsigned char)(src[m])]) {
              tranerr(m,"invalid hexadecimal escape");
            }
            strextend(src[m]);
            if (++m >= srclen) unexpected(m);
            if (!hexdigit[(unsigned char)(src[m])]) {
              m--;
              break;
            }
            strextend(src[m]);
            break;
          case octlead:  /* 0,1,2,3 */
            strextend('\\');
            strextend(src[m]);
            if (++m >= srclen) unexpected(m);
            if (!octdigit[(unsigned char)(src[m])]) {
              m--;
              break;
            }
            strextend(src[m]);
            if (++m >= srclen) unexpected(m);
            if (!octdigit[(unsigned char)(src[m])]) {
              m--;
              break;
            }
            strextend(src[m]);
            break;
          default:  unexpected(escapee[(unsigned char)(src[m])]);
          }
        }
        continue;
      }
    case quoteseen:
      switch (chartype(m)) {
      case dquote:  /* the 2nd in a row */
        strextend('\"');  /* put just 1 in the single-quoted string */
        state = within;
        continue;
      default:
        strextend('\'');
        strextend('\0');
        return(mark+1);  /* 1 char past the closing input dquote */
      }
    default:  unexpected(state);
    }
  }
  unexpected(m);  /* reached srclen without seeing \n */
} /* end spandqstring */

/* .................................................................. */

#ifdef EMBEDDED_C

static const char *octdigs = "01234567";

/*
 *  Find the end of a section of embedded C or C++ code;
 *  also build up a "pretty" form in 'strbuf' as if the chars
 *  between the \c and \s were processed by the SETL 'PRETTY'
 *  operator.
 */
static long spanc(m)
long m;
{
  enum stateval {in_c, in_com_old, in_com_new, in_string};
  enum stateval state;
  int q = -1;
  long n;
# define strextend_pretty(c)\
  {\
    switch (cppchar[(unsigned char)(c)]) {\
    case octalize:\
      strextend('\\');\
      strextend(octdigs[((unsigned char)(c)>>6)&3]);\
      strextend(octdigs[((unsigned char)(c)>>3)&7]);\
      strextend(octdigs[((unsigned char)(c)>>0)&7]);\
      break;\
    case musttwin:\
      strextend(c);\
      strextend(c);\
      break;\
    case ctrlchar:\
      strextend('\\');\
      switch (c) {\
      case '\a':  strextend('a'); break;\
      case '\b':  strextend('b'); break;\
      case '\f':  strextend('f'); break;\
      case '\n':  strextend('n'); break;\
      case '\r':  strextend('r'); break;\
      case '\t':  strextend('t'); break;\
      case '\v':  strextend('v'); break;\
      default:  unexpected(c);\
      }\
      break;\
    case already_pretty:\
      strextend(c);\
      break;\
    default:\
      unexpected(cppchar[(unsigned char)(c)]);\
    }\
  }
  assert (src[m]=='c');  /* just so we agree where we're starting */
  strbuf[0] = '\'';
  n = 1;
  state = in_c;
  for (m++; m<srclen; m++) {
    switch (state) {
    case in_c:
      switch (src[m]) {
      case '/':
        strextend(src[m]);
        /*
         *  The designation "unexpected" here and in similar cases
         *  below is because we expect a newline at the end of src,
         *  but have just seen something else there instead.
         */
        if (++m >= srclen) unexpected(m);  /* src[srclen-1] != '\n' */
        switch (src[m]) {
        case '*':
          state = in_com_old;
          break;
        case '/':
          state = in_com_new;
          break;
        }
        break;
      case '\'':  case '\"':
        q = src[m];
        state = in_string;
        break;
      case '\\':
        if (++m >= srclen) unexpected(m);
        while (src[m] == '\\') {
          strextend_pretty('\\');
          if (++m >= srclen) unexpected(m);
        }
        if (src[m] == 's') {
          strextend('\'');
          strextend('\0');
          return m+1;
        }
        strextend_pretty('\\');
        break;
      }
      break;
    case in_com_old:
      if (src[m] == '*') {
        while (src[m] == '*') {
          strextend('*');
          if (++m >= srclen) unexpected(m);
        }
        if (src[m] == '/') {
          state = in_c;
        }
      }
      break;
    case in_com_new:
      if (src[m] == '\n') {
        state = in_c;
      }
      break;
    case in_string:
      if (src[m] == q) {
        state = in_c;
      } else if (src[m] == '\\') {
        strextend_pretty('\\');
        if (++m >= srclen) unexpected(m);
      }
      break;
    default:  unexpected(state);
    }
    strextend_pretty(src[m]);
  }
  unexpected(m);
} /* end spanc */

#endif /* EMBEDDED_C */

/* ------------------------------------------------------------------ */

/*
 *  What I'm doing here in this "pre-parse" is not strictly correct,
 *  because actually you can locally redeclare tags, even those you
 *  might have used elsewhere for operators.  Effectively, an OP
 *  declaration anywhere in the current compilation will change
 *  the lexical category of the operator's tag throughout this
 *  compilation's source, ungoverned by compilation unit scopes and
 *  USE statements.  TODO:  FIXME maybe.
 *
 *  The same applies for tags introduced by COMMAND.
 */

/* This subroutine is called by the driver */
void preparse(void)
{
  long i;
  table optab;
  table cmdtab;
  patcre(&optab);
  patcre(&cmdtab);
  for (i=0; i<ntokens; i++) {
    if (i == 0 || tokens[i-1].code != END) {
      switch (tokens[i].code) {
      case OP:
        if (++i < ntokens) {
          long op_tn = i;
          token *t = &tokens[i];
          if (t->code == Name) {
            const char *s = (tagcase == anycase) ?
                             strmakelower(t->graphic) :
                                          t->graphic;
            DATA *ii;
            int c = -1;
            keysub(optab,s,&ii);
            while (++i < ntokens && (c = tokens[i].code) != ',' && c != ')') ;
            if (i < ntokens) ii->valence = (c == ',') ? 2 : 1;
            else tokerr(op_tn,"unterminated OP header");
          }
        }
        break;
      case COMMAND:
        if (++i < ntokens) {
          token *t = &tokens[i];
          if (t->code == Name) {
            const char *s = (tagcase == anycase) ?
                             strmakelower(t->graphic) :
                                          t->graphic;
            DATA *ii;
            keysub(cmdtab,s,&ii);
          }
        }
        break;
      default:
        break;
      }
    }
  }
  if (size(optab) + size(cmdtab) == 0) return;
  for (i=0; i<ntokens; i++) {
    token *t = &tokens[i];
    if (t->code == Name && (i == 0 || tokens[i-1].code != '.')) {
      const char *s = (tagcase == anycase) ?
                       strmakelower(t->graphic) :
                                    t->graphic;
      DATA *ii;
      if (lookup(optab,s,&ii)) {
        if (ii->valence == 1) {
          t->code = Uop_name;
        } else {
          if (i < ntokens-1 && tokens[i+1].code == Becomes) {
            t->code = Accum;  /* e.g. for the + in +:= */
          } else {
            t->code = Bop_name;
          }
        }
      } else if (lookup(cmdtab,s,&ii)) {
        t->code = Command_name;
      }
    }
  }
}
