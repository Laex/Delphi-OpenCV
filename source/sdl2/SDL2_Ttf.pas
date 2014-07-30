unit SDL2_Ttf;
{*******************************************************************************

  SDL2_Ttf.pas    v1.0  29/07/2013 first version for DelphiXE
                  v1.1  27/08/2013 add MACOS compability
                  v1.2  31/05/2014 delete sdl2.inc

  Simple DirectMedia Layer
  Copyright (C) 1997-2013 Sam Lantinga <slouken@libsdl.org>

  Pascal-Header-Conversion SDL from the JEDI-Team written by Domenique Louis and others.

  convert SDL/SDL2 to SDL2 for DelphiXE by Kotai 2013/2014  www.remakesonline.com

  The initial developer of this Pascal code was :
  Dominqiue Louis <Dominique@SavageSoftware.com.au>


*******************************************************************************}


interface

uses

{$IFDEF MSWINDOWS}
  Winapi.Windows,
{$ENDIF}
  SDL2;

const

  {$IFDEF MSWINDOWS}
    SDL_ttfLibName = 'SDL2_ttf.dll';
  {$ENDIF}

  {$IFDEF ANDROID}
    SDL_ttfLibName = 'libSDL2_ttf.so';
  {$ENDIF}

  {$IFDEF MACOS}
    {$IFDEF IOS}
      SDL_ttfLibName = 'libSDL2_ttf.a';
    {$ELSE}
      SDL_ttfLibName = 'SDL2_ttf';
//      SDL_ttfLibName = '../Frameworks/SDL2_ttf.framework/Versions/A/SDL2_ttf';
    {$ENDIF}
  {$ENDIF}

{* Set up for C function definitions, even when using C++ *}

{* Printable format: "%d.%d.%d", MAJOR, MINOR, PATCHLEVEL *}
const
  SDL_TTF_MAJOR_VERSION = 2;
  SDL_TTF_MINOR_VERSION = 0;
  SDL_TTF_PATCHLEVEL    = 12;

{* Backwards compatibility *}
const
  TTF_MAJOR_VERSION = SDL_TTF_MAJOR_VERSION;
  TTF_MINOR_VERSION = SDL_TTF_MINOR_VERSION;
  TTF_PATCHLEVEL    = SDL_TTF_PATCHLEVEL;
  //TTF_VERSION(X)    = SDL_TTF_VERSION(X);

 {* This function gets the version of the dynamically linked SDL_ttf library.
   it should NOT be used to fill a version structure, instead you should
   use the SDL_TTF_VERSION() macro.
 *}
function TTF_Linked_Version(): TSDL_Version;
cdecl; external SDL_ttfLibName {$IFDEF MACOS} {$IFNDEF IOS} name '_TTF_Linked_Version' {$ENDIF} {$ENDIF};

{* ZERO WIDTH NO-BREAKSPACE (Unicode byte order mark) *}
const
  UNICODE_BOM_NATIVE  = $FEFF;
  UNICODE_BOM_SWAPPED = $FFFE;

{* This function tells the library whether UNICODE text is generally
   byteswapped.  A UNICODE BOM character in a string will override
   this setting for the remainder of that string.
*}
procedure TTF_ByteSwappedUNICODE(swapped: Integer);
cdecl; external SDL_ttfLibName {$IFDEF MACOS} {$IFNDEF IOS} name '_TTF_ByteSwappedUNICODE' {$ENDIF} {$ENDIF};

{* The internal structure containing font information *}
type
  PTTF_Font = ^TTTF_Font;
  TTTF_Font = record  end; //todo?

{* Initialize the TTF engine - returns 0 if successful, -1 on error *}
function TTF_Init(): Integer;
cdecl; external SDL_ttfLibName {$IFDEF MACOS} {$IFNDEF IOS} name '_TTF_Init' {$ENDIF} {$ENDIF};

{* Open a font file and create a font of the specified point size.
 * Some .fon fonts will have several sizes embedded in the file, so the
 * point size becomes the index of choosing which size.  If the value
 * is too high, the last indexed size will be the default. *}
function TTF_OpenFont(_file: PChar; ptsize: Integer): PTTF_Font;
cdecl; external SDL_ttfLibName {$IFDEF MACOS} {$IFNDEF IOS} name '_TTF_OpenFont' {$ENDIF} {$ENDIF};

function TTF_OpenFontIndex(_file: PChar; ptsize: Integer; index: LongInt): PTTF_Font;
cdecl; external SDL_ttfLibName {$IFDEF MACOS} {$IFNDEF IOS} name '_TTF_OpenFontIndex' {$ENDIF} {$ENDIF};

function TTF_OpenFontRW(src: PSDL_RWops; freesrc: Integer; ptsize: LongInt): PTTF_Font;
cdecl; external SDL_ttfLibName {$IFDEF MACOS} {$IFNDEF IOS} name '_TTF_OpenFontRW' {$ENDIF} {$ENDIF};

function TTF_OpenFontIndexRW(src: PSDL_RWops; freesrc: Integer; ptsize: Integer; index: LongInt): PTTF_Font;
cdecl; external SDL_ttfLibName {$IFDEF MACOS} {$IFNDEF IOS} name '_TTF_OpenFontIndexRW' {$ENDIF} {$ENDIF};

{* Set and retrieve the font style *}
const
  TTF_STYLE_NORMAL        = $00;
  TTF_STYLE_BOLD          = $01;
  TTF_STYLE_ITALIC        = $02;
  TTF_STYLE_UNDERLINE     = $04;
  TTF_STYLE_STRIKETHROUGH = $08;

function TTF_GetFontStyle(font: PTTF_Font): Integer;
cdecl; external SDL_ttfLibName {$IFDEF MACOS} {$IFNDEF IOS} name '_TTF_GetFontStyle' {$ENDIF} {$ENDIF};

procedure TTF_SetFontStyle(font: PTTF_Font; style: Integer);
cdecl; external SDL_ttfLibName {$IFDEF MACOS} {$IFNDEF IOS} name '_TTF_SetFontStyle' {$ENDIF} {$ENDIF};

function TTF_GetFontOutline(font: PTTF_Font): Integer;
cdecl; external SDL_ttfLibName {$IFDEF MACOS} {$IFNDEF IOS} name '_TTF_GetFontOutline' {$ENDIF} {$ENDIF};

procedure TTF_SetFontOutline(font: PTTF_Font; outline: Integer);
cdecl; external SDL_ttfLibName {$IFDEF MACOS} {$IFNDEF IOS} name '_TTF_SetFontOutline' {$ENDIF} {$ENDIF};

{* Set and retrieve FreeType hinter settings *}
const
  TTF_HINTING_NORMAL  = 0;
  TTF_HINTING_LIGHT   = 1;
  TTF_HINTING_MONO    = 2;
  TTF_HINTING_NONE    = 3;

function TTF_GetFontHinting(font: PTTF_Font): Integer;
cdecl; external SDL_ttfLibName {$IFDEF MACOS} {$IFNDEF IOS} name '_TTF_GetFontHinting' {$ENDIF} {$ENDIF};

procedure TTF_SetFontHinting(font: PTTF_Font; hinting: Integer);
cdecl; external SDL_ttfLibName {$IFDEF MACOS} {$IFNDEF IOS} name '_TTF_SetFontHinting' {$ENDIF} {$ENDIF};

{* Get the total height of the font - usually equal to point size *}
function TTF_FontHeight(font: PTTF_Font): Integer;
cdecl; external SDL_ttfLibName {$IFDEF MACOS} {$IFNDEF IOS} name '_TTF_FontHeight' {$ENDIF} {$ENDIF};

{* Get the offset from the baseline to the top of the font
   This is a positive value, relative to the baseline.
 *}
function TTF_FontAscent(font: PTTF_Font): Integer;
cdecl; external SDL_ttfLibName {$IFDEF MACOS} {$IFNDEF IOS} name '_TTF_FontAscent' {$ENDIF} {$ENDIF};

{* Get the offset from the baseline to the bottom of the font
   This is a negative value, relative to the baseline.
 *}
function TTF_FontDescent(font: PTTF_Font): Integer;
cdecl; external SDL_ttfLibName {$IFDEF MACOS} {$IFNDEF IOS} name '_TTF_FontDescent' {$ENDIF} {$ENDIF};

{* Get the recommended spacing between lines of text for this font *}
function TTF_FontLineSkip(font: PTTF_Font): Integer;
cdecl; external SDL_ttfLibName {$IFDEF MACOS} {$IFNDEF IOS} name '_TTF_FontLineSkip' {$ENDIF} {$ENDIF};

{* Get/Set whether or not kerning is allowed for this font *}
function TTF_GetFontKerning(font: PTTF_Font): Integer;
cdecl; external SDL_ttfLibName {$IFDEF MACOS} {$IFNDEF IOS} name '_TTF_GetFontKerning' {$ENDIF} {$ENDIF};

procedure TTF_SetFontKerning(font: PTTF_Font; allowed: Integer);
cdecl; external SDL_ttfLibName {$IFDEF MACOS} {$IFNDEF IOS} name '_TTF_SetFontKerning' {$ENDIF} {$ENDIF};

{* Get the number of faces of the font *}
function TTF_FontFaces(font: PTTF_Font): LongInt;
cdecl; external SDL_ttfLibName {$IFDEF MACOS} {$IFNDEF IOS} name '_TTF_FontFaces' {$ENDIF} {$ENDIF};

{* Get the font face attributes, if any *}
function TTF_FontFaceIsFixedWidth(font: PTTF_Font): Integer;
cdecl; external SDL_ttfLibName {$IFDEF MACOS} {$IFNDEF IOS} name '_TTF_FontFaceIsFixedWidth' {$ENDIF} {$ENDIF};

function TTF_FontFaceFamilyName(font: PTTF_Font): PChar;
cdecl; external SDL_ttfLibName {$IFDEF MACOS} {$IFNDEF IOS} name '_TTF_FontFaceFamilyName' {$ENDIF} {$ENDIF};

function TTF_FontFaceStyleName(font: PTTF_Font): PChar;
cdecl; external SDL_ttfLibName {$IFDEF MACOS} {$IFNDEF IOS} name '_TTF_FontFaceStyleName' {$ENDIF} {$ENDIF};

{* Check wether a glyph is provided by the font or not *}
function TTF_GlyphIsProvided(font: PTTF_Font; ch: UInt16): Integer;
cdecl; external SDL_ttfLibName {$IFDEF MACOS} {$IFNDEF IOS} name '_TTF_GlyphIsProvided' {$ENDIF} {$ENDIF};

{* Get the metrics (dimensions) of a glyph
   To understand what these metrics mean, here is a useful link:
    http://freetype.sourceforge.net/freetype2/docs/tutorial/step2.html
 *}
function TTF_GlyphMetrics(font: PTTF_Font; ch: UInt16; minx, maxx: PInt; miny, maxy: PInt; advance: PInt): Integer;
cdecl; external SDL_ttfLibName {$IFDEF MACOS} {$IFNDEF IOS} name '_TTF_GlyphMetrics' {$ENDIF} {$ENDIF};

{* Get the dimensions of a rendered string of text *}
function TTF_SizeText(font: PTTF_Font; text: PChar; w, h: PInt): Integer;
cdecl; external SDL_ttfLibName {$IFDEF MACOS} {$IFNDEF IOS} name '_TTF_SizeText' {$ENDIF} {$ENDIF};

function TTF_SizeUTF8(font: PTTF_Font; text: PChar; w, h: PInt): Integer;
cdecl; external SDL_ttfLibName {$IFDEF MACOS} {$IFNDEF IOS} name '_TTF_SizeUTF8' {$ENDIF} {$ENDIF};

function TTF_SizeUNICODE(font: PTTF_Font; text: PUInt16; w, h: PInt): Integer;
cdecl; external SDL_ttfLibName {$IFDEF MACOS} {$IFNDEF IOS} name '_TTF_SizeUNICODE' {$ENDIF} {$ENDIF};

{* Create an 8-bit palettized surface and render the given text at
   fast quality with the given font and color.  The 0 pixel is the
   colorkey, giving a transparent background, and the 1 pixel is set
   to the text color.
   This function returns the new surface, or NULL if there was an error.
*}
function TTF_RenderText_Solid(font: PTTF_Font; text: PChar; fg: TSDL_Color): PSDL_Surface;
cdecl; external SDL_ttfLibName {$IFDEF MACOS} {$IFNDEF IOS} name '_TTF_RenderText_Solid' {$ENDIF} {$ENDIF};

function TTF_RenderUTF8_Solid(font: PTTF_Font; text: PChar; fg: TSDL_Color): PSDL_Surface;
cdecl; external SDL_ttfLibName {$IFDEF MACOS} {$IFNDEF IOS} name '_TTF_RenderUTF8_Solid' {$ENDIF} {$ENDIF};

function TTF_RenderUNICODE_Solid(font: PTTF_Font; text: PUInt16; fg: TSDL_Color): PSDL_Surface;
cdecl; external SDL_ttfLibName {$IFDEF MACOS} {$IFNDEF IOS} name '_TTF_RenderUNICODE_Solid' {$ENDIF} {$ENDIF};

{* Create an 8-bit palettized surface and render the given glyph at
   fast quality with the given font and color.  The 0 pixel is the
   colorkey, giving a transparent background, and the 1 pixel is set
   to the text color.  The glyph is rendered without any padding or
   centering in the X direction, and aligned normally in the Y direction.
   This function returns the new surface, or NULL if there was an error.
*}
function TTF_RenderGlyph_Solid(font: PTTF_Font; ch: UInt16; fg: TSDL_Color): PSDL_Surface;
cdecl; external SDL_ttfLibName {$IFDEF MACOS} {$IFNDEF IOS} name '_TTF_RenderGlyph_Solid' {$ENDIF} {$ENDIF};

{* Create an 8-bit palettized surface and render the given text at
   high quality with the given font and colors.  The 0 pixel is background,
   while other pixels have varying degrees of the foreground color.
   This function returns the new surface, or NULL if there was an error.
*}
function TTF_RenderText_Shaded(font: PTTF_Font; text: PChar; fg, bg: TSDL_Color): PSDL_Surface;
cdecl; external SDL_ttfLibName {$IFDEF MACOS} {$IFNDEF IOS} name '_TTF_RenderText_Shaded' {$ENDIF} {$ENDIF};

function TTF_RenderUTF8_Shaded(font: PTTF_Font; text: PChar; fg, bg: TSDL_Color): PSDL_Surface;
cdecl; external SDL_ttfLibName {$IFDEF MACOS} {$IFNDEF IOS} name '_TTF_RenderUTF8_Shaded' {$ENDIF} {$ENDIF};

function TTF_RenderUNICODE_Shaded(font: PTTF_Font; text: PUInt16; fg, bg: TSDL_Color): PSDL_Surface;
cdecl; external SDL_ttfLibName {$IFDEF MACOS} {$IFNDEF IOS} name '_TTF_RenderUNICODE_Shaded' {$ENDIF} {$ENDIF};

{* Create an 8-bit palettized surface and render the given glyph at
   high quality with the given font and colors.  The 0 pixel is background,
   while other pixels have varying degrees of the foreground color.
   The glyph is rendered without any padding or centering in the X
   direction, and aligned normally in the Y direction.
   This function returns the new surface, or NULL if there was an error.
*}
function TTF_RenderGlyph_Shaded(font: PTTF_Font; ch: UInt16; fg, bg: TSDL_Color): PSDL_Surface;
cdecl; external SDL_ttfLibName {$IFDEF MACOS} {$IFNDEF IOS} name '_TTF_RenderGlyph_Shaded' {$ENDIF} {$ENDIF};

{* Create a 32-bit ARGB surface and render the given text at high quality,
   using alpha blending to dither the font with the given color.
   This function returns the new surface, or NULL if there was an error.
*}
function TTF_RenderText_Blended(font: PTTF_Font; text: PChar; fg: TSDL_Color): PSDL_Surface;
cdecl; external SDL_ttfLibName {$IFDEF MACOS} {$IFNDEF IOS} name '_TTF_RenderText_Blended' {$ENDIF} {$ENDIF};

function TTF_RenderUTF8_Blended(font: PTTF_Font; text: PChar; fg: TSDL_Color): PSDL_Surface;
cdecl; external SDL_ttfLibName {$IFDEF MACOS} {$IFNDEF IOS} name '_TTF_RenderUTF8_Blended' {$ENDIF} {$ENDIF};

function TTF_RenderUNICODE_Blended(font: PTTF_Font; text: UInt16; fg: TSDL_Color): PSDL_Surface;
cdecl; external SDL_ttfLibName {$IFDEF MACOS} {$IFNDEF IOS} name '_TTF_RenderUNICODE_Blended' {$ENDIF} {$ENDIF};

{* Create a 32-bit ARGB surface and render the given text at high quality,
   using alpha blending to dither the font with the given color.
   Text is wrapped to multiple lines on line endings and on word boundaries
   if it extends beyond wrapLength in pixels.
   This function returns the new surface, or NULL if there was an error.
*}
function TTF_RenderText_Blended_Wrapped(font: PTTF_Font; text: PChar; fg: TSDL_Color; wrapLength: UInt32): PSDL_Surface;
cdecl; external SDL_ttfLibName {$IFDEF MACOS} {$IFNDEF IOS} name '_TTF_RenderText_Blended_Wrapped' {$ENDIF} {$ENDIF};

function TTF_RenderUTF8_Blended_Wrapped(font: PTTF_Font; text: PChar; fg: TSDL_Color; wrapLength: UInt32): PSDL_Surface;
cdecl; external SDL_ttfLibName {$IFDEF MACOS} {$IFNDEF IOS} name '_TTF_RenderUTF8_Blended_Wrapped' {$ENDIF} {$ENDIF};

function TTF_RenderUNICODE_Blended_Wrapped(font: PTTF_Font; text: PUInt16; fg: TSDL_Color; wrapLength: UInt32): PSDL_Surface;
cdecl; external SDL_ttfLibName {$IFDEF MACOS} {$IFNDEF IOS} name '_TTF_RenderUNICODE_Blended_Wrapped' {$ENDIF} {$ENDIF};

{* Create a 32-bit ARGB surface and render the given glyph at high quality,
   using alpha blending to dither the font with the given color.
   The glyph is rendered without any padding or centering in the X
   direction, and aligned normally in the Y direction.
   This function returns the new surface, or NULL if there was an error.
*}
function TTF_RenderGlyph_Blended(font: PTTF_Font; ch: UInt16; fg: TSDL_Color): PSDL_Surface;
cdecl; external SDL_ttfLibName {$IFDEF MACOS} {$IFNDEF IOS} name '_TTF_RenderGlyph_Blended' {$ENDIF} {$ENDIF};

{* For compatibility with previous versions, here are the old functions *}
function TTF_RenderText(font: PTTF_Font; text: PChar; fg, bg: TSDL_Color): PSDL_Surface;
function TTF_RenderUTF8(font: PTTF_Font; text: PChar; fg, bg: TSDL_Color): PSDL_Surface;
function TTF_RenderUNICODE(font: PTTF_Font; text: PUInt16; fg, bg: TSDL_Color): PSDL_Surface;

{* Close an opened font file *}
procedure TTF_CloseFont(font: PTTF_Font);
cdecl; external SDL_ttfLibName {$IFDEF MACOS} {$IFNDEF IOS} name '_TTF_CloseFont' {$ENDIF} {$ENDIF};

{* De-initialize the TTF engine *}
procedure TTF_Quit();
cdecl; external SDL_ttfLibName {$IFDEF MACOS} {$IFNDEF IOS} name '_TTF_Quit' {$ENDIF} {$ENDIF};

{* Check if the TTF engine is initialized *}
function TTF_WasInit(): Boolean;
cdecl; external SDL_ttfLibName {$IFDEF MACOS} {$IFNDEF IOS} name '_TTF_WasInit' {$ENDIF} {$ENDIF};

{* Get the kerning size of two glyphs *}
function TTF_GetFontKerningSize(font: PTTF_Font; prev_index, index: Integer): Integer;
cdecl; external SDL_ttfLibName {$IFDEF MACOS} {$IFNDEF IOS} name '_TTF_GetFontKerningSize' {$ENDIF} {$ENDIF};

{* We'll use SDL for reporting errors *}
function TTF_SetError(const fmt: PChar): SInt32;
function TTF_GetError: PChar;







//******************************************************************************
//******************************************************************************
//******************************************************************************
//******************************************************************************
//******************************************************************************




implementation

//******************************************************************************

function TTF_SetError(const fmt: PChar): SInt32;
begin
     Result := SDL_SetError(fmt);
end;

//******************************************************************************

function TTF_GetError: PChar;
begin
     Result := SDL_GetError;
end;

//******************************************************************************

function TTF_RenderText(font: PTTF_Font; text: PChar; fg, bg: TSDL_Color): PSDL_Surface;
begin
     Result := TTF_RenderText_Shaded(font, text, fg, bg);
end;

//******************************************************************************

function TTF_RenderUTF8(font: PTTF_Font; text: PChar; fg, bg: TSDL_Color): PSDL_Surface;
begin
     Result := TTF_RenderUTF8_Shaded(font, text, fg, bg);
end;

//******************************************************************************

function TTF_RenderUNICODE(font: PTTF_Font; text: PUInt16; fg, bg: TSDL_Color): PSDL_Surface;
begin
     Result := TTF_RenderUNICODE_Shaded(font, text, fg, bg);
end;

end.

