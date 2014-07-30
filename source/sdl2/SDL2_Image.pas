unit SDL2_Image;
{*******************************************************************************

  SDL2_Image.pas  v1.0  29/07/2013 first version for DelphiXE
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
  SDL2;

const

  {$IFDEF MSWINDOWS}
    SDL_ImageLibName =  'SDL2_Image.dll';
  {$ENDIF}

  {$IFDEF ANDROID}
    SDL_ImageLibName = 'libSDL2_image.so';
  {$ENDIF}

  {$IFDEF MACOS}
    {$IFDEF IOS}
      SDL_ImageLibName = 'libSDL2_image.a';
    {$ELSE}
      SDL_ImageLibName = 'SDL2_image';
//      SDL_ImageLibName = '../Frameworks/SDL2_image.framework/Versions/A/SDL2_image';
    {$ENDIF}
  {$ENDIF}

  {* Printable format: "%d.%d.%d", MAJOR, MINOR, PATCHLEVEL *}
  SDL_IMAGE_MAJOR_VERSION = 2;
  SDL_IMAGE_MINOR_VERSION = 0;
  SDL_IMAGE_PATCHLEVEL    = 0;

  {* This macro can be used to fill a version structure with the compile-time
   * version of the SDL_image library.
   *}

procedure SDL_IMAGE_VERSION(var X: TSDL_Version);

  {* This function gets the version of the dynamically linked SDL_image library.
     it should NOT be used to fill a version structure, instead you should
     use the SDL_IMAGE_VERSION() macro.
   *}
function IMG_Linked_Version(): TSDL_Version;
cdecl; external SDL_ImageLibName {$IFDEF MACOS} {$IFNDEF IOS} name '_IMG_Linked_Version' {$ENDIF} {$ENDIF};

const
  IMG_INIT_JPG  = $00000001;
  IMG_INIT_PNG  = $00000002;
  IMG_INIT_TIF  = $00000004;
  IMG_INIT_WEBP = $00000008;

type
  TIMG_InitFlags = DWord;

  {* Loads dynamic libraries and prepares them for use.  Flags should be
     one or more flags from IMG_InitFlags OR'd together.
     It returns the flags successfully initialized, or 0 on failure.
   *}
function IMG_Init(flags: SInt32): SInt32;
cdecl; external SDL_ImageLibName {$IFDEF MACOS} {$IFNDEF IOS} name '_IMG_Init' {$ENDIF} {$ENDIF};

  {* Unloads libraries loaded with IMG_Init *}
procedure IMG_Quit();
cdecl; external SDL_ImageLibName {$IFDEF MACOS} {$IFNDEF IOS} name '_IMG_Quit' {$ENDIF} {$ENDIF};

  {* Load an image from an SDL data source.
     The 'type' may be one of: "BMP", "GIF", "PNG", etc.

     If the image format supports a transparent pixel, SDL will set the
     colorkey for the surface.  You can enable RLE acceleration on the
     surface afterwards by calling:
      SDL_SetColorKey(image, SDL_RLEACCEL, image->format->colorkey);
   *}
function IMG_LoadTyped_RW(src: PSDL_RWops; freesrc: SInt32; _type: PChar): PSDL_Surface;
cdecl; external SDL_ImageLibName {$IFDEF MACOS} {$IFNDEF IOS} name '_IMG_LoadTyped_RW' {$ENDIF} {$ENDIF};

  {* Convenience functions *}
function IMG_Load(_file: PChar): PSDL_Surface;
cdecl; external SDL_ImageLibName {$IFDEF MACOS} {$IFNDEF IOS} name '_IMG_Load' {$ENDIF} {$ENDIF};

function IMG_Load_RW(src: PSDL_RWops; freesrc: SInt32): PSDL_Surface;
cdecl; external SDL_ImageLibName {$IFDEF MACOS} {$IFNDEF IOS} name '_IMG_Load_RW' {$ENDIF} {$ENDIF};

  {* Load an image directly into a render texture. *}
function IMG_LoadTexture(renderer: PSDL_Renderer; _file: PChar): PSDL_Texture;
cdecl; external SDL_ImageLibName {$IFDEF MACOS} {$IFNDEF IOS} name '_IMG_LoadTexture' {$ENDIF} {$ENDIF};

function IMG_LoadTexture_RW(renderer: PSDL_Renderer; src: PSDL_RWops; freesrc: SInt32): PSDL_Texture;
cdecl; external SDL_ImageLibName {$IFDEF MACOS} {$IFNDEF IOS} name '_IMG_LoadTexture_RW' {$ENDIF} {$ENDIF};

function IMG_LoadTextureTyped_RW(renderer: PSDL_Renderer; src: PSDL_RWops; freesrc: SInt32; _type: PChar): PSDL_Texture;
cdecl; external SDL_ImageLibName {$IFDEF MACOS} {$IFNDEF IOS} name '_IMG_LoadTextureTyped_RW' {$ENDIF} {$ENDIF};

  {* Functions to detect a file type, given a seekable source *}
function IMG_isICO(src: PSDL_RWops): SInt32;
cdecl; external SDL_ImageLibName {$IFDEF MACOS} {$IFNDEF IOS} name '_IMG_isICO' {$ENDIF} {$ENDIF};

function IMG_isCUR(src: PSDL_RWops): SInt32;
cdecl; external SDL_ImageLibName {$IFDEF MACOS} {$IFNDEF IOS} name '_IMG_isCUR' {$ENDIF} {$ENDIF};

function IMG_isBMP(src: PSDL_RWops): SInt32;
cdecl; external SDL_ImageLibName {$IFDEF MACOS} {$IFNDEF IOS} name '_IMG_isBMP' {$ENDIF} {$ENDIF};

function IMG_isGIF(src: PSDL_RWops): SInt32;
cdecl; external SDL_ImageLibName {$IFDEF MACOS} {$IFNDEF IOS} name '_IMG_isGIF' {$ENDIF} {$ENDIF};

function IMG_isJPG(src: PSDL_RWops): SInt32;
cdecl; external SDL_ImageLibName {$IFDEF MACOS} {$IFNDEF IOS} name '_IMG_isJPG' {$ENDIF} {$ENDIF};

function IMG_isLBM(src: PSDL_RWops): SInt32;
cdecl; external SDL_ImageLibName {$IFDEF MACOS} {$IFNDEF IOS} name '_IMG_isLBM' {$ENDIF} {$ENDIF};

function IMG_isPCX(src: PSDL_RWops): SInt32;
cdecl; external SDL_ImageLibName {$IFDEF MACOS} {$IFNDEF IOS} name '_IMG_isPCX' {$ENDIF} {$ENDIF};

function IMG_isPNG(src: PSDL_RWops): SInt32;
cdecl; external SDL_ImageLibName {$IFDEF MACOS} {$IFNDEF IOS} name '_IMG_isPNG' {$ENDIF} {$ENDIF};

function IMG_isPNM(src: PSDL_RWops): SInt32;
cdecl; external SDL_ImageLibName {$IFDEF MACOS} {$IFNDEF IOS} name '_IMG_isPNM' {$ENDIF} {$ENDIF};

function IMG_isTIF(src: PSDL_RWops): SInt32;
cdecl; external SDL_ImageLibName {$IFDEF MACOS} {$IFNDEF IOS} name '_IMG_isTIF' {$ENDIF} {$ENDIF};

function IMG_isXCF(src: PSDL_RWops): SInt32;
cdecl; external SDL_ImageLibName {$IFDEF MACOS} {$IFNDEF IOS} name '-IMG_isXCF' {$ENDIF} {$ENDIF};

function IMG_isXPM(src: PSDL_RWops): SInt32;
cdecl; external SDL_ImageLibName {$IFDEF MACOS} {$IFNDEF IOS} name '_IMG_isXPM' {$ENDIF} {$ENDIF};

function IMG_isXV(src: PSDL_RWops): SInt32;
cdecl; external SDL_ImageLibName {$IFDEF MACOS} {$IFNDEF IOS} name '_IMG_isXV' {$ENDIF} {$ENDIF};

function IMG_isWEBP(src: PSDL_RWops): SInt32;
cdecl; external SDL_ImageLibName {$IFDEF MACOS} {$IFNDEF IOS} name '_IMG_isWEBP' {$ENDIF} {$ENDIF};


  {* Individual loading functions *}
function IMG_LoadICO_RW(src: PSDL_RWops): PSDL_Surface;
cdecl; external SDL_ImageLibName {$IFDEF MACOS} {$IFNDEF IOS} name '_IMG_LoadICO_RW' {$ENDIF} {$ENDIF};

function IMG_LoadCUR_RW(src: PSDL_RWops): PSDL_Surface;
cdecl; external SDL_ImageLibName {$IFDEF MACOS} {$IFNDEF IOS} name '_IMG_LoadCUR_RW' {$ENDIF} {$ENDIF};

function IMG_LoadBMP_RW(src: PSDL_RWops): PSDL_Surface;
cdecl; external SDL_ImageLibName {$IFDEF MACOS} {$IFNDEF IOS} name '_IMG_LoadBMP_RW' {$ENDIF} {$ENDIF};

function IMG_LoadGIF_RW(src: PSDL_RWops): PSDL_Surface;
cdecl; external SDL_ImageLibName {$IFDEF MACOS} {$IFNDEF IOS} name '_IMG_LoadGIF_RW' {$ENDIF} {$ENDIF};

function IMG_LoadJPG_RW(src: PSDL_RWops): PSDL_Surface;
cdecl; external SDL_ImageLibName {$IFDEF MACOS} {$IFNDEF IOS} name '_IMG_LoadJPG_RW' {$ENDIF} {$ENDIF};

function IMG_LoadLBM_RW(src: PSDL_RWops): PSDL_Surface;
cdecl; external SDL_ImageLibName {$IFDEF MACOS} {$IFNDEF IOS} name '_IMG_LoadLBM_RW' {$ENDIF} {$ENDIF};

function IMG_LoadPCX_RW(src: PSDL_RWops): PSDL_Surface;
cdecl; external SDL_ImageLibName {$IFDEF MACOS} {$IFNDEF IOS} name '_IMG_LoadPCX_RW' {$ENDIF} {$ENDIF};

function IMG_LoadPNG_RW(src: PSDL_RWops): PSDL_Surface;
cdecl; external SDL_ImageLibName {$IFDEF MACOS} {$IFNDEF IOS} name '_IMG_LoadPNG_RW' {$ENDIF} {$ENDIF};

function IMG_LoadPNM_RW(src: PSDL_RWops): PSDL_Surface;
cdecl; external SDL_ImageLibName {$IFDEF MACOS} {$IFNDEF IOS} name '_IMG_LoadPNM_RW' {$ENDIF} {$ENDIF};

function IMG_LoadTGA_RW(src: PSDL_RWops): PSDL_Surface;
cdecl; external SDL_ImageLibName {$IFDEF MACOS} {$IFNDEF IOS} name '_IMG_LoadTGA_RW' {$ENDIF} {$ENDIF};

function IMG_LoadTIF_RW(src: PSDL_RWops): PSDL_Surface;
cdecl; external SDL_ImageLibName {$IFDEF MACOS} {$IFNDEF IOS} name '_IMG_LoadTIF_RW' {$ENDIF} {$ENDIF};

function IMG_LoadXCF_RW(src: PSDL_RWops): PSDL_Surface;
cdecl; external SDL_ImageLibName {$IFDEF MACOS} {$IFNDEF IOS} name '_IMG_LoadXCF_RW' {$ENDIF} {$ENDIF};

function IMG_LoadXPM_RW(src: PSDL_RWops): PSDL_Surface;
cdecl; external SDL_ImageLibName {$IFDEF MACOS} {$IFNDEF IOS} name '_IMG_LoadXMP_RW' {$ENDIF} {$ENDIF};

function IMG_LoadXV_RW(src: PSDL_RWops): PSDL_Surface;
cdecl; external SDL_ImageLibName {$IFDEF MACOS} {$IFNDEF IOS} name '_IMG_LoadXV_RW' {$ENDIF} {$ENDIF};

function IMG_LoadWEBP_RW(src: PSDL_RWops): PSDL_Surface;
cdecl; external SDL_ImageLibName {$IFDEF MACOS} {$IFNDEF IOS} name '_IMG_LoadWEBP_RW' {$ENDIF} {$ENDIF};

function IMG_ReadXPMFromArray(xpm: PPChar): PSDL_Surface;
cdecl; external SDL_ImageLibName {$IFDEF MACOS} {$IFNDEF IOS} name '_IMG_ReadXPMFromArray' {$ENDIF} {$ENDIF};

  {* Individual saving functions *}
function IMG_SavePNG(surface: PSDL_Surface; const _file: PChar): SInt32;
cdecl; external SDL_ImageLibName {$IFDEF MACOS} {$IFNDEF IOS} name '_IMG_SavePNG' {$ENDIF} {$ENDIF};

function IMG_SavePNG_RW(surface: PSDL_Surface; dst: PSDL_RWops; freedst: SInt32): SInt32;
cdecl; external SDL_ImageLibName {$IFDEF MACOS} {$IFNDEF IOS} name '_IMG_SavePNG_RW' {$ENDIF} {$ENDIF};

{* We'll use SDL for reporting errors *}
function IMG_SetError(fmt: PChar): SInt32;
function IMG_GetError: PChar;






//******************************************************************************
//******************************************************************************
//******************************************************************************
//******************************************************************************
//******************************************************************************





implementation

//******************************************************************************

procedure SDL_IMAGE_VERSION(var X: TSDL_Version);
begin
     X.major := SDL_IMAGE_MAJOR_VERSION;
     X.minor := SDL_IMAGE_MINOR_VERSION;
     X.patch := SDL_IMAGE_PATCHLEVEL;
end;

//******************************************************************************

function IMG_SetError(fmt: PChar): SInt32;
begin
     Result := SDL_SetError(fmt);
end;

//******************************************************************************

function IMG_GetError: PChar;
begin
     Result := SDL_GetError;
end;

end.
