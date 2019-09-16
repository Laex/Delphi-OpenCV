unit SDL2;
{*******************************************************************************

  SDL2.pas        v1.0  29/07/2013 first version for DelphiXE
                  v1.1  27/08/2013 add MACOS compability
                  v1.2  31/05/2014 delete sdl2.inc

  Simple DirectMedia Layer
  Copyright (C) 1997-2013 Sam Lantinga <slouken@libsdl.org>

  Pascal-Header-Conversion SDL from the JEDI-Team written by Domenique Louis and others.
  Pascal-Header-Conversion SDL2 from the Copyright (C) 2012/13 Tim Blume aka End.

  convert SDL/SDL2 to SDL2 for DelphiXE by Kotai 2013/2014  www.remakesonline.com

  The initial developer of this Pascal code was :
  Dominqiue Louis <Dominique@SavageSoftware.com.au>


*******************************************************************************}


interface

uses
   {$IFDEF FPC}
   SysUtils,Classes
   {$ELSE}
   System.SysUtils,System.Classes
   {$ENDIF}
    {$IFDEF MSWINDOWS}
       ,
       {$IFDEF FPC}
       Windows;
       {$ELSE}
       Winapi.Windows;
       {$ENDIF}
    {$ELSE}
       {$IFDEF LINUX}
          ,X
          ,XLib;
       {$ELSE}
          ;
       {$ENDIF}
    {$ENDIF}

const

  {$IFDEF MSWINDOWS}
    SDL_LibName = 'SDL2.dll';
  {$ENDIF}

  {$IFDEF ANDROID}
    SDL_LibName = 'libSDL2.so';
  {$ENDIF}

  {$IFDEF MACOS}
    {$IFDEF IOS}
      SDL_LibName = 'libSDL2.a';
    {$ELSE}
      SDL_LibName = 'SDL2';
//      SDL_LibName = '../Frameworks/SDL2.framework/Versions/A/SDL2';
    {$ENDIF}
  {$ENDIF}



////////////////////////////////////////////////////////////////////////////////////////////////////////
//////////////////////  SDLtype_s.h / SDL_stdinc.h  ////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////  

type

  TSDL_Bool = (SDL_FALSE,SDL_TRUE);

  DWord = LongWord;

  PUInt8Array = ^TUInt8Array;
  PUInt8 = ^UInt8;
  PPUInt8 = ^PUInt8;
  UInt8 = Byte;
  {$EXTERNALSYM UInt8}
  TUInt8Array = array [0..MAXINT shr 1] of UInt8;

  PUInt16 = ^UInt16;
  UInt16 = word;
  {$EXTERNALSYM UInt16}

  PSInt8 = ^SInt8;
  SInt8 = Shortint;
  {$EXTERNALSYM SInt8}

  PSInt16 = ^SInt16;
  SInt16 = smallint;
  {$EXTERNALSYM SInt16}

  PUInt32 = ^UInt32;
  UInt32 = Cardinal;
  {$EXTERNALSYM UInt32}

  SInt32 = LongInt;
  {$EXTERNALSYM SInt32}

  PFloat = ^Float;
  PInt = ^LongInt;

  PShortInt = ^ShortInt;

  PSInt64 = ^SInt64;
  SInt64 = Int64;

  {$IFNDEF WIN64}
    size_t = UInt32;
  {$ELSE}
    size_t = UInt64;
  {$ENDIF}
  {$EXTERNALSYM SIZE_T}

  Float = Single;
  {$EXTERNALSYM Float}
		


////////////////////////////////////////////////////////////////////////////////////////////////////////
//////////////////////        SDL_version.h         ////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////

  {**
   *  Information the version of SDL in use.
   *
   *  Represents the library's version as three levels: major revision
   *  (increments with massive changes, additions, and enhancements),
   *  minor revision (increments with backwards-compatible changes to the
   *  major revision), and patchlevel (increments with fixes to the minor
   *  revision).
   *
   *  SDL_VERSION
   *  SDL_GetVersion
   *}
type
  PSDL_Version = ^TSDL_Version;
  TSDL_Version = record
    major,         {**< major version *}
    minor,         {**< minor version *}
    patch: UInt8;  {**< update version *}
  end;

{* Printable format: "%d.%d.%d", MAJOR, MINOR, PATCHLEVEL
*}
const
  SDL_MAJOR_VERSION = 2;
  SDL_MINOR_VERSION = 0;
  SDL_PATCHLEVEL    = 0;

{**
 *  Macro to determine SDL version program was compiled against.
 *
 *  This macro fills in a SDL_version structure with the version of the
 *  library you compiled against. This is determined by what header the
 *  compiler uses. Note that if you dynamically linked the library, you might
 *  have a slightly newer or older version at runtime. That version can be
 *  determined with SDL_GetVersion(), which, unlike SDL_VERSION(),
 *  is not a macro.
 *
 *   x A pointer to a SDL_version struct to initialize.
 *
 *  SDL_version
 *  SDL_GetVersion
 *}
procedure SDL_VERSION(x: PSDL_Version);

{**
 *  This macro turns the version numbers into a numeric value:
 *
 *  (1,2,3) -> (1203)
 *
 *
 *  This assumes that there will never be more than 100 patchlevels.
 *}
function SDL_VERSIONNUM(X,Y,Z: UInt32): Cardinal;

  {**
   *  This is the version number macro for the current SDL version.
   *}
function SDL_COMPILEDVERSION(): Cardinal;

  {**
   *  This macro will evaluate to true if compiled with SDL at least X.Y.Z.
   *}
function SDL_VERSION_ATLEAST(X,Y,Z: Cardinal): Boolean;

  {**
   *  Get the version of SDL that is linked against your program.
   *
   *  If you are linking to SDL dynamically, then it is possible that the
   *  current version will be different than the version you compiled against.
   *  This function returns the current version, while SDL_VERSION() is a
   *  macro that tells you what version you compiled with.
   *
   *
   *  compiled: TSDL_Version;
   *  linked: TSDL_Version;
   *
   *  SDL_VERSION(@compiled);
   *  SDL_GetVersion(@linked);
   *  WriteLn('We compiled against SDL version: ' +
   *           IntToStr(compiled.major) +
   *           IntToStr(compiled.minor) +
   *           IntToStr(compiled.patch));
   *  WriteLn('But we linked against SDL version:' +
   *           IntToStr(compiled.major) +
   *           IntToStr(compiled.minor) +
   *           IntToStr(compiled.patch));
   *
   *
   *  This function may be called safely at any time, even before SDL_Init().
   *
   *  SDL_VERSION
   *}
procedure SDL_GetVersion(ver: PSDL_Version);
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_GetVersion' {$ENDIF} {$ENDIF};

  {**
   *  Get the code revision of SDL that is linked against your program.
   *
   *  Returns an arbitrary string (a hash value) uniquely identifying the
   *  exact revision of the SDL library in use, and is only useful in comparing
   *  against other revisions. It is NOT an incrementing number.
   *}
function SDL_GetRevision(): PChar;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_GetRevision' {$ENDIF} {$ENDIF};

  {**
   *  Get the revision number of SDL that is linked against your program.
   *
   *  Returns a number uniquely identifying the exact revision of the SDL
   *  library in use. It is an incrementing number based on commits to
   *  hg.libsdl.org.
   *}
function SDL_GetRevisionNumber(): SInt32;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_GetRevisionNumber' {$ENDIF} {$ENDIF};
 


////////////////////////////////////////////////////////////////////////////////////////////////////////  
//////////////////////         SDL_error.h          ////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////  

const
  ERR_MAX_STRLEN = 128;
  ERR_MAX_ARGS   = 5;

  {* Public functions *}

  {* SDL_SetError() unconditionally returns -1. *}
function SDL_SetError(const fmt: PChar): SInt32;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_SetError' {$ENDIF} {$ENDIF};

function SDL_GetError(): PChar;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_GetError' {$ENDIF} {$ENDIF};

procedure SDL_ClearError();
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_ClearError' {$ENDIF} {$ENDIF};

  {*Internal error functions*}
  {**
   *  Internal error functions
   *
   *  Private error reporting function - used internally.
   *}

    {
#define SDL_OutOfMemory()   SDL_Error(SDL_ENOMEM)
#define SDL_Unsupported()   SDL_Error(SDL_UNSUPPORTED)
#define SDL_InvalidParamError(param)    SDL_SetError("Parameter '%s' is invalid", (param))
   }
type
  TSDL_ErrorCode = (SDL_ENOMEM,
                    SDL_EFREAD,
                    SDL_EFWRITE,
                    SDL_EFSEEK,
                    SDL_UNSUPPORTED,
                    SDL_LASTERROR);

  TSDL_Error = record
    {* This is a numeric value corresponding to the current error *}
    error: SInt32;

    {* This is a key used to index into a language hashtable containing
       internationalized versions of the SDL error messages.  If the key
       is not in the hashtable, or no hashtable is available, the key is
       used directly as an error message format string.
     *}
    key: array [0..ERR_MAX_STRLEN] of Char;
//    key: String[ERR_MAX_STRLEN];    **KTI**

    {* These are the arguments for the error functions *}
    argc: SInt32;
    case SInt32 of
         {* What is a character anyway?  (UNICODE issues) *}
      0: (value_c: Byte;);
      1: (value_ptr: Pointer;);
      2: (value_i: SInt32;);
      3: (value_f: Double;);
      4: (buf: array [0..ERR_MAX_STRLEN] of Char;);
//      4: (buf: String[ERR_MAX_STRLEN];);  **KTI**
  end;

  {* SDL_Error() unconditionally returns -1. *}
function SDL_Error(code: TSDL_ErrorCode): SInt32;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_Error' {$ENDIF} {$ENDIF};




////////////////////////////////////////////////////////////////////////////////////////////////////////
//////////////////////         SDL_rwops.h          ////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////

const
  {* RWops Types *}
  SDL_RWOPS_UNKNOWN	  = 0;	{* Unknown stream type *}
  SDL_RWOPS_WINFILE	  = 1;	{* Win32 file *}
  SDL_RWOPS_STDFILE	  = 2;	{* Stdio file *}
  SDL_RWOPS_JNIFILE	  = 3;	{* Android asset *}
  SDL_RWOPS_MEMORY    =	4;	{* Memory stream *}
  SDL_RWOPS_MEMORY_RO =	5;	{* Read-Only memory stream *}

type
  PSDL_RWops = ^TSDL_RWops;

  {**
   * This is the read/write operation structure -- very basic.
   *}

  {**
   *  Return the size of the file in this rwops, or -1 if unknown
   *}
  TSize = function(context: PSDL_RWops): SInt64; cdecl;

  {**
   *  Seek to offset relative to whence, one of stdio's whence values:
   *  RW_SEEK_SET, RW_SEEK_CUR, RW_SEEK_END
   *
   *  the final offset in the data stream, or -1 on error.
   *}
  TSeek = function(context: PSDL_RWops; offset: SInt64; whence: SInt32): SInt64; cdecl;

  {**
   *  Read up to maxnum objects each of size size from the data
   *  stream to the area pointed at by ptr.
   *
   *  the number of objects read, or 0 at error or end of file.
   *}

   TRead = function(context: PSDL_RWops; ptr: Pointer; size: size_t; maxnum: size_t): size_t; cdecl;

  {**
   *  Write exactly num objects each of size size from the area
   *  pointed at by ptr to data stream.
   *
   *  the number of objects written, or 0 at error or end of file.
   *}

   TWrite = function(context: PSDL_RWops; const ptr: Pointer; size: size_t; num: size_t): size_t; cdecl;

  {**
   *  Close and free an allocated SDL_RWops structure.
   *
   *  0 if successful or -1 on write error when flushing data.
   *}

  TClose =  function(context: PSDL_RWops): SInt32; cdecl;

  TStdio = record
    autoclose: TSDL_Bool;
	   fp: file;
  end;

  TMem = record
    base: PUInt8;
	   here: PUInt8;
	   stop: PUInt8;
  end;

  TUnknown = record
    data1: Pointer;
  end;

  TAndroidIO = record
    fileNameRef: Pointer;
    inputStreamRef: Pointer;
    readableByteChannelRef: Pointer;
    readMethod: Pointer;
    assetFileDescriptorRef: Pointer;
    position: LongInt;
    size: LongInt;
    offset: LongInt;
    fd: SInt32;
  end;

  TWindowsIOBuffer = record
    data: Pointer;
	   size: size_t;
	   left: size_t;
  end;

  TWindowsIO = record
    append: TSDL_Bool;
    h: Pointer;
    buffer: TWindowsIOBuffer;
  end;

  TSDL_RWops = packed record
    size: TSize;
    seek: TSeek;
    read: TRead;
    write: TWrite;
    close: TClose;
    _type: UInt32;
    case Integer of
      0: (stdio: TStdio);
      1: (mem: TMem);
      2: (unknown: TUnknown);
      {$IFDEF ANDROID}
      3: (androidio: TAndroidIO);
      {$ENDIF}
      {$IFDEF MSWINDOWS}
      3: (windowsio: TWindowsIO);
      {$ENDIF}
  end;

  {**
   *  RWFrom functions
   *
   *  Functions to create SDL_RWops structures from various data streams.
   *}

function SDL_RWFromFile(const _file: PChar; const mode: PChar): PSDL_RWops;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_RWFromFile' {$ENDIF} {$ENDIF};


function SDL_RWFromFP(fp: Pointer; autoclose: TSDL_Bool): PSDL_RWops;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_RWFromFP' {$ENDIF} {$ENDIF};

function SDL_RWFromMem(mem: Pointer; size: SInt32): PSDL_RWops;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_RWFromMem' {$ENDIF} {$ENDIF};

function SDL_RWFromConstMem(const mem: Pointer; size: SInt32): PSDL_RWops;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_RWFromConstMem' {$ENDIF} {$ENDIF};

{*RWFrom functions*}


function SDL_AllocRW(): PSDL_RWops;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_AllocRW' {$ENDIF} {$ENDIF};

procedure SDL_FreeRW(area: PSDL_RWops);
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_FreeRW' {$ENDIF} {$ENDIF};

const
  RW_SEEK_SET = 0;       {**< Seek from the beginning of data *}
  RW_SEEK_CUR = 1;       {**< Seek relative to current read point *}
  RW_SEEK_END = 2;       {**< Seek relative to the end of data *}

  {**
   *  Read/write macros
   *
   *  Macros to easily read and write from an SDL_RWops structure.
   *}

  function SDL_RWsize(ctx: PSDL_RWops): SInt64;
  function SDL_RWseek(ctx: PSDL_RWops; offset: SInt64; whence: SInt32): SInt64;
  function SDL_RWtell(ctx: PSDL_RWops): SInt64;
  function SDL_RWread(ctx: PSDL_RWops; ptr: Pointer; size: size_t; n: size_t): size_t;
  function SDL_RWwrite(ctx: PSDL_RWops; ptr: Pointer; size: size_t; n: size_t): size_t;
  function SDL_RWclose(ctx: PSDL_RWops): SInt32;
  { Read/write macros }


  {**
   *  Read endian functions
   *
   *  Read an item of the specified endianness and return in native format.
   *}

function SDL_ReadU8(src: PSDL_RWops): UInt8;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_ReadU8' {$ENDIF} {$ENDIF};

function SDL_ReadLE16(src: PSDL_RWops): UInt16;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_ReadLE16' {$ENDIF} {$ENDIF};

function SDL_ReadBE16(src: PSDL_RWops): UInt16;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_ReadBE16' {$ENDIF} {$ENDIF};

function SDL_ReadLE32(src: PSDL_RWops): UInt32;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_ReadLE32' {$ENDIF} {$ENDIF};

function SDL_ReadBE32(src: PSDL_RWops): UInt32;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_ReadBE32' {$ENDIF} {$ENDIF};

function SDL_ReadLE64(src: PSDL_RWops): UInt64;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_ReadLE64' {$ENDIF} {$ENDIF};

function SDL_ReadBE64(src: PSDL_RWops): UInt64;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_ReadBE64' {$ENDIF} {$ENDIF};

  {*Read endian functions*}

  {**
   *  Write endian functions
   *
   *  Write an item of native format to the specified endianness.
   *}

function SDL_WriteU8(dst: PSDL_RWops; value: UInt8): size_t;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_WriteU8' {$ENDIF} {$ENDIF};

function SDL_WriteLE16(dst: PSDL_RWops; value: UInt16): size_t;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_WriteLE16' {$ENDIF} {$ENDIF};

function SDL_WriteBE16(dst: PSDL_RWops; value: UInt16): size_t;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_WriteBE16' {$ENDIF} {$ENDIF};

function SDL_WriteLE32(dst: PSDL_RWops; value: UInt32): size_t;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_WriteLE32' {$ENDIF} {$ENDIF};

function SDL_WriteBE32(dst: PSDL_RWops; value: UInt32): size_t;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_WriteBE32' {$ENDIF} {$ENDIF};

function SDL_WriteLE64(dst: PSDL_RWops; value: UInt64): size_t;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_WriteLE64' {$ENDIF} {$ENDIF};

function SDL_WriteBE64(dst: PSDL_RWops; value: UInt64): size_t;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_WriteBE64' {$ENDIF} {$ENDIF};
  { Write endian functions }






////////////////////////////////////////////////////////////////////////////////////////////////////////
//////////////////////         SDL_audio.h          ////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////

 {**
   *   Audio format flags.
   *
   *  These are what the 16 bits in SDL_AudioFormat currently mean...
   *  (Unspecified bits are always zero).
   *
   *
      ++-----------------------sample is signed if set
      ||
      ||       ++-----------sample is bigendian if set
      ||       ||
      ||       ||          ++---sample is float if set
      ||       ||          ||
      ||       ||          || +---sample bit size---+
      ||       ||          || |                     |
      15 14 13 12 11 10 09 08 07 06 05 04 03 02 01 00
   *
   *  There are macros in SDL 2.0 and later to query these bits.
   *}
type
  TSDL_AudioFormat = UInt16;

  {**
   *   Audio flags
   *}
const
  SDL_AUDIO_MASK_BITSIZE      = ($FF);
  SDL_AUDIO_MASK_DATATYPE     = (1 shl 8);
  SDL_AUDIO_MASK_ENDIAN       = (1 shl 12);
  SDL_AUDIO_MASK_SIGNED       = (1 shl 15);

function SDL_AUDIO_BITSIZE(x: Cardinal): Cardinal;
function SDL_AUDIO_ISFLOAT(x: Cardinal): Cardinal;
function SDL_AUDIO_ISBIGENDIAN(x: Cardinal): Cardinal;
function SDL_AUDIO_ISSIGNED(x: Cardinal): Cardinal;
function SDL_AUDIO_ISINT(x: Cardinal): Cardinal;
function SDL_AUDIO_ISLITTLEENDIAN(x: Cardinal): Cardinal;
function SDL_AUDIO_ISUNSIGNED(x: Cardinal): Cardinal;

  {**
   *   Audio format flags
   *
   *  Defaults to LSB byte order.
   *}
const
  AUDIO_U8      = $0008;  {**< Unsigned 8-bit samples *}
  AUDIO_S8      = $8008;  {**< Signed 8-bit samples *}
  AUDIO_U16LSB  = $0010;  {**< Unsigned 16-bit samples *}
  AUDIO_S16LSB  = $8010;  {**< Signed 16-bit samples *}
  AUDIO_U16MSB  = $1010;  {**< As above, but big-endian byte order *}
  AUDIO_S16MSB  = $9010;  {**< As above, but big-endian byte order *}
  AUDIO_U16     = AUDIO_U16LSB;
  AUDIO_S16     = AUDIO_S16LSB;

  {**
   *   int32 support
   *}
const
  AUDIO_S32LSB  = $8020;  {**< 32-bit integer samples *}
  AUDIO_S32MSB  = $9020;  {**< As above, but big-endian byte order *}
  AUDIO_S32     = AUDIO_S32LSB;

  {**
   *   float32 support
   *}
const
  AUDIO_F32LSB  = $8120;  {**< 32-bit floating point samples *}
  AUDIO_F32MSB  = $9120;  {**< As above, but big-endian byte order *}
  AUDIO_F32     = AUDIO_F32LSB;

  {**
   *   Native audio byte ordering
   *}
         {
#if SDL_BYTEORDER == SDL_LIL_ENDIAN
#define AUDIO_U16SYS    AUDIO_U16LSB
#define AUDIO_S16SYS    AUDIO_S16LSB
#define AUDIO_S32SYS    AUDIO_S32LSB
#define AUDIO_F32SYS    AUDIO_F32LSB
#else
#define AUDIO_U16SYS    AUDIO_U16MSB
#define AUDIO_S16SYS    AUDIO_S16MSB
#define AUDIO_S32SYS    AUDIO_S32MSB
#define AUDIO_F32SYS    AUDIO_F32MSB
#endif}

  {**
   *   Allow change flags
   *
   *  Which audio format changes are allowed when opening a device.
   *}
const
  SDL_AUDIO_ALLOW_FREQUENCY_CHANGE  = $00000001;
  SDL_AUDIO_ALLOW_FORMAT_CHANGE     = $00000002;
  SDL_AUDIO_ALLOW_CHANNELS_CHANGE   = $00000004;
  SDL_AUDIO_ALLOW_ANY_CHANGE        = (SDL_AUDIO_ALLOW_FREQUENCY_CHANGE or
                                       SDL_AUDIO_ALLOW_FORMAT_CHANGE or
									                              SDL_AUDIO_ALLOW_CHANNELS_CHANGE);

  {*Audio flags*}

  {**
   *  This function is called when the audio device needs more data.
   *
   *   userdata An application-specific parameter saved in
   *                  the SDL_AudioSpec structure
   *   stream A pointer to the audio data buffer.
   *   len    The length of that buffer in bytes.
   *
   *  Once the callback returns, the buffer will no longer be valid.
   *  Stereo samples are stored in a LRLRLR ordering.
   *}
type
  TSDL_AudioCallback = procedure(userdata: Pointer; stream: PUInt8; len: Integer);

  {**
   *  The calculated values in this structure are calculated by SDL_OpenAudio().
   *}
type
  PSDL_AudioSpec = ^TSDL_AudioSpec;
  TSDL_AudioSpec = record
    freq: Integer;                {**< DSP frequency -- samples per second *}
    format: TSDL_AudioFormat;     {**< Audio data format *}
    channels: UInt8;              {**< Number of channels: 1 mono, 2 stereo *}
    silence: UInt8;               {**< Audio buffer silence value (calculated) *}
    samples: UInt16;              {**< Audio buffer size in samples (power of 2) *}
    padding: UInt16;              {**< Necessary for some compile environments *}
    size: UInt32;                 {**< Audio buffer size in bytes (calculated) *}
    callback: TSDL_AudioCallback;
    userdata: Pointer;
  end;

  PSDL_AudioCVT = ^TSDL_AudioCVT;
  TSDL_AudioFilter = procedure(cvt: PSDL_AudioCVT; format: TSDL_AudioFormat);

  {**
   *  A structure to hold a set of audio conversion filters and buffers.
   *}
  TSDL_AudioCVT = record
    needed: Integer;                    		  {**< Set to 1 if conversion possible *}
    src_format: TSDL_AudioFormat; 		        {**< Source audio format *}
    dst_format: TSDL_AudioFormat;		          {**< Target audio format *}
    rate_incr: Double;          		          {**< Rate conversion increment *}
    buf: PUInt8;                	    	      {**< Buffer to hold entire audio data *}
    len: Integer;                    		      {**< Length of original audio buffer *}
    len_cvt: Integer;               		      {**< Length of converted audio buffer *}
    len_mult: Integer;              		      {**< buffer must be len*len_mult big *}
    len_ratio: Double;          		          {**< Given len, final size is len*len_ratio *}
    filters: array[0..9] of TSDL_AudioFilter; {**< Filter list *}
    filter_index: Integer;           		      {**< Current audio conversion function *}
  end;


  {* Function prototypes *}

  {**
   *   Driver discovery functions
   *
   *  These functions return the list of built in audio drivers, in the
   *  order that they are normally initialized by default.
   *}

function SDL_GetNumAudioDrivers(): Integer;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_GetNumAudioDrivers' {$ENDIF} {$ENDIF};

function SDL_GetAudioDriver(index: Integer): PChar;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_GetAudioDriver' {$ENDIF} {$ENDIF};

  {**
   *   Initialization and cleanup
   *
   *  These functions are used internally, and should not be used unless
   *  you have a specific need to specify the audio driver you want to
   *  use.  You should normally use SDL_Init() or SDL_InitSubSystem().
   *}

function SDL_AudioInit(driver_name: PChar): Integer;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_AudioInit' {$ENDIF} {$ENDIF};

procedure SDL_AudioQuit();
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_AudioQuit' {$ENDIF} {$ENDIF};

  {**
   *  This function returns the name of the current audio driver, or NULL
   *  if no driver has been initialized.
   *}
function SDL_GetCurrentAudioDriver(): PChar;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_GetCurrentAudioDriver' {$ENDIF} {$ENDIF};

  {**
   *  This function opens the audio device with the desired parameters, and
   *  returns 0 if successful, placing the actual hardware parameters in the
   *  structure pointed to by obtained.  If obtained is NULL, the audio
   *  data passed to the callback function will be guaranteed to be in the
   *  requested format, and will be automatically converted to the hardware
   *  audio format if necessary.  This function returns -1 if it failed
   *  to open the audio device, or couldn't set up the audio thread.
   *
   *  When filling in the desired audio spec structure,
   *    - desired->freq should be the desired audio frequency in samples-per-
   *      second.
   *    - desired->format should be the desired audio format.
   *    - desired->samples is the desired size of the audio buffer, in
   *      samples.  This number should be a power of two, and may be adjusted by
   *      the audio driver to a value more suitable for the hardware.  Good values
   *      seem to range between 512 and 8096 inclusive, depending on the
   *      application and CPU speed.  Smaller values yield faster response time,
   *      but can lead to underflow if the application is doing heavy processing
   *      and cannot fill the audio buffer in time.  A stereo sample consists of
   *      both right and left channels in LR ordering.
   *      Note that the number of samples is directly related to time by the
   *      following formula:  ms := (samples*1000)/freq;
   *    - desired->size is the size in bytes of the audio buffer, and is
   *      calculated by SDL_OpenAudio().
   *    - desired->silence is the value used to set the buffer to silence,
   *      and is calculated by SDL_OpenAudio().
   *    - desired->callback should be set to a function that will be called
   *      when the audio device is ready for more data.  It is passed a pointer
   *      to the audio buffer, and the length in bytes of the audio buffer.
   *      This function usually runs in a separate thread, and so you should
   *      protect data structures that it accesses by calling SDL_LockAudio()
   *      and SDL_UnlockAudio() in your code.
   *    - desired->userdata is passed as the first parameter to your callback
   *      function.
   *
   *  The audio device starts out playing silence when it's opened, and should
   *  be enabled for playing by calling SDL_PauseAudio(0) when you are ready
   *  for your audio callback function to be called.  Since the audio driver
   *  may modify the requested size of the audio buffer, you should allocate
   *  any local mixing buffers after you open the audio device.
   *}
function SDL_OpenAudio(desired: PSDL_AudioSpec; obtained: PSDL_AudioSpec): Integer;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_OpenAudio' {$ENDIF} {$ENDIF};

  {**
   *  SDL Audio Device IDs.
   *
   *  A successful call to SDL_OpenAudio() is always device id 1, and legacy
   *  SDL audio APIs assume you want this device ID. SDL_OpenAudioDevice() calls
   *  always returns devices >= 2 on success. The legacy calls are good both
   *  for backwards compatibility and when you don't care about multiple,
   *  specific, or capture devices.
   *}
type
  TSDL_AudioDeviceID = UInt32;

  {**
   *  Get the number of available devices exposed by the current driver.
   *  Only valid after a successfully initializing the audio subsystem.
   *  Returns -1 if an explicit list of devices can't be determined; this is
   *  not an error. For example, if SDL is set up to talk to a remote audio
   *  server, it can't list every one available on the Internet, but it will
   *  still allow a specific host to be specified to SDL_OpenAudioDevice().
   *
   *  In many common cases, when this function returns a value <= 0, it can still
   *  successfully open the default device (NULL for first argument of
   *  SDL_OpenAudioDevice()).
   *}
function SDL_GetNumAudioDevices(iscapture: Integer): Integer;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_GetNumAudioDevices' {$ENDIF} {$ENDIF};

  {**
   *  Get the human-readable name of a specific audio device.
   *  Must be a value between 0 and (number of audio devices-1).
   *  Only valid after a successfully initializing the audio subsystem.
   *  The values returned by this function reflect the latest call to
   *  SDL_GetNumAudioDevices(); recall that function to redetect available
   *  hardware.
   *
   *  The string returned by this function is UTF-8 encoded, read-only, and
   *  managed internally. You are not to free it. If you need to keep the
   *  string for any length of time, you should make your own copy of it, as it
   *  will be invalid next time any of several other SDL functions is called.
   *}
function SDL_GetAudioDeviceName(index: Integer; iscapture: Integer): PChar;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_GetAudioDeviceName' {$ENDIF} {$ENDIF};

  {**
   *  Open a specific audio device. Passing in a device name of NULL requests
   *  the most reasonable default (and is equivalent to calling SDL_OpenAudio()).
   *
   *  The device name is a UTF-8 string reported by SDL_GetAudioDeviceName(), but
   *  some drivers allow arbitrary and driver-specific strings, such as a
   *  hostname/IP address for a remote audio server, or a filename in the
   *  diskaudio driver.
   *
   *   0 on error, a valid device ID that is >= 2 on success.
   *
   *  SDL_OpenAudio(), unlike this function, always acts on device ID 1.
   *}
function SDL_OpenAudioDevice(device: PChar; iscapture: Integer; desired: PSDL_AudioSpec; obtained: PSDL_AudioSpec; allowed_changes: Integer): TSDL_AudioDeviceID;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_OpenAudioDevice' {$ENDIF} {$ENDIF};

  {**
   *   Audio state
   *
   *  Get the current audio state.
   *}

type
  TSDL_AudioStatus = (SDL_AUDIO_STOPPED,SDL_AUDIO_PLAYING,SDL_AUDIO_PAUSED);

function SDL_GetAudioStatus(): TSDL_AudioStatus;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_GetAudioStatus' {$ENDIF} {$ENDIF};

function SDL_GetAudioDeviceStatus(dev: TSDL_AudioDeviceID): TSDL_AudioStatus;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_GetAudioDeviceStatus' {$ENDIF} {$ENDIF};
  {*Audio State*}

  {**
   *   Pause audio functions
   *
   *  These functions pause and unpause the audio callback processing.
   *  They should be called with a parameter of 0 after opening the audio
   *  device to start playing sound.  This is so you can safely initialize
   *  data for your callback function after opening the audio device.
   *  Silence will be written to the audio device during the pause.
   *}

procedure SDL_PauseAudio(pause_on: Integer);
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_PauseAudio' {$ENDIF} {$ENDIF};

procedure SDL_PauseAudioDevice(dev: TSDL_AudioDeviceID; pause_on: Integer);
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_PauseAudioDevice' {$ENDIF} {$ENDIF};
  {*Pause audio functions*}

  {**
   *  This function loads a WAVE from the data source, automatically freeing
   *  that source if freesrc is non-zero.  For example, to load a WAVE file,
   *  you could do:
   *
   *      SDL_LoadWAV_RW(SDL_RWFromFile("sample.wav", "rb"), 1, ...);
   *
   *
   *  If this function succeeds, it returns the given SDL_AudioSpec,
   *  filled with the audio data format of the wave data, and sets
   *   *audio_buf to a malloc()'d buffer containing the audio data,
   *  and sets  *audio_len to the length of that audio buffer, in bytes.
   *  You need to free the audio buffer with SDL_FreeWAV() when you are
   *  done with it.
   *
   *  This function returns NULL and sets the SDL error message if the
   *  wave file cannot be opened, uses an unknown data format, or is
   *  corrupt.  Currently raw and MS-ADPCM WAVE files are supported.
   *}
function SDL_LoadWAV_RW(src: PSDL_RWops; freesrc: Integer; spec: PSDL_AudioSpec; audio_buf: PPUInt8; audio_len: PUInt32): PSDL_AudioSpec;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_LoadWAV_RW' {$ENDIF} {$ENDIF};

  {**
   *  Loads a WAV from a file.
   *  Compatibility convenience function.
   *}

function SDL_LoadWAV(_file: PChar; spec: PSDL_AudioSpec; audio_buf: PPUInt8; audio_len: PUInt32): PSDL_AudioSpec;

  {**
   *  This function frees data previously allocated with SDL_LoadWAV_RW()
   *}
procedure SDL_FreeWAV(audio_buf: PUInt8);
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_FreeWAV' {$ENDIF} {$ENDIF};

  {**
   *  This function takes a source format and rate and a destination format
   *  and rate, and initializes the cvt structure with information needed
   *  by SDL_ConvertAudio() to convert a buffer of audio data from one format
   *  to the other.
   *
   *   -1 if the format conversion is not supported, 0 if there's
   *  no conversion needed, or 1 if the audio filter is set up.
   *}
function SDL_BuildAudioCVT(cvt: PSDL_AudioCVT; src_format: TSDL_AudioFormat; src_channels: UInt8; src_rate: Integer; dst_format: TSDL_AudioFormat; dst_channels: UInt8; dst_rate: Integer): Integer;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_BuildAudioCVT' {$ENDIF} {$ENDIF};

{**
 *  Once you have initialized the cvt structure using SDL_BuildAudioCVT(),
 *  created an audio buffer cvt->buf, and filled it with cvt->len bytes of
 *  audio data in the source format, this function will convert it in-place
 *  to the desired format.
 *
 *  The data conversion may expand the size of the audio data, so the buffer
 *  cvt->buf should be allocated after the cvt structure is initialized by
 *  SDL_BuildAudioCVT(), and should be cvt->len*cvt->len_mult bytes long.
 *}
function SDL_ConvertAudio(cvt: PSDL_AudioCVT): Integer;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_ConvertAudio' {$ENDIF} {$ENDIF};

const
  SDL_MIX_MAXVOLUME = 128;

  {**
   *  This takes two audio buffers of the playing audio format and mixes
   *  them, performing addition, volume adjustment, and overflow clipping.
   *  The volume ranges from 0 - 128, and should be set to ::SDL_MIX_MAXVOLUME
   *  for full audio volume.  Note this does not change hardware volume.
   *  This is provided for convenience -- you can mix your own audio data.
   *}
procedure SDL_MixAudio(dst: PUInt8; src: PUInt8; len: UInt32; volume: Integer);
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_MixAudio' {$ENDIF} {$ENDIF};

  {**
   *  This works like SDL_MixAudio(), but you specify the audio format instead of
   *  using the format of audio device 1. Thus it can be used when no audio
   *  device is open at all.
   *}
procedure SDL_MixAudioFormat(dst: PUInt8; src: PUInt8; format: TSDL_AudioFormat; len: UInt32; volume: Integer);
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_MixAudioFormat' {$ENDIF} {$ENDIF};

  {**
   *   Audio lock functions
   *
   *  The lock manipulated by these functions protects the callback function.
   *  During a SDL_LockAudio()/SDL_UnlockAudio() pair, you can be guaranteed that
   *  the callback function is not running.  Do not call these from the callback
   *  function or you will cause deadlock.
   *}

procedure SDL_LockAudio();
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_LockAudio' {$ENDIF} {$ENDIF};

procedure SDL_LockAudioDevice(dev: TSDL_AudioDeviceID);
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_LockAudioDevice' {$ENDIF} {$ENDIF};

procedure SDL_UnlockAudio();
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_Unlock' {$ENDIF} {$ENDIF};

procedure SDL_UnlockAudioDevice(dev: TSDL_AudioDeviceID);
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_UnlockAudioDevice' {$ENDIF} {$ENDIF};
  {*Audio lock functions*}

  {**
   *  This function shuts down audio processing and closes the audio device.
   *}
procedure SDL_CloseAudio();
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_CloseAudio' {$ENDIF} {$ENDIF};

procedure SDL_CloseAudioDevice(dev: TSDL_AudioDeviceID);
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_CloseAudioDevice' {$ENDIF} {$ENDIF};

  {**
   *  1 if audio device is still functioning, zero if not, -1 on error.
   *}
function SDL_AudioDeviceConnected(dev: TSDL_AudioDeviceID): Integer;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_AudioDeviceConnected' {$ENDIF} {$ENDIF};





////////////////////////////////////////////////////////////////////////////////////////////////////////
//////////////////////         SDL_power.h          ////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////

  {**
   *  The basic state for the system's power supply.
   *}
type
  TSDL_PowerState = (SDL_POWERSTATE_UNKNOWN,      {**< cannot determine power status *}
                     SDL_POWERSTATE_ON_BATTERY,   {**< Not plugged in, running on the battery *}
                     SDL_POWERSTATE_NO_BATTERY,   {**< Plugged in, no battery available *}
                     SDL_POWERSTATE_CHARGING,     {**< Plugged in, charging battery *}
                     SDL_POWERSTATE_CHARGED);     {**< Plugged in, battery charged *}

  {**
   *  Get the current power supply details.
   *
   *   secs Seconds of battery life left. You can pass a NULL here if
   *        you don't care. Will return -1 if we can't determine a
   *        value, or we're not running on a battery.
   *
   *   pct Percentage of battery life left, between 0 and 100. You can
   *       pass a NULL here if you don't care. Will return -1 if we
   *       can't determine a value, or we're not running on a battery.
   *
   *  The state of the battery (if any).
   *}
function SDL_GetPowerInfo(secs: PInt; pct: PInt): TSDL_PowerState;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_GetPowerInfo' {$ENDIF} {$ENDIF};

	  

////////////////////////////////////////////////////////////////////////////////////////////////////////
//////////////////////        SDL_thread .h         ////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////

{* The SDL thread structure, defined in SDL_thread.c *}
//todo!
type
  {* The SDL thread priority
   *
   * Note: On many systems you require special privileges to set high priority.
   *}

  TSDL_ThreadPriority = (SDL_THREAD_PRIORITY_LOW,
                         SDL_THREAD_PRIORITY_NORMAL,
                         SDL_THREAD_PRIORITY_HIGH);

  {* The function passed to SDL_CreateThread()
     It is passed a void* user context parameter and returns an int.
   *}
  PSDL_ThreadFunction = ^TSDL_ThreadFunction;
  TSDL_ThreadFunction = function(data: Pointer): Integer; cdecl;

  {* The SDL thread ID *}
  TSDL_ThreadID = LongWord;
   {
  PSDL_Thread = Pointer;
     }

  PSDL_Thread = ^TSDL_Thread;
  TSDL_Thread = record
    threadid: TSDL_ThreadID;
    handle: THandle;
    status: SInt32;
    errbuf: TSDL_Error;
    name: PChar;
    data: Pointer;
  end;

  TSDL_TLSID = Cardinal;

{$IFDEF MSWINDOWS}
  {**
   *  SDL_thread.h
   *
   *  We compile SDL into a DLL. This means, that it's the DLL which
   *  creates a new thread for the calling process with the SDL_CreateThread()
   *  API. There is a problem with this, that only the RTL of the SDL.DLL will
   *  be initialized for those threads, and not the RTL of the calling
   *  application!
   *
   *  To solve this, we make a little hack here.
   *
   *  We'll always use the caller's _beginthread() and _endthread() APIs to
   *  start a new thread. This way, if it's the SDL.DLL which uses this API,
   *  then the RTL of SDL.DLL will be used to create the new thread, and if it's
   *  the application, then the RTL of the application will be used.
   *
   *  So, in short:
   *  Always use the _beginthread() and _endthread() of the calling runtime
   *  library!
   *}
{$DEFINE SDL_PASSED_BEGINTHREAD_ENDTHREAD}

type
  TThreadID = Cardinal;

  TpfnSDL_CurrentBeginThread = function(SecurityAttributes: Pointer; StackSize: LongWord; ThreadFunc: TThreadFunc; Parameter: Pointer; CreationFlags: LongWord; var ThreadId: TThreadID): Integer;

  TpfnSDL_CurrentEndThread = procedure(ExitCode: Integer);

  {**
   *  Create a thread.
   *}
function SDL_CreateThread(fn: TSDL_ThreadFunction; name: PChar; data: Pointer; pfnBeginThread: TpfnSDL_CurrentBeginThread; pfnEndThread: TpfnSDL_CurrentEndThread): PSDL_Thread;
cdecl; overload; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_CreateThread' {$ENDIF} {$ENDIF};

  {**
   *  Create a thread.
   *}
function SDL_CreateThread(fn: TSDL_ThreadFunction; name: PChar; data: Pointer): PSDL_Thread; overload;

{$ELSE}

  {**
   *  Create a thread.
   *
   *   Thread naming is a little complicated: Most systems have very small
   *    limits for the string length (BeOS has 32 bytes, Linux currently has 16,
   *    Visual C++ 6.0 has nine!), and possibly other arbitrary rules. You'll
   *    have to see what happens with your system's debugger. The name should be
   *    UTF-8 (but using the naming limits of C identifiers is a better bet).
   *   There are no requirements for thread naming conventions, so long as the
   *    string is null-terminated UTF-8, but these guidelines are helpful in
   *    choosing a name:
   *
   *    http://stackoverflow.com/questions/149932/naming-conventions-for-threads
   *
   *   If a system imposes requirements, SDL will try to munge the string for
   *    it (truncate, etc), but the original string contents will be available
   *    from SDL_GetThreadName().
   *}
function SDL_CreateThread(fn: TSDL_ThreadFunction; name: PChar; data: Pointer): PSDL_Thread;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_CreateThread' {$ENDIF} {$ENDIF};

{$ENDIF}

  {**
   * Get the thread name, as it was specified in SDL_CreateThread().
   *  This function returns a pointer to a UTF-8 string that names the
   *  specified thread, or NULL if it doesn't have a name. This is internal
   *  memory, not to be free()'d by the caller, and remains valid until the
   *  specified thread is cleaned up by SDL_WaitThread().
   *}
function SDL_GetThreadName(thread: PSDL_Thread): PChar;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_GetThreadName' {$ENDIF} {$ENDIF};

  {**
   *  Get the thread identifier for the current thread.
   *}
function SDL_ThreadID(): TSDL_ThreadID;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_ThreadID' {$ENDIF} {$ENDIF};

  {**
   *  Get the thread identifier for the specified thread.
   *
   *  Equivalent to SDL_ThreadID() if the specified thread is NULL.
   *}
function SDL_GetThreadID(thread: PSDL_Thread): TSDL_ThreadID;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_GetThreadID' {$ENDIF} {$ENDIF};

  {**
   *  Set the priority for the current thread
   *}
function SDL_SetThreadPriority(priority: TSDL_ThreadPriority): SInt32;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_SetThreadPriority' {$ENDIF} {$ENDIF};

  {**
   *  Wait for a thread to finish.
   *
   *  The return code for the thread function is placed in the area
   *  pointed to by status, if status is not NULL.
   *}
procedure SDL_WaitThread(thread: PSDL_Thread; status: PInt);
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_WaitThread' {$ENDIF} {$ENDIF};

  {**
   *  Create an identifier that is globally visible to all threads but refers to data that is thread-specific.
   *
   *   The newly created thread local storage identifier, or 0 on error
   *
   *  static SDL_SpinLock tls_lock;
   *  static SDL_TLSID thread_local_storage;
   *
   *  void SetMyThreadData(void *value)
   *  {
   *      if (!thread_local_storage) {
   *          SDL_AtomicLock(&tls_lock);
   *          if (!thread_local_storage) {
   *              thread_local_storage = SDL_TLSCreate();
   *          }   {
   *          SDL_AtomicUnLock(&tls_lock);
   *      } {
   *      SDL_TLSSet(thread_local_storage, value);
   *  } {
   *
   *  void *GetMyThreadData(void)
   *  {
   *      return SDL_TLSGet(thread_local_storage);
   *  }{
   *
   *   SDL_TLSGet()
   *   SDL_TLSSet()
   *}
function SDL_TLSCreate(): TSDL_TLSID;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_TLSCreate' {$ENDIF} {$ENDIF};

  {**
   *  Get the value associated with a thread local storage ID for the current thread.
   *
   *   id The thread local storage ID
   *
   *   The value associated with the ID for the current thread, or NULL if no value has been set.
   *
   *   SDL_TLSCreate()
   *   SDL_TLSSet()
   *}
function SDL_TLSGet(id: TSDL_TLSID): Pointer;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_TLSGet' {$ENDIF} {$ENDIF};

  {**
   *  Set the value associated with a thread local storage ID for the current thread.
   *
   *   id The thread local storage ID
   *   value The value to associate with the ID for the current thread
   *   destructor_ A function called when the thread exits, to free the value.
   *
   *   0 on success, -1 on error
   *
   *   SDL_TLSCreate()
   *   SDL_TLSGet()
   *}
function SDL_TLSSet(id: TSDL_TLSID; value: Pointer; destructor_: Pointer): SInt32;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_TLSSet' {$ENDIF} {$ENDIF};

  

////////////////////////////////////////////////////////////////////////////////////////////////////////  
//////////////////////          SDL_mutex.h         ////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////  

  {**
   *  Synchronization functions which can time out return this value
   *  if they time out.
   *}
const
  SDL_MUTEX_TIMEDOUT = 1;

  {**
   *  This is the timeout value which corresponds to never time out.
   *}
  //SDL_MUTEX_MAXWAIT   (~(Uint32)0)


  {**
   *  Mutex functions
   *}
type
  {* The SDL mutex structure, defined in SDL_mutex.c *}
//  PSDL_Mutex = Pointer; //todo!
  PSDL_Mutex = ^TSDL_Mutex;    //***KTI***
  TSDL_Mutex = record
    id: THANDLE;
  end;

  {**
   *  Create a mutex, initialized unlocked.
   *}
function SDL_CreateMutex(): PSDL_Mutex;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_CreateMutex' {$ENDIF} {$ENDIF};

  {**
   *  Lock the mutex.
   *
   *   0, or -1 on error.
   *}
//#define SDL_mutexP(m)   SDL_LockMutex(m)
function SDL_LockMutex(mutex: PSDL_Mutex): SInt32;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_LockMutex' {$ENDIF} {$ENDIF};

  {**
   *  Try to lock the mutex
   *
   *   0, SDL_MUTEX_TIMEDOUT, or -1 on error
   *}
function SDL_TryLockMutex(mutex: PSDL_Mutex): SInt32;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_TryLockMutex' {$ENDIF} {$ENDIF};

  {**
   *  Unlock the mutex.
   *
   *   0, or -1 on error.
   *
   *   It is an error to unlock a mutex that has not been locked by
   *   the current thread, and doing so results in undefined behavior.
   *}
//#define SDL_mutexV(m)   SDL_UnlockMutex(m)
function SDL_UnlockMutex(mutex: PSDL_Mutex): SInt32;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_UnlockMutex' {$ENDIF} {$ENDIF};

  {**
   *  Destroy a mutex.
   *}
procedure SDL_DestroyMutex(mutex: PSDL_Mutex);
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_DestroyMutex' {$ENDIF} {$ENDIF};

  {*Mutex functions*}

  {**
   *   Semaphore functions
   *}
type
  {* The SDL semaphore structure, defined in SDL_sem.c *}
  PSDL_Sem = Pointer; //todo!

  {**
   *  Create a semaphore, initialized with value, returns NULL on failure.
   *}
function SDL_CreateSemaphore(initial_value: UInt32): PSDL_sem;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_CreateSemaphore' {$ENDIF} {$ENDIF};

  {**
   *  Destroy a semaphore.
   *}
procedure SDL_DestroySemaphore(sem: PSDL_Sem);
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_DestroySemaphore' {$ENDIF} {$ENDIF};

  {**
   *  This function suspends the calling thread until the semaphore pointed
   *  to by sem has a positive count. It then atomically decreases the
   *  semaphore count.
   *}
function SDL_SemWait(sem: PSDL_Sem): SInt32;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_SemWait' {$ENDIF} {$ENDIF};

  {**
   *  Non-blocking variant of SDL_SemWait().
   *
   *   0 if the wait succeeds, SDL_MUTEX_TIMEDOUT if the wait would
   *   block, and -1 on error.
   *}
function SDL_SemTryWait(sem: PSDL_Sem): SInt32;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_SemTryWait' {$ENDIF} {$ENDIF};

  {**
   *  Variant of SDL_SemWait() with a timeout in milliseconds.
   *
   *   0 if the wait succeeds, ::SDL_MUTEX_TIMEDOUT if the wait does not
   *   succeed in the allotted time, and -1 on error.
   *
   *   On some platforms this function is implemented by looping with a
   *   delay of 1 ms, and so should be avoided if possible.
   *}
function SDL_SemWaitTimeout(sem: PSDL_Sem; ms: UInt32): SInt32;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_SemWaitTimeout' {$ENDIF} {$ENDIF};

  {**
   *  Atomically increases the semaphore's count (not blocking).
   *
   *   0, or -1 on error.
   *}
function SDL_SemPost(sem: PSDL_Sem): SInt32;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_SemPost' {$ENDIF} {$ENDIF};

  {**
   *  Returns the current count of the semaphore.
   *}
function SDL_SemValue(sem: PSDL_Sem): UInt32;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_SemValue' {$ENDIF} {$ENDIF};

  {*Semaphore functions*}

  {**
   *  Condition variable functions
   * }
type
  {* The SDL condition variable structure, defined in SDL_cond.c *}
  PSDL_Cond = Pointer; //todo!!

  {**
   *  Create a condition variable.
   *
   *  Typical use of condition variables:
   *
   *  Thread A:
   *    SDL_LockMutex(lock);
   *    while ( not condition )
   *    begin
   *      SDL_CondWait(cond, lock);
   *    end;
   *    SDL_UnlockMutex(lock);
   *
   *  Thread B:
   *    SDL_LockMutex(lock);
   *    ...
   *    condition := true;
   *    ...
   *    SDL_CondSignal(cond);
   *    SDL_UnlockMutex(lock);
   *
   *  There is some discussion whether to signal the condition variable
   *  with the mutex locked or not.  There is some potential performance
   *  benefit to unlocking first on some platforms, but there are some
   *  potential race conditions depending on how your code is structured.
   *
   *  In general it's safer to signal the condition variable while the
   *  mutex is locked.
   *}
function SDL_CreateCond(): PSDL_Cond;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_CreateCond' {$ENDIF} {$ENDIF};

  {**
   *  Destroy a condition variable.
   *}
procedure SDL_DestroyCond(cond: PSDL_Cond);
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_DestroyCond' {$ENDIF} {$ENDIF};

  {**
   *  Restart one of the threads that are waiting on the condition variable.
   *
   *   0 or -1 on error.
   *}
function SDL_CondSignal(cond: PSDL_Cond): SInt32;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_CondSignal' {$ENDIF} {$ENDIF};

  {**
   *  Restart all threads that are waiting on the condition variable.
   *
   *   0 or -1 on error.
   *}
function SDL_CondBroadcast(cond: PSDL_Cond): SInt32;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_CondBroadcast' {$ENDIF} {$ENDIF};

  {**
   *  Wait on the condition variable, unlocking the provided mutex.
   *
   *   The mutex must be locked before entering this function!
   *
   *  The mutex is re-locked once the condition variable is signaled.
   *
   *   0 when it is signaled, or -1 on error.
   *}
function SDL_CondWait(cond: PSDL_Cond; mutex: PSDL_Mutex): SInt32;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_CondWait' {$ENDIF} {$ENDIF};

  {**
   *  Waits for at most ms milliseconds, and returns 0 if the condition
   *  variable is signaled, SDL_MUTEX_TIMEDOUT if the condition is not
   *  signaled in the allotted time, and -1 on error.
   *
   *   On some platforms this function is implemented by looping with a
   *   delay of 1 ms, and so should be avoided if possible.
   *}
function SDL_CondWaitTimeout(cond: PSDL_Cond; mutex: PSDL_Mutex; ms: UInt32): SInt32;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_CondWaitTimeout' {$ENDIF} {$ENDIF};

		 

////////////////////////////////////////////////////////////////////////////////////////////////////////  
//////////////////////         SDL_timer.h          ////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////  

  {**
   *  Get the number of milliseconds since the SDL library initialization.
   *
   *  This value wraps if the program runs for more than ~49 days.
   *}
function SDL_GetTicks(): UInt32;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_GetTicks' {$ENDIF} {$ENDIF};

  {**
   *  Get the current value of the high resolution counter
   *}
function SDL_GetPerformanceCounter(): UInt64;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_GetPerformanceCounter' {$ENDIF} {$ENDIF};

  {**
   *  Get the count per second of the high resolution counter
   *}
function SDL_GetPerformanceFrequency(): UInt64;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_GetPerformanceFrequency' {$ENDIF} {$ENDIF};

  {**
   *  Wait a specified number of milliseconds before returning.
   *}
procedure SDL_Delay(ms: UInt32);
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_Delay' {$ENDIF} {$ENDIF};

  {**
   *  Function prototype for the timer callback function.
   *
   *  The callback function is passed the current timer interval and returns
   *  the next timer interval.  If the returned value is the same as the one
   *  passed in, the periodic alarm continues, otherwise a new alarm is
   *  scheduled.  If the callback returns 0, the periodic alarm is cancelled.
   *}

type
  TSDL_TimerCallback = function(interval: UInt32; param: Pointer): UInt32;

  {**
   * Definition of the timer ID type.
   *}
  TSDL_TimerID = SInt32;

  {**
   *  Add a new timer to the pool of timers already running.
   *
   *  A timer ID, or NULL when an error occurs.
   *}
function SDL_AddTimer(interval: UInt32; callback: TSDL_TimerCallback; param: Pointer): TSDL_TimerID;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_AddTimer' {$ENDIF} {$ENDIF};

  {**
   *  Remove a timer knowing its ID.
   *
   *  A boolean value indicating success or failure.
   *
   *  It is not safe to remove a timer multiple times.
   *}
function SDL_RemoveTimer(id: TSDL_TimerID): Boolean;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_RemoveTimer' {$ENDIF} {$ENDIF};

   

////////////////////////////////////////////////////////////////////////////////////////////////////////
//////////////////////          SDL_pixels.h         ///////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////

  {**
   *  Transparency definitions
   *
   *  These define alpha as the opacity of a surface.
   *}

  const
    SDL_ALPHA_OPAQUE = 255;
    SDL_ALPHA_TRANSPARENT = 0;

    {** Pixel type. *}
    SDL_PIXELTYPE_UNKNOWN = 0;
    SDL_PIXELTYPE_INDEX1 = 1;
    SDL_PIXELTYPE_INDEX4 = 2;
    SDL_PIXELTYPE_INDEX8 = 3;
    SDL_PIXELTYPE_PACKED8 = 4;
    SDL_PIXELTYPE_PACKED16 = 5;
    SDL_PIXELTYPE_PACKED32 = 6;
    SDL_PIXELTYPE_ARRAYU8 = 7;
    SDL_PIXELTYPE_ARRAYU16 = 8;
    SDL_PIXELTYPE_ARRAYU32 = 9;
    SDL_PIXELTYPE_ARRAYF16 = 10;
    SDL_PIXELTYPE_ARRAYF32 = 11;

    {** Bitmap pixel order, high bit -> low bit. *}
    SDL_BITMAPORDER_NONE = 0;
    SDL_BITMAPORDER_4321 = 1;
    SDL_BITMAPORDER_1234 = 2;

    {** Packed component order, high bit -> low bit. *}

    SDL_PACKEDORDER_NONE = 0;
    SDL_PACKEDORDER_XRGB = 1;
    SDL_PACKEDORDER_RGBX = 2;
    SDL_PACKEDORDER_ARGB = 3;
    SDL_PACKEDORDER_RGBA = 4;
    SDL_PACKEDORDER_XBGR = 5;
    SDL_PACKEDORDER_BGRX = 6;
    SDL_PACKEDORDER_ABGR = 7;
    SDL_PACKEDORDER_BGRA = 8;

    {** Array component order, low byte -> high byte. *}
    SDL_ARRAYORDER_NONE = 0;
    SDL_ARRAYORDER_RGB = 1;
    SDL_ARRAYORDER_RGBA = 2;
    SDL_ARRAYORDER_ARGB = 3;
    SDL_ARRAYORDER_BGR = 4;
    SDL_ARRAYORDER_BGRA = 5;
    SDL_ARRAYORDER_ABGR = 6;

    {** Packed component layout. *}
    SDL_PACKEDLAYOUT_NONE = 0;
    SDL_PACKEDLAYOUT_332 = 1;
    SDL_PACKEDLAYOUT_4444 = 2;
    SDL_PACKEDLAYOUT_1555 = 3;
    SDL_PACKEDLAYOUT_5551 = 4;
    SDL_PACKEDLAYOUT_565 = 5;
    SDL_PACKEDLAYOUT_8888 = 6;
    SDL_PACKEDLAYOUT_2101010 = 7;
    SDL_PACKEDLAYOUT_1010102 = 8;

    {
        //todo!!
function SDL_DEFINE_PIXELFORMAT(type, order, layour, bit, bytes: UInt32): Result;

function SDL_DEFINE_PIXELFOURCC(A,B,C,D: Variant): Variant;

#define SDL_DEFINE_PIXELFORMAT(type, order, layout, bits, bytes) \
    ((1 << 28) | ((type) << 24) | ((order) << 20) | ((layout) << 16) | \
     ((bits) << 8) | ((bytes) << 0))
       }

function SDL_PIXELFLAG(X: Cardinal): Boolean;
function SDL_PIXELTYPE(X: Cardinal): Boolean;
function SDL_PIXELORDER(X: Cardinal): Boolean;
function SDL_PIXELLAYOUT(X: Cardinal): Boolean;
function SDL_BITSPERPIXEL(X: Cardinal): Boolean;

function SDL_BYTESPERPIXEL(X: Integer): Integer;
     {
#define SDL_BYTESPERPIXEL(X) \
    (SDL_ISPIXELFORMAT_FOURCC(X) ? \
        ((((X) == SDL_PIXELFORMAT_YUY2) || \
          ((X) == SDL_PIXELFORMAT_UYVY) || \
          ((X) == SDL_PIXELFORMAT_YVYU)) ? 2 : 1) : (((X) >> 0) & 0xFF))

#define SDL_ISPIXELFORMAT_INDEXED(format)   \
    (!SDL_ISPIXELFORMAT_FOURCC(format) && \
     ((SDL_PIXELTYPE(format) == SDL_PIXELTYPE_INDEX1) || \
      (SDL_PIXELTYPE(format) == SDL_PIXELTYPE_INDEX4) || \
      (SDL_PIXELTYPE(format) == SDL_PIXELTYPE_INDEX8)))

#define SDL_ISPIXELFORMAT_ALPHA(format)   \
    (!SDL_ISPIXELFORMAT_FOURCC(format) && \
     ((SDL_PIXELORDER(format) == SDL_PACKEDORDER_ARGB) || \
      (SDL_PIXELORDER(format) == SDL_PACKEDORDER_RGBA) || \
      (SDL_PIXELORDER(format) == SDL_PACKEDORDER_ABGR) || \
      (SDL_PIXELORDER(format) == SDL_PACKEDORDER_BGRA)))

  function SDL_IsPixelFormat_FOURCC(format: Variant);

  {* Note: If you modify this list, update SDL_GetPixelFormatName() *}

const
    SDL_PIXELFORMAT_UNKNOWN = 0;
    SDL_PIXELFORMAT_INDEX1LSB = (1 shl 28)                    or
                                (SDL_PIXELTYPE_INDEX1 shl 24) or
                                (SDL_BITMAPORDER_4321 shl 20) or
                                (0 shl 16)                    or
                                (1 shl 8)                     or
                                (0 shl 0);

    SDL_PIXELFORMAT_INDEX1MSB = (1 shl 28)                    or
                                (SDL_PIXELTYPE_INDEX1 shl 24) or
                                (SDL_BITMAPORDER_1234 shl 20) or
                                (0 shl 16)                    or
                                (1 shl 8)                     or
                                (0 shl 0);

    SDL_PIXELFORMAT_INDEX4LSB = (1 shl 28)                    or
                                (SDL_PIXELTYPE_INDEX4 shl 24) or
                                (SDL_BITMAPORDER_4321 shl 20) or
                                (0 shl 16)                    or
                                (4 shl 8)                     or
                                (0 shl 0);

    SDL_PIXELFORMAT_INDEX4MSB = (1 shl 28)                    or
                                (SDL_PIXELTYPE_INDEX4 shl 24) or
                                (SDL_BITMAPORDER_1234 shl 20) or
                                (0 shl 16)                    or
                                (4 shl 8)                     or
                                (0 shl 0);

    SDL_PIXELFORMAT_INDEX8 =    (1 shl 28)                      or
                                (SDL_PIXELTYPE_PACKED8 shl 24)  or
                                (0 shl 20)                      or
                                (0 shl 16)                      or
                                (8 shl 8)                       or
                                (1 shl 0);
                                
    SDL_PIXELFORMAT_RGB332 =    (1 shl 28)                      or
                                (SDL_PIXELTYPE_PACKED8 shl 24)  or
                                (SDL_PACKEDORDER_XRGB shl 20)   or
                                (SDL_PACKEDLAYOUT_332 shl 16)   or
                                (8 shl 8)                       or
                                (1 shl 0);

    SDL_PIXELFORMAT_RGB444 =    (1 shl 28)                      or
                                (SDL_PIXELTYPE_PACKED16 shl 24) or
                                (SDL_PACKEDORDER_XRGB shl 20)   or
                                (SDL_PACKEDLAYOUT_4444 shl 16)  or
                                (12 shl 8)                      or
                                (2 shl 0);

    SDL_PIXELFORMAT_RGB555 =    (1 shl 28)                      or
                                (SDL_PIXELTYPE_PACKED16 shl 24) or
                                (SDL_PACKEDORDER_XRGB shl 20)   or
                                (SDL_PACKEDLAYOUT_1555 shl 16)  or
                                (15 shl 8)                      or
                                (2 shl 0);

    SDL_PIXELFORMAT_BGR555 =    (1 shl 28)                      or
                                (SDL_PIXELTYPE_PACKED16 shl 24) or
                                (SDL_PACKEDORDER_XBGR shl 20)   or
                                (SDL_PACKEDLAYOUT_1555 shl 16)  or
                                (15 shl 8)                      or
                                (2 shl 0);

    SDL_PIXELFORMAT_ARGB4444 =  (1 shl 28)                      or
                                (SDL_PIXELTYPE_PACKED16 shl 24) or
                                (SDL_PACKEDORDER_ARGB shl 20)   or
                                (SDL_PACKEDLAYOUT_4444 shl 16)  or
                                (16 shl 8)                      or
                                (2 shl 0);

    SDL_PIXELFORMAT_RGBA4444 =  (1 shl 28)                      or
                                (SDL_PIXELTYPE_PACKED16 shl 24) or
                                (SDL_PACKEDORDER_RGBA shl 20)   or
                                (SDL_PACKEDLAYOUT_4444 shl 16)  or
                                (16 shl 8)                      or
                                (2 shl 0);

    SDL_PIXELFORMAT_ABGR4444 =  (1 shl 28)                      or
                                (SDL_PIXELTYPE_PACKED16 shl 24) or
                                (SDL_PACKEDORDER_ABGR shl 20)   or
                                (SDL_PACKEDLAYOUT_4444 shl 16)  or
                                (16 shl 8)                      or
                                (2 shl 0);

    SDL_PIXELFORMAT_BGRA4444 =  (1 shl 28)                      or
                                (SDL_PIXELTYPE_PACKED16 shl 24) or
                                (SDL_PACKEDORDER_BGRA shl 20)   or
                                (SDL_PACKEDLAYOUT_4444 shl 16)  or
                                (16 shl 8)                      or
                                (2 shl 0);

    SDL_PIXELFORMAT_ARGB1555 =  (1 shl 28)                      or
                                (SDL_PIXELTYPE_PACKED16 shl 24) or
                                (SDL_PACKEDORDER_ARGB shl 20)   or
                                (SDL_PACKEDLAYOUT_1555 shl 16)  or
                                (16 shl 8)                      or
                                (2 shl 0);

    SDL_PIXELFORMAT_RGBA5551 =  (1 shl 28)                      or
                                (SDL_PIXELTYPE_PACKED16 shl 24) or
                                (SDL_PACKEDORDER_RGBA shl 20)   or
                                (SDL_PACKEDLAYOUT_5551 shl 16)  or
                                (16 shl 8)                      or
                                (2 shl 0);

    SDL_PIXELFORMAT_ABGR1555 =  (1 shl 28)                      or
                                (SDL_PIXELTYPE_PACKED16 shl 24) or
                                (SDL_PACKEDORDER_ABGR shl 20)   or
                                (SDL_PACKEDLAYOUT_1555 shl 16)  or
                                (16 shl 8)                      or
                                (2 shl 0);

    SDL_PIXELFORMAT_BGRA5551 =  (1 shl 28)                      or
                                (SDL_PIXELTYPE_PACKED16 shl 24) or
                                (SDL_PACKEDORDER_BGRA shl 20)   or
                                (SDL_PACKEDLAYOUT_5551 shl 16)  or
                                (16 shl 8)                      or
                                (2 shl 0);

    SDL_PIXELFORMAT_RGB565 =    (1 shl 28)                      or
                                (SDL_PIXELTYPE_PACKED16 shl 24) or
                                (SDL_PACKEDORDER_XRGB shl 20)   or
                                (SDL_PACKEDLAYOUT_565 shl 16)   or
                                (16 shl 8)                      or
                                (2 shl 0);

    SDL_PIXELFORMAT_BGR565 =    (1 shl 28)                      or
                                (SDL_PIXELTYPE_PACKED16 shl 24) or
                                (SDL_PACKEDORDER_XBGR shl 20)   or
                                (SDL_PACKEDLAYOUT_1555 shl 16)  or
                                (16 shl 8)                      or
                                (2 shl 0);

    SDL_PIXELFORMAT_RGB24 =     (1 shl 28)                      or
                                (SDL_PIXELTYPE_ARRAYU8 shl 24)  or
                                (SDL_ARRAYORDER_RGB shl 20)     or
                                (0 shl 16)                      or
                                (24 shl 8)                      or
                                (3 shl 0);

    SDL_PIXELFORMAT_BGR24 =     (1 shl 28)                      or
                                (SDL_PIXELTYPE_ARRAYU8 shl 24)  or
                                (SDL_ARRAYORDER_BGR shl 20)     or
                                (0 shl 16)                      or
                                (24 shl 8)                      or
                                (3 shl 0);

    SDL_PIXELFORMAT_RGB888 =    (1 shl 28)                      or
                                (SDL_PIXELTYPE_PACKED32 shl 24) or
                                (SDL_PACKEDORDER_XRGB shl 20)   or
                                (SDL_PACKEDLAYOUT_8888 shl 16)  or
                                (24 shl 8)                      or
                                (4 shl 0);

    SDL_PIXELFORMAT_RGBX8888 =  (1 shl 28)                      or
                                (SDL_PIXELTYPE_PACKED32 shl 24) or
                                (SDL_PACKEDORDER_RGBX shl 20)   or
                                (SDL_PACKEDLAYOUT_8888 shl 16)  or
                                (24 shl 8)                      or
                                (4 shl 0);

    SDL_PIXELFORMAT_BGR888 =    (1 shl 28)                      or
                                (SDL_PIXELTYPE_PACKED32 shl 24) or
                                (SDL_PACKEDORDER_XBGR shl 20)   or
                                (SDL_PACKEDLAYOUT_8888 shl 16)  or
                                (24 shl 8)                      or
                                (4 shl 0);

    SDL_PIXELFORMAT_BGRX8888 =  (1 shl 28)                      or
                                (SDL_PIXELTYPE_PACKED32 shl 24) or
                                (SDL_PACKEDORDER_BGRX shl 20)   or
                                (SDL_PACKEDLAYOUT_8888 shl 16)  or
                                (24 shl 8)                      or
                                (4 shl 0);

    SDL_PIXELFORMAT_ARGB8888 =  (1 shl 28)                      or
                                (SDL_PIXELTYPE_PACKED32 shl 24) or
                                (SDL_PACKEDORDER_ARGB shl 20)   or
                                (SDL_PACKEDLAYOUT_8888 shl 16)  or
                                (32 shl 8)                      or
                                (4 shl 0);

    SDL_PIXELFORMAT_RGBA8888 =  (1 shl 28)                      or
                                (SDL_PIXELTYPE_PACKED32 shl 24) or
                                (SDL_PACKEDORDER_RGBA shl 20)   or
                                (SDL_PACKEDLAYOUT_8888 shl 16)  or
                                (32 shl 8)                      or
                                (4 shl 0);

    SDL_PIXELFORMAT_ABGR8888 =  (1 shl 28)                      or
                                (SDL_PIXELTYPE_PACKED32 shl 24) or
                                (SDL_PACKEDORDER_ABGR shl 20)   or
                                (SDL_PACKEDLAYOUT_8888 shl 16)  or
                                (32 shl 8)                      or
                                (4 shl 0);

    SDL_PIXELFORMAT_BGRA8888 =  (1 shl 28)                      or
                                (SDL_PIXELTYPE_PACKED32 shl 24) or
                                (SDL_PACKEDORDER_RGBX shl 20)   or
                                (SDL_PACKEDLAYOUT_8888 shl 16)  or
                                (32 shl 8)                      or
                                (4 shl 0);

    SDL_PIXELFORMAT_ARGB2101010 = (1 shl 28)                       or
                                  (SDL_PIXELTYPE_PACKED32 shl 24)  or
                                  (SDL_PACKEDORDER_ARGB shl 20)    or
                                  (SDL_PACKEDLAYOUT_2101010 shl 16)or
                                  (32 shl 8)                       or
                                  (4 shl 0);

    {**< Planar mode: Y + V + U  (3 planes) *}
    SDL_PIXELFORMAT_YV12 = (Integer('Y')       ) or
                           (Integer('V') shl  8) or
                           (Integer('1') shl 16) or
                           (Integer('2') shl 24);
    {**< Planar mode: Y + U + V  (3 planes) *}
    SDL_PIXELFORMAT_IYUV = (Integer('I')       ) or
                           (Integer('Y') shl  8) or
                           (Integer('U') shl 16) or
                           (Integer('V') shl 24);
    {**< Packed mode: Y0+U0+Y1+V0 (1 plane) *}
    SDL_PIXELFORMAT_YUY2 = (Integer('Y')       ) or
                           (Integer('U') shl  8) or
                           (Integer('Y') shl 16) or
                           (Integer('2') shl 24);
    {**< Packed mode: U0+Y0+V0+Y1 (1 plane) *}
    SDL_PIXELFORMAT_UYVY = (Integer('U')       ) or
                           (Integer('Y') shl  8) or
                           (Integer('V') shl 16) or
                           (Integer('Y') shl 24);
    {**< Packed mode: Y0+V0+Y1+U0 (1 plane) *}
    SDL_PIXELFORMAT_YVYU = (Integer('Y')       ) or
                           (Integer('V') shl  8) or
                           (Integer('Y') shl 16) or
                           (Integer('U') shl 24);

type
  PSDL_Color = ^TSDL_Color;
  TSDL_Color = record
    r: UInt8;
    g: UInt8;
    b: UInt8;
    unused: UInt8;
  end;

  TSDL_Colour = TSDL_Color;
  PSDL_Colour = ^TSDL_Colour;

  PSDL_Palette = ^TSDL_Palette;
  TSDL_Palette = record
    ncolors: SInt32;
    colors: PSDL_Color;
    version: UInt32;
    refcount: SInt32;
  end;

  {**
   *  Everything in the pixel format structure is read-only.
   *}

  PSDL_PixelFormat = ^TSDL_PixelFormat;
  TSDL_PixelFormat = record
    format: UInt32;
    palette: PSDL_Palette;
    BitsPerPixel: UInt8;
    BytesPerPixel: UInt8;
    padding: array[0..1] of UInt8;
    Rmask: UInt32;
    Gmask: UInt32;
    Bmask: UInt32;
    Amask: UInt32;
    Rloss: UInt8;
    Gloss: UInt8;
    Bloss: UInt8;
    Aloss: UInt8;
    Rshift: UInt8;
    Gshift: UInt8;
    Bshift: UInt8;
    Ashift: UInt8;
    refcount: SInt32;
    next: PSDL_PixelFormat;
  end;

  {**
   *  Get the human readable name of a pixel format
   *}
function SDL_GetPixelFormatName(format: UInt32): PChar;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_GetPixelFormatName' {$ENDIF} {$ENDIF};

  {**
   *  Convert one of the enumerated pixel formats to a bpp and RGBA masks.
   *
   *  SDL_TRUE, or SDL_FALSE if the conversion wasn't possible.
   *
   *  SDL_MasksToPixelFormatEnum()
   *}
function SDL_PixelFormatEnumToMasks(format: UInt32; bpp: PInt; Rmask: PUInt32; Gmask: PUInt32; Bmask: PUInt32; Amask: PUInt32): TSDL_Bool;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_PixelFormatEnumToMasks' {$ENDIF} {$ENDIF};

  {**
   *  Convert a bpp and RGBA masks to an enumerated pixel format.
   *
   *  The pixel format, or SDL_PIXELFORMAT_UNKNOWN if the conversion
   *  wasn't possible.
   *
   *  SDL_PixelFormatEnumToMasks()
   *}
function SDL_MasksToPixelFormatEnum(bpp: SInt32; Rmask: UInt32; Gmask: UInt32; Bmask: UInt32; Amask: UInt32): UInt32;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_MasksToPixelFormatEnum' {$ENDIF} {$ENDIF};

  {**
   *  Create an SDL_PixelFormat structure from a pixel format enum.
   *}
function SDL_AllocFormat(pixel_format: UInt32): PSDL_PixelFormat;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_AllocFormat' {$ENDIF} {$ENDIF};

  {**
   *  Free an SDL_PixelFormat structure.
   *}
procedure SDL_FreeFormat(format: PSDL_PixelFormat);
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_FreeFormat' {$ENDIF} {$ENDIF};

  {**
   *  Create a palette structure with the specified number of color
   *  entries.
   *
   *  A new palette, or nil if there wasn't enough memory.
   *
   *  The palette entries are initialized to white.
   *  
   *  SDL_FreePalette()
   *}
function SDL_AllocPalette(ncolors: SInt32): PSDL_Palette;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_AllocPalette' {$ENDIF} {$ENDIF};

  {**
   *  Set the palette for a pixel format structure.
   *}
function SDL_SetPixelFormatPalette(format: PSDL_PixelFormat; palette: PSDL_Palette): SInt32;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_SetPixelFormatPalette' {$ENDIF} {$ENDIF};

  {**
   *  Set a range of colors in a palette.
   *
   *  palette    The palette to modify.
   *  colors     An array of colors to copy into the palette.
   *  firstcolor The index of the first palette entry to modify.
   *  ncolors    The number of entries to modify.
   *
   *  0 on success, or -1 if not all of the colors could be set.
   *}
function SDL_SetPaletteColors(palette: PSDL_Palette; const colors: PSDL_Color; firstcolor: SInt32; ncolors: SInt32): SInt32;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_SetPaletteColors' {$ENDIF} {$ENDIF};

  {**
   *  Free a palette created with SDL_AllocPalette().
   *
   *  SDL_AllocPalette()
   *}
procedure SDL_FreePalette(palette: PSDL_Palette);
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_FreePalette' {$ENDIF} {$ENDIF};

  {**
   *  Maps an RGB triple to an opaque pixel value for a given pixel format.
   *
   *  SDL_MapRGBA
   *}
function SDL_MapRGB(const format: PSDL_PixelFormat; r: UInt8; g: UInt8; b: UInt8): UInt32;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_MapRGB' {$ENDIF} {$ENDIF};

  {**
   *  Maps an RGBA quadruple to a pixel value for a given pixel format.
   *
   *  SDL_MapRGB
   *}
function SDL_MapRGBA(const format: PSDL_PixelFormat; r: UInt8; g: UInt8; b: UInt8; a: UInt8): UInt32;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_MapRGBA' {$ENDIF} {$ENDIF};

  {**
   *  Get the RGB components from a pixel of the specified format.
   *
   *  SDL_GetRGBA
   *}
procedure SDL_GetRGB(pixel: UInt32; const format: PSDL_PixelFormat; r: PUInt8; g: PUInt8; b: PUInt8);
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_GetRGB' {$ENDIF} {$ENDIF};

  {**
   *  Get the RGBA components from a pixel of the specified format.
   *
   *  SDL_GetRGB
   *}
procedure SDL_GetRGBA(pixel: UInt32; const format: PSDL_PixelFormat; r: PUInt8; g: PUInt8; b: PUInt8; a: PUInt8);
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_GetRGBA' {$ENDIF} {$ENDIF};

  {**
   *  Calculate a 256 entry gamma ramp for a gamma value.
   *}
procedure SDL_CalculateGammaRamp(gamma: Float; ramp: PUInt16);
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_CalculateGammaRamp' {$ENDIF} {$ENDIF};



////////////////////////////////////////////////////////////////////////////////////////////////////////
//////////////////////          SDL_rect.h          ////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////

type
  {**
   *  The structure that defines a point
   *
   *  SDL_EnclosePoints
   *}
  PSDL_Point = ^TSDL_Point;
  TSDL_Point = record
    x: SInt32;
    y: SInt32;
  end;

  {**
   *  A rectangle, with the origin at the upper left.
   *
   *  SDL_RectEmpty
   *  SDL_RectEquals
   *  SDL_HasIntersection
   *  SDL_IntersectRect
   *  SDL_UnionRect
   *  SDL_EnclosePoints
   *}
  PSDL_Rect = ^TSDL_Rect;
  TSDL_Rect = record
    x,y: SInt32;
    w,h: SInt32;
  end;

  {**
   *  Returns true if the rectangle has no area.
   *}
  //changed from variant(b?????h!) to TSDL_Rect
  //maybe PSDL_Rect?
function SDL_RectEmpty(X: TSDL_Rect): Boolean;

    {**
     *  Returns true if the two rectangles are equal.
     *}
function SDL_RectEquals(A: TSDL_Rect; B: TSDL_Rect): Boolean;

  {**
   *  Determine whether two rectangles intersect.
   *
   *  SDL_TRUE if there is an intersection, SDL_FALSE otherwise.
   *}
function SDL_HasIntersection(const A: PSDL_Rect; const B: PSDL_Rect): TSDL_Bool;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_HasIntersection' {$ENDIF} {$ENDIF};

  {**
   *  Calculate the intersection of two rectangles.
   *
   *  SDL_TRUE if there is an intersection, SDL_FALSE otherwise.
   *}
function SDL_IntersectRect(const A: PSDL_Rect; const B: PSDL_Rect; result: PSDL_Rect): TSDL_Bool;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_IntersectRect' {$ENDIF} {$ENDIF};

  {**
   *  Calculate the union of two rectangles.
   *}
procedure SDL_UnionRect(const A: PSDL_Rect; const B: PSDL_Rect; result: PSDL_Rect);
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_UnionRect' {$ENDIF} {$ENDIF};

  {**
   *  Calculate a minimal rectangle enclosing a set of points
   *
   *  SDL_TRUE if any points were within the clipping rect
   *}
function SDL_EnclosePoints(const points: PSDL_Point; count: SInt32; const clip: PSDL_Rect; result: PSDL_Rect): TSDL_Bool;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_EnclosePoints' {$ENDIF} {$ENDIF};

  {**
   *  Calculate the intersection of a rectangle and line segment.
   *
   *  SDL_TRUE if there is an intersection, SDL_FALSE otherwise.
   *}
function SDL_IntersectRectAndLine(const rect: PSDL_Rect; X1: PInt; Y1: PInt; X2: PInt; Y2: PInt): TSDL_Bool;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_IntersectRectAndLine' {$ENDIF} {$ENDIF};



////////////////////////////////////////////////////////////////////////////////////////////////////////
//////////////////////        SDL_blendmode.h       ////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////  

{**
 *  The blend mode used in SDL_RenderCopy() and drawing operations.
 *}
 
type
  PSDL_BlendMode = ^TSDL_BlendMode;
  TSDL_BlendMode = DWord;
 
const
  SDL_BLENDMODE_NONE  = $00000000;    {**< No blending *}
  SDL_BLENDMODE_BLEND = $00000001;    {**< dst = (src * A) + (dst * (1-A)) *}
  SDL_BLENDMODE_ADD   = $00000002;    {**< dst = (src * A) + dst *}
  SDL_BLENDMODE_MOD   = $00000004;    {**< dst = src * dst *}



////////////////////////////////////////////////////////////////////////////////////////////////////////  
//////////////////////        SDL_surface.h         ////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////  

const
  {**
   *  Surface flags
   *
   *  These are the currently supported flags for the ::SDL_surface.
   *
   *  Used internally (read-only).
   *}

  SDL_SWSURFACE = 0;          {**< Just here for compatibility *}
  SDL_PREALLOC  = $00000001;  {**< Surface uses preallocated memory *}
  SDL_RLEACCEL  = $00000002;  {**< Surface is RLE encoded *}
  SDL_DONTFREE  = $00000004;  {**< Surface is referenced internally *}

  {*Surface flags*}

  {**
   *  Evaluates to true if the surface needs to be locked before access.
   *}

  //SDL_MUSTLOCK(S)	(((S)->flags & SDL_RLEACCEL) != 0)

type
  {**
   *  A collection of pixels used in software blitting.
   *
   *  This structure should be treated as read-only, except for \c pixels,
   *  which, if not NULL, contains the raw pixel data for the surface.
   *}

  PSDL_BlitMap = ^TSDL_BlitMap;
  TSDL_BlitMap = record
    map: Pointer;
  end;

  PSDL_Surface = ^TSDL_Surface;
  TSDL_Surface = record
    flags: UInt32;              {**< Read-only *}
    format: PSDL_PixelFormat;   {**< Read-only *}
    w, h: SInt32;               {**< Read-only *}
    pitch: SInt32;              {**< Read-only *}
    pixels: Pointer;            {**< Read-write *}

    {** Application data associated with the surface *}
    userdata: Pointer;          {**< Read-write *}

    {** information needed for surfaces requiring locks *}
    locked: SInt32;             {**< Read-only *}
    lock_data: Pointer;         {**< Read-only *}

    {** clipping information *}
    clip_rect: PSDL_Rect;       {**< Read-only *}

    {** info for fast blit mapping to other surfaces *}
    map: Pointer;               {**< Private *} //SDL_BlitMap

    {** Reference count -- used when freeing surface *}
    refcount: SInt32;           {**< Read-mostly *}
  end;

  {**
   *  The type of function used for surface blitting functions.
   *}

   TSDL_Blit = function(src: PSDL_Surface; srcrect: PSDL_Rect; dst: PSDL_Surface; dstrect: PSDL_Rect): SInt32;

  {**
   *  Allocate and free an RGB surface.
   *
   *  If the depth is 4 or 8 bits, an empty palette is allocated for the surface.
   *  If the depth is greater than 8 bits, the pixel format is set using the
   *  flags '[RGB]mask'.
   *
   *  If the function runs out of memory, it will return NULL.
   *
   *  flags The flags are obsolete and should be set to 0.
   *}
function SDL_CreateRGBSurface(flags: UInt32; width: SInt32; height: SInt32; depth: SInt32; Rmask: UInt32; Gmask: UInt32; Bmask: UInt32; Amask: UInt32): PSDL_Surface;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_CreateRGBSurface' {$ENDIF} {$ENDIF};

function SDL_CreateRGBSurfaceFrom(pixels: Pointer; width: SInt32; height: SInt32; depth: SInt32; pitch: SInt32; Rmask: UInt32; Gmask: UInt32; Bmask: UInt32; Amask: UInt32): PSDL_Surface;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_CreateRGBSurfaceFrom' {$ENDIF} {$ENDIF};

procedure SDL_FreeSurface(surface: PSDL_Surface);
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_FreeSurface' {$ENDIF} {$ENDIF};

  {**
   *  Set the palette used by a surface.
   *
   *  0, or -1 if the surface format doesn't use a palette.
   *
   *  A single palette can be shared with many surfaces.
   *}
function SDL_SetSurfacePalette(surface: PSDL_Surface; palette: PSDL_Palette): SInt32;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_SetSurfacePalette' {$ENDIF} {$ENDIF};

  {**
   *  Sets up a surface for directly accessing the pixels.
   *
   *  Between calls to SDL_LockSurface() / SDL_UnlockSurface(), you can write
   *  to and read from surface.pixels, using the pixel format stored in
   *  surface.format. Once you are done accessing the surface, you should
   *  use SDL_UnlockSurface() to release it.
   *
   *  Not all surfaces require locking.  If SDL_MUSTLOCK(surface) evaluates
   *  to 0, then you can read and write to the surface at any time, and the
   *  pixel format of the surface will not change.
   *
   *  No operating system or library calls should be made between lock/unlock
   *  pairs, as critical system locks may be held during this time.
   *
   *  SDL_LockSurface() returns 0, or -1 if the surface couldn't be locked.
   *
   *  SDL_UnlockSurface()
   *}
function SDL_LockSurface(surface: PSDL_Surface): SInt32;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_LockSurface' {$ENDIF} {$ENDIF};

  {** SDL_LockSurface() *}
procedure SDL_UnlockSurface(surface: PSDL_Surface);
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_UnlockSurface' {$ENDIF} {$ENDIF};

  {**
   *  Load a surface from a seekable SDL data stream (memory or file).
   *
   *  If freesrc is non-zero, the stream will be closed after being read.
   *
   *  The new surface should be freed with SDL_FreeSurface().
   *
   *  the new surface, or NULL if there was an error.
   *}
function SDL_LoadBMP_RW(src: PSDL_RWops; freesrc: SInt32): PSDL_Surface;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_LoadBMP_RW' {$ENDIF} {$ENDIF};

  {**
   *  Load a surface from a file.
   *
   *  Convenience macro.
   *}
function SDL_LoadBMP(_file: PChar): PSDL_Surface;

  {**
   *  Save a surface to a seekable SDL data stream (memory or file).
   *
   *  If freedst is non-zero, the stream will be closed after being written.
   *
   *  0 if successful or -1 if there was an error.
   *}
function SDL_SaveBMP_RW(surface: PSDL_Surface; dst: PSDL_RWops; freedst: SInt32): SInt32;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_LoadBMP_RW' {$ENDIF} {$ENDIF};

    {**
     *  Save a surface to a file.
     *
     *  Convenience macro.
     *}
function SDL_SaveBMP(surface: PSDL_Surface; _file: PChar): SInt32;

  {**
   *  Sets the RLE acceleration hint for a surface.
   *
   *  0 on success, or -1 if the surface is not valid
   *
   *  If RLE is enabled, colorkey and alpha blending blits are much faster,
   *  but the surface must be locked before directly accessing the pixels.
   *}
function SDL_SetSurfaceRLE(surface: PSDL_Surface; flag: SInt32): SInt32;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_SetSurfaceRLE' {$ENDIF} {$ENDIF};

  {**
   *  Sets the color key (transparent pixel) in a blittable surface.
   *
   *  surface The surface to update
   *  flag Non-zero to enable colorkey and 0 to disable colorkey
   *  key The transparent pixel in the native surface format
   *  
   *  0 on success, or -1 if the surface is not valid
   *
   *  You can pass SDL_RLEACCEL to enable RLE accelerated blits.
   *}
function SDL_SetColorKey(surface: PSDL_Surface; flag: SInt32; key: UInt32): SInt32;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_SetColorKey' {$ENDIF} {$ENDIF};

  {**
   *  Gets the color key (transparent pixel) in a blittable surface.
   *  
   *  surface The surface to update
   *  key A pointer filled in with the transparent pixel in the native
   *      surface format
   *  
   *  0 on success, or -1 if the surface is not valid or colorkey is not
   *  enabled.
   *}
function SDL_GetColorKey(surface: PSDL_Surface; key: PUInt32): SInt32;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_GetColorKey' {$ENDIF} {$ENDIF};

  {**
   *  Set an additional color value used in blit operations.
   *
   *  surface The surface to update.
   *  r The red color value multiplied into blit operations.
   *  g The green color value multiplied into blit operations.
   *  b The blue color value multiplied into blit operations.
   *
   *  0 on success, or -1 if the surface is not valid.
   *
   *  SDL_GetSurfaceColorMod()
   *}
function SDL_SetSurfaceColorMod(surface: PSDL_Surface; r: UInt8; g: UInt8; b: UInt8): SInt32;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_SetSurfaceColorMod' {$ENDIF} {$ENDIF};

  {**
   *  Get the additional color value used in blit operations.
   *
   *  surface The surface to query.
   *  r A pointer filled in with the current red color value.
   *  g A pointer filled in with the current green color value.
   *  b A pointer filled in with the current blue color value.
   *
   *  0 on success, or -1 if the surface is not valid.
   *
   *  SDL_SetSurfaceColorMod()
   *}
function SDL_GetSurfaceColorMod(surface: PSDL_Surface; r: PUInt8; g: PUInt8; b: PUInt8): SInt32;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_GetSurfaceColorMod' {$ENDIF} {$ENDIF};

  {**
   *  Set an additional alpha value used in blit operations.
   *
   *  surface The surface to update.
   *  alpha The alpha value multiplied into blit operations.
   *
   *  0 on success, or -1 if the surface is not valid.
   *
   *  SDL_GetSurfaceAlphaMod()
   *}
function SDL_SetSurfaceAlphaMod(surface: PSDL_Surface; alpha: UInt8): SInt32;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_SetSurfaceAlphaMod' {$ENDIF} {$ENDIF};

  {**
   *  Get the additional alpha value used in blit operations.
   *
   *  surface The surface to query.
   *  alpha A pointer filled in with the current alpha value.
   *
   *  0 on success, or -1 if the surface is not valid.
   *
   *  SDL_SetSurfaceAlphaMod()
   *}
function SDL_GetSurfaceAlphaMod(surface: PSDL_Surface; alpha: PUInt8): SInt32;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_GetSurfaceAlphaMod' {$ENDIF} {$ENDIF};

  {**
   *  Set the blend mode used for blit operations.
   *
   *  surface The surface to update.
   *  blendMode ::SDL_BlendMode to use for blit blending.
   *
   *  0 on success, or -1 if the parameters are not valid.
   *
   *  SDL_GetSurfaceBlendMode()
   *}
function SDL_SetSurfaceBlendMode(surface: PSDL_Surface; blendMode: TSDL_BlendMode): SInt32;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_SetSurfaceBlendMode' {$ENDIF} {$ENDIF};

  {**
   *  Get the blend mode used for blit operations.
   *
   *  surface   The surface to query.
   *  blendMode A pointer filled in with the current blend mode.
   *
   *  0 on success, or -1 if the surface is not valid.
   *
   *  SDL_SetSurfaceBlendMode()
   *}
function SDL_GetSurfaceBlendMode(surface: PSDL_Surface; blendMode: PSDL_BlendMode): SInt32;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_GetSurfaceBlendMode' {$ENDIF} {$ENDIF};

  {**
   *  Sets the clipping rectangle for the destination surface in a blit.
   *
   *  If the clip rectangle is NULL, clipping will be disabled.
   *
   *  If the clip rectangle doesn't intersect the surface, the function will
   *  return SDL_FALSE and blits will be completely clipped.  Otherwise the
   *  function returns SDL_TRUE and blits to the surface will be clipped to
   *  the intersection of the surface area and the clipping rectangle.
   *
   *  Note that blits are automatically clipped to the edges of the source
   *  and destination surfaces.
   *}
function SDL_SetClipRect(surface: PSDL_Surface; const rect: PSDL_Rect): TSDL_Bool;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_SetClipRect' {$ENDIF} {$ENDIF};

  {**
   *  Gets the clipping rectangle for the destination surface in a blit.
   *
   *  rect must be a pointer to a valid rectangle which will be filled
   *  with the correct values.
   *}
procedure SDL_GetClipRect(surface: PSDL_Surface; rect: PSDL_Rect);
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_GetClipRect' {$ENDIF} {$ENDIF};

  {**
   *  Creates a new surface of the specified format, and then copies and maps
   *  the given surface to it so the blit of the converted surface will be as
   *  fast as possible.  If this function fails, it returns NULL.
   *
   *  The flags parameter is passed to SDL_CreateRGBSurface() and has those
   *  semantics.  You can also pass SDL_RLEACCEL in the flags parameter and
   *  SDL will try to RLE accelerate colorkey and alpha blits in the resulting
   *  surface.
   *}
function SDL_ConvertSurface(src: PSDL_Surface; fmt: PSDL_PixelFormat; flags: UInt32): PSDL_Surface;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_ConvertSurface' {$ENDIF} {$ENDIF};

function SDL_ConvertSurfaceFormat(src: PSDL_Surface; pixel_format: UInt32; flags: UInt32): PSDL_Surface;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_ConvertSurfaceFormat' {$ENDIF} {$ENDIF};

  {**
   *  Copy a block of pixels of one format to another format
   *
   *  0 on success, or -1 if there was an error
   *}
function SDL_ConvertPixels(width: SInt32; height: SInt32; src_format: UInt32; const src: Pointer; src_pitch: SInt32; dst_format: UInt32; dst: Pointer; dst_pitch: SInt32): SInt32;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_ConvertPixels' {$ENDIF} {$ENDIF};

  {**
   *  Performs a fast fill of the given rectangle with color.
   *
   *  If rect is NULL, the whole surface will be filled with color.
   *
   *  The color should be a pixel of the format used by the surface, and 
   *  can be generated by the SDL_MapRGB() function.
   *  
   *  0 on success, or -1 on error.
   *}
function SDL_FillRect(dst: PSDL_Surface; const rect: PSDL_Rect; color: UInt32): SInt32;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_FillRect' {$ENDIF} {$ENDIF};

function SDL_FillRects(dst: PSDL_Surface; const rects: PSDL_Rect; count: SInt32; color: UInt32): SInt32;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_FillRects' {$ENDIF} {$ENDIF};

  {**
   *  Performs a fast blit from the source surface to the destination surface.
   *
   *  This assumes that the source and destination rectangles are
   *  the same size.  If either \c srcrect or \c dstrect are NULL, the entire
   *  surface ( src or  dst) is copied.  The final blit rectangles are saved
   *  in srcrect and dstrect after all clipping is performed.
   *
   *  If the blit is successful, it returns 0, otherwise it returns -1.
   *
   *  The blit function should not be called on a locked surface.
   *
   *  The blit semantics for surfaces with and without alpha and colorkey
   *  are defined as follows:
   *
      RGBA->RGB:
        SDL_SRCALPHA set:
          alpha-blend (using alpha-channel).
          SDL_SRCCOLORKEY ignored.
        SDL_SRCALPHA not set:
          copy RGB.
          if SDL_SRCCOLORKEY set, only copy the pixels matching the
          RGB values of the source colour key, ignoring alpha in the
          comparison.
   
      RGB->RGBA:
        SDL_SRCALPHA set:
          alpha-blend (using the source per-surface alpha value);
          set destination alpha to opaque.
        SDL_SRCALPHA not set:
          copy RGB, set destination alpha to source per-surface alpha value.
        both:
          if SDL_SRCCOLORKEY set, only copy the pixels matching the
          source colour key.
   
      RGBA->RGBA:
        SDL_SRCALPHA set:
          alpha-blend (using the source alpha channel) the RGB values;
          leave destination alpha untouched. [Note: is this correct?]
          SDL_SRCCOLORKEY ignored.
        SDL_SRCALPHA not set:
          copy all of RGBA to the destination.
          if SDL_SRCCOLORKEY set, only copy the pixels matching the
          RGB values of the source colour key, ignoring alpha in the
         comparison.

      RGB->RGB:
        SDL_SRCALPHA set:
          alpha-blend (using the source per-surface alpha value).
        SDL_SRCALPHA not set:
          copy RGB.
        both:
          if SDL_SRCCOLORKEY set, only copy the pixels matching the
          source colour key.r
   *
   *  You should call SDL_BlitSurface() unless you know exactly how SDL
   *  blitting works internally and how to use the other blit functions.
   *}

  {**
   *  This is the public blit function, SDL_BlitSurface(), and it performs
   *  rectangle validation and clipping before passing it to SDL_LowerBlit()
   *}
function SDL_UpperBlit(src: PSDL_Surface; const srcrect: PSDL_Rect; dst: PSDL_Surface; dstrect: PSDL_Rect): SInt32;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_UpperBlit' {$ENDIF} {$ENDIF};

  //SDL_BlitSurface = SDL_UpperBlit;

  {**
   *  This is a semi-private blit function and it performs low-level surface
   *  blitting only.
   *}
function SDL_LowerBlit(src: PSDL_Surface; srcrect: PSDL_Rect; dst: PSDL_Surface; dstrect: PSDL_Rect): SInt32;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_LowerBlit' {$ENDIF} {$ENDIF};

  {**
   *  Perform a fast, low quality, stretch blit between two surfaces of the
   *  same pixel format.
   *  
   *  This function uses a static buffer, and is not thread-safe.
   *}
function SDL_SoftStretch(src: PSDL_Surface; const srcrect: PSDL_Rect; dst: PSDL_Surface; const dstrect: PSDL_Surface): SInt32;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_SoftStretch' {$ENDIF} {$ENDIF};

  //SDL_BlitScaled = SDL_UpperBlitScaled;

  {**
   *  This is the public scaled blit function, SDL_BlitScaled(), and it performs
   *  rectangle validation and clipping before passing it to SDL_LowerBlitScaled()
   *}
function SDL_UpperBlitScaled(src: PSDL_Surface; const srcrect: PSDL_Rect; dst: PSDL_Surface; dstrect: PSDL_Rect): SInt32;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_UpperBlitScaled' {$ENDIF} {$ENDIF};

  {**
   *  This is a semi-private blit function and it performs low-level surface
   *  scaled blitting only.
   *}
function SDL_LowerBlitScaled(src: PSDL_Surface; srcrect: PSDL_Rect; dst: PSDL_Surface; dstrect: PSDL_Rect): SInt32;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_LowerBlitScaled' {$ENDIF} {$ENDIF};



////////////////////////////////////////////////////////////////////////////////////////////////////////  
//////////////////////         SDL_shape.h          ////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////  

{**  SDL_shape.h
 *
 * Header file for the shaped window API.
 *}
const
  SDL_NONSHAPEABLE_WINDOW = -1;
  SDL_INVALID_SHAPE_ARGUMENT = -2;
  SDL_WINDOW_LACKS_SHAPE = -3;
  
type
  PPSDL_Window = ^PSDL_Window;
  PSDL_Window = ^TSDL_Window;

  {** An enum denoting the specific type of contents present in an SDL_WindowShapeParams union. *}
  TWindowShapeMode = ({** The default mode, a binarized alpha cutoff of 1. *}
                      ShapeModeDefault,
                      {** A binarized alpha cutoff with a given integer value. *}
                      ShapeModeBinarizeAlpha,
                      {** A binarized alpha cutoff with a given integer value, but with the opposite comparison. *}
                      ShapeModeReverseBinarizeAlpha,
                      {** A color key is applied. *}
                      ShapeModeColorKey);

//#define SDL_SHAPEMODEALPHA(mode) (mode == ShapeModeDefault || mode == ShapeModeBinarizeAlpha || mode == ShapeModeReverseBinarizeAlpha)

  {** A union containing parameters for shaped windows. *}
  TSDL_WindowShapeParams = record
    case Integer of
      {** a cutoff alpha value for binarization of the window shape's alpha channel. *}
      0: (binarizationCutoff: UInt8;);
      1: (colorKey: TSDL_Color;);
  end;

  {** A struct that tags the SDL_WindowShapeParams union with an enum describing the type of its contents. *}
  PSDL_WindowShapeMode = ^TSDL_WindowShapeMode;
  TSDL_WindowShapeMode = record
    {** The mode of these window-shape parameters. *}
    mode: TWindowShapeMode;
    {** Window-shape parameters. *}
    parameters: TSDL_WindowShapeParams;
  end;



////////////////////////////////////////////////////////////////////////////////////////////////////////
//////////////////////        SDL_video.h           ////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////

  {**
   *  The structure that defines a display mode
   *
   *   SDL_GetNumDisplayModes()
   *   SDL_GetDisplayMode()
   *   SDL_GetDesktopDisplayMode()
   *   SDL_GetCurrentDisplayMode()
   *   SDL_GetClosestDisplayMode()
   *   SDL_SetWindowDisplayMode()
   *   SDL_GetWindowDisplayMode()
   *}

  PSDL_DisplayMode = ^TSDL_DisplayMode;
  TSDL_DisplayMode = record
    format: UInt32;              {**< pixel format *}
    w: SInt32;                   {**< width *}
    h: SInt32;                   {**< height *}
    refresh_rate: SInt32;        {**< refresh rate (or zero for unspecified) *}
    driverdata: Pointer;         {**< driver-specific data, initialize to 0 *}
  end;

  {* Define the SDL window-shaper structure *}
  PSDL_WindowShaper = ^TSDL_WindowShaper;
  TSDL_WindowShaper = record
    {* The window associated with the shaper *}
    window: PSDL_Window;
    {* The user's specified coordinates for the window, for once we give it a shape. *}
    userx,usery: UInt32;
    {* The parameters for shape calculation. *}
    mode: TSDL_WindowShapeMode;
    {* Has this window been assigned a shape? *}
    hasshape: TSDL_Bool;
    driverdata: Pointer;
  end;

  PSDL_WindowUserData = ^TSDL_WindowUserData;
  TSDL_WindowUserData = record
    name: PChar;
    data: Pointer;
    next: PSDL_WindowUserData;
  end;

  {* Define the SDL window structure, corresponding to toplevel windows *}
  TSDL_Window = record
    magic: Pointer;
    id: UInt32;
    title: PChar;
    fin_UTF8_title: SInt32;  // esta variable está puesta porque la última versión SDL2.dll devuelve un caracter de más en el title y se desplazan todos los otros campos
    x,y: SInt32;
    w,h: SInt32;
    min_w, min_h: SInt32;
    max_w, max_h: SInt32;
    flags: UInt32;
    {* Stored position and size for windowed mode * }
    windowed: TSDL_Rect;
    fullscreen_mode: TSDL_DisplayMode;
    brightness: Float;
    gamma: PUInt16;
    saved_gamma: PUInt16;  {* (just offset into gamma) *}
    surface: PSDL_Surface;
    surface_valid: TSDL_Bool;
    shaper: PSDL_WindowShaper;
    data: PSDL_WindowUserData;
    driverdata: Pointer;
    prev: PSDL_Window;
    next: PSDL_Window;
  end;

  {**
   * Get the shape parameters of a shaped window.
   *
   *  window The shaped window whose parameters should be retrieved.
   *  shape_mode An empty shape-mode structure to fill, or NULL to check whether the window has a shape.
   *
   *  0 if the window has a shape and, provided shape_mode was not NULL, shape_mode has been filled with the mode
   *  data, SDL_NONSHAPEABLE_WINDOW if the SDL_Window given is not a shaped window, or SDL_WINDOW_LACKS_SHAPE if
   *  the SDL_Window* given is a shapeable window currently lacking a shape.
   *
   *  SDL_WindowShapeMode
   *  SDL_SetWindowShape
   *}
function SDL_GetShapedWindowMode(window: PSDL_Window; shape_mode: TSDL_WindowShapeMode): SInt32;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_GetShapedWindowMode' {$ENDIF} {$ENDIF};

  {**
   * Set the shape and parameters of a shaped window.
   *
   *  window The shaped window whose parameters should be set.
   *  shape A surface encoding the desired shape for the window.
   *  shape_mode The parameters to set for the shaped window.
   *
   *  0 on success, SDL_INVALID_SHAPE_ARGUMENT on invalid an invalid shape argument, or SDL_NONSHAPEABLE_WINDOW
   *  if the SDL_Window* given does not reference a valid shaped window.
   *
   *  SDL_WindowShapeMode
   *  SDL_GetShapedWindowMode.
   *}
function SDL_SetWindowShape(window: PSDL_Window; shape: PSDL_Surface; shape_mode: PSDL_WindowShapeMode): SInt32;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_SetWindowShape' {$ENDIF} {$ENDIF};

  {**
   *  Create a window that can be shaped with the specified position, dimensions, and flags.
   *
   *   title The title of the window, in UTF-8 encoding.
   *   x     The x position of the window, ::SDL_WINDOWPOS_CENTERED, or
   *               ::SDL_WINDOWPOS_UNDEFINED.
   *   y     The y position of the window, ::SDL_WINDOWPOS_CENTERED, or
   *               ::SDL_WINDOWPOS_UNDEFINED.
   *   w     The width of the window.
   *   h     The height of the window.
   *   flags The flags for the window, a mask of SDL_WINDOW_BORDERLESS with any of the following:
   *         SDL_WINDOW_OPENGL,     SDL_WINDOW_INPUT_GRABBED,
   *         SDL_WINDOW_SHOWN,      SDL_WINDOW_RESIZABLE,
   *         SDL_WINDOW_MAXIMIZED,  SDL_WINDOW_MINIMIZED,
   *         SDL_WINDOW_BORDERLESS is always set, and SDL_WINDOW_FULLSCREEN is always unset.
   *
   *   The window created, or NULL if window creation failed.
   *
   *  SDL_DestroyWindow()
   *}
function SDL_CreateShapedWindow(title: PChar; x: UInt32; y: UInt32; w: UInt32; h: UInt32; flags: UInt32): PSDL_Window;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_CreateShapedWindow' {$ENDIF} {$ENDIF};

  {**
   * Return whether the given window is a shaped window.
   *
   *  window The window to query for being shaped.
   *
   *  SDL_TRUE if the window is a window that can be shaped, SDL_FALSE if the window is unshaped or NULL.
   *  SDL_CreateShapedWindow
   *}
function SDL_IsShapedWindow(window: PSDL_Window): TSDL_Bool;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_IsShapedWindow' {$ENDIF} {$ENDIF};

  {**
   *  The type used to identify a window
   *  
   *   SDL_CreateWindow()
   *   SDL_CreateWindowFrom()
   *   SDL_DestroyWindow()
   *   SDL_GetWindowData()
   *   SDL_GetWindowFlags()
   *   SDL_GetWindowGrab()
   *   SDL_GetWindowPosition()
   *   SDL_GetWindowSize()
   *   SDL_GetWindowTitle()
   *   SDL_HideWindow()
   *   SDL_MaximizeWindow()
   *   SDL_MinimizeWindow()
   *   SDL_RaiseWindow()
   *   SDL_RestoreWindow()
   *   SDL_SetWindowData()
   *   SDL_SetWindowFullscreen()
   *   SDL_SetWindowGrab()
   *   SDL_SetWindowIcon()
   *   SDL_SetWindowPosition()
   *   SDL_SetWindowSize()
   *   SDL_SetWindowBordered()
   *   SDL_SetWindowTitle()
   *   SDL_ShowWindow()
   *}

const
  {**
   *  The flags on a window
   *  
   *   SDL_GetWindowFlags()
   *}
  SDL_WINDOW_FULLSCREEN = $00000001;         {**< fullscreen window *}
  SDL_WINDOW_OPENGL = $00000002;             {**< window usable with OpenGL context *}
  SDL_WINDOW_SHOWN = $00000004;              {**< window is visible *}
  SDL_WINDOW_HIDDEN = $00000008;             {**< window is not visible *}
  SDL_WINDOW_BORDERLESS = $00000010;         {**< no window decoration *}
  SDL_WINDOW_RESIZABLE = $00000020;          {**< window can be resized *}
  SDL_WINDOW_MINIMIZED = $00000040;          {**< window is minimized *}
  SDL_WINDOW_MAXIMIZED = $00000080;          {**< window is maximized *}
  SDL_WINDOW_INPUT_GRABBED = $00000100;      {**< window has grabbed input focus *}
  SDL_WINDOW_INPUT_FOCUS = $00000200;        {**< window has input focus *}
  SDL_WINDOW_MOUSE_FOCUS = $00000400;        {**< window has mouse focus *}
  SDL_WINDOW_FULLSCREEN_DESKTOP = SDL_WINDOW_FULLSCREEN or $00001000;
  SDL_WINDOW_FOREIGN = $00000800;            {**< window not created by SDL *}

type
  TSDL_WindowFlags = DWord;

function SDL_WindowPos_IsUndefined(X: Variant): Variant;
function SDL_WindowPos_IsCentered(X: Variant): Variant;

const
   {**
   *  Used to indicate that you don't care what the window position is.
   *}

  SDL_WINDOWPOS_UNDEFINED_MASK = $1FFF0000;
  SDL_WINDOWPOS_UNDEFINED = SDL_WINDOWPOS_UNDEFINED_MASK or 0;


  {**
   *  Used to indicate that the window position should be centered.
   *}

  SDL_WINDOWPOS_CENTERED_MASK = $2FFF0000;
  SDL_WINDOWPOS_CENTERED = SDL_WINDOWPOS_CENTERED_MASK or 0;

  {**
   *  Event subtype for window events
   *}

  SDL_WINDOWEVENT_NONE = 0;           {**< Never used *}
  SDL_WINDOWEVENT_SHOWN = 1;          {**< Window has been shown *}
  SDL_WINDOWEVENT_HIDDEN = 2;         {**< Window has been hidden *}
  SDL_WINDOWEVENT_EXPOSED = 3;        {**< Window has been exposed and should be redrawn *}
  SDL_WINDOWEVENT_MOVED = 4;          {**< Window has been moved to data1; data2 *}
  SDL_WINDOWEVENT_RESIZED = 5;        {**< Window has been resized to data1xdata2 *}
  SDL_WINDOWEVENT_SIZE_CHANGED = 6;   {**< The window size has changed; either as a result of an API call or through the system or user changing the window size. *}
  SDL_WINDOWEVENT_MINIMIZED = 7;      {**< Window has been minimized *}
  SDL_WINDOWEVENT_MAXIMIZED = 8;      {**< Window has been maximized *}
  SDL_WINDOWEVENT_RESTORED = 9;       {**< Window has been restored to normal size and position *}
  SDL_WINDOWEVENT_ENTER = 10;          {**< Window has gained mouse focus *}
  SDL_WINDOWEVENT_LEAVE = 11;          {**< Window has lost mouse focus *}
  SDL_WINDOWEVENT_FOCUS_GAINED = 12;   {**< Window has gained keyboard focus *}
  SDL_WINDOWEVENT_FOCUS_LOST = 13;     {**< Window has lost keyboard focus *}
  SDL_WINDOWEVENT_CLOSE = 14;          {**< The window manager requests that the window be closed *}

type
  TSDL_WindowEventID = DWord;

  {**
   *  An opaque handle to an OpenGL context.
   *}

  TSDL_GLContext = Pointer;

  {**
   *  OpenGL configuration attributes
   *}
   
const
  SDL_GL_RED_SIZE = 0;
  SDL_GL_GREEN_SIZE = 1;
  SDL_GL_BLUE_SIZE = 2;
  SDL_GL_ALPHA_SIZE = 3;
  SDL_GL_BUFFER_SIZE = 4;
  SDL_GL_DOUBLEBUFFER = 5;
  SDL_GL_DEPTH_SIZE = 6;
  SDL_GL_STENCIL_SIZE = 7;
  SDL_GL_ACCUM_RED_SIZE = 8;
  SDL_GL_ACCUM_GREEN_SIZE = 9;
  SDL_GL_ACCUM_BLUE_SIZE = 10;
  SDL_GL_ACCUM_ALPHA_SIZE = 11;
  SDL_GL_STEREO = 12;
  SDL_GL_MULTISAMPLEBUFFERS = 13;
  SDL_GL_MULTISAMPLESAMPLES = 14;
  SDL_GL_ACCELERATED_VISUAL = 15;
  SDL_GL_RETAINED_BACKING = 16;
  SDL_GL_CONTEXT_MAJOR_VERSION = 17;
  SDL_GL_CONTEXT_MINOR_VERSION = 18;
  SDL_GL_CONTEXT_EGL = 19;
  SDL_GL_CONTEXT_FLAGS = 20;
  SDL_GL_CONTEXT_PROFILE_MASK = 21;
  SDL_GL_SHARE_WITH_CURRENT_CONTEXT = 22;

type
  TSDL_GLattr = DWord;

const
  SDL_GL_CONTEXT_PROFILE_CORE           = $0001;
  SDL_GL_CONTEXT_PROFILE_COMPATIBILITY  = $0002;
  SDL_GL_CONTEXT_PROFILE_ES             = $0004;

type
  TSDL_GLprofile = DWord;

const
  SDL_GL_CONTEXT_DEBUG_FLAG              = $0001;
  SDL_GL_CONTEXT_FORWARD_COMPATIBLE_FLAG = $0002;
  SDL_GL_CONTEXT_ROBUST_ACCESS_FLAG      = $0004;
  SDL_GL_CONTEXT_RESET_ISOLATION_FLAG    = $0008;

type
  TSDL_GLcontextFlag = DWord;

  {* Function prototypes *}

  {**
   *  Get the number of video drivers compiled into SDL
   *
   *  SDL_GetVideoDriver()
   *}
function SDL_GetNumVideoDrivers(): SInt32;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_GetNumVideoDrivers' {$ENDIF} {$ENDIF};

  {**
   *  Get the name of a built in video driver.
   *
   *  The video drivers are presented in the order in which they are
   *  normally checked during initialization.
   *
   *  SDL_GetNumVideoDrivers()
   *}
function SDL_GetVideoDriver(index: SInt32): PChar;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_GetVideoDriver' {$ENDIF} {$ENDIF};

  {**
   *  Initialize the video subsystem, optionally specifying a video driver.
   *  
   *  driver_name Initialize a specific driver by name, or nil for the
   *  default video driver.
   *  
   *  0 on success, -1 on error
   *  
   *  This function initializes the video subsystem; setting up a connection
   *  to the window manager, etc, and determines the available display modes
   *  and pixel formats, but does not initialize a window or graphics mode.
   *  
   *  SDL_VideoQuit()
   *}
function SDL_VideoInit(const driver_name: PChar): SInt32;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_VideoInit' {$ENDIF} {$ENDIF};

  {**
   *  Shuts down the video subsystem.
   *  
   *  function closes all windows, and restores the original video mode.
   *  
   *  SDL_VideoInit()
   *}
procedure SDL_VideoQuit();
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_VideoQuit' {$ENDIF} {$ENDIF};

  {**
   *  Returns the name of the currently initialized video driver.
   *
   *  The name of the current video driver or nil if no driver
   *  has been initialized
   *  
   *  SDL_GetNumVideoDrivers()
   *  SDL_GetVideoDriver()
   *}
function SDL_GetCurrentVideoDriver(): PChar;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_GetCurrentVideoDriver' {$ENDIF} {$ENDIF};

  {**
   *  Returns the number of available video displays.
   *  
   *  SDL_GetDisplayBounds()
   *}
function SDL_GetNumVideoDisplays(): SInt32;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_GetNumVideoDisplays' {$ENDIF} {$ENDIF};

  {**
   *  Get the name of a display in UTF-8 encoding
   *
   *  The name of a display, or nil for an invalid display index.
   *  
   *  SDL_GetNumVideoDisplays()
   *}
function SDL_GetDisplayName(displayIndex: SInt32): PChar;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_GetDisplayName' {$ENDIF} {$ENDIF};

  {**
   *  Get the desktop area represented by a display, with the primary
   *  display located at 0,0
   *  
   *  0 on success, or -1 if the index is out of range.
   *  
   *  SDL_GetNumVideoDisplays()
   *}
function SDL_GetDisplayBounds(displayIndex: SInt32; rect: PSDL_Rect): SInt32;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_GetDisplayBounds' {$ENDIF} {$ENDIF};

  {**
   *  Returns the number of available display modes.
   *  
   *  SDL_GetDisplayMode()
   *}
function SDL_GetNumDisplayModes(displayIndex: SInt32): SInt32;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_GetNumDisplayModes' {$ENDIF} {$ENDIF};

  {**
   *  Fill in information about a specific display mode.
   *
   *  The display modes are sorted in this priority:
   *        bits per pixel -> more colors to fewer colors
   *        width -> largest to smallest
   *        height -> largest to smallest
   *        refresh rate -> highest to lowest
   *
   *  SDL_GetNumDisplayModes()
   *}
function SDL_GetDisplayMode(displayIndex: SInt32; modeIndex: SInt32; mode: PSDL_DisplayMode): SInt32;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_GetDisplayMode' {$ENDIF} {$ENDIF};

  {**
   *  Fill in information about the desktop display mode.
   *}
function SDL_GetDesktopDisplayMode(displayIndex: SInt32; mode: PSDL_DisplayMode): SInt32;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_GetDesktopDisplayMode' {$ENDIF} {$ENDIF};

  {**
   *  Fill in information about the current display mode.
   *}
function SDL_GetCurrentDisplayMode(displayIndex: SInt32; mode: PSDL_DisplayMode): SInt32;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_GetCurrentDisplayIndex' {$ENDIF} {$ENDIF};

  {**
   *  Get the closest match to the requested display mode.
   *  
   *  mode The desired display mode
   *  closest A pointer to a display mode to be filled in with the closest
   *  match of the available display modes.
   *  
   *  The passed in value closest, or nil if no matching video mode
   *  was available.
   *  
   *  The available display modes are scanned, and closest is filled in with the
   *  closest mode matching the requested mode and returned.  The mode format and 
   *  refresh_rate default to the desktop mode if they are 0.  The modes are 
   *  scanned with size being first priority, format being second priority, and 
   *  finally checking the refresh_rate.  If all the available modes are too 
   *  small, then nil is returned.
   *  
   *  SDL_GetNumDisplayModes()
   *  SDL_GetDisplayMode()
   *}
function SDL_GetClosestDisplayMode(displayIndex: SInt32; const mode: PSDL_DisplayMode; closest: PSDL_DisplayMode): PSDL_DisplayMode;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_GetClosestDisplayMode' {$ENDIF} {$ENDIF};

  {**
   *  Get the display index associated with a window.
   *  
   *  the display index of the display containing the center of the
   *  window, or -1 on error.
   *}
function SDL_GetWindowDisplayIndex(window: PSDL_Window): SInt32;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_GetWindowDisplayIndex' {$ENDIF} {$ENDIF};

  {**
   *  Set the display mode used when a fullscreen window is visible.
   *
   *  By default the window's dimensions and the desktop format and refresh rate
   *  are used.
   *  
   *  mode The mode to use, or nil for the default mode.
   *  
   *  0 on success, or -1 if setting the display mode failed.
   *  
   *  SDL_GetWindowDisplayMode()
   *  SDL_SetWindowFullscreen()
   *}
function SDL_SetWindowDisplayMode(window: PSDL_Window; const mode: PSDL_DisplayMode): SInt32;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_SetWindowDisplayMode' {$ENDIF} {$ENDIF};

  {**
   *  Fill in information about the display mode used when a fullscreen
   *  window is visible.
   *
   *  SDL_SetWindowDisplayMode()
   *  SDL_SetWindowFullscreen()
   *}
function SDL_GetWindowDisplayMode(window: PSDL_Window; mode: PSDL_DisplayMode): SInt32;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_GetWindowDisplayMode' {$ENDIF} {$ENDIF};

  {**
   *  Get the pixel format associated with the window.
   *}
function SDL_GetWindowPixelFormat(window: PSDL_Window): UInt32;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_GetWindowPixelFormat' {$ENDIF} {$ENDIF};

  {**
   *  Create a window with the specified position, dimensions, and flags.
   *  
   *  title The title of the window, in UTF-8 encoding.
   *  x     The x position of the window, ::SDL_WINDOWPOS_CENTERED, or
   *               ::SDL_WINDOWPOS_UNDEFINED.
   *  y     The y position of the window, ::SDL_WINDOWPOS_CENTERED, or
   *               ::SDL_WINDOWPOS_UNDEFINED.
   *  w     The width of the window.
   *  h     The height of the window.
   *  flags The flags for the window, a mask of any of the following:
   *               ::SDL_WINDOW_FULLSCREEN, ::SDL_WINDOW_OPENGL, 
   *               ::SDL_WINDOW_SHOWN,      ::SDL_WINDOW_BORDERLESS, 
   *               ::SDL_WINDOW_RESIZABLE,  ::SDL_WINDOW_MAXIMIZED, 
   *               ::SDL_WINDOW_MINIMIZED,  ::SDL_WINDOW_INPUT_GRABBED.
   *  
   *  The id of the window created, or zero if window creation failed.
   *  
   *  SDL_DestroyWindow()
   *}
function SDL_CreateWindow(const title: PChar; x: SInt32; y: SInt32; w: SInt32; h: SInt32; flags: UInt32): PSDL_Window;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_CreateWindow' {$ENDIF} {$ENDIF};

  {**
   *  Create an SDL window from an existing native window.
   *  
   *  data A pointer to driver-dependent window creation data
   *  
   *  The id of the window created, or zero if window creation failed.
   *
   *  SDL_DestroyWindow()
   *}
function SDL_CreateWindowFrom(const data: Pointer): PSDL_Window;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_CreateWindowFrom' {$ENDIF} {$ENDIF};

  {**
   *  Get the numeric ID of a window, for logging purposes.
   *}
function SDL_GetWindowID(window: PSDL_Window): UInt32;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_GetWindowID' {$ENDIF} {$ENDIF};

  {**
   *  Get a window from a stored ID, or nil if it doesn't exist.
   *}
function SDL_GetWindowFromID(id: UInt32): PSDL_Window;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_GetWindowFromID' {$ENDIF} {$ENDIF};

  {**
   *  Get the window flags.
   *}
function SDL_GetWindowFlags(window: PSDL_Window): UInt32;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_GetWindowFlags' {$ENDIF} {$ENDIF};

  {**
   *  Set the title of a window, in UTF-8 format.
   *  
   *  SDL_GetWindowTitle()
   *}
procedure SDL_SetWindowTitle(window: PSDL_Window; const title: PChar);
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_GetWindowTitle' {$ENDIF} {$ENDIF};

  {**
   *  Get the title of a window, in UTF-8 format.
   *  
   *  SDL_SetWindowTitle()
   *}
function SDL_GetWindowTitle(window: PSDL_Window): PChar;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_GetWindowTitle' {$ENDIF} {$ENDIF};

  {**
   *  Set the icon for a window.
   *  
   *  icon The icon for the window.
   *}
procedure SDL_SetWindowIcon(window: PSDL_Window; icon: PSDL_Surface);
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_SetWindowIcon' {$ENDIF} {$ENDIF};

  {**
   *  Associate an arbitrary named pointer with a window.
   *  
   *  window   The window to associate with the pointer.
   *  name     The name of the pointer.
   *  userdata The associated pointer.
   *
   *  The previous value associated with 'name'
   *
   *  The name is case-sensitive.
   *
   *  SDL_GetWindowData()
   *}
function SDL_SetWindowData(window: PSDL_Window; const name: PChar; userdata: Pointer): Pointer;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_SetWindowData' {$ENDIF} {$ENDIF};

  {**
   *  Retrieve the data pointer associated with a window.
   *  
   *  window   The window to query.
   *  name     The name of the pointer.
   *
   *  The value associated with 'name'
   *  
   *  SDL_SetWindowData()
   *}
function SDL_GetWindowData(window: PSDL_Window; const name: PChar): Pointer;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_GetWindowData' {$ENDIF} {$ENDIF};

  {**
   *  Set the position of a window.
   *  
   *  window   The window to reposition.
   *  x        The x coordinate of the window, SDL_WINDOWPOS_CENTERED, or
   *                  SDL_WINDOWPOS_UNDEFINED.
   *  y        The y coordinate of the window, SDL_WINDOWPOS_CENTERED, or
   *                  SDL_WINDOWPOS_UNDEFINED.
   *  
   *  The window coordinate origin is the upper left of the display.
   *  
   *  SDL_GetWindowPosition()
   *}
procedure SDL_SetWindowPosition(window: PSDL_Window; x: SInt32; y: SInt32);
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_SetWindowPosition' {$ENDIF} {$ENDIF};

  {**
   *  Get the position of a window.
   *  
   *  x        Pointer to variable for storing the x position, may be nil
   *  y        Pointer to variable for storing the y position, may be nil
   *
   *  SDL_SetWindowPosition()
   *}
procedure SDL_GetWindowPosition(window: PSDL_Window; x: PInt; y: PInt);
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_GetWindowPosition' {$ENDIF} {$ENDIF};

  {**
   *  Set the size of a window's client area.
   *  
   *  w        The width of the window, must be >0
   *  h        The height of the window, must be >0
   *
   *  You can't change the size of a fullscreen window, it automatically
   *  matches the size of the display mode.
   *  
   *  SDL_GetWindowSize()
   *}
procedure SDL_SetWindowSize(window: PSDL_Window; w: SInt32; h: SInt32);
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_SetWindowSize' {$ENDIF} {$ENDIF};

  {**
   *  Get the size of a window's client area.
   *  
   *  w        Pointer to variable for storing the width, may be nil
   *  h        Pointer to variable for storing the height, may be nil
   *  
   *  SDL_SetWindowSize()
   *}
procedure SDL_GetWindowSize(window: PSDL_Window; w: PInt; h: PInt);
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_GetWindowSize' {$ENDIF} {$ENDIF};
    
  {**
   *  Set the minimum size of a window's client area.
   *  
   *  min_w     The minimum width of the window, must be >0
   *  min_h     The minimum height of the window, must be >0
   *
   *  You can't change the minimum size of a fullscreen window, it
   *  automatically matches the size of the display mode.
   *
   *  SDL_GetWindowMinimumSize()
   *  SDL_SetWindowMaximumSize()
   *}
procedure SDL_SetWindowMinimumSize(window: PSDL_Window; min_w: SInt32; min_h: SInt32);
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_SetWindowMinimumSize' {$ENDIF} {$ENDIF};
    
  {**
   *  Get the minimum size of a window's client area.
   *  
   *  w        Pointer to variable for storing the minimum width, may be nil
   *  h        Pointer to variable for storing the minimum height, may be nil
   *  
   *  SDL_GetWindowMaximumSize()
   *  SDL_SetWindowMinimumSize()
   *}
procedure SDL_GetWindowMinimumSize(window: PSDL_Window; w: PInt; h: PInt);
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_GetWindowMinimumSize' {$ENDIF} {$ENDIF};

  {**
   *  Set the maximum size of a window's client area.
   *
   *  max_w     The maximum width of the window, must be >0
   *  max_h     The maximum height of the window, must be >0
   *
   *  You can't change the maximum size of a fullscreen window, it
   *  automatically matches the size of the display mode.
   *
   *  SDL_GetWindowMaximumSize()
   *  SDL_SetWindowMinimumSize()
   *}
procedure SDL_SetWindowMaximumSize(window: PSDL_Window; max_w: SInt32; max_h: SInt32);
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_SetWindowMaximumSize' {$ENDIF} {$ENDIF};

  {**
   *  Get the maximum size of a window's client area.
   *  
   *  w        Pointer to variable for storing the maximum width, may be nil
   *  h        Pointer to variable for storing the maximum height, may be nil
   *
   *  SDL_GetWindowMinimumSize()
   *  SDL_SetWindowMaximumSize()
   *}
procedure SDL_GetWindowMaximumSize(window: PSDL_Window; w: PInt; h: PInt);
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_GetWindowMaximumSize' {$ENDIF} {$ENDIF};

  {**
   *  Set the border state of a window.
   *
   *  This will add or remove the window's SDL_WINDOW_BORDERLESS flag and
   *  add or remove the border from the actual window. This is a no-op if the
   *  window's border already matches the requested state.
   *
   *  window The window of which to change the border state.
   *  bordered SDL_FALSE to remove border, SDL_TRUE to add border.
   *
   *  You can't change the border state of a fullscreen window.
   *  
   *  SDL_GetWindowFlags()
   *}
procedure SDL_SetWindowBordered(window: PSDL_Window; bordered: TSDL_Bool);
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_SetWindowBordered' {$ENDIF} {$ENDIF};

  {**
   *  Show a window.
   *  
   *  SDL_HideWindow()
   *}
procedure SDL_ShowWindow(window: PSDL_Window);
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_ShowWindow' {$ENDIF} {$ENDIF};

  {**
   *  Hide a window.
   *
   *  SDL_ShowWindow()
   *}
procedure SDL_HideWindow(window: PSDL_Window);
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_HideWindow' {$ENDIF} {$ENDIF};

  {**
   *  Raise a window above other windows and set the input focus.
   *}
procedure SDL_RaiseWindow(window: PSDL_Window);
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_RaiseWindow' {$ENDIF} {$ENDIF};

  {**
   *  Make a window as large as possible.
   *  
   *  SDL_RestoreWindow()
   *}
procedure SDL_MaximizeWindow(window: PSDL_Window);
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_MaximizeWindow' {$ENDIF} {$ENDIF};

  {**
   *  Minimize a window to an iconic representation.
   *  
   *  SDL_RestoreWindow()
   *}
procedure SDL_MinimizeWindow(window: PSDL_Window);
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_MinimizeWindow' {$ENDIF} {$ENDIF};

  {**
   *  Restore the size and position of a minimized or maximized window.
   *  
   *  SDL_MaximizeWindow()
   *  SDL_MinimizeWindow()
   *}
procedure SDL_RestoreWindow(window: PSDL_Window);
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_RestoreWindow' {$ENDIF} {$ENDIF};

  {**
   *  Set a window's fullscreen state.
   *  
   *  0 on success, or -1 if setting the display mode failed.
   *  
   *  SDL_SetWindowDisplayMode()
   *  SDL_GetWindowDisplayMode()
   *}
function SDL_SetWindowFullscreen(window: PSDL_Window; flags: UInt32): SInt32;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_SetWindowFullscreen' {$ENDIF} {$ENDIF};

  {**
   *  Get the SDL surface associated with the window.
   *
   *  The window's framebuffer surface, or nil on error.
   *
   *  A new surface will be created with the optimal format for the window,
   *  if necessary. This surface will be freed when the window is destroyed.
   *
   *  You may not combine this with 3D or the rendering API on this window.
   *
   *  SDL_UpdateWindowSurface()
   *  SDL_UpdateWindowSurfaceRects()
   *}
function SDL_GetWindowSurface(window: PSDL_Window): PSDL_Surface;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_GetWindowSurface' {$ENDIF} {$ENDIF};

  {**
   *  Copy the window surface to the screen.
   *
   *  0 on success, or -1 on error.
   *
   *  SDL_GetWindowSurface()
   *  SDL_UpdateWindowSurfaceRects()
   *}
function SDL_UpdateWindowSurface(window: PSDL_Window): SInt32;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_UpdateWindowSurface' {$ENDIF} {$ENDIF};

  {**
   *  Copy a number of rectangles on the window surface to the screen.
   *
   *  0 on success, or -1 on error.
   *
   *  SDL_GetWindowSurface()
   *  SDL_UpdateWindowSurfaceRect()
   *}
function SDL_UpdateWindowSurfaceRects(window: PSDL_Window; rects: PSDL_Rect; numrects: SInt32): SInt32;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_UpdateWindowSurfaceRects' {$ENDIF} {$ENDIF};

  {**
   *  Set a window's input grab mode.
   *  
   *  grabbed This is SDL_TRUE to grab input, and SDL_FALSE to release input.
   *  
   *  SDL_GetWindowGrab()
   *}
procedure SDL_SetWindowGrab(window: PSDL_Window; grabbed: TSDL_Bool);
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_SetWindowGrab' {$ENDIF} {$ENDIF};

  {**
   *  Get a window's input grab mode.
   *  
   *  This returns SDL_TRUE if input is grabbed, and SDL_FALSE otherwise.
   *
   *  SDL_SetWindowGrab()
   *}
function SDL_GetWindowGrab(window: PSDL_Window): TSDL_Bool;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_GetWindowGrab' {$ENDIF} {$ENDIF};

  {**
   *  Set the brightness (gamma correction) for a window.
   *
   *  0 on success, or -1 if setting the brightness isn't supported.
   *  
   *  SDL_GetWindowBrightness()
   *  SDL_SetWindowGammaRamp()
   *}
function SDL_SetWindowBrightness(window: PSDL_Window; brightness: Float): SInt32;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_SetWindowBrightness' {$ENDIF} {$ENDIF};

  {**
   *  Get the brightness (gamma correction) for a window.
   *  
   *  The last brightness value passed to SDL_SetWindowBrightness()
   *  
   *  SDL_SetWindowBrightness()
   *}
function SDL_GetWindowBrightness(window: PSDL_Window): Float;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_GetWindowBrightness' {$ENDIF} {$ENDIF};

  {**
   *  Set the gamma ramp for a window.
   *  
   *  red The translation table for the red channel, or nil.
   *  green The translation table for the green channel, or nil.
   *  blue The translation table for the blue channel, or nil.
   *
   *  0 on success, or -1 if gamma ramps are unsupported.
   *  
   *  Set the gamma translation table for the red, green, and blue channels
   *  of the video hardware.  Each table is an array of 256 16-bit quantities,
   *  representing a mapping between the input and output for that channel.
   *  The input is the index into the array, and the output is the 16-bit
   *  gamma value at that index, scaled to the output color precision.
   *
   *  SDL_GetWindowGammaRamp()
   *}
function SDL_SetWindowGammaRamp(window: PSDL_Window; const red: PUInt16; const green: PUInt16; const blue: PUInt16): SInt32;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_SetWindowGammaRamp' {$ENDIF} {$ENDIF};

  {**
   *  Get the gamma ramp for a window.
   *  
   *  red   A pointer to a 256 element array of 16-bit quantities to hold
   *        the translation table for the red channel, or nil.
   *  green A pointer to a 256 element array of 16-bit quantities to hold
   *        the translation table for the green channel, or nil.
   *  blue  A pointer to a 256 element array of 16-bit quantities to hold
   *        the translation table for the blue channel, or nil.
   *   
   *  0 on success, or -1 if gamma ramps are unsupported.
   *  
   *  SDL_SetWindowGammaRamp()
   *}
function SDL_GetWindowGammaRamp(window: PSDL_Window; red: PUInt16; green: PUInt16; blue: PUInt16): SInt32;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_GetWindowGammaRamp' {$ENDIF} {$ENDIF};

  {**
   *  Destroy a window.
   *}
procedure SDL_DestroyWindow(window: PSDL_Window);
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_DestroyWindow' {$ENDIF} {$ENDIF};

  {**
   *  Returns whether the screensaver is currently enabled (default on).
   *  
   *  SDL_EnableScreenSaver()
   *  SDL_DisableScreenSaver()
   *}
function SDL_IsScreenSaverEnabled: TSDL_Bool;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_IsScreenSaverEnabled' {$ENDIF} {$ENDIF};

  {**
   *  Allow the screen to be blanked by a screensaver
   *  
   *  SDL_IsScreenSaverEnabled()
   *  SDL_DisableScreenSaver()
   *}
procedure SDL_EnableScreenSaver();
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_EnableScreenSaver' {$ENDIF} {$ENDIF};

  {**
   *  Prevent the screen from being blanked by a screensaver
   *  
   *  SDL_IsScreenSaverEnabled()
   *  SDL_EnableScreenSaver()
   *}
procedure SDL_DisableScreenSaver();
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_DisableScreenSaver' {$ENDIF} {$ENDIF};

  {**
   *  OpenGL support functions
   *}

  {**
   *  Dynamically load an OpenGL library.
   *  
   *  path The platform dependent OpenGL library name, or nil to open the
   *              default OpenGL library.
   *  
   *  0 on success, or -1 if the library couldn't be loaded.
   *
   *  This should be done after initializing the video driver, but before
   *  creating any OpenGL windows.  If no OpenGL library is loaded, the default
   *  library will be loaded upon creation of the first OpenGL window.
   *  
   *  If you do this, you need to retrieve all of the GL functions used in
   *  your program from the dynamic library using SDL_GL_GetProcAddress().
   *  
   *  SDL_GL_GetProcAddress()
   *  SDL_GL_UnloadLibrary()
   *}
function SDL_GL_LoadLibrary(const path: PChar): SInt32;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_GL_LoadLibrary' {$ENDIF} {$ENDIF};

  {**
   *  Get the address of an OpenGL function.
   *}
function SDL_GL_GetProcAddress(const proc: PChar): Pointer;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_GL_GetProcAddress' {$ENDIF} {$ENDIF};

  {**
   *  Unload the OpenGL library previously loaded by SDL_GL_LoadLibrary().
   *  
   *  SDL_GL_LoadLibrary()
   *}
procedure SDL_GL_UnloadLibrary();
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_GL_UnloadLibrary' {$ENDIF} {$ENDIF};

  {**
   *  Return true if an OpenGL extension is supported for the current
   *  context.
   *}
function SDL_GL_ExtensionSupported(const extension: PChar): TSDL_Bool;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_GL_ExtensionSupported' {$ENDIF} {$ENDIF};

  {**
   *  Set an OpenGL window attribute before window creation.
   *}
function SDL_GL_SetAttribute(attr: TSDL_GLattr; value: SInt32): SInt32;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_GL_SetAttribute' {$ENDIF} {$ENDIF};

  {**
   *  Get the actual value for an attribute from the current context.
   *}
function SDL_GL_GetAttribute(attr: TSDL_GLattr; value: PInt): SInt32;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_GL_GetAttribute' {$ENDIF} {$ENDIF};

  {**
   *  Create an OpenGL context for use with an OpenGL window, and make it
   *  current.
   *
   *  SDL_GL_DeleteContext()
   *}
function SDL_GL_CreateContext(window: PSDL_Window): TSDL_GLContext;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_GL_CreateContext' {$ENDIF} {$ENDIF};

  {**
   *  Set up an OpenGL context for rendering into an OpenGL window.
   *  
   *  The context must have been created with a compatible window.
   *}
function SDL_GL_MakeCurrent(window: PSDL_Window; context: TSDL_GLContext): SInt32;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_GL_MakeCurrent' {$ENDIF} {$ENDIF};

  {**
   *  Get the currently active OpenGL window.
   *}
function SDL_GL_GetCurrentWindow(): PSDL_Window;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_GL_GetCurrentWindow' {$ENDIF} {$ENDIF};

  {**
   *  Get the currently active OpenGL context.
   *}
function SDL_GL_GetCurrentContext(): TSDL_GLContext;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_GL_GetCurrentContext' {$ENDIF} {$ENDIF};

  {**
   *  Set the swap interval for the current OpenGL context.
   *  
   *  interval 0 for immediate updates, 1 for updates synchronized with the
   *  vertical retrace. If the system supports it, you may
   *  specify -1 to allow late swaps to happen immediately
   *  instead of waiting for the next retrace.
   *
   *  0 on success, or -1 if setting the swap interval is not supported.
   *  
   *  SDL_GL_GetSwapInterval()
   *}
function SDL_GL_SetSwapInterval(interval: SInt32): SInt32;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_GL_SetSwapInterval' {$ENDIF} {$ENDIF};

  {**
   *  Get the swap interval for the current OpenGL context.
   *  
   *  0 if there is no vertical retrace synchronization, 1 if the buffer
   *  swap is synchronized with the vertical retrace, and -1 if late
   *  swaps happen immediately instead of waiting for the next retrace.
   *  If the system can't determine the swap interval, or there isn't a
   *  valid current context, this will return 0 as a safe default.
   *  
   *  SDL_GL_SetSwapInterval()
   *}
function SDL_GL_GetSwapInterval(): SInt32;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_GL_GetSwapInterval' {$ENDIF} {$ENDIF};

  {**
   *  Swap the OpenGL buffers for a window, if double-buffering is
   *  supported.
   *}
procedure SDL_GL_SwapWindow(window: PSDL_Window);
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_GL_SwapWindow' {$ENDIF} {$ENDIF};

  {**
   *  Delete an OpenGL context.
   *  
   *  SDL_GL_CreateContext()
   *}
procedure SDL_GL_DeleteContext(context: TSDL_GLContext);
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_GL_DeleteContext' {$ENDIF} {$ENDIF};

  {*OpenGL support functions*}



////////////////////////////////////////////////////////////////////////////////////////////////////////  
//////////////////////        SDL_renderer.h         ///////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////  

  {**
   *  Flags used when creating a rendering context
   *}
const
  SDL_RENDERER_SOFTWARE = $00000001;          {**< The renderer is a software fallback *}
  SDL_RENDERER_ACCELERATED = $00000002;       {**< The renderer uses hardware
                                                   acceleration *}
  SDL_RENDERER_PRESENTVSYNC = $00000004;      {**< Present is synchronized
                                                   with the refresh rate *}
  SDL_RENDERER_TARGETTEXTURE = $00000008;     {**< The renderer supports
                                                   rendering to texture *}

type
  PSDL_RendererFlags = ^TSDL_RendererFlags;
  TSDL_RendererFlags = Word;

  {**
   *  Information on the capabilities of a render driver or context.
   *}
  PSDL_RendererInfo = ^TSDL_RendererInfo;
  TSDL_RendererInfo = record  
    name: PChar;                         {**< The name of the renderer *}
    flags: UInt32;                           {**< Supported ::SDL_RendererFlags *}
    num_texture_formats: UInt32;             {**< The number of available texture formats *}
    texture_formats: array[0..15] of UInt32; {**< The available texture formats *}
    max_texture_width: SInt32;               {**< The maximimum texture width *}
    max_texture_height: SInt32;              {**< The maximimum texture height *}
  end;

  {**
   *  The access pattern allowed for a texture.
   *}
type
  PSDL_TextureAccess = ^TSDL_TextureAccess;
  TSDL_TextureAccess = (
                        SDL_TEXTUREACCESS_STATIC,    {**< Changes rarely, not lockable *}
                        SDL_TEXTUREACCESS_STREAMING, {**< Changes frequently, lockable *}
                        SDL_TEXTUREACCESS_TARGET     {**< Texture can be used as a render target *}
                        );

  {**
   *  The texture channel modulation used in SDL_RenderCopy().
   *}
  PSDL_TextureModulate = ^TSDL_TextureModulate;
  TSDL_TextureModulate = (
                          SDL_TEXTUREMODULATE_NONE,     {**< No modulation *}
                          SDL_TEXTUREMODULATE_COLOR,    {**< srcC = srcC * color *}
                          SDL_TEXTUREMODULATE_ALPHA     {**< srcA = srcA * alpha *}
                          );

  {**
   *  Flip constants for SDL_RenderCopyEx
   *}
type
  PSDL_RendererFlip = ^TSDL_RendererFlip;
  TSDL_RendererFlip = (SDL_FLIP_NONE,       {**< Do not flip *}
                       SDL_FLIP_HORIZONTAL, {**< flip horizontally *}
                       SDL_FLIP_VERTICAL    {**< flip vertically *}
                       );

  {**
   *  A structure representing rendering state
   *}

  PPSDL_Renderer = ^PSDL_Renderer;
  PSDL_Renderer = Pointer; //todo!

  {**
   *  An efficient driver-specific representation of pixel data
   *}
  PSDL_Texture = Pointer; //todo!

  {* Function prototypes *}

  {**
   *  Get the number of 2D rendering drivers available for the current
   *  display.
   *
   *  A render driver is a set of code that handles rendering and texture
   *  management on a particular display.  Normally there is only one, but
   *  some drivers may have several available with different capabilities.
   *
   *   SDL_GetRenderDriverInfo()
   *   SDL_CreateRenderer()
   *}
function SDL_GetNumRenderDrivers(): SInt32;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_GetNumRenderDrivers' {$ENDIF} {$ENDIF};

  {**
   *  Get information about a specific 2D rendering driver for the current
   *  display.
   *
   *   index The index of the driver to query information about.
   *   info  A pointer to an SDL_RendererInfo struct to be filled with
   *               information on the rendering driver.
   *
   *   0 on success, -1 if the index was out of range.
   *
   *   SDL_CreateRenderer()
   *}
function SDL_GetRenderDriverInfo(index: SInt32; info: PSDL_RendererInfo): SInt32;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_GetRenderDriverInfo' {$ENDIF} {$ENDIF};

  {**
   *  Create a window and default renderer
   *
   *   width    The width of the window
   *   height   The height of the window
   *   window_flags The flags used to create the window
   *   window   A pointer filled with the window, or NULL on error
   *   renderer A pointer filled with the renderer, or NULL on error
   *
   *   0 on success, or -1 on error
   *}
function SDL_CreateWindowAndRenderer(width: SInt32; height: SInt32; window_flags: UInt32; window: PPSDL_Window; renderer: PPSDL_Renderer): SInt32;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_CreateWindowAndRenderer' {$ENDIF} {$ENDIF};

  {**
   *  Create a 2D rendering context for a window.
   *
   *   window The window where rendering is displayed.
   *   index    The index of the rendering driver to initialize, or -1 to
   *                  initialize the first one supporting the requested flags.
   *   flags    ::SDL_RendererFlags.
   *
   *   A valid rendering context or NULL if there was an error.
   *
   *   SDL_CreateSoftwareRenderer()
   *   SDL_GetRendererInfo()
   *   SDL_DestroyRenderer()
   *}
function SDL_CreateRenderer(window: PSDL_Window; index: SInt32; flags: UInt32): PSDL_Renderer;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_CreateRenderer' {$ENDIF} {$ENDIF};

  {**
   *  Create a 2D software rendering context for a surface.
   *
   *   surface The surface where rendering is done.
   *
   *   A valid rendering context or NULL if there was an error.
   *
   *   SDL_CreateRenderer()
   *   SDL_DestroyRenderer()
   *}
function SDL_CreateSoftwareRenderer(surface: PSDL_Surface): PSDL_Renderer;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_CreateSoftwareRenderer' {$ENDIF} {$ENDIF};

  {**
   *  Get the renderer associated with a window.
   *}
function SDL_GetRenderer(window: PSDL_Window): PSDL_Renderer;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_GetRenderer' {$ENDIF} {$ENDIF};

  {**
   *  Get information about a rendering context.
   *}
function SDL_GetRendererInfo(renderer: PSDL_Renderer; info: PSDL_RendererInfo): SInt32;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_GetRendererInfo' {$ENDIF} {$ENDIF};

  {**
   *  Get the output size of a rendering context.
   *}
function SDL_GetRendererOutputSize(renderer: PSDL_Renderer; w: PInt; h: PInt): SInt32;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_GetRendererOutputSize' {$ENDIF} {$ENDIF};

  {**
   *  Create a texture for a rendering context.
   *
   *   renderer The renderer.
   *   format The format of the texture.
   *   access One of the enumerated values in ::SDL_TextureAccess.
   *   w      The width of the texture in pixels.
   *   h      The height of the texture in pixels.
   *
   *   The created texture is returned, or 0 if no rendering context was
   *   active,  the format was unsupported, or the width or height were out
   *   of range.
   *
   *  SDL_QueryTexture()
   *  SDL_UpdateTexture()
   *  SDL_DestroyTexture()
   *}
function SDL_CreateTexture(renderer: PSDL_Renderer; format: UInt32; access: SInt32; w: SInt32; h: SInt32): PSDL_Texture;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_CreateTexture' {$ENDIF} {$ENDIF};

  {**
   *  Create a texture from an existing surface.
   *
   *   renderer The renderer.
   *   surface The surface containing pixel data used to fill the texture.
   *
   *   The created texture is returned, or 0 on error.
   *
   *   The surface is not modified or freed by this function.
   *
   *   SDL_QueryTexture()
   *   SDL_DestroyTexture()
   *}
function SDL_CreateTextureFromSurface(renderer: PSDL_Renderer; surface: PSDL_Surface): PSDL_Texture;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_CreateTextureFromSurface' {$ENDIF} {$ENDIF};

  {**
   *  Query the attributes of a texture
   *
   *   texture A texture to be queried.
   *   format  A pointer filled in with the raw format of the texture.  The
   *           actual format may differ, but pixel transfers will use this
   *           format.
   *   access  A pointer filled in with the actual access to the texture.
   *   w       A pointer filled in with the width of the texture in pixels.
   *   h       A pointer filled in with the height of the texture in pixels.
   *
   *   0 on success, or -1 if the texture is not valid.
   *}
function SDL_QueryTexture(texture: PSDL_Texture; format: PUInt32; access: PInt; w: PInt; h: PInt): SInt32;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_QueryTexture' {$ENDIF} {$ENDIF};

  {**
   *  Set an additional color value used in render copy operations.
   *
   *   texture The texture to update.
   *   r       The red color value multiplied into copy operations.
   *   g       The green color value multiplied into copy operations.
   *   b       The blue color value multiplied into copy operations.
   *
   *   0 on success, or -1 if the texture is not valid or color modulation
   *   is not supported.
   *
   *   SDL_GetTextureColorMod()
   *}
function SDL_SetTextureColorMod(texture: PSDL_Texture; r: UInt8; g: UInt8; b: UInt8): SInt32;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_SetTextureColorMod' {$ENDIF} {$ENDIF};

  {**
   *  Get the additional color value used in render copy operations.
   *
   *   texture The texture to query.
   *   r         A pointer filled in with the current red color value.
   *   g         A pointer filled in with the current green color value.
   *   b         A pointer filled in with the current blue color value.
   *
   *   0 on success, or -1 if the texture is not valid.
   *
   *   SDL_SetTextureColorMod()
   *}
function SDL_GetTextureColorMod(texture: PSDL_Texture; r: PUInt8; g: PUInt8; b: PUInt8): SInt32;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_GetTextureColorMod' {$ENDIF} {$ENDIF};

  {**
   *  Set an additional alpha value used in render copy operations.
   *
   *   texture The texture to update.
   *   alpha     The alpha value multiplied into copy operations.
   *
   *   0 on success, or -1 if the texture is not valid or alpha modulation
   *   is not supported.
   *
   *   SDL_GetTextureAlphaMod()
   *}
function SDL_SetTextureAlphaMod(texture: PSDL_Texture; alpha: UInt8): SInt32;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_SetTextureAlphaMod' {$ENDIF} {$ENDIF};

  {**
   *  Get the additional alpha value used in render copy operations.
   *
   *   texture The texture to query.
   *   alpha     A pointer filled in with the current alpha value.
   *
   *   0 on success, or -1 if the texture is not valid.
   *
   *   SDL_SetTextureAlphaMod()
   *}
function SDL_GetTextureAlphaMod(texture: PSDL_Texture; alpha: PUInt8): SInt32;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_GetTextureAlphaMod' {$ENDIF} {$ENDIF};

  {**
   *   Set the blend mode used for texture copy operations.
   *
   *   texture The texture to update.
   *   blendMode ::SDL_BlendMode to use for texture blending.
   *
   *   0 on success, or -1 if the texture is not valid or the blend mode is
   *   not supported.
   *
   *   If the blend mode is not supported, the closest supported mode is
   *   chosen.
   *
   *   SDL_GetTextureBlendMode()
   *}
function SDL_SetTextureBlendMode(texture: PSDL_Texture; blendMode: TSDL_BlendMode): SInt32;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_SetTextureBlendMode' {$ENDIF} {$ENDIF};

  {**
   *  Get the blend mode used for texture copy operations.
   *
   *   texture   The texture to query.
   *   blendMode A pointer filled in with the current blend mode.
   *
   *   0 on success, or -1 if the texture is not valid.
   *
   *   SDL_SetTextureBlendMode()
   *}
function SDL_GetTextureBlendMode(texture: PSDL_Texture; blendMode: PSDL_BlendMode): SInt32;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_GetTextureBlendMode' {$ENDIF} {$ENDIF};

  {**
   *  Update the given texture rectangle with new pixel data.
   *
   *   texture   The texture to update
   *   rect      A pointer to the rectangle of pixels to update, or NULL to
   *                   update the entire texture.
   *   pixels    The raw pixel data.
   *   pitch     The number of bytes between rows of pixel data.
   *
   *   0 on success, or -1 if the texture is not valid.
   *
   *   This is a fairly slow function.
   *}
function SDL_UpdateTexture(texture: PSDL_Texture; rect: PSDL_Rect; pixels: Pointer; pitch: SInt32): SInt32;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_UpdateTexture' {$ENDIF} {$ENDIF};

  {**
   *  Lock a portion of the texture for write-only pixel access.
   *
   *   texture   The texture to lock for access, which was created with
   *             SDL_TEXTUREACCESS_STREAMING.
   *   rect      A pointer to the rectangle to lock for access. If the rect
   *             is NULL, the entire texture will be locked.
   *   pixels    This is filled in with a pointer to the locked pixels,
   *             appropriately offset by the locked area.
   *   pitch     This is filled in with the pitch of the locked pixels.
   *
   *   0 on success, or -1 if the texture is not valid or was not created with ::SDL_TEXTUREACCESS_STREAMING.
   *
   *   SDL_UnlockTexture()
   *}
function SDL_LockTexture(texture: PSDL_Texture; rect: PSDL_Rect; pixels: PPointer; pitch: PInt): SInt32;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_LockTexture' {$ENDIF} {$ENDIF};

  {**
   *  Unlock a texture, uploading the changes to video memory, if needed.
   *
   *   SDL_LockTexture()
   *}
procedure SDL_UnlockTexture(texture: PSDL_Texture);
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_LockTexture' {$ENDIF} {$ENDIF};

  {**
   *  Determines whether a window supports the use of render targets
   *
   *  renderer The renderer that will be checked
   *
   *  SDL_TRUE if supported, SDL_FALSE if not.
   *}
function SDL_RenderTargetSupported(renderer: PSDL_Renderer): Boolean;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_RenderTargetSupported' {$ENDIF} {$ENDIF};

  {**
   *  Set a texture as the current rendering target.
   *
   *  renderer The renderer.
   *  texture The targeted texture, which must be created with the SDL_TEXTUREACCESS_TARGET flag, or NULL for the default render target
   *
   *  0 on success, or -1 on error
   *
   *   SDL_GetRenderTarget()
   *}
function SDL_SetRenderTarget(renderer: PSDL_Renderer; texture: PSDL_Texture): SInt32;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_SetRenderTarget' {$ENDIF} {$ENDIF};

  {**
   *  Get the current render target or NULL for the default render target.
   *
   *  The current render target
   *
   *   SDL_SetRenderTarget()
   *}
function SDL_GetRenderTarget(renderer: PSDL_Renderer): PSDL_Texture;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_GetRenderTarget' {$ENDIF} {$ENDIF};

  {**
   *  Set device independent resolution for rendering
   *
   *   renderer The renderer for which resolution should be set.
   *   w      The width of the logical resolution
   *   h      The height of the logical resolution
   *
   *  This function uses the viewport and scaling functionality to allow a fixed logical
   *  resolution for rendering, regardless of the actual output resolution.  If the actual
   *  output resolution doesn't have the same aspect ratio the output rendering will be
   *  centered within the output display.
   *
   *  If the output display is a window, mouse events in the window will be filtered
   *  and scaled so they seem to arrive within the logical resolution.
   *
   *   If this function results in scaling or subpixel drawing by the
   *   rendering backend, it will be handled using the appropriate
   *   quality hints.
   *
   *   SDL_RenderGetLogicalSize()
   *   SDL_RenderSetScale()
   *   SDL_RenderSetViewport()
   *}
function SDL_RenderSetLogicalSize(renderer: PSDL_Renderer; w: SInt32; h: SInt32): SInt32;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_RenderSetLogicalSize' {$ENDIF} {$ENDIF};

  {**
   *  Get device independent resolution for rendering
   *
   *   renderer The renderer from which resolution should be queried.
   *   w      A pointer filled with the width of the logical resolution
   *   h      A pointer filled with the height of the logical resolution
   *
   *   SDL_RenderSetLogicalSize()
   *}
procedure SDL_RenderGetLogicalSize(renderer: PSDL_Renderer; w: PInt; h: PInt);
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_RenderGetLogicalSize' {$ENDIF} {$ENDIF};

  {**
   *  Set the drawing area for rendering on the current target.
   *
   *   renderer The renderer for which the drawing area should be set.
   *   rect The rectangle representing the drawing area, or NULL to set the viewport to the entire target.
   *
   *  The x,y of the viewport rect represents the origin for rendering.
   *
   *   0 on success, or -1 on error
   *
   *  If the window associated with the renderer is resized, the viewport is automatically reset.
   *
   *   SDL_RenderGetViewport()
   *   SDL_RenderSetLogicalSize()
   *}
function SDL_RenderSetViewport(renderer: PSDL_Renderer; const rect: PSDL_Rect): SInt32;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_RenderSetViewport' {$ENDIF} {$ENDIF};

  {**
   *  Get the drawing area for the current target.
   *
   *   SDL_RenderSetViewport()
   *}
procedure SDL_RenderGetViewport(renderer: PSDL_Renderer; rect: PSDL_Rect);
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_RenderGetViewport' {$ENDIF} {$ENDIF};

  {**
   *  Set the clip rectangle for the current target.
   *
   *   renderer The renderer for which clip rectangle should be set.
   *   rect   A pointer to the rectangle to set as the clip rectangle, or
   *          NULL to disable clipping.
   *
   *   0 on success, or -1 on error
   *
   *   SDL_RenderGetClipRect()
   *}
function SDL_RenderSetClipRect(renderer: PSDL_Renderer; rect: PSDL_Rect): SInt32;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_RenderSetClipRect' {$ENDIF} {$ENDIF};

  {**
   *  Get the clip rectangle for the current target.
   *
   *   renderer The renderer from which clip rectangle should be queried.
   *   rect   A pointer filled in with the current clip rectangle, or
   *          an empty rectangle if clipping is disabled.
   *
   *   SDL_RenderSetClipRect()
   *}
procedure SDL_RenderGetClipRect(renderer: PSDL_Renderer; rect: PSDL_Rect);
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_RenderGetClipRect' {$ENDIF} {$ENDIF};

  {**
   *  Set the drawing scale for rendering on the current target.
   *
   *   renderer The renderer for which the drawing scale should be set.
   *   scaleX The horizontal scaling factor
   *   scaleY The vertical scaling factor
   *
   *  The drawing coordinates are scaled by the x/y scaling factors
   *  before they are used by the renderer.  This allows resolution
   *  independent drawing with a single coordinate system.
   *
   *  If this results in scaling or subpixel drawing by the
   *  rendering backend, it will be handled using the appropriate
   *  quality hints.  For best results use integer scaling factors.
   *
   *   SDL_RenderGetScale()
   *   SDL_RenderSetLogicalSize()
   *}
function SDL_RenderSetScale(renderer: PSDL_Renderer; scaleX: Float; scaleY: Float): SInt32;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_RenderSetScale' {$ENDIF} {$ENDIF};

  {**
   *  Get the drawing scale for the current target.
   *
   *   renderer The renderer from which drawing scale should be queried.
   *   scaleX A pointer filled in with the horizontal scaling factor
   *   scaleY A pointer filled in with the vertical scaling factor
   *
   *   SDL_RenderSetScale()
   *}
procedure SDL_RenderGetScale(renderer: PSDL_Renderer; scaleX: PFloat; scaleY: PFloat);
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_RenderGetScale' {$ENDIF} {$ENDIF};

  {**
   *  Set the color used for drawing operations (Rect, Line and Clear).
   *
   *   renderer The renderer for which drawing color should be set.
   *   r The red value used to draw on the rendering target.
   *   g The green value used to draw on the rendering target.
   *   b The blue value used to draw on the rendering target.
   *   a The alpha value used to draw on the rendering target, usually
   *     SDL_ALPHA_OPAQUE (255).
   *
   *   0 on success, or -1 on error
   *}
function SDL_SetRenderDrawColor(renderer: PSDL_Renderer; r: UInt8; g: UInt8; b: UInt8; a: UInt8): SInt32;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_SetRenderDrawColor' {$ENDIF} {$ENDIF};

  {**
   *  Get the color used for drawing operations (Rect, Line and Clear).
   *
   *   renderer The renderer from which drawing color should be queried.
   *   r A pointer to the red value used to draw on the rendering target.
   *   g A pointer to the green value used to draw on the rendering target.
   *   b A pointer to the blue value used to draw on the rendering target.
   *   a A pointer to the alpha value used to draw on the rendering target,
   *     usually SDL_ALPHA_OPAQUE (255).
   *
   *   0 on success, or -1 on error
   *}
function SDL_GetRenderDrawColor(renderer: PSDL_Renderer; r: PUInt8; g: PUInt8; b: PUInt8; a: PUInt8): SInt32;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_GetRenderDrawColor' {$ENDIF} {$ENDIF};

  {**
   *  Set the blend mode used for drawing operations (Fill and Line).
   *
   *   renderer The renderer for which blend mode should be set.
   *   blendMode SDL_BlendMode to use for blending.
   *
   *   0 on success, or -1 on error
   *
   *   If the blend mode is not supported, the closest supported mode is
   *        chosen.
   *
   *   SDL_GetRenderDrawBlendMode()
   *}
function SDL_SetRenderDrawBlendMode(renderer: PSDL_Renderer; blendMode: TSDL_BlendMode): SInt32;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_SetRenderDrawBlendMode' {$ENDIF} {$ENDIF};

  {**
   *  Get the blend mode used for drawing operations.
   *
   *   renderer The renderer from which blend mode should be queried.
   *   blendMode A pointer filled in with the current blend mode.
   *
   *   0 on success, or -1 on error
   *
   *   SDL_SetRenderDrawBlendMode()
   *}
function SDL_GetRenderDrawBlendMode(renderer: PSDL_Renderer; blendMode: PSDL_BlendMode): SInt32;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_GetRenderDrawBlendMode' {$ENDIF} {$ENDIF};

  {**
   *  Clear the current rendering target with the drawing color
   *
   *  This function clears the entire rendering target, ignoring the viewport.
   *
   *   0 on success, or -1 on error
   *}
function SDL_RenderClear(renderer: PSDL_Renderer): SInt32;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_RenderClear' {$ENDIF} {$ENDIF};

  {**
   *  Draw a point on the current rendering target.
   *
   *   renderer The renderer which should draw a point.
   *   x The x coordinate of the point.
   *   y The y coordinate of the point.
   *
   *   0 on success, or -1 on error
   *}
function SDL_RenderDrawPoint(renderer: PSDL_Renderer; x: SInt32; y: SInt32): SInt32;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_RenderDrawPoint' {$ENDIF} {$ENDIF};

  {**
   *  Draw multiple points on the current rendering target.
   *
   *   renderer The renderer which should draw multiple points.
   *   points The points to draw
   *   count The number of points to draw
   *
   *   0 on success, or -1 on error
   *}
function SDL_RenderDrawPoints(renderer: PSDL_Renderer; points: PSDL_Point; count: SInt32): SInt32;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_RenderDrawPoints' {$ENDIF} {$ENDIF};

  {**
   *  Draw a line on the current rendering target.
   *
   *   renderer The renderer which should draw a line.
   *   x1 The x coordinate of the start point.
   *   y1 The y coordinate of the start point.
   *   x2 The x coordinate of the end point.
   *   y2 The y coordinate of the end point.
   *
   *   0 on success, or -1 on error
   *}
function SDL_RenderDrawLine(renderer: PSDL_Renderer; x1: SInt32; y1: SInt32; x2: SInt32; y2: SInt32): SInt32;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_RenderDrawLine' {$ENDIF} {$ENDIF};

  {**
   *  \brief Draw a series of connected lines on the current rendering target.
   *
   *  \param renderer The renderer which should draw multiple lines.
   *  \param points The points along the lines
   *  \param count The number of points, drawing count-1 lines
   *
   *  \return 0 on success, or -1 on error
   *}
function SDL_RenderDrawLines(renderer: PSDL_Renderer; points: PSDL_Point; count: SInt32): SInt32;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_RenderDrawLines' {$ENDIF} {$ENDIF};

  {**
   *  Draw a rectangle on the current rendering target.
   *
   *   renderer The renderer which should draw a rectangle.
   *   rect A pointer to the destination rectangle, or NULL to outline the entire rendering target.
   *
   *   0 on success, or -1 on error
   *}
function SDL_RenderDrawRect(renderer: PSDL_Renderer; rect: PSDL_Rect): SInt32;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_RenderDrawRect' {$ENDIF} {$ENDIF};

  {**
   *  Draw some number of rectangles on the current rendering target.
   *
   *   renderer The renderer which should draw multiple rectangles.
   *   rects A pointer to an array of destination rectangles.
   *   count The number of rectangles.
   *
   *   0 on success, or -1 on error
   *}
function SDL_RenderDrawRects(renderer: PSDL_Renderer; rects: PSDL_Rect; count: SInt32): SInt32;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_RenderDrawRects' {$ENDIF} {$ENDIF};

  {**
   *  Fill a rectangle on the current rendering target with the drawing color.
   *
   *   renderer The renderer which should fill a rectangle.
   *   rect A pointer to the destination rectangle, or NULL for the entire
   *        rendering target.
   *
   *   0 on success, or -1 on error
   *}
function SDL_RenderFillRect(renderer: PSDL_Renderer; rect: PSDL_Rect): SInt32;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_RenderFillRect' {$ENDIF} {$ENDIF};

  {**
   *  Fill some number of rectangles on the current rendering target with the drawing color.
   *
   *   renderer The renderer which should fill multiple rectangles.
   *   rects A pointer to an array of destination rectangles.
   *   count The number of rectangles.
   *
   *   0 on success, or -1 on error
   *}
function SDL_RenderFillRects(renderer: PSDL_Renderer; rects: PSDL_Rect; count: SInt32): SInt32;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_RenderFillRects' {$ENDIF} {$ENDIF};

  {**
   *  Copy a portion of the texture to the current rendering target.
   *
   *   renderer The renderer which should copy parts of a texture.
   *   texture The source texture.
   *   srcrect   A pointer to the source rectangle, or NULL for the entire
   *             texture.
   *   dstrect   A pointer to the destination rectangle, or NULL for the
   *             entire rendering target.
   *
   *   0 on success, or -1 on error
   *}
function SDL_RenderCopy(renderer: PSDL_Renderer; texture: PSDL_Texture; srcrect: PSDL_Rect; dstrect: PSDL_Rect): SInt32;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_RenderCopy' {$ENDIF} {$ENDIF};

  {**
   *  Copy a portion of the source texture to the current rendering target, rotating it by angle around the given center
   *
   *   renderer The renderer which should copy parts of a texture.
   *   texture The source texture.
   *   srcrect   A pointer to the source rectangle, or NULL for the entire
   *                   texture.
   *   dstrect   A pointer to the destination rectangle, or NULL for the
   *                   entire rendering target.
   *   angle    An angle in degrees that indicates the rotation that will be applied to dstrect
   *   center   A pointer to a point indicating the point around which dstrect will be rotated (if NULL, rotation will be done aroud dstrect.w/2, dstrect.h/2)
   *   flip     An SDL_RendererFlip value stating which flipping actions should be performed on the texture
   *
   *   0 on success, or -1 on error
   *}
function SDL_RenderCopyEx(renderer: PSDL_Renderer; texture: PSDL_Texture; const srcrect: PSDL_Rect; dstrect: PSDL_Rect; angle: Double; center: PSDL_Point; flip: PSDL_RendererFlip): SInt32;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_RenderCopyEx' {$ENDIF} {$ENDIF};

  {**
   *  Read pixels from the current rendering target.
   *
   *   renderer The renderer from which pixels should be read.
   *   rect   A pointer to the rectangle to read, or NULL for the entire
   *                render target.
   *   format The desired format of the pixel data, or 0 to use the format
   *                of the rendering target
   *   pixels A pointer to be filled in with the pixel data
   *   pitch  The pitch of the pixels parameter.
   *
   *   0 on success, or -1 if pixel reading is not supported.
   *
   *   This is a very slow operation, and should not be used frequently.
   *}
function SDL_RenderReadPixels(renderer: PSDL_Renderer; rect: PSDL_Rect; format: UInt32; pixels: Pointer; pitch: SInt32): SInt32;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_RenderReadPixels' {$ENDIF} {$ENDIF};

  {**
   *  Update the screen with rendering performed.
   *}
procedure SDL_RenderPresent(renderer: PSDL_Renderer);
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_RenderPresent' {$ENDIF} {$ENDIF};

  {**
   *  Destroy the specified texture.
   *
   *   SDL_CreateTexture()
   *   SDL_CreateTextureFromSurface()
   *}
procedure SDL_DestroyTexture(texture: PSDL_Texture);
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_DestroyTexture' {$ENDIF} {$ENDIF};

  {**
   *  Destroy the rendering context for a window and free associated
   *  textures.
   *
   *   SDL_CreateRenderer()
   *}
procedure SDL_DestroyRenderer(renderer: PSDL_Renderer);
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_DestroyRenderer' {$ENDIF} {$ENDIF};

  {**
   *  Bind the texture to the current OpenGL/ES/ES2 context for use with
   *  OpenGL instructions.
   *
   *   texture  The SDL texture to bind
   *   texw     A pointer to a float that will be filled with the texture width
   *   texh     A pointer to a float that will be filled with the texture height
   *
   *   0 on success, or -1 if the operation is not supported
   *}
function SDL_GL_BindTexture(texture: PSDL_Texture; texw: PFloat; texh: PFloat): SInt32;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_GL_BindTexture' {$ENDIF} {$ENDIF};

  {**
   *  Unbind a texture from the current OpenGL/ES/ES2 context.
   *
   *   texture  The SDL texture to unbind
   *
   *   0 on success, or -1 if the operation is not supported
   *}
function SDL_GL_UnbindTexture(texture: PSDL_Texture): SInt32;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_GL_UnbindTexture' {$ENDIF} {$ENDIF};



////////////////////////////////////////////////////////////////////////////////////////////////////////  
//////////////////////        SDL_scancode.h        ////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////  

  {**
   *  The SDL keyboard scancode representation.
   *
   *  Values of this type are used to represent keyboard keys, among other places
   *  in the SDL_Keysym.scancode key.keysym.scancode \endlink field of the
   *  SDL_Event structure.
   *
   *  The values in this enumeration are based on the USB usage page standard:
   *  http://www.usb.org/developers/devclass_docs/Hut1_12v2.pdf
   *}

const
  SDL_SCANCODE_UNKNOWN = 0;

  {**
   *  Usage page $07
   *
   *  These values are from usage page $07 (USB keyboard page).
   *}

  SDL_SCANCODE_A = 4;
  SDL_SCANCODE_B = 5;
  SDL_SCANCODE_C = 6;
  SDL_SCANCODE_D = 7;
  SDL_SCANCODE_E = 8;
  SDL_SCANCODE_F = 9;
  SDL_SCANCODE_G = 10;
  SDL_SCANCODE_H = 11;
  SDL_SCANCODE_I = 12;
  SDL_SCANCODE_J = 13;
  SDL_SCANCODE_K = 14;
  SDL_SCANCODE_L = 15;
  SDL_SCANCODE_M = 16;
  SDL_SCANCODE_N = 17;
  SDL_SCANCODE_O = 18;
  SDL_SCANCODE_P = 19;
  SDL_SCANCODE_Q = 20;
  SDL_SCANCODE_R = 21;
  SDL_SCANCODE_S = 22;
  SDL_SCANCODE_T = 23;
  SDL_SCANCODE_U = 24;
  SDL_SCANCODE_V = 25;
  SDL_SCANCODE_W = 26;
  SDL_SCANCODE_X = 27;
  SDL_SCANCODE_Y = 28;
  SDL_SCANCODE_Z = 29;

  SDL_SCANCODE_1 = 30;
  SDL_SCANCODE_2 = 31;
  SDL_SCANCODE_3 = 32;
  SDL_SCANCODE_4 = 33;
  SDL_SCANCODE_5 = 34;
  SDL_SCANCODE_6 = 35;
  SDL_SCANCODE_7 = 36;
  SDL_SCANCODE_8 = 37;
  SDL_SCANCODE_9 = 38;
  SDL_SCANCODE_0 = 39;

  SDL_SCANCODE_RETURN = 40;
  SDL_SCANCODE_ESCAPE = 41;
  SDL_SCANCODE_BACKSPACE = 42;
  SDL_SCANCODE_TAB = 43;
  SDL_SCANCODE_SPACE = 44;

  SDL_SCANCODE_MINUS = 45;
  SDL_SCANCODE_EQUALS = 46;
  SDL_SCANCODE_LEFTBRACKET = 47;
  SDL_SCANCODE_RIGHTBRACKET = 48;
  SDL_SCANCODE_BACKSLASH = 49; {**< Located at the lower left of the return
                                *   key on ISO keyboards and at the right end
                                *   of the QWERTY row on ANSI keyboards.
                                *   Produces REVERSE SOLIDUS (backslash) and
                                *   VERTICAL LINE in a US layout; REVERSE 
                                *   SOLIDUS and VERTICAL LINE in a UK Mac
                                *   layout; NUMBER SIGN and TILDE in a UK 
                                *   Windows layout; DOLLAR SIGN and POUND SIGN
                                *   in a Swiss German layout; NUMBER SIGN and
                                *   APOSTROPHE in a German layout; GRAVE
                                *   ACCENT and POUND SIGN in a French Mac 
                                *   layout; and ASTERISK and MICRO SIGN in a
                                *   French Windows layout.
                                *}
  SDL_SCANCODE_NONUSHASH = 50; {**< ISO USB keyboards actually use this code
                                *   instead of 49 for the same key; but all
                                *   OSes I've seen treat the two codes 
                                *   identically. So; as an implementor; unless
                                *   your keyboard generates both of those 
                                *   codes and your OS treats them differently;
                                *   you should generate SDL_SCANCODE_BACKSLASH
                                *   instead of this code. As a user; you
                                *   should not rely on this code because SDL
                                *   will never generate it with most (all?)
                                *   keyboards.
                                *}
  SDL_SCANCODE_SEMICOLON = 51;
  SDL_SCANCODE_APOSTROPHE = 52;
  SDL_SCANCODE_GRAVE = 53;     {**< Located in the top left corner (on both ANSI
                                *   and ISO keyboards). Produces GRAVE ACCENT and
                                *   TILDE in a US Windows layout and in US and UK
                                *   Mac layouts on ANSI keyboards; GRAVE ACCENT
                                *   and NOT SIGN in a UK Windows layout; SECTION
                                *   SIGN and PLUS-MINUS SIGN in US and UK Mac
                                *   layouts on ISO keyboards; SECTION SIGN and
                                *   DEGREE SIGN in a Swiss German layout (Mac:
                                *   only on ISO keyboards); CIRCUMFLEX ACCENT and
                                *   DEGREE SIGN in a German layout (Mac: only on
                                *   ISO keyboards); SUPERSCRIPT TWO and TILDE in a
                                *   French Windows layout; COMMERCIAL AT and
                                *   NUMBER SIGN in a French Mac layout on ISO
                                *   keyboards; and LESS-THAN SIGN and GREATER-THAN
                                *   SIGN in a Swiss German; German; or French Mac
                                *   layout on ANSI keyboards.
                                *}
  SDL_SCANCODE_COMMA = 54;
  SDL_SCANCODE_PERIOD = 55;
  SDL_SCANCODE_SLASH = 56;

  SDL_SCANCODE_CAPSLOCK = 57;

  SDL_SCANCODE_F1 = 58;
  SDL_SCANCODE_F2 = 59;
  SDL_SCANCODE_F3 = 60;
  SDL_SCANCODE_F4 = 61;
  SDL_SCANCODE_F5 = 62;
  SDL_SCANCODE_F6 = 63;
  SDL_SCANCODE_F7 = 64;
  SDL_SCANCODE_F8 = 65;
  SDL_SCANCODE_F9 = 66;
  SDL_SCANCODE_F10 = 67;
  SDL_SCANCODE_F11 = 68;
  SDL_SCANCODE_F12 = 69;

  SDL_SCANCODE_PRINTSCREEN = 70;
  SDL_SCANCODE_SCROLLLOCK = 71;
  SDL_SCANCODE_PAUSE = 72;
  SDL_SCANCODE_INSERT = 73; {**< insert on PC; help on some Mac keyboards (but
                                 does send code 73; not 117) *}
  SDL_SCANCODE_HOME = 74;
  SDL_SCANCODE_PAGEUP = 75;
  SDL_SCANCODE_DELETE = 76;
  SDL_SCANCODE_END = 77;
  SDL_SCANCODE_PAGEDOWN = 78;
  SDL_SCANCODE_RIGHT = 79;
  SDL_SCANCODE_LEFT = 80;
  SDL_SCANCODE_DOWN = 81;
  SDL_SCANCODE_UP = 82;

  SDL_SCANCODE_NUMLOCKCLEAR = 83; {**< num lock on PC; clear on Mac keyboards
                                   *}
  SDL_SCANCODE_KP_DIVIDE = 84;
  SDL_SCANCODE_KP_MULTIPLY = 85;
  SDL_SCANCODE_KP_MINUS = 86;
  SDL_SCANCODE_KP_PLUS = 87;
  SDL_SCANCODE_KP_ENTER = 88;
  SDL_SCANCODE_KP_1 = 89;
  SDL_SCANCODE_KP_2 = 90;
  SDL_SCANCODE_KP_3 = 91;
  SDL_SCANCODE_KP_4 = 92;
  SDL_SCANCODE_KP_5 = 93;
  SDL_SCANCODE_KP_6 = 94;
  SDL_SCANCODE_KP_7 = 95;
  SDL_SCANCODE_KP_8 = 96;
  SDL_SCANCODE_KP_9 = 97;
  SDL_SCANCODE_KP_0 = 98;
  SDL_SCANCODE_KP_PERIOD = 99;

  SDL_SCANCODE_NONUSBACKSLASH = 100; {**< This is the additional key that ISO
                                      *   keyboards have over ANSI ones; 
                                      *   located between left shift and Y. 
                                      *   Produces GRAVE ACCENT and TILDE in a
                                      *   US or UK Mac layout; REVERSE SOLIDUS
                                      *   (backslash) and VERTICAL LINE in a 
                                      *   US or UK Windows layout; and 
                                      *   LESS-THAN SIGN and GREATER-THAN SIGN
                                      *   in a Swiss German; German; or French
                                      *   layout. *}
  SDL_SCANCODE_APPLICATION = 101;    {**< windows contextual menu; compose *}
  SDL_SCANCODE_POWER = 102;          {**< The USB document says this is a status flag;
                                       *  not a physical key - but some Mac keyboards
                                       *  do have a power key. *}
  SDL_SCANCODE_KP_EQUALS = 103;
  SDL_SCANCODE_F13 = 104;
  SDL_SCANCODE_F14 = 105;
  SDL_SCANCODE_F15 = 106;
  SDL_SCANCODE_F16 = 107;
  SDL_SCANCODE_F17 = 108;
  SDL_SCANCODE_F18 = 109;
  SDL_SCANCODE_F19 = 110;
  SDL_SCANCODE_F20 = 111;
  SDL_SCANCODE_F21 = 112;
  SDL_SCANCODE_F22 = 113;
  SDL_SCANCODE_F23 = 114;
  SDL_SCANCODE_F24 = 115;
  SDL_SCANCODE_EXECUTE = 116;
  SDL_SCANCODE_HELP = 117;
  SDL_SCANCODE_MENU = 118;
  SDL_SCANCODE_SELECT = 119;
  SDL_SCANCODE_STOP = 120;
  SDL_SCANCODE_AGAIN = 121;   {**< redo *}
  SDL_SCANCODE_UNDO = 122;
  SDL_SCANCODE_CUT = 123;
  SDL_SCANCODE_COPY = 124;
  SDL_SCANCODE_PASTE = 125;
  SDL_SCANCODE_FIND = 126;
  SDL_SCANCODE_MUTE = 127;
  SDL_SCANCODE_VOLUMEUP = 128;
  SDL_SCANCODE_VOLUMEDOWN = 129;
  {* not sure whether there's a reason to enable these *}
  {*     SDL_SCANCODE_LOCKINGCAPSLOCK = 130;  *}
  {*     SDL_SCANCODE_LOCKINGNUMLOCK = 131; *}
  {*     SDL_SCANCODE_LOCKINGSCROLLLOCK = 132; *}
  SDL_SCANCODE_KP_COMMA = 133;
  SDL_SCANCODE_KP_EQUALSAS400 = 134;

  SDL_SCANCODE_INTERNATIONAL1 = 135; {**< used on Asian keyboards; see footnotes in USB doc *}
  SDL_SCANCODE_INTERNATIONAL2 = 136;
  SDL_SCANCODE_INTERNATIONAL3 = 137; {**< Yen *}
  SDL_SCANCODE_INTERNATIONAL4 = 138;
  SDL_SCANCODE_INTERNATIONAL5 = 139;
  SDL_SCANCODE_INTERNATIONAL6 = 140;
  SDL_SCANCODE_INTERNATIONAL7 = 141;
  SDL_SCANCODE_INTERNATIONAL8 = 142;
  SDL_SCANCODE_INTERNATIONAL9 = 143;
  SDL_SCANCODE_LANG1 = 144; {**< Hangul{English toggle *}
  SDL_SCANCODE_LANG2 = 145; {**< Hanja conversion *}
  SDL_SCANCODE_LANG3 = 146; {**< Katakana *}
  SDL_SCANCODE_LANG4 = 147; {**< Hiragana *}
  SDL_SCANCODE_LANG5 = 148; {**< Zenkaku{Hankaku *}
  SDL_SCANCODE_LANG6 = 149; {**< reserved *}
  SDL_SCANCODE_LANG7 = 150; {**< reserved *}
  SDL_SCANCODE_LANG8 = 151; {**< reserved *}
  SDL_SCANCODE_LANG9 = 152; {**< reserved *}

  SDL_SCANCODE_ALTERASE = 153; {**< Erase-Eaze *}
  SDL_SCANCODE_SYSREQ = 154;
  SDL_SCANCODE_CANCEL = 155;
  SDL_SCANCODE_CLEAR = 156;
  SDL_SCANCODE_PRIOR = 157;
  SDL_SCANCODE_RETURN2 = 158;
  SDL_SCANCODE_SEPARATOR = 159;
  SDL_SCANCODE_OUT = 160;
  SDL_SCANCODE_OPER = 161;
  SDL_SCANCODE_CLEARAGAIN = 162;
  SDL_SCANCODE_CRSEL = 163;
  SDL_SCANCODE_EXSEL = 164;

  SDL_SCANCODE_KP_00 = 176;
  SDL_SCANCODE_KP_000 = 177;
  SDL_SCANCODE_THOUSANDSSEPARATOR = 178;
  SDL_SCANCODE_DECIMALSEPARATOR = 179;
  SDL_SCANCODE_CURRENCYUNIT = 180;
  SDL_SCANCODE_CURRENCYSUBUNIT = 181;
  SDL_SCANCODE_KP_LEFTPAREN = 182;
  SDL_SCANCODE_KP_RIGHTPAREN = 183;
  SDL_SCANCODE_KP_LEFTBRACE = 184;
  SDL_SCANCODE_KP_RIGHTBRACE = 185;
  SDL_SCANCODE_KP_TAB = 186;
  SDL_SCANCODE_KP_BACKSPACE = 187;
  SDL_SCANCODE_KP_A = 188;
  SDL_SCANCODE_KP_B = 189;
  SDL_SCANCODE_KP_C = 190;
  SDL_SCANCODE_KP_D = 191;
  SDL_SCANCODE_KP_E = 192;
  SDL_SCANCODE_KP_F = 193;
  SDL_SCANCODE_KP_XOR = 194;
  SDL_SCANCODE_KP_POWER = 195;
  SDL_SCANCODE_KP_PERCENT = 196;
  SDL_SCANCODE_KP_LESS = 197;
  SDL_SCANCODE_KP_GREATER = 198;
  SDL_SCANCODE_KP_AMPERSAND = 199;
  SDL_SCANCODE_KP_DBLAMPERSAND = 200;
  SDL_SCANCODE_KP_VERTICALBAR = 201;
  SDL_SCANCODE_KP_DBLVERTICALBAR = 202;
  SDL_SCANCODE_KP_COLON = 203;
  SDL_SCANCODE_KP_HASH = 204;
  SDL_SCANCODE_KP_SPACE = 205;
  SDL_SCANCODE_KP_AT = 206;
  SDL_SCANCODE_KP_EXCLAM = 207;
  SDL_SCANCODE_KP_MEMSTORE = 208;
  SDL_SCANCODE_KP_MEMRECALL = 209;
  SDL_SCANCODE_KP_MEMCLEAR = 210;
  SDL_SCANCODE_KP_MEMADD = 211;
  SDL_SCANCODE_KP_MEMSUBTRACT = 212;
  SDL_SCANCODE_KP_MEMMULTIPLY = 213;
  SDL_SCANCODE_KP_MEMDIVIDE = 214;
  SDL_SCANCODE_KP_PLUSMINUS = 215;
  SDL_SCANCODE_KP_CLEAR = 216;
  SDL_SCANCODE_KP_CLEARENTRY = 217;
  SDL_SCANCODE_KP_BINARY = 218;
  SDL_SCANCODE_KP_OCTAL = 219;
  SDL_SCANCODE_KP_DECIMAL = 220;
  SDL_SCANCODE_KP_HEXADECIMAL = 221;

  SDL_SCANCODE_LCTRL = 224;
  SDL_SCANCODE_LSHIFT = 225;
  SDL_SCANCODE_LALT = 226; {**< alt; option *}
  SDL_SCANCODE_LGUI = 227; {**< windows; command (apple); meta *}
  SDL_SCANCODE_RCTRL = 228;
  SDL_SCANCODE_RSHIFT = 229;
  SDL_SCANCODE_RALT = 230; {**< alt gr; option *}
  SDL_SCANCODE_RGUI = 231; {**< windows; command (apple); meta *}

  SDL_SCANCODE_MODE = 257;    {**< I'm not sure if this is really not covered 
                               *   by any of the above; but since there's a 
                               *   special KMOD_MODE for it I'm adding it here
                               *}
    
  {*Usage page $07*}

  {**
   *  Usage page $0C
   *
   *  These values are mapped from usage page $0C (USB consumer page).
   *}

  SDL_SCANCODE_AUDIONEXT = 258;
  SDL_SCANCODE_AUDIOPREV = 259;
  SDL_SCANCODE_AUDIOSTOP = 260;
  SDL_SCANCODE_AUDIOPLAY = 261;
  SDL_SCANCODE_AUDIOMUTE = 262;
  SDL_SCANCODE_MEDIASELECT = 263;
  SDL_SCANCODE_WWW = 264;
  SDL_SCANCODE_MAIL = 265;
  SDL_SCANCODE_CALCULATOR = 266;
  SDL_SCANCODE_COMPUTER = 267;
  SDL_SCANCODE_AC_SEARCH = 268;
  SDL_SCANCODE_AC_HOME = 269;
  SDL_SCANCODE_AC_BACK = 270;
  SDL_SCANCODE_AC_FORWARD = 271;
  SDL_SCANCODE_AC_STOP = 272;
  SDL_SCANCODE_AC_REFRESH = 273;
  SDL_SCANCODE_AC_BOOKMARKS = 274;

  {*Usage page $0C*}

  {**
   *  Walther keys
   *
   *  These are values that Christian Walther added (for mac keyboard?).
   *}

  SDL_SCANCODE_BRIGHTNESSDOWN = 275;
  SDL_SCANCODE_BRIGHTNESSUP = 276;
  SDL_SCANCODE_DISPLAYSWITCH = 277; {**< display mirroring{dual display
                                         switch; video mode switch *}
  SDL_SCANCODE_KBDILLUMTOGGLE = 278;
  SDL_SCANCODE_KBDILLUMDOWN = 279;
  SDL_SCANCODE_KBDILLUMUP = 280;
  SDL_SCANCODE_EJECT = 281;
  SDL_SCANCODE_SLEEP = 282;

	SDL_SCANCODE_APP1 = 283;
	SDL_SCANCODE_APP2 = 284;

  {*Walther keys*}

  {* Add any other keys here. *}

  SDL_NUM_SCANCODES = 512; {**< not a key, just marks the number of scancodes
                               for array bounds *}

type
  PSDL_ScanCode = ^TSDL_ScanCode;
  TSDL_ScanCode = DWord;

  //from "sdl_keycode.h"


  {**
   *  The SDL virtual key representation.
   *
   *  Values of this type are used to represent keyboard keys using the current
   *  layout of the keyboard.  These values include Unicode values representing
   *  the unmodified character that would be generated by pressing the key, or
   *  an SDLK_* constant for those keys that do not generate characters.
   *}
  PSDL_KeyCode = ^TSDL_KeyCode;
  TSDL_KeyCode = SInt32;

const
  SDLK_SCANCODE_MASK = 1 shl 30;

  SDLK_UNKNOWN = 0;

  SDLK_RETURN = '\r';
  SDLK_ESCAPE = '\033';
  SDLK_BACKSPACE = '\b';
  SDLK_TAB = '\t';
  SDLK_SPACE = ' ';
  SDLK_EXCLAIM = '!';
  SDLK_QUOTEDBL = '"';
  SDLK_HASH = '#';
  SDLK_PERCENT = '%';
  SDLK_DOLLAR = '$';
  SDLK_AMPERSAND = '&';
  SDLK_QUOTE = '\';
  SDLK_LEFTPAREN = '(';
  SDLK_RIGHTPAREN = ')';
  SDLK_ASTERISK = '*';
  SDLK_PLUS = '+';
  SDLK_COMMA = ';';
  SDLK_MINUS = '-';
  SDLK_PERIOD = '.';
  SDLK_SLASH = '/';
  SDLK_0 = '0';
  SDLK_1 = '1';
  SDLK_2 = '2';
  SDLK_3 = '3';
  SDLK_4 = '4';
  SDLK_5 = '5';
  SDLK_6 = '6';
  SDLK_7 = '7';
  SDLK_8 = '8';
  SDLK_9 = '9';
  SDLK_COLON = ':';
  SDLK_SEMICOLON = ';';
  SDLK_LESS = '<';
  SDLK_EQUALS = '=';
  SDLK_GREATER = '>';
  SDLK_QUESTION = '?';
  SDLK_AT = '@';
  {*
     Skip uppercase letters
   *}
  SDLK_LEFTBRACKET = '[';
  SDLK_BACKSLASH = '\\';
  SDLK_RIGHTBRACKET = ']';
  SDLK_CARET = '^';
  SDLK_UNDERSCORE = '_';
  SDLK_BACKQUOTE = '`';
  SDLK_a = 'a';
  SDLK_b = 'b';
  SDLK_c = 'c';
  SDLK_d = 'd';
  SDLK_e = 'e';
  SDLK_f = 'f';
  SDLK_g = 'g';
  SDLK_h = 'h';
  SDLK_i = 'i';
  SDLK_j = 'j';
  SDLK_k = 'k';
  SDLK_l = 'l';
  SDLK_m = 'm';
  SDLK_n = 'n';
  SDLK_o = 'o';
  SDLK_p = 'p';
  SDLK_q = 'q';
  SDLK_r = 'r';
  SDLK_s = 's';
  SDLK_t = 't';
  SDLK_u = 'u';
  SDLK_v = 'v';
  SDLK_w = 'w';
  SDLK_x = 'x';
  SDLK_y = 'y';
  SDLK_z = 'z';

  SDLK_CAPSLOCK = SDL_SCANCODE_CAPSLOCK or SDLK_SCANCODE_MASK;

  SDLK_F1 = SDL_SCANCODE_F1 or SDLK_SCANCODE_MASK;
  SDLK_F2 = SDL_SCANCODE_F2 or SDLK_SCANCODE_MASK;
  SDLK_F3 = SDL_SCANCODE_F3 or SDLK_SCANCODE_MASK;
  SDLK_F4 = SDL_SCANCODE_F4 or SDLK_SCANCODE_MASK;
  SDLK_F5 = SDL_SCANCODE_F5 or SDLK_SCANCODE_MASK;
  SDLK_F6 = SDL_SCANCODE_F6 or SDLK_SCANCODE_MASK;
  SDLK_F7 = SDL_SCANCODE_F7 or SDLK_SCANCODE_MASK;
  SDLK_F8 = SDL_SCANCODE_F8 or SDLK_SCANCODE_MASK;
  SDLK_F9 = SDL_SCANCODE_F9 or SDLK_SCANCODE_MASK;
  SDLK_F10 = SDL_SCANCODE_F10 or SDLK_SCANCODE_MASK;
  SDLK_F11 = SDL_SCANCODE_F11 or SDLK_SCANCODE_MASK;
  SDLK_F12 = SDL_SCANCODE_F12 or SDLK_SCANCODE_MASK;

  SDLK_PRINTSCREEN = SDL_SCANCODE_PRINTSCREEN or SDLK_SCANCODE_MASK;
  SDLK_SCROLLLOCK = SDL_SCANCODE_SCROLLLOCK or SDLK_SCANCODE_MASK;
  SDLK_PAUSE = SDL_SCANCODE_PAUSE or SDLK_SCANCODE_MASK;
  SDLK_INSERT = SDL_SCANCODE_INSERT or SDLK_SCANCODE_MASK;
  SDLK_HOME = SDL_SCANCODE_HOME or SDLK_SCANCODE_MASK;
  SDLK_PAGEUP = SDL_SCANCODE_PAGEUP or SDLK_SCANCODE_MASK;
  SDLK_DELETE = '\177';
  SDLK_END = SDL_SCANCODE_END or SDLK_SCANCODE_MASK;
  SDLK_PAGEDOWN = SDL_SCANCODE_PAGEDOWN or SDLK_SCANCODE_MASK;
  SDLK_RIGHT = SDL_SCANCODE_RIGHT or SDLK_SCANCODE_MASK;
  SDLK_LEFT = SDL_SCANCODE_LEFT or SDLK_SCANCODE_MASK;
  SDLK_DOWN = SDL_SCANCODE_DOWN or SDLK_SCANCODE_MASK;
  SDLK_UP = SDL_SCANCODE_UP or SDLK_SCANCODE_MASK;

  SDLK_NUMLOCKCLEAR = SDL_SCANCODE_NUMLOCKCLEAR or SDLK_SCANCODE_MASK;
  SDLK_KP_DIVIDE = SDL_SCANCODE_KP_DIVIDE or SDLK_SCANCODE_MASK;
  SDLK_KP_MULTIPLY = SDL_SCANCODE_KP_MULTIPLY or SDLK_SCANCODE_MASK;
  SDLK_KP_MINUS = SDL_SCANCODE_KP_MINUS or SDLK_SCANCODE_MASK;
  SDLK_KP_PLUS = SDL_SCANCODE_KP_PLUS or SDLK_SCANCODE_MASK;
  SDLK_KP_ENTER = SDL_SCANCODE_KP_ENTER or SDLK_SCANCODE_MASK;
  SDLK_KP_1 = SDL_SCANCODE_KP_1 or SDLK_SCANCODE_MASK;
  SDLK_KP_2 = SDL_SCANCODE_KP_2 or SDLK_SCANCODE_MASK;
  SDLK_KP_3 = SDL_SCANCODE_KP_3 or SDLK_SCANCODE_MASK;
  SDLK_KP_4 = SDL_SCANCODE_KP_4 or SDLK_SCANCODE_MASK;
  SDLK_KP_5 = SDL_SCANCODE_KP_5 or SDLK_SCANCODE_MASK;
  SDLK_KP_6 = SDL_SCANCODE_KP_6 or SDLK_SCANCODE_MASK;
  SDLK_KP_7 = SDL_SCANCODE_KP_7 or SDLK_SCANCODE_MASK;
  SDLK_KP_8 = SDL_SCANCODE_KP_8 or SDLK_SCANCODE_MASK;
  SDLK_KP_9 = SDL_SCANCODE_KP_9 or SDLK_SCANCODE_MASK;
  SDLK_KP_0 = SDL_SCANCODE_KP_0 or SDLK_SCANCODE_MASK;
  SDLK_KP_PERIOD = SDL_SCANCODE_KP_PERIOD or SDLK_SCANCODE_MASK;

  SDLK_APPLICATION = SDL_SCANCODE_APPLICATION or SDLK_SCANCODE_MASK;
  SDLK_POWER = SDL_SCANCODE_POWER or SDLK_SCANCODE_MASK;
  SDLK_KP_EQUALS = SDL_SCANCODE_KP_EQUALS or SDLK_SCANCODE_MASK;
  SDLK_F13 = SDL_SCANCODE_F13 or SDLK_SCANCODE_MASK;
  SDLK_F14 = SDL_SCANCODE_F14 or SDLK_SCANCODE_MASK;
  SDLK_F15 = SDL_SCANCODE_F15 or SDLK_SCANCODE_MASK;
  SDLK_F16 = SDL_SCANCODE_F16 or SDLK_SCANCODE_MASK;
  SDLK_F17 = SDL_SCANCODE_F17 or SDLK_SCANCODE_MASK;
  SDLK_F18 = SDL_SCANCODE_F18 or SDLK_SCANCODE_MASK;
  SDLK_F19 = SDL_SCANCODE_F19 or SDLK_SCANCODE_MASK;
  SDLK_F20 = SDL_SCANCODE_F20 or SDLK_SCANCODE_MASK;
  SDLK_F21 = SDL_SCANCODE_F21 or SDLK_SCANCODE_MASK;
  SDLK_F22 = SDL_SCANCODE_F22 or SDLK_SCANCODE_MASK;
  SDLK_F23 = SDL_SCANCODE_F23 or SDLK_SCANCODE_MASK;
  SDLK_F24 = SDL_SCANCODE_F24 or SDLK_SCANCODE_MASK;
  SDLK_EXECUTE = SDL_SCANCODE_EXECUTE or SDLK_SCANCODE_MASK;
  SDLK_HELP = SDL_SCANCODE_HELP or SDLK_SCANCODE_MASK;
  SDLK_MENU = SDL_SCANCODE_MENU or SDLK_SCANCODE_MASK;
  SDLK_SELECT = SDL_SCANCODE_SELECT or SDLK_SCANCODE_MASK;
  SDLK_STOP = SDL_SCANCODE_STOP or SDLK_SCANCODE_MASK;
  SDLK_AGAIN = SDL_SCANCODE_AGAIN or SDLK_SCANCODE_MASK;
  SDLK_UNDO = SDL_SCANCODE_UNDO or SDLK_SCANCODE_MASK;
  SDLK_CUT = SDL_SCANCODE_CUT or SDLK_SCANCODE_MASK;
  SDLK_COPY = SDL_SCANCODE_COPY or SDLK_SCANCODE_MASK;
  SDLK_PASTE = SDL_SCANCODE_PASTE or SDLK_SCANCODE_MASK;
  SDLK_FIND = SDL_SCANCODE_FIND or SDLK_SCANCODE_MASK;
  SDLK_MUTE = SDL_SCANCODE_MUTE or SDLK_SCANCODE_MASK;
  SDLK_VOLUMEUP = SDL_SCANCODE_VOLUMEUP or SDLK_SCANCODE_MASK;
  SDLK_VOLUMEDOWN = SDL_SCANCODE_VOLUMEDOWN or SDLK_SCANCODE_MASK;
  SDLK_KP_COMMA = SDL_SCANCODE_KP_COMMA or SDLK_SCANCODE_MASK;
  SDLK_KP_EQUALSAS400 = SDL_SCANCODE_KP_EQUALSAS400 or SDLK_SCANCODE_MASK;

  SDLK_ALTERASE = SDL_SCANCODE_ALTERASE or SDLK_SCANCODE_MASK;
  SDLK_SYSREQ = SDL_SCANCODE_SYSREQ or SDLK_SCANCODE_MASK;
  SDLK_CANCEL = SDL_SCANCODE_CANCEL or SDLK_SCANCODE_MASK;
  SDLK_CLEAR = SDL_SCANCODE_CLEAR or SDLK_SCANCODE_MASK;
  SDLK_PRIOR = SDL_SCANCODE_PRIOR or SDLK_SCANCODE_MASK;
  SDLK_RETURN2 = SDL_SCANCODE_RETURN2 or SDLK_SCANCODE_MASK;
  SDLK_SEPARATOR = SDL_SCANCODE_SEPARATOR or SDLK_SCANCODE_MASK;
  SDLK_OUT = SDL_SCANCODE_OUT or SDLK_SCANCODE_MASK;
  SDLK_OPER = SDL_SCANCODE_OPER or SDLK_SCANCODE_MASK;
  SDLK_CLEARAGAIN = SDL_SCANCODE_CLEARAGAIN or SDLK_SCANCODE_MASK;
  SDLK_CRSEL = SDL_SCANCODE_CRSEL or SDLK_SCANCODE_MASK;
  SDLK_EXSEL = SDL_SCANCODE_EXSEL or SDLK_SCANCODE_MASK;

  SDLK_KP_00 = SDL_SCANCODE_KP_00 or SDLK_SCANCODE_MASK;
  SDLK_KP_000 = SDL_SCANCODE_KP_000 or SDLK_SCANCODE_MASK;
  SDLK_THOUSANDSSEPARATOR = SDL_SCANCODE_THOUSANDSSEPARATOR or SDLK_SCANCODE_MASK;
  SDLK_DECIMALSEPARATOR = SDL_SCANCODE_DECIMALSEPARATOR or SDLK_SCANCODE_MASK;
  SDLK_CURRENCYUNIT = SDL_SCANCODE_CURRENCYUNIT or SDLK_SCANCODE_MASK;
  SDLK_CURRENCYSUBUNIT = SDL_SCANCODE_CURRENCYSUBUNIT or SDLK_SCANCODE_MASK;
  SDLK_KP_LEFTPAREN = SDL_SCANCODE_KP_LEFTPAREN or SDLK_SCANCODE_MASK;
  SDLK_KP_RIGHTPAREN = SDL_SCANCODE_KP_RIGHTPAREN or SDLK_SCANCODE_MASK;
  SDLK_KP_LEFTBRACE = SDL_SCANCODE_KP_LEFTBRACE or SDLK_SCANCODE_MASK;
  SDLK_KP_RIGHTBRACE = SDL_SCANCODE_KP_RIGHTBRACE or SDLK_SCANCODE_MASK;
  SDLK_KP_TAB = SDL_SCANCODE_KP_TAB or SDLK_SCANCODE_MASK;
  SDLK_KP_BACKSPACE = SDL_SCANCODE_KP_BACKSPACE or SDLK_SCANCODE_MASK;
  SDLK_KP_A = SDL_SCANCODE_KP_A or SDLK_SCANCODE_MASK;
  SDLK_KP_B = SDL_SCANCODE_KP_B or SDLK_SCANCODE_MASK;
  SDLK_KP_C = SDL_SCANCODE_KP_C or SDLK_SCANCODE_MASK;
  SDLK_KP_D = SDL_SCANCODE_KP_D or SDLK_SCANCODE_MASK;
  SDLK_KP_E = SDL_SCANCODE_KP_E or SDLK_SCANCODE_MASK;
  SDLK_KP_F = SDL_SCANCODE_KP_F or SDLK_SCANCODE_MASK;
  SDLK_KP_XOR = SDL_SCANCODE_KP_XOR or SDLK_SCANCODE_MASK;
  SDLK_KP_POWER = SDL_SCANCODE_KP_POWER or SDLK_SCANCODE_MASK;
  SDLK_KP_PERCENT = SDL_SCANCODE_KP_PERCENT or SDLK_SCANCODE_MASK;
  SDLK_KP_LESS = SDL_SCANCODE_KP_LESS or SDLK_SCANCODE_MASK;
  SDLK_KP_GREATER = SDL_SCANCODE_KP_GREATER or SDLK_SCANCODE_MASK;
  SDLK_KP_AMPERSAND = SDL_SCANCODE_KP_AMPERSAND or SDLK_SCANCODE_MASK;
  SDLK_KP_DBLAMPERSAND = SDL_SCANCODE_KP_DBLAMPERSAND or SDLK_SCANCODE_MASK;
  SDLK_KP_VERTICALBAR = SDL_SCANCODE_KP_VERTICALBAR or SDLK_SCANCODE_MASK;
  SDLK_KP_DBLVERTICALBAR = SDL_SCANCODE_KP_DBLVERTICALBAR or SDLK_SCANCODE_MASK;
  SDLK_KP_COLON = SDL_SCANCODE_KP_COLON or SDLK_SCANCODE_MASK;
  SDLK_KP_HASH = SDL_SCANCODE_KP_HASH or SDLK_SCANCODE_MASK;
  SDLK_KP_SPACE = SDL_SCANCODE_KP_SPACE or SDLK_SCANCODE_MASK;
  SDLK_KP_AT = SDL_SCANCODE_KP_AT or SDLK_SCANCODE_MASK;
  SDLK_KP_EXCLAM = SDL_SCANCODE_KP_EXCLAM or SDLK_SCANCODE_MASK;
  SDLK_KP_MEMSTORE = SDL_SCANCODE_KP_MEMSTORE or SDLK_SCANCODE_MASK;
  SDLK_KP_MEMRECALL = SDL_SCANCODE_KP_MEMRECALL or SDLK_SCANCODE_MASK;
  SDLK_KP_MEMCLEAR = SDL_SCANCODE_KP_MEMCLEAR or SDLK_SCANCODE_MASK;
  SDLK_KP_MEMADD = SDL_SCANCODE_KP_MEMADD or SDLK_SCANCODE_MASK;
  SDLK_KP_MEMSUBTRACT = SDL_SCANCODE_KP_MEMSUBTRACT or SDLK_SCANCODE_MASK;
  SDLK_KP_MEMMULTIPLY = SDL_SCANCODE_KP_MEMMULTIPLY or SDLK_SCANCODE_MASK;
  SDLK_KP_MEMDIVIDE = SDL_SCANCODE_KP_MEMDIVIDE or SDLK_SCANCODE_MASK;
  SDLK_KP_PLUSMINUS = SDL_SCANCODE_KP_PLUSMINUS or SDLK_SCANCODE_MASK;
  SDLK_KP_CLEAR = SDL_SCANCODE_KP_CLEAR or SDLK_SCANCODE_MASK;
  SDLK_KP_CLEARENTRY = SDL_SCANCODE_KP_CLEARENTRY or SDLK_SCANCODE_MASK;
  SDLK_KP_BINARY = SDL_SCANCODE_KP_BINARY or SDLK_SCANCODE_MASK;
  SDLK_KP_OCTAL = SDL_SCANCODE_KP_OCTAL or SDLK_SCANCODE_MASK;
  SDLK_KP_DECIMAL = SDL_SCANCODE_KP_DECIMAL or SDLK_SCANCODE_MASK;
  SDLK_KP_HEXADECIMAL = SDL_SCANCODE_KP_HEXADECIMAL or SDLK_SCANCODE_MASK;

  SDLK_LCTRL = SDL_SCANCODE_LCTRL or SDLK_SCANCODE_MASK;
  SDLK_LSHIFT = SDL_SCANCODE_LSHIFT or SDLK_SCANCODE_MASK;
  SDLK_LALT = SDL_SCANCODE_LALT or SDLK_SCANCODE_MASK;
  SDLK_LGUI = SDL_SCANCODE_LGUI or SDLK_SCANCODE_MASK;
  SDLK_RCTRL = SDL_SCANCODE_RCTRL or SDLK_SCANCODE_MASK;
  SDLK_RSHIFT = SDL_SCANCODE_RSHIFT or SDLK_SCANCODE_MASK;
  SDLK_RALT = SDL_SCANCODE_RALT or SDLK_SCANCODE_MASK;
  SDLK_RGUI = SDL_SCANCODE_RGUI or SDLK_SCANCODE_MASK;

  SDLK_MODE = SDL_SCANCODE_MODE or SDLK_SCANCODE_MASK;

  SDLK_AUDIONEXT = SDL_SCANCODE_AUDIONEXT or SDLK_SCANCODE_MASK;
  SDLK_AUDIOPREV = SDL_SCANCODE_AUDIOPREV or SDLK_SCANCODE_MASK;
  SDLK_AUDIOSTOP = SDL_SCANCODE_AUDIOSTOP or SDLK_SCANCODE_MASK;
  SDLK_AUDIOPLAY = SDL_SCANCODE_AUDIOPLAY or SDLK_SCANCODE_MASK;
  SDLK_AUDIOMUTE = SDL_SCANCODE_AUDIOMUTE or SDLK_SCANCODE_MASK;
  SDLK_MEDIASELECT = SDL_SCANCODE_MEDIASELECT or SDLK_SCANCODE_MASK;
  SDLK_WWW = SDL_SCANCODE_WWW or SDLK_SCANCODE_MASK;
  SDLK_MAIL = SDL_SCANCODE_MAIL or SDLK_SCANCODE_MASK;
  SDLK_CALCULATOR = SDL_SCANCODE_CALCULATOR or SDLK_SCANCODE_MASK;
  SDLK_COMPUTER = SDL_SCANCODE_COMPUTER or SDLK_SCANCODE_MASK;
  SDLK_AC_SEARCH = SDL_SCANCODE_AC_SEARCH or SDLK_SCANCODE_MASK;
  SDLK_AC_HOME = SDL_SCANCODE_AC_HOME or SDLK_SCANCODE_MASK;
  SDLK_AC_BACK = SDL_SCANCODE_AC_BACK or SDLK_SCANCODE_MASK;
  SDLK_AC_FORWARD = SDL_SCANCODE_AC_FORWARD or SDLK_SCANCODE_MASK;
  SDLK_AC_STOP = SDL_SCANCODE_AC_STOP or SDLK_SCANCODE_MASK;
  SDLK_AC_REFRESH = SDL_SCANCODE_AC_REFRESH or SDLK_SCANCODE_MASK;
  SDLK_AC_BOOKMARKS = SDL_SCANCODE_AC_BOOKMARKS or SDLK_SCANCODE_MASK;

  SDLK_BRIGHTNESSDOWN = SDL_SCANCODE_BRIGHTNESSDOWN or SDLK_SCANCODE_MASK;
  SDLK_BRIGHTNESSUP = SDL_SCANCODE_BRIGHTNESSUP or SDLK_SCANCODE_MASK;
  SDLK_DISPLAYSWITCH = SDL_SCANCODE_DISPLAYSWITCH or SDLK_SCANCODE_MASK;
  SDLK_KBDILLUMTOGGLE = SDL_SCANCODE_KBDILLUMTOGGLE or SDLK_SCANCODE_MASK;
  SDLK_KBDILLUMDOWN = SDL_SCANCODE_KBDILLUMDOWN or SDLK_SCANCODE_MASK;
  SDLK_KBDILLUMUP = SDL_SCANCODE_KBDILLUMUP or SDLK_SCANCODE_MASK;
  SDLK_EJECT = SDL_SCANCODE_EJECT or SDLK_SCANCODE_MASK;
  SDLK_SLEEP = SDL_SCANCODE_SLEEP or SDLK_SCANCODE_MASK;

  {**
   *  Enumeration of valid key mods (possibly OR'd together).
   *}

  KMOD_NONE = $0000;
  KMOD_LSHIFT = $0001;
  KMOD_RSHIFT = $0002;
  KMOD_LCTRL = $0040;
  KMOD_RCTRL = $0080;
  KMOD_LALT = $0100;
  KMOD_RALT = $0200;
  KMOD_LGUI = $0400;
  KMOD_RGUI = $0800;
  KMOD_NUM = $1000;
  KMOD_CAPS = $2000;
  KMOD_MODE = $4000;
  KMOD_RESERVED = $8000;

type
  PSDL_KeyMod = ^TSDL_KeyMod;
  TSDL_KeyMod = Word;

const
  KMOD_CTRL	 = KMOD_LCTRL  or KMOD_RCTRL;
  KMOD_SHIFT = KMOD_LSHIFT or KMOD_RSHIFT;
  KMOD_ALT	 = KMOD_LALT   or KMOD_RALT;
  KMOD_GUI	 = KMOD_LGUI   or KMOD_RGUI;



////////////////////////////////////////////////////////////////////////////////////////////////////////  
//////////////////////        SDL_keyboard.h        ////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////  

type  
  {**
   *  The SDL keysym structure, used in key events.
   *}
  PSDL_Keysym = ^TSDL_Keysym;
  TSDL_Keysym = record
    scancode: TSDL_ScanCode;      // SDL physical key code - see SDL_Scancode for details
    sym: TSDL_KeyCode;            // SDL virtual key code - see SDL_Keycode for details
    _mod: UInt16;                 // current key modifiers
    unicode: UInt32;              // (deprecated) use SDL_TextInputEvent instead
  end;

  {**
   *  Get the window which currently has keyboard focus.
   *}
function SDL_GetKeyboardFocus(): PSDL_Window;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_GetKeyboardFocus' {$ENDIF} {$ENDIF};

  {**
   *  Get a snapshot of the current state of the keyboard.
   *
   *  numkeys if non-nil, receives the length of the returned array.
   *
   *  An array of key states. Indexes into this array are obtained by using SDL_Scancode values.
   *
   *}
function SDL_GetKeyboardState(numkeys: PInt): PUInt8;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_GetKeyboardState' {$ENDIF} {$ENDIF};

  {**
   *  Get the current key modifier state for the keyboard.
   *}
function SDL_GetModState(): TSDL_KeyMod;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_GetModState' {$ENDIF} {$ENDIF};

  {**
   *  Set the current key modifier state for the keyboard.
   *  
   *  This does not change the keyboard state, only the key modifier flags.
   *}
procedure SDL_SetModState(modstate: TSDL_KeyMod);
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_SetModState' {$ENDIF} {$ENDIF};

  {**
   *  Get the key code corresponding to the given scancode according
   *         to the current keyboard layout.
   *
   *  See SDL_Keycode for details.
   *  
   *  SDL_GetKeyName()
   *}
function SDL_GetKeyFromScancode(scancode: TSDL_ScanCode): TSDL_KeyCode;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_GetKeyFromScancode' {$ENDIF} {$ENDIF};

  {**
   *  Get the scancode corresponding to the given key code according to the
   *         current keyboard layout.
   *
   *  See SDL_Scancode for details.
   *
   *  SDL_GetScancodeName()
   *}
function SDL_GetScancodeFromKey(key: TSDL_KeyCode): TSDL_ScanCode;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_GetScancodeFromKey' {$ENDIF} {$ENDIF};

  {**
   *  Get a human-readable name for a scancode.
   *
   *  A pointer to the name for the scancode.
   *
   *  If the scancode doesn't have a name, this function returns
   *  an empty string ("").
   *
   *}
function SDL_GetScancodeName(scancode: TSDL_ScanCode): PChar;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_GetScancodeName' {$ENDIF} {$ENDIF};

  {**
   *  Get a scancode from a human-readable name
   *
   *  scancode, or SDL_SCANCODE_UNKNOWN if the name wasn't recognized
   *
   *  SDL_Scancode
   *}
function SDL_GetScancodeFromName(const name: PChar): TSDL_ScanCode;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_GetScancodeFromName' {$ENDIF} {$ENDIF};

  {**
   *  Get a human-readable name for a key.
   *
   *  A pointer to a UTF-8 string that stays valid at least until the next
   *  call to this function. If you need it around any longer, you must
   *  copy it.  If the key doesn't have a name, this function returns an
   *  empty string ("").
   *  
   *  SDL_Key
   *}
function SDL_GetKeyName(key: TSDL_ScanCode): PChar;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_GetKeyName' {$ENDIF} {$ENDIF};

  {**
   *  Get a key code from a human-readable name
   *
   *  key code, or SDLK_UNKNOWN if the name wasn't recognized
   *
   *  SDL_Keycode
   *}
function SDL_GetKeyFromName(const name: PChar): TSDL_KeyCode;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_GetKeyFromName' {$ENDIF} {$ENDIF};

  {**
   *  Start accepting Unicode text input events.
   *  This function will show the on-screen keyboard if supported.
   *  
   *  SDL_StopTextInput()
   *  SDL_SetTextInputRect()
   *  SDL_HasScreenKeyboardSupport()
   *}
procedure SDL_StartTextInput();
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_StartTextInput' {$ENDIF} {$ENDIF};

  {**
   *  Return whether or not Unicode text input events are enabled.
   *
   *  SDL_StartTextInput()
   *  SDL_StopTextInput()
   *}
function SDL_IsTextInputActive(): TSDL_Bool;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_IsTextInputActive' {$ENDIF} {$ENDIF};

  {**
   *  Stop receiving any text input events.
   *  This function will hide the on-screen keyboard if supported.
   *
   *  SDL_StartTextInput()
   *  SDL_HasScreenKeyboardSupport()
   *}
procedure SDL_StopTextInput();
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_StopTextInput' {$ENDIF} {$ENDIF};

  {**
   *  Set the rectangle used to type Unicode text inputs.
   *  This is used as a hint for IME and on-screen keyboard placement.
   *  
   *  SDL_StartTextInput()
   *}
procedure SDL_SetTextInputRect(rect: PSDL_Rect);
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_SetTextInputRect' {$ENDIF} {$ENDIF};

  {**
   *  Returns whether the platform has some screen keyboard support.
   *  
   *  SDL_TRUE if some keyboard support is available else SDL_FALSE.
   *
   *  Not all screen keyboard functions are supported on all platforms.
   *
   *  SDL_IsScreenKeyboardShown()
   *}
function SDL_HasScreenKeyboardSupport(): TSDL_Bool;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_HasScreenKeyboardSupport' {$ENDIF} {$ENDIF};

  {**
   *  Returns whether the screen keyboard is shown for given window.
   *
   *  window The window for which screen keyboard should be queried.
   *
   *  Result - SDL_TRUE if screen keyboard is shown else SDL_FALSE.
   *
   *  SDL_HasScreenKeyboardSupport()
   *}
function SDL_IsScreenKeyboardShown(window: PSDL_Window): TSDL_Bool;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_IsScreenKeyboardShown' {$ENDIF} {$ENDIF};



////////////////////////////////////////////////////////////////////////////////////////////////////////  
//////////////////////         SDL_mouse.h          ////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////  

type
  PSDL_Cursor = Pointer;
  
const

  {**
   *  Cursor types for SDL_CreateSystemCursor.
   *}

  SDL_SYSTEM_CURSOR_ARROW = 0;     // Arrow
  SDL_SYSTEM_CURSOR_IBEAM = 1;     // I-beam
  SDL_SYSTEM_CURSOR_WAIT = 2;      // Wait
  SDL_SYSTEM_CURSOR_CROSSHAIR = 3; // Crosshair
  SDL_SYSTEM_CURSOR_WAITARROW = 4; // Small wait cursor (or Wait if not available)
  SDL_SYSTEM_CURSOR_SIZENWSE = 5;  // Double arrow pointing northwest and southeast
  SDL_SYSTEM_CURSOR_SIZENESW = 6;  // Double arrow pointing northeast and southwest
  SDL_SYSTEM_CURSOR_SIZEWE = 7;    // Double arrow pointing west and east
  SDL_SYSTEM_CURSOR_SIZENS = 8;    // Double arrow pointing north and south
  SDL_SYSTEM_CURSOR_SIZEALL = 9;   // Four pointed arrow pointing north, south, east, and west
  SDL_SYSTEM_CURSOR_NO = 10;        // Slashed circle or crossbones
  SDL_SYSTEM_CURSOR_HAND = 11;      // Hand
  SDL_NUM_SYSTEM_CURSORS = 12;

type
  PSDL_SystemCursor = ^TSDL_SystemCursor;
  TSDL_SystemCursor = Word;

  {* Function prototypes *}

  {**
   *  Get the window which currently has mouse focus.
   *}
function SDL_GetMouseFocus(): PSDL_Window;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_GetMouseFocus' {$ENDIF} {$ENDIF};

  {**
   *  Retrieve the current state of the mouse.
   *  
   *  The current button state is returned as a button bitmask, which can
   *  be tested using the SDL_BUTTON(X) macros, and x and y are set to the
   *  mouse cursor position relative to the focus window for the currently
   *  selected mouse.  You can pass nil for either x or y.
   *
   * SDL_Button = 1 shl ((X)-1)
   *}
function SDL_GetMouseState(x: PInt; y: PInt): UInt32;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_GetMouseState' {$ENDIF} {$ENDIF};

  {**
   *  Retrieve the relative state of the mouse.
   *
   *  The current button state is returned as a button bitmask, which can
   *  be tested using the SDL_BUTTON(X) macros, and x and y are set to the
   *  mouse deltas since the last call to SDL_GetRelativeMouseState().
   *}
function SDL_GetRelativeMouseState(x: PInt; y: PInt): UInt32;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_GetRelativeMouseState' {$ENDIF} {$ENDIF};

  {**
   *  Moves the mouse to the given position within the window.
   *
   *   window The window to move the mouse into, or nil for the current mouse focus
   *   x The x coordinate within the window
   *   y The y coordinate within the window
   *
   *  This function generates a mouse motion event
   *}
procedure SDL_WarpMouseInWindow(window: PSDL_Window; x: SInt32; y: SInt32);
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_WarpMouseInWindow' {$ENDIF} {$ENDIF};

  {**
   *  Set relative mouse mode.
   *
   *  enabled Whether or not to enable relative mode
   *
   *  0 on success, or -1 if relative mode is not supported.
   *
   *  While the mouse is in relative mode, the cursor is hidden, and the
   *  driver will try to report continuous motion in the current window.
   *  Only relative motion events will be delivered, the mouse position
   *  will not change.
   *  
   *  This function will flush any pending mouse motion.
   *  
   *  SDL_GetRelativeMouseMode()
   *}
function SDL_SetRelativeMouseMode(enabled: TSDL_Bool): SInt32;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_SetRelativeMouseMode' {$ENDIF} {$ENDIF};

  {**
   *  Query whether relative mouse mode is enabled.
   *  
   *  SDL_SetRelativeMouseMode()
   *}
function SDL_GetRelativeMouseMode(): TSDL_Bool;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_GetRelativeMouseMode' {$ENDIF} {$ENDIF};

  {**
   *  Create a cursor, using the specified bitmap data and
   *  mask (in MSB format).
   *
   *  The cursor width must be a multiple of 8 bits.
   *
   *  The cursor is created in black and white according to the following:
   *  <table>
   *  <tr><td> data </td><td> mask </td><td> resulting pixel on screen </td></tr>
   *  <tr><td>  0   </td><td>  1   </td><td> White </td></tr>
   *  <tr><td>  1   </td><td>  1   </td><td> Black </td></tr>
   *  <tr><td>  0   </td><td>  0   </td><td> Transparent </td></tr>
   *  <tr><td>  1   </td><td>  0   </td><td> Inverted color if possible, black 
   *                                         if not. </td></tr>
   *  </table>
   *  
   *  SDL_FreeCursor()
   *}
function SDL_CreateCursor(const data: PUInt8; const mask: PUInt8; w: SInt32; h: SInt32; hot_x: SInt32; hot_y: SInt32): PSDL_Cursor;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_CreateCursor' {$ENDIF} {$ENDIF};

  {**
   *  Create a color cursor.
   *
   *  SDL_FreeCursor()
   *}
function SDL_CreateColorCursor(surface: PSDL_Surface; hot_x: SInt32; hot_y: SInt32): PSDL_Cursor;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_CreateColorCursor' {$ENDIF} {$ENDIF};

  {**
   *  Create a system cursor.
   *
   *  SDL_FreeCursor()
   *}
function SDL_CreateSystemCursor(id: TSDL_SystemCursor): PSDL_Cursor;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_CreateSystemCursor' {$ENDIF} {$ENDIF};

  {**
   *  Set the active cursor.
   *}
procedure SDL_SetCursor(cursor: PSDL_Cursor);
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_SetCursor' {$ENDIF} {$ENDIF};

  {**
   *  Return the active cursor.
   *}
function SDL_GetCursor(): PSDL_Cursor;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_GetCursor' {$ENDIF} {$ENDIF};

  {**
   *  Frees a cursor created with SDL_CreateCursor().
   *
   *  SDL_CreateCursor()
   *}
procedure SDL_FreeCursor(cursor: PSDL_Cursor);
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_FreeCursor' {$ENDIF} {$ENDIF};

  {**
   *  Toggle whether or not the cursor is shown.
   *
   *  toggle 1 to show the cursor, 0 to hide it, -1 to query the current
   *                state.
   *  
   *  1 if the cursor is shown, or 0 if the cursor is hidden.
   *}
function SDL_ShowCursor(toggle: SInt32): SInt32;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_ShowCursor' {$ENDIF} {$ENDIF};


const
  {**
   *  Used as a mask when testing buttons in buttonstate.
   *   - Button 1:  Left mouse button
   *   - Button 2:  Middle mouse button
   *   - Button 3:  Right mouse button
   *}

  SDL_BUTTON_LEFT	= 1;
  SDL_BUTTON_MIDDLE	= 2;
  SDL_BUTTON_RIGHT	= 3;
  SDL_BUTTON_X1	    = 4;
  SDL_BUTTON_X2	    = 5;
  SDL_BUTTON_LMASK  = 1 shl ((SDL_BUTTON_LEFT) - 1);
  SDL_BUTTON_MMASK  = 1 shl ((SDL_BUTTON_MIDDLE) - 1);
  SDL_BUTTON_RMASK  = 1 shl ((SDL_BUTTON_RIGHT) - 1);
  SDL_BUTTON_X1MASK = 1 shl ((SDL_BUTTON_X1) - 1);
  SDL_BUTTON_X2MASK = 1 shl ((SDL_BUTTON_X2) - 1);



////////////////////////////////////////////////////////////////////////////////////////////////////////  
//////////////////////        SDL_joystick.h        ////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////  

  {**
   *  SDL_joystick.h
   *
   *  In order to use these functions, SDL_Init() must have been called
   *  with the ::SDL_INIT_JOYSTICK flag.  This causes SDL to scan the system
   *  for joysticks, and load appropriate drivers.
   *}

type

  {* The joystick structure used to identify an SDL joystick *}
  PSDL_Joystick = Pointer; // todo!!

{* A structure that encodes the stable unique id for a joystick device *}

  TSDL_JoystickGUID = record
    data: array[0..15] of UInt8;
  end;

  TSDL_JoystickID = SInt32;

  {* Function prototypes *}
  {**
   *  Count the number of joysticks attached to the system right now
   *}
function SDL_NumJoysticks(): SInt32;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_NumJoysticks' {$ENDIF} {$ENDIF};

  {**
   *  Get the implementation dependent name of a joystick.
   *  This can be called before any joysticks are opened.
   *  If no name can be found, this function returns NULL.
   *}
function SDL_JoystickNameForIndex(device_index: SInt32): PChar;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_JoystickNameForIndex' {$ENDIF} {$ENDIF};

  {**
   *  Open a joystick for use.
   *  The index passed as an argument refers tothe N'th joystick on the system.
   *  This index is the value which will identify this joystick in future joystick
   *  events.
   *
   *  A joystick identifier, or NULL if an error occurred.
   *}
function SDL_JoystickOpen(device_index: SInt32): PSDL_Joystick;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_JoystickOpen' {$ENDIF} {$ENDIF};

  {**
   *  Return the name for this currently opened joystick.
   *  If no name can be found, this function returns NULL.
   *}
function SDL_JoystickName(joystick: PSDL_Joystick): PChar;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_JoystickName' {$ENDIF} {$ENDIF};

  {**
   *  Return the GUID for the joystick at this index
   *}
function SDL_JoystickGetDeviceGUID(device_index: SInt32): TSDL_JoystickGUID;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_JoystickGetDeviceGUID' {$ENDIF} {$ENDIF};

  {**
   *  Return the GUID for this opened joystick
   *}
function SDL_JoystickGetGUID(joystick: PSDL_Joystick): TSDL_JoystickGUID;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_JoystickGetGUID' {$ENDIF} {$ENDIF};

  {**
   *  Return a string representation for this guid. pszGUID must point to at least 33 bytes
   *  (32 for the string plus a NULL terminator).
   *}
procedure SDL_JoystickGetGUIDString(guid: TSDL_JoystickGUId; pszGUID: PChar; cbGUID: SInt32);
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_JoystickGetGUIDString' {$ENDIF} {$ENDIF};

  {**
   *  convert a string into a joystick formatted guid
   *}
function SDL_JoystickGetGUIDFromString(const pchGUID: PChar): TSDL_JoystickGUID;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_JoystickGetGUIDFromString' {$ENDIF} {$ENDIF};

  {**
   *  Returns SDL_TRUE if the joystick has been opened and currently connected, or SDL_FALSE if it has not.
   *}
function SDL_JoystickGetAttached(joystick: PSDL_Joystick): TSDL_Bool;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_JoystickGetAttached' {$ENDIF} {$ENDIF};

  {**
   *  Get the instance ID of an opened joystick or -1 if the joystick is invalid.
   *}
function SDL_JoystickInstanceID(joystick: PSDL_Joystick): TSDL_JoystickID;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_JoystickInstanceID' {$ENDIF} {$ENDIF};

  {**
   *  Get the number of general axis controls on a joystick.
   *}
function SDL_JoystickNumAxes(joystick: PSDL_Joystick): SInt32;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_JoystickNumAxes' {$ENDIF} {$ENDIF};

  {**
   *  Get the number of trackballs on a joystick.
   *
   *  Joystick trackballs have only relative motion events associated
   *  with them and their state cannot be polled.
   *}
function SDL_JoystickNumBalls(joystick: PSDL_Joystick): SInt32;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_JoystickNumBalls' {$ENDIF} {$ENDIF};

  {**
   *  Get the number of POV hats on a joystick.
   *}
function SDL_JoystickNumHats(joystick: PSDL_Joystick): SInt32;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_JoystickNumHats' {$ENDIF} {$ENDIF};

  {**
   *  Get the number of buttons on a joystick.
   *}
function SDL_JoystickNumButtons(joystick: PSDL_Joystick): SInt32;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_JoystickNumButtons' {$ENDIF} {$ENDIF};

  {**
   *  Update the current state of the open joysticks.
   *
   *  This is called automatically by the event loop if any joystick
   *  events are enabled.
   *}
procedure SDL_JoystickUpdate();
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_JoystickUpdate' {$ENDIF} {$ENDIF};

  {**
   *  Enable/disable joystick event polling.
   *
   *  If joystick events are disabled, you must call SDL_JoystickUpdate()
   *  yourself and check the state of the joystick when you want joystick
   *  information.
   *
   *  The state can be one of ::SDL_QUERY, ::SDL_ENABLE or ::SDL_IGNORE.
   *}
function SDL_JoystickEventState(state: SInt32): SInt32;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_JoystickEventState' {$ENDIF} {$ENDIF};

  {**
   *  Get the current state of an axis control on a joystick.
   *
   *  The state is a value ranging from -32768 to 32767.
   *
   *  The axis indices start at index 0.
   *}
function SDL_JoystickGetAxis(joystick: PSDL_Joystick; axis: SInt32): SInt16;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_JoystickGetAxis' {$ENDIF} {$ENDIF};

  {**
   *  Hat positions
   *}
const
  SDL_HAT_CENTERED  = $00;
  SDL_HAT_UP        = $01;
  SDL_HAT_RIGHT     = $02;
  SDL_HAT_DOWN      = $04;
  SDL_HAT_LEFT      = $08;
  SDL_HAT_RIGHTUP   = SDL_HAT_RIGHT or SDL_HAT_UP;
  SDL_HAT_RIGHTDOWN = SDL_HAT_RIGHT or SDL_HAT_DOWN;
  SDL_HAT_LEFTUP    = SDL_HAT_LEFT or SDL_HAT_UP;
  SDL_HAT_LEFTDOWN  = SDL_HAT_LEFT or SDL_HAT_DOWN;

  {**
   *  Get the current state of a POV hat on a joystick.
   *
   *  The hat indices start at index 0.
   *
   *  The return value is one of the following positions:
   *   - SDL_HAT_CENTERED
   *   - SDL_HAT_UP
   *   - SDL_HAT_RIGHT
   *   - SDL_HAT_DOWN
   *   - SDL_HAT_LEFT
   *   - SDL_HAT_RIGHTUP
   *   - SDL_HAT_RIGHTDOWN
   *   - SDL_HAT_LEFTUP
   *   - SDL_HAT_LEFTDOWN
   *}
function SDL_JoystickGetHat(joystick: PSDL_Joystick; hat: SInt32): UInt8;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_JoystickGetHat' {$ENDIF} {$ENDIF};

  {**
   *  Get the ball axis change since the last poll.
   *
   *  0, or -1 if you passed it invalid parameters.
   *
   *  The ball indices start at index 0.
   *}
function SDL_JoystickGetBall(joystick: PSDL_Joystick; ball: SInt32; dx: PInt; dy: PInt): SInt32;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_JoystickGetBall' {$ENDIF} {$ENDIF};

  {**
   *  Get the current state of a button on a joystick.
   *
   *  The button indices start at index 0.
   *}
function SDL_JoystickGetButton(joystick: PSDL_Joystick; button: SInt32): UInt8;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_JoystickGetButton' {$ENDIF} {$ENDIF};
  {**
   *  Close a joystick previously opened with SDL_JoystickOpen().
   *}
procedure SDL_JoystickClose(joystick: PSDL_Joystick);
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_JoystickClose' {$ENDIF} {$ENDIF};



////////////////////////////////////////////////////////////////////////////////////////////////////////
//////////////////////         SDL_gamecontroller.h          ////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////


{**
 *   SDL_gamecontroller.h
 *
 *  In order to use these functions, SDL_Init() must have been called
 *  with the ::SDL_INIT_JOYSTICK flag.  This causes SDL to scan the system
 *  for game controllers, and load appropriate drivers.
 *
 *  If you would like to receive controller updates while the application
 *  is in the background, you should set the following hint before calling
 *  SDL_Init(): SDL_HINT_JOYSTICK_ALLOW_BACKGROUND_EVENTS
 *}


{* The gamecontroller structure used to identify an SDL game controller *}
type
  PSDL_GameController = ^TSDL_GameController;
  TSDL_GameController = Pointer; //todo


  TSDL_GameControllerBindType = (SDL_CONTROLLER_BINDTYPE_NONE,
                                 SDL_CONTROLLER_BINDTYPE_BUTTON,
                                 SDL_CONTROLLER_BINDTYPE_AXIS,
                                 SDL_CONTROLLER_BINDTYPE_HAT);


  {**
   *  Get the SDL joystick layer binding for this controller button/axis mapping
   *}
  THat = record
    hat: Integer;
    hat_mask: Integer;
  end;


  TSDL_GameControllerButtonBind = record
    bindType: TSDL_GameControllerBindType;
    case Integer of
      0: ( button: Integer; );
      1: ( axis: Integer; );
      2: ( hat: THat; );
  end;


  {**
   *  To count the number of game controllers in the system for the following:
   *  int nJoysticks = SDL_NumJoysticks();
   *  int nGameControllers = 0;
   *  for ( int i = 0; i < nJoysticks; i++ ) {
   *      if ( SDL_IsGameController(i) ) {
   *          nGameControllers++;
   *
   *
   *
   *  Using the SDL_HINT_GAMECONTROLLERCONFIG hint or the SDL_GameControllerAddMapping you can add support for controllers SDL is unaware of or cause an existing controller to have a different binding. The format is:
   *  guid,name,mappings
   *
   *  Where GUID is the string value from SDL_JoystickGetGUIDString(), name is the human readable string for the device and mappings are controller mappings to joystick ones.
   *  Under Windows there is a reserved GUID of "xinput" that covers any XInput devices.
   *  The mapping format for joystick is:
   *      bX - a joystick button, index X
   *      hX.Y - hat X with value Y
   *      aX - axis X of the joystick
   *  Buttons can be used as a controller axis and vice versa.
   *
   *  This string shows an example of a valid mapping for a controller
   *  "341a3608000000000000504944564944,Afterglow PS3 Controller,a:b1,b:b2,y:b3,x:b0,start:b9,guide:b12,back:b8,dpup:h0.1,dpleft:h0.8,dpdown:h0.4,dpright:h0.2,leftshoulder:b4,rightshoulder:b5,leftstick:b10,rightstick:b11,leftx:a0,lefty:a1,rightx:a2,righty:a3,lefttrigger:b6,righttrigger:b7",
   *
   *}


  {**
   *  Add or update an existing mapping configuration
   *
   *  1 if mapping is added, 0 if updated, -1 on error
   *}
function SDL_GameControllerAddMapping( mappingString: PChar ): Integer;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_GameControllerAddMapping' {$ENDIF} {$ENDIF};


  {**
   *  Get a mapping string for a GUID
   *
   *   the mapping string.  Must be freed with SDL_free.  Returns NULL if no mapping is available
   *}
function SDL_GameControllerMappingForGUID( guid: TSDL_JoystickGUID ): PChar;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_GameControllerMappingForGUID' {$ENDIF} {$ENDIF};


  {**
   *  Get a mapping string for an open GameController
   *
   *   the mapping string.  Must be freed with SDL_free.  Returns NULL if no mapping is available
   *}
function SDL_GameControllerMapping( gamecontroller: PSDL_GameController ): PChar;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_GameControllerMapping' {$ENDIF} {$ENDIF};


  {**
   *  Is the joystick on this index supported by the game controller interface?
   *}
function SDL_IsGameController(joystick_index: Integer): TSDL_Bool;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_IsGameController' {$ENDIF} {$ENDIF};


  {**
   *  Get the implementation dependent name of a game controller.
   *  This can be called before any controllers are opened.
   *  If no name can be found, this function returns NULL.
   *}
function SDL_GameControllerNameForIndex(joystick_index: Integer): PChar;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS}name '_SDL_GameControllerNameForIndex' {$ENDIF} {$ENDIF};


  {**
   *  Open a game controller for use.
   *  The index passed as an argument refers to the N'th game controller on the system.
   *  This index is the value which will identify this controller in future controller
   *  events.
   *
   *   A controller identifier, or NULL if an error occurred.
   *}
function SDL_GameControllerOpen(joystick_index: Integer): PSDL_GameController;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_GameControllerOpen' {$ENDIF} {$ENDIF};


  {**
   *  Return the name for this currently opened controller
   *}
function SDL_GameControllerName(gamecontroller: PSDL_GameController): PChar;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_GameControllerName' {$ENDIF} {$ENDIF};


  {**
   *  Returns SDL_TRUE if the controller has been opened and currently connected,
   *  or SDL_FALSE if it has not.
   *}
function SDL_GameControllerGetAttached(gamecontroller: PSDL_GameController): TSDL_Bool;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_GameControllerGetAttached' {$ENDIF} {$ENDIF};


  {**
   *  Get the underlying joystick object used by a controller
   *}
function SDL_GameControllerGetJoystick(gamecontroller: PSDL_GameController): PSDL_Joystick;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_GameControllerGetJoystick' {$ENDIF} {$ENDIF};


  {**
   *  Enable/disable controller event polling.
   *
   *  If controller events are disabled, you must call SDL_GameControllerUpdate()
   *  yourself and check the state of the controller when you want controller
   *  information.
   *
   *  The state can be one of ::SDL_QUERY, ::SDL_ENABLE or ::SDL_IGNORE.
   *}
function SDL_GameControllerEventState(state: Integer): Integer;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_GameControllerEventState' {$ENDIF} {$ENDIF};


  {**
   *  Update the current state of the open game controllers.
   *
   *  This is called automatically by the event loop if any game controller
   *  events are enabled.
   *}
procedure SDL_GameControllerUpdate();
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_GameControllerUpdate' {$ENDIF} {$ENDIF};


  {**
   *  The list of axes available from a controller
   *}


const
  SDL_CONTROLLER_AXIS_INVALID = -1;
  SDL_CONTROLLER_AXIS_LEFTX = 0;
  SDL_CONTROLLER_AXIS_LEFTY = 1;
  SDL_CONTROLLER_AXIS_RIGHTX = 2;
  SDL_CONTROLLER_AXIS_RIGHTY = 3;
  SDL_CONTROLLER_AXIS_TRIGGERLEFT = 4;
  SDL_CONTROLLER_AXIS_TRIGGERRIGHT = 5;
  SDL_CONTROLLER_AXIS_MAX = 6;
type
  TSDL_GameControllerAxis = Byte;


  {**
   *  turn this string into a axis mapping
   *}
function SDL_GameControllerGetAxisFromString(pchString: PChar): TSDL_GameControllerAxis;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_GameControllerGetAxisFromString' {$ENDIF} {$ENDIF};


  {**
   *  turn this axis enum into a string mapping
   *}
function SDL_GameControllerGetStringForAxis(axis: TSDL_GameControllerAxis): PChar;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS}name '_SDL_GameControllerGetStringForAxis' {$ENDIF} {$ENDIF};


  {**
   *  Get the SDL joystick layer binding for this controller button mapping
   *}
function SDL_GameControllerGetBindForAxis(gamecontroller: PSDL_GameController; axis: TSDL_GameControllerAxis): TSDL_GameControllerButtonBind;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_GameControllerGetBindForAxis' {$ENDIF} {$ENDIF};


  {**
   *  Get the current state of an axis control on a game controller.
   *
   *  The state is a value ranging from -32768 to 32767.
   *
   *  The axis indices start at index 0.
   *}
function SDL_GameControllerGetAxis(gamecontroller: PSDL_GameController; axis: TSDL_GameControllerAxis): SInt16;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_GameControllerGetAxis' {$ENDIF} {$ENDIF};


  {**
   *  The list of buttons available from a controller
   *}
const
  SDL_CONTROLLER_BUTTON_INVALID = -1;
  SDL_CONTROLLER_BUTTON_A = 0;
  SDL_CONTROLLER_BUTTON_B = 1;
  SDL_CONTROLLER_BUTTON_X = 2;
  SDL_CONTROLLER_BUTTON_Y = 3;
  SDL_CONTROLLER_BUTTON_BACK = 4;
  SDL_CONTROLLER_BUTTON_GUIDE = 5;
  SDL_CONTROLLER_BUTTON_START = 6;
  SDL_CONTROLLER_BUTTON_LEFTSTICK = 7;
  SDL_CONTROLLER_BUTTON_RIGHTSTICK = 8;
  SDL_CONTROLLER_BUTTON_LEFTSHOULDER = 9;
  SDL_CONTROLLER_BUTTON_RIGHTSHOULDER = 10;
  SDL_CONTROLLER_BUTTON_DPAD_UP = 11;
  SDL_CONTROLLER_BUTTON_DPAD_DOWN = 12;
  SDL_CONTROLLER_BUTTON_DPAD_LEFT = 13;
  SDL_CONTROLLER_BUTTON_DPAD_RIGHT = 14;
  SDL_CONTROLLER_BUTTON_MAX = 15;
type
  TSDL_GameControllerButton = Byte;


  {**
   *  turn this string into a button mapping
   *}
function SDL_GameControllerGetButtonFromString(pchString: PChar): TSDL_GameControllerButton;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_GameControllerGetButtonFromString' {$ENDIF} {$ENDIF};


  {**
   *  turn this button enum into a string mapping
   *}
function SDL_GameControllerGetStringForButton(button: TSDL_GameControllerButton): PChar;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_GameControllerGetStringForButton' {$ENDIF} {$ENDIF};


{**
 *  Get the SDL joystick layer binding for this controller button mapping
 *}
function SDL_GameControllerGetBindForButton(gamecontroller: PSDL_GameController; button: TSDL_GameControllerButton): TSDL_GameControllerButtonBind;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_GameControllerGetBindForButton' {$ENDIF} {$ENDIF};




{**
 *  Get the current state of a button on a game controller.
 *
 *  The button indices start at index 0.
 *}
function SDL_GameControllerGetButton(gamecontroller: PSDL_GameController; button: TSDL_GameControllerButton): UInt8;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_GameControllerGetButton' {$ENDIF} {$ENDIF};


{**
 *  Close a controller previously opened with SDL_GameControllerOpen().
 *}
procedure SDL_GameControllerClose(gamecontroller: PSDL_GameController);
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_GameControllerClose' {$ENDIF} {$ENDIF};



////////////////////////////////////////////////////////////////////////////////////////////////////////
//////////////////////         SDL_haptic.h          ////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////

{**
 *
 *   The SDL Haptic subsystem allows you to control haptic (force feedback)
 *   devices.
 *
 *  The basic usage is as follows:
 *   - Initialize the Subsystem (::SDL_INIT_HAPTIC).
 *   - Open a Haptic Device.
 *    - SDL_HapticOpen() to open from index.
 *    - SDL_HapticOpenFromJoystick() to open from an existing joystick.
 *   - Create an effect (::SDL_HapticEffect).
 *   - Upload the effect with SDL_HapticNewEffect().
 *   - Run the effect with SDL_HapticRunEffect().
 *   - (optional) Free the effect with SDL_HapticDestroyEffect().
 *   - Close the haptic device with SDL_HapticClose().
 *
 *  Simple rumble example:
 *
 *    SDL_Haptic *haptic;
 *
 *    // Open the device
 *    haptic = SDL_HapticOpen( 0 );
 *    if (haptic == NULL)
 *       return -1;
 *
 *    // Initialize simple rumble
 *    if (SDL_HapticRumbleInit( haptic ) != 0)
 *       return -1;
 *
 *    // Play effect at 50% strength for 2 seconds
 *    if (SDL_HapticRumblePlay( haptic, 0.5, 2000 ) != 0)
 *       return -1;
 *    SDL_Delay( 2000 );
 *
 *    // Clean up
 *    SDL_HapticClose( haptic );
 *
 *
 *  Complete example:
 *
 * int test_haptic( SDL_Joystick * joystick )
 *    SDL_Haptic *haptic;
 *    SDL_HapticEffect effect;
 *    int effect_id;
 *
 *    // Open the device
 *    haptic = SDL_HapticOpenFromJoystick( joystick );
 *    if (haptic == NULL) return -1; // Most likely joystick isn't haptic
 *
 *    // See if it can do sine waves
 *    if ((SDL_HapticQuery(haptic) & SDL_HAPTIC_SINE)==0)
 *       SDL_HapticClose(haptic); // No sine effect
 *       return -1;
 *
 *
 *    // Create the effect
 *    memset( &effect, 0, sizeof(SDL_HapticEffect) ); // 0 is safe default
 *    effect.type = SDL_HAPTIC_SINE;
 *    effect.periodic.direction.type = SDL_HAPTIC_POLAR; // Polar coordinates
 *    effect.periodic.direction.dir[0] = 18000; // Force comes from south
 *    effect.periodic.period = 1000; // 1000 ms
 *    effect.periodic.magnitude = 20000; // 20000/32767 strength
 *    effect.periodic.length = 5000; // 5 seconds long
 *    effect.periodic.attack_length = 1000; // Takes 1 second to get max strength
 *    effect.periodic.fade_length = 1000; // Takes 1 second to fade away
 *
 *    // Upload the effect
 *    effect_id = SDL_HapticNewEffect( haptic, &effect );
 *
 *    // Test the effect
 *    SDL_HapticRunEffect( haptic, effect_id, 1 );
 *    SDL_Delay( 5000); // Wait for the effect to finish
 *
 *    // We destroy the effect, although closing the device also does this
 *    SDL_HapticDestroyEffect( haptic, effect_id );
 *
 *    // Close the device
 *    SDL_HapticClose(haptic);
 *
 *    return 0; // Success
 *
 *
 *
 * You can also find out more information on my blog:
 * http://bobbens.dyndns.org/journal/2010/sdl_haptic/
 *
 *  Edgar Simo Serra
 *}


  {**
   *   SDL_Haptic
   *
   *  The haptic structure used to identify an SDL haptic.
   *
   *   SDL_HapticOpen
   *   SDL_HapticOpenFromJoystick
   *   SDL_HapticClose
   *}
type
  PSDL_Haptic = ^TSDL_Haptic;
  TSDL_Haptic = record end;

  {**
   *   Haptic features
   *
   *  Different haptic features a device can have.
   *}

  {**
   *   Haptic effects
   *}

  {**
   *   Constant effect supported.
   *
   *  Constant haptic effect.
   *
   *   SDL_HapticCondition
   *}
const
  SDL_HAPTIC_CONSTANT = (1 shl 0);

  {**
   *   Sine wave effect supported.
   *
   *  Periodic haptic effect that simulates sine waves.
   *
   *   SDL_HapticPeriodic
   *}
const
  SDL_HAPTIC_SINE     = (1 shl 1);

  {**
   *   Square wave effect supported.
   *
   *  Periodic haptic effect that simulates square waves.
   *
   *   SDL_HapticPeriodic
   *}
const
  SDL_HAPTIC_SQUARE   = (1 shl 2);

  {**
   *   Triangle wave effect supported.
   *
   *  Periodic haptic effect that simulates triangular waves.
   *
   *   SDL_HapticPeriodic
   *}
const
  SDL_HAPTIC_TRIANGLE = (1 shl 3);

  {**
   *   Sawtoothup wave effect supported.
   *
   *  Periodic haptic effect that simulates saw tooth up waves.
   *
   *   SDL_HapticPeriodic
   *}
const
  SDL_HAPTIC_SAWTOOTHUP = (1 shl 4);

  {**
   *   Sawtoothdown wave effect supported.
   *
   *  Periodic haptic effect that simulates saw tooth down waves.
   *
   *   SDL_HapticPeriodic
   *}
const
  SDL_HAPTIC_SAWTOOTHDOWN = (1 shl 5);

  {**
   *   Ramp effect supported.
   *
   *  Ramp haptic effect.
   *
   *   SDL_HapticRamp
   *}
const
  SDL_HAPTIC_RAMP = (1 shl 6);

  {**
   *   Spring effect supported - uses axes position.
   *
   *  Condition haptic effect that simulates a spring.  Effect is based on the
   *  axes position.
   *
   *   SDL_HapticCondition
   *}
const
  SDL_HAPTIC_SPRING = (1 shl 7);

  {**
   *   Damper effect supported - uses axes velocity.
   *
   *  Condition haptic effect that simulates dampening.  Effect is based on the
   *  axes velocity.
   *
   *   SDL_HapticCondition
   *}
const
  SDL_HAPTIC_DAMPER = (1 shl 8);

  {**
   *   Inertia effect supported - uses axes acceleration.
   *
   *  Condition haptic effect that simulates inertia.  Effect is based on the axes
   *  acceleration.
   *
   *   SDL_HapticCondition
   *}
const
  SDL_HAPTIC_INERTIA = (1 shl 9);

  {**
   *   Friction effect supported - uses axes movement.
   *
   *  Condition haptic effect that simulates friction.  Effect is based on the
   *  axes movement.
   *
   *   SDL_HapticCondition
   *}
const
  SDL_HAPTIC_FRICTION = (1 shl 10);

  {**
   *   Custom effect is supported.
   *
   *  User defined custom haptic effect.
   *}
const
  SDL_HAPTIC_CUSTOM = (1 shl 11);

  {*Haptic effects*}

  {* These last few are features the device has, not effects *}

  {**
   *   Device can set global gain.
   *
   *  Device supports setting the global gain.
   *
   *   SDL_HapticSetGain
   *}
const
  SDL_HAPTIC_GAIN = (1 shl 12);

  {**
   *   Device can set autocenter.
   *
   *  Device supports setting autocenter.
   *
   *   SDL_HapticSetAutocenter
   *}
const
  SDL_HAPTIC_AUTOCENTER = (1 shl 13);

  {**
   *   Device can be queried for effect status.
   *
   *  Device can be queried for effect status.
   *
   *   SDL_HapticGetEffectStatus
   *}
const
  SDL_HAPTIC_STATUS = (1 shl 14);

  {**
   *   Device can be paused.
   *
   *   SDL_HapticPause
   *   SDL_HapticUnpause
   *}
const
  SDL_HAPTIC_PAUSE = (1 shl 15);

  {**
   *  Direction encodings
   *}

  {**
   *   Uses polar coordinates for the direction.
   *
   *   SDL_HapticDirection
   *}
const
  SDL_HAPTIC_POLAR = 0;

  {**
   *   Uses cartesian coordinates for the direction.
   *
   *   SDL_HapticDirection
   *}
const
  SDL_HAPTIC_CARTESIAN = 1;

  {**
   *   Uses spherical coordinates for the direction.
   *
   *   SDL_HapticDirection
   *}
const
  SDL_HAPTIC_SPHERICAL = 2;

  {*Direction encodings*}

  {*Haptic features*}

  {*
   * Misc defines.
   *}

  {**
   *  Used to play a device an infinite number of times.
   *
   *  SDL_HapticRunEffect
   *}
const
  //SDL_HAPTIC_INFINITY = 4294967295U;
  SDL_HAPTIC_INFINITY = 4294967295; //right?!

  {**
   *   Structure that represents a haptic direction.
   *
   *  Directions can be specified by:
   *   - SDL_HAPTIC_POLAR : Specified by polar coordinates.
   *   - SDL_HAPTIC_CARTESIAN : Specified by cartesian coordinates.
   *   - SDL_HAPTIC_SPHERICAL : Specified by spherical coordinates.
   *
   *  Cardinal directions of the haptic device are relative to the positioning
   *  of the device.  North is considered to be away from the user.
   *
   *  The following diagram represents the cardinal directions:
   *
                   .--.
                   |__| .-------.
                   |=.| |.-----.|
                   |--| ||     ||
                   |  | |'-----'|
                   |__|~')_____('
                     [ COMPUTER ]


                       North (0,-1)
                           ^
                           |
                           |
      (1,0)  West <----[ HAPTIC ]----> East (-1,0)
                           |
                           |
                           v
                        South (0,1)


                        [ USER ]
                          \|||/
                          (o o)
                    ---ooO-(_)-Ooo---

   *
   *  If type is SDL_HAPTIC_POLAR, direction is encoded by hundredths of a
   *  degree starting north and turning clockwise.  ::SDL_HAPTIC_POLAR only uses
   *  the first dir parameter.  The cardinal directions would be:
   *   - North: 0 (0 degrees)
   *   - East: 9000 (90 degrees)
   *   - South: 18000 (180 degrees)
   *   - West: 27000 (270 degrees)
   *
   *  If type is SDL_HAPTIC_CARTESIAN, direction is encoded by three positions
   *  (X axis, Y axis and Z axis (with 3 axes)).  ::SDL_HAPTIC_CARTESIAN uses
   *  the first three dir parameters.  The cardinal directions would be:
   *   - North:  0,-1, 0
   *   - East:  -1, 0, 0
   *   - South:  0, 1, 0
   *   - West:   1, 0, 0
   *
   *  The Z axis represents the height of the effect if supported, otherwise
   *  it's unused.  In cartesian encoding (1, 2) would be the same as (2, 4), you
   *  can use any multiple you want, only the direction matters.
   *
   *  If type is SDL_HAPTIC_SPHERICAL, direction is encoded by two rotations.
   *  The first two dir parameters are used.  The dir parameters are as
   *  follows (all values are in hundredths of degrees):
   *   - Degrees from (1, 0) rotated towards (0, 1).
   *   - Degrees towards (0, 0, 1) (device needs at least 3 axes).
   *
   *
   *  Example of force coming from the south with all encodings (force coming
   *  from the south means the user will have to pull the stick to counteract):
   *
   *  SDL_HapticDirection direction;
   *
   *  // Cartesian directions
   *  direction.type = SDL_HAPTIC_CARTESIAN; // Using cartesian direction encoding.
   *  direction.dir[0] = 0; // X position
   *  direction.dir[1] = 1; // Y position
   *  // Assuming the device has 2 axes, we don't need to specify third parameter.
   *
   *  // Polar directions
   *  direction.type = SDL_HAPTIC_POLAR; // We'll be using polar direction encoding.
   *  direction.dir[0] = 18000; // Polar only uses first parameter
   *
   *  // Spherical coordinates
   *  direction.type = SDL_HAPTIC_SPHERICAL; // Spherical encoding
   *  direction.dir[0] = 9000; // Since we only have two axes we don't need more parameters.
   *
   *
   *   SDL_HAPTIC_POLAR
   *   SDL_HAPTIC_CARTESIAN
   *   SDL_HAPTIC_SPHERICAL
   *   SDL_HapticEffect
   *   SDL_HapticNumAxes
   *}
type
  TSDL_HapticDirection = record
    _type: UInt8;               {**< The type of encoding. *}
    dir: array[0..2] of SInt32; {**< The encoded direction. *}
  end;

  {**
   *   A structure containing a template for a Constant effect.
   *
   *  The struct is exclusive to the ::SDL_HAPTIC_CONSTANT effect.
   *
   *  A constant effect applies a constant force in the specified direction
   *  to the joystick.
   *
   *   SDL_HAPTIC_CONSTANT
   *   SDL_HapticEffect
   *}
type
  TSDL_HapticConstant = record
    {* Header *}
    _type: UInt16;                   {**< SDL_HAPTIC_CONSTANT *}
    direction: TSDL_HapticDirection; {**< Direction of the effect. *}

    {* Replay *}
    length: UInt32;          {**< Duration of the effect. *}
    delay: UInt16;           {**< Delay before starting the effect. *}

    {* Trigger *}
    button: UInt16;          {**< Button that triggers the effect. *}
    interval: UInt16;        {**< How soon it can be triggered again after button. *}

    {* Constant *}
    level: SInt16;           {**< Strength of the constant effect. *}

    {* Envelope *}
    attack_length: UInt16;   {**< Duration of the attack. *}
    attack_level: UInt16;    {**< Level at the start of the attack. *}
    fade_length: UInt16;     {**< Duration of the fade. *}
    fade_level: UInt16;      {**< Level at the end of the fade. *}
  end;

  {**
   *   A structure containing a template for a Periodic effect.
   *
   *  The struct handles the following effects:
   *   - SDL_HAPTIC_SINE
   *   - SDL_HAPTIC_SQUARE
   *   - SDL_HAPTIC_TRIANGLE
   *   - SDL_HAPTIC_SAWTOOTHUP
   *   - SDL_HAPTIC_SAWTOOTHDOWN
   *
   *  A periodic effect consists in a wave-shaped effect that repeats itself
   *  over time.  The type determines the shape of the wave and the parameters
   *  determine the dimensions of the wave.
   *
   *  Phase is given by hundredth of a cycle meaning that giving the phase a value
   *  of 9000 will displace it 25% of its period.  Here are sample values:
   *   -     0: No phase displacement.
   *   -  9000: Displaced 25% of its period.
   *   - 18000: Displaced 50% of its period.
   *   - 27000: Displaced 75% of its period.
   *   - 36000: Displaced 100% of its period, same as 0, but 0 is preferred.
   *
   *  Examples:
   *
      SDL_HAPTIC_SINE
        __      __      __      __
       /  \    /  \    /  \    /
      /    \__/    \__/    \__/

      SDL_HAPTIC_SQUARE
       __    __    __    __    __
      |  |  |  |  |  |  |  |  |  |
      |  |__|  |__|  |__|  |__|  |

      SDL_HAPTIC_TRIANGLE
        /\    /\    /\    /\    /\
       /  \  /  \  /  \  /  \  /
      /    \/    \/    \/    \/

      SDL_HAPTIC_SAWTOOTHUP
        /|  /|  /|  /|  /|  /|  /|
       / | / | / | / | / | / | / |
      /  |/  |/  |/  |/  |/  |/  |

      SDL_HAPTIC_SAWTOOTHDOWN
      \  |\  |\  |\  |\  |\  |\  |
       \ | \ | \ | \ | \ | \ | \ |
        \|  \|  \|  \|  \|  \|  \|

   *
   *   SDL_HAPTIC_SINE
   *   SDL_HAPTIC_SQUARE
   *   SDL_HAPTIC_TRIANGLE
   *   SDL_HAPTIC_SAWTOOTHUP
   *   SDL_HAPTIC_SAWTOOTHDOWN
   *   SDL_HapticEffect
   *}
type
  TSDL_HapticPeriodic = record
    { Header *}
    _type: UInt16;        {**< SDL_HAPTIC_SINE, SDL_HAPTIC_SQUARE,
                               SDL_HAPTIC_TRIANGLE, SDL_HAPTIC_SAWTOOTHUP or
                               SDL_HAPTIC_SAWTOOTHDOWN *}
    direction: TSDL_HapticDirection;  {**< Direction of the effect. *}

    {* Replay *}
    length: UInt32;          {**< Duration of the effect. *}
    delay: UInt16;           {**< Delay before starting the effect. *}

    {* Trigger *}
    button: UInt16;          {**< Button that triggers the effect. *}
    interval: UInt16;        {**< How soon it can be triggered again after button. *}

    {* Periodic *}
    period: UInt16;          {**< Period of the wave. *}
    magnitude: SInt16;       {**< Peak value. *}
    offset: SInt16;          {**< Mean value of the wave. *}
    phase: UInt16;           {**< Horizontal shift given by hundredth of a cycle. *}

    {* Envelope *}
    attack_length: UInt16;   {**< Duration of the attack. *}
    attack_level: UInt16;    {**< Level at the start of the attack. *}
    fade_length: UInt16;     {**< Duration of the fade. *}
    fade_level: UInt16;      {**< Level at the end of the fade. *}
  end;

  {**
   *   A structure containing a template for a Condition effect.
   *
   *  The struct handles the following effects:
   *   - SDL_HAPTIC_SPRING: Effect based on axes position.
   *   - SDL_HAPTIC_DAMPER: Effect based on axes velocity.
   *   - SDL_HAPTIC_INERTIA: Effect based on axes acceleration.
   *   - SDL_HAPTIC_FRICTION: Effect based on axes movement.
   *
   *  Direction is handled by condition internals instead of a direction member.
   *  The condition effect specific members have three parameters.  The first
   *  refers to the X axis, the second refers to the Y axis and the third
   *  refers to the Z axis.  The right terms refer to the positive side of the
   *  axis and the left terms refer to the negative side of the axis.  Please
   *  refer to the ::SDL_HapticDirection diagram for which side is positive and
   *  which is negative.
   *
   *   SDL_HapticDirection
   *   SDL_HAPTIC_SPRING
   *   SDL_HAPTIC_DAMPER
   *   SDL_HAPTIC_INERTIA
   *   SDL_HAPTIC_FRICTION
   *   SDL_HapticEffect
   *}
type
  TSDL_HapticCondition = record
    {* Header *}
    _type: UInt16;                    {**< SDL_HAPTIC_SPRING, SDL_HAPTIC_DAMPER,
                                           SDL_HAPTIC_INERTIA or SDL_HAPTIC_FRICTION *}
    direction: TSDL_HapticDirection;  {**< Direction of the effect - Not used ATM. *}

    {* Replay *}
    length: UInt32;                   {**< Duration of the effect. *}
    delay: UInt16;                    {**< Delay before starting the effect. *}

    {* Trigger *}
    button: UInt16;                   {**< Button that triggers the effect. *}
    interval: UInt16;                 {**< How soon it can be triggered again after button. *}

    {* Condition *}
    right_sat: array[0..2] of UInt16; {**< Level when joystick is to the positive side. *}
    left_sat: array[0..2] of UInt16;  {**< Level when joystick is to the negative side. *}
    right_coeff: array[0..2] of SInt16;  {**< How fast to increase the force towards the positive side. *}
    left_coeff: array[0..2] of SInt16;   {**< How fast to increase the force towards the negative side. *}
    deadband: array[0..2] of UInt16;     {**< Size of the dead zone. *}
    center: array[0..2] of SInt16;       {**< Position of the dead zone. *}
  end;

  {**
   *   A structure containing a template for a Ramp effect.
   *
   *  This struct is exclusively for the ::SDL_HAPTIC_RAMP effect.
   *
   *  The ramp effect starts at start strength and ends at end strength.
   *  It augments in linear fashion.  If you use attack and fade with a ramp
   *  the effects get added to the ramp effect making the effect become
   *  quadratic instead of linear.
   *
   *   SDL_HAPTIC_RAMP
   *   SDL_HapticEffect
   *}
type
  TSDL_HapticRamp = record
    {* Header *}
    _type: UInt16;                    {**< SDL_HAPTIC_RAMP *}
    direction: TSDL_HapticDirection;  {**< Direction of the effect. *}

    {* Replay *}
    length: UInt32;                   {**< Duration of the effect. *}
    delay: UInt16;                    {**< Delay before starting the effect. *}

    {* Trigger *}
    button: UInt16;                   {**< Button that triggers the effect. *}
    interval: UInt16;                 {**< How soon it can be triggered again after button. *}

    {* Ramp *}
    start: SInt16;                    {**< Beginning strength level. *}
    _end: SInt16;                     {**< Ending strength level. *}

    {* Envelope *}
    attack_length: UInt16;            {**< Duration of the attack. *}
    attack_level: UInt16;             {**< Level at the start of the attack. *}
    fade_length: UInt16;              {**< Duration of the fade. *}
    fade_level: UInt16;               {**< Level at the end of the fade. *}
  end;

  {**
   *   A structure containing a template for the ::SDL_HAPTIC_CUSTOM effect.
   *
   *  A custom force feedback effect is much like a periodic effect, where the
   *  application can define its exact shape.  You will have to allocate the
   *  data yourself.  Data should consist of channels * samples Uint16 samples.
   *
   *  If channels is one, the effect is rotated using the defined direction.
   *  Otherwise it uses the samples in data for the different axes.
   *
   *   SDL_HAPTIC_CUSTOM
   *   SDL_HapticEffect
   *}
type
  TSDL_HapticCustom = record
    {* Header *}
    _type: UInt16;                    {**< SDL_HAPTIC_CUSTOM *}
    direction: TSDL_HapticDirection;  {**< Direction of the effect. *}

    {* Replay *}
    length: UInt32;                   {**< Duration of the effect. *}
    delay: UInt16;                    {**< Delay before starting the effect. *}

    {* Trigger *}
    button: UInt16;                   {**< Button that triggers the effect. *}
    interval: UInt16;                 {**< How soon it can be triggered again after button. *}

    {* Custom *}
    channels: UInt8;                  {**< Axes to use, minimum of one. *}
    period: UInt16;                   {**< Sample periods. *}
    samples: UInt16;                  {**< Amount of samples. *}
    data: PUInt16;                    {**< Should contain channels*samples items. *}

    {* Envelope *}
    attack_length: UInt16;            {**< Duration of the attack. *}
    attack_level: UInt16;             {**< Level at the start of the attack. *}
    fade_length: UInt16;              {**< Duration of the fade. *}
    fade_level: UInt16;               {**< Level at the end of the fade. *}
  end;

  {**
   *   The generic template for any haptic effect.
   *
   *  All values max at 32767 (0x7FFF).  Signed values also can be negative.
   *  Time values unless specified otherwise are in milliseconds.
   *
   *  You can also pass SDL_HAPTIC_INFINITY to length instead of a 0-32767
   *  value.  Neither delay, interval, attack_length nor fade_length support
   *  SDL_HAPTIC_INFINITY.  Fade will also not be used since effect never ends.
   *
   *  Additionally, the SDL_HAPTIC_RAMP effect does not support a duration of
   *  SDL_HAPTIC_INFINITY.
   *
   *  Button triggers may not be supported on all devices, it is advised to not
   *  use them if possible.  Buttons start at index 1 instead of index 0 like
   *  the joystick.
   *
   *  If both attack_length and fade_level are 0, the envelope is not used,
   *  otherwise both values are used.
   *
   *  Common parts:
   *
   *  // Replay - All effects have this
   *  Uint32 length;        // Duration of effect (ms).
   *  Uint16 delay;         // Delay before starting effect.
   *
   *  // Trigger - All effects have this
   *  Uint16 button;        // Button that triggers effect.
   *  Uint16 interval;      // How soon before effect can be triggered again.
   *
   *  // Envelope - All effects except condition effects have this
   *  Uint16 attack_length; // Duration of the attack (ms).
   *  Uint16 attack_level;  // Level at the start of the attack.
   *  Uint16 fade_length;   // Duration of the fade out (ms).
   *  Uint16 fade_level;    // Level at the end of the fade.
   *
   *
   *
   *  Here we have an example of a constant effect evolution in time:
   *
      Strength
      ^
      |
      |    effect level -->  _________________
      |                     /                 \
      |                    /                   \
      |                   /                     \
      |                  /                       \
      | attack_level --> |                        \
      |                  |                        |  <---  fade_level
      |
      +--------------------------------------------------> Time
                         [--]                 [---]
                         attack_length        fade_length

      [------------------][-----------------------]
      delay               length

   *
   *  Note either the attack_level or the fade_level may be above the actual
   *  effect level.
   *
   *   SDL_HapticConstant
   *   SDL_HapticPeriodic
   *   SDL_HapticCondition
   *   SDL_HapticRamp
   *   SDL_HapticCustom
   *}
type
  PSDL_HapticEffect = ^TSDL_HapticEffect;
  TSDL_HapticEffect = record
    {* Common for all force feedback effects *}
    _type: UInt16;                  {**< Effect type. *}
    case UInt16 of
      0: (constant: TSDL_HapticConstant;);    {**< Constant effect. *}
      1: (periodic: TSDL_HapticPeriodic;);    {**< Periodic effect. *}
      2: (condition: TSDL_HapticCondition;);  {**< Condition effect. *}
      3: (ramp: TSDL_HapticRamp;);            {**< Ramp effect. *}
      4: (custom: TSDL_HapticCustom;);        {**< Custom effect. *}
  end;

  {* Function prototypes *}

  {**
   *   Count the number of haptic devices attached to the system.
   *
   *   Number of haptic devices detected on the system.
   *}
function SDL_NumHaptics(): Integer;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_NumHaptics' {$ENDIF} {$ENDIF};

  {**
   *   Get the implementation dependent name of a Haptic device.
   *
   *  This can be called before any joysticks are opened.
   *  If no name can be found, this function returns NULL.
   *
   *   device_index Index of the device to get its name.
   *   Name of the device or NULL on error.
   *
   *   SDL_NumHaptics
   *}
function SDL_HapticName(device_index: Integer): PChar;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_HapticName' {$ENDIF} {$ENDIF};

  {**
   *   Opens a Haptic device for usage.
   *
   *  The index passed as an argument refers to the N'th Haptic device on this
   *  system.
   *
   *  When opening a haptic device, its gain will be set to maximum and
   *  autocenter will be disabled.  To modify these values use
   *  SDL_HapticSetGain() and SDL_HapticSetAutocenter().
   *
   *   device_index Index of the device to open.
   *   Device identifier or NULL on error.
   *
   *   SDL_HapticIndex
   *   SDL_HapticOpenFromMouse
   *   SDL_HapticOpenFromJoystick
   *   SDL_HapticClose
   *   SDL_HapticSetGain
   *   SDL_HapticSetAutocenter
   *   SDL_HapticPause
   *   SDL_HapticStopAll
   *}
function SDL_HapticOpen(device_index: Integer): PSDL_Haptic;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_HapticOpen' {$ENDIF} {$ENDIF};

  {**
   *   Checks if the haptic device at index has been opened.
   *
   *   device_index Index to check to see if it has been opened.
   *   1 if it has been opened or 0 if it hasn't.
   *
   *   SDL_HapticOpen
   *   SDL_HapticIndex
   *}
function SDL_HapticOpened(device_index: Integer): Integer;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_HapticOpened' {$ENDIF} {$ENDIF};

  {**
   *   Gets the index of a haptic device.
   *
   *   haptic Haptic device to get the index of.
   *   The index of the haptic device or -1 on error.
   *
   *   SDL_HapticOpen
   *   SDL_HapticOpened
   *}
function SDL_HapticIndex(haptic: PSDL_Haptic): Integer;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_HapticIndex' {$ENDIF} {$ENDIF};

  {**
   *   Gets whether or not the current mouse has haptic capabilities.
   *
   *   SDL_TRUE if the mouse is haptic, SDL_FALSE if it isn't.
   *
   *   SDL_HapticOpenFromMouse
   *}
function SDL_MouseIsHaptic() : Integer;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_MouseInHaptic' {$ENDIF} {$ENDIF};

  {**
   *   Tries to open a haptic device from the current mouse.
   *
   *   The haptic device identifier or NULL on error.
   *
   *   SDL_MouseIsHaptic
   *   SDL_HapticOpen
   *}
function SDL_HapticOpenFromMouse() : PSDL_Haptic;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_HapticOpenFromMouse' {$ENDIF} {$ENDIF};

  {**
   *   Checks to see if a joystick has haptic features.
   *
   *   joystick Joystick to test for haptic capabilities.
   *   1 if the joystick is haptic, 0 if it isn't
   *   or -1 if an error ocurred.
   *
   *   SDL_HapticOpenFromJoystick
   *}
function SDL_JoystickIsHaptic(joystick: PSDL_Joystick): Integer;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_JoystickIsHaptic' {$ENDIF} {$ENDIF};

  {**
   *   Opens a Haptic device for usage from a Joystick device.
   *
   *  You must still close the haptic device seperately.  It will not be closed
   *  with the joystick.
   *
   *  When opening from a joystick you should first close the haptic device before
   *  closing the joystick device.  If not, on some implementations the haptic
   *  device will also get unallocated and you'll be unable to use force feedback
   *  on that device.
   *
   *   joystick Joystick to create a haptic device from.
   *   A valid haptic device identifier on success or NULL on error.
   *
   *   SDL_HapticOpen
   *   SDL_HapticClose
   *}
function SDL_HapticOpenFromJoystick(joystick: PSDL_Joystick): PSDL_Haptic;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_HapticOpenFromJoystick' {$ENDIF} {$ENDIF};

  {**
   *   Closes a Haptic device previously opened with SDL_HapticOpen().
   *
   *   haptic Haptic device to close.
   *}
procedure SDL_HapticClose(haptic: PSDL_Haptic);
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_HapticClose' {$ENDIF} {$ENDIF};

  {**
   *   Returns the number of effects a haptic device can store.
   *
   *  On some platforms this isn't fully supported, and therefore is an
   *  approximation.  Always check to see if your created effect was actually
   *  created and do not rely solely on SDL_HapticNumEffects().
   *
   *   haptic The haptic device to query effect max.
   *   The number of effects the haptic device can store or
   *   -1 on error.
   *
   *   SDL_HapticNumEffectsPlaying
   *   SDL_HapticQuery
   *}
function SDL_HapticNumEffects(haptic: PSDL_Haptic): Integer;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_HapticNumEffects' {$ENDIF} {$ENDIF};

  {**
   *   Returns the number of effects a haptic device can play at the same
   *   time.
   *
   *  This is not supported on all platforms, but will always return a value.
   *  Added here for the sake of completeness.
   *
   *   haptic The haptic device to query maximum playing effects.
   *   The number of effects the haptic device can play at the same time
   *   or -1 on error.
   *
   *   SDL_HapticNumEffects
   *   SDL_HapticQuery
   *}
function SDL_HapticNumEffectsPlaying(haptic: PSDL_Haptic): Integer;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_HapticNumEffectsPlaying' {$ENDIF} {$ENDIF};

  {**
   *   Gets the haptic devices supported features in bitwise matter.
   *
   *  Example:
   *
   *  if (SDL_HapticQueryEffects(haptic) & SDL_HAPTIC_CONSTANT)
   *      printf("We have constant haptic effect!");
   *
   *
   *
   *   haptic The haptic device to query.
   *   Haptic features in bitwise manner (OR'd).
   *
   *   SDL_HapticNumEffects
   *   SDL_HapticEffectSupported
   *}
function SDL_HapticQuery(haptic: PSDL_Haptic): UInt32;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_HapticQuery' {$ENDIF} {$ENDIF};

  {**
   *   Gets the number of haptic axes the device has.
   *
   *   SDL_HapticDirection
   *}
function SDL_HapticNumAxes(haptic: PSDL_Haptic): Integer;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_HapticNumAxes' {$ENDIF} {$ENDIF};

  {**
   *   Checks to see if effect is supported by haptic.
   *
   *   haptic Haptic device to check on.
   *   effect Effect to check to see if it is supported.
   *   SDL_TRUE if effect is supported, SDL_FALSE if it isn't or -1 on error.
   *
   *   SDL_HapticQuery
   *   SDL_HapticNewEffect
   *}
function SDL_HapticEffectSupported(haptic: PSDL_Haptic; effect: PSDL_HapticEffect): Integer;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_HapticEffectSupported' {$ENDIF} {$ENDIF};

  {**
   *   Creates a new haptic effect on the device.
   *
   *   haptic Haptic device to create the effect on.
   *   effect Properties of the effect to create.
   *   The id of the effect on success or -1 on error.
   *
   *   SDL_HapticUpdateEffect
   *   SDL_HapticRunEffect
   *   SDL_HapticDestroyEffect
   *}
function SDL_HapticNewEffect(haptic: PSDL_Haptic; effect: PSDL_HapticEffect): Integer;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_HapticNewEffect' {$ENDIF} {$ENDIF};

  {**
   *   Updates the properties of an effect.
   *
   *  Can be used dynamically, although behaviour when dynamically changing
   *  direction may be strange.  Specifically the effect may reupload itself
   *  and start playing from the start.  You cannot change the type either when
   *  running SDL_HapticUpdateEffect().
   *
   *   haptic Haptic device that has the effect.
   *   effect Effect to update.
   *   data New effect properties to use.
   *   The id of the effect on success or -1 on error.
   *
   *   SDL_HapticNewEffect
   *   SDL_HapticRunEffect
   *   SDL_HapticDestroyEffect
   *}
function SDL_HapticUpdateEffect(haptic: PSDL_Haptic; effect: Integer; data: PSDL_HapticEffect): Integer;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_HapticUpdateEffect' {$ENDIF} {$ENDIF};

  {**
   *   Runs the haptic effect on its associated haptic device.
   *
   *  If iterations are ::SDL_HAPTIC_INFINITY, it'll run the effect over and over
   *  repeating the envelope (attack and fade) every time.  If you only want the
   *  effect to last forever, set ::SDL_HAPTIC_INFINITY in the effect's length
   *  parameter.
   *
   *   haptic Haptic device to run the effect on.
   *   effect Identifier of the haptic effect to run.
   *   iterations Number of iterations to run the effect. Use
   *   SDL_HAPTIC_INFINITY for infinity.
   *   0 on success or -1 on error.
   *
   *   SDL_HapticStopEffect
   *   SDL_HapticDestroyEffect
   *   SDL_HapticGetEffectStatus
   *}
function SDL_HapticRunEffect(haptic: PSDL_Haptic; effect: Integer; iterations: UInt32): Integer;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_HapticRunEffect' {$ENDIF} {$ENDIF};

  {**
   *   Stops the haptic effect on its associated haptic device.
   *
   *   haptic Haptic device to stop the effect on.
   *   effect Identifier of the effect to stop.
   *   0 on success or -1 on error.
   *
   *   SDL_HapticRunEffect
   *   SDL_HapticDestroyEffect
   *}
function SDL_HapticStopEffect(haptic: PSDL_Haptic; effect: Integer): Integer;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_HapticStopEffect' {$ENDIF} {$ENDIF};

  {**
   *   Destroys a haptic effect on the device.
   *
   *  This will stop the effect if it's running.  Effects are automatically
   *  destroyed when the device is closed.
   *
   *   haptic Device to destroy the effect on.
   *   effect Identifier of the effect to destroy.
   *
   *   SDL_HapticNewEffect
   *}
procedure SDL_HapticDestroyEffect(haptic: PSDL_Haptic; effect: Integer);
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_HapticDestroyEffect' {$ENDIF} {$ENDIF};

  {**
   *   Gets the status of the current effect on the haptic device.
   *
   *  Device must support the ::SDL_HAPTIC_STATUS feature.
   *
   *   haptic Haptic device to query the effect status on.
   *   effect Identifier of the effect to query its status.
   *   0 if it isn't playing, 1 if it is playing or -1 on error.
   *
   *   SDL_HapticRunEffect
   *   SDL_HapticStopEffect
   *}
function SDL_HapticGetEffectStatus(haptic: PSDL_Haptic; effect: Integer): Integer;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_HapticGetEffectStatus' {$ENDIF} {$ENDIF};

  {**
   *   Sets the global gain of the device.
   *
   *  Device must support the SDL_HAPTIC_GAIN feature.
   *
   *  The user may specify the maximum gain by setting the environment variable
   *  SDL_HAPTIC_GAIN_MAX which should be between 0 and 100.  All calls to
   *  SDL_HapticSetGain() will scale linearly using SDL_HAPTIC_GAIN_MAX as the
   *  maximum.
   *
   *   haptic Haptic device to set the gain on.
   *   gain Value to set the gain to, should be between 0 and 100.
   *   0 on success or -1 on error.
   *
   *   SDL_HapticQuery
   *}
function SDL_HapticSetGain(haptic: PSDL_Haptic; gain: Integer): Integer;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_HapticSetGain' {$ENDIF} {$ENDIF};

  {**
   *   Sets the global autocenter of the device.
   *
   *  Autocenter should be between 0 and 100.  Setting it to 0 will disable
   *  autocentering.
   *
   *  Device must support the ::SDL_HAPTIC_AUTOCENTER feature.
   *
   *   haptic Haptic device to set autocentering on.
   *   autocenter Value to set autocenter to, 0 disables autocentering.
   *   0 on success or -1 on error.
   *
   *   SDL_HapticQuery
   *}
function SDL_HapticSetAutocenter(haptic: PSDL_Haptic; autocenter: Integer): Integer;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_HapticSetAutocenter' {$ENDIF} {$ENDIF};

  {**
   *   Pauses a haptic device.
   *
   *  Device must support the SDL_HAPTIC_PAUSE feature.  Call
   *  SDL_HapticUnpause() to resume playback.
   *
   *  Do not modify the effects nor add new ones while the device is paused.
   *  That can cause all sorts of weird errors.
   *
   *   haptic Haptic device to pause.
   *   0 on success or -1 on error.
   *
   *   SDL_HapticUnpause
   *}
function SDL_HapticPause(haptic: PSDL_Haptic): Integer;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_HapticPause' {$ENDIF} {$ENDIF};

  {**
   *   Unpauses a haptic device.
   *
   *  Call to unpause after SDL_HapticPause().
   *
   *   haptic Haptic device to pause.
   *   0 on success or -1 on error.
   *
   *   SDL_HapticPause
   *}
function SDL_HapticUnpause(haptic: PSDL_Haptic): Integer;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_HapticUnPause' {$ENDIF} {$ENDIF};

  {**
   *   Stops all the currently playing effects on a haptic device.
   *
   *   haptic Haptic device to stop.
   *   0 on success or -1 on error.
   *}
function SDL_HapticStopAll(haptic: PSDL_Haptic): Integer;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_HapticStopAll' {$ENDIF} {$ENDIF};

  {**
   *   Checks to see if rumble is supported on a haptic device..
   *
   *   haptic Haptic device to check to see if it supports rumble.
   *   SDL_TRUE if effect is supported, SDL_FALSE if it isn't or -1 on error.
   *
   *   SDL_HapticRumbleInit
   *   SDL_HapticRumblePlay
   *   SDL_HapticRumbleStop
   *}
function SDL_HapticRumbleSupported(haptic: PSDL_Haptic): Integer;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_HapticRumbleSupported' {$ENDIF} {$ENDIF};

  {**
   *   Initializes the haptic device for simple rumble playback.
   *
   *   haptic Haptic device to initialize for simple rumble playback.
   *   0 on success or -1 on error.
   *
   *   SDL_HapticOpen
   *   SDL_HapticRumbleSupported
   *   SDL_HapticRumblePlay
   *   SDL_HapticRumbleStop
   *}
function SDL_HapticRumbleInit(haptic: PSDL_Haptic): Integer;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_HapticRumbleInit' {$ENDIF} {$ENDIF};

  {**
   *   Runs simple rumble on a haptic device
   *
   *   haptic Haptic device to play rumble effect on.
   *   strength Strength of the rumble to play as a 0-1 float value.
   *   length Length of the rumble to play in milliseconds.
   *   0 on success or -1 on error.
   *
   *   SDL_HapticRumbleSupported
   *   SDL_HapticRumbleInit
   *   SDL_HapticRumbleStop
   *}
function SDL_HapticRumblePlay(haptic: PSDL_Haptic; strength: Float; length: UInt32): Integer;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_HapticRumblePlay' {$ENDIF} {$ENDIF};

  {**
   *   Stops the simple rumble on a haptic device.
   *
   *   haptic Haptic to stop the rumble on.
   *   0 on success or -1 on error.
   *
   *   SDL_HapticRumbleSupported
   *   SDL_HapticRumbleInit
   *   SDL_HapticRumblePlay
   *}
function SDL_HapticRumbleStop(haptic: PSDL_Haptic): Integer;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_HapticRumbleStop' {$ENDIF} {$ENDIF};







////////////////////////////////////////////////////////////////////////////////////////////////////////
//////////////////////         SDL_touch.h          ////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////  

type
  PSDL_TouchID  = ^TSDL_TouchID;
  TSDL_TouchID  = SInt64;

  PSDL_FingerID = ^TSDL_FingerID;
  TSDL_FingerID = SInt64;

  PSDL_Finger = ^TSDL_Finger;
  TSDL_Finger = record
    id: TSDL_FingerID;
    x: Float;
    y: Float;
    pressure: Float;
  end;

{* Used as the device ID for mouse events simulated with touch input *}
const
  SDL_TOUCH_MOUSEID = UInt32(-1);

  {* Function prototypes *}

  {**
   *  Get the number of registered touch devices.
   *}
function SDL_GetNumTouchDevices(): SInt32;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_GetNumTouchDevices' {$ENDIF} {$ENDIF};

  {**
   *  Get the touch ID with the given index, or 0 if the index is invalid.
   *}
function SDL_GetTouchDevice(index: SInt32): TSDL_TouchID;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_GetTouchDevice' {$ENDIF} {$ENDIF};

  {**
   *  Get the number of active fingers for a given touch device.
   *}
function SDL_GetNumTouchFingers(touchID: TSDL_TouchID): SInt32;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_GetNumTouchFingers' {$ENDIF} {$ENDIF};

  {**
   *  Get the finger object of the given touch, with the given index.
   *}
function SDL_GetTouchFinger(touchID: TSDL_TouchID; index: SInt32): PSDL_Finger;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_GetTouchFinger' {$ENDIF} {$ENDIF};



////////////////////////////////////////////////////////////////////////////////////////////////////////  
//////////////////////        SDL_gesture.h         ////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////  

type
  TSDL_GestureID = SInt64;

  {* Function prototypes *}

  {**
   *  Begin Recording a gesture on the specified touch, or all touches (-1)
   *
   *
   *}
function SDL_RecordGesture(touchId: TSDL_TouchID): SInt32;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_RecordGesture' {$ENDIF} {$ENDIF};

  {**
   *  Save all currently loaded Dollar Gesture templates
   *
   *
   *}
function SDL_SaveAllDollarTemplates(src: PSDL_RWops): SInt32;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_SaveAllDollarTemplates' {$ENDIF} {$ENDIF};

  {**
   *  Save a currently loaded Dollar Gesture template
   *
   *
   *}
function SDL_SaveDollarTemplate(gestureId: TSDL_GestureID; src: PSDL_RWops): SInt32;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_SaveDollarTemplate' {$ENDIF} {$ENDIF};


  {**
   *  Load Dollar Gesture templates from a file
   *
   *
   *}
function SDL_LoadDollarTemplates(touchId: TSDL_TouchID; src: PSDL_RWops): SInt32;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_LoadDollarTemplates' {$ENDIF} {$ENDIF};



////////////////////////////////////////////////////////////////////////////////////////////////////////
//////////////////////        SDL_events.h          ////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////

  {**
   *  The types of events that can be delivered.
   *}

const

  SDL_FIRSTEVENT       = 0;     // Unused (do not remove) (needed in pascal?)

  SDL_COMMONEVENT      = 1;     //added for pascal-compatibility

  { Application events }
  SDL_QUITEV           = $100;  // User-requested quit (originally SDL_QUIT, but changed, cause theres a method called SDL_QUIT)


  {* These application events have special meaning on iOS, see README.iOS for details *}
  SDL_APP_TERMINATING  = $101;   {**< The application is being terminated by the OS
                                      Called on iOS in applicationWillTerminate()
                                      Called on Android in onDestroy()
                                  *}
  SDL_APP_LOWMEMORY    = $102;   {**< The application is low on memory, free memory if possible.
                                      Called on iOS in applicationDidReceiveMemoryWarning()
                                      Called on Android in onLowMemory()
                                  *}
  SDL_APP_WILLENTERBACKGROUND = $103; {**< The application is about to enter the background
                                           Called on iOS in applicationWillResignActive()
                                           Called on Android in onPause()
                                       *}
  SDL_APP_DIDENTERBACKGROUND = $104;  {**< The application did enter the background and may not get CPU for some time
                                           Called on iOS in applicationDidEnterBackground()
                                           Called on Android in onPause()
                                       *}
  SDL_APP_WILLENTERFOREGROUND = $105; {**< The application is about to enter the foreground
                                           Called on iOS in applicationWillEnterForeground()
                                           Called on Android in onResume()
                                       *}
  SDL_APP_DIDENTERFOREGROUND = $106;  {**< The application is now interactive
                                           Called on iOS in applicationDidBecomeActive()
                                           Called on Android in onResume()
                                       *}

  { Window events }
  SDL_WINDOWEVENT      = $200;  // Window state change
  SDL_SYSWMEVENT       = $201;  // System specific event

  { Keyboard events }
  SDL_KEYDOWN          = $300;  // Key pressed
  SDL_KEYUP            = $301;  // Key released
  SDL_TEXTEDITING      = $302;  // Keyboard text editing (composition)
  SDL_TEXTINPUT        = $303;  // Keyboard text input

  { Mouse events }
  SDL_MOUSEMOTION      = $400;  // Mouse moved
  SDL_MOUSEBUTTONDOWN  = $401;  // Mouse button pressed
  SDL_MOUSEBUTTONUP    = $402;  // Mouse button released
  SDL_MOUSEWHEEL       = $403;  // Mouse wheel motion

  { Joystick events }
  SDL_JOYAXISMOTION    = $600;  // Joystick axis motion
  SDL_JOYBALLMOTION    = $601;  // Joystick trackball motion
  SDL_JOYHATMOTION     = $602;  // Joystick hat position change
  SDL_JOYBUTTONDOWN    = $603;  // Joystick button pressed
  SDL_JOYBUTTONUP      = $604;  // Joystick button released 
  SDL_JOYDEVICEADDED   = $605;  // A new joystick has been inserted into the system 
  SDL_JOYDEVICEREMOVED = $606;  // An opened joystick has been removed 

  { Game controller events }
  SDL_CONTROLLERAXISMOTION     = $650;  // Game controller axis motion
  SDL_CONTROLLERBUTTONDOWN     = $651;  // Game controller button pressed 
  SDL_CONTROLLERBUTTONUP       = $652;  // Game controller button released 
  SDL_CONTROLLERDEVICEADDED    = $653;  // A new Game controller has been inserted into the system 
  SDL_CONTROLLERDEVICEREMOVED  = $654;  // An opened Game controller has been removed 
  SDL_CONTROLLERDEVICEREMAPPED = $655;  // The controller mapping was updated 

  { Touch events }
  SDL_FINGERDOWN      = $700;
  SDL_FINGERUP        = $701;
  SDL_FINGERMOTION    = $702;

  { Gesture events }
  SDL_DOLLARGESTURE   = $800;
  SDL_DOLLARRECORD    = $801;
  SDL_MULTIGESTURE    = $802;

  { Clipboard events }
  SDL_CLIPBOARDUPDATE = $900; // The clipboard changed

  { Drag and drop events }
  SDL_DROPFILE        = $1000; // The system requests a file open

  {** Events SDL_USEREVENT through SDL_LASTEVENT are for your use,
   *  and should be allocated with SDL_RegisterEvents()
   *}
  SDL_USEREVENT    = $8000;

  {**
   *  This last event is only for bounding internal arrays (needed in pascal ??)
   *}
  SDL_LASTEVENT    = $FFFF;

type

  TSDL_EventType = Word;

  {**
   *  Fields shared by every event
   *}

  TSDL_CommonEvent = record
    type_: UInt32;
    timestamp: UInt32;
  end;

  {**
   *  Window state change event data (event.window.*)
   *}

  TSDL_WindowEvent = record
    type_: UInt32;       // SDL_WINDOWEVENT 
    timestamp: UInt32;
    windowID: UInt32;    // The associated window 
    event: UInt8;        // SDL_WindowEventID 
    padding1: UInt8;
    padding2: UInt8;
    padding3: UInt8;
    data1: SInt32;       // event dependent data
    data2: SInt32;       // event dependent data 
  end;

  {**
   *  Keyboard button event structure (event.key.*)
   *}
  TSDL_KeyboardEvent = record
    type_: UInt32;        // SDL_KEYDOWN or SDL_KEYUP 
    timestamp: UInt32;
    windowID: UInt32;     // The window with keyboard focus, if any 
    state: UInt8;         // SDL_PRESSED or SDL_RELEASED 
    _repeat: UInt8;       // Non-zero if this is a key repeat
    padding2: UInt8;
    padding3: UInt8;
    keysym: TSDL_KeySym;  // The key that was pressed or released
  end;

const
  SDL_TEXTEDITINGEVENT_TEXT_SIZE = 32;
  
type
 
  {**
   *  Keyboard text editing event structure (event.edit.*)
   *}
 
  TSDL_TextEditingEvent = record
    type_: UInt32;                               // SDL_TEXTEDITING 
    timestamp: UInt32;
    windowID: UInt32;                            // The window with keyboard focus, if any
    text: array[0..SDL_TEXTEDITINGEVENT_TEXT_SIZE] of Char;  // The editing text 
    start: SInt32;                               // The start cursor of selected editing text 
    length: SInt32;                              // The length of selected editing text
  end;

const
  SDL_TEXTINPUTEVENT_TEXT_SIZE = 32;

type

  {**
   *  Keyboard text input event structure (event.text.*)
   *}
 
  TSDL_TextInputEvent = record
    type_: UInt32;                                          // SDL_TEXTINPUT 
    timestamp: UInt32;
    windowID: UInt32;                                       // The window with keyboard focus, if any
    text: array[0..SDL_TEXTINPUTEVENT_TEXT_SIZE] of Char;   // The input text 
  end;

  {**
   *  Mouse motion event structure (event.motion.*)
   *}
 
  TSDL_MouseMotionEvent = record
    type_: UInt32;       // SDL_MOUSEMOTION
    timestamp: UInt32;
    windowID: UInt32;    // The window with mouse focus, if any 
    which: UInt32;       // The mouse instance id, or SDL_TOUCH_MOUSEID
    state: UInt8;        // The current button state 
    padding1: UInt8;
    padding2: UInt8;
    padding3: UInt8;
    x: SInt32;           // X coordinate, relative to window 
    y: SInt32;           // Y coordinate, relative to window
    xrel: SInt32;        // The relative motion in the X direction 
    yrel: SInt32;        // The relative motion in the Y direction 
  end;

  {**
   *  Mouse button event structure (event.button.*)
   *}
 
  TSDL_MouseButtonEvent = record
    type_: UInt32;       // SDL_MOUSEBUTTONDOWN or SDL_MOUSEBUTTONUP 
    timestamp: UInt32;
    windowID: UInt32;    // The window with mouse focus, if any
    which: UInt32;       // The mouse instance id, or SDL_TOUCH_MOUSEID 
    button: UInt8;       // The mouse button index 
    state: UInt8;        // SDL_PRESSED or SDL_RELEASED
    padding1: UInt8;
    padding2: UInt8;
    x: SInt32;           // X coordinate, relative to window
    y: SInt32;           // Y coordinate, relative to window 
  end;

  {**
   *  Mouse wheel event structure (event.wheel.*)
   *}
 
  TSDL_MouseWheelEvent = record
    type_: UInt32;        // SDL_MOUSEWHEEL
    timestamp: UInt32;
    windowID: UInt32;    // The window with mouse focus, if any 
    which: UInt32;       // The mouse instance id, or SDL_TOUCH_MOUSEID
    x: SInt32;           // The amount scrolled horizontally 
    y: SInt32;           // The amount scrolled vertically 
  end;

  {**
   *  Joystick axis motion event structure (event.jaxis.*)
   *}
 
  TSDL_JoyAxisEvent = record
    type_: UInt32;         // SDL_JOYAXISMOTION 
    timestamp: UInt32;
    which: TSDL_JoystickID; // The joystick instance id
    axis: UInt8;           // The joystick axis index 
    padding1: UInt8;
    padding2: UInt8;
    padding3: UInt8;
    value: SInt16;         // The axis value (range: -32768 to 32767) 
    padding4: UInt16;
  end;

  {**
   *  Joystick trackball motion event structure (event.jball.*)
   *}

  TSDL_JoyBallEvent = record
    type_: UInt32;         // SDL_JOYBALLMOTION
    timestamp: UInt32;
    which: TSDL_JoystickID; // The joystick instance id
    ball: UInt8;           // The joystick trackball index
    padding1: UInt8;
    padding2: UInt8;
    padding3: UInt8;
    xrel: SInt16;          // The relative motion in the X direction
    yrel: SInt16;          // The relative motion in the Y direction
  end;

  {**
   *  Joystick hat position change event structure (event.jhat.*)
   *}

  TSDL_JoyHatEvent = record
    type_: UInt32;         // SDL_JOYHATMOTION
    timestamp: UInt32;
    which: TSDL_JoystickID; // The joystick instance id
    hat: UInt8;            // The joystick hat index
    value: UInt8;         {*  The hat position value.
                           *  SDL_HAT_LEFTUP   SDL_HAT_UP       SDL_HAT_RIGHTUP
                           *  SDL_HAT_LEFT     SDL_HAT_CENTERED SDL_HAT_RIGHT
                           *  SDL_HAT_LEFTDOWN SDL_HAT_DOWN     SDL_HAT_RIGHTDOWN
                           *
                           *  Note that zero means the POV is centered.
                           *}
    padding1: UInt8;
    padding2: UInt8;
  end;

  {**
   *  Joystick button event structure (event.jbutton.*)
   *}

  TSDL_JoyButtonEvent = record
    type_: UInt32;        // SDL_JOYBUTTONDOWN or SDL_JOYBUTTONUP
    timestamp: UInt32;
    which: TSDL_JoystickID; // The joystick instance id 
    button: UInt8;         // The joystick button index 
    state: UInt8;          // SDL_PRESSED or SDL_RELEASED
    padding1: UInt8;
    padding2: UInt8;
  end;

  {**
   *  Joystick device event structure (event.jdevice.*)
   *}

  TSDL_JoyDeviceEvent = record
    type_: UInt32;      // SDL_JOYDEVICEADDED or SDL_JOYDEVICEREMOVED
    timestamp: UInt32;
    which: SInt32;      // The joystick device index for the ADDED event, instance id for the REMOVED event
  end;

  {**
   *  Game controller axis motion event structure (event.caxis.*)
   *}

  TSDL_ControllerAxisEvent = record
    type_: UInt32;         // SDL_CONTROLLERAXISMOTION
    timestamp: UInt32;
    which: TSDL_JoystickID; // The joystick instance id
    axis: UInt8;           // The controller axis (SDL_GameControllerAxis)
    padding1: UInt8;
    padding2: UInt8;
    padding3: UInt8;
    value: SInt16;         // The axis value (range: -32768 to 32767)
    padding4: UInt16;
  end;

  {**
   *  Game controller button event structure (event.cbutton.*)
   *}

  TSDL_ControllerButtonEvent = record
    type_: UInt32;         // SDL_CONTROLLERBUTTONDOWN or SDL_CONTROLLERBUTTONUP
    timestamp: UInt32;
    which: TSDL_JoystickID; // The joystick instance id
    button: UInt8;         // The controller button (SDL_GameControllerButton)
    state: UInt8;          // SDL_PRESSED or SDL_RELEASED
    padding1: UInt8;
    padding2: UInt8;
  end;


  {**
   *  Controller device event structure (event.cdevice.*)
   *}

  TSDL_ControllerDeviceEvent = record
    type_: UInt32;       // SDL_CONTROLLERDEVICEADDED, SDL_CONTROLLERDEVICEREMOVED, or SDL_CONTROLLERDEVICEREMAPPED
    timestamp: UInt32;
    which: SInt32;       // The joystick device index for the ADDED event, instance id for the REMOVED or REMAPPED event
  end;

  {**
   *  Touch finger event structure (event.tfinger.*)
   *}

  TSDL_TouchFingerEvent = record
    type_: UInt32;         // SDL_FINGERMOTION or SDL_FINGERDOWN or SDL_FINGERUP
    timestamp: UInt32;
    touchId: TSDL_TouchID;  // The touch device id
    fingerId: TSDL_FingerID;
    x: Float;              // Normalized in the range 0...1
    y: Float;              // Normalized in the range 0...1
    dx: Float;             // Normalized in the range 0...1
    dy: Float;             // Normalized in the range 0...1
    pressure: Float;       // Normalized in the range 0...1
  end;

  {**
   *  Multiple Finger Gesture Event (event.mgesture.*)
   *}
  TSDL_MultiGestureEvent = record
    type_: UInt32;        // SDL_MULTIGESTURE
    timestamp: UInt32;
    touchId: TSDL_TouchID; // The touch device index
    dTheta: Float;
    dDist: Float;
    x: Float;
    y: Float;
    numFingers: UInt16;
    padding: UInt16;
  end;


  {* (event.dgesture.*) *}
  TSDL_DollarGestureEvent = record
    type_: UInt32;         // SDL_DOLLARGESTURE
    timestamp: UInt32;
    touchId: TSDL_TouchID;  // The touch device id
    gestureId: TSDL_GestureID;
    numFingers: UInt32;
    error: Float;
    x: Float;              // Normalized center of gesture
    y: Float;              // Normalized center of gesture
  end;


  {**
   *  An event used to request a file open by the system (event.drop.*)
   *  This event is disabled by default, you can enable it with SDL_EventState()
   *  If you enable this event, you must free the filename in the event.
   *}

  TSDL_DropEvent = record
    type_: UInt32;      // SDL_DROPFILE
    timestamp: UInt32;
    _file: PChar;   // The file name, which should be freed with SDL_free()
  end;

  {**
   *  The "quit requested" event
   *}

  TSDL_QuitEvent = record
    type_: UInt32;        // SDL_QUIT
    timestamp: UInt32;
  end;

  {**
   *  A user-defined event type (event.user.*)
   *}

  TSDL_UserEvent = record
    type_: UInt32;       // SDL_USEREVENT through SDL_NUMEVENTS-1
    timestamp: UInt32;
    windowID: UInt32;    // The associated window if any
    code: SInt32;        // User defined event code
    data1: Pointer;      // User defined data pointer
    data2: Pointer;      // User defined data pointer
  end;

  {$IFDEF Unix}
    //These are the various supported subsystems under UNIX
    TSDL_SysWm = ( SDL_SYSWM_X11 ) ;
  {$ENDIF}

  // The windows custom event structure
  {$IFDEF MSWINDOWS}
    PSDL_SysWMmsg = ^TSDL_SysWMmsg;
    TSDL_SysWMmsg = record
      version: TSDL_Version;
      h_wnd: HWND; // The window for the message
      msg: UInt32; // The type of message
      w_Param: WPARAM; // WORD message parameter
      lParam: LPARAM; // LONG message parameter
    end;
  {$ELSE}

    {$IFDEF Unix}
      { The Linux custom event structure }
      PSDL_SysWMmsg = ^TSDL_SysWMmsg;
      TSDL_SysWMmsg = record
        version : TSDL_Version;
        subsystem : TSDL_SysWm;
        {$IFDEF FPC}
          {$IFNDEF DARWIN}
          event : TXEvent;
          {$ENDIF}
        {$ELSE}
          event : XEvent;
        {$ENDIF}
      end;
    {$ELSE}
      { The generic custom event structure }
      PSDL_SysWMmsg = ^TSDL_SysWMmsg;
      TSDL_SysWMmsg = record
        version: TSDL_Version;
        data: Integer;
      end;
    {$ENDIF}

  {$ENDIF}

  // The Windows custom window manager information structure
  {$IFDEF MSWINDOWS}
    PSDL_SysWMinfo = ^TSDL_SysWMinfo;
    TSDL_SysWMinfo = record
      version : TSDL_Version;
      window : HWnd;	// The display window
    end;
  {$ELSE}
    // The Linux custom window manager information structure
    {$IFDEF Unix}
      {$IFNDEF DARWIN}
      TX11 = record
        display : PDisplay;	// The X11 display
        window : TWindow;		// The X11 display window */
        {* These locking functions should be called around
           any X11 functions using the display variable.
           They lock the event thread, so should not be
           called around event functions or from event filters.
         *}
        lock_func : Pointer;
        unlock_func : Pointer;

        // Introduced in SDL 1.0.2
        fswindow : TWindow;	// The X11 fullscreen window */
        wmwindow : TWindow;	// The X11 managed input window */
      end;
      {$ENDIF}

      PSDL_SysWMinfo = ^TSDL_SysWMinfo;
      TSDL_SysWMinfo = record
         version : TSDL_Version;
         subsystem : TSDL_SysWm;
         {$IFNDEF DARWIN}
         X11 : TX11;
         {$ENDIF}
      end;
    {$ELSE}
      // The generic custom window manager information structure
      PSDL_SysWMinfo = ^TSDL_SysWMinfo;
      TSDL_SysWMinfo = record
        version : TSDL_Version;
        data : integer;
      end;
    {$ENDIF}

  {$ENDIF}

  {**
   *  A video driver dependent system event (event.syswm.*)
   *  This event is disabled by default, you can enable it with SDL_EventState()
   *
   *  If you want to use this event, you should include SDL_syswm.h.
   *}

  PSDL_SysWMEvent = ^TSDL_SysWMEvent;
  TSDL_SysWMEvent = record
    type_: UInt32;       // SDL_SYSWMEVENT
    timestamp: UInt32;
    msg: PSDL_SysWMmsg;  // driver dependent data (defined in SDL_syswm.h)
  end;

  {**
   *  General event structure
   *}

  PSDL_Event = ^TSDL_Event;
  TSDL_Event = record
    case Integer of
      0:  (type_: UInt32);

      SDL_COMMONEVENT:  (common: TSDL_CommonEvent);
      SDL_WINDOWEVENT:  (window: TSDL_WindowEvent);

      SDL_KEYUP,
      SDL_KEYDOWN:  (key: TSDL_KeyboardEvent);
      SDL_TEXTEDITING:  (edit: TSDL_TextEditingEvent);
      SDL_TEXTINPUT:  (text: TSDL_TextInputEvent);

      SDL_MOUSEMOTION:  (motion: TSDL_MouseMotionEvent);
      SDL_MOUSEBUTTONUP,
      SDL_MOUSEBUTTONDOWN:  (button: TSDL_MouseButtonEvent);
      SDL_MOUSEWHEEL:  (wheel: TSDL_MouseWheelEvent);
	  
      SDL_JOYAXISMOTION:  (jaxis: TSDL_JoyAxisEvent);
      SDL_JOYBALLMOTION: (jball: TSDL_JoyBallEvent);
      SDL_JOYHATMOTION: (jhat: TSDL_JoyHatEvent);
      SDL_JOYBUTTONDOWN,
      SDL_JOYBUTTONUP: (jbutton: TSDL_JoyButtonEvent);
      SDL_JOYDEVICEADDED,
      SDL_JOYDEVICEREMOVED: (jdevice: TSDL_JoyDeviceEvent);

      SDL_CONTROLLERAXISMOTION: (caxis: TSDL_ControllerAxisEvent);
      SDL_CONTROLLERBUTTONUP,
      SDL_CONTROLLERBUTTONDOWN: (cbutton: TSDL_ControllerButtonEvent);
      SDL_CONTROLLERDEVICEADDED,
      SDL_CONTROLLERDEVICEREMOVED,
      SDL_CONTROLLERDEVICEREMAPPED: (cdevice: TSDL_ControllerDeviceEvent);

      SDL_QUITEV: (quit: TSDL_QuitEvent);

      SDL_USEREVENT: (user: TSDL_UserEvent);
      SDL_SYSWMEVENT: (syswm: TSDL_SysWMEvent);

      SDL_FINGERDOWN,
      SDL_FINGERUP,
      SDL_FINGERMOTION: (tfinger: TSDL_TouchFingerEvent);
      SDL_MULTIGESTURE: (mgesture: TSDL_MultiGestureEvent);
      SDL_DOLLARGESTURE,SDL_DOLLARRECORD: (dgesture: TSDL_DollarGestureEvent);

      SDL_DROPFILE: (drop: TSDL_DropEvent);
  end;


  {* Function prototypes *}

  {**
   *  Pumps the event loop, gathering events from the input devices.
   *  
   *  This function updates the event queue and internal input device state.
   *  
   *  This should only be run in the thread that sets the video mode.
   *}
procedure SDL_PumpEvents();
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_PumpEvents' {$ENDIF} {$ENDIF};

const
  SDL_ADDEVENT = 0;
  SDL_PEEKEVENT = 1;
  SDL_GETEVENT = 2;

type
  TSDL_EventAction = Word;

  {**
   *  Checks the event queue for messages and optionally returns them.
   *
   *  If action is SDL_ADDEVENT, up to numevents events will be added to
   *  the back of the event queue.
   *
   *  If action is SDL_PEEKEVENT, up to numevents events at the front
   *  of the event queue, within the specified minimum and maximum type,
   *  will be returned and will not be removed from the queue.
   *
   *  If action is SDL_GETEVENT, up to numevents events at the front
   *  of the event queue, within the specified minimum and maximum type,
   *  will be returned and will be removed from the queue.
   *
   *  Result: The number of events actually stored, or -1 if there was an error.
   *
   *  This function is thread-safe.
   *}

function SDL_PeepEvents(events: PSDL_Event; numevents: SInt32; action: TSDL_EventAction; minType: UInt32; maxType: UInt32): SInt32;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_PeepEvents' {$ENDIF} {$ENDIF};

  {**
   *  Checks to see if certain event types are in the event queue.
   *}

function SDL_HasEvent(type_: UInt32): TSDL_Bool;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_HasEvent' {$ENDIF} {$ENDIF};

function SDL_HasEvents(minType: UInt32; maxType: UInt32): TSDL_Bool;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_HasEvents' {$ENDIF} {$ENDIF};

  {**
   *  This function clears events from the event queue
   *}
procedure SDL_FlushEvent(type_: UInt32);
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_FlushEvent' {$ENDIF} {$ENDIF};

procedure SDL_FlushEvents(minType: UInt32; maxType: UInt32);
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_FlushEvents' {$ENDIF} {$ENDIF};

  {**
   *  Polls for currently pending events.
   *
   *  1 if there are any pending events, or 0 if there are none available.
   *
   *  event - If not nil, the next event is removed from the queue and
   *               stored in that area.
   *}
function SDL_PollEvent(event: PSDL_Event): SInt32;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_PollEvent' {$ENDIF} {$ENDIF};

  {**
   *  Waits indefinitely for the next available event.
   *
   *  1, or 0 if there was an error while waiting for events.
   *
   *  event - If not nil, the next event is removed from the queue and
   *  stored in that area.
   *}
function SDL_WaitEvent(event: PSDL_Event): SInt32;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_WaitEvent' {$ENDIF} {$ENDIF};

  {**
   *  Waits until the specified timeout (in milliseconds) for the next
   *  available event.
   *
   *  1, or 0 if there was an error while waiting for events.
   *
   *  event - If not nil, the next event is removed from the queue and
   *  stored in that area.
   *}
function SDL_WaitEventTimeout(event: PSDL_Event; timeout: SInt32): SInt32;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_WaitEventTimeout' {$ENDIF} {$ENDIF};

  {**
   *  Add an event to the event queue.
   *
   *  1 on success, 0 if the event was filtered, or -1 if the event queue
   *  was full or there was some other error.
   *}
function SDL_PushEvent(event: PSDL_Event): SInt32;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_PumpEvents' {$ENDIF} {$ENDIF};

type
  PSDL_EventFilter = ^TSDL_EventFilter;
  TSDL_EventFilter = function( event: PSDL_Event ): Integer; cdecl;

  {**
   *  Sets up a filter to process all events before they change internal state and
   *  are posted to the internal event queue.
   *  
   *  If the filter returns 1, then the event will be added to the internal queue.
   *  If it returns 0, then the event will be dropped from the queue, but the 
   *  internal state will still be updated.  This allows selective filtering of
   *  dynamically arriving events.
   *  
   *  Be very careful of what you do in the event filter function, as 
   *  it may run in a different thread!
   *  
   *  There is one caveat when dealing with the SDL_QUITEVENT event type.  The
   *  event filter is only called when the window manager desires to close the
   *  application window.  If the event filter returns 1, then the window will
   *  be closed, otherwise the window will remain open if possible.
   *
   *  If the quit event is generated by an interrupt signal, it will bypass the
   *  internal queue and be delivered to the application at the next event poll.
   *}
procedure SDL_SetEventFilter(filter: TSDL_EventFilter; userdata: Pointer);
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_SetEventFilter' {$ENDIF} {$ENDIF};

  {**
   *  Return the current event filter - can be used to "chain" filters.
   *  If there is no event filter set, this function returns SDL_FALSE.
   *}
function SDL_GetEventFilter(filter: PSDL_EventFilter; userdata: Pointer): TSDL_Bool;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_GetEventFilter' {$ENDIF} {$ENDIF};

  {**
   *  Add a function which is called when an event is added to the queue.
   *}
procedure SDL_AddEventWatch(filter: TSDL_EventFilter; userdata: Pointer);
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_AddEventWatch' {$ENDIF} {$ENDIF};

  {**
   *  Remove an event watch function added with SDL_AddEventWatch()
   *}
procedure SDL_DelEventWatch(filter: TSDL_EventFilter; userdata: Pointer);
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_DelEventWatch' {$ENDIF} {$ENDIF};

  {**
   *  Run the filter function on the current event queue, removing any
   *  events for which the filter returns 0.
   *}
procedure SDL_FilterEvents(filter: TSDL_EventFilter; userdata: Pointer);
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_FilterEvents' {$ENDIF} {$ENDIF};

const

  SDL_QUERY   =	-1;
  SDL_IGNORE  =	 0;
  SDL_DISABLE =	 0;
  SDL_ENABLE  =  1;

  {**
   *  This function allows you to set the state of processing certain events.
   *   - If state is set to SDL_IGNORE, that event will be automatically
   *     dropped from the event queue and will not event be filtered.
   *   - If state is set to SDL_ENABLE, that event will be processed
   *     normally.
   *   - If state is set to SDL_QUERY, SDL_EventState() will return the
   *     current processing state of the specified event.
   *}
function SDL_EventState(type_: UInt32; state: SInt32): UInt8;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_EventState' {$ENDIF} {$ENDIF};

function SDL_GetEventState(type_: UInt32): UInt8;

  {**
   *  This function allocates a set of user-defined events, and returns
   *  the beginning event number for that set of events.
   *
   *  If there aren't enough user-defined events left, this function
   *  returns (Uint32)-1
   *}
function SDL_RegisterEvents(numevents: SInt32): UInt32;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_RegisterEvents' {$ENDIF} {$ENDIF};



////////////////////////////////////////////////////////////////////////////////////////////////////////
//////////////////////           SDL.h              ////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////

const

  SDL_INIT_TIMER          = $00000001;
  {$EXTERNALSYM SDL_INIT_TIMER}
  SDL_INIT_AUDIO          = $00000010;
  {$EXTERNALSYM SDL_INIT_AUDIO}
  SDL_INIT_VIDEO          = $00000020;
  {$EXTERNALSYM SDL_INIT_VIDEO}
  SDL_INIT_JOYSTICK       = $00000200;
  {$EXTERNALSYM SDL_INIT_JOYSTICK}
  SDL_INIT_HAPTIC         = $00001000;
  {$EXTERNALSYM SDL_INIT_HAPTIC}
  SDL_INIT_GAMECONTROLLER = $00002000;  //turn on game controller also implicitly does JOYSTICK
  {$EXTERNALSYM SDL_INIT_GAMECONTROLLER}
  SDL_INIT_NOPARACHUTE    = $00100000;  //Don't catch fatal signals
  {$EXTERNALSYM SDL_INIT_NOPARACHUTE}
  SDL_INIT_EVERYTHING     = SDL_INIT_TIMER    or
							SDL_INIT_AUDIO    or
							SDL_INIT_VIDEO    or
							SDL_INIT_JOYSTICK or
							SDL_INIT_HAPTIC   or
							SDL_INIT_GAMECONTROLLER;
  {$EXTERNALSYM SDL_INIT_EVERYTHING}

{**
 *  This function initializes  the subsystems specified by flags
 *  Unless the SDL_INIT_NOPARACHUTE flag is set, it will install cleanup
 *  signal handlers for some commonly ignored fatal signals (like SIGSEGV).
 *}
function SDL_Init(flags: UInt32): SInt32;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS}  name '_SDL_Init' {$ENDIF} {$ENDIF};

{**
 *  This function initializes specific SDL subsystems
 *}
function SDL_InitSubSystem(flags: UInt32): SInt32;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_InitSubSystem' {$ENDIF} {$ENDIF};

{**
 *  This function cleans up specific SDL subsystems
 *}

procedure SDL_QuitSubSystem(flags: UInt32);
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_QuitSubSystem' {$ENDIF} {$ENDIF};

{**
 *  This function returns a mask of the specified subsystems which have
 *  previously been initialized.
 *
 *  If flags is 0, it returns a mask of all initialized subsystems.
 *}

function SDL_WasInit(flags: UInt32): UInt32;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_WasInit' {$ENDIF} {$ENDIF};

{**
 *  This function cleans up all initialized subsystems. You should
 *  call it upon all exit conditions.
 *}

procedure SDL_Quit();
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_Quit' {$ENDIF} {$ENDIF};



////////////////////////////////////////////////////////////////////////////////////////////////////////
//////////////////////        SDL_loadso.h          ////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////

{**
 *  This function dynamically loads a shared object and returns a pointer
 *  to the object handle (or NULL if there was an error).
 *  The 'sofile' parameter is a system dependent name of the object file.
 *}
function SDL_LoadObject(sofile: PChar): Pointer;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_LoadObject' {$ENDIF} {$ENDIF};

{**
 *  Given an object handle, this function looks up the address of the
 *  named function in the shared object and returns it.  This address
 *  is no longer valid after calling SDL_UnloadObject().
 *}
function SDL_LoadFunction(handle: Pointer; name: PChar): Pointer;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_LoadFunction' {$ENDIF} {$ENDIF};

{**
 *  Unload a shared object from memory.
 *}
procedure SDL_UnloadObject(handle: Pointer);
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_UnloadObject' {$ENDIF} {$ENDIF};



////////////////////////////////////////////////////////////////////////////////////////////////////////
//////////////////////        SDL_platform.h         ////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////

{**
 *  Gets the name of the platform.
 *}
function SDL_GetPlatform(): PChar;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_GetPlatform' {$ENDIF} {$ENDIF};



////////////////////////////////////////////////////////////////////////////////////////////////////////
//////////////////////         SDL_hints.h          ////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////


{/**
 *  \file SDL_hints.h
 *
 *  Official documentation for SDL configuration variables
 *
 *  This file contains functions to set and get configuration hints,
 *  as well as listing each of them alphabetically.
 *
 *  The convention for naming hints is SDL_HINT_X, where "SDL_X" is
 *  the environment variable that can be used to override the default.
 *
 *  In general these hints are just that - they may or may not be
 *  supported or applicable on any given platform, but they provide
 *  a way for an application or user to give the library a hint as
 *  to how they would like the library to work.
 */
}

const
(**
 *  \brief  A variable controlling how 3D acceleration is used to accelerate the SDL screen surface.
 *
 *  SDL can try to accelerate the SDL screen surface by using streaming
 *  textures with a 3D rendering engine.  This variable controls whether and
 *  how this is done.
 *
 *  This variable can be set to the following values:
 *    "0"       - Disable 3D acceleration
 *    "1"       - Enable 3D acceleration, using the default renderer.
 *    "X"       - Enable 3D acceleration, using X where X is one of the valid rendering drivers.  (e.g. "direct3d", "opengl", etc.)
 *
 *  By default SDL tries to make a best guess for each platform whether
 *  to use acceleration or not.
 *)
SDL_HINT_FRAMEBUFFER_ACCELERATION  = 'SDL_FRAMEBUFFER_ACCELERATION';

{/**
 *  \brief  A variable specifying which render driver to use.
 *
 *  If the application doesn't pick a specific renderer to use, this variable
 *  specifies the name of the preferred renderer.  If the preferred renderer
 *  can't be initialized, the normal default renderer is used.
 *
 *  This variable is case insensitive and can be set to the following values:
 *    "direct3d"
 *    "opengl"
 *    "opengles2"
 *    "opengles"
 *    "software"
 *
 *  The default varies by platform, but it's the first one in the list that
 *  is available on the current platform.
 */}
SDL_HINT_RENDER_DRIVER = 'SDL_RENDER_DRIVER';

{/**
 *  \brief  A variable controlling whether the OpenGL render driver uses shaders if they are available.
 *
 *  This variable can be set to the following values:
 *    "0"       - Disable shaders
 *    "1"       - Enable shaders
 *
 *  By default shaders are used if OpenGL supports them.
 */}
SDL_HINT_RENDER_OPENGL_SHADERS = 'SDL_RENDER_OPENGL_SHADERS';

{/**
 *  \brief  A variable controlling the scaling quality
 *
 *  This variable can be set to the following values:
 *    "0" or "nearest" - Nearest pixel sampling
 *    "1" or "linear"  - Linear filtering (supported by OpenGL and Direct3D)
 *    "2" or "best"    - Currently this is the same as "linear"
 *
 *  By default nearest pixel sampling is used
 */}
SDL_HINT_RENDER_SCALE_QUALITY = 'SDL_RENDER_SCALE_QUALITY';

{/**
 *  \brief  A variable controlling whether updates to the SDL screen surface should be synchronized with the vertical refresh, to avoid tearing.
 *
 *  This variable can be set to the following values:
 *    "0"       - Disable vsync
 *    "1"       - Enable vsync
 *
 *  By default SDL does not sync screen surface updates with vertical refresh.
 */}
SDL_HINT_RENDER_VSYNC = 'SDL_RENDER_VSYNC';

{/**
 *  \brief  A variable controlling whether the X11 VidMode extension should be used.
 *
 *  This variable can be set to the following values:
 *    "0"       - Disable XVidMode
 *    "1"       - Enable XVidMode
 *
 *  By default SDL will use XVidMode if it is available.
 */}
SDL_HINT_VIDEO_X11_XVIDMODE = 'SDL_VIDEO_X11_XVIDMODE';

{/**
 *  \brief  A variable controlling whether the X11 Xinerama extension should be used.
 *
 *  This variable can be set to the following values:
 *    "0"       - Disable Xinerama
 *    "1"       - Enable Xinerama
 *
 *  By default SDL will use Xinerama if it is available.
 */}
SDL_HINT_VIDEO_X11_XINERAMA = 'SDL_VIDEO_X11_XINERAMA';

{/**
 *  \brief  A variable controlling whether the X11 XRandR extension should be used.
 *
 *  This variable can be set to the following values:
 *    "0"       - Disable XRandR
 *    "1"       - Enable XRandR
 *
 *  By default SDL will not use XRandR because of window manager issues.
 */}
SDL_HINT_VIDEO_X11_XRANDR = 'SDL_VIDEO_X11_XRANDR';

{/**
 *  \brief  A variable controlling whether grabbing input grabs the keyboard
 *
 *  This variable can be set to the following values:
 *    "0"       - Grab will affect only the mouse
 *    "1"       - Grab will affect mouse and keyboard
 *
 *  By default SDL will not grab the keyboard so system shortcuts still work.
 */}
SDL_HINT_GRAB_KEYBOARD = 'SDL_GRAB_KEYBOARD';

{/**
 *  \brief Minimize your SDL_Window if it loses key focus when in Fullscreen mode. Defaults to true.
 *
 */}
SDL_HINT_VIDEO_MINIMIZE_ON_FOCUS_LOSS = 'SDL_VIDEO_MINIMIZE_ON_FOCUS_LOSS';


{/**
 *  \brief  A variable controlling whether the idle timer is disabled on iOS.
 *
 *  When an iOS app does not receive touches for some time, the screen is
 *  dimmed automatically. For games where the accelerometer is the only input
 *  this is problematic. This functionality can be disabled by setting this
 *  hint.
 *
 *  This variable can be set to the following values:
 *    "0"       - Enable idle timer
 *    "1"       - Disable idle timer
 */}
SDL_HINT_IDLE_TIMER_DISABLED = 'SDL_IOS_IDLE_TIMER_DISABLED';

{/**
 *  \brief  A variable controlling which orientations are allowed on iOS.
 *
 *  In some circumstances it is necessary to be able to explicitly control
 *  which UI orientations are allowed.
 *
 *  This variable is a space delimited list of the following values:
 *    "LandscapeLeft", "LandscapeRight", "Portrait" "PortraitUpsideDown"
 */}
SDL_HINT_ORIENTATIONS = 'SDL_IOS_ORIENTATIONS';


{/**
 *  \brief  A variable that lets you disable the detection and use of Xinput gamepad devices
 *
 *  The variable can be set to the following values:
 *    "0"       - Disable XInput timer (only uses direct input)
 *    "1"       - Enable XInput timer (the default)
 */}
SDL_HINT_XINPUT_ENABLED = 'SDL_XINPUT_ENABLED';


{/**
 *  \brief  A variable that lets you manually hint extra gamecontroller db entries
 *
 *  The variable should be newline delimited rows of gamecontroller config data, see SDL_gamecontroller.h
 *
 *  This hint must be set before calling SDL_Init(SDL_INIT_GAMECONTROLLER)
 *  You can update mappings after the system is initialized with SDL_GameControllerMappingForGUID() and SDL_GameControllerAddMapping()
 */}
SDL_HINT_GAMECONTROLLERCONFIG = 'SDL_GAMECONTROLLERCONFIG';


{/**
 *  \brief  A variable that lets you enable joystick (and gamecontroller) events even when your app is in the background.
 *
 *  The variable can be set to the following values:
 *    "0"       - Disable joystick & gamecontroller input events when the
 *                application is in the background.
 *    "1"       - Enable joystick & gamecontroller input events when the
 *                application is in the backgroumd.
 *
 *  The default value is "0".  This hint may be set at any time.
 */}
SDL_HINT_JOYSTICK_ALLOW_BACKGROUND_EVENTS = 'SDL_JOYSTICK_ALLOW_BACKGROUND_EVENTS';


{/**
 *  \brief If set to 0 then never set the top most bit on a SDL Window, even if the video mode expects it.
 *      This is a debugging aid for developers and not expected to be used by end users. The default is "1"
 *
 *  This variable can be set to the following values:
 *    "0"       - don't allow topmost
 *    "1"       - allow topmost
 */}
SDL_HINT_ALLOW_TOPMOST = 'SDL_ALLOW_TOPMOST';


{/**
 *  \brief A variable that controls the timer resolution, in milliseconds.
 *
 *  The higher resolution the timer, the more frequently the CPU services
 *  timer interrupts, and the more precise delays are, but this takes up
 *  power and CPU time.  This hint is only used on Windows 7 and earlier.
 *
 *  See this blog post for more information:
 *  http://randomascii.wordpress.com/2013/07/08/windows-timer-resolution-megawatts-wasted/
 *
 *  If this variable is set to "0", the system timer resolution is not set.
 *
 *  The default value is "1". This hint may be set at any time.
 */}
SDL_HINT_TIMER_RESOLUTION = 'SDL_TIMER_RESOLUTION';


{/**
 *  \brief A hint that specifies whether the Direct3D device is initialized for thread-safe operations.
 *
 *  By default the Direct3D device is created with thread-safety disabled.
 *
 *  This variable can be set to the following values:
 *    "0"       - disable thread-safety (faster)
 *    "1"       - enable thread-safety (slower)
 */}
SDL_HINT_RENDER_DIRECT3D_THREADSAFE = 'SDL_RENDER_DIRECT3D_THREADSAFE';


{/**
 *  \brief  An enumeration of hint priorities
 */}
type
  SDL_HintPriority = (SDL_HINT_DEFAULT, SDL_HINT_NORMAL, SDL_HINT_OVERRIDE);


{/**
 *  \brief Set a hint with a specific priority
 *
 *  The priority controls the behavior when setting a hint that already
 *  has a value.  Hints will replace existing hints of their priority and
 *  lower.  Environment variables are considered to have override priority.
 *
 *  \return SDL_TRUE if the hint was set, SDL_FALSE otherwise
 */}
function SDL_SetHintWithPriority( const name: PChar; const value: PChar; priority: SDL_HintPriority) : boolean;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_SetHintWithPriority' {$ENDIF} {$ENDIF};

{/**
 *  \brief Set a hint with normal priority
 *
 *  \return SDL_TRUE if the hint was set, SDL_FALSE otherwise
 */}
function SDL_SetHint( const name: PChar; const value: PChar) : boolean;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_SetHint' {$ENDIF} {$ENDIF};

{/**
 *  \brief Get a hint
 *
 *  \return The string value of a hint variable.
 */}
function SDL_GetHint( const name: PChar): PChar;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_GetHint' {$ENDIF} {$ENDIF};

{/**
 *  \brief Add a function to watch a particular hint
 *
 *  \param name The hint to watch
 *  \param callback The function to call when the hint value changes
 *  \param userdata A pointer to pass to the callback function
 */}
type
  TSDL_HintCallback = procedure(userdata: Pointer; const name: PChar; const oldValue: PChar; const newValue: PChar);

procedure SDL_AddHintCallback(const name: PChar; callback: TSDL_HintCallback; userdata: Pointer);
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_AddHintCallback' {$ENDIF} {$ENDIF};

{/**
 *  \brief Remove a function watching a particular hint
 *
 *  \param name The hint being watched
 *  \param callback The function being called when the hint value changes
 *  \param userdata A pointer being passed to the callback function
 */}
procedure SDL_DelHintCallback(const name: PChar; callback: TSDL_HintCallback; userdata: Pointer);
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_DelHintCallback' {$ENDIF} {$ENDIF};

{/**
 *  \brief  Clear all hints
 *
 *  This function is called during SDL_Quit() to free stored hints.
 */}
procedure SDL_ClearHints();
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_ClearHints' {$ENDIF} {$ENDIF};





////////////////////////////////////////////////////////////////////////////////////////////////////////
//////////////////////         SDL_byteorder.h          ////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////

const
  // SDL_byteorder.h constants
  // The two types of endianness
  SDL_LIL_ENDIAN = 1234;
  SDL_BIG_ENDIAN = 4321;

  SDL_BYTEORDER = SDL_LIL_ENDIAN;


////////////////////////////////////////////////////////////////////////////////////////////////////////
//////////////////////       SDL_messagebox.h       ////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////


  {**
   *  SDL_MessageBox flags. If supported will display warning icon, etc.
   *}

const
  SDL_MESSAGEBOX_ERROR        = $00000010;   {**< error dialog *}
  SDL_MESSAGEBOX_WARNING      = $00000020;   {**< warning dialog *}
  SDL_MESSAGEBOX_INFORMATION  = $00000040;   {**< informational dialog *}

type
  TSDL_MessageBoxFlags = Byte;

  {**
   *  Flags for SDL_MessageBoxButtonData.
   *}
const
  SDL_MESSAGEBOX_BUTTON_RETURNKEY_DEFAULT = $00000001;  {**< Marks the default button when return is hit *}
  SDL_MESSAGEBOX_BUTTON_ESCAPEKEY_DEFAULT = $00000002;   {**< Marks the default button when escape is hit *}

type
  TSDL_MessageBoxButtonFlags = Byte;

  {**
   *   Individual button data.
   *}
type
  PSDL_MessageBoxButtonData = ^TSDL_MessageBoxButtonData;
  TSDL_MessageBoxButtonData = record
    flags: UInt32;     {**< ::SDL_MessageBoxButtonFlags *}
    buttonid: Integer; {**< User defined button id (value returned via SDL_ShowMessageBox) *}
    text: PChar;   {**< The UTF-8 button text *}
  end;

  {**
   *  RGB value used in a message box color scheme
   *}
type
  PSDL_MessageBoxColor = ^TSDL_MessageBoxColor;
  TSDL_MessageBoxColor = record
    r, g, b: UInt8;
  end;

  PSDL_MessageBoxColorType = ^TSDL_MessageBoxColorType;
  TSDL_MessageBoxColorType = (SDL_MESSAGEBOX_COLOR_BACKGROUND,
                              SDL_MESSAGEBOX_COLOR_TEXT,
                              SDL_MESSAGEBOX_COLOR_BUTTON_BORDER,
                              SDL_MESSAGEBOX_COLOR_BUTTON_BACKGROUND,
                              SDL_MESSAGEBOX_COLOR_BUTTON_SELECTED,
                              SDL_MESSAGEBOX_COLOR_MAX);

  {**
   *  A set of colors to use for message box dialogs
   *}
type
  PSDL_MessageBoxColorScheme = ^TSDL_MessageBoxColorScheme;
  TSDL_MessageBoxColorScheme = record
    //colors: array[0..SDL_MESSAGEBOX_COLOR_MAX-1] of TSDL_MessageBoxColor;
    colors: array[0..4] of TSDL_MessageBoxColor;   //right?!
  end;

  {**
   *   MessageBox structure containing title, text, window, etc.
   *}
type
  PSDL_MessageBoxData = ^TSDL_MessageBoxData;
  TSDL_MessageBoxData = record
    flags: UInt32;             {**< SDL_MessageBoxFlags *}
    window: PSDL_Window;       {**< Parent window, can be NULL *}
    title: PChar;              {**< UTF-8 title *}
    _message: PChar;           {**< UTF-8 message text *}
    numbuttons: Integer;
    buttons: PSDL_MessageBoxButtonData;
    colorScheme: PSDL_MessageBoxColorScheme;   {**< SDL_MessageBoxColorScheme, can be NULL to use system settings *}
  end;

  {**
   *   Create a modal message box.
   *
   *   messageboxdata The SDL_MessageBoxData structure with title, text, etc.
   *   buttonid The pointer to which user id of hit button should be copied.
   *
   *   -1 on error, otherwise 0 and buttonid contains user id of button
   *   hit or -1 if dialog was closed.
   *
   *   This function should be called on the thread that created the parent
   *   window, or on the main thread if the messagebox has no parent.  It will
   *   block execution of that thread until the user clicks a button or
   *   closes the messagebox.
   *}
function SDL_ShowMessageBox(messageboxdata: PSDL_MessageBoxData; buttonid: PInt): Integer;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_ShowMessageBox' {$ENDIF} {$ENDIF};

  {**
   *   Create a simple modal message box
   *
   *   flags    SDL_MessageBoxFlags
   *   title    UTF-8 title text
   *   message  UTF-8 message text
   *   window   The parent window, or NULL for no parent
   *
   *   0 on success, -1 on error
   *
   *   SDL_ShowMessageBox
   *}
function SDL_ShowSimpleMessageBox(flags: UInt32; title: PChar; _message: PChar; window: PSDL_Window): Integer;
cdecl; external SDL_LibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDL_ShowSimpleMessageBox' {$ENDIF} {$ENDIF};



////////////////////////////////////////////////////////////////////////////////////////////////////////
//////////////////////            *** KTI ***           ////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////

type
   // SDLPantalla contiene la ventana SDL el Render donde se pinta y el zoom de los ejes X e Y
   PSDLPantalla = ^TSDLPantalla;
   TSDLPantalla = record
      Window   : PSDL_Window;
      Renderer : PSDL_Renderer;
      rx : Integer; // X donde empieza el render (para los ClipRect y el ratón) expresado en resolución lógica
      ry : Integer; // Y donde empieza el render (para los ClipRect y el ratón)
      lw : Integer; // ancho de la ventana en resolución lógica
      lh : Integer; // alto de la ventana en resolución lógica
      sx : Float;   // multiplicador para pasar de resolución lógica a resolución real
      sy : Float;   // multiplicador para pasar de resolución lógica a resolución real
      max_texture_width  : SInt32;   // ancho máximo de las texturas
      max_texture_height : SInt32;   // alto máximo de las texturas
      hardware           : Boolean;  // indica si usamos aceleración hardware
      render_name        : String;   // nombre de la VGA utilizada
   end;


function SDL_Swap32(D: Uint32): Uint32;
function SDLStreamSetup( stream : TStream ) : PSDL_RWops;
function LoadSDLBMPFromStream( Stream : TStream ) : PSDL_Surface;
procedure SaveSDLBMPToStream( SDL_Surface : PSDL_Surface; stream : TStream );
procedure SDLStreamCloseRWops( SDL_RWops : PSDL_RWops );







//******************************************************************************
//******************************************************************************
//******************************************************************************
//******************************************************************************
//******************************************************************************






implementation

////////////////////////////////////////////////////////////////////////////////////////////////////////
//////////////////////        SDL_version.h         ////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////

procedure SDL_VERSION(x: PSDL_Version);
begin
     x.major := SDL_MAJOR_VERSION;
     x.minor := SDL_MINOR_VERSION;
     x.patch := SDL_PATCHLEVEL;
end;

//******************************************************************************

function SDL_VERSIONNUM(X,Y,Z: UInt32): Cardinal;
begin
     Result := X*1000 + Y*100 + Z;
end;

//******************************************************************************

function SDL_COMPILEDVERSION(): Cardinal;
begin
     Result := SDL_VERSIONNUM(SDL_MAJOR_VERSION,
                              SDL_MINOR_VERSION,
                              SDL_PATCHLEVEL);
end;

//******************************************************************************

function SDL_VERSION_ATLEAST(X,Y,Z: Cardinal): Boolean;
begin
     Result := SDL_COMPILEDVERSION >= SDL_VERSIONNUM(X,Y,Z);
end;



////////////////////////////////////////////////////////////////////////////////////////////////////////
//////////////////////        SDL_thread .h         ////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////

{$IFDEF MSWINDOWS}
   function SDL_CreateThread(fn: TSDL_ThreadFunction; name: PChar; data: Pointer): PSDL_Thread; overload;
   begin
        Result := SDL_CreateThread(fn,name,data,nil,nil);
   end;
{$ENDIF}



////////////////////////////////////////////////////////////////////////////////////////////////////////
//////////////////////          SDL_rect.h          ////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////

function SDL_RectEmpty(X: TSDL_Rect): Boolean;
begin
     Result := (X.w <= 0) or (X.h <= 0);
end;

//******************************************************************************

function SDL_RectEquals(A: TSDL_Rect; B: TSDL_Rect): Boolean;
begin
     Result := (A.x = B.x) and (A.y = B.y) and (A.w = B.w) and (A.h = B.h);
end;



////////////////////////////////////////////////////////////////////////////////////////////////////////
//////////////////////         SDL_rwops.h          ////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////

function SDL_RWsize(ctx: PSDL_RWops): SInt64;
begin
     Result := ctx^.size(ctx);
end;

//******************************************************************************

function SDL_RWseek(ctx: PSDL_RWops; offset: SInt64; whence: SInt32): SInt64;
begin
     Result := ctx^.seek(ctx,offset,whence);
end;

//******************************************************************************

function SDL_RWtell(ctx: PSDL_RWops): SInt64;
begin
     Result := ctx^.seek(ctx, 0, RW_SEEK_CUR);
end;

//******************************************************************************

function SDL_RWread(ctx: PSDL_RWops; ptr: Pointer; size: size_t; n: size_t): size_t;
begin
     Result := ctx^.read(ctx, ptr, size, n);
end;

//******************************************************************************

function SDL_RWwrite(ctx: PSDL_RWops; ptr: Pointer; size: size_t; n: size_t): size_t;
begin
     Result := ctx^.write(ctx, ptr, size, n);
end;

//******************************************************************************

function SDL_RWclose(ctx: PSDL_RWops): SInt32;
begin
     Result := ctx^.close(ctx);
end;



////////////////////////////////////////////////////////////////////////////////////////////////////////
//////////////////////           SDL_audio.h         ///////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////

function SDL_LoadWAV(_file: PChar; spec: PSDL_AudioSpec; audio_buf: PPUInt8; audio_len: PUInt32): PSDL_AudioSpec;
begin
     Result := SDL_LoadWAV_RW(SDL_RWFromFile(_file, 'rb'), 1, spec, audio_buf, audio_len);
end;

//******************************************************************************

function SDL_AUDIO_BITSIZE(x: Cardinal): Cardinal;
begin
     Result := x and SDL_AUDIO_MASK_BITSIZE;
end;

//******************************************************************************

function SDL_AUDIO_ISFLOAT(x: Cardinal): Cardinal;
begin
     Result := x and SDL_AUDIO_MASK_DATATYPE;
end;

//******************************************************************************

function SDL_AUDIO_ISBIGENDIAN(x: Cardinal): Cardinal;
begin
     Result := x and SDL_AUDIO_MASK_ENDIAN;
end;

//******************************************************************************

function SDL_AUDIO_ISSIGNED(x: Cardinal): Cardinal;
begin
     Result := x and SDL_AUDIO_MASK_SIGNED;
end;

//******************************************************************************

function SDL_AUDIO_ISINT(x: Cardinal): Cardinal;
begin
     Result := not SDL_AUDIO_ISFLOAT(x);
end;

//******************************************************************************

function SDL_AUDIO_ISLITTLEENDIAN(x: Cardinal): Cardinal;
begin
     Result := not SDL_AUDIO_ISLITTLEENDIAN(x);
end;

//******************************************************************************

function SDL_AUDIO_ISUNSIGNED(x: Cardinal): Cardinal;
begin
     Result := not SDL_AUDIO_ISSIGNED(x);
end;



////////////////////////////////////////////////////////////////////////////////////////////////////////
//////////////////////          SDL_pixels.h         ///////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////

function SDL_PIXELFLAG(X: Cardinal): Boolean;
begin
     Result := (X shr 28) = $0F;
end;

//******************************************************************************

function SDL_PIXELTYPE(X: Cardinal): Boolean;
begin
     Result := (X shr 24) = $0F;
end;

//******************************************************************************

function SDL_PIXELORDER(X: Cardinal): Boolean;
begin
     Result := (X shr 20) = $0F;
end;

//******************************************************************************

function SDL_PIXELLAYOUT(X: Cardinal): Boolean;
begin
     Result := (X shr 16) = $0F;
end;

//******************************************************************************

function SDL_BITSPERPIXEL(X: Cardinal): Boolean;
begin
     Result := (X shr 8) = $FF;
end;

//******************************************************************************

function SDL_IsPixelFormat_FOURCC(format: Variant): Boolean;
begin
     {* The flag is set to 1 because 0x1? is not in the printable ASCII range *}
     Result := format and SDL_PIXELFLAG(format) <> 1;
end;

function SDL_BYTESPERPIXEL(X: Integer): Integer;
begin
  if SDL_ISPIXELFORMAT_FOURCC(X) then
  begin
    if (X = SDL_PIXELFORMAT_YUY2) or (X = SDL_PIXELFORMAT_UYVY) or (X = SDL_PIXELFORMAT_YVYU) then
      Result := 2
    else
      Result := 1;
  end
  else
    Result := X and $FF;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////
//////////////////////          SDL_surface.h         //////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////

function SDL_LoadBMP(_file: PChar): PSDL_Surface;
begin
     Result := SDL_LoadBMP_RW(SDL_RWFromFile(_file, 'rb'), 1);
end;

//******************************************************************************

function SDL_SaveBMP(surface: PSDL_Surface; _file: PChar): SInt32;
begin
     Result := SDL_SaveBMP_RW(surface, SDL_RWFromFile(_file, 'wb'), 1);
end;



////////////////////////////////////////////////////////////////////////////////////////////////////////
//////////////////////        SDL_video.h           ////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////

function SDL_WindowPos_IsUndefined(X: Variant): Variant;
begin
     Result := (X and $FFFF0000) = SDL_WINDOWPOS_UNDEFINED_MASK;
end;

//******************************************************************************

function SDL_WindowPos_IsCentered(X: Variant): Variant;
begin
     Result := (X and $FFFF0000) = SDL_WINDOWPOS_CENTERED_MASK;
end;



////////////////////////////////////////////////////////////////////////////////////////////////////////
//////////////////////        SDL_events.h          ////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////

function SDL_GetEventState(type_: UInt32): UInt8;
begin
     Result := SDL_EventState(type_, SDL_QUERY);
end;








////////////////////////////////////////////////////////////////////////////////////////////////////////
//////////////////////            *** KTI ***           ////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////


function SDL_Swap32(D: Uint32): Uint32;
begin
     Result := ((D shl 24) or ((D shl 8) and $00FF0000) or ((D shr 8) and $0000FF00) or (D shr 24));
end;

//******************************************************************************

function SdlStreamSeek( context : PSDL_RWops; offset : SInt64; whence : SInt32 ) : SInt64; cdecl;
var
   stream : TStream;
   origin : Word;
begin
     stream := TStream( context.unknown );
     if ( stream = nil ) then
        raise EInvalidContainer.Create( 'SDLStreamSeek on nil' );
     case whence of
       0 : origin := soFromBeginning; //	Offset is from the beginning of the resource. Seek moves to the position Offset. Offset must be >= 0.
       1 : origin := soFromCurrent; //	Offset is from the current position in the resource. Seek moves to Position + Offset.
       2 : origin := soFromEnd;
     else
       origin := soFromBeginning; // just in case
     end;
     Result := stream.Seek( offset, origin );
end;

//******************************************************************************

function SDLStreamWrite( context : PSDL_RWops; const Ptr : Pointer;  size : size_t; num : size_t ) : size_t; cdecl;
var
   stream : TStream;
begin
     stream := TStream( context.unknown );
     if ( stream = nil ) then
       raise EInvalidContainer.Create( 'SDLStreamWrite on nil' );
     try
       Result := stream.Write( Ptr^, Size * num ) div size;
     except
       Result := 0;
     end;
end;

//******************************************************************************

function SdlStreamRead( context : PSDL_RWops; Ptr : Pointer; size : size_t; maxnum : size_t ) : size_t; cdecl;
var
   stream : TStream;
begin
     stream := TStream( context.unknown );
     if ( stream = nil ) then
       raise EInvalidContainer.Create( 'SDLStreamRead on nil' );
     try
       Result := stream.read( Ptr^, Size * maxnum ) div size;
     except
       Result := 0;
     end;
end;

//******************************************************************************

function SDLStreamClose( context : PSDL_RWops ) : Integer; cdecl;
var
   stream : TStream;
begin
     stream := TStream( context.unknown );
     if ( stream = nil ) then
        raise EInvalidContainer.Create( 'SDLStreamClose on nil' );
     stream.Free;
     Result := 1;
end;

//******************************************************************************

function SDLStreamSetup( stream : TStream ) : PSDL_RWops;
begin
     result := SDL_AllocRW;
     if ( result = nil ) then
       raise EInvalidContainer.Create( 'could not create SDLStream on nil' );
     result.unknown := TUnknown( stream );
     result.seek    := SDLStreamSeek;
     result.read    := SDLStreamRead;
     result.write   := SDLStreamWrite;
     result.close   := SDLStreamClose;
     Result._type   := 2; // TUnknown
end;

//******************************************************************************

procedure SDLStreamCloseRWops( SDL_RWops : PSDL_RWops );
begin
     // this only closes the SDL part of the stream, not the context
     SDL_FreeRW( SDL_RWops );
end;

//******************************************************************************

function LoadSDLBMPFromStream( stream : TStream ) : PSDL_Surface;
var
   SDL_RWops : PSDL_RWops;
begin
     SDL_RWops := SDLStreamSetup( stream );
     result := SDL_LoadBMP_RW( SDL_RWops, 0 );
     SDLStreamCloseRWops( SDL_RWops );
end;

//******************************************************************************

procedure SaveSDLBMPToStream( SDL_Surface : PSDL_Surface; stream : TStream );
var
   SDL_RWops : PSDL_RWops;
begin
     SDL_RWops := SDLStreamSetup( stream );
     SDL_SaveBMP_RW( SDL_Surface, SDL_RWops, 0 );
     SDLStreamCloseRWops( SDL_RWops );
     stream.Position := 0;
end;

end.

