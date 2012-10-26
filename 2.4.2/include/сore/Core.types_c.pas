(* ************************************************************************

  ** Converted with C to Pascal Converter 2.0
  ** Release: 2.1.4.2012
  ** Email: al_gun@ncable.net.au
  ** Updates: http://cc.codegear.com/Author/302259
  ** Blogs: http://delphiprogrammingwithalgun.blogspot.com/
  ** Copyright (c) 2005, 2012 Ural Gunaydin (a.k.a. Al Gun)

  ************************************************************************* *)

unit Core.types_c;

{$ifdef DEBUG}
{$A8,B-,C+,D+,E-,F-,G+,H+,I+,J-,K-,L+,M-,N+,O-,P+,Q+,R+,S-,T-,U-,V+,W+,X+,Y+,Z1}
{$else}
{$A8,B-,C-,D-,E-,F-,G+,H+,I+,J-,K-,L-,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y-,Z1}
{$endif}
{$WARN SYMBOL_DEPRECATED OFF}
{$WARN SYMBOL_PLATFORM OFF}
{$WARN UNIT_PLATFORM OFF}
{$WARN UNSAFE_TYPE OFF}
{$WARN UNSAFE_CODE OFF}
{$WARN UNSAFE_CAST OFF}

interface

(*
  ** 'C2PTypes.pas' declares external windows data types for the conversion purposes.
  ** It's created by the CtoPas converter and saved under
  ** "\Program Files\Common Files\AlGun Shared\CToPas 2.0\P_Files" folder.
  ** Consult the Windows and Delphi help files for more information about defined data types
*)

// uses
// {C2PTypes,} Windows, Messages, SysUtils, Classes;

(* M///////////////////////////////////////////////////////////////////////////////////////
  //
  //  IMPORTANT: READ BEFORE DOWNLOADING, COPYING, INSTALLING OR USING.
  //
  //  By downloading, copying, installing or using the software you agree to this license.
  //  If you do not agree to this license, do not download, install,
  //  copy or use the software.
  //
  //
  //                          License Agreement
  //                For Open Source Computer Vision Library
  //
  // Copyright (C) 2000-2008, Intel Corporation, all rights reserved.
  // Copyright (C) 2009, Willow Garage Inc., all rights reserved.
  // Third party copyrights are property of their respective owners.
  //
  // Redistribution and use in source and binary forms, with or without modification,
  // are permitted provided that the following conditions are met:
  //
  //   * Redistribution's of source code must retain the above copyright notice,
  //     this list of conditions and the following disclaimer.
  //
  //   * Redistribution's in binary form must reproduce the above copyright notice,
  //     this list of conditions and the following disclaimer in the documentation
  //     and/or other materials provided with the distribution.
  //
  //   * The name of the copyright holders may not be used to endorse or promote products
  //     derived from this software without specific prior written permission.
  //
  // This software is provided by the copyright holders and contributors "as is" and
  // any express or implied warranties, including, but not limited to, the implied
  // warranties of merchantability and fitness for a particular purpose are disclaimed.
  // In no event shall the Intel Corporation or contributors be liable for any direct,
  // indirect, incidental, special, exemplary, or consequential damages
  // (including, but not limited to, procurement of substitute goods or services;
  // loss of use, data, or profits; or business interruption) however caused
  // and on any theory of liability, whether in contract, strict liability,
  // or tort (including negligence or otherwise) arising in any way out of
  // the use of this software, even if advised of the possibility of such damage.
  //
  //M *)

{$IFNDEF __OPENCV_CORE_TYPES_H__}
{$DEFINE __OPENCV_CORE_TYPES_H__}
{$IFNDEF  _CRT_SECURE_NO_DEPRECATE && defined _MSC_VER}
{$HPPEMIT '#  if _MSC_VER > 1300'}
{$HPPEMIT '#    define _CRT_SECURE_NO_DEPRECATE'}	(* to avoid multiple Visual Studio 2005 warnings *)
{$HPPEMIT '#  endif'}
{$ENDIF}
{$IFNDEF SKIP_INCLUDES}
{$HPPEMIT '#include <assert.h>'}
{$HPPEMIT '#include <stdlib.h>'}
{$HPPEMIT '#include <cString.h>'}
{$HPPEMIT '#include <float.h>'}
{$IFNDEF  _MSC_VER && !defined __BORLANDC__}
{$HPPEMIT '#  include <stdint.h>'}
{$ENDIF}
{$IFNDEF  __ICL}
{$HPPEMIT '#  define CV_ICC   __ICL'}
{$HPPEMIT '#elif defined __ICC'}
{$HPPEMIT '#  define CV_ICC   __ICC'}
{$HPPEMIT '#elif defined __ECL'}
{$HPPEMIT '#  define CV_ICC   __ECL'}
{$HPPEMIT '#elif defined __ECC'}
{$HPPEMIT '#  define CV_ICC   __ECC'}
{$HPPEMIT '#elif defined __INTEL_COMPILER'}
{$HPPEMIT '#  define CV_ICC   __INTEL_COMPILER'}
{$ENDIF}
{$IFNDEF CV_ICC && !defined CV_ENABLE_UNROLLED}
{$HPPEMIT '#  define CV_ENABLE_UNROLLED 0'}
{$ELSE}
{$HPPEMIT '#  define CV_ENABLE_UNROLLED 1'}
{$ENDIF}
{$IFNDEF  _M_X64 && defined _MSC_VER && _MSC_VER >= 1400 || __GNUC__ >= 4 && defined __x86_64__}
{$HPPEMIT '#  if defined WIN32'}
{$HPPEMIT '#    include <intrin.h>'}
{$HPPEMIT '#  endif'}
{$HPPEMIT '#  if defined __SSE2__ || !defined __GNUC__'}
{$HPPEMIT '#    include <emmintrin.h>'}
{$HPPEMIT '#  endif'}
{$ENDIF}
{$IFNDEF  __BORLANDC__}
{$HPPEMIT '#  include <fastmath.h>'}
{$ELSE}
{$HPPEMIT '#  include <math.h>'}
{$ENDIF}
{$IFDEF HAVE_IPL}
{$HPPEMIT '#  ifndef __IPL_H__'}
{$HPPEMIT '#    if defined WIN32 || defined _WIN32'}
{$HPPEMIT '#      include <ipl.h>'}
{$HPPEMIT '#    else'}
{$HPPEMIT '#      include <ipl/ipl.h>'}
{$HPPEMIT '#    endif'}
{$HPPEMIT '#  endif'}
{$HPPEMIT '#elif defined __IPL_H__'}
{$HPPEMIT '#  define HAVE_IPL'}
{$ENDIF}
{$ENDIF}	// SKIP_INCLUDES

{$IFNDEF  WIN32 || defined _WIN32}
{$HPPEMIT '#  define CV_CDECL __cdecl'}
{$HPPEMIT '#  define CV_STDCALL __stdcall'}
{$ELSE}
{$HPPEMIT '#  define CV_CDECL'}
{$HPPEMIT '#  define CV_STDCALL'}
{$ENDIF}
{$IFNDEF CV_EXTERN_C}
{$HPPEMIT '#  ifdef __cplusplus'}
{$HPPEMIT '#    define CV_EXTERN_C'}	// extern "C"
{$HPPEMIT '#    define CV_DEFAULT(val) = val'}
{$HPPEMIT '#  else'}
{$HPPEMIT '#    define CV_EXTERN_C'}
{$HPPEMIT '#    define CV_DEFAULT(val)'}
{$HPPEMIT '#  endif'}
{$ENDIF}
{$IFNDEF CV_EXTERN_C_FUNCPTR}
{$HPPEMIT '#  ifdef __cplusplus'}
{$HPPEMIT '#    define CV_EXTERN_C_FUNCPTR(x)'}	// extern "C" { typedef x; }
{$HPPEMIT '#  else'}
{$HPPEMIT '#    define CV_EXTERN_C_FUNCPTR(x) typedef x'}
{$HPPEMIT '#  endif'}
{$ENDIF}
{$IFNDEF CV_INLINE}
{$HPPEMIT '#  if defined __cplusplus'}
{$HPPEMIT '#    define CV_INLINE'}
{$HPPEMIT '#  elif (defined WIN32 || defined _WIN32 || defined WINCE) && !defined __GNUC__'}
{$HPPEMIT '#    define CV_INLINE __inline'}
{$HPPEMIT '#  else'}
{$HPPEMIT '#    define CV_INLINE static'}
{$HPPEMIT '#  endif'}
{$ENDIF}	(* CV_INLINE *)
{$IFNDEF  WIN32 || defined _WIN32 || defined WINCE && defined CVAPI_EXPORTS}
{$HPPEMIT '#  define CV_EXPORTS __declspec(dllexport)'}
{$ELSE}
{$HPPEMIT '#  define CV_EXPORTS'}
{$ENDIF}
{$IFNDEF CVAPI}
{$HPPEMIT '#  define CVAPI(rettype) CV_EXTERN_C CV_EXPORTS rettype CV_CDECL'}
{$ENDIF}
{$IFNDEF  _MSC_VER || defined __BORLANDC__}
// type
// int64 = int64;
// {$EXTERNALSYM int64}

Type
  pCVChar = pAnsiChar;
  ppCVChar = ^pCVChar;
  CVChar = AnsiChar;

type
  uint64 = int64;
{$EXTERNALSYM uint64}
{$HPPEMIT '#  define CV_BIG_function (n)   n##I64' }
{$HPPEMIT '#  define CV_BIG_UINT(n)  n##UI64'}
{$ELSE}

type
  int64 = int64_t;
{$EXTERNALSYM int64}

type
  uint64 = uint64_t;
{$EXTERNALSYM uint64}
$ HPPEMIT '#  define CV_BIG_INT(n)   n##LL' }
{$HPPEMIT '#  define CV_BIG_UINT(n)  n##ULL'}
{$ENDIF}
{$IFNDEF HAVE_IPL}
  type uchar = Byte;
{$EXTERNALSYM uchar}

type
  ushort = Word;
{$EXTERNALSYM ushort}
{$ENDIF }

type
  schar = ShortInt;
  pschar = ^schar;

{$EXTERNALSYM schar}
  (* special informative macros for wrapper generators *)
{$DEFINE CV_CARRAY(counter)}
{$DEFINE CV_CUSTOM_CARRAY(args)}
  // const
  // CV_EXPORTS_W = CV_EXPORTS;
  // {$EXTERNALSYM CV_EXPORTS_W}
  // const
  // CV_EXPORTS_W_SIMPLE = CV_EXPORTS;
  // {$EXTERNALSYM CV_EXPORTS_W_SIMPLE}

  // >> Following declaration is a macro definition!
  // const
  // CV_EXPORTS_AS(synonym)CV_EXPORTS;

  // const
  // CV_EXPORTS_W_MAP = CV_EXPORTS;
  // {$EXTERNALSYM CV_EXPORTS_W_MAP}
{$DEFINE CV_IN_OUT}
{$DEFINE CV_OUT}
{$DEFINE CV_PROP}
{$DEFINE CV_PROP_RW}
{$DEFINE CV_WRAP}
{$DEFINE CV_WRAP_AS(synonym)}
{$DEFINE CV_WRAP_DEFAULT(value)}

  (* CvArr* is used to pass arbitrary
    * cArray-like data structures
    * into functions where the particular
    * cArray cType is recognized at runtime:
  *)
type
  CvArr = Pointer;
  TCvArr = CvArr;
  pCvArr = ^TCvArr;
{$EXTERNALSYM CvArr}

type
  Cv32suf = packed record
    i: Integer;
    u: Cardinal;
    f: Single;
  end;

  Cv64suf = packed record
    i: int64;
    u: uint64;
    f: Double;
  end;

  CVStatus = Integer;
{$EXTERNALSYM CVStatus}

const
  CV_StsOk = 0; (* everithing is ok *)
  CV_StsBackTrace = -1; (* pseudo error for back trace *)
  CV_StsError = -2; (* unknown /unspecified error *)
  CV_StsInternal = -3; (* internal error (bad state) *)
  CV_StsNoMem = -4; (* insufficient memory *)
  CV_StsBadArg = -5; (* function arg/param is bad *)
  CV_StsBadFunc = -6; (* unsupported function *)
  CV_StsNoConv = -7; (* iter. didn't converge *)
  CV_StsAutoTrace = -8; (* tracing *)
  CV_HeaderIsNull = -9; (* image header is 0 *)
  CV_BadImageSize = -10; (* image size is invalid *)
  CV_BadOffset = -11; (* offset is invalid *)
  CV_BadDataPtr = -12; (* *)
  CV_BadStep = -13; (* *)
  CV_BadModelOrChSeq = -14; (* *)
  CV_BadNumChannels = -15; (* *)
  CV_BadNumChannel1U = -16; (* *)
  CV_BadDepth = -17; (* *)
  CV_BadAlphaChannel = -18; (* *)
  CV_BadOrder = -19; (* *)
  CV_BadOrigin = -20; (* *)
  CV_BadAlign = -21; (* *)
  CV_BadCallBack = -22; (* *)
  CV_BadTileSize = -23; (* *)
  CV_BadCOI = -24; (* *)
  CV_BadROISize = -25; (* *)
  CV_MaskIsTiled = -26; (* *)
  CV_StsNullPtr = -27; (* null pointer *)
  CV_StsVecLengthErr = -28; (* incorrect vector length *)
  CV_StsFilterStructContentErr = -29; (* incorr. filter structure content *)
  CV_StsKernelStructContentErr = -30; (* incorr. transform kernel content *)
  CV_StsFilterOffsetErr = -31; (* incorrect filter ofset value *)
  CV_StsBadSize = -201; (* the input/output structure size is incorrect *)
  CV_StsDivByZero = -202; (* division by zero *)
  CV_StsInplaceNotSupported = -203; (* in-place operation is not supported *)
  CV_StsObjectNotFound = -204; (* request can't be completed *)
  CV_StsUnmatchedFormats = -205; (* formats of input/output arrays differ *)
  CV_StsBadFlag = -206; (* flag is wrong or not supported *)
  CV_StsBadPoint = -207; (* bad CvPoint *)
  CV_StsBadMask = -208; (* bad format of mask (neither 8uC1 nor 8sC1) *)
  CV_StsUnmatchedSizes = -209; (* sizes of input/output structures do not match *)
  CV_StsUnsupportedFormat = -210; (* the data format/type is not supported by the function *)
  CV_StsOutOfRange = -211; (* some of parameters are out of range *)
  CV_StsParseError = -212; (* invalid syntax/structure of the parsed file *)
  CV_StsNotImplemented = -213; (* the requested function/feature is not implemented *)
  CV_StsBadMemBlock = -214; (* an allocated block has been corrupted *)
  CV_StsAssert = -215; (* assertion failed *)
  CV_GpuNotSupported = -216;
  CV_GpuApiCallError = -217;
  CV_OpenGlNotSupported = -218;
  CV_OpenGlApiCallError = -219;

  (* ***************************************************************************************\
    *                             Common macros and  functions                         *
    *************************************************************************************** *)

{$IFDEF HAVE_TEGRA_OPTIMIZATION}
{$HPPEMIT '#  include 'tegra_round.hpp''}
{$ENDIF}

const
  CV_PI = 3.1415926535897932384626433832795;
{$EXTERNALSYM CV_PI}
  CV_LOG2 = 0.69314718055994530941723212145818;
{$EXTERNALSYM CV_LOG2}

  { ************** Random number generation ****************** }
type
  TCvRNG = uint64;
{EXTERNALSYM CvRNG}

const
  CV_RNG_COEFF = Cardinal(4164903690);
{$EXTERNALSYM CV_RNG_COEFF}
  (* ***************************************************************************************\
    *                                  Image cType (IplImage)                                 *
    *************************************************************************************** *)

{$IFNDEF HAVE_IPL}

  (*
    {$HPPEMIT '* The following definitions (until #endif)'}
    * is an extract from IPL headers.
    * Copyright (c) 1995 Intel Corporation.
  *)
const
  IPL_DEPTH_SIGN = $80000000;
{$EXTERNALSYM IPL_DEPTH_SIGN}
  IPL_DEPTH_1U = 1;
{$EXTERNALSYM IPL_DEPTH_1U}
  IPL_DEPTH_8U = 8;
{$EXTERNALSYM IPL_DEPTH_8U}
  IPL_DEPTH_16U = 16;
{$EXTERNALSYM IPL_DEPTH_16U}
  IPL_DEPTH_32F = 32;
{$EXTERNALSYM IPL_DEPTH_32F}
  IPL_DEPTH_8S: TCvRNG = (IPL_DEPTH_SIGN or 8);
{$EXTERNALSYM IPL_DEPTH_8S}
  IPL_DEPTH_16S = (IPL_DEPTH_SIGN or 16);
{$EXTERNALSYM IPL_DEPTH_16S}
  IPL_DEPTH_32S = (IPL_DEPTH_SIGN or 32);
{$EXTERNALSYM IPL_DEPTH_32S}
  IPL_DATA_ORDER_PIXEL = 0;
{$EXTERNALSYM IPL_DATA_ORDER_PIXEL}
  IPL_DATA_ORDER_PLANE = 1;
{$EXTERNALSYM IPL_DATA_ORDER_PLANE}
  IPL_ORIGIN_TL = 0;
{$EXTERNALSYM IPL_ORIGIN_TL}
  IPL_ORIGIN_BL = 1;
{$EXTERNALSYM IPL_ORIGIN_BL}
  IPL_ALIGN_4BYTES = 4;
{$EXTERNALSYM IPL_ALIGN_4BYTES}
  IPL_ALIGN_8BYTES = 8;
{$EXTERNALSYM IPL_ALIGN_8BYTES}
  IPL_ALIGN_16BYTES = 16;
{$EXTERNALSYM IPL_ALIGN_16BYTES}
  IPL_ALIGN_32BYTES = 32;
{$EXTERNALSYM IPL_ALIGN_32BYTES}
  IPL_ALIGN_DWORD = IPL_ALIGN_4BYTES;
{$EXTERNALSYM IPL_ALIGN_DWORD}
  IPL_ALIGN_QWORD = IPL_ALIGN_8BYTES;
{$EXTERNALSYM IPL_ALIGN_QWORD}
  IPL_BORDER_CONSTANT = 0;
{$EXTERNALSYM IPL_BORDER_CONSTANT}
  IPL_BORDER_REPLICATE = 1;
{$EXTERNALSYM IPL_BORDER_REPLICATE}
  IPL_BORDER_REFLECT = 2;
{$EXTERNALSYM IPL_BORDER_REFLECT}
  IPL_BORDER_WRAP = 3;
{$EXTERNALSYM IPL_BORDER_WRAP}

type

  pIplImage = ^TIplImage;
  ppIplImage = ^pIplImage;
  pIplROI = ^TIplROI;
  pIplTileInfo = ^TIplTileInfo;

  TIplROI = packed record
    coi: Integer; (* 0 - no COI (all channels are selected), 1 - 0th channel is selected ... *)
    xOffset: Integer;
    yOffset: Integer;
    width: Integer;
    height: Integer;
  end;

  TiplCallBack = procedure(img: pIplImage; xIndex: Integer; yIndex: Integer; mode: Integer);

  TIplTileInfo = packed record
    callBack: TiplCallBack;
    id: Pointer;
    tileData: pCVChar;
    width: Integer;
    height: Integer;
  end;

  TIplImage = packed record
    nSize: Integer; (* sizeof(IplImage) *)
    id: Integer; (* version (=0) *)
    nChannels: Integer; (* Most of OpenCV functions support 1,2,3 or 4 channels *)
    alphaChannel: Integer; (* Ignored by OpenCV *)
    depth: Integer; (* Pixel depth in bits: IPL_DEPTH_8U, IPL_DEPTH_8S, IPL_DEPTH_16S, *)
    colorModel: array [0 .. 3] of CVChar; (* Ignored by OpenCV *)
    channelSeq: array [0 .. 3] of CVChar; (* ditto *)
    dataOrder: Integer; (* 0 - interleaved color channels, 1 - separate color channels. *)
    origin: Integer; (* 0 - top-left origin, *)
    align: Integer; (* Alignment of image rows (4 or 8). *)
    width: Integer; (* Image width in pixels. *)
    height: Integer; (* Image height in pixels. *)
    roi: pIplROI; (* Image ROI. If NULL, the whole image is selected. *)
    maskROI: pIplImage; (* Must be NULL. *)
    imageId: Pointer; (* "           " *)
    tileInfo: pIplTileInfo; (* "           " *)
    imageSize: Integer; (* Image data size in bytes *)
    imageData: pCVChar; (* Pointer to aligned image data. *)
    widthStep: Integer; (* Size of aligned image row in bytes. *)
    BorderMode: array [0 .. 3] of Integer; (* Ignored by OpenCV. *)
    BorderConst: array [0 .. 3] of Integer; (* Ditto. *)
    imageDataOrigin: pCVChar; (* Pointer to very origin of image data *)
  end;

  // type       _IplTileInfo IplTileInfo = ;
  pIplConvKernel=^TIplConvKernel;
  TIplConvKernel = packed record
    nCols: Integer;
    nRows: Integer;
    anchorX: Integer;
    anchorY: Integer;
    values: ^Integer;
    nShiftR: Integer;
  end;

  IplConvKernelFP = packed record
    nCols: Integer;
    nRows: Integer;
    anchorX: Integer;
    anchorY: Integer;
    values: ^Single;
  end;

const
  IPL_IMAGE_HEADER = 1;
{$EXTERNALSYM IPL_IMAGE_HEADER}
  IPL_IMAGE_DATA = 2;
{$EXTERNALSYM IPL_IMAGE_DATA}
  IPL_IMAGE_ROI = 4;
{$EXTERNALSYM IPL_IMAGE_ROI}
{$ENDIF}
  (* HAVE_IPL *)
  (* extra border mode *)
  IPL_BORDER_REFLECT_101 = 4;
{$EXTERNALSYM IPL_BORDER_REFLECT_101}
  IPL_BORDER_TRANSPARENT = 5;
{$EXTERNALSYM IPL_BORDER_TRANSPARENT}
  IPL_IMAGE_MAGIC_VAL = SizeOf(TIplImage);
{$EXTERNALSYM IPL_IMAGE_MAGIC_VAL}
  CV_TYPE_NAME_IMAGE = 'opencv-image';

  (* ***************************************************************************************\
    *                                  Matrix cType (CvMat)                                   *
    *************************************************************************************** *)

const
  CV_CN_MAX = 512;
{$EXTERNALSYM CV_CN_MAX}
  CV_CN_SHIFT = 3;
{$EXTERNALSYM CV_CN_SHIFT}
  CV_DEPTH_MAX = (1 shl CV_CN_SHIFT);
{$EXTERNALSYM CV_DEPTH_MAX}
  CV_8U = 0;
{$EXTERNALSYM CV_8U}
  CV_8S = 1;
{$EXTERNALSYM CV_8S}
  CV_16U = 2;
{$EXTERNALSYM CV_16U}
  CV_16S = 3;
{$EXTERNALSYM CV_16S}
  CV_32S = 4;
{$EXTERNALSYM CV_32S}
  CV_32F = 5;
{$EXTERNALSYM CV_32F}
  CV_64F = 6;
{$EXTERNALSYM CV_64F}
  CV_USRTYPE1 = 7;
{$EXTERNALSYM CV_USRTYPE1}
  CV_MAT_DEPTH_MASK = (CV_DEPTH_MAX - 1);
{$EXTERNALSYM CV_MAT_DEPTH_MASK}

  (*
    const
    CV_8UC1 = CV.CV_MAKETYPE(CV_8U, 1);
    {$EXTERNALSYM CV_8UC1}
    CV_8UC2 = CV.CV_MAKETYPE(CV_8U, 2);
    {$EXTERNALSYM CV_8UC2}
    CV_8UC3 = CV.CV_MAKETYPE(CV_8U, 3);
    {$EXTERNALSYM CV_8UC3}
    CV_8UC4 = CV.CV_MAKETYPE(CV_8U, 4);
    {$EXTERNALSYM CV_8UC4}
    CV_8SC1 = CV.CV_MAKETYPE(CV_8S, 1);
    {$EXTERNALSYM CV_8SC1}
    CV_8SC2 = CV.CV_MAKETYPE(CV_8S, 2);
    {$EXTERNALSYM CV_8SC2}
    CV_8SC3 = CV.CV_MAKETYPE(CV_8S, 3);
    {$EXTERNALSYM CV_8SC3}
    CV_8SC4 = CV.CV_MAKETYPE(CV_8S, 4);
    {$EXTERNALSYM CV_8SC4}
    CV_16UC1 = CV.CV_MAKETYPE(CV_16U, 1);
    {$EXTERNALSYM CV_16UC1}
    CV_16UC2 = CV.CV_MAKETYPE(CV_16U, 2);
    {$EXTERNALSYM CV_16UC2}
    CV_16UC3 = CV_MAKETYPE(CV_16U, 3);
    {$EXTERNALSYM CV_16UC3}
    CV_16UC4 = CV_MAKETYPE(CV_16U, 4);
    {$EXTERNALSYM CV_16UC4}
    CV_16SC1 = CV_MAKETYPE(CV_16S, 1);
    {$EXTERNALSYM CV_16SC1}

    const
    CV_16SC2 = CV_MAKETYPE(CV_16S, 2);
    {$EXTERNALSYM CV_16SC2}

    const
    CV_16SC3 = CV_MAKETYPE(CV_16S, 3);
    {$EXTERNALSYM CV_16SC3}

    const
    CV_16SC4 = CV_MAKETYPE(CV_16S, 4);
    {$EXTERNALSYM CV_16SC4}

    // >> Following declaration is a macro definition!
    const
    CV_16SC(n)CV_MAKETYPE(CV_16S, (n));

    const
    CV_32SC1 = CV_MAKETYPE(CV_32S, 1);
    {$EXTERNALSYM CV_32SC1}

    const
    CV_32SC2 = CV_MAKETYPE(CV_32S, 2);
    {$EXTERNALSYM CV_32SC2}

    const
    CV_32SC3 = CV_MAKETYPE(CV_32S, 3);
    {$EXTERNALSYM CV_32SC3}

    const
    CV_32SC4 = CV_MAKETYPE(CV_32S, 4);
    {$EXTERNALSYM CV_32SC4}

    // >> Following declaration is a macro definition!
    const
    CV_32SC(n)CV_MAKETYPE(CV_32S, (n));

    const
    CV_32FC1 = CV_MAKETYPE(CV_32F, 1);
    {$EXTERNALSYM CV_32FC1}

    const
    CV_32FC2 = CV_MAKETYPE(CV_32F, 2);
    {$EXTERNALSYM CV_32FC2}

    const
    CV_32FC3 = CV_MAKETYPE(CV_32F, 3);
    {$EXTERNALSYM CV_32FC3}

    const
    CV_32FC4 = CV_MAKETYPE(CV_32F, 4);
    {$EXTERNALSYM CV_32FC4}

    // >> Following declaration is a macro definition!
    const
    CV_32FC(n)CV_MAKETYPE(CV_32F, (n));

    const
    CV_64FC1 = CV_MAKETYPE(CV_64F, 1);
    {$EXTERNALSYM CV_64FC1}

    const
    CV_64FC2 = CV_MAKETYPE(CV_64F, 2);
    {$EXTERNALSYM CV_64FC2}

    const
    CV_64FC3 = CV_MAKETYPE(CV_64F, 3);
    {$EXTERNALSYM CV_64FC3}

    const
    CV_64FC4 = CV_MAKETYPE(CV_64F, 4);
    {$EXTERNALSYM CV_64FC4}

    // >> Following declaration is a macro definition!
    const
    CV_64FC(n)CV_MAKETYPE(CV_64F, (n));

    const
    CV_AUTO_STEP = $7FFFFFFF;
    {$EXTERNALSYM CV_AUTO_STEP}

    const
    CV_WHOLE_ARR = cvSlice(0, $3FFFFFFF);
    {$EXTERNALSYM CV_WHOLE_ARR}
  *)
const
  CV_MAT_CN_MASK = (CV_CN_MAX - 1) shl CV_CN_SHIFT;
{$EXTERNALSYM CV_MAT_CN_MASK}
  CV_MAT_TYPE_MASK = (CV_DEPTH_MAX * CV_CN_MAX - 1);
{$EXTERNALSYM CV_MAT_TYPE_MASK}
  CV_MAT_CONT_FLAG_SHIFT = 14;
{$EXTERNALSYM CV_MAT_CONT_FLAG_SHIFT}
  CV_MAT_CONT_FLAG = (1 shl CV_MAT_CONT_FLAG_SHIFT);
{$EXTERNALSYM CV_MAT_CONT_FLAG}
  // const
  // CV_IS_CONT_MAT = CV_IS_MAT_CONT;
  // {$EXTERNALSYM CV_IS_CONT_MAT}
  CV_SUBMAT_FLAG_SHIFT = 15;
{$EXTERNALSYM CV_SUBMAT_FLAG_SHIFT}
  CV_SUBMAT_FLAG = (1 shl CV_SUBMAT_FLAG_SHIFT);
{$EXTERNALSYM CV_SUBMAT_FLAG}
  CV_MAGIC_MASK = $FFFF0000;
{$EXTERNALSYM CV_MAGIC_MASK}
  CV_MAT_MAGIC_VAL = $42420000;
{$EXTERNALSYM CV_MAT_MAGIC_VAL}
  CV_TYPE_NAME_MAT = 'opencv-matrix';
{$EXTERNALSYM CV_TYPE_NAME_MAT}

type

  pCvMat = ^TCvMat;

  TCvMat = packed record
    _type: Integer;
    step: Integer;

    refcount: PInteger;
    hdr_refcount: Integer;

    data: Pointer;

    rows: Integer;
    cols: Integer;
  end;

  // TCvMat = packed record
  // cType: Integer;
  // step: Integer;
  // refcount: ^Integer;
  // hdr_refcount: Integer;
  // { *    case data:integer of
  // ptr: ^uchar;
  // s: ^SmallInt;
  // i: ^Integer;
  // fl: ^Single;
  // db: ^Double;
  // data = CvMat;*)
  //
  // //{$EXTERNALSYM data }
  // // {$IFDEF __cplusplus}
  // (*
  // union
  // begin
  // Integer rows;
  // Integer height;);
  //
  // union begin Integer cols; Integer width;);
  // {$ELSE}
  // Integer rows;
  // Integer cols;
  // {$ENDIF}
  // *)
  // end;

  (* ***************************************************************************************\
    *                       Multi-dimensional dense cArray (CvMatND)                          *
    *************************************************************************************** *)

const
  CV_MATND_MAGIC_VAL = $42430000;
{$EXTERNALSYM CV_MATND_MAGIC_VAL}
  CV_TYPE_NAME_MATND = 'opencv-nd-matrix';
{$EXTERNALSYM CV_TYPE_NAME_MATND}
  CV_MAX_DIM = 32;
{$EXTERNALSYM CV_MAX_DIM}
  CV_MAX_DIM_HEAP = 1024;
{$EXTERNALSYM CV_MAX_DIM_HEAP}

type
  CvMatND = packed record
    cType: Integer;
    dims: Integer;
    refcount: ^Integer;
    hdr_refcount: Integer;
    (*
      ptr: ^uchar;
      fl: ^Single;
      db: ^Double;
      i: ^Integer;
      s: ^SmallInt;
      end;

      data = CvMatND;
      {$EXTERNALSYM data}
      begin
      Integer size;
      Integer step;
      end;
      dim:
      array [0 .. CV_MAX_DIM - 1] of; *)
  end;

  (* ***************************************************************************************\
    *                      Multi-dimensional sparse cArray (CvSparseMat)                      *
    *************************************************************************************** *)
  (*
    const
    CV_SPARSE_MAT_MAGIC_VAL = $42440000;
    {$EXTERNALSYM CV_SPARSE_MAT_MAGIC_VAL}
    CV_TYPE_NAME_SPARSE_MAT = 'opencv-sparse-matrix';
    {$EXTERNALSYM CV_TYPE_NAME_SPARSE_MAT}

    type
    CvSet = packed record;

    CvSparseMat

    begin
    Integer cType;
    CvSparseMat

    begin
    Integer cType;
    Integer dims;
    Integer * refcount;
    Integer hdr_refcount;

    type
    CvSet * heap = packed record
    end;
    Pointer * hashtable;
    Integer hashsize;
    Integer valoffset;
    Integer idxoffset;
    size:
    array [0 .. CV_MAX_DIM - 1] of Integer;
    end;
    CvSparseMat;

    const
    CV_IS_SPARSE_MAT_HDR(mat)((mat) <> 0 and (((CvSparseMat(mat))^.cType and CV_MAGIC_MASK)
    = CV_SPARSE_MAT_MAGIC_VAL)

    // >> Following declaration is a macro definition!
    const CV_IS_SPARSE_MAT(mat)CV_IS_SPARSE_MAT_HDR(mat);
  *)
  (* *************** iteration through a sparse array **************** *)
  (*
    type CvSparseNode begin Cardinal hashval; CvSparseNode begin Cardinal hashval;
    type CvSparseNode * next = packed record end; end; CvSparseNode;

    type CvSparseMatIterator = packed record mat: ^CvSparseMat; node: ^CvSparseNode; curidx: Integer; end;

    // >> Following declaration is a macro definition!
    const CV_NODE_VAL(mat, node)# define CV_NODE_VAL(mat, node)((
    procedure(+(mat)^.valoffset))((Integer(uchar(node) + (mat)^.idxoffset): node));
  *)
  (* ***************************************************************************************\
    *                                         Histogram                                      *
    *************************************************************************************** *)

type
  CvHistType = Integer;
{$EXTERNALSYM CvHistType}

const
  CV_HIST_MAGIC_VAL = $42450000;
{$EXTERNALSYM CV_HIST_MAGIC_VAL}
  CV_HIST_UNIFORM_FLAG = (1 shl 10);
{$EXTERNALSYM CV_HIST_UNIFORM_FLAG}
  (* indicates whether bin ranges are set already or not *)
  CV_HIST_RANGES_FLAG = (1 shl 11);
{$EXTERNALSYM CV_HIST_RANGES_FLAG}
  CV_HIST_ARRAY = 0;
{$EXTERNALSYM CV_HIST_ARRAY}
  CV_HIST_SPARSE = 1;
{$EXTERNALSYM CV_HIST_SPARSE}
  CV_HIST_TREE = CV_HIST_SPARSE;
{$EXTERNALSYM CV_HIST_TREE}
  (* should be used as a parameter only,
    it turns to CV_HIST_UNIFORM_FLAG of hist^.cType *)
  CV_HIST_UNIFORM = 1;
{$EXTERNALSYM CV_HIST_UNIFORM}

type
  CvHistogram = packed record
    cType: Integer;
    bins: ^TCvArr;
    thresh: array [0 .. CV_MAX_DIM - 1, 0 .. 1] of Single;
    (* For uniform histograms. *)
    thresh2: ^Single; (* For non-uniform histograms. *)
    mat: CvMatND; (* Embedded matrix header for array histograms. *)
  end;

  (* ***************************************************************************************\
    *                      Other supplementary data cType definitions                         *
    *************************************************************************************** *)

  (* ************************************** CvRect **************************************** *)

type
  TCvRect = packed record
    x: Integer;
    y: Integer;
    width: Integer;
    height: Integer;
  end;

  (* ********************************** CvTermCriteria ************************************ *)

const
  CV_TERMCRIT_ITER = 1;
{$EXTERNALSYM CV_TERMCRIT_ITER}
  CV_TERMCRIT_NUMBER = CV_TERMCRIT_ITER;
{$EXTERNALSYM CV_TERMCRIT_NUMBER}
  CV_TERMCRIT_EPS = 2;
{$EXTERNALSYM CV_TERMCRIT_EPS}

type
  CvTermCriteria = packed record
    cType: Integer; (* may be combination of *)
    max_iter: Integer;
    epsilon: Double;
  end;

  (* ****************************** CvPoint and variants ********************************** *)

type
  pCvPoint = ^TCvPoint;

  TCvPoint = packed record
    x: Integer;
    y: Integer;
  end;

  CvPoint2D32f = packed record
    x: Single;
    y: Single;
  end;

  CvPoint3D32f = packed record
    x: Single;
    y: Single;
    z: Single;
  end;

  CvPoint2D64f = packed record
    x: Double;
    y: Double;
  end;

  CvPoint3D64f = packed record
    x: Double;
    y: Double;
    z: Double;
  end;

  (* ******************************* CvSize's & CvBox **************************************/ *)

type
  TCvSize = packed record
    width: Integer;
    height: Integer;
  end;

type
  CvSize2D32f = packed record
    width: Single;
    height: Single;
  end;

type
  CvBox2D = packed record
    center: CvPoint2D32f; (* Center of the box. *)
    size: CvSize2D32f; (* Box width and length. *)
    angle: Single; (* Angle between the horizontal axis *)
  end;

  (* Line iterator state: *)
type
  CvLineIterator = packed record
    ptr: ^uchar;
    err: Integer;
    plus_delta: Integer;
    minus_delta: Integer;
    plus_step: Integer;
    minus_step: Integer;
  end;

  (* ************************************ CvSlice ***************************************** *)

type
  cvSlice = packed record
    start_index, end_index: Integer;
  end;

const
  CV_WHOLE_SEQ_END_INDEX = $3FFFFFFF;
{$EXTERNALSYM CV_WHOLE_SEQ_END_INDEX}
  CV_WHOLE_SEQ: cvSlice = (start_index: 0; end_index: CV_WHOLE_SEQ_END_INDEX);
{$EXTERNALSYM CV_WHOLE_SEQ}
  (* ************************************ CvScalar **************************************** *)

type
  TCvScalar = packed record
    val: array [0 .. 3] of Double;
  end;

  (* ************************************************************************************** *)
  (* Dynamic Data structures *)
  (* ************************************************************************************** *)
  (* ******************************* Memory storage *************************************** *)
type
  CvMemBlock = packed record
    prev: ^CvMemBlock;
    next: ^CvMemBlock;
  end;

const
  CV_STORAGE_MAGIC_VAL = $42890000;
{$EXTERNALSYM CV_STORAGE_MAGIC_VAL}

type
  pCvMemStorage = ^CvMemStorage;

  CvMemStorage = packed record
    signature: Integer;
    bottom: ^CvMemBlock;
    top: ^CvMemBlock; (* First allocated block. *)
    parent: ^CvMemStorage;
    (* Current memory block - top of the stack. *)    (* We get new blocks from parent as needed. *)
    block_size: Integer; (* Block size. *)
    free_space: Integer; (* Remaining free space in current block. *)
  end;

type
  CvMemStoragePos = packed record
    top: ^CvMemBlock;
    free_space: Integer;
  end;

  (* ********************************** Sequence ****************************************** *)
type
  pCvSeqBlock = ^CvSeqBlock;

  CvSeqBlock = packed record
    prev: ^CvSeqBlock; (* Previous sequence block. *)
    next: ^CvSeqBlock; (* Next sequence block. *)
    start_index: Integer; (* Index of the first element in the block + *)
    count: Integer; (* Number of elements in the block. *)
    data: ^schar; (* Pointer to the first element of the block. *)
  end;

  pCvSeq = ^CvSeq;

  CvSeq = packed record
    flags: Integer; (* Miscellaneous flags. *)
    header_size: Integer; (* Size of sequence header. *)
    h_prev: pCvSeq; (* Previous sequence. *)
    h_next: pCvSeq; (* Next sequence. *)
    v_prev: pCvSeq; (* 2nd previous sequence. *)
    v_next: pCvSeq; (* 2nd next sequence. *)
    total: Integer; (* Total number of elements. *)
    elem_size: Integer; (* Size of sequence element in bytes. *)
    block_max: pschar; (* Maximal bound of the last block. *)
    ptr: pschar; (* Current write pointer. *)
    delta_elems: Integer; (* Grow seq this many at a time. *)
    storage: pCvMemStorage; (* Where the seq is stored. *)
    free_blocks: pCvSeqBlock; (* Free blocks list. *)
    first: pCvSeqBlock; (* Pointer to the first sequence block. *)
  end;

const
  CV_TYPE_NAME_SEQ = 'opencv-sequence';
{$EXTERNALSYM CV_TYPE_NAME_SEQ}
  CV_TYPE_NAME_SEQ_TREE = 'opencv-sequence-tree';
{$EXTERNALSYM CV_TYPE_NAME_SEQ_TREE}

  (* ************************************** Set ******************************************* *)
  (*
    Set.
    Order is not preserved. There can be gaps between sequence elements.
    After the element has been inserted it stays in the same place all the time.
    The MSB(most-significant or sign bit) of the first field (flags) is 0 iff the element exists.
  *)
type
  pCvSetElem = ^TCvSetElem;

  TCvSetElem = packed record
    flags: Integer;
    next_free: pCvSetElem;
  end;

  TCvSet = packed record
    flags: Integer; (* Miscellaneous flags. *)
    header_size: Integer; (* Size of sequence header. *)
    h_prev: pCvSeq; (* Previous sequence. *)
    h_next: pCvSeq; (* Next sequence. *)
    v_prev: pCvSeq; (* 2nd previous sequence. *)
    v_next: pCvSeq; (* 2nd next sequence. *)
    total: Integer; (* Total number of elements. *)
    elem_size: Integer; (* Size of sequence element in bytes. *)
    block_max: pschar; (* Maximal bound of the last block. *)
    ptr: pschar; (* Current write pointer. *)
    delta_elems: Integer; (* Grow seq this many at a time. *)
    storage: pCvMemStorage; (* Where the seq is stored. *)
    free_blocks: pCvSeqBlock; (* Free blocks list. *)
    first: pCvSeqBlock; (* Pointer to the first sequence block. *)
    free_elems: pCvSetElem;
    active_count: Integer;
  end;

const
  CV_SET_ELEM_IDX_MASK = (1 shl 26) - 1;
{$EXTERNALSYM CV_SET_ELEM_IDX_MASK}
  CV_SET_ELEM_FREE_FLAG = 1 shl (SizeOf(Integer) * 8 - 1);
{$EXTERNALSYM CV_SET_ELEM_FREE_FLAG}
  (* ************************************ Graph ******************************************* *)

  (*
    We represent a graph as a set of vertices.
    Vertices contain their adjacency lists (more exactly, pointers to first incoming or
    outcoming edge (or 0 if isolated vertex)) then . Edges are stored in another set.
    There is a singly-linked list of incoming/outcoming edges for each vertex.

    Each edge consists of

    o   Two pointers to the starting and ending vertices
    (vtx : array[0..-1] of  and vtx[1] respectively).

    A graph may be oriented or not. In the latter , edges between
    vertex i to vertex j are not distinguished during search operations.

    o   Two pointers to next edges for the starting and ending vertices, where
    next : array[0..-1] of  points to the next edge in the vtx[0] adjacency list and
    next : array[0..0] of  points to the next edge in the vtx[1] adjacency list.
  *)
  // >> Following declaration is a macro definition!
  // const
  // CV_GRAPH_EDGE_FIELDS()Integer flags;
  // Single weight;
  //
  // type;
  //
  // type
  // next:
  // array [0 .. 1] of;
  // struct CvGraphVtx * vtx[2] = ^RAPH_EDGE_FIELDS()Integer flags;
  // Single weight;
  // struct CvGraphEdge;
  // end;
  //
  // type
  //
  // = packed record
  // end;
  // CvGraphEdge;
  //
  // type
  //
  // = packed record
  // end;
  // CvGraphVtx;
  //
  // type
  //
  // = packed record ptr: ^CvPoint2D32f;
  // end;
  // CvGraphVtx2D;
  //
  // (*
  // Graph is 'derived' from the set (this is set a of vertices)
  // and includes another set (edges)
  // *)
  /// / >> Following declaration is a macro definition!
  // const
  // CV_GRAPH_FIELDS()CV_SET_FIELDS()CvSet * edges;;
  //
  // type
  //
  // = packed record
  // end;
  // CvGraph;
  //
  // const
  // CV_TYPE_NAME_GRAPH = 'opencv-graph';
  // {$EXTERNALSYM CV_TYPE_NAME_GRAPH}

  (* ********************************** Chain/Countour ************************************ *)

type

  CvChain = packed record
    origin: TCvPoint;
  end;

  // >> Following declaration is a macro definition!
  // CV_CONTOUR_FIELDS()CV_SEQUENCE_FIELDS()CvRect rect;
  // Integer color;
  // Integer reserved: array [0 .. 2] of
  //
  // const;;
  //
  // type
  //
  // = packed record
  // end;
  // CvContour;
  //
  // type
  // CvContour CvPoint2DSeq;

  (* ***************************************************************************************\
    *                                    Sequence types                                      *
    *************************************************************************************** *)

const
  CV_SEQ_MAGIC_VAL = $42990000;
{$EXTERNALSYM CV_SEQ_MAGIC_VAL}
  // const
  // CV_IS_SEQ(seq)((seq) <> 0 and (((CvSeq(seq))^.flags and CV_MAGIC_MASK) = CV_SEQ_MAGIC_VAL)
  CV_SET_MAGIC_VAL = $42980000;
  // {$EXTERNALSYM CV_SET_MAGIC_VAL}
  // const CV_IS_SET(set)((set) <> 0 and (((CvSeq(set))^.flags and CV_MAGIC_MASK) = CV_SET_MAGIC_VAL)

  CV_SEQ_ELTYPE_BITS = 12;
{$EXTERNALSYM CV_SEQ_ELTYPE_BITS}
  CV_SEQ_ELTYPE_MASK = (1 shl CV_SEQ_ELTYPE_BITS) - 1;
{$EXTERNALSYM CV_SEQ_ELTYPE_MASK}
  // CV_SEQ_ELTYPE_POINT = CV_32SC2; (* (x,y) *)
  // {$EXTERNALSYM CV_SEQ_ELTYPE_POINT}
  // CV_SEQ_ELTYPE_CODE = CV_8UC1; (* freeman code: 0..7 *)
  // {$EXTERNALSYM CV_SEQ_ELTYPE_CODE}
  CV_SEQ_ELTYPE_GENERIC = 0;
{$EXTERNALSYM CV_SEQ_ELTYPE_GENERIC}
  CV_SEQ_ELTYPE_PTR = CV_USRTYPE1;
{$EXTERNALSYM CV_SEQ_ELTYPE_PTR}
  CV_SEQ_ELTYPE_PPOINT = CV_SEQ_ELTYPE_PTR; (* &(x,y) *)
{$EXTERNALSYM CV_SEQ_ELTYPE_PPOINT}
  // CV_SEQ_ELTYPE_INDEX = CV_32SC1; (* #(x,y) *)
  // {$EXTERNALSYM CV_SEQ_ELTYPE_INDEX}
  CV_SEQ_ELTYPE_GRAPH_EDGE = 0; (* &next_o, &next_d, &vtx_o, &vtx_d *)
{$EXTERNALSYM CV_SEQ_ELTYPE_GRAPH_EDGE}
  CV_SEQ_ELTYPE_GRAPH_VERTEX = 0; (* first_edge, &(x,y) *)
{$EXTERNALSYM CV_SEQ_ELTYPE_GRAPH_VERTEX}
  CV_SEQ_ELTYPE_TRIAN_ATR = 0; (* vertex of the binary tree *)
{$EXTERNALSYM CV_SEQ_ELTYPE_TRIAN_ATR}
  CV_SEQ_ELTYPE_CONNECTED_COMP = 0; (* connected component *)
{$EXTERNALSYM CV_SEQ_ELTYPE_CONNECTED_COMP}
  // CV_SEQ_ELTYPE_POINT3D = CV_32FC3; (* (x,y,z) *)
  // {$EXTERNALSYM CV_SEQ_ELTYPE_POINT3D}
  CV_SEQ_KIND_BITS = 2;
{$EXTERNALSYM CV_SEQ_KIND_BITS}
  CV_SEQ_KIND_MASK = ((1 shl CV_SEQ_KIND_BITS) - 1) shl CV_SEQ_ELTYPE_BITS;
{$EXTERNALSYM CV_SEQ_KIND_MASK}
  (* types of sequences *)
  CV_SEQ_KIND_GENERIC = (0 shl CV_SEQ_ELTYPE_BITS);
{$EXTERNALSYM CV_SEQ_KIND_GENERIC}
  CV_SEQ_KIND_CURVE = (1 shl CV_SEQ_ELTYPE_BITS);
{$EXTERNALSYM CV_SEQ_KIND_CURVE}
  CV_SEQ_KIND_BIN_TREE = (2 shl CV_SEQ_ELTYPE_BITS);
{$EXTERNALSYM CV_SEQ_KIND_BIN_TREE}
  (* types of sparse sequences (sets) *)
  CV_SEQ_KIND_GRAPH = (1 shl CV_SEQ_ELTYPE_BITS);
{$EXTERNALSYM CV_SEQ_KIND_GRAPH}
  CV_SEQ_KIND_SUBDIV2D = (2 shl CV_SEQ_ELTYPE_BITS);
{$EXTERNALSYM CV_SEQ_KIND_SUBDIV2D}
  CV_SEQ_FLAG_SHIFT = (CV_SEQ_KIND_BITS + CV_SEQ_ELTYPE_BITS);
{$EXTERNALSYM CV_SEQ_FLAG_SHIFT}
  (* flags for curves *)
  CV_SEQ_FLAG_CLOSED = (1 shl CV_SEQ_FLAG_SHIFT);
{$EXTERNALSYM CV_SEQ_FLAG_CLOSED}
  CV_SEQ_FLAG_SIMPLE = (0 shl CV_SEQ_FLAG_SHIFT);
{$EXTERNALSYM CV_SEQ_FLAG_SIMPLE}
  CV_SEQ_FLAG_CONVEX = (0 shl CV_SEQ_FLAG_SHIFT);
{$EXTERNALSYM CV_SEQ_FLAG_CONVEX}
  CV_SEQ_FLAG_HOLE = (2 shl CV_SEQ_FLAG_SHIFT);
{$EXTERNALSYM CV_SEQ_FLAG_HOLE}
  (* flags for graphs *)
  CV_GRAPH_FLAG_ORIENTED = (1 shl CV_SEQ_FLAG_SHIFT);
{$EXTERNALSYM CV_GRAPH_FLAG_ORIENTED}
  CV_GRAPH = CV_SEQ_KIND_GRAPH;
{$EXTERNALSYM CV_GRAPH}
  CV_ORIENTED_GRAPH = (CV_SEQ_KIND_GRAPH or CV_GRAPH_FLAG_ORIENTED);
{$EXTERNALSYM CV_ORIENTED_GRAPH}
  (* point sets *)
  // CV_SEQ_POINT_SET = (CV_SEQ_KIND_GENERIC or CV_SEQ_ELTYPE_POINT);
  // {$EXTERNALSYM CV_SEQ_POINT_SET}
  // CV_SEQ_POINT3D_SET = (CV_SEQ_KIND_GENERIC or CV_SEQ_ELTYPE_POINT3D);
  // {$EXTERNALSYM CV_SEQ_POINT3D_SET}
  // CV_SEQ_POLYLINE = (CV_SEQ_KIND_CURVE or CV_SEQ_ELTYPE_POINT);
  // {$EXTERNALSYM CV_SEQ_POLYLINE}
  // CV_SEQ_POLYGON = (CV_SEQ_FLAG_CLOSED or CV_SEQ_POLYLINE);
  // {$EXTERNALSYM CV_SEQ_POLYGON}
  // CV_SEQ_CONTOUR = CV_SEQ_POLYGON;
  // {$EXTERNALSYM CV_SEQ_CONTOUR}
  // CV_SEQ_SIMPLE_POLYGON = (CV_SEQ_FLAG_SIMPLE or CV_SEQ_POLYGON);
  // {$EXTERNALSYM CV_SEQ_SIMPLE_POLYGON}
  (* chain-coded curves *)
  // CV_SEQ_CHAIN = (CV_SEQ_KIND_CURVE or CV_SEQ_ELTYPE_CODE);
  // {$EXTERNALSYM CV_SEQ_CHAIN}
  // CV_SEQ_CHAIN_CONTOUR = (CV_SEQ_FLAG_CLOSED or CV_SEQ_CHAIN);
  // {$EXTERNALSYM CV_SEQ_CHAIN_CONTOUR}
  // (* binary tree for the contour *)
  // CV_SEQ_POLYGON_TREE = (CV_SEQ_KIND_BIN_TREE or CV_SEQ_ELTYPE_TRIAN_ATR);
  // {$EXTERNALSYM CV_SEQ_POLYGON_TREE}
  (* sequence of the connected components *)
  CV_SEQ_CONNECTED_COMP = (CV_SEQ_KIND_GENERIC or CV_SEQ_ELTYPE_CONNECTED_COMP);
{$EXTERNALSYM CV_SEQ_CONNECTED_COMP}
  (* sequence of the integer numbers *)
  // CV_SEQ_INDEX = (CV_SEQ_KIND_GENERIC or CV_SEQ_ELTYPE_INDEX);
  // {$EXTERNALSYM CV_SEQ_INDEX}
  // >> Following declaration is a macro definition!
  // const
  // CV_SEQ_ELTYPE(seq)((seq)^.flags and CV_SEQ_ELTYPE_MASK);
  /// / >> Following declaration is a macro definition!
  // const
  // CV_SEQ_KIND(seq)((seq)^.flags and CV_SEQ_KIND_MASK);
  // (* flag checking *)
  // const
  // CV_IS_SEQ_INDEX(seq)((CV_SEQ_ELTYPE(seq) = CV_SEQ_ELTYPE_INDEX) and
  // (CV_SEQ_KIND(seq) = CV_SEQ_KIND_GENERIC))
  // const
  // CV_IS_SEQ_CURVE(seq)(CV_SEQ_KIND(seq) = CV_SEQ_KIND_CURVE)
  /// / >> Following declaration is a macro definition!
  // const
  // CV_IS_SEQ_CLOSED(seq)(((seq)^.flags and CV_SEQ_FLAG_CLOSED) <> 0);
  //
  /// / >> Following declaration is a macro definition!
  // const
  // CV_IS_SEQ_CONVEX(seq)0;
  //
  /// / >> Following declaration is a macro definition!
  // const
  // CV_IS_SEQ_HOLE(seq)(((seq)^.flags and CV_SEQ_FLAG_HOLE) <> 0);
  //
  /// / >> Following declaration is a macro definition!
  // const
  // CV_IS_SEQ_SIMPLE(seq)1;
  //
  // (* type checking macros *)
  // const
  // CV_IS_SEQ_POINT_SET(seq)((CV_SEQ_ELTYPE(seq) = CV_32SC2 or CV_SEQ_ELTYPE(seq) = CV_32FC2))
  //
  // const
  // CV_IS_SEQ_POINT_SUBSET(seq)(CV_IS_SEQ_INDEX(seq) or CV_SEQ_ELTYPE(seq) = CV_SEQ_ELTYPE_PPOINT)
  //
  // const
  // CV_IS_SEQ_POLYLINE(seq)(CV_SEQ_KIND(seq) = CV_SEQ_KIND_CURVE and CV_IS_SEQ_POINT_SET(seq))
  //
  /// / >> Following declaration is a macro definition!
  // const
  // CV_IS_SEQ_POLYGON(seq)(CV_IS_SEQ_POLYLINE(seq) and CV_IS_SEQ_CLOSED(seq));
  //
  // const
  // CV_IS_SEQ_CHAIN(seq)(CV_SEQ_KIND(seq) = CV_SEQ_KIND_CURVE and (seq)^.elem_size = 1)
  //
  /// / >> Following declaration is a macro definition!
  // const
  // CV_IS_SEQ_CONTOUR(seq)(CV_IS_SEQ_CLOSED(seq) and (CV_IS_SEQ_POLYLINE(seq) or CV_IS_SEQ_CHAIN(seq)));
  //
  /// / >> Following declaration is a macro definition!
  // const
  // CV_IS_SEQ_CHAIN_CONTOUR(seq)(CV_IS_SEQ_CHAIN(seq) and CV_IS_SEQ_CLOSED(seq));
  //
  // const
  // CV_IS_SEQ_POLYGON_TREE(seq)(CV_SEQ_ELTYPE(seq) = CV_SEQ_ELTYPE_TRIAN_ATR and CV_SEQ_KIND(seq)
  // = CV_SEQ_KIND_BIN_TREE)
  //
  // const
  // CV_IS_GRAPH(seq)(CV_IS_SET(seq) and CV_SEQ_KIND((CvSet(seq)) = CV_SEQ_KIND_GRAPH)
  //
  // // >> Following declaration is a macro definition!
  // const CV_IS_GRAPH_ORIENTED(seq)(((seq)^.flags and CV_GRAPH_FLAG_ORIENTED) <> 0);
  //
  // const CV_IS_SUBDIV2D(seq)(CV_IS_SET(seq) and CV_SEQ_KIND((CvSet(seq)) = CV_SEQ_KIND_SUBDIV2D)

  (* ************************************************************************************** *)
  (* Sequence writer & reader *)
  (* ************************************************************************************** *)

  // // >> Following declaration is a macro definition!
  // const CV_SEQ_WRITER_FIELDS()Integer header_size; CvSeq * seq;; CvSeqBlock * block;
  // (* current block *) \ schar * ptr; (* pointer to free space *) \ schar * block_min;
  // (* pointer to the beginning of block *) \ schar * block_max; (* pointer to the end of block *)
  //
  // type
  //
  // = packed record end; CvSeqWriter;
  //
  // // >> Following declaration is a macro definition!
  // const CV_SEQ_READER_FIELDS()Integer header_size; CvSeq * seq;; CvSeqBlock * block;
  // (* current block *) \ schar * ptr; (* pointer to element be read next *) \ schar * block_min;
  // (* pointer to the beginning of block *) \ schar * block_max;
  // (* pointer to the end of block *) \ Integer delta_index;
  // (* = seq->first->start_index *) \ schar * prev_elem; (* pointer to previous element *)
  //
  // type
  //
  // = packed record end; CvSeqReader;
  //
  // (* ************************************************************************************** *)
  // (* Operations on sequences *)
  // (* ************************************************************************************** *)
  //
  // // >> Following declaration is a macro definition!
  // const CV_SEQ_ELEM(seq, elem_type, index);
  // (* assert gives some guarantee that <seq> parameter is valid *) \
  // (Assert(SizeOf((seq)^.first[0 .. -1] of) = SizeOf(CvSeqBlock) and
  // (seq)^.elem_size = SizeOf(elem_type = array));
  // {$EXTERNALSYM }
  // (elem_type(seq)^.first and (Cardinal)index < (Cardinal(seq)^.first^.count)?(seq)^.first^.data +
  // (index)SizeOf(elem_type)cvGetSeqElem((CvSeq(seq), (index)))) = ^;
  // {$EXTERNALSYM (elem_type(seq)^.first and (Cardinal)index <                (Cardinal(seq)^.first^.count) ?                             (seq)^.first^.data + (index)  SizeOf(elem_type)             cvGetSeqElem( (CvSeq(seq), (index) )))}
  // // >> Following declaration is a macro definition!
  // const CV_GET_SEQ_ELEM(elem_type, seq, index)CV_SEQ_ELEM((seq), elem_type, (index));
  //
  // (* Add element to sequence: *)
  // // >> Following declaration is a macro definition!
  // const CV_WRITE_SEQ_ELEM_VAR(elem_ptr, writer);
  // begin if ((writer).ptr >= (writer).block_max)begin cvCreateSeqBlock(and writer); end;
  // memcpy((writer).ptr, elem_ptr, (writer).seq^.elem_size);
  // (writer).ptr := mod +(writer) then .seq^.elem_size; end;
  //
  // // >> Following declaration is a macro definition!
  // const CV_WRITE_SEQ_ELEM(elem, writer); begin Assert((writer).seq^.elem_size = SizeOf(elem));
  // if ((writer).ptr >= (writer).block_max)begin cvCreateSeqBlock(and writer); end;
  // Assert((writer).ptr <= (writer).block_max - SizeOf(elem));
  // memcpy((writer).ptr, and (elem), SizeOf(elem)); (writer).ptr := mod +SizeOf(elem) then; end;
  //
  // (* Move reader position forward: *)
  // // >> Following declaration is a macro definition!
  // const CV_NEXT_SEQ_ELEM(elem_size, reader);
  // begin if (((reader).ptr := mod +(elem_size)) >= (reader).block_max)begin cvChangeSeqBlock(and
  // (reader), 1) then; end; end;
  //
  // (* Move reader position backward: *)
  // // >> Following declaration is a macro definition!
  // const CV_PREV_SEQ_ELEM(elem_size, reader);
  // begin if (((reader).ptr := mod -(elem_size)) < (reader).block_min)begin cvChangeSeqBlock(and
  // (reader), -1) then; end; end;
  //
  // (* Read element and move read position forward: *)
  // // >> Following declaration is a macro definition!
  // const CV_READ_SEQ_ELEM(elem, reader); begin Assert((reader).seq^.elem_size := SizeOf(elem));
  // memcpy(and (elem), (reader).ptr, SizeOf((elem))); CV_NEXT_SEQ_ELEM(SizeOf(elem), reader) end;
  //
  // (* Read element and move read position backward: *)
  // // >> Following declaration is a macro definition!
  // const CV_REV_READ_SEQ_ELEM(elem, reader); begin Assert((reader).seq^.elem_size := SizeOf(elem));
  // memcpy(and (elem), (reader).ptr, SizeOf((elem))); CV_PREV_SEQ_ELEM(SizeOf(elem), reader) end;
  //
  // // >> Following declaration is a macro definition!
  // const CV_READ_CHAIN_POINT(_pt, reader); (_pt) = (reader).pt;
  // if ((reader).ptr)begin CV_READ_SEQ_ELEM((reader).code, 0 .. 70009E r));
  // Assert(((reader).code and ~ 7) = 0);
  // (reader).pt.x = mod +(reader).deltas: array [0 .. -1(Integer(reader).code, 0] of begin;
  // (reader).pt.y := mod +(reader).deltas[(Integer(reader) then .code][1]; end; end;
  //
  // // >> Following declaration is a macro definition!
  // const CV_CURRENT_POINT(reader)(((CvPoint(reader).ptr)));
  // // >> Following declaration is a macro definition!
  // const CV_PREV_POINT(reader)(((CvPoint(reader).prev_elem)));
  //
  // // >> Following declaration is a macro definition!
  // const CV_READ_EDGE(pt1, pt2, reader);
  // begin Assert(SizeOf(pt1) := SizeOf(CvPoint) and SizeOf(pt2) = SizeOf(CvPoint) and
  // reader.seq^.elem_size = SizeOf(CvPoint)); (pt1) = CV_PREV_POINT(reader);
  // (pt2) = CV_CURRENT_POINT(reader); (reader).prev_elem = (reader).ptr;
  // CV_NEXT_SEQ_ELEM(SizeOf(CvPoint), (reader)); end;
  //
  // (* *********** Graph macros *********** *)
  //
  // (* Return next graph edge for given vertex: *)
  // CV_NEXT_GRAPH_EDGE(edge, vertex)(Assert((edge)^.vtx: array [0 .. -1] of const = (vertex) or
  // (edge)^.vtx[1] = (vertex)), (edge)^.next[(edge)^.vtx[1] = (vertex)])

  (* ***************************************************************************************\
    *             Data structures for persistence (a.k.a serialization) functionality        *
    *************************************************************************************** *)

  (* "black box" file storage *)
  // type type CvFileStorage = leStorage;

  (* Storage flags: *)
const
  CV_STORAGE_READ = 0;
{$EXTERNALSYM CV_STORAGE_READ}
  CV_STORAGE_WRITE = 1;
{$EXTERNALSYM CV_STORAGE_WRITE}
  CV_STORAGE_WRITE_TEXT = CV_STORAGE_WRITE;
{$EXTERNALSYM CV_STORAGE_WRITE_TEXT}
  CV_STORAGE_WRITE_BINARY = CV_STORAGE_WRITE;
{$EXTERNALSYM CV_STORAGE_WRITE_BINARY}
  CV_STORAGE_APPEND = 2;
{$EXTERNALSYM CV_STORAGE_APPEND}
  CV_STORAGE_MEMORY = 4;
{$EXTERNALSYM CV_STORAGE_MEMORY}
  CV_STORAGE_FORMAT_MASK = (7 shl 3);
{$EXTERNALSYM CV_STORAGE_FORMAT_MASK}
  CV_STORAGE_FORMAT_AUTO = 0;
{$EXTERNALSYM CV_STORAGE_FORMAT_AUTO}
  CV_STORAGE_FORMAT_XML = 8;
{$EXTERNALSYM CV_STORAGE_FORMAT_XML}
  CV_STORAGE_FORMAT_YAML = 16;
{$EXTERNALSYM CV_STORAGE_FORMAT_YAML}

  (* List of attributes: *)
type

  pCvAttrList = ^TCvAttrList;

  TCvAttrList = packed record
    attr: ppCVChar; (* NULL-terminated array of (attribute_name,attribute_value) pairs. *)
    next: pCvAttrList; (* Pointer to next chunk of the attributes list. *)
  end;
  (*
    CV_INLINE CvAttrList cvAttrList( const char** attr CV_DEFAULT(NULL),
    CvAttrList* next CV_DEFAULT(NULL) )
    {   CvAttrList l;
    l.attr = attr;
    l.next = next;
    return l;} *)

Const
  ZeroCvAttrList: TCvAttrList = (attr: nil; next: nil);

function CvAttrList(const attr: ppCVChar = nil; next: pCvAttrList = nil): TCvAttrList;

type
  CvTypeInfo = packed record
  end;

const
  CV_NODE_NONE = 0;
{$EXTERNALSYM CV_NODE_NONE}
  CV_NODE_INT = 1;
{$EXTERNALSYM CV_NODE_INT}
  CV_NODE_INTEGER = CV_NODE_INT;
{$EXTERNALSYM CV_NODE_INTEGER}
  CV_NODE_REAL = 2;
{$EXTERNALSYM CV_NODE_REAL}
  CV_NODE_FLOAT = LongInt(CV_NODE_REAL);
{$EXTERNALSYM CV_NODE_FLOAT}
  CV_NODE_STR = 3;
{$EXTERNALSYM CV_NODE_STR}
  CV_NODE_STRING = CV_NODE_STR;
{$EXTERNALSYM CV_NODE_STRING}
  CV_NODE_REF = 4; (* not used *)
{$EXTERNALSYM CV_NODE_REF}
  CV_NODE_SEQ = 5;
{$EXTERNALSYM CV_NODE_SEQ}
  CV_NODE_MAP = 6;
{$EXTERNALSYM CV_NODE_MAP}
  CV_NODE_TYPE_MASK = 7;
{$EXTERNALSYM CV_NODE_TYPE_MASK}
  // >> Following declaration is a macro definition!
  // CV_NODE_TYPE(flags)((flags) and CV_NODE_TYPE_MASK);
  (* file node flags *)
  CV_NODE_FLOW = 8; (* Used only for writing structures in YAML format. *)
{$EXTERNALSYM CV_NODE_FLOW}
  CV_NODE_USER = 16;
{$EXTERNALSYM CV_NODE_USER}
  CV_NODE_EMPTY = 32;
{$EXTERNALSYM CV_NODE_EMPTY}
  CV_NODE_NAMED = 64;
{$EXTERNALSYM CV_NODE_NAMED}

const
  // CV_NODE_IS_INT(flags)(CV_NODE_TYPE(flags) = CV_NODE_INT)const CV_NODE_IS_REAL(flags)
  // (CV_NODE_TYPE(flags) = CV_NODE_REAL)const CV_NODE_IS_STRING(flags)
  // (CV_NODE_TYPE(flags) = CV_NODE_STRING)const CV_NODE_IS_SEQ(flags)
  // (CV_NODE_TYPE(flags) = CV_NODE_SEQ)const CV_NODE_IS_MAP(flags)
  // (CV_NODE_TYPE(flags) = CV_NODE_MAP)const CV_NODE_IS_COLLECTION
  // (flags(CV_NODE_TYPE(flags) >= CV_NODE_SEQ)
  // // >> Following declaration is a macro definition!
  // const CV_NODE_IS_FLOW(flags)(((flags) and CV_NODE_FLOW) <> 0);
  // // >> Following declaration is a macro definition!
  // const CV_NODE_IS_EMPTY(flags)(((flags) and CV_NODE_EMPTY) <> 0);
  // // >> Following declaration is a macro definition!
  // const CV_NODE_IS_USER(flags)(((flags) and CV_NODE_USER) <> 0);
  // // >> Following declaration is a macro definition!
  // const CV_NODE_HAS_NAME(flags)(((flags) and CV_NODE_NAMED) <> 0);

  CV_NODE_SEQ_SIMPLE = 256;
{$EXTERNALSYM CV_NODE_SEQ_SIMPLE}
  //
  // // >> Following declaration is a macro definition!
  // const
  // CV_NODE_SEQ_IS_SIMPLE(seq(((seq)^.flags and CV_NODE_SEQ_SIMPLE) <> 0);

type

  CvString = packed record
    len: Integer;
    ptr: pCVChar;
  end;

  (* All the keys (names) of elements in the readed file storage
    are stored in the hash to speed up the lookup operations: *)
  // type
  // CvStringHashNode
  // begin
  // Cardinal hashval;
  // CvStringHashNode
  // begin
  // Cardinal hashval;
  // CvString str;
  //
  // type
  // CvStringHashNode * next = packed record
  // end;
  // end;
  // CvStringHashNode;
  //
  // type
  // type
  // CvFileNodeHash = nericHash;

  (* Basic element of the file storage - scalar or collection: *)
  // type
  // CvFileNode = packed record
  // tag: Integer;
  // info: ^CvTypeInfo; (* cType information
  // f: Double; (* scalar floating-point number *)
  // i: Integer; (* scalar integer number *)
  // str: CvString; (* text cString *)
  // seq: ^CvSeq; (* sequence (ordered collection of file nodes) *)
  // map: ^CvFileNodeHash; (* map (collection of named cFile nodes) *)
  // end;
  //
  // data = CvFileNode;
  // {$EXTERNALSYM data}
  // end;
  // CvFileNode;

{$IFDEF __cplusplus}
  // extern "C" {
{$ENDIF}
  // type
  // type) =;
  //
  // type type) = tr;
  //
  // type * CvReadFunc = function(var storage: CvFileStorage;
  //
  // var node: CvFileNode): Pointer; CV_CDECL;
  //
  // type
  //
  // procedure(CV_CDECL * CvWriteFunc(
  // CvFileStorage * storage, PCVChar name, Pointer type struct_ptr, CvAttrList attributes) = packed record) =
  // : ptr; end;
  // {$ENDIF}
  // type CvTypeInfo
  //
  // begin Integer flags; CvTypeInfo
  //
  // begin Integer flags; Integer header_size;
  //
  // type CvTypeInfo * prev = packed record end;
  //
  // type CvTypeInfo * next = packed record end; PCVChar type_name; CvIsInstanceFunc is_instance;
  // CvReleaseFunc release; CvReadFunc read; CvWriteFunc write; CvCloneFunc clone; end; CvTypeInfo;
  //
  // (* *** System data types ***** *)
  //
  // type
  //
  // = packed record func_addr: ^Pointer; default_func_addr: Pointer; func_names: PCVChar;
  // search_modules: Integer; loaded_from: Integer; end; CvPluginFuncInfo;
  //
  // type CvModuleInfo
  //
  // begin
  //
  // type CvModuleInfo * next = packed record end; CvModuleInfo
  //
  // begin
  //
  // type CvModuleInfo * next = packed record end; PCVChar name; PCVChar version; CvPluginFuncInfo * func_tab;
  // end; CvModuleInfo;
  //
{$ENDIF}	(* __OPENCV_CORE_TYPES_H__*/

    (* End of file. *)

Type
  CV = class
    // // Common
    // class procedure CV_SWAP<T: packed record >(
    //
    // Var a, b: T); inline;
    // { Var c:T; begin c := a; a := b; b := c; end; }
    // {$IFNDEF MIN}
    // {$HPPEMIT '#  define MIN(a,b)  ((a) > (b) ? (b) : (a))'}
    // {$ENDIF}
    // {$IFNDEF MAX}
    // {$HPPEMIT '#  define MAX(a,b)  ((a) < (b) ? (b) : (a))'}
    // {$ENDIF}
    // (* min & max without jumps *)
    // // >> Following declaration is a macro definition!
    // class function CV_IMIN(
    //
    // const a, b: Integer): Integer; inline;
    // { ((a) xor (((a) xor (b)) and (((a) < (b)) - 1))): INT; }
    // // >> Following declaration is a macro definition!
    // class function CV_IMAX(
    //
    // const a, b: Integer): Integer; inline;
    // { ((a) xor (((a) xor (b)) and (((a) > (b)) - 1))); }
    //
    // (* absolute value without jumps *)
    // {$IFNDEF __cplusplus}
    // {$HPPEMIT '#  define  CV_IABS(a)     (((a) ^ ((a) < 0 ? -1 : 0)) - ((a) < 0 ? -1 : 0))'}
    // {$ELSE}
    // {$HPPEMIT '#  define  CV_IABS(a)     abs(a)'}
    // {$ENDIF}
    // // >> Following declaration is a macro definition!
    // class function CV_CMP(
    //
    // const a, b: Integer): Integer; inline;
    // { (((a) > (b)) - ((a) < (b))); }
    // // >> Following declaration is a macro definition!
    // class function CV_SIGN(
    //
    // const a: Integer): Integer; inline;
    // { PCV_CMP((a), 0); }
    // // CV_INLINE
    // class function cvRound<T: packed record >(
    //
    // const v1: T): Integer; inline;
    // (*
    // result := _mm_cvtsd_si32(T);
    // {$HPPEMIT '#elif defined _MSC_VER && defined _M_IX86'}
    // Integer T;
    // asm
    // //begin
    // fld value;
    // fistp t;
    // end;
    // result := T;
    // {$HPPEMIT '#elif defined HAVE_LRINT || defined CV_ICC || defined __GNUC__'}
    // {$HPPEMIT '#  ifdef HAVE_TEGRA_OPTIMIZATION'}
    // TEGRA_ROUND(value);
    // {$HPPEMIT '#  else'}
    // result := (Integer)lrint(value);
    // {$HPPEMIT '#  endif'}
    // {$ELSE}
    // // while this is not IEEE754-compliant rounding, it's usually a good enough approximation
    // result := (Integer(value + (value >= 0 ? 0.5: - 0.5));
    // {$ENDIF}
    // end; *)
    // (*
    //
    // //temporary not implemented
    //
    // {$IFNDEF  __SSE2__ || defined _M_IX86_FP && 2 == _M_IX86_FP}
    // {$HPPEMIT '#  include 'emmintrin.h''}
    // {$ENDIF}
    //
    // CV_INLINE
    // function cvFloor(v1: value): Integer;
    // Integer i := _mm_cvtsd_si32(T);
    // result := i - _mm_movemask_pd(_mm_cmplt_sd(T, _mm_cvtsi32_sd(T, i)));
    // {$HPPEMIT '#elif defined __GNUC__'}
    // Integer i := (Integer)value;
    // result := i - (i > value);
    // {$ELSE}
    // Integer i := cvRound(value);
    // Cv32suf diff;
    // diff.f := (Single(value - i); result := i - (diff.i < 0);
    // {$ENDIF}
    // end;
    //
    // CV_INLINE
    // function cvCeil(v1: value): Integer; Integer i := _mm_cvtsd_si32(T);
    // result := i + _mm_movemask_pd(_mm_cmplt_sd(_mm_cvtsi32_sd(T, i), T));
    // {$HPPEMIT '#elif defined __GNUC__'}
    // Integer i := (Integer)value; result := i + (i < value);
    // {$ELSE}
    // Integer i := cvRound(value); Cv32suf diff; diff.f := (Single(i - value); result := i + (diff.i < 0);
    // {$ENDIF}
    // end;
    //
    // {$DEFINE cvInvSqrt(value((float(1./sqrt(value)))}
    // // >> Following declaration is a macro definition!
    // const cvSqrt(value)((Single)sqrt(value));
    //
    // CV_INLINE
    // function cvIsNaN(var defined _MSC_VER | | defined __BORLANDC__ result := _isnan(value: ) and
    // (1)}(): Integer;
    // {$HPPEMIT '#elif defined __GNUC__'}
    // result := isnan(value);
    // {$ELSE} * )Cv64suf ieee754; ieee754.f := value;
    // result := ((Cardinal(ieee754.u shr 32) and $7FFFFFFF) + ((Cardinal)ieee754.u <> 0) > $7FF00000;
    // {$ENDIF}
    // end;
    //
    // CV_INLINE
    // function cvIsInf(var defined _MSC_VER | | defined __BORLANDC__ result := ! _finite(value: ) and
    // (1)}(): Integer;
    // {$HPPEMIT '#elif defined __GNUC__'}
    // result := isinf(value);
    // {$ELSE} * )Cv64suf ieee754; ieee754.f := value;
    // result := ((Cardinal(ieee754.u shr 32) and $7FFFFFFF) = $7FF00000 and (Cardinal)ieee754.u := 0;
    // {$ENDIF}
    // end;
    //
    // CV_INLINE
    // function CvRNG(var Return random 32 - bit unsigned Integer: * )CV_INLINE
    // function cvRandInt(CvRNG * rng)begin uint64 temp := * rng;
    // temp := (uint64(Cardinal)temp * CV_RNG_COEFF + (temp shr 32: - 1))begin CvRNG rng := seed ?(uint64)
    // seed: (uint64(int64) - 1; result := rng; end; (): Cardinal; * rng := temp;
    // result := (Cardinal)temp; end;

    (*
      CV_INLINE unsigned cvRandInt( CvRNG* rng )
      {
      uint64 temp = *rng;
      temp = (uint64)(unsigned)temp*CV_RNG_COEFF + (temp >> 32);
      *rng = temp;
      return (unsigned)temp;
      }
    *)

    //
    // { Returns random floating-point number between 0 and 1: }
    // CV_INLINE
    // function cvRandReal(var 2.3283064365386962890625E-10 (* 2^-32 *: rng)): Double;
    // end;
    // *)
    //
    // // --------- IplImage -----------------
    // {$EXTERNALSYM CV_TYPE_NAME_IMAGE}
    // class function CV_IS_IMAGE_HDR(
    //
    // const img: pIplImage): Integer; inline;
    // { ((img) <> 0 and ((IplImage(img))^.nSize = SizeOf(IplImage)) }
    // // >> Following declaration is a macro definition!
    // class function CV_IS_IMAGE(
    //
    // const img: pIplImage): Integer; inline;
    // // (CV_IS_IMAGE_HDR(img) and ((IplImage)img)^.imageData <> 0);
    // (* for storing double-precision
    // floating point data in IplImage's */
    // const IPL_DEPTH_64F = 64;
    // {$EXTERNALSYM IPL_DEPTH_64F}
    // (* get reference to pixel at (col,row),
    // for multi-channel images (col) should be multiplied by number of channels *)
    // // >> Following declaration is a macro definition!
    // // ageData + (image)^.widthStep * (row)) * (row)) = array)[0 .. (col) - 1] of;
    // // {$EXTERNALSYM *(row))}row, col)(((elemtype(image)^.imageData + (image)^.widthStep);
    // // ---------- Matrix cType (CvMat) ---------
    // // >> Following declaration is a macro definition!
    // class function CV_MAT_DEPTH(
    //
    // const flags: Integer): Integer; inline;
    // { ((flags) and CV_MAT_DEPTH_MASK); }
    // // >> Following declaration is a macro definition!
    // class function CV_MAKETYPE(
    //
    // const depth, cn: Integer): Integer; inline;
    // // (CV_MAT_DEPTH(depth) + (((cn) - 1) shl CV_CN_SHIFT));
    // class function CV_MAKE_TYPE(
    //
    // const depth, cn: Integer): Integer; inline;
    // // (CV_MAT_DEPTH(depth) + (((cn) - 1) shl CV_CN_SHIFT));
    // // >> Following declaration is a macro definition!
    // class function CV_8UC(
    //
    // const n: Integer): Integer; inline;
    // // CV_MAKETYPE(CV_8U, (n));
    // // >> Following declaration is a macro definition!
    // class function CV_8SC(
    //
    // const n: Integer): Integer; inline;
    // // CV_MAKETYPE(CV_8S, (n));
    // // >> Following declaration is a macro definition!
    // class function CV_16UC(
    //
    // const n: Integer): Integer; inline;
    // // CV_MAKETYPE(CV_16U, (n));
    // (*
    // // >> Following declaration is a macro definition!
    // const CV_MAT_CN(flags)((((flags) and CV_MAT_CN_MASK) shr CV_CN_SHIFT) + 1);
    //
    // // >> Following declaration is a macro definition!
    // const CV_MAT_TYPE(flags)((flags) and CV_MAT_TYPE_MASK);
    //
    // // >> Following declaration is a macro definition!
    // const CV_IS_MAT_CONT(flags)((flags) and CV_MAT_CONT_FLAG);
    //
    // // >> Following declaration is a macro definition!
    // const CV_IS_SUBMAT(flags)((flags) and CV_MAT_SUBMAT_FLAG);
    //
    // const CV_IS_MAT_HDR(mat)((mat) <> 0 and (((CvMat(mat))^.cType and CV_MAGIC_MASK)
    // = CV_MAT_MAGIC_VAL and ((CvMat(mat))^.cols > 0 and ((CvMat(mat))^.rows > 0)
    //
    // const CV_IS_MAT_HDR_Z(mat)((mat) <> 0 and (((CvMat(mat))^.cType and CV_MAGIC_MASK)
    // = CV_MAT_MAGIC_VAL and ((CvMat(mat))^.cols >= 0 and ((CvMat(mat))^.rows >= 0)
    //
    // // >> Following declaration is a macro definition!
    // const CV_IS_MAT(mat)(CV_IS_MAT_HDR(mat) and ((CvMat(mat))^.data.ptr <> 0);
    //
    // const CV_IS_MASK_ARR(mat)(((mat)^.cType and (CV_MAT_TYPE_MASK and ~ CV_8SC1)) = 0)
    //
    // const CV_ARE_TYPES_EQ(mat1, mat2)((((mat1)^.cType xor (mat2)^.cType) and CV_MAT_TYPE_MASK) = 0)
    //
    // const CV_ARE_CNS_EQ(mat1, mat2)((((mat1)^.cType xor (mat2)^.cType) and CV_MAT_CN_MASK) = 0)
    //
    // const CV_ARE_DEPTHS_EQ(mat1, mat2)((((mat1)^.cType xor (mat2)^.cType) and CV_MAT_DEPTH_MASK) = 0)
    //
    // const CV_ARE_SIZES_EQ(mat1, mat2)((mat1)^.rows = (mat2)^.rows and (mat1)^.cols = (mat2)^.cols)
    //
    // const CV_IS_MAT_CONST(mat)(((mat)^.rows or (mat)^.cols) = 1) *)
    //
    // (* Size of each channel item,
    // $124489 = 1000 0100 0100 0010 0010 0001 0001 ~ cArray of SizeOf(arr_type_elem) *)
    // // >> Following declaration is a macro definition!
    // // const CV_ELEM_SIZE1(cType)((((SizeOf(size_t) shl 28) or $8442211) shr CV_MAT_DEPTH(cType) * 4) and 15);
    //
    // (* 0x3a50 = 11 10 10 01 01 00 00 ~ array of log2(sizeof(arr_type_elem)) *)
    // // >> Following declaration is a macro definition!
    // (* const CV_ELEM_SIZE(cType)(CV_MAT_CN(cType) shl ((((SizeOf(size_t) / 4 + 1) * 16384 or
    // $3A50) shr CV_MAT_DEPTH(cType) * 2) and 3));
    //
    // // >> Following declaration is a macro definition!
    // const IPL2CV_DEPTH(depth)((((CV_8U) + (CV_16U shl 4) + (CV_32F shl 8) + (CV_64F shl 16) +
    // (CV_8S shl 20) + (CV_16S shl 24) + (CV_32S shl 28)) shr ((((depth) and $F0) shr 2) +
    // (((depth) and IPL_DEPTH_SIGN)? 20: 0))) and 15); *)
    //
    // // >> Following declaration is a macro definition!
    // const CV_MAT_ELEM_PTR_FAST(mat, row, col, pix_size)(Assert((Cardinal(row) < (Cardinal(mat)
    // .rows and (Cardinal(col) < (Cardinal(mat).cols), (mat).data.ptr + (size_t(mat).step * (row) +
    // (pix_size) * (col));
    //
    // // >> Following declaration is a macro definition!
    // const CV_MAT_ELEM_PTR(mat, row, col)CV_MAT_ELEM_PTR_FAST(mat, row, col,
    // CV_ELEM_SIZE((mat).cType));
    //
    // // >> Following declaration is a macro definition!
    // const CV_MAT_ELEM(mat, elemtype, row, col)((elemtype)CV_MAT_ELEM_PTR_FAST(mat, row, col,
    // SizeOf(elemtype)));
    //
    // CV_INLINE
    // function cvmGet(v1: mat^.cType): Double; Assert((Cardinal)row < (Cardinal)mat^.rows and
    // (Cardinal)col < (Cardinal)mat^.cols);
    //
    // if (cType = CV_32FC1) then * row)): array [0 .. col - 1] of result = ((Single(mat^.data.ptr +
    // (size_t)mat^.step; else begin Assert(cType := CV_64FC1);
    // * row)): array [0 .. col - 1] of result = ((Double(mat^.data.ptr + (size_t)mat^.step; end; end;
    //
    // CV_INLINE CV_INLINE
    // procedure cvmSet(v1: mat^.cType); Assert((Cardinal)row < (Cardinal)mat^.rows and (Cardinal)col <
    // (Cardinal)mat^.cols);
    //
    // if (cType = CV_32FC1) then * row)): array [0 .. col - 1] of ((Single(mat^.data.ptr + (size_t)
    // mat^.step = (Single)value; else begin Assert(cType := CV_64FC1);
    // * row)): array [0 .. col - 1] of ((Double(mat^.data.ptr + (size_t)mat^.step = (Double)
    // value; end; end;
    //
    // CV_INLINE
    // function cvIplDepth(v1: cType): Integer; result := CV_ELEM_SIZE1(depth) * 8 or (depth = CV_8S or
    // depth = CV_16S or depth := CV_32S ? IPL_DEPTH_SIGN: 0); end; *)
    //
    // (* ***************************************************************************************\
    // *                       Multi-dimensional dense cArray (CvMatND)                          *
    // *************************************************************************************** *)
    // (*
    // const CV_IS_MATND_HDR(mat)((mat) <> 0 and (((CvMatND(mat))^.cType and CV_MAGIC_MASK)
    // = CV_MATND_MAGIC_VAL)
    //
    // // >> Following declaration is a macro definition!
    // const CV_IS_MATND(mat)(CV_IS_MATND_HDR(mat) and ((CvMatND(mat))^.data.ptr <> 0); *)
    //
    // (* ***************************************************************************************\
    // *                                         Histogram                                      *
    // *************************************************************************************** *)
    // (* const CV_IS_HIST(hist)((hist) <> 0 and (((CvHistogram(hist))^.cType and CV_MAGIC_MASK)
    // = CV_HIST_MAGIC_VAL and (hist)^.bins <> 0)
    //
    // // >> Following declaration is a macro definition!
    // const CV_IS_UNIFORM_HIST(hist)(((hist)^.cType and CV_HIST_UNIFORM_FLAG) <> 0);
    //
    // // >> Following declaration is a macro definition!
    // const CV_IS_SPARSE_HIST(hist)CV_IS_SPARSE_MAT((hist)^.bins);
    //
    // // >> Following declaration is a macro definition!
    // const CV_HIST_HAS_RANGES(hist)(((hist)^.cType and CV_HIST_RANGES_FLAG) <> 0); *)
    // (* ***************************************************************************************\
    // *                      Other supplementary data cType definitions                         *
    // *************************************************************************************** *)
    //
    // (* ************************************** CvRect **************************************** *)
    //
    // CV_INLINE IplROI cvRectToROI(CvRect rect, Integer coi)begin IplROI roi; roi.xOffset := rect.x;
    // roi.yOffset := rect.y; roi.width := rect.width; roi.height := rect.height; roi.coi := coi;
    //
    // result := roi; end;
    //
    // CV_INLINE CvRect cvROIToRect(IplROI roi)begin result := CvRect(roi.xOffset, roi.yOffset,
    // roi.width, roi.height); end; *)
    // (* ********************************** CvTermCriteria ************************************ *)
    // (* CV_INLINE CvTermCriteria CvTermCriteria(Integer cType, Integer max_iter, Double epsilon)
    // begin CvTermCriteria T;
    //
    // T.cType := cType; T.max_iter := max_iter; T.epsilon := (Single)epsilon;
    //
    // result := T; end; *)
    // (* ****************************** CvPoint and variants ********************************** *)
    //
    // (*
    //
    // CV_INLINE CvPoint2D32f CvPoint2D32f(Double x, Double y)
    //
    // begin
    // CvPoint2D32f p;
    //
    // p.x := (Single)x;
    // p.y := (Single)y;
    //
    // result := p;
    // end;
    //
    // CV_INLINE CvPoint2D32f cvPointTo32f(CvPoint point)
    //
    // begin
    // result := CvPoint2D32f((Single)point.x, (Single)point.y);
    // end;
    //
    // CV_INLINE CvPoint cvPointFrom32f(CvPoint2D32f point)
    //
    // begin
    // CvPoint ipt;
    // ipt.x := cvRound(point.x);
    // ipt.y := cvRound(point.y);
    //
    // result := ipt;
    // end;
    //
    // CV_INLINE CvPoint3D32f CvPoint3D32f(Double x, Double y, Double z)
    //
    // begin
    // CvPoint3D32f p;
    //
    // p.x := (Single)x;
    // p.y := (Single)y;
    // p.z := (Single)z;
    //
    // result := p;
    // end;
    // CV_INLINE CvPoint2D64f CvPoint2D64f(Double x, Double y)
    //
    // begin
    // CvPoint2D64f p;
    //
    // p.x := x;
    // p.y := y;
    //
    // result := p;
    // end;
    //
    // CV_INLINE CvPoint3D64f CvPoint3D64f(Double x, Double y, Double z)
    //
    // begin
    // CvPoint3D64f p;
    //
    // p.x := x;
    // p.y := y;
    // p.z := z;
    //
    // result := p;
    // end;
    //
    // *)
    // (* ******************************* CvSize's & CvBox **************************************/ *)
    // (*
    // CV_INLINE CvSize2D32f CvSize2D32f(Double width, Double height)
    //
    // begin
    // CvSize2D32f s;
    //
    // s.width := (Single)width;
    // s.height := (Single)height;
    //
    // result := s;
    // end;
    // *)
    //
    // (* ************************************ CvSlice ***************************************** *)
    // (*
    //
    // CV_INLINE cvSlice cvSlice(Integer start, Integer end)
    //
    // begin
    // cvSlice slice;
    // slice.start_index := start;
    // slice.end_index :=
    // end;
    //
    // result := slice;
    // end;
    //
    // *)
    //
    // (* ************************************ CvScalar **************************************** *)
    // (*
    //
    // CV_INLINE  CvScalar  cvRealScalar( double val0 )
    // {
    // CvScalar scalar;
    // scalar.val[0] = val0;
    // scalar.val[1] = scalar.val[2] = scalar.val[3] = 0;
    // return scalar;
    // }
    //
    // CV_INLINE  CvScalar  cvScalarAll( double val0123 )
    // {
    // CvScalar scalar;
    // scalar.val[0] = val0123;
    // scalar.val[1] = val0123;
    // scalar.val[2] = val0123;
    // scalar.val[3] = val0123;
    // return scalar;
    // }
    // *)
    //
    // (* ************************************************************************************** *)
    // (* Dynamic Data structures *)
    // (* ************************************************************************************** *)
    // (* ******************************* Memory storage *************************************** *)
    // (*
    // const
    // CV_IS_STORAGE(storage)((storage) <> 0 and (((CvMemStorage(storage))^.signature and CV_MAGIC_MASK)
    // = CV_STORAGE_MAGIC_VAL)
    // *)
    //
    // (* ********************************** Sequence ****************************************** *)
    //
    // (* ************************************** Set ******************************************* *)
    // (*
    // { Checks whether the element pointed by ptr belongs to a set or not }
    // CV_IS_SET_ELEM(ptr)(((CvSetElem(ptr))^.flags >= 0)
    // *)
  end;

function CvPoint(const x, y: Integer): TCvPoint; inline;
function CvSize(const width, height: Integer): TCvSize; inline;
function cvScalar(const val0: Double; const val1: Double = 0; const val2: Double = 0;
  const val3: Double = 0): TCvScalar; inline;
function cvRandInt(Var rng: TCvRNG): Cardinal; inline;
function CvRect(Const x, y, width, height: Integer): TCvRect; inline;
(* Inline constructor. No data is allocated internally!!!
  * (Use together with cvCreateData, or use cvCreateMat instead to
  * get a matrix with allocated data):
*)
{
  CV_INLINE CvMat cvMat( int rows, int cols, int type, void* data CV_DEFAULT(NULL))
  {
  CvMat m;

  assert( (unsigned)CV_MAT_DEPTH(type) <= CV_64F );
  type = CV_MAT_TYPE(type);
  m.type = CV_MAT_MAGIC_VAL | CV_MAT_CONT_FLAG | type;
  m.cols = cols;
  m.rows = rows;
  m.step = m.cols*CV_ELEM_SIZE(type);
  m.data.ptr = (uchar*)data;
  m.refcount = NULL;
  m.hdr_refcount = 0;

  return m;
}
function cvMat(rows: Integer; cols: Integer; etype: Integer; data: Pointer = nil): TCvMat;
function CV_MAT_DEPTH(flags: Integer): Integer;
function CV_MAT_TYPE(flags: Integer): Integer;
function CV_ELEM_SIZE(_type: Integer): Integer;
function CV_MAT_CN(flags: Integer): Integer;
function CV_32FC1: Integer;
function CV_MAKETYPE(depth, cn: Integer): Integer;
// #define CV_MAT_ELEM( mat, elemtype, row, col )           \
// (*(elemtype*)CV_MAT_ELEM_PTR_FAST( mat, row, col, sizeof(elemtype)))
function CV_MAT_ELEM(const mat: TCvMat; const elemtype: Integer; const row, col: Integer): Pointer;
// #define CV_MAT_ELEM_PTR_FAST( mat, row, col, pix_size )  \
// (assert( (unsigned)(row) < (unsigned)(mat).rows &&   \
// (unsigned)(col) < (unsigned)(mat).cols ),   \
// (mat).data.ptr + (size_t)(mat).step*(row) + (pix_size)*(col))
function CV_MAT_ELEM_PTR_FAST(const mat: TCvMat; const row, col, pix_size: Integer): Pointer;

implementation

function CV_MAT_ELEM_PTR_FAST(const mat: TCvMat; const row, col, pix_size: Integer): Pointer;
begin
  Assert((Cardinal(row) < mat.rows) and (Cardinal(col) < mat.cols));
  Result := Pointer(Integer(mat.data) + mat.step * row + pix_size * col);
end;

function CV_MAT_ELEM(const mat: TCvMat; const elemtype: Integer; const row, col: Integer): Pointer;
begin
  Result := CV_MAT_ELEM_PTR_FAST(mat, row, col, CV_ELEM_SIZE(elemtype));
end;

function CvAttrList(const attr: ppCVChar = nil; next: pCvAttrList = nil): TCvAttrList;
begin
  Result.attr := attr;
  Result.next := next;
end;

function CV_MAT_DEPTH(flags: Integer): Integer;
begin
  Result := flags and CV_MAT_DEPTH_MASK;
end;

function CV_MAT_TYPE(flags: Integer): Integer;
begin
  Result := flags and CV_MAT_TYPE_MASK;
end;

function CV_MAT_CN(flags: Integer): Integer;
begin
  Result := ((((flags) and CV_MAT_CN_MASK) shr CV_CN_SHIFT) + 1);
end;

function CV_ELEM_SIZE(_type: Integer): Integer;
begin
  Result := (CV_MAT_CN(_type) shl ((((SizeOf(Integer) div 4 + 1) * 16384 or $3A50)
    shr CV_MAT_DEPTH(_type) * 2) and 3));
end;

function CV_32FC1: Integer;
begin
  Result := CV_MAKETYPE(CV_32F, 1);
end;

function CV_MAKETYPE(depth, cn: Integer): Integer;
begin
  Result := (CV_MAT_DEPTH(depth) + (((cn) - 1) shl CV_CN_SHIFT));
end;

function cvMat(rows: Integer; cols: Integer; etype: Integer; data: Pointer = nil): TCvMat;
begin
  if not(CV_MAT_DEPTH(etype) <= CV_64F) then
    exit;
  etype := CV_MAT_TYPE(etype);
  Result._type := CV_MAT_MAGIC_VAL or CV_MAT_CONT_FLAG or etype;
  Result.cols := cols;
  Result.rows := rows;
  Result.step := Result.cols * CV_ELEM_SIZE(etype);
  Result.data := data;
  Result.refcount := nil;
  Result.hdr_refcount := 0;
end;

function CvPoint(const x, y: Integer): TCvPoint;
begin
  Result.x := x;
  Result.y := y;
end;

function cvScalar(const val0, val1, val2, val3: Double): TCvScalar;
begin
  Result.val[0] := val0;
  Result.val[1] := val1;
  Result.val[2] := val2;
  Result.val[3] := val3;
end;

function CvSize(const width, height: Integer): TCvSize;
begin
  Result.width := width;
  Result.height := height;
end;

function CvRect(Const x, y, width, height: Integer): TCvRect;
begin
  Result.x := x;
  Result.y := y;
  Result.width := width;
  Result.height := height;
end;

function cvRandInt(Var rng: TCvRNG): Cardinal;
begin
{$Q-}
  rng := TCvRNG(rng * CV_RNG_COEFF + (rng shr 32));
{$Q+}
  Result := Cardinal(rng);
end;

end.
