(*
  **************************************************************************************************
  Project Delphi-OpenCV
  **************************************************************************************************
  Contributor:
  Laentir Valetov
  email:laex@bk.ru
  Mikhail Grigorev
  email:sleuthhound@gmail.com
  **************************************************************************************************
  You may retrieve the latest version of this file at the GitHub,
  located at git://github.com/Laex/Delphi-OpenCV.git
  **************************************************************************************************
  License:
  The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License");
  you may not use this file except in compliance with the License. You may obtain a copy of the
  License at http://www.mozilla.org/MPL/

  Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF
  ANY KIND, either express or implied. See the License for the specific language governing rights
  and limitations under the License.

  Alternatively, the contents of this file may be used under the terms of the
  GNU Lesser General Public License (the  "LGPL License"), in which case the
  provisions of the LGPL License are applicable instead of those above.
  If you wish to allow use of your version of this file only under the terms
  of the LGPL License and not to allow others to use your version of this file
  under the MPL, indicate your decision by deleting  the provisions above and
  replace  them with the notice and other provisions required by the LGPL
  License.  If you do not delete the provisions above, a recipient may use
  your version of this file under either the MPL or the LGPL License.

  For more information about the LGPL: http://www.gnu.org/copyleft/lesser.html
  **************************************************************************************************
  Warning: Using Delphi XE3 syntax!
  **************************************************************************************************
  The Initial Developer of the Original Code:
  OpenCV: open source computer vision library
  Homepage:    http://ocv.org
  Online docs: http://docs.ocv.org
  Q&A forum:   http://answers.ocv.org
  Dev zone:    http://code.ocv.org
  **************************************************************************************************
  Original file:
  opencv\modules\core\include\opencv2\core\types_c.h
  *************************************************************************************************
*)

unit ocv.core.types_c;

{$I OpenCV.inc}

interface

Uses
{$IFDEF HAS_UNITSCOPE}
  Winapi.Windows
{$ELSE}
    Windows
{$ENDIF}
    ;

const
  // Ќаименьшее число дл€ которого выполн€етс€ условие 1.0+DBL_EPSILON <> 1.0
  DBL_EPSILON = 2.2204460492503131E-016;
  DBL_MAX = 1.7976931348623157E+308;
  FLT_EPSILON = 1.19209290E-07;
  FLT_MAX = 1E+37;

type
  cbool = bytebool;
  Float = Single;
  uint = NativeUInt;
  size_t = NativeUInt;
  pFloat = ^Float;
  ppFloat = ^pFloat;
  pPointer = System.pPointer; // ^Pointer;
  ppvoid = pPointer;

  TSingleArray1D = array [0 .. 1] of Single;
  pSingleArray1D = ^TSingleArray1D;
  TSingleArray2D = array [0 .. 1] of pSingleArray1D;
  pSingleArray2D = ^TSingleArray2D;

  CVChar = AnsiChar;
  TCVChar = CVChar;
  pCVChar = pAnsiChar;
  TpCVCharArray = array [0 .. 0] of pCVChar;
  ppCVChar = ^TpCVCharArray;
  // {$IFNDEF WIN64}
  // size_t = UInt32;
  // {$ELSE}
  // size_t = UInt64;
  // {$ENDIF}
{$IFNDEF DELPHIXE2_UP}
{$IFDEF CLR}
{$IFDEF DELPHI2007}
  NativeInt = size_t;
  NativeUInt = size_t;
{$ELSE}
  NativeInt = Integer;
  NativeUInt = Cardinal;
{$ENDIF}
{$ELSE}
{$IFDEF FPC}
  NativeInt = SizeInt;
  NativeUInt = SizeUInt;
{$ELSE}
  NativeInt = Integer;
  NativeUInt = Cardinal;
{$ENDIF}
{$ENDIF}
{$ENDIF}
function strdup(const str: pCVChar): pCVChar;
function cv_stricmp(const str1, str2: pCVChar): Integer;
procedure strcpy(var str1: pCVChar; const str2: pCVChar); overload;
procedure strcpync(const str1: pCVChar; const str2: pCVChar); overload;
procedure strcat(var str1: pCVChar; const str2: pCVChar);

type
  uchar = Byte;
{$EXTERNALSYM uchar}
  puchar = PByte;
  ushort = Word;
{$EXTERNALSYM ushort}
  schar = ShortInt;
{$EXTERNALSYM schar}
  pschar = ^schar;
{$EXTERNALSYM pschar}
  unsigned = longint;
{$EXTERNALSYM unsigned}
  punsigned = ^longint;
{$EXTERNALSYM punsigned}

  (* CvArr* is used to pass arbitrary
    * cArray-like data structures
    * into functions where the particular
    * cArray cType is recognized at runtime:
  *)
type
  // TCvArr = record
  // end;

  TVoid = record
  end;

  pCvArr = Pointer;
  TCvArrArray = array [0 .. 0] of pCvArr;
  pCvArrArray = ^TCvArrArray;

  TCv32suf = record
    case Byte of
      0:
        (i: Integer);
      1:
        (u: Cardinal);
      2:
        (f: Single);
  end;

  TCv64suf = record
    case Byte of
      0:
        (i: int64);
      1:
        (u: UInt64);
      2:
        (f: Double);
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
{$HPPEMIT '#include 'tegra_round.hpp''}
{$ENDIF}

const
  CV_PI = 3.1415926535897932384626433832795;
{$EXTERNALSYM CV_PI}
  CV_LOG2 = 0.69314718055994530941723212145818;
{$EXTERNALSYM CV_LOG2}

  { ************** Random number generation ****************** }
type
  TCvRNG = UInt64;
  pCvRNG = ^TCvRNG;
  { EXTERNALSYM CvRNG }

const
  CV_RNG_COEFF = Cardinal(4164903690);
{$EXTERNALSYM CV_RNG_COEFF}
  (*
    CV_INLINE CvRNG cvRNG( int64 seed CV_DEFAULT(-1))
    {
    CvRNG rng = seed ? (uint64)seed : (uint64)(int64)-1;
    return rng;
    }
  *)
function cvRNG(seed: int64 = -1): TCvRNG; {$IFDEF USE_INLINE}inline; {$ENDIF}
{ ***************************************************************************************
  *                                  Image cType (IplImage)                             *
  *************************************************************************************** }

{$IFNDEF HAVE_IPL}

(*
  * The following definitions (until #endif)'
  * is an extract from IPL headers.
  * Copyright (c) 1995 Intel Corporation.
*)
const
  IPL_DEPTH_SIGN = Integer($80000000);
{$EXTERNALSYM IPL_DEPTH_SIGN}
  IPL_DEPTH_1U = 1;
{$EXTERNALSYM IPL_DEPTH_1U}
  IPL_DEPTH_8U = 8;
{$EXTERNALSYM IPL_DEPTH_8U}
  IPL_DEPTH_16U = 16;
{$EXTERNALSYM IPL_DEPTH_16U}
  IPL_DEPTH_32F = 32;
{$EXTERNALSYM IPL_DEPTH_32F}
  { for storing double-precision
    floating point data in IplImage's }
  IPL_DEPTH_64F = 64;
{$EXTERNALSYM IPL_DEPTH_64F}
  IPL_DEPTH_8S: TCvRNG = TCvRNG(IPL_DEPTH_SIGN or 8);
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
  // * Sub-pixel interpolation methods */

  CV_INTER_NN = 0;
  CV_INTER_LINEAR = 1;
  CV_INTER_CUBIC = 2;
  CV_INTER_AREA = 3;
  CV_INTER_LANCZOS4 = 4;

type

  pIplImage = ^TIplImage;
  TpIplImageArray = array [0 .. 1] of pIplImage;
  TArrayOfpIplImage = TArray<pIplImage>;
  ppIplImage = ^TpIplImageArray;
  pIplROI = ^TIplROI;
  pIplTileInfo = ^TIplTileInfo;

  TIplROI = record
    coi: Integer; (* 0 - no COI (all channels are selected), 1 - 0th channel is selected ... *)
    xOffset: Integer;
    yOffset: Integer;
    width: Integer;
    height: Integer;
  end;

  TiplCallBack = procedure(img: pIplImage; xIndex: Integer; yIndex: Integer; mode: Integer); cdecl;

  TIplTileInfo = record
    callBack: TiplCallBack;
    id: Pointer;
    tileData: pCVChar;
    width: Integer;
    height: Integer;
  end;

  TA4CVChar = array [0 .. 3] of CVChar;

  TIplImage = record
    nSize: Integer; (* sizeof(IplImage) *)
    id: Integer; (* version (=0) *)
    nChannels: Integer; (* Most of OpenCV functions support 1,2,3 or 4 channels *)
    alphaChannel: Integer; (* Ignored by OpenCV *)
    depth: Integer; (* Pixel depth in bits: Pixel depth in bits: IPL_DEPTH_8U, IPL_DEPTH_8S, IPL_DEPTH_16S,
      IPL_DEPTH_32S, IPL_DEPTH_32F and IPL_DEPTH_64F are supported. *)
    colorModel: TA4CVChar; (* Ignored by OpenCV *)
    channelSeq: TA4CVChar; (* ditto *)
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
    imageData: PByte; (* Pointer to aligned image data. *)
    widthStep: Integer; (* Size of aligned image row in bytes. *)
    BorderMode: array [0 .. 3] of Integer; (* Ignored by ocv. *)
    BorderConst: array [0 .. 3] of Integer; (* Ditto. *)
    imageDataOrigin: PByte; (* Pointer to very origin of image data *)
  end;

  TcvImage = TIplImage;
  pcvImage = pIplImage;

  // type       _IplTileInfo IplTileInfo = ;
  pIplConvKernel = ^TIplConvKernel;

  TIplConvKernel = record
    nCols: Integer;
    nRows: Integer;
    anchorX: Integer;
    anchorY: Integer;
    values: ^Integer;
    nShiftR: Integer;
  end;

  IplConvKernelFP = record
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

  (* ***************************************************************************************
    *                                 Matrix cType (CvMat)                                *************************************************************************************** *)

const
  CV_CN_MAX = 512;
{$EXTERNALSYM CV_CN_MAX}
  CV_CN_SHIFT = 3;
{$EXTERNALSYM CV_CN_SHIFT}
  CV_DEPTH_MAX = (1 shl CV_CN_SHIFT);
{$EXTERNALSYM CV_DEPTH_MAX}
  CV_8U = 0; // byte - 1-byte unsigned
{$EXTERNALSYM CV_8U}
  CV_8S = 1; // ShortInt - 1-byte signed
{$EXTERNALSYM CV_8S}
  CV_16U = 2; // word - 2-byte unsigned
{$EXTERNALSYM CV_16U}
  CV_16S = 3; // SmallInt - 2-byte signed
{$EXTERNALSYM CV_16S}
  CV_32S = 4; // integer - 4-byte signed integer
{$EXTERNALSYM CV_32S}
  CV_32F = 5; // single - 4-byte floating point
{$EXTERNALSYM CV_32F}
  CV_64F = 6; // double - 8-byte floating point
{$EXTERNALSYM CV_64F}
  CV_USRTYPE1 = 7;
{$EXTERNALSYM CV_USRTYPE1}
  CV_MAT_DEPTH_MASK = (CV_DEPTH_MAX - 1);
{$EXTERNALSYM CV_MAT_DEPTH_MASK}
function CV_8UC1: Integer; {$IFDEF USE_INLINE}inline; {$ENDIF}
{$EXTERNALSYM CV_8UC1}
function CV_8UC2: Integer; {$IFDEF USE_INLINE}inline; {$ENDIF}
{$EXTERNALSYM CV_8UC2}
function CV_8UC3: Integer; {$IFDEF USE_INLINE}inline; {$ENDIF}
{$EXTERNALSYM CV_8UC3}
(*
  CV_8UC4 = CV_MAKETYPE(CV_8U, 4);
  {$EXTERNALSYM CV_8UC4}
  CV_8SC1 = CV_MAKETYPE(CV_8S, 1);
  {$EXTERNALSYM CV_8SC1}
  CV_8SC2 = CV_MAKETYPE(CV_8S, 2);
  {$EXTERNALSYM CV_8SC2}
  CV_8SC3 = CV_MAKETYPE(CV_8S, 3);
  {$EXTERNALSYM CV_8SC3}
  CV_8SC4 = CV_MAKETYPE(CV_8S, 4);
  {$EXTERNALSYM CV_8SC4}
  CV_16UC1 = CV_MAKETYPE(CV_16U, 1);
  {$EXTERNALSYM CV_16UC1}
  CV_16UC2 = CV_MAKETYPE(CV_16U, 2);
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
  CV_16SC(n) CV_MAKETYPE(CV_16S, (n));
*)
// const
// CV_32SC2 = CV_MAKETYPE(CV_32S, 2);
{$EXTERNALSYM CV_32SC2}
function CV_32SC2: Integer; {$IFDEF USE_INLINE}inline; {$ENDIF}
(*
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
*)
function CV_32FC2: Integer; {$IFDEF USE_INLINE}inline; {$ENDIF}
{$EXTERNALSYM CV_32FC2}
function CV_32FC3: Integer; {$IFDEF USE_INLINE}inline; {$ENDIF}
{$EXTERNALSYM CV_32FC3}
(*
  const
  CV_32FC4 = CV_MAKETYPE(CV_32F, 4);
  {$EXTERNALSYM CV_32FC4}

  // >> Following declaration is a macro definition!
  const
  CV_32FC(n)CV_MAKETYPE(CV_32F, (n));
*)

function CV_64FC1: Integer; {$IFDEF USE_INLINE}inline; {$ENDIF}
{$EXTERNALSYM CV_64FC1}
function CV_64FC2: Integer; {$IFDEF USE_INLINE}inline; {$ENDIF}
{$EXTERNALSYM CV_64FC2}
function CV_64FC3: Integer; {$IFDEF USE_INLINE}inline; {$ENDIF}
{$EXTERNALSYM CV_64FC3}
(*
  const
  CV_64FC4 = CV_MAKETYPE(CV_64F, 4);
  {$EXTERNALSYM CV_64FC4}

  // >> Following declaration is a macro definition!
  const
  CV_64FC(n)CV_MAKETYPE(CV_64F, (n));
*)

// * get reference to pixel at (col,row),
// for multi-channel images (col) should be multiplied by number of channels */
function CV_IMAGE_ELEM(image: pIplImage; size_elemtype, row, col: Integer): Pointer; {$IFDEF USE_INLINE}inline; {$ENDIF}
// (((elemtype*)((image)->imageData + (image)->widthStep*(row)))[(col)])

const
  CV_AUTO_STEP = $7FFFFFFF;
{$EXTERNALSYM CV_AUTO_STEP}

  (*
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
  ppCvMat = ^pCvMat;

  TCvMatDataUnion = record
    case Byte of
      0:
        (ptr: PByte);
      1:
        (s: PSmallInt);
      2:
        (i: PInteger);
      3:
        (fl: pSingle);
      4:
        (db: pDouble);
  end;

  TCvMat = record
  public
    _type: Integer;
    step: Integer;

    refcount: PInteger;
    hdr_refcount: Integer;

    data: TCvMatDataUnion;

    rows: Integer;
    cols: Integer;
  private
    function GetMatByte(const row, col: Integer): Byte; inline;
    function GetMatDouble(const row, col: Integer): Double; inline;
    function GetMatInteger(const row, col: Integer): Integer; inline;
    function GetMatSingle(const row, col: Integer): Single; inline;
    procedure SetMatByte(const row, col: Integer; const Value: Byte); inline;
    procedure SetMatDouble(const row, col: Integer; const Value: Double); inline;
    procedure SetMatInteger(const row, col, Value: Integer); inline;
    procedure SetMatSingle(const row, col: Integer; const Value: Single); inline;
  public
    function AsDouble: pDouble; inline;
    function AsInteger: PInteger; inline;
    function AsByte: PByte; inline;
    function AsSingle: pSingle; inline;
    property MatDouble[const row, col: Integer]: Double Read GetMatDouble write SetMatDouble;
    property MatSingle[const row, col: Integer]: Single Read GetMatSingle write SetMatSingle;
    property MatInteger[const row, col: Integer]: Integer Read GetMatInteger write SetMatInteger;
    property MatByte[const row, col: Integer]: Byte Read GetMatByte write SetMatByte;
  end;

  { ***************************************************************************************
    *                       Multi-dimensional dense cArray (CvMatND)                      *
    *************************************************************************************** }

const
  CV_MATND_MAGIC_VAL = $42430000;
{$EXTERNALSYM CV_MATND_MAGIC_VAL}
  CV_TYPE_NAME_MATND = 'opencv-nd-matrix';
{$EXTERNALSYM CV_TYPE_NAME_MATND}
  CV_MAX_DIM = 32;
{$EXTERNALSYM CV_MAX_DIM}
  CV_MAX_DIM_HEAP = 1024;
{$EXTERNALSYM CV_MAX_DIM_HEAP}
  CV_MAX_ARR = 10;
{$EXTERNALSYM CV_MAX_ARR}

type
  TCvMatNDdim = record
    size: Integer;
    step: Integer;
  end;

  TCvMatNDdata = TCvMatDataUnion;

  pCvMatND = ^TCvMatND;

  TCvMatND = record
    _type: Integer;
    dims: Integer;
    refcount: ^Integer;
    hdr_refcount: Integer;
    data: TCvMatNDdata;
    dim: array [0 .. CV_MAX_DIM - 1] of TCvMatNDdim;
  end;

function CV_IS_MATND_HDR(const mat: Pointer): Boolean; inline;
// #define CV_IS_MATND_HDR(mat) \
// ((mat) != NULL && (((const CvMatND*) (mat)) - > type & CV_MAGIC_MASK) = = CV_MATND_MAGIC_VAL)

function CV_IS_MATND(const mat: Pointer): Boolean; inline;
// #define CV_IS_MATND(mat) \
// (CV_IS_MATND_HDR(mat) && ((const CvMatND*)(mat))->data.ptr != NULL)

// ***************************************************************************************
// *                                         Histogram                                   *
// ***************************************************************************************

type
  TCvHistType = Integer;
{$EXTERNALSYM TCvHistType}

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
  pCvHistogram = ^TCvHistogram;

  TCvHistogram = record
    _type: Integer;
    bins: pIplImage;
    thresh: array [0 .. CV_MAX_DIM - 1, 0 .. 1] of Single;
    (* For uniform histograms. *)
    thresh2: pSingle; (* For non-uniform histograms. *)
    mat: TCvMatND; (* Embedded matrix header for array histograms. *)
  end;

  (* ***************************************************************************************\
    *                      Other supplementary data cType definitions                         *
    *************************************************************************************** *)

  (* ************************************** CvRect **************************************** *)

type
  pCvRect = ^TCvRect;

  TCvRect = record
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
  pCvTermCriteria = ^TCvTermCriteria;

  TCvTermCriteria = record
    cType: Integer; (* may be combination of *)
    max_iter: Integer;
    epsilon: Double;
  end;

  (* ****************************** CvPoint and variants ********************************** *)

type
  pCvPoint = ^TCvPoint;

  TCvPoint = record
    x: Integer;
    y: Integer;
  end;

  TCvPointArray = array [0 .. 100] of TCvPoint;
  pCvPointArray = ^TCvPointArray;

  pCvPoint2D32f = ^TCvPoint2D32f;

  TCvPoint2D32f = record
    x: Single;
    y: Single;
{$IFDEF DELPHIXE2_UP}
    class operator Subtract(a, b: TCvPoint2D32f): TCvPoint2D32f; // Subtraction of type TCvPoint2D32f
    function Norm: Single;
    function cvPoint: TCvPoint;
{$ENDIF}
  end;

  TcvPoint2f = TCvPoint2D32f;
  pcvPoint2f = pCvPoint2D32f;
  TArrayOfcvPoint2f = TArray<TcvPoint2f>;

  pCvPoint3D32f = ^TCvPoint3D32f;

  TCvPoint3D32f = record
    x: Single;
    y: Single;
    z: Single;
  end;

  pCvPoint3D64f = ^TCvPoint3D64f;

  TCvPoint3D64f = record
    x: Double;
    y: Double;
    z: Double;
  end;

function cvPoint3D32f(const x, y, z: Single): TCvPoint3D32f; {$IFDEF USE_INLINE}inline; {$ENDIF}

Type
  pCvPoint2D64f = ^TCvPoint2D64f;

  TCvPoint2D64f = record
    x: Double;
    y: Double;
  end;

Const
  cvZeroPoint: TCvPoint = (x: 0; y: 0);

  (* ******************************* CvSize's & CvBox **************************************/ *)

type
  pCvSize = ^TCvSize;

  TCvSize = record
    width: Integer;
    height: Integer;
  end;

const
  ZeroCvSize: TCvSize = (width: 0; height: 0);

Type
  pCvSize2D32f = ^TCvSize2D32f;

  TCvSize2D32f = record
    width: Single;
    height: Single;
  end;

  pCvBox2D = ^TCvBox2D;

  TCvBox2D = record
    center: TCvPoint2D32f; (* Center of the box. *)
    size: TCvSize2D32f; (* Box width and length. *)
    angle: Single; (* Angle between the horizontal axis *)
  end;

  (* Line iterator state: *)
type
  pCvLineIterator = ^TCvLineIterator;

  TCvLineIterator = record
    ptr: ^uchar;
    err: Integer;
    plus_delta: Integer;
    minus_delta: Integer;
    plus_step: Integer;
    minus_step: Integer;
  end;

  (* ************************************ CvSlice ***************************************** *)

type
  TCvSlice = record
    start_index, end_index: Integer;
  end;

const
  CV_WHOLE_SEQ_END_INDEX = $3FFFFFFF;
{$EXTERNALSYM CV_WHOLE_SEQ_END_INDEX}
  CV_WHOLE_SEQ: TCvSlice = (start_index: 0; end_index: CV_WHOLE_SEQ_END_INDEX);
{$EXTERNALSYM CV_WHOLE_SEQ}
  (* ************************************ CvScalar **************************************** *)

type
  pCvScalar = ^TCvScalar;

  TCvScalar = record
    val: array [0 .. 3] of Double;
    class operator LessThan(a, b: TCvScalar): Boolean;
    class operator GreaterThan(a, b: TCvScalar): Boolean;
  end;

  (* ************************************************************************************** *)
  (* Dynamic Data structures *)
  (* ************************************************************************************** *)
  (* ******************************* Memory storage *************************************** *)
type
  pCvMemBlock = ^TCvMemBlock;

  TCvMemBlock = record
    prev: pCvMemBlock;
    next: pCvMemBlock;
  end;

const
  CV_STORAGE_MAGIC_VAL = $42890000;
{$EXTERNALSYM CV_STORAGE_MAGIC_VAL}

type
  pCvMemStorage = ^TCvMemStorage;

  TCvMemStorage = record
    signature: Integer;
    bottom: pCvMemBlock;
    top: pCvMemBlock; (* First allocated block. *)
    parent: pCvMemStorage;
    (* Current memory block - top of the stack. *)    (* We get new blocks from parent as needed. *)
    block_size: Integer; (* Block size. *)
    free_space: Integer; (* Remaining free space in current block. *)
  end;

type
  pCvMemStoragePos = ^TCvMemStoragePos;

  TCvMemStoragePos = record
    top: pCvMemBlock;
    free_space: Integer;
  end;

  (* ********************************** Sequence ****************************************** *)
type
  pCvSeqBlock = ^TCvSeqBlock;

  TCvSeqBlock = record
    prev: pCvSeqBlock; (* Previous sequence block. *)
    next: pCvSeqBlock; (* Next sequence block. *)
    start_index: Integer; (* Index of the first element in the block + *)
    count: Integer; (* Number of elements in the block. *)
    data: Pointer; (* Pointer to the first element of the block. *)
  end;

  TCvSeqBlockArray = array [0 .. 0] of TCvSeqBlock;
  pCvSeqBlockArray = ^TCvSeqBlockArray;

  TCV_TREE_NODE_FIELDS<node_type> = record
    flags: Integer; (* Miscellaneous flags. *)
    header_size: Integer; (* Size of sequence header. *)
    h_prev: ^node_type; (* Previous sequence. *)
    h_next: ^node_type; (* Next sequence. *)
    v_prev: ^node_type; (* 2nd previous sequence. *)
    v_next: ^node_type; (* 2nd next sequence. *)
  end;

  pCvSeq = ^TCvSeq;

  TCvSeq = record
    flags: Integer; (* Miscellaneous flags. *)
    header_size: Integer; (* Size of sequence header. *)
    h_prev: pCvSeq; (* Previous sequence. *)
    h_next: pCvSeq; (* Next sequence. *)
    v_prev: pCvSeq; (* 2nd previous sequence. *)
    v_next: pCvSeq; (* 2nd next sequence. *)
    total: Integer; (* Total number of elements. *)
    elem_size: Integer; (* Size of sequence element in bytes. *)
    block_max: Pointer; (* Maximal bound of the last block. *)
    ptr: pschar; (* Current write pointer. *)
    delta_elems: Integer; (* Grow seq this many at a time. *)
    storage: pCvMemStorage; (* Where the seq is stored. *)
    free_blocks: pCvSeqBlock; (* Free blocks list. *)
    first: pCvSeqBlock; (* Pointer to the first sequence block. *)
  end;

  (*
    Read/Write sequence.
    Elements can be dynamically inserted to or deleted from the sequence.
  *)
  TCV_SEQUENCE_FIELDS = record
    CV_TREE_NODE_FIELDS: TCV_TREE_NODE_FIELDS<TCvSeq>;
    total: Integer; (* Total number of elements. *)
    elem_size: Integer; (* Size of sequence element in bytes. *)
    block_max: pschar; (* Maximal bound of the last block. *)
    ptr: pschar; (* Current write pointer. *)
    delta_elems: Integer; (* Grow seq this many at a time. *)
    storage: pCvMemStorage; (* Where the seq is stored. *)
    free_blocks: pCvSeqBlock; (* Free blocks list. *)
    first: pCvSeqBlock; (* Pointer to the first sequence block. *)
  end;

  pCvSeqArray = array [0 .. 1] of pCvSeq;
  ppCvSeq = ^pCvSeqArray;

const
  CV_TYPE_NAME_SEQ = 'opencv-sequence';
{$EXTERNALSYM CV_TYPE_NAME_SEQ}
  CV_TYPE_NAME_SEQ_TREE = 'opencv-sequence-tree';
{$EXTERNALSYM CV_TYPE_NAME_SEQ_TREE}

  // ***************************************************************************************
  // *                                         Contours                                    *
  // ***************************************************************************************
type
  pCvContour = ^TCvContour;

  TCvContour = record
    flags: Integer; // micsellaneous flags
    header_size: Integer; // size of sequence header
    h_prev: pCvArr; // previous sequence
    h_next: pCvArr; // next sequence
    v_prev: pCvArr; // 2nd previous sequence
    v_next: pCvArr; // 2nd next sequence
    total: Integer; // total number of elements
    elem_size: Integer; // size of sequence element in bytes
    block_max: pAnsiChar; // maximal bound of the last block
    ptr: pAnsiChar; // current write pointer
    delta_elems: Integer; // how many elements allocated when the seq grows
    storage: pCvMemStorage; // where the seq is stored
    free_blocks: pCvSeqBlock; // free blocks list
    first: pCvSeqBlock; // pointer to the first sequence block
    rect: TCvRect;
    color: Integer;
    reserved: array [0 .. 2] of Integer;
  end;

  (* ************************************** Set ******************************************* *)
  (*
    Set.
    Order is not preserved. There can be gaps between sequence elements.
    After the element has been inserted it stays in the same place all the time.
    The MSB(most-significant or sign bit) of the first field (flags) is 0 iff the element exists.
  *)
type
  TCV_SET_ELEM_FIELDS<elem_type> = record
    flags: Integer;
    next_free: ^elem_type;
  end;

  pCvSetElem = ^TCvSetElem;

  TCvSetElem = record
    flags: Integer;
    next_free: pCvSetElem;
  end;

  TCvSetElemArray = array [0 .. 0] of TCvSetElem;
  pCvSetElemArray = ^TCvSetElemArray;

  TCV_SET_FIELDS = record
    CV_SEQUENCE_FIELDS: TCV_SEQUENCE_FIELDS;
    free_elems: pCvSetElem;
    active_count: Integer;
  end;

  pCvSet = ^TCvSet;

  TCvSet = record
    flags: Integer; (* Miscellaneous flags. *)
    header_size: Integer; (* Size of sequence header. *)
    h_prev: pCvSeq; (* Previous sequence. *)
    h_next: pCvSeq; (* Next sequence. *)
    v_prev: pCvSeq; (* 2nd previous sequence. *)
    v_next: pCvSeq; (* 2nd next sequence. *)
    total: Integer; (* Total number of elements. *)
    elem_size: Integer; (* Size of sequence element in bytes. *)
    block_max: Pointer; (* Maximal bound of the last block. *)
    ptr: Pointer; (* Current write pointer. *)
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
  (*
    Checks whether the element pointed by ptr belongs to a set or not
    #define CV_IS_SET_ELEM( ptr )  (((CvSetElem* )(ptr))->flags >= 0)
  *)
function CV_IS_SET_ELEM(ptr: Pointer): Boolean; {$IFDEF USE_INLINE}inline; {$ENDIF}
{ ***************************************************************************************
  *                      Multi-dimensional sparse cArray (CvSparseMat)                  *
  *************************************************************************************** }

const
  CV_SPARSE_MAT_MAGIC_VAL = $42440000;
{$EXTERNALSYM CV_SPARSE_MAT_MAGIC_VAL}
  CV_TYPE_NAME_SPARSE_MAT = 'opencv-sparse-matrix';
{$EXTERNALSYM CV_TYPE_NAME_SPARSE_MAT}

type
  pCvSparseMat = ^TCvSparseMat;

  TCvSparseMat = record
    cType: Integer;
    dims: Integer;
    refcount: ^Integer;
    hdr_refcount: Integer;
    heap: pCvSet;
    hashtable: ^Pointer;
    hashsize: Integer;
    valoffset: Integer;
    idxoffset: Integer;
    size: array [0 .. CV_MAX_DIM - 1] of Integer;
  end;

  { #define CV_IS_SPARSE_MAT_HDR(mat) \
    ((mat) != NULL && \
    (((const CvSparseMat*)(mat))->type & CV_MAGIC_MASK) == CV_SPARSE_MAT_MAGIC_VAL) }

  { #define CV_IS_SPARSE_MAT(mat) \
    CV_IS_SPARSE_MAT_HDR(mat) }

  // **************** iteration through a sparse array *****************
  pCvSparseNode = ^TCvSparseNode;

  TCvSparseNode = record
    hashval: Cardinal;
    next: pCvSparseNode;
  end;

  pCvSparseMatIterator = ^TCvSparseMatIterator;

  TCvSparseMatIterator = record
    mat: pCvSparseMat;
    node: pCvSparseNode;
    curidx: Integer;
  end;

  // define CV_NODE_VAL(mat,node)   ((void*)((uchar*)(node) + (mat)->valoffset))
  // define CV_NODE_IDX(mat,node)   ((int*)((uchar*)(node) + (mat)->idxoffset))

  (* ************************************ Graph ******************************************* *)

  (*
    We represent a graph as a set of vertices.
    Vertices contain their adjacency lists (more exactly, pointers to first incoming or
    outcoming edge (or 0 if isolated vertex)). Edges are stored in another set.
    There is a singly-linked list of incoming/outcoming edges for each vertex.

    Each edge consists of

    o   Two pointers to the starting and ending vertices
    (vtx[0] and vtx[1] respectively).

    A graph may be oriented or not. In the latter case, edges between
    vertex i to vertex j are not distinguished during search operations.

    o   Two pointers to next edges for the starting and ending vertices, where
    next[0] points to the next edge in the vtx[0] adjacency list and
    next[1] points to the next edge in the vtx[1] adjacency list.
  *)
Type

  pCvGraphEdge = ^TCvGraphEdge;
  pCvGraphVtx = ^TCvGraphVtx;

  TCV_GRAPH_EDGE_FIELDS = record
    flags: Integer;
    weight: Single;
    next: array [0 .. 1] of pCvGraphEdge;
    vtx: array [0 .. 1] of pCvGraphVtx;
  end;

  TCV_GRAPH_VERTEX_FIELDS = record
    flags: Integer;
    first: pCvGraphEdge;
  end;

  (*
    typedef struct CvGraphEdge
    {
    CV_GRAPH_EDGE_FIELDS()
    } CvGraphEdge;
  *)

  TCvGraphEdge = record
    CV_GRAPH_EDGE_FIELDS: TCV_GRAPH_EDGE_FIELDS;
  end;

  TCvGraphEdgeArray = array [0 .. 0] of pCvGraphEdge;
  pCvGraphEdgeArray = ^TCvGraphEdgeArray;

  (*
    typedef struct CvGraphVtx
    {
    CV_GRAPH_VERTEX_FIELDS()
    } CvGraphVtx;
  *)
  TCvGraphVtx = record
    CV_GRAPH_VERTEX_FIELDS: TCV_GRAPH_VERTEX_FIELDS;
  end;

  TCvGraphVtxArray = array [0 .. 0] of pCvGraphVtx;
  pCvGraphVtxArray = ^TCvGraphVtxArray;

  (*
    typedef struct CvGraphVtx2D
    {
    CV_GRAPH_VERTEX_FIELDS()
    CvPoint2D32f* ptr;
    } CvGraphVtx2D;
  *)
  pCvGraphVtx2D = ^TCvGraphVtx2D;

  TCvGraphVtx2D = record
    CV_GRAPH_VERTEX_FIELDS: TCV_GRAPH_VERTEX_FIELDS;
    ptr: pCvPoint2D32f;
  end;

  (*
    Graph is "derived" from the set (this is set a of vertices)
    and includes another set (edges)
  *)
  TCV_GRAPH_FIELDS = record
    CV_SET_FIELDS: TCV_SET_FIELDS;
    edges: pCvSet;
  end;

  (*
    typedef struct CvGraph
    {
    CV_GRAPH_FIELDS()
    } CvGraph;
  *)

  pCvGraph = ^TCvGraph;

  TCvGraph = record
    CV_GRAPH_FIELDS: TCV_GRAPH_FIELDS;
    edges: pCvSet;
  end;

const
  CV_TYPE_NAME_GRAPH = 'opencv-graph';

  (* ********************************** Chain/Countour ************************************ *)

type
  pCvChain = ^TCvChain;

  TCvChain = record
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
  // = record
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
{$EXTERNALSYM CV_SET_MAGIC_VAL}
  // const CV_IS_SET(set)((set) <> 0 and (((CvSeq(set))^.flags and CV_MAGIC_MASK) = CV_SET_MAGIC_VAL)
  CV_SEQ_ELTYPE_BITS = 12;
{$EXTERNALSYM CV_SEQ_ELTYPE_BITS}
  CV_SEQ_ELTYPE_MASK = (1 shl CV_SEQ_ELTYPE_BITS) - 1;
{$EXTERNALSYM CV_SEQ_ELTYPE_MASK}
  CV_SEQ_ELTYPE_POINT: Integer = 0; // CV_32SC2; (* (x,y)*)
{$EXTERNALSYM CV_SEQ_ELTYPE_POINT}
  CV_SEQ_ELTYPE_CODE: Integer = 0; // CV_8UC1; (* freeman code: 0..7 *)
{$EXTERNALSYM CV_SEQ_ELTYPE_CODE}
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
  CV_SEQ_ELTYPE_POINT3D: Integer = 0; // CV_32FC3; (* (x,y,z) *)
{$EXTERNALSYM CV_SEQ_ELTYPE_POINT3D}
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
  CV_SEQ_POINT_SET: Integer = 0; // (CV_SEQ_KIND_GENERIC or CV_SEQ_ELTYPE_POINT);
{$EXTERNALSYM CV_SEQ_POINT_SET}
  CV_SEQ_POINT3D_SET: Integer = 0; // (CV_SEQ_KIND_GENERIC or CV_SEQ_ELTYPE_POINT3D);
{$EXTERNALSYM CV_SEQ_POINT3D_SET}
  CV_SEQ_POLYLINE: Integer = 0; // (CV_SEQ_KIND_CURVE or CV_SEQ_ELTYPE_POINT);
{$EXTERNALSYM CV_SEQ_POLYLINE}
  CV_SEQ_POLYGON: Integer = 0; // (CV_SEQ_FLAG_CLOSED or CV_SEQ_POLYLINE);
{$EXTERNALSYM CV_SEQ_POLYGON}
  CV_SEQ_CONTOUR: Integer = 0; // CV_SEQ_POLYGON;
{$EXTERNALSYM CV_SEQ_CONTOUR}
  CV_SEQ_SIMPLE_POLYGON: Integer = 0; // (CV_SEQ_FLAG_SIMPLE or CV_SEQ_POLYGON);
{$EXTERNALSYM CV_SEQ_SIMPLE_POLYGON}
  (* chain-coded curves *)
  CV_SEQ_CHAIN: Integer = 0; // (CV_SEQ_KIND_CURVE or CV_SEQ_ELTYPE_CODE);
{$EXTERNALSYM CV_SEQ_CHAIN}
  CV_SEQ_CHAIN_CONTOUR: Integer = 0; // (CV_SEQ_FLAG_CLOSED or CV_SEQ_CHAIN);
{$EXTERNALSYM CV_SEQ_CHAIN_CONTOUR}
  (* binary tree for the contour *)
  CV_SEQ_POLYGON_TREE = (CV_SEQ_KIND_BIN_TREE or CV_SEQ_ELTYPE_TRIAN_ATR);
{$EXTERNALSYM CV_SEQ_POLYGON_TREE}
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

  // CV_IS_SEQ_CLOSED(seq)(((seq)^.flags and CV_SEQ_FLAG_CLOSED) <> 0);
function CV_IS_SEQ_CLOSED(const Seq: pCvSeq): Boolean; {$IFDEF USE_INLINE}inline; {$ENDIF}
/// / >> Following declaration is a macro definition!
// const
// CV_IS_SEQ_CONVEX(seq)0;

// Following declaration is a macro definition!
// CV_IS_SEQ_HOLE(seq)(((seq)^.flags and CV_SEQ_FLAG_HOLE) <> 0);
function CV_IS_SEQ_HOLE(const Seq: pCvSeq): Boolean; {$IFDEF USE_INLINE}inline; {$ENDIF}
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

// ****************************************************************************************/
// *                            Sequence writer & reader                                  */
// ****************************************************************************************/
type
  pCvSeqWriter = ^TCvSeqWriter;

  TCvSeqWriter = record
    header_size: Integer;
    Seq: pCvSeq; // * the sequence written */
    block: pCvSeqBlock; // * current block */
    ptr: Pointer; // * pointer to free space */
    block_min: Pointer; // * pointer to the beginning of block*/
    block_max: Pointer; // * pointer to the end of block */
  end;

  pCvSeqReader = ^TCvSeqReader;

  TCvSeqReader = record
    header_size: Integer;
    Seq: pCvSeq; // * sequence, beign read */
    block: pCvSeqBlock; // * current block */
    ptr: Pointer; // * pointer to element be read next */
    block_min: Pointer; // * pointer to the beginning of block */
    block_max: Pointer; // * pointer to the end of block */
    delta_index: Integer; // * = seq->first->start_index   */
    prev_elem: Pointer; // * pointer to previous element */
  end;

  { ****************************************************************************************
    *                                Operations on sequences                               *
    **************************************************************************************** }
  {
    #define  CV_SEQ_ELEM( seq, elem_type, index )                    \
    /* assert gives some guarantee that <seq> parameter is valid */  \
    (   assert(sizeof((seq)->first[0]) == sizeof(CvSeqBlock) &&      \
    (seq)->elem_size == sizeof(elem_type)),                      \
    (elem_type*)((seq)->first && (unsigned)index <               \
    (unsigned)((seq)->first->count) ?                            \
    (seq)->first->data + (index) * sizeof(elem_type) :           \
    cvGetSeqElem( (CvSeq*)(seq), (index) )))
  }
{$IFDEF DELPHIXE_UP}

function CV_SEQ_ELEM(Seq: pCvSeq; const size_of_elem: Integer; index: Integer): Pointer; {$IFDEF USE_INLINE}inline;
{$ENDIF}
{ #define CV_GET_SEQ_ELEM( elem_type, seq, index ) CV_SEQ_ELEM( (seq), elem_type, (index) ) }
function CV_GET_SEQ_ELEM(const size_of_elem: Integer; Seq: pCvSeq; index: Integer): Pointer; {$IFDEF USE_INLINE}inline;
{$ENDIF}
{$ENDIF DELPHIXE_UP}
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

function CV_CAST_8U(t: Integer): uchar; {$IFDEF USE_INLINE}inline; {$ENDIF}
(*
  /* Move reader position forward: */
  #define CV_NEXT_SEQ_ELEM( elem_size, reader )                 \
  {                                                             \
  if( ((reader).ptr += (elem_size)) >= (reader).block_max ) \
  {                                                         \
  cvChangeSeqBlock( &(reader), 1 );                     \
  }                                                         \
  }
*)
procedure CV_NEXT_SEQ_ELEM(const elem_size: Integer; const Reader: TCvSeqReader);
// {$IFDEF USE_INLINE}inline;{$ENDIF}

// (* Move reader position backward: *)
// // >> Following declaration is a macro definition!
// const CV_PREV_SEQ_ELEM(elem_size, reader);
// begin if (((reader).ptr := mod -(elem_size)) < (reader).block_min)begin cvChangeSeqBlock(and
// (reader), -1) then; end; end;

(*
  /* Read element and move read position forward: */
  #define CV_READ_SEQ_ELEM( elem, reader )                       \
  {                                                              \
  assert( (reader).seq->elem_size == sizeof(elem));          \
  memcpy( &(elem), (reader).ptr, sizeof((elem)));            \
  CV_NEXT_SEQ_ELEM( sizeof(elem), reader )                   \
  }
*)
procedure CV_READ_SEQ_ELEM(const Elem: Pointer; const Reader: TCvSeqReader; const SizeOfElem: Integer);
// {$IFDEF USE_INLINE}inline;{$ENDIF}


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

{ ***************************************************************************************
  *             Data structures for persistence (a.k.a serialization) functionality     *
  *************************************************************************************** }

(* "black box" file storage *)
// type type CvFileStorage = leStorage;
type
  pCvFileStorage = ^TCvFileStorage;

  TCvFileStorage = record
  end;

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

  TCvAttrList = record
    attr: ppCVChar;
    (* NULL-terminated array of (attribute_name,attribute_value) pairs. *)
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

(*
  /* Basic element of the file storage - scalar or collection: */

  typedef struct CvFileNode
  {
  int tag;
  struct CvTypeInfo* info; /* type information
  (only for user-defined object, for others it is 0) */
  union
  {
  double f; /* scalar floating-point number */
  int i;    /* scalar integer number */
  CvString str; /* text string */
  CvSeq* seq; /* sequence (ordered collection of file nodes) */
  CvFileNodeHash* map; /* map (collection of named file nodes) */
  } data;
  }
  CvFileNode;
*)
Type

  pCvString = ^TCvString;

  TCvString = record
    len: Integer;
    ptr: pCVChar;
  end;

  pCvFileNode = ^TCvFileNode;
  pCvTypeInfo = ^TCvTypeInfo;

  pCvFileNodeHash = Pointer;

  TCvFileNode = record
    tag: Integer;
    info: pCvTypeInfo;
    case Integer of
      0:
        (f: Double); // * scalar floating-point number */
      1:
        (i: Integer); // * scalar integer number */
      2:
        (str: TCvString); // * text string */
      3:
        (Seq: pCvSeq); // * sequence (ordered collection of file nodes) */
      4:
        (map: pCvFileNodeHash); // * map (collection of named file nodes) */
  end;

  // typedef int (CV_CDECL *CvIsInstanceFunc)( const void* struct_ptr );
  TCvIsInstanceFunc = function(var struct_ptr: Pointer): Integer; cdecl;
  // typedef void (CV_CDECL *CvReleaseFunc)( void** struct_dblptr );
  TCvReleaseFunc = procedure(struct_dblptr: pPointer); cdecl;
  // typedef void* (CV_CDECL *CvReadFunc)( CvFileStorage* storage, CvFileNode* node );
  TCvReadFunc = function(storage: pCvFileStorage; node: pCvFileNode): Pointer; cdecl;
  // typedef void (CV_CDECL *CvWriteFunc)( CvFileStorage* storage, const char* name,const void* struct_ptr, CvAttrList attributes );
  TCvWriteFunc = procedure(storage: pCvFileStorage; const name: pCVChar; const struct_ptr: pPointer;
    attributes: TCvAttrList); cdecl;
  // typedef void* (CV_CDECL *CvCloneFunc)( const void* struct_ptr );
  TCvCloneFunc = function(const struct_ptr: pPointer): Pointer; cdecl;

  TCvTypeInfo = record
    flags: Integer;
    header_size: Integer;
    prev: pCvTypeInfo;
    next: pCvTypeInfo;
    type_name: pCVChar;
    is_instance: TCvIsInstanceFunc;
    release: TCvReleaseFunc;
    read: TCvReadFunc;
    write: TCvWriteFunc;
    clone: TCvCloneFunc;
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
  CV_NODE_FLOAT = longint(CV_NODE_REAL);
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
function CV_NODE_TYPE(const flags: Integer): Integer; {$IFDEF USE_INLINE}inline; {$ENDIF}

const
  (* file node flags *)
  CV_NODE_FLOW = 8; (* Used only for writing structures in YAML format. *)
{$EXTERNALSYM CV_NODE_FLOW}
  CV_NODE_USER = 16;
{$EXTERNALSYM CV_NODE_USER}
  CV_NODE_EMPTY = 32;
{$EXTERNALSYM CV_NODE_EMPTY}
  CV_NODE_NAMED = 64;
{$EXTERNALSYM CV_NODE_NAMED}
  // CV_NODE_IS_INT(flags)        (CV_NODE_TYPE(flags) == CV_NODE_INT)
function CV_NODE_IS_INT(const flags: Integer): Boolean; {$IFDEF USE_INLINE}inline; {$ENDIF}
// CV_NODE_IS_REAL(flags)       (CV_NODE_TYPE(flags) == CV_NODE_REAL)
function CV_NODE_IS_REAL(const flags: Integer): Boolean; {$IFDEF USE_INLINE}inline; {$ENDIF}
// const CV_NODE_IS_INT(flags)(CV_NODE_TYPE(flags) = CV_NODE_INT)
// const CV_NODE_IS_REAL(flags) (CV_NODE_TYPE(flags) = CV_NODE_REAL)
function CV_NODE_IS_STRING(const flags: Integer): Boolean; {$IFDEF USE_INLINE}inline; {$ENDIF}
// (CV_NODE_TYPE(flags) = CV_NODE_STRING)
// const CV_NODE_IS_SEQ(flags) (CV_NODE_TYPE(flags) = CV_NODE_SEQ)
// const CV_NODE_IS_MAP(flags) (CV_NODE_TYPE(flags) = CV_NODE_MAP)
// const CV_NODE_IS_COLLECTION (flags(CV_NODE_TYPE(flags) >= CV_NODE_SEQ)
// // >> Following declaration is a macro definition!
// const CV_NODE_IS_FLOW(flags)(((flags) and CV_NODE_FLOW) <> 0);
// // >> Following declaration is a macro definition!
// const CV_NODE_IS_EMPTY(flags)(((flags) and CV_NODE_EMPTY) <> 0);
// // >> Following declaration is a macro definition!
// const CV_NODE_IS_USER(flags)(((flags) and CV_NODE_USER) <> 0);
// // >> Following declaration is a macro definition!
// const CV_NODE_HAS_NAME(flags)(((flags) and CV_NODE_NAMED) <> 0);

const
  CV_NODE_SEQ_SIMPLE = 256;
{$EXTERNALSYM CV_NODE_SEQ_SIMPLE}
  //
  // // >> Following declaration is a macro definition!
  // const
  // CV_NODE_SEQ_IS_SIMPLE(seq(((seq)^.flags and CV_NODE_SEQ_SIMPLE) <> 0);

type

  CvString = record
    len: Integer;
    ptr: pCVChar;
  end;

  (* All the keys (names) of elements in the readed file storage
    are stored in the hash to speed up the lookup operations: *)
type
  (*
    typedef struct CvStringHashNode
    {
    unsigned hashval;
    CvString str;
    struct CvStringHashNode* next;
    } CvStringHashNode;
  *)

  pCvStringHashNode = ^TCvStringHashNode;

  TCvStringHashNode = record
    hashval: Cardinal;
    str: TCvString;
    next: pCvStringHashNode;
  end;

  //
  // type
  // CvStringHashNode * next = record
  // end;
  // end;
  // CvStringHashNode;
  //
  // type
  // type
  // CvFileNodeHash = nericHash;

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
  // CvFileStorage * storage, PCVChar name, Pointer type struct_ptr, CvAttrList attributes) = record) =
  // : ptr; end;
  // {$ENDIF}
  // type CvTypeInfo
  //
  // begin Integer flags; CvTypeInfo
  //
  // begin Integer flags; Integer header_size;
  //
  // type CvTypeInfo * prev = record end;
  //
  // type CvTypeInfo * next = record end; PCVChar type_name; CvIsInstanceFunc is_instance;
  // CvReleaseFunc release; CvReadFunc read; CvWriteFunc write; CvCloneFunc clone; end; CvTypeInfo;

Type
  (* *** System data types ***** *)
  (*
    typedef struct CvPluginFuncInfo
    {
    void** func_addr;
    void* default_func_addr;
    const char* func_names;
    int search_modules;
    int loaded_from;
    } CvPluginFuncInfo;
  *)

  PCvPluginFuncInfo = ^TCvPluginFuncInfo;

  TCvPluginFuncInfo = record
    func_addr: pPointer;
    default_func_addr: Pointer;
    func_names: pAnsiChar;
    search_modules: Integer;
    loaded_from: Integer;
  end;

  (*
    typedef struct CvModuleInfo
    {
    struct CvModuleInfo* next;
    const char* name;
    const char* version;
    CvPluginFuncInfo* func_tab;
    } CvModuleInfo;
  *)

  PCvModuleInfo = ^TCvModuleInfo;

  TCvModuleInfo = record
    next: PCvModuleInfo;
    name: pAnsiChar;
    version: pAnsiChar;
    func_tab: PCvPluginFuncInfo;
  end;

  // #define CV_SWAP(a,b,t) ((t) = (a), (a) = (b), (b) = (t))
procedure CV_SWAP(var a, b, t: pIplImage); {$IFDEF USE_INLINE}inline; {$ENDIF} overload;
procedure CV_SWAP(var a, b: pIplImage); {$IFDEF USE_INLINE}inline; {$ENDIF} overload;
procedure CV_SWAP(var a, b, t: pCvPoint2D32f); {$IFDEF USE_INLINE}inline; {$ENDIF} overload;
procedure CV_SWAP(var a, b: pCvPoint2D32f); {$IFDEF USE_INLINE}inline; {$ENDIF} overload;
procedure CV_SWAP(var a, b, t: pCvMat); {$IFDEF USE_INLINE}inline; {$ENDIF} overload;
procedure CV_SWAP(var a, b, t: Pointer); {$IFDEF USE_INLINE}inline; {$ENDIF} overload;
procedure CV_SWAP(var a, b: Pointer); {$IFDEF USE_INLINE}inline; {$ENDIF} overload;

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
// const a, b: Integer): Integer; {$IFDEF USE_INLINE}inline;{$ENDIF}
// { ((a) xor (((a) xor (b)) and (((a) < (b)) - 1))): INT; }
// // >> Following declaration is a macro definition!
// class function CV_IMAX(
//
// const a, b: Integer): Integer; {$IFDEF USE_INLINE}inline;{$ENDIF}
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
// const a, b: Integer): Integer; {$IFDEF USE_INLINE}inline;{$ENDIF}
// { (((a) > (b)) - ((a) < (b))); }
// // >> Following declaration is a macro definition!
// class function CV_SIGN(
//
// const a: Integer): Integer; {$IFDEF USE_INLINE}inline;{$ENDIF}
// { PCV_CMP((a), 0); }
// // CV_INLINE
// class function cvRound<T: packed record >(
//
// const v1: T): Integer; {$IFDEF USE_INLINE}inline;{$ENDIF}
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
// const img: pIplImage): Integer; {$IFDEF USE_INLINE}inline;{$ENDIF}
// { ((img) <> 0 and ((IplImage(img))^.nSize = SizeOf(IplImage)) }
// // >> Following declaration is a macro definition!
// class function CV_IS_IMAGE(
//
// const img: pIplImage): Integer; {$IFDEF USE_INLINE}inline;{$ENDIF}
// // (CV_IS_IMAGE_HDR(img) and ((IplImage)img)^.imageData <> 0);
// (* for storing double-precision
// floating point data in IplImage's */

// (* get reference to pixel at (col,row),
// for multi-channel images (col) should be multiplied by number of channels *)
// // >> Following declaration is a macro definition!
// // ageData + (image)^.widthStep * (row)) * (row)) = array)[0 .. (col) - 1] of;
// // {$EXTERNALSYM *(row))}row, col)(((elemtype(image)^.imageData + (image)^.widthStep);
// // ---------- Matrix cType (CvMat) ---------
// // >> Following declaration is a macro definition!
// class function CV_MAT_DEPTH(
//
// const flags: Integer): Integer; {$IFDEF USE_INLINE}inline;{$ENDIF}
// { ((flags) and CV_MAT_DEPTH_MASK); }
// // >> Following declaration is a macro definition!
// class function CV_MAKETYPE(
//
// const depth, cn: Integer): Integer; {$IFDEF USE_INLINE}inline;{$ENDIF}
// // (CV_MAT_DEPTH(depth) + (((cn) - 1) shl CV_CN_SHIFT));
// class function CV_MAKE_TYPE(
//
// const depth, cn: Integer): Integer; {$IFDEF USE_INLINE}inline;{$ENDIF}
// // (CV_MAT_DEPTH(depth) + (((cn) - 1) shl CV_CN_SHIFT));
// // >> Following declaration is a macro definition!
// class function CV_8UC(
//
// const n: Integer): Integer; {$IFDEF USE_INLINE}inline;{$ENDIF}
// // CV_MAKETYPE(CV_8U, (n));
// // >> Following declaration is a macro definition!
// class function CV_8SC(
//
// const n: Integer): Integer; {$IFDEF USE_INLINE}inline;{$ENDIF}
// // CV_MAKETYPE(CV_8S, (n));
// // >> Following declaration is a macro definition!
// class function CV_16UC(
//
// const n: Integer): Integer; {$IFDEF USE_INLINE}inline;{$ENDIF}
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

// #define CV_IS_MAT_HDR(mat) \
// ((mat) != NULL && \
// (((const CvMat*)(mat))->type & CV_MAGIC_MASK) == CV_MAT_MAGIC_VAL && \
// ((const CvMat*)(mat))->cols > 0 && ((const CvMat*)(mat))->rows > 0)
function CV_IS_MAT_HDR(const mat: Pointer): Boolean; inline;

// const CV_IS_MAT_HDR_Z(mat)((mat) <> 0 and (((CvMat(mat))^.cType and CV_MAGIC_MASK)
// = CV_MAT_MAGIC_VAL and ((CvMat(mat))^.cols >= 0 and ((CvMat(mat))^.rows >= 0)

// #define CV_IS_MAT(mat) \
// (CV_IS_MAT_HDR(mat) && ((const CvMat*)(mat))->data.ptr != NULL)
function CV_IS_MAT(const mat: Pointer): Boolean; inline;

// const CV_IS_MASK_ARR(mat)(((mat)^.cType and (CV_MAT_TYPE_MASK and ~ CV_8SC1)) = 0)

function CV_ARE_TYPES_EQ(const mat1, mat2: pCvMat): Boolean; {$IFDEF USE_INLINE}inline; {$ENDIF}
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

// CV_INLINE int cvIplDepth( int type )
// {
// int depth = CV_MAT_DEPTH(type);
// return CV_ELEM_SIZE1(depth)*8 | (depth == CV_8S || depth == CV_16S ||
// depth == CV_32S ? IPL_DEPTH_SIGN : 0);
// }
function cvIplDepth(_type: Integer): Integer; // {$IFDEF USE_INLINE}inline;{$ENDIF}

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

// (* ****************************** CvPoint and variants ********************************** *)

function CvPoint2D32f(x, y: Single): TCvPoint2D32f; {$IFDEF USE_INLINE}inline; {$ENDIF}
function cvPointTo32f(point: TCvPoint): TCvPoint2D32f; {$IFDEF USE_INLINE}inline; {$ENDIF}
function cvPointFrom32f(point: TCvPoint2D32f): TCvPoint; {$IFDEF USE_INLINE}inline; {$ENDIF}
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

// CV_INLINE  CvScalar  cvRealScalar( double val0 )
// {
// CvScalar scalar;
// scalar.val[0] = val0;
// scalar.val[1] = scalar.val[2] = scalar.val[3] = 0;
// return scalar;
// }

function cvRealScalar(val0: Double): TCvScalar; {$IFDEF USE_INLINE}inline; {$ENDIF}
(* ************************************************************************************** *)
(* Dynamic Data structures *)
(* ************************************************************************************** *)
(* ******************************* Memory storage *************************************** *)
(*
  const
  CV_IS_STORAGE(storage)((storage) <> 0 and (((CvMemStorage(storage))^.signature and CV_MAGIC_MASK)
  = CV_STORAGE_MAGIC_VAL)
*)

(* ********************************** Sequence ****************************************** *)

(* ************************************** Set ******************************************* *)
(*
  { Checks whether the element pointed by ptr belongs to a set or not }
  CV_IS_SET_ELEM(ptr)(((CvSetElem(ptr))^.flags >= 0)
*)

(* ********************************** CvTermCriteria ************************************ *)
(*
  CV_INLINE CvTermCriteria CvTermCriteria(Integer cType, Integer max_iter, Double epsilon)
*)
function CvTermCriteria(_type: Integer; max_iter: Integer; epsilon: Double): TCvTermCriteria; {$IFDEF USE_INLINE}inline;
{$ENDIF}
(*
  CV_INLINE  int  cvFloor( double value )
  {
  #if defined _MSC_VER && defined _M_X64 || (defined __GNUC__ && defined __SSE2__ && !defined __APPLE__)

  __m128d t = _mm_set_sd( value );
  int i = _mm_cvtsd_si32(t);
  return i - _mm_movemask_pd(_mm_cmplt_sd(t, _mm_cvtsi32_sd(t,i)));

  #elif defined __GNUC__

  int i = (int)value;
  return i - (i > value);

  #else

  int i = cvRound(value);
  Cv32suf diff;
  diff.f = (float)(value - i);
  return i - (diff.i < 0);

  #endif
  }
*)
function cvFloor(Value: Double): Integer; {$IFDEF USE_INLINE}inline; {$ENDIF}
function cvScalarAll(val0123: Double): TCvScalar; {$IFDEF USE_INLINE}inline; {$ENDIF}
function cvPoint(const x: Integer = 0; const y: Integer = 0): TCvPoint; {$IFDEF USE_INLINE}inline; {$ENDIF}
function CvPoint2f(const x: Single = 0; const y: Single = 0): TcvPoint2f; {$IFDEF USE_INLINE}inline; {$ENDIF}
function CvSize(const width, height: Integer): TCvSize; {$IFDEF USE_INLINE}inline; {$ENDIF}
function CvScalar(const val0: Double; const val1: Double = 0; const val2: Double = 0; const val3: Double = 0)
  : TCvScalar;
{$IFDEF USE_INLINE}inline; {$ENDIF}
function cvRandInt(Var rng: TCvRNG): Cardinal; {$IFDEF USE_INLINE}inline; {$ENDIF}
function CvRect(Const x, y, width, height: Integer): TCvRect; {$IFDEF USE_INLINE}inline; {$ENDIF}
function cvRound(Value: Double): Integer;

const
  cvZeroRect: TCvRect = (x: 0; y: 0; width: 0; height: 0);

  {
    * Inline constructor. No data is allocated internally!!!
    * (Use together with cvCreateData, or use cvCreateMat instead to
    * get a matrix with allocated data):
    *
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
function cvMat(const rows, cols: Integer; etype: Integer; data: Pointer = nil): TCvMat;
function CV_MAT_DEPTH(const flags: Integer): Integer;
function CV_MAT_TYPE(const flags: Integer): Integer;

(* Size of each channel item,

  0x124489 = 1000 0100 0100 0010 0010 0001 0001 ~ array of sizeof(arr_type_elem) */
  #define CV_ELEM_SIZE1(type) \
  ((((sizeof(size_t)<<28)|0x8442211) >> CV_MAT_DEPTH(type)*4) & 15)
*)
function CV_ELEM_SIZE1(const _type: Integer): Integer;
function CV_ELEM_SIZE(const _type: Integer): Integer;
function CV_MAT_CN(const flags: Integer): Integer;
function CV_32FC1: Integer; {$IFDEF USE_INLINE}inline; {$ENDIF}
function CV_32SC1: Integer; {$IFDEF USE_INLINE}inline; {$ENDIF}
function CV_MAKETYPE(depth, cn: Integer): Integer; {$IFDEF USE_INLINE}inline; {$ENDIF}
(*
  #define CV_MAT_ELEM( mat, elemtype, row, col )
  (*(elemtype* )CV_MAT_ELEM_PTR_FAST( mat, row, col, sizeof(elemtype)))
*)
function CV_MAT_ELEM(const mat: TCvMat; const elemsize: Integer; const row, col: Integer): Pointer;
{$IFDEF USE_INLINE}inline; {$ENDIF}
(*
  #define CV_MAT_ELEM_PTR_FAST( mat, row, col, pix_size )
  (assert( (unsigned)(row) < (unsigned)(mat).rows &&
  (unsigned)(col) < (unsigned)(mat).cols ),
  (mat).data.ptr + (size_t)(mat).step*(row) + (pix_size)*(col))
*)
function CV_MAT_ELEM_PTR_FAST(const mat: TCvMat; const row, col, pix_size: Integer): Pointer; {$IFDEF USE_INLINE}inline;
{$ENDIF}
function iif(const Conditional: Boolean; const ifTrue, ifFalse: Variant): Variant; {$IFDEF USE_INLINE}inline;
{$ENDIF} overload;
function iif(const Conditional: Boolean; const ifTrue, ifFalse: Pointer): Pointer; {$IFDEF USE_INLINE}inline;
{$ENDIF} overload;

function CvBox2D(const cX, cY, width, height, angle: Single): TCvBox2D;
function CvSize2D32f(const width, height: Single): TCvSize2D32f;

implementation

uses
  ocv.core_c,
{$IFDEF HAS_UNITSCOPE}
  System.SysUtils,
  System.AnsiStrings
{$ELSE}
  SysUtils,
  AnsiStrings
{$ENDIF}
    ;

function strdup(const str: pCVChar): pCVChar;
begin
  Result := AllocMem(Length(str) * SizeOf(CVChar));
  Move(str^, Result^, Length(str) * SizeOf(CVChar));
  // CopyMemory(Result, str, Length(str) * SizeOf(CVChar));
end;

function cv_stricmp(const str1, str2: pCVChar): Integer;
begin
  Result := {$IFDEF HAS_UNITSCOPE}System.{$ENDIF}SysUtils.AnsiStrComp(str1, str2);
end;

procedure strcpy(var str1: pCVChar; const str2: pCVChar);
Var
  n: Integer;
begin
  n := Length(str2) * SizeOf(CVChar);
  str1 := AllocMem(n);
  Move(str2^, str1^, n);
  // CopyMemory(str1, str2, n);
end;

procedure strcpync(const str1: pCVChar; const str2: pCVChar);
Var
  n: Integer;
begin
  n := Length(str2) * SizeOf(CVChar);
  Move(str2^, str1^, n);
  // CopyMemory(str1, str2, n);
end;

procedure strcat(var str1: pCVChar; const str2: pCVChar);
Var
  n: Integer;
begin
  n := Length(str1) * SizeOf(CVChar);
  ReallocMem(str1, (Length(str1) + Length(str2)) * SizeOf(CVChar));
  Move(str2^, pCVChar(str1 + n)^, Length(str2) * SizeOf(CVChar));
  // CopyMemory(str1 + n, str2, Length(str2) * SizeOf(CVChar));
end;

function CV_MAT_ELEM_PTR_FAST(const mat: TCvMat; const row, col, pix_size: Integer): Pointer;
begin
  Assert((row < mat.rows) and (col < mat.cols) and (row >= 0) and (col >= 0));
  Result := Pointer(Integer(mat.data.ptr) + mat.step * row + pix_size * col);
end;

function CV_MAT_ELEM(const mat: TCvMat; const elemsize: Integer; const row, col: Integer): Pointer;
begin
  Result := CV_MAT_ELEM_PTR_FAST(mat, row, col, elemsize);
end;

function CvAttrList(const attr: ppCVChar = nil; next: pCvAttrList = nil): TCvAttrList;
begin
  Result.attr := attr;
  Result.next := next;
end;

function CV_MAT_DEPTH;
begin
  Result := flags and CV_MAT_DEPTH_MASK;
end;

function CV_MAT_TYPE;
begin
  Result := flags and CV_MAT_TYPE_MASK;
end;

function CV_MAT_CN;
begin
  Result := ((((flags) and CV_MAT_CN_MASK) shr CV_CN_SHIFT) + 1);
end;

function CV_ELEM_SIZE;
begin
  Result := (CV_MAT_CN(_type) shl ((((SizeOf(NativeInt) div 4 + 1) * (16384 or $3A50)) shr CV_MAT_DEPTH(_type) *
    2) and 3));
end;

function CV_32SC1: Integer;
begin
  Result := CV_MAKETYPE(CV_32S, 1);
end;

function CV_32FC1: Integer;
begin
  Result := CV_MAKETYPE(CV_32F, 1);
end;

function CV_MAKETYPE(depth, cn: Integer): Integer;
begin
  Result := (CV_MAT_DEPTH(depth) + (((cn) - 1) shl CV_CN_SHIFT));
end;

function cvMat;
begin
  if not(CV_MAT_DEPTH(etype) <= CV_64F) then
    exit;
  etype := CV_MAT_TYPE(etype);
  Result._type := CV_MAT_MAGIC_VAL or CV_MAT_CONT_FLAG or etype;
  Result.cols := cols;
  Result.rows := rows;
  Result.step := Result.cols * CV_ELEM_SIZE(etype);
  Result.data.ptr := data;
  Result.refcount := nil;
  Result.hdr_refcount := 0;
end;

function cvScalarAll;
begin
  Result.val[0] := val0123;
  Result.val[1] := val0123;
  Result.val[2] := val0123;
  Result.val[3] := val0123;
end;

function cvPoint;
begin
  Result.x := x;
  Result.y := y;
end;

function CvPoint2f;
begin
  Result.x := x;
  Result.y := y;
end;

function CvScalar;
begin
  Result.val[0] := val0;
  Result.val[1] := val1;
  Result.val[2] := val2;
  Result.val[3] := val3;
end;

function CvSize;
begin
  Result.width := width;
  Result.height := height;
end;

function CvRect;
begin
  Result.x := x;
  Result.y := y;
  Result.width := width;
  Result.height := height;
end;

function cvRandInt;
begin
{$Q-}
  rng := TCvRNG(rng * CV_RNG_COEFF + (rng shr 32));
{$Q+}
  Result := Cardinal(rng);
end;

function cvRound;
begin
  Result := Round(Value);
end;

function iif(const Conditional: Boolean; const ifTrue, ifFalse: Variant): Variant; {$IFDEF USE_INLINE}inline;
{$ENDIF} overload;
begin
  if Conditional then
    Result := ifTrue
  else
    Result := ifFalse;
end;

function iif(const Conditional: Boolean; const ifTrue, ifFalse: Pointer): Pointer; {$IFDEF USE_INLINE}inline;
{$ENDIF} overload;
begin
  if Conditional then
    Result := ifTrue
  else
    Result := ifFalse;
end;

function CV_NODE_TYPE;
begin
  // CV_NODE_TYPE(flags)((flags) and CV_NODE_TYPE_MASK);
  Result := flags and CV_NODE_TYPE_MASK;
end;

function CV_NODE_IS_INT;
begin
  // CV_NODE_IS_INT(flags) (CV_NODE_TYPE(flags) == CV_NODE_INT)
  Result := CV_NODE_TYPE(flags) = CV_NODE_INT;
end;

function CV_NODE_IS_REAL;
begin
  // CV_NODE_IS_REAL(flags) (CV_NODE_TYPE(flags) == CV_NODE_REAL)
  Result := CV_NODE_TYPE(flags) = CV_NODE_REAL;
end;

function CV_NODE_IS_STRING(const flags: Integer): Boolean; {$IFDEF USE_INLINE}inline; {$ENDIF}
begin
  Result := CV_NODE_TYPE(flags) = CV_NODE_STRING
end;

procedure CV_READ_SEQ_ELEM(const Elem: Pointer; const Reader: TCvSeqReader; const SizeOfElem: Integer);
begin
  // assert( (reader).seq->elem_size == sizeof(elem));
  Assert(Reader.Seq^.elem_size = SizeOfElem);
  // memcpy( &(elem), (reader).ptr, sizeof((elem)));
  Move(Reader.ptr^, Elem^, SizeOfElem);
  // CopyMemory(Elem, Reader.ptr, SizeOfElem);
  // CV_NEXT_SEQ_ELEM( sizeof(elem), reader )
  CV_NEXT_SEQ_ELEM(SizeOfElem, Reader);
end;

procedure CV_NEXT_SEQ_ELEM(const elem_size: Integer; const Reader: TCvSeqReader);
Var
  ptr: PInteger;
begin
  // if( ((reader).ptr += (elem_size)) >= (reader).block_max )
  // cvChangeSeqBlock( &(reader), 1 );
  ptr := @Reader.ptr;
  ptr^ := ptr^ + elem_size;
  if Integer(Reader.ptr) >= Integer(Reader.block_max) then
    cvChangeSeqBlock(@Reader, 1);
end;

function cvFloor;
Var
  diff: TCv32suf;
  i: Integer;
begin
  i := cvRound(Value);
  diff.f := (Value - i);
  if diff.i < 0 then
    Dec(i);
  Result := i;
end;

function cvPointFrom32f;
begin
  Result.x := cvRound(point.x);
  Result.y := cvRound(point.y);
end;

function CvTermCriteria;
begin
  Result.cType := _type;
  Result.max_iter := max_iter;
  Result.epsilon := epsilon;
end;

function cvPointTo32f;
begin
  Result := CvPoint2D32f(point.x, point.y);
end;

function CvPoint2D32f;
begin
  Result.x := x;
  Result.y := y;
end;

function cvPoint3D32f(const x, y, z: Single): TCvPoint3D32f; {$IFDEF USE_INLINE}inline; {$ENDIF}
begin
  Result.x := x;
  Result.y := y;
  Result.z := z;
end;

procedure CV_SWAP(var a, b, t: Pointer);
begin
  t := a;
  a := b;
  b := t;
end;

procedure CV_SWAP(var a, b: Pointer);
Var
  t: Pointer;
begin
  CV_SWAP(a, b, t);
end;

procedure CV_SWAP(var a, b, t: pCvMat);
begin
  CV_SWAP(Pointer(a), Pointer(b), Pointer(t));
end;

procedure CV_SWAP(var a, b, t: pIplImage);
begin
  CV_SWAP(Pointer(a), Pointer(b), Pointer(t));
end;

procedure CV_SWAP(var a, b: pIplImage);
begin
  CV_SWAP(Pointer(a), Pointer(b));
end;

procedure CV_SWAP(var a, b, t: pCvPoint2D32f);
begin
  CV_SWAP(Pointer(a), Pointer(b), Pointer(t));
end;

procedure CV_SWAP(var a, b: pCvPoint2D32f);
begin
  CV_SWAP(Pointer(a), Pointer(b));
end;

function CV_32SC2;
begin
  Result := CV_MAKETYPE(CV_32S, 2);
end;

function CV_CAST_8U(t: Integer): uchar;
begin
  if (not(t and (not 255)) <> 0) then
    Result := t
  else if t > 0 then
    Result := 255
  else
    Result := 0;
end;

{$IFDEF DELPHIXE_UP}

function CV_GET_SEQ_ELEM;
begin
  { #define CV_GET_SEQ_ELEM( elem_type, seq, index ) CV_SEQ_ELEM( (seq), elem_type, (index) ) }
  Result := CV_SEQ_ELEM(Seq, size_of_elem, index);
end;

function CV_SEQ_ELEM(Seq: pCvSeq; const size_of_elem: Integer; index: Integer): Pointer; {$IFDEF USE_INLINE}inline;
{$ENDIF}
begin
  // assert(sizeof((seq)->first[0]) == sizeof(CvSeqBlock) && (seq)->elem_size == sizeof(elem_type))
  Assert(Assigned(Seq^.first) and (SizeOf(Seq^.first[0]) = SizeOf(TCvSeqBlock)) and (Seq^.elem_size = size_of_elem));
  // (elem_type*)((seq)->first && (unsigned)index <(unsigned)((seq)->first->count) ?
  if Assigned(Seq^.first) and (Cardinal(index) < Cardinal(Seq^.first^.count)) then
    // (seq)->first->data + (index) * sizeof(elem_type) :
    Result := Pointer(Integer(Seq^.first^.data) + index * size_of_elem)
  else
    // cvGetSeqElem( (CvSeq*)(seq), (index) )))
    Result := cvGetSeqElem(Seq, index);
end;
{$ENDIF}

function CV_8UC1: Integer; {$IFDEF USE_INLINE}inline; {$ENDIF}
begin
  Result := CV_MAKETYPE(CV_8U, 1);
end;

function CV_32FC2: Integer;
begin
  Result := CV_MAKETYPE(CV_32F, 2);
end;

function CV_32FC3: Integer; {$IFDEF USE_INLINE}inline; {$ENDIF}
begin
  Result := CV_MAKETYPE(CV_32F, 3);
end;

function CV_64FC1: Integer;
begin
  Result := CV_MAKETYPE(CV_64F, 1);
end;

function CV_64FC2: Integer;
begin
  Result := CV_MAKETYPE(CV_64F, 2);
end;

function CV_64FC3: Integer; {$IFDEF USE_INLINE}inline; {$ENDIF}
begin
  Result := CV_MAKETYPE(CV_64F, 3);
end;

function CV_8UC2: Integer;
begin
  Result := CV_MAKETYPE(CV_8U, 2);
end;

function CV_8UC3: Integer;
begin
  Result := CV_MAKETYPE(CV_8U, 3);
end;

function CV_IS_SET_ELEM(ptr: Pointer): Boolean;
begin
  // #define CV_IS_SET_ELEM( ptr )  (((CvSetElem*)(ptr))->flags >= 0)
  Result := Assigned(ptr) and (pCvSetElem(ptr)^.flags >= 0);
end;

function CV_IMAGE_ELEM(image: pIplImage; size_elemtype, row, col: Integer): Pointer; {$IFDEF USE_INLINE}inline; {$ENDIF}
begin
  // (((elemtype*)((image)->imageData + (image)->widthStep*(row)))[(col)])
  Result := {$IFDEF DELPHI7}Pointer({$ENDIF DELPHI7}{$IFDEF DELPHI2005_UP}PByte{$ELSE}Integer{$ENDIF}(image^.imageData)
    + image^.widthStep * row + col * size_elemtype{$IFDEF DELPHI7}){$ENDIF DELPHI7};
end;

function cvRealScalar(val0: Double): TCvScalar; {$IFDEF USE_INLINE}inline; {$ENDIF}
begin
  Result.val[0] := val0;
  Result.val[1] := 0;
  Result.val[2] := 0;
  Result.val[3] := 0;
end;

function cvRNG(seed: int64 = -1): TCvRNG; {$IFDEF USE_INLINE}inline; {$ENDIF}
begin
  // CvRNG rng = seed ? (uint64)seed : (uint64)(int64)-1;
  Result := iif(seed > 0, seed, UInt64(int64(-1)));
end;

function CV_ELEM_SIZE1(const _type: Integer): Integer;
begin
  // * Size of each channel item,
  // 0x124489 = 1000 0100 0100 0010 0010 0001 0001 ~ array of sizeof(arr_type_elem) */
  // #define CV_ELEM_SIZE1(type) \
  // ((((sizeof(size_t)<<28)|0x8442211) >> CV_MAT_DEPTH(type)*4) & 15)
  Result := ((((int64(SizeOf(NativeUInt)) shl 28) or $8442211) shr (CV_MAT_DEPTH(_type) * 4)) and 15);
end;

function cvIplDepth(_type: Integer): Integer;
Var
  depth: Integer;
begin
  // return CV_ELEM_SIZE1(depth)*8 | (depth == CV_8S || depth == CV_16S || depth == CV_32S ? IPL_DEPTH_SIGN : 0);
  depth := CV_MAT_DEPTH(_type);
  Result := CV_ELEM_SIZE1(depth) * 8;
  if (depth = CV_8S) or (depth = CV_16S) or (depth = CV_32S) then
    Result := Result or Integer(IPL_DEPTH_SIGN);
end;

function CV_ARE_TYPES_EQ(const mat1, mat2: pCvMat): Boolean; {$IFDEF USE_INLINE}inline; {$ENDIF}
begin
  Result := ((((mat1)^._type xor (mat2)^._type) and CV_MAT_TYPE_MASK) = 0);
end;

function CV_IS_SEQ_CLOSED(const Seq: pCvSeq): Boolean; {$IFDEF USE_INLINE}inline; {$ENDIF}
begin
  Result := (Seq^.flags and CV_SEQ_FLAG_CLOSED) <> 0;
end;

function CV_IS_SEQ_HOLE(const Seq: pCvSeq): Boolean; {$IFDEF USE_INLINE}inline; {$ENDIF}
begin
  Result := (Seq^.flags and CV_SEQ_FLAG_HOLE) <> 0;
end;

function CvBox2D(const cX, cY, width, height, angle: Single): TCvBox2D;
begin
  Result.center := CvPoint2D32f(cX, cY);
  Result.size := CvSize2D32f(width, height);
  Result.angle := angle;
end;

function CvSize2D32f(const width, height: Single): TCvSize2D32f;
begin
  Result.width := width;
  Result.height := height;
end;

{ TCvPoint2D32f }
{$IFDEF DELPHIXE2_UP}

function TCvPoint2D32f.cvPoint: TCvPoint;
begin
  Result.x := Trunc(x);
  Result.y := Trunc(y);
end;

function TCvPoint2D32f.Norm: Single;
begin
  Result := sqrt(x * x + y * y);
end;

class operator TCvPoint2D32f.Subtract(a, b: TCvPoint2D32f): TCvPoint2D32f;
begin
  Result.x := a.x - b.x;
  Result.y := a.y - b.y;
end;
{$ENDIF}
{ TCvScalar }

class operator TCvScalar.GreaterThan(a, b: TCvScalar): Boolean;
begin
  Result := (a.val[0] > b.val[0]) and (a.val[1] > b.val[1]) and (a.val[2] > b.val[2]) and (a.val[3] >= b.val[3]);
end;

class operator TCvScalar.LessThan(a, b: TCvScalar): Boolean;
begin
  Result := (a.val[0] < b.val[0]) and (a.val[1] < b.val[1]) and (a.val[2] < b.val[2]) and (a.val[3] <= b.val[3]);
end;

{ TCvMat }

function TCvMat.AsByte: PByte;
begin
  Result := data.ptr;
end;

function TCvMat.AsDouble: pDouble;
begin
  Result := data.db;
end;

function TCvMat.AsInteger: PInteger;
begin
  Result := data.i;
end;

function TCvMat.AsSingle: pSingle;
begin
  Result := data.fl;
end;

function TCvMat.GetMatByte(const row, col: Integer): Byte;
begin
  Result := PByte(CV_MAT_ELEM(Self, SizeOf(Byte), row, col))^;
end;

function TCvMat.GetMatDouble(const row, col: Integer): Double;
begin
  Result := pDouble(CV_MAT_ELEM(Self, SizeOf(Double), row, col))^;
end;

function TCvMat.GetMatInteger(const row, col: Integer): Integer;
begin
  Result := PInteger(CV_MAT_ELEM(Self, SizeOf(Integer), row, col))^;
end;

function TCvMat.GetMatSingle(const row, col: Integer): Single;
begin
  Result := pSingle(CV_MAT_ELEM(Self, SizeOf(Single), row, col))^;
end;

procedure TCvMat.SetMatByte(const row, col: Integer; const Value: Byte);
begin
  PByte(CV_MAT_ELEM(Self, SizeOf(Byte), row, col))^ := Value;
end;

procedure TCvMat.SetMatDouble(const row, col: Integer; const Value: Double);
begin
  pDouble(CV_MAT_ELEM(Self, SizeOf(Double), row, col))^ := Value;
end;

procedure TCvMat.SetMatInteger(const row, col, Value: Integer);
begin
  PInteger(CV_MAT_ELEM(Self, SizeOf(Integer), row, col))^ := Value;
end;

procedure TCvMat.SetMatSingle(const row, col: Integer; const Value: Single);
begin
  pSingle(CV_MAT_ELEM(Self, SizeOf(Single), row, col))^ := Value;
end;

function CV_IS_MATND_HDR(const mat: Pointer): Boolean; inline;
begin
  // #define CV_IS_MATND_HDR(mat) \
  // ((mat) != NULL && (((const CvMatND*) (mat)) - > type & CV_MAGIC_MASK) = = CV_MATND_MAGIC_VAL)
  Result := Assigned(mat) and ((pCvMatND(mat)^._type and CV_MAGIC_MASK) = CV_MATND_MAGIC_VAL);
end;

function CV_IS_MATND(const mat: Pointer): Boolean; inline;
begin
  // #define CV_IS_MATND(mat) \
  // (CV_IS_MATND_HDR(mat) && ((const CvMatND*)(mat))->data.ptr != NULL)
  Result := CV_IS_MATND_HDR(mat) and Assigned(pCvMatND(mat).data.ptr);
end;

function CV_IS_MAT(const mat: Pointer): Boolean; inline;
begin
  // #define CV_IS_MAT(mat) \
  // (CV_IS_MAT_HDR(mat) && ((const CvMat*)(mat))->data.ptr != NULL)
  Result := CV_IS_MAT_HDR(mat) and Assigned(pCvMat(mat)^.data.ptr);
end;

function CV_IS_MAT_HDR(const mat: Pointer): Boolean; inline;
begin
  // #define CV_IS_MAT_HDR(mat) \
  // ((mat) != NULL && \
  // (((const CvMat*)(mat))->type & CV_MAGIC_MASK) == CV_MAT_MAGIC_VAL && \
  // ((const CvMat*)(mat))->cols > 0 && ((const CvMat*)(mat))->rows > 0)
  Result := Assigned(mat) and //
    ((pCvMat(mat)^._type and CV_MAGIC_MASK) = CV_MAT_MAGIC_VAL) and //
    (pCvMat(mat)^.cols > 0) and //
    (pCvMat(mat)^.rows > 0);
end;

initialization

CV_SEQ_ELTYPE_POINT := CV_32SC2;
CV_SEQ_ELTYPE_CODE := CV_8UC1;
CV_SEQ_ELTYPE_POINT3D := CV_32FC3;
CV_SEQ_POINT_SET := (CV_SEQ_KIND_GENERIC or CV_SEQ_ELTYPE_POINT);
CV_SEQ_POINT3D_SET := (CV_SEQ_KIND_GENERIC or CV_SEQ_ELTYPE_POINT3D);
CV_SEQ_POLYLINE := (CV_SEQ_KIND_CURVE or CV_SEQ_ELTYPE_POINT);
CV_SEQ_POLYGON := (CV_SEQ_FLAG_CLOSED or CV_SEQ_POLYLINE);
CV_SEQ_CONTOUR := CV_SEQ_POLYGON;
CV_SEQ_SIMPLE_POLYGON := (CV_SEQ_FLAG_SIMPLE or CV_SEQ_POLYGON);
CV_SEQ_CHAIN := (CV_SEQ_KIND_CURVE or CV_SEQ_ELTYPE_CODE);
CV_SEQ_CHAIN_CONTOUR := (CV_SEQ_FLAG_CLOSED or CV_SEQ_CHAIN);

end.
