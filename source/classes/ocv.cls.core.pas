(*
  *****************************************************************
  Delphi-OpenCV
  Copyright (C) 2013 Project Delphi-OpenCV
  ****************************************************************
  Contributor:
  Laentir Valetov
  email:laex@bk.ru
  ****************************************************************
  You may retrieve the latest version of this file at the GitHub,
  located at git://github.com/Laex/Delphi-OpenCV.git
  ****************************************************************
  The contents of this file are used with permission, subject to
  the Mozilla Public License Version 1.1 (the "License"); you may
  not use this file except in compliance with the License. You may
  obtain a copy of the License at
  http://www.mozilla.org/MPL/MPL-1_1Final.html

  Software distributed under the License is distributed on an
  "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
  implied. See the License for the specific language governing
  rights and limitations under the License.
  *******************************************************************
*)

unit ocv.cls.core;

{$I OpenCV.inc}

interface

Uses
  ocv.core.types_c,
  ocv.cls.types;

Type
  ISize = interface(IOCVCommon)
    ['{33DA805D-9B45-4EF9-A657-C53DB60CDE9A}']
  end;

  TSize = class(TOCVCommon, ISize)
  public
    constructor Create; overload;
    constructor Create(const sz: TCvSize); overload;
    destructor Destroy; override;
  end;

  IRect2i = interface(IOCVCommon)
    ['{417FF1CA-D2A6-46BC-8FBA-84EF5642CE63}']
  end;

  TRect2i = class(TOCVCommon, IRect2i)

  end;

  IRect = IRect2i;
  TVectorRect = TArray<IRect>;
  TVectorInt = TArray<integer>;
  TVectorDouble = TArray<double>;

  (* !
    When the break-on-error mode is set, the default error handler
    issues a hardware exception, which can make debugging more convenient.

    \return the previous state
  *)
  // CV_EXPORTS bool setBreakOnError(bool flag);

{$IFDEF SAFELOADLIB}

type
  TsetBreakOnError = function(flag: cbool): cbool; cdecl;

var
  setBreakOnError: TsetBreakOnError;
{$ELSE}
function setBreakOnError(flag: cbool): cbool; cdecl;
{$ENDIF}

(*
  typedef int (CV_CDECL *ErrorCallback)( int status, const char* func_name,
  const char* err_msg, const char* file_name,
  int line, void* userdata );
*)
type
  TErrorCallback = function(status: integer; const func_name: pAnsiChar; const err_msg: pAnsiChar; const file_name: pAnsiChar;
    line: integer; userdata: pointer): integer; cdecl;
  // ! Sets the new error handler and the optional user data.

  (* !
    The function sets the new error handler, called from cv::error().

    \param errCallback the new error handler. If NULL, the default error handler is used.
    \param userdata the optional user data pointer, passed to the callback.
    \param prevUserdata the optional output parameter where the previous user data pointer is stored

    \return the previous error handler
  *)
  // CV_EXPORTS ErrorCallback redirectError( ErrorCallback errCallback, void* userdata=0, void** prevUserdata=0);

{$IFDEF SAFELOADLIB}

type
  TRedirectError = function(errCallback: TErrorCallback; userdata: pointer = nil; prevUserdata: PPointer = nil): TErrorCallback; cdecl;

var
  redirectError: TRedirectError;
{$ELSE}
function redirectError(errCallback: TErrorCallback; userdata: pointer = nil; prevUserdata: PPointer = nil): TErrorCallback; cdecl;
{$ENDIF}
// CV_EXPORTS void glob(String pattern, std::vector<String>& result, bool recursive = false);

// CV_EXPORTS_W void setNumThreads(int nthreads);
{$IFDEF SAFELOADLIB}

type
  TsetNumThreads = procedure(nthreads: integer); cdecl;

var
  setNumThreads: TsetNumThreads;
{$ELSE}
procedure setNumThreads(nthreads: integer); cdecl;
{$ENDIF}
// CV_EXPORTS_W int getNumThreads();
{$IFDEF SAFELOADLIB}

type
  TgetNumThreads = function(): integer; cdecl;

var
  getNumThreads: TgetNumThreads;
{$ELSE}
function getNumThreads(): integer; cdecl;
{$ENDIF}
// CV_EXPORTS_W int getThreadNum();
{$IFDEF SAFELOADLIB}

type
  TgetThreadNum = function: integer; cdecl;

var
  getThreadNum: TgetThreadNum;
{$ELSE}
function getThreadNum(): integer; cdecl;
{$ENDIF}
// CV_EXPORTS_W const string& getBuildInformation();

// ! Returns the number of ticks.

(* !
  The function returns the number of ticks since the certain event (e.g. when the machine was turned on).
  It can be used to initialize cv::RNG or to measure a function execution time by reading the tick count
  before and after the function call. The granularity of ticks depends on the hardware and OS used. Use
  cv::getTickFrequency() to convert ticks to seconds.
*)
// CV_EXPORTS_W int64 getTickCount();
{$IFDEF SAFELOADLIB}

type
  TgetTickCount = function: int64; cdecl;

var
  getTickCount: TgetTickCount;
{$ELSE}
function getTickCount(): int64; cdecl;
{$ENDIF}
(* !
  Returns the number of ticks per seconds.

  The function returns the number of ticks (as returned by cv::getTickCount()) per second.
  The following code computes the execution time in milliseconds:

  \code
  double exec_time = (double)getTickCount();
  // do something ...
  exec_time = ((double)getTickCount() - exec_time)*1000./getTickFrequency();
  \endcode
*)
// CV_EXPORTS_W double getTickFrequency();
{$IFDEF SAFELOADLIB}

type
  TgetTickFrequency = function: double; cdecl;

var
  getTickFrequency: TgetTickFrequency;
{$ELSE}
function getTickFrequency(): double; cdecl;
{$ENDIF}
(* !
  Returns the number of CPU ticks.

  On platforms where the feature is available, the function returns the number of CPU ticks
  since the certain event (normally, the system power-on moment). Using this function
  one can accurately measure the execution time of very small code fragments,
  for which cv::getTickCount() granularity is not enough.
*)
// CV_EXPORTS_W int64 getCPUTickCount();
{$IFDEF SAFELOADLIB}

type
  TgetCPUTickCount = function: int64; cdecl;

var
  getCPUTickCount: TgetCPUTickCount;
{$ELSE}
function getCPUTickCount(): int64; cdecl;
{$ENDIF}
(* !
  Returns SSE etc. support status

  The function returns true if certain hardware features are available.
  Currently, the following features are recognized:
  - CV_CPU_MMX - MMX
  - CV_CPU_SSE - SSE
  - CV_CPU_SSE2 - SSE 2
  - CV_CPU_SSE3 - SSE 3
  - CV_CPU_SSSE3 - SSSE 3
  - CV_CPU_SSE4_1 - SSE 4.1
  - CV_CPU_SSE4_2 - SSE 4.2
  - CV_CPU_POPCNT - POPCOUNT
  - CV_CPU_AVX - AVX
  - CV_CPU_AVX2 - AVX2

  \note {Note that the function output is not static. Once you called cv::useOptimized(false),
  most of the hardware acceleration is disabled and thus the function will returns false,
  until you call cv::useOptimized(true)}
*)
// CV_EXPORTS_W bool checkHardwareSupport(int feature);
{$IFDEF SAFELOADLIB}

type
  TcheckHardwareSupport = function(feature: integer): cbool; cdecl;

var
  checkHardwareSupport: TcheckHardwareSupport;
{$ELSE}
function checkHardwareSupport(feature: integer): cbool; cdecl;
{$ENDIF}
// ! returns the number of CPUs (including hyper-threading)
// CV_EXPORTS_W int getNumberOfCPUs();
{$IFDEF SAFELOADLIB}

type
  TgetNumberOfCPUs = function: integer; cdecl;

var
  getNumberOfCPUs: TgetNumberOfCPUs;
{$ELSE}
function getNumberOfCPUs(): integer; cdecl;
{$ENDIF}
(* !
  Allocates memory buffer

  This is specialized OpenCV memory allocation function that returns properly aligned memory buffers.
  The usage is identical to malloc(). The allocated buffers must be freed with cv::fastFree().
  If there is not enough memory, the function calls cv::error(), which raises an exception.

  \param bufSize buffer size in bytes
  \return the allocated memory buffer.
*)
// CV_EXPORTS void*  fastMalloc(size_t bufSize);
{$IFDEF SAFELOADLIB}

type
  TfastMalloc = function(bufSize: size_t): pointer; cdecl;

var
  fastMalloc: TfastMalloc;
{$ELSE}
function fastMalloc(bufSize: size_t): pointer; cdecl;
{$ENDIF}
(* !
  Frees the memory allocated with cv::fastMalloc

  This is the corresponding deallocation function for cv::fastMalloc().
  When ptr==NULL, the function has no effect.
*)
// CV_EXPORTS void fastFree(void * ptr);
{$IFDEF SAFELOADLIB}

type
  TfastFree = procedure(ptr: pointer); cdecl;

var
  fastFree: TfastFree;
{$ELSE}
procedure fastFree(ptr: pointer); cdecl;
{$ENDIF}
(* !
  Turns on/off available optimization

  The function turns on or off the optimized code in OpenCV. Some optimization can not be enabled
  or disabled, but, for example, most of SSE code in OpenCV can be temporarily turned on or off this way.

  \note{Since optimization may imply using special data structures, it may be unsafe
  to call this function anywhere in the code. Instead, call it somewhere at the top level.}
*)
// CV_EXPORTS_W void setUseOptimized(bool onoff);
{$IFDEF SAFELOADLIB}

type
  TsetUseOptimized = procedure(onoff: cbool); cdecl;

var
  setUseOptimized: TsetUseOptimized;
{$ELSE}
procedure setUseOptimized(onoff: cbool); cdecl;
{$ENDIF}
(* !
  Returns the current optimization status

  The function returns the current optimization status, which is controlled by cv::setUseOptimized().
*)
// CV_EXPORTS_W bool useOptimized();
{$IFDEF SAFELOADLIB}

type
  TuseOptimized = function: cbool; cdecl;

var
  useOptimized: TuseOptimized;
{$ELSE}
function useOptimized(): cbool; cdecl;
{$ENDIF}

Type

  IMat = interface(IOCVCommon)
    ['{2CFB1B8E-4D18-4C1D-839F-0AFE4213F57D}']
    function elemSize(): size_t; // 0
    function elemSize1(): size_t; // 1
    function _type(): integer; // 2
    function depth(): integer; // 3
    function channels(): integer; // 4
    function step1(i: integer = 0): size_t; // 5
    function empty(): cbool;
    function total(): size_t; // 6
    function flags(): integer; // 7
    function dims(): integer; // 8
    function rows(): integer; // 9
    function cols(): integer; // 10
    function data(): pointer; // 11
  end;

  TMat = class(TOCVCommon, IMat)
  private
    FNeedDestroy: boolean;
  public
    constructor Create(const _M: TOpenCVClass = nil; const NeedDestroy: boolean = True); overload;
    constructor Create(const Image: pIplImage); overload;
    destructor Destroy; override;
    function elemSize(): size_t; // 0
    function elemSize1(): size_t; // 1
    function _type(): integer; // 2
    function depth(): integer; // 3
    function channels(): integer; // 4
    function step1(i: integer = 0): size_t; // 5
    function empty(): cbool;
    function total(): size_t; // 6
    function flags(): integer; // 7
    function dims(): integer; // 8
    function rows(): integer; // 9
    function cols(): integer; // 10
    function data(): pointer; // 11
  end;

  TArrayOfTMat = TArray<TMat>;
  TArrayOfIMat = TArray<IMat>;

  TIplImageRecordHelper = record helper for TIplImage
    function InitFromMat(const Mat: IMat): TIplImage;
  end;

implementation

Uses
  ocv.core_c,
  ocv.lib;

// ------------------------------ Mat ------------------------------
function _CreateMat: TOpenCVClass; stdcall; external opencv_classes_lib name '_CreateMat@0';
function _GetMatData(const e: TOpenCVClass; index: integer; param: integer = 0): integer; stdcall;
  external opencv_classes_lib name '_GetMatData@12';
function _MatEmpty(const e: TOpenCVClass): cbool; stdcall; external opencv_classes_lib name '_MatEmpty@4';
function _CreateMatFromImage(const Image: pIplImage): TOpenCVClass; stdcall; external opencv_classes_lib name '_CreateMatFromImage@4';
procedure _DestroyMat(const M: TOpenCVClass); stdcall; external opencv_classes_lib name '_DestroyMat@4';

{ ------------------------------ TMat ------------------------------ }

function TMat.channels: integer;
begin
  Result := _GetMatData(FData, 4);
end;

function TMat.cols: integer;
begin
  Result := _GetMatData(FData, 10);
end;

constructor TMat.Create(const Image: pIplImage);
begin
  FData := _CreateMatFromImage(Image);
  FNeedDestroy := True;
end;

constructor TMat.Create(const _M: TOpenCVClass; const NeedDestroy: boolean);
begin
  FNeedDestroy := NeedDestroy;
  if Assigned(_M) then
    FData := _M
  else
    FData := _CreateMat;
end;

function TMat.data: pointer;
begin
  Result := pointer(_GetMatData(FData, 11));
end;

function TMat.depth: integer;
begin
  Result := _GetMatData(FData, 3);
end;

destructor TMat.Destroy;
begin
  if Assigned(FData) and FNeedDestroy then
    _DestroyMat(FData);
  inherited;
end;

function TMat.dims: integer;
begin
  Result := _GetMatData(FData, 8);
end;

function TMat.elemSize: size_t;
begin
  Result := _GetMatData(FData, 0);
end;

function TMat.elemSize1: size_t;
begin
  Result := _GetMatData(FData, 1);
end;

function TMat.empty: cbool;
begin
  Result := _MatEmpty(FData);
end;

function TMat.flags: integer;
begin
  Result := _GetMatData(FData, 7);
end;

function TMat.rows: integer;
begin
  Result := _GetMatData(FData, 9);
end;

function TMat.step1(i: integer): size_t;
begin
  Result := _GetMatData(FData, 5, i);
end;

function TMat.total: size_t;
begin
  Result := _GetMatData(FData, 6);
end;

function TMat._type: integer;
begin
  Result := _GetMatData(FData, 2);
end;

{ ------------------------------ TIplImageRecordHelper ------------------------------ }

function TIplImageRecordHelper.InitFromMat(const Mat: IMat): TIplImage;
begin
  Assert(Mat.dims <= 2);
  cvInitImageHeader(@Self, CvSize(Mat.cols, Mat.rows), cvIplDepth(Mat.flags), Mat.channels);
  cvSetData(@Self, Mat.data, Mat.step1);
end;

{$IFDEF SAFELOADLIB}

Var
  coreDLL: Cardinal;

procedure Init_opencv_cls_core;
begin
  coreDLL := ocvLoadLibrary(core_lib);
  Assert(coreDLL <> 0, 'Can not init ' + core_lib);

  setBreakOnError := ocvGetProcAddress('?setBreakOnError@cv@@YA_N_N@Z', coreDLL);
  redirectError := ocvGetProcAddress('?redirectError@cv@@YAP6AHHPBD00HPAX@ZP6AHH000H1@Z1PAPAX@Z', coreDLL);
  setNumThreads := ocvGetProcAddress('?setNumThreads@cv@@YAXH@Z', coreDLL);
  getNumThreads := ocvGetProcAddress('?getNumThreads@cv@@YAHXZ', coreDLL);
  getThreadNum := ocvGetProcAddress('?getThreadNum@cv@@YAHXZ', coreDLL);
  getTickCount := ocvGetProcAddress('?getTickCount@cv@@YA_JXZ', coreDLL);
  getTickFrequency := ocvGetProcAddress('?getTickFrequency@cv@@YANXZ', coreDLL);
  getCPUTickCount := ocvGetProcAddress('?getCPUTickCount@cv@@YA_JXZ', coreDLL);
  checkHardwareSupport := ocvGetProcAddress('?checkHardwareSupport@cv@@YA_NH@Z', coreDLL);
  getNumberOfCPUs := ocvGetProcAddress('?getNumberOfCPUs@cv@@YAHXZ', coreDLL);
  fastMalloc := ocvGetProcAddress('?fastMalloc@cv@@YAPAXI@Z', coreDLL);
  fastFree := ocvGetProcAddress('?fastFree@cv@@YAXPAX@Z', coreDLL);
  setUseOptimized := ocvGetProcAddress('?setUseOptimized@cv@@YAX_N@Z', coreDLL);
  useOptimized := ocvGetProcAddress('?useOptimized@cv@@YA_NXZ', coreDLL);
end;

initialization

Init_opencv_cls_core;

{$ELSE}
function setBreakOnError; external core_lib name '?setBreakOnError@cv@@YA_N_N@Z';
function redirectError; external core_lib name '?redirectError@cv@@YAP6AHHPBD00HPAX@ZP6AHH000H1@Z1PAPAX@Z';
procedure setNumThreads; external core_lib name '?setNumThreads@cv@@YAXH@Z';
function getNumThreads; external core_lib name '?getNumThreads@cv@@YAHXZ';
function getThreadNum; external core_lib name '?getThreadNum@cv@@YAHXZ';
function getTickCount; external core_lib name '?getTickCount@cv@@YA_JXZ';
function getTickFrequency; external core_lib name '?getTickFrequency@cv@@YANXZ';
function getCPUTickCount; external core_lib name '?getCPUTickCount@cv@@YA_JXZ';
function checkHardwareSupport; external core_lib name '?checkHardwareSupport@cv@@YA_NH@Z';
function getNumberOfCPUs; external core_lib name '?getNumberOfCPUs@cv@@YAHXZ';
function fastMalloc; external core_lib name '?fastMalloc@cv@@YAPAXI@Z';
procedure fastFree; external core_lib name '?fastFree@cv@@YAXPAX@Z';
procedure setUseOptimized; external core_lib name '?setUseOptimized@cv@@YAX_N@Z';
function useOptimized; external core_lib name '?useOptimized@cv@@YA_NXZ';
{$ENDIF}

{ TSize }

function _CreateSize: TOpenCVClass; stdcall; external opencv_classes_lib name '_CreateSize@0';
procedure _DestroySize(const s: TOpenCVClass); stdcall; external opencv_classes_lib name '_DestroySize@4';
function _CreateSizeFromCvSize(const sz: PCvSize): TOpenCVClass; stdcall; external opencv_classes_lib name '_CreateSizeFromCvSize@4';

constructor TSize.Create;
begin
  inherited Create(_CreateSize);
end;

constructor TSize.Create(const sz: TCvSize);
begin
  inherited Create(_CreateSizeFromCvSize(@sz));
end;

destructor TSize.Destroy;
begin
  if Assigned(FData) then
    _DestroySize(FData);
  inherited;
end;

end.
