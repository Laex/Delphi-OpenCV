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
  opencv\modules\legacy\include\opencv2\legacy\compat.hpp
  *************************************************************************************************
*)

unit ocv.compat;

{$I OpenCV.inc}

interface

uses
  ocv.core.types_c,
  ocv.imgproc.types_c,
  ocv.core_c;

type
  // typedef int CvMatType;
  TCvMatType = Integer;
  // typedef int CvDisMaskType;
  TCvDisMaskType = Integer;
  // typedef CvMat CvMatArray;
  TCvMatArray = TCvMat;

  // typedef int CvThreshType;
  TCvThreshType = Integer;
  // typedef int CvAdaptiveThreshMethod;
  TCvAdaptiveThreshMethod = Integer;
  // typedef int CvCompareMethod;
  TCvCompareMethod = Integer;
  // typedef int CvFontFace;
  TCvFontFace = Integer;
  // typedef int CvPolyApproxMethod;
  TCvPolyApproxMethod = Integer;
  // typedef int CvContoursMatchMethod;
  TCvContoursMatchMethod = Integer;
  // typedef int CvContourTreesMatchMethod;
  TCvContourTreesMatchMethod = Integer;
  // typedef int CvCoeffType;
  TCvCoeffType = Integer;
  // typedef int CvRodriguesType;
  TCvRodriguesType = Integer;
  // typedef int CvElementShape;
  TCvElementShape = Integer;
  // typedef int CvMorphOp;
  TCvMorphOp = Integer;
  // typedef int CvTemplMatchMethod;
  TCvTemplMatchMethod = Integer;

  // typedef CvPoint2D64f CvPoint2D64d;
  TCvPoint2D64d = TCvPoint2D64f;
  // typedef CvPoint3D64f CvPoint3D64d;
  TCvPoint3D64d = TCvPoint3D64f;

var
  // Assigning values t​othe constants has been moved to the section of the module initialization
  CV_MAT32F: Integer; // = CV_32FC1;
  CV_MAT3x1_32F: Integer; // = CV_32FC1;
  CV_MAT4x1_32F: Integer; // = CV_32FC1;
  CV_MAT3x3_32F: Integer; // = CV_32FC1;
  CV_MAT4x4_32F: Integer; // = CV_32FC1;

  CV_MAT64D: Integer; // = CV_64FC1;
  CV_MAT3x1_64D: Integer; // = CV_64FC1;
  CV_MAT4x1_64D: Integer; // = CV_64FC1;
  CV_MAT3x3_64D: Integer; // = CV_64FC1;
  CV_MAT4x4_64D: Integer; // = CV_64FC1;

const
  IPL_GAUSSIAN_5x5 = 7;

type
  // typedef CvBox2D  CvBox2D32f;
  TCvBox2D32f = TCvBox2D;

  // * allocation/deallocation macros */
  // #define cvCreateImageData   cvCreateData
  // #define cvReleaseImageData  cvReleaseData
  // #define cvSetImageData      cvSetData
  // #define cvGetImageRawData   cvGetRawData
  //
  // #define cvmAlloc            cvCreateData
  // #define cvmFree             cvReleaseData
  // #define cvmAllocArray       cvCreateData
  // #define cvmFreeArray        cvReleaseData
  //
  // #define cvIntegralImage     cvIntegral
  // #define cvMatchContours     cvMatchShapes

  (*
    CV_EXPORTS CvMat cvMatArray( int rows, int cols, int type,
    int count, void* data CV_DEFAULT(0));
  *)
{$IFDEF SAFELOADLIB}

type
  TcvMatArray_fun = function(rows: Integer; cols: Integer; type_: Integer; count: Integer; data: Pointer = nil): TCvMat; cdecl;

Var
  cvMatArray: TcvMatArray_fun;
{$ELSE}
function cvMatArray(rows: Integer; cols: Integer; type_: Integer; count: Integer; data: Pointer = nil): TCvMat; cdecl;

{$ENDIF}
// #define cvUpdateMHIByTime  cvUpdateMotionHistory
//
// #define cvAccMask cvAcc
// #define cvSquareAccMask cvSquareAcc
// #define cvMultiplyAccMask cvMultiplyAcc
// #define cvRunningAvgMask(imgY, imgU, mask, alpha) cvRunningAvg(imgY, imgU, alpha, mask)
//
// #define cvSetHistThresh  cvSetHistBinRanges
// #define cvCalcHistMask(img, mask, hist, doNotClear) cvCalcHist(img, hist, doNotClear, mask)

(*
  CV_EXPORTS double cvMean( const CvArr* image, const CvArr* mask CV_DEFAULT(0));
*)
{$IFDEF SAFELOADLIB}

Type
  TcvMean = function(const image: PCvArr; const mask: PCvArr = nil): Double; cdecl;

var
  cvMean: TcvMean;
{$ELSE}
function cvMean(const image: PCvArr; const mask: PCvArr = nil): Double; cdecl;
{$ENDIF}
(*
  CV_EXPORTS double cvSumPixels( const CvArr* image );
*)
{$IFDEF SAFELOADLIB}

type
  TcvSumPixels = function(const image: PCvArr): Double; cdecl;

var
  cvSumPixels: TcvSumPixels;
{$ELSE}
function cvSumPixels(const image: PCvArr): Double; cdecl;
{$ENDIF}
(*
  CV_EXPORTS void  cvMean_StdDev( const CvArr* image, double* mean, double* sdv,
  const CvArr* mask CV_DEFAULT(0));
*)
{$IFDEF SAFELOADLIB}

type
  TcvMean_StdDev = procedure(const image: PCvArr; mean: PDouble; sdv: PDouble; const mask: PCvArr = nil); cdecl;

var
  cvMean_StdDev: TcvMean_StdDev;
{$ELSE}
procedure cvMean_StdDev(const image: PCvArr; mean: PDouble; sdv: PDouble; const mask: PCvArr = nil); cdecl;
{$ENDIF}
(*
  CV_EXPORTS void cvmPerspectiveProject( const CvMat* mat, const CvArr* src, CvArr* dst );
*)
{$IFDEF SAFELOADLIB}

type
  TcvmPerspectiveProject = procedure(const mat: PCvMat; const src: PCvArr; dst: PCvArr); cdecl;

var
  cvmPerspectiveProject: TcvmPerspectiveProject;
{$ELSE}
procedure cvmPerspectiveProject(const mat: PCvMat; const src: PCvArr; dst: PCvArr); cdecl;
{$ENDIF}
(*
  CV_EXPORTS void cvFillImage( CvArr* mat, double color );
*)
{$IFDEF SAFELOADLIB}

type
  TcvFillImage = procedure(mat: PCvArr; color: Double); cdecl;

var
  cvFillImage: TcvFillImage;
{$ELSE}
procedure cvFillImage(mat: PCvArr; color: Double); cdecl;
{$ENDIF}
//
// #define cvCvtPixToPlane cvSplit //defined in ocv.core_c
// #define cvCvtPlaneToPix cvMerge //defined in ocv.core_c

type
  // typedef struct CvRandState
  // {
  // CvRNG     state;    /* RNG state (the current seed and carry)*/
  // int       disttype; /* distribution type */
  // CvScalar  param[2]; /* parameters of RNG */
  // } CvRandState;
  pCvRandState = ^TCvRandState;

  TCvRandState = record
    state: TCvRNG; (* RNG state (the current seed and carry) *)
    disttype: Integer; (* distribution type *)
    param: array [0 .. 1] of TCvScalar; (* parameters of RNG *)
  end;

  (*
    Changes RNG range while preserving RNG state

    CV_EXPORTS void  cvRandSetRange( CvRandState* state, double param1,
    double param2, int index CV_DEFAULT(-1));
  *)
{$IFDEF SAFELOADLIB}

type
  TcvRandSetRange = procedure(state: pCvRandState; param1: Double; param2: Double; index: Integer = -1); cdecl;

var
  cvRandSetRange: TcvRandSetRange;
{$ELSE}
procedure cvRandSetRange(state: pCvRandState; param1: Double; param2: Double; index: Integer = -1); cdecl;
{$ENDIF}
(*
  CV_EXPORTS void  cvRandInit( CvRandState* state, double param1,
  double param2, int seed,
  int disttype CV_DEFAULT(CV_RAND_UNI));
*)
{$IFDEF SAFELOADLIB}

type
  TcvRandInit = procedure(state: pCvRandState; param1: Double; param2: Double; seed: Integer; disttype: Integer = CV_RAND_UNI); cdecl;

var
  cvRandInit: TcvRandInit;
{$ELSE}
procedure cvRandInit(state: pCvRandState; param1: Double; param2: Double; seed: Integer; disttype: Integer = CV_RAND_UNI); cdecl;
{$ENDIF}
(*
  Fills array with random numbers

  CV_EXPORTS void cvRand( CvRandState* state, CvArr* arr );
*)
{$IFDEF SAFELOADLIB}

type
  TcvRand = procedure(state: pCvRandState; arr: PCvArr); cdecl;

var
  cvRand: TcvRand;
{$ELSE}
procedure cvRand(state: pCvRandState; arr: PCvArr); cdecl;
{$ENDIF}
// #define cvRandNext( _state ) cvRandInt( &(_state)->state )

// CV_EXPORTS void cvbRand( CvRandState* state, float* dst, int len );
{$IFDEF SAFELOADLIB}

type
  TcvbRand = procedure(state: pCvRandState; dst: PSingle; len: Integer); cdecl;

var
  cvbRand: TcvbRand;
{$ELSE}
procedure cvbRand(state: pCvRandState; dst: PSingle; len: Integer); cdecl;
{$ENDIF}
// CV_EXPORTS void  cvbCartToPolar( const float* y, const float* x, float* magnitude, float* angle, int len );
{$IFDEF SAFELOADLIB}

type
  TcvbCartToPolar = procedure(const y: PSingle; const x: PSingle; Var magnitude: Single; Var angle: Single; len: Integer); cdecl;

var
  cvbCartToPolar: TcvbCartToPolar;
{$ELSE}
procedure cvbCartToPolar(const y: PSingle; const x: PSingle; Var magnitude: Single; Var angle: Single; len: Integer); cdecl;
{$ENDIF}
// CV_EXPORTS void  cvbFastArctan( const float* y, const float* x, float* angle, int len );
{$IFDEF SAFELOADLIB}

type
  TcvbFastArctan = procedure(const y: PSingle; const x: PSingle; Var angle: Single; len: Integer); cdecl;

var
  cvbFastArctan: TcvbFastArctan;
{$ELSE}
procedure cvbFastArctan(const y: PSingle; const x: PSingle; Var angle: Single; len: Integer); cdecl;
{$ENDIF}
// CV_EXPORTS void  cvbSqrt( const float* x, float* y, int len );
{$IFDEF SAFELOADLIB}

type
  TcvbSqrt = procedure(const x: PSingle; Var y: Single; len: Integer); cdecl;

var
  cvbSqrt: TcvbSqrt;
{$ELSE}
procedure cvbSqrt(const x: PSingle; Var y: Single; len: Integer); cdecl;
{$ENDIF}
// CV_EXPORTS void  cvbInvSqrt( const float* x, float* y, int len );
{$IFDEF SAFELOADLIB}

type
  TcvbInvSqrt = procedure(const x: PSingle; Var y: Single; len: Integer); cdecl;

var
  cvbInvSqrt: TcvbInvSqrt;
{$ELSE}
procedure cvbInvSqrt(const x: PSingle; Var y: Single; len: Integer); cdecl;
{$ENDIF}
// CV_EXPORTS void  cvbReciprocal( const float* x, float* y, int len );
{$IFDEF SAFELOADLIB}

type
  TcvbReciprocal = procedure(const x: PSingle; var y: Single; len: Integer); cdecl;

var
  cvbReciprocal: TcvbReciprocal;
{$ELSE}
procedure cvbReciprocal(const x: PSingle; var y: Single; len: Integer); cdecl;
{$ENDIF}
// CV_EXPORTS void  cvbFastExp( const float* x, double* y, int len );
{$IFDEF SAFELOADLIB}

type
  TcvbFastExp = procedure(const x: PSingle; Var y: Double; len: Integer); cdecl;

var
  cvbFastExp: TcvbFastExp;
{$ELSE}
procedure cvbFastExp(const x: PSingle; Var y: Double; len: Integer); cdecl;
{$ENDIF}
// CV_EXPORTS void  cvbFastLog( const double* x, float* y, int len );
{$IFDEF SAFELOADLIB}

type
  TcvbFastLog = procedure(const x: PDouble; Var y: Single; len: Integer); cdecl;

var
  cvbFastLog: TcvbFastLog;
{$ELSE}
procedure cvbFastLog(const x: PDouble; Var y: Single; len: Integer); cdecl;
{$ENDIF}
(*
  CV_EXPORTS CvRect  cvContourBoundingRect( void* point_set, int update CV_DEFAULT(0));
*)
{$IFDEF SAFELOADLIB}

type
  TcvContourBoundingRect = function(point_set: Pointer; update: Integer = 0): TCvRect; cdecl;

var
  cvContourBoundingRect: TcvContourBoundingRect;
{$ELSE}
function cvContourBoundingRect(point_set: Pointer; update: Integer = 0): TCvRect; cdecl;
{$ENDIF}
(*
  CV_EXPORTS double cvPseudoInverse( const CvArr* src, CvArr* dst );
*)
{$IFDEF SAFELOADLIB}

type
  TcvPseudoInverse = function(const src: PCvArr; dst: PCvArr): Double; cdecl;

var
  cvPseudoInverse: TcvPseudoInverse;
{$ELSE}
function cvPseudoInverse(const src: PCvArr; dst: PCvArr): Double; cdecl;
{$ENDIF}
// #define cvPseudoInv cvPseudoInverse
// #define cvContourMoments( contour, moments ) cvMoments( contour, moments, 0 )
//
// #define cvGetPtrAt              cvPtr2D
// #define cvGetAt                 cvGet2D
// #define cvSetAt(arr,val,y,x)    cvSet2D((arr),(y),(x),(val))
//
// #define cvMeanMask  cvMean
// #define cvMean_StdDevMask(img,mask,mean,sdv) cvMean_StdDev(img,mean,sdv,mask)
//
// #define cvNormMask(imgA,imgB,mask,normType) cvNorm(imgA,imgB,normType,mask)
//
// #define cvMinMaxLocMask(img, mask, min_val, max_val, min_loc, max_loc) \
// cvMinMaxLoc(img, min_val, max_val, min_loc, max_loc, mask)
//
// #define cvRemoveMemoryManager  CV_NOOP
// #define cvSetMemoryManager     CV_NOOP
//
// #define cvmSetZero( mat )               cvSetZero( mat )
// #define cvmSetIdentity( mat )           cvSetIdentity( mat )
// #define cvmAdd( src1, src2, dst )       cvAdd( src1, src2, dst, 0 )
// #define cvmSub( src1, src2, dst )       cvSub( src1, src2, dst, 0 )
// #define cvmCopy( src, dst )             cvCopy( src, dst, 0 )
// #define cvmMul( src1, src2, dst )       cvMatMulAdd( src1, src2, 0, dst )
// #define cvmTranspose( src, dst )        cvT( src, dst )
// #define cvmInvert( src, dst )           cvInv( src, dst )
// #define cvmMahalanobis(vec1, vec2, mat) cvMahalanobis( vec1, vec2, mat )
// #define cvmDotProduct( vec1, vec2 )     cvDotProduct( vec1, vec2 )
// #define cvmCrossProduct(vec1, vec2,dst) cvCrossProduct( vec1, vec2, dst )
// #define cvmTrace( mat )                 (cvTrace( mat )).val[0]
// #define cvmMulTransposed( src, dst, order ) cvMulTransposed( src, dst, order )
// #define cvmEigenVV( mat, evec, eval, eps)   cvEigenVV( mat, evec, eval, eps )
// #define cvmDet( mat )                   cvDet( mat )
// #define cvmScale( src, dst, scale )     cvScale( src, dst, scale )
//
// #define cvCopyImage( src, dst )         cvCopy( src, dst, 0 )
// #define cvReleaseMatHeader              cvReleaseMat

(*
  Calculates exact convex hull of 2d point set

  CV_EXPORTS void cvConvexHull( CvPoint* points, int num_points,
  CvRect* bound_rect,
  int orientation, int* hull, int* hullsize );
*)
{$IFDEF SAFELOADLIB}

type
  TcvConvexHull = procedure(points: PCvPoint; num_points: Integer; bound_rect: PCvRect; orientation: Integer; Var hull: Integer;
    Var hullsize: Integer); cdecl;

var
  cvConvexHull: TcvConvexHull;
{$ELSE}
procedure cvConvexHull(points: PCvPoint; num_points: Integer; bound_rect: PCvRect; orientation: Integer; Var hull: Integer;
  Var hullsize: Integer); cdecl;
{$ENDIF}
(*
  CV_EXPORTS void cvMinAreaRect( CvPoint* points, int n,
  int left, int bottom,
  int right, int top,
  CvPoint2D32f* anchor,
  CvPoint2D32f* vect1,
  CvPoint2D32f* vect2 );
*)
{$IFDEF SAFELOADLIB}

type
  TcvMinAreaRect = procedure(points: PCvPoint; n: Integer; left: Integer; bottom: Integer; right: Integer; top: Integer;
    anchor: PCvPoint2D32f; vect1: PCvPoint2D32f; vect2: PCvPoint2D32f); cdecl;

var
  cvMinAreaRect: TcvMinAreaRect;
{$ELSE}
procedure cvMinAreaRect(points: PCvPoint; n: Integer; left: Integer; bottom: Integer; right: Integer; top: Integer; anchor: PCvPoint2D32f;
  vect1: PCvPoint2D32f; vect2: PCvPoint2D32f); cdecl;
{$ENDIF}

Type
  TCvDisType = type Integer;
  TCvChainApproxMethod = type Integer;
  TCvContourRetrievalMode = type Integer;

  (*
    CV_EXPORTS  void  cvFitLine3D( CvPoint3D32f* points, int count, int dist,
    void *param, float reps, float aeps, float* line );
  *)
{$IFDEF SAFELOADLIB}

type
  TcvFitLine3D = procedure(points: PCvPoint3D32f; count: Integer; dist: Integer; param: Pointer; reps: Single; aeps: Single;
    Var line: Single); cdecl;

var
  cvFitLine3D: TcvFitLine3D;
{$ELSE}
procedure cvFitLine3D(points: PCvPoint3D32f; count: Integer; dist: Integer; param: Pointer; reps: Single; aeps: Single;
  Var line: Single); cdecl;
{$ENDIF}
(*
  Fits a line into set of 2d points in a robust way (M-estimator technique)

  CV_EXPORTS  void  cvFitLine2D( CvPoint2D32f* points, int count, int dist,
  void *param, float reps, float aeps, float* line );
*)
{$IFDEF SAFELOADLIB}

type
  TcvFitLine2D = procedure(points: PCvPoint2D32f; count: Integer; dist: Integer; param: Pointer; reps: Single; aeps: Single;
    Var line: Single); cdecl;

var
  cvFitLine2D: TcvFitLine2D;
{$ELSE}
procedure cvFitLine2D(points: PCvPoint2D32f; count: Integer; dist: Integer; param: Pointer; reps: Single; aeps: Single;
  Var line: Single); cdecl;
{$ENDIF}
// CV_EXPORTS  void  cvFitEllipse( const CvPoint2D32f* points, int count, CvBox2D* box );
{$IFDEF SAFELOADLIB}

type
  TcvFitEllipse = procedure(const points: PCvPoint2D32f; count: Integer; Var box: TCvBox2D); cdecl;

var
  cvFitEllipse: TcvFitEllipse;
{$ELSE}
procedure cvFitEllipse(const points: PCvPoint2D32f; count: Integer; Var box: TCvBox2D); cdecl;
{$ENDIF}
(*
  Projects 2d points to one of standard coordinate planes
  (i.e. removes one of coordinates)

  CV_EXPORTS  void  cvProject3D( CvPoint3D32f* points3D, int count,
  CvPoint2D32f* points2D,
  int xIndx CV_DEFAULT(0),
  int yIndx CV_DEFAULT(1));
*)
{$IFDEF SAFELOADLIB}

type
  TcvProject3D = procedure(points3D: PCvPoint3D32f; count: Integer; points2D: PCvPoint2D32f; xIndx: Integer = 0; yIndx: Integer = 1); cdecl;

var
  cvProject3D: TcvProject3D;
{$ELSE}
procedure cvProject3D(points3D: PCvPoint3D32f; count: Integer; points2D: PCvPoint2D32f; xIndx: Integer = 0; yIndx: Integer = 1); cdecl;
{$ENDIF}
(* Retrieves value of the particular bin
  of x-dimensional (x=1,2,3,...) histogram

  #define cvQueryHistValue_1D( hist, idx0 ) \
  ((float)cvGetReal1D( (hist)->bins, (idx0)))
  #define cvQueryHistValue_2D( hist, idx0, idx1 ) \
  ((float)cvGetReal2D( (hist)->bins, (idx0), (idx1)))
  #define cvQueryHistValue_3D( hist, idx0, idx1, idx2 ) \
  ((float)cvGetReal3D( (hist)->bins, (idx0), (idx1), (idx2)))
  #define cvQueryHistValue_nD( hist, idx ) \
  ((float)cvGetRealND( (hist)->bins, (idx)))
*)

(* Returns pointer to the particular bin of x-dimesional histogram.
  For sparse histogram the bin is created if it didn't exist before

  #define cvGetHistValue_1D( hist, idx0 ) \
  ((float* )cvPtr1D( (hist)->bins, (idx0), 0))
  #define cvGetHistValue_2D( hist, idx0, idx1 ) \
  ((float* )cvPtr2D( (hist)->bins, (idx0), (idx1), 0))
  #define cvGetHistValue_3D( hist, idx0, idx1, idx2 ) \
  ((float* )cvPtr3D( (hist)->bins, (idx0), (idx1), (idx2), 0))
  #define cvGetHistValue_nD( hist, idx ) \
  ((float* )cvPtrND( (hist)->bins, (idx), 0))
*)

// #define CV_IS_SET_ELEM_EXISTS CV_IS_SET_ELEM

(*
  CV_EXPORTS  int  cvHoughLines( CvArr* image, double rho,
  double theta, int threshold,
  float* lines, int linesNumber );
*)
{$IFDEF SAFELOADLIB}

type
  TcvHoughLines = function(image: PCvArr; rho: Double; theta: Double; threshold: Integer; lines: pfloat; linesNumber: Integer)
    : Integer; cdecl;

var
  cvHoughLines: TcvHoughLines;
{$ELSE}
function cvHoughLines(image: PCvArr; rho: Double; theta: Double; threshold: Integer; lines: pfloat; linesNumber: Integer): Integer; cdecl;
{$ENDIF}
(*
  CV_EXPORTS  int  cvHoughLinesP( CvArr* image, double rho,
  double theta, int threshold,
  int lineLength, int lineGap,
  int* lines, int linesNumber );
*)
{$IFDEF SAFELOADLIB}

type
  TcvHoughLinesP = function(image: PCvArr; rho: Double; theta: Double; threshold: Integer; lineLength: Integer; lineGap: Integer;
    lines: pInteger; linesNumber: Integer): Integer; cdecl;

var
  cvHoughLinesP: TcvHoughLinesP;
{$ELSE}
function cvHoughLinesP(image: PCvArr; rho: Double; theta: Double; threshold: Integer; lineLength: Integer; lineGap: Integer;
  lines: pInteger; linesNumber: Integer): Integer; cdecl;
{$ENDIF}
(*
  CV_EXPORTS  int  cvHoughLinesSDiv( CvArr* image, double rho, int srn,
  double theta, int stn, int threshold,
  float* lines, int linesNumber );
*)
{$IFDEF SAFELOADLIB}

type
  TcvHoughLinesSDiv = function(image: PCvArr; rho: Double; srn: Integer; theta: Double; stn: Integer; threshold: Integer; lines: pfloat;
    linesNumber: Integer): Integer; cdecl;

var
  cvHoughLinesSDiv: TcvHoughLinesSDiv;
{$ELSE}
function cvHoughLinesSDiv(image: PCvArr; rho: Double; srn: Integer; theta: Double; stn: Integer; threshold: Integer; lines: pfloat;
  linesNumber: Integer): Integer; cdecl;
{$ENDIF}
(*
  CV_EXPORTS  float  cvCalcEMD( const float* signature1, int size1,
  const float* signature2, int size2,
  int dims, int dist_type CV_DEFAULT(CV_DIST_L2),
  CvDistanceFunction dist_func CV_DEFAULT(0),
  float* lower_bound CV_DEFAULT(0),
  void* user_param CV_DEFAULT(0));
*)
{$IFDEF SAFELOADLIB}

type
  TcvCalcEMD = function(const signature1: pfloat; size1: Integer; const signature2: pfloat; size2: Integer; dims: Integer;
    dist_type: Integer = CV_DIST_L2; dist_func: TCvDistanceFunction = nil; lower_bound: pfloat = nil; user_param: Pointer = nil)
    : float; cdecl;

var
  cvCalcEMD: TcvCalcEMD;

{$ELSE}
function cvCalcEMD(const signature1: pfloat; size1: Integer; const signature2: pfloat; size2: Integer; dims: Integer;
  dist_type: Integer = CV_DIST_L2; dist_func: TCvDistanceFunction = nil; lower_bound: pfloat = nil; user_param: Pointer = nil)
  : float; cdecl;
{$ENDIF}
(*
  CV_EXPORTS  void  cvKMeans( int num_clusters, float** samples,
  int num_samples, int vec_size,
  CvTermCriteria termcrit, int* cluster_idx );
*)
{$IFDEF SAFELOADLIB}

type
  TcvKMeans = procedure(num_clusters: Integer; Var samples: PSingle; num_samples: Integer; vec_size: Integer; termcrit: TCvTermCriteria;
    Var cluster_idx: Integer); cdecl;

var
  cvKMeans: TcvKMeans;
{$ELSE}
procedure cvKMeans(num_clusters: Integer; Var samples: PSingle; num_samples: Integer; vec_size: Integer; termcrit: TCvTermCriteria;
  Var cluster_idx: Integer); cdecl;
{$ENDIF}
(*
  CV_EXPORTS void  cvStartScanGraph( CvGraph* graph, CvGraphScanner* scanner,
  CvGraphVtx* vtx CV_DEFAULT(NULL),
  int mask CV_DEFAULT(CV_GRAPH_ALL_ITEMS));
*)
{$IFDEF SAFELOADLIB}

type
  TcvStartScanGraph = procedure(graph: PCvGraph; scanner: PCvGraphScanner; vtx: PCvGraphVtx = nil;
    mask: Integer = CV_GRAPH_ALL_ITEMS); cdecl;

var
  cvStartScanGraph: TcvStartScanGraph;
{$ELSE}
procedure cvStartScanGraph(graph: PCvGraph; scanner: PCvGraphScanner; vtx: PCvGraphVtx = nil; mask: Integer = CV_GRAPH_ALL_ITEMS); cdecl;
{$ENDIF}
// CV_EXPORTS  void  cvEndScanGraph( CvGraphScanner* scanner );
{$IFDEF SAFELOADLIB}

type
  TcvEndScanGraph = procedure(scanner: PCvGraphScanner); cdecl;

var
  cvEndScanGraph: TcvEndScanGraph;
{$ELSE}
procedure cvEndScanGraph(scanner: PCvGraphScanner); cdecl;
{$ENDIF}
(* old drawing functions *)

(*
  CV_EXPORTS void  cvLineAA( CvArr* img, CvPoint pt1, CvPoint pt2,
  double color, int scale CV_DEFAULT(0));
*)
{$IFDEF SAFELOADLIB}

type
  TcvLineAA = procedure(img: PCvArr; pt1: TCvPoint; pt2: TCvPoint; color: Double; scale: Integer = 0); cdecl;

var
  cvLineAA: TcvLineAA;
{$ELSE}
procedure cvLineAA(img: PCvArr; pt1: TCvPoint; pt2: TCvPoint; color: Double; scale: Integer = 0); cdecl;
{$ENDIF}
(*
  CV_EXPORTS void  cvCircleAA( CvArr* img, CvPoint center, int radius,
  double color, int scale CV_DEFAULT(0) );
*)
{$IFDEF SAFELOADLIB}

type
  TcvCircleAA = procedure(img: PCvArr; center: TCvPoint; radius: Integer; color: Double; scale: Integer = 0); cdecl;

var
  cvCircleAA: TcvCircleAA;
{$ELSE}
procedure cvCircleAA(img: PCvArr; center: TCvPoint; radius: Integer; color: Double; scale: Integer = 0); cdecl;
{$ENDIF}
(*
  CV_EXPORTS void  cvEllipseAA( CvArr* img, CvPoint center, CvSize axes,
  double angle, double start_angle,
  double end_angle, double color,
  int scale CV_DEFAULT(0) );
*)
{$IFDEF SAFELOADLIB}

type
  TcvEllipseAA = procedure(img: PCvArr; center: TCvPoint; axes: TCvSize; angle: Double; start_angle: Double; end_angle: Double;
    color: Double; scale: Integer = 0); cdecl;

var
  cvEllipseAA: TcvEllipseAA;
{$ELSE}
procedure cvEllipseAA(img: PCvArr; center: TCvPoint; axes: TCvSize; angle: Double; start_angle: Double; end_angle: Double; color: Double;
  scale: Integer = 0); cdecl;
{$ENDIF}
(*
  CV_EXPORTS void  cvPolyLineAA( CvArr* img, CvPoint** pts, int* npts, int contours,
  int is_closed, double color, int scale CV_DEFAULT(0) );
*)
{$IFDEF SAFELOADLIB}

type
  TcvPolyLineAA = procedure(img: PCvArr; Var pts: PCvPoint; Var npts: Integer; contours: Integer; is_closed: Integer; color: Double;
    scale: Integer = 0); cdecl;

var
  cvPolyLineAA: TcvPolyLineAA;
{$ELSE}
procedure cvPolyLineAA(img: PCvArr; Var pts: PCvPoint; Var npts: Integer; contours: Integer; is_closed: Integer; color: Double;
  scale: Integer = 0); cdecl;
{$ENDIF}

(* ***************************************************************************************\
  *                                   Pixel Access Macros                                  *
  \*************************************************************************************** *)
Type
  // typedef struct _CvPixelPosition8u
  // {
  // uchar*  currline;      /* pointer to the start of the current pixel line   */
  // uchar*  topline;       /* pointer to the start of the top pixel line       */
  // uchar*  bottomline;    /* pointer to the start of the first line           */
  // /* which is below the image                         */
  // int     x;                      /* current x coordinate ( in pixels )               */
  // int     width;                  /* width of the image  ( in pixels )                */
  // int     height;                 /* height of the image  ( in pixels )               */
  // int     step;                   /* distance between lines ( in elements of single   */
  // /* plane )                                          */
  // int     step_arr[3];            /* array: ( 0, -step, step ). It is used for        */
  // /* vertical moving                                  */
  // } CvPixelPosition8u;

  TCvPixelPosition8u = record
    currline: PUChar; (* pointer to the start of the current pixel line *)
    topline: PUChar; (* pointer to the start of the top pixel line *)
    bottomline: PUChar; (* pointer to the start of the first line *)
    (* which is below the image *)
    x: Integer; (* current x coordinate ( in pixels ) *)
    width: Integer; (* width of the image  ( in pixels ) *)
    height: Integer; (* height of the image  ( in pixels ) *)
    step: Integer; (* distance between lines ( in elements of single *)
    (* plane ) *)
    step_arr: array [0 .. 2] of Integer; (* array: ( 0, -step, step ). It is used for *)
    (* vertical moving *)
  end;

  /// * this structure differs from the above only in data type */
  // typedef struct _CvPixelPosition8s
  // {
  // schar*  currline;
  // schar*  topline;
  // schar*  bottomline;
  // int     x;
  // int     width;
  // int     height;
  // int     step;
  // int     step_arr[3];
  // } CvPixelPosition8s;

  TCvPixelPosition8s = record
    currline: Pschar;
    topline: Pschar;
    bottomline: Pschar;
    x: Integer;
    width: Integer;
    height: Integer;
    step: Integer;
    step_arr: array [0 .. 2] of Integer;
  end;

  /// * this structure differs from the CvPixelPosition8u only in data type */
  // typedef struct _CvPixelPosition32f
  // {
  // float*  currline;
  // float*  topline;
  // float*  bottomline;
  // int     x;
  // int     width;
  // int     height;
  // int     step;
  // int     step_arr[3];
  // } CvPixelPosition32f;

  TCvPixelPosition32f = record
    currline: PSingle;
    topline: PSingle;
    bottomline: PSingle;
    x: Integer;
    width: Integer;
    height: Integer;
    step: Integer;
    step_arr: array [0 .. 2] of Integer;
  end;

  (* Initialize one of the CvPixelPosition structures. *)
  (* pos    - initialized structure *)
  (* origin - pointer to the left-top corner of the ROI *)
  (* step   - width of the whole image in bytes *)
  (* roi    - width & height of the ROI *)
  (* x, y   - initial position *)
  (*
    #define CV_INIT_PIXEL_POS(pos, origin, _step, roi, _x, _y, orientation)    \
    (                                                                        \
    (pos).step = (_step)/sizeof((pos).currline[0]) * (orientation ? -1 : 1), \
    (pos).width = (roi).width,                                               \
    (pos).height = (roi).height,                                             \
    (pos).bottomline = (origin) + (pos).step*(pos).height,                   \
    (pos).topline = (origin) - (pos).step,                                   \
    (pos).step_arr[0] = 0,                                                   \
    (pos).step_arr[1] = -(pos).step,                                         \
    (pos).step_arr[2] = (pos).step,                                          \
    (pos).x = (_x),                                                          \
    (pos).currline = (origin) + (pos).step*(_y) )
  *)
procedure CV_INIT_PIXEL_POS(var pos: TCvPixelPosition8u; origin, _step: Integer; roi: TIplROI; _x, _y, orientation: Integer);
  inline; overload;
procedure CV_INIT_PIXEL_POS(var pos: TCvPixelPosition8s; origin, _step: Integer; roi: TIplROI; _x, _y, orientation: Integer);
  inline; overload;
procedure CV_INIT_PIXEL_POS(var pos: TCvPixelPosition32f; origin, _step: Integer; roi: TIplROI; _x, _y, orientation: Integer);
  inline; overload;

(* Move to specified point ( absolute shift ) *)
(* pos    - position structure *)
(* x, y   - coordinates of the new position *)
(* cs     - number of the image channels *)
// #define CV_MOVE_TO( pos, _x, _y, cs )                                                   \
// ((pos).currline = (_y) >= 0 && (_y) < (pos).height ? (pos).topline + ((_y)+1)*(pos).step : 0, \
// (pos).x = (_x) >= 0 && (_x) < (pos).width ? (_x) : 0, (pos).currline + (_x) * (cs) )

(* Get current coordinates *)
(* pos    - position structure *)
(* x, y   - coordinates of the new position *)
(* cs     - number of the image channels *)
// #define CV_GET_CURRENT( pos, cs )  ((pos).currline + (pos).x * (cs))

(* Move by one pixel relatively to current position *)
(* pos    - position structure *)
(* cs     - number of the image channels *)

(* left *)
// #define CV_MOVE_LEFT( pos, cs ) \
// ( --(pos).x >= 0 ? (pos).currline + (pos).x*(cs) : 0 )

(* right *)
// #define CV_MOVE_RIGHT( pos, cs ) \
// ( ++(pos).x < (pos).width ? (pos).currline + (pos).x*(cs) : 0 )

(* up *)
// #define CV_MOVE_UP( pos, cs ) \
// (((pos).currline -= (pos).step) != (pos).topline ? (pos).currline + (pos).x*(cs) : 0 )

(* down *)
// #define CV_MOVE_DOWN( pos, cs ) \
// (((pos).currline += (pos).step) != (pos).bottomline ? (pos).currline + (pos).x*(cs) : 0 )

(* left up *)
// #define CV_MOVE_LU( pos, cs ) ( CV_MOVE_LEFT(pos, cs), CV_MOVE_UP(pos, cs))

(* right up *)
// #define CV_MOVE_RU( pos, cs ) ( CV_MOVE_RIGHT(pos, cs), CV_MOVE_UP(pos, cs))

(* left down *)
// #define CV_MOVE_LD( pos, cs ) ( CV_MOVE_LEFT(pos, cs), CV_MOVE_DOWN(pos, cs))

(* right down *)
// #define CV_MOVE_RD( pos, cs ) ( CV_MOVE_RIGHT(pos, cs), CV_MOVE_DOWN(pos, cs))

(* Move by one pixel relatively to current position with wrapping when the position     */
  (* achieves image boundary                                                              */
  (*  pos    - position structure                                                         */
  (*  cs     - number of the image channels                                               */
  //
  (* left *)
// #define CV_MOVE_LEFT_WRAP( pos, cs ) \
// ((pos).currline + ( --(pos).x >= 0 ? (pos).x : ((pos).x = (pos).width-1))*(cs))
//
(* right *)
// #define CV_MOVE_RIGHT_WRAP( pos, cs ) \
// ((pos).currline + ( ++(pos).x < (pos).width ? (pos).x : ((pos).x = 0))*(cs) )
//
(* up *)
// #define CV_MOVE_UP_WRAP( pos, cs ) \
// ((((pos).currline -= (pos).step) != (pos).topline ? \
// (pos).currline : ((pos).currline = (pos).bottomline - (pos).step)) + (pos).x*(cs) )
//
(* down *)
// #define CV_MOVE_DOWN_WRAP( pos, cs ) \
// ((((pos).currline += (pos).step) != (pos).bottomline ? \
// (pos).currline : ((pos).currline = (pos).topline + (pos).step)) + (pos).x*(cs) )

(* left up *)
// #define CV_MOVE_LU_WRAP( pos, cs ) ( CV_MOVE_LEFT_WRAP(pos, cs), CV_MOVE_UP_WRAP(pos, cs))
(* right up *)
// #define CV_MOVE_RU_WRAP( pos, cs ) ( CV_MOVE_RIGHT_WRAP(pos, cs), CV_MOVE_UP_WRAP(pos, cs))
(* left down *)
// #define CV_MOVE_LD_WRAP( pos, cs ) ( CV_MOVE_LEFT_WRAP(pos, cs), CV_MOVE_DOWN_WRAP(pos, cs))
(* right down *)
// #define CV_MOVE_RD_WRAP( pos, cs ) ( CV_MOVE_RIGHT_WRAP(pos, cs), CV_MOVE_DOWN_WRAP(pos, cs))

const
  (* Numeric constants which used for moving in arbitrary direction *)
  CV_SHIFT_NONE = 2;
  CV_SHIFT_LEFT = 1;
  CV_SHIFT_RIGHT = 3;
  CV_SHIFT_UP = 6;
  CV_SHIFT_DOWN = 10;
  CV_SHIFT_LU = 5;
  CV_SHIFT_RU = 7;
  CV_SHIFT_LD = 9;
  CV_SHIFT_RD = 11;

  (* Move by one pixel in specified direction *)
  (* pos    - position structure *)
  (* shift  - direction ( it's value must be one of the CV_SHIFT_Ö constants ) *)
  (* cs     - number of the image channels *)
  // #define CV_MOVE_PARAM( pos, shift, cs )                                             \
  // ( (pos).currline += (pos).step_arr[(shift)>>2], (pos).x += ((shift)&3)-2,       \
  // ((pos).currline != (pos).topline && (pos).currline != (pos).bottomline &&       \
  // (pos).x >= 0 && (pos).x < (pos).width) ? (pos).currline + (pos).x*(cs) : 0 )

  (* Move by one pixel in specified direction with wrapping when the *)
  (* position achieves image boundary *)
  (* pos    - position structure *)
  (* shift  - direction ( it's value must be one of the CV_SHIFT_Ö constants ) *)
  (* cs     - number of the image channels *)
  // #define CV_MOVE_PARAM_WRAP( pos, shift, cs )                                        \
  // ( (pos).currline += (pos).step_arr[(shift)>>2],                                 \
  // (pos).currline = ((pos).currline == (pos).topline ?                             \
  // (pos).bottomline - (pos).step :                                                 \
  // (pos).currline == (pos).bottomline ?                                            \
  // (pos).topline + (pos).step : (pos).currline),                                   \
  // \
  // (pos).x += ((shift)&3)-2,                                                       \
  // (pos).x = ((pos).x < 0 ? (pos).width-1 : (pos).x >= (pos).width ? 0 : (pos).x), \
  // \
  // (pos).currline + (pos).x*(cs) )

Type
  // typedef float*   CvVect32f;
  TCvVect32f = PSingle;
  // typedef float*   CvMatr32f;
  TCvMatr32f = PSingle;
  // typedef double*  CvVect64d;
  TCvVect64d = PDouble;
  // typedef double*  CvMatr64d;
  TCvMatr64d = PDouble;

  (*
    CV_EXPORTS void cvUnDistortOnce( const CvArr* src, CvArr* dst,
    const float* intrinsic_matrix,
    const float* distortion_coeffs,
    int interpolate );
  *)
{$IFDEF SAFELOADLIB}

type
  TcvUnDistortOnce = procedure(const src: PCvArr; dst: PCvArr; const intrinsic_matrix: PSingle; const distortion_coeffs: PSingle;
    interpolate: Integer); cdecl;

var
  cvUnDistortOnce: TcvUnDistortOnce;

{$ELSE}
procedure cvUnDistortOnce(const src: PCvArr; dst: PCvArr; const intrinsic_matrix: PSingle; const distortion_coeffs: PSingle;
  interpolate: Integer); cdecl;
{$ENDIF}
(*
  the two functions below have quite hackerish implementations, use with care
  (or, which is better, switch to cvUndistortInitMap and cvRemap instead

  CV_EXPORTS void cvUnDistortInit( const CvArr* src,
  CvArr* undistortion_map,
  const float* A, const float* k,
  int interpolate );

  CV_EXPORTS void  cvUnDistort( const CvArr* src, CvArr* dst,
  const CvArr* undistortion_map,
  int interpolate );
*)
{$IFDEF SAFELOADLIB}

type
  TcvUnDistortInit = procedure(const src: PCvArr; undistortion_map: PCvArr; const A: PSingle; const k: PSingle;
    interpolate: Integer); cdecl;
  TcvUnDistort = procedure(const src: PCvArr; dst: PCvArr; const undistortion_map: PCvArr; interpolate: Integer); cdecl;

var
  cvUnDistortInit: TcvUnDistortInit;
  cvUnDistort: TcvUnDistort;

{$ELSE}
procedure cvUnDistortInit(const src: PCvArr; undistortion_map: PCvArr; const A: PSingle; const k: PSingle; interpolate: Integer); cdecl;
procedure cvUnDistort(const src: PCvArr; dst: PCvArr; const undistortion_map: PCvArr; interpolate: Integer); cdecl;
{$ENDIF}
(*
  Find fundamental matrix

  CV_EXPORTS void  cvFindFundamentalMatrix( int* points1, int* points2,
  int numpoints, int method, float* matrix );
*)
{$IFDEF SAFELOADLIB}

type
  TcvFindFundamentalMatrix = procedure(Var points1: Integer; Var points2: Integer; numpoints: Integer; method: Integer;
    Var matrix: Single); cdecl;

var
  cvFindFundamentalMatrix: TcvFindFundamentalMatrix;
{$ELSE}
procedure cvFindFundamentalMatrix(Var points1: Integer; Var points2: Integer; numpoints: Integer; method: Integer;
  Var matrix: Single); cdecl;
{$ENDIF}
(*
  CV_EXPORTS int cvFindChessBoardCornerGuesses( const void* arr, void* thresharr,
  CvMemStorage* storage,
  CvSize pattern_size, CvPoint2D32f * corners,
  int *corner_count );
*)
{$IFDEF SAFELOADLIB}

type
  TcvFindChessBoardCornerGuesses = function(const arr: Pointer; thresharr: Pointer; storage: PCvMemStorage; pattern_size: TCvSize;
    corners: PCvPoint2D32f; corner_count: pInteger): Integer; cdecl;

var
  cvFindChessBoardCornerGuesses: TcvFindChessBoardCornerGuesses;

{$ELSE}
function cvFindChessBoardCornerGuesses(const arr: Pointer; thresharr: Pointer; storage: PCvMemStorage; pattern_size: TCvSize;
  corners: PCvPoint2D32f; corner_count: pInteger): Integer; cdecl;
{$ENDIF}
(*
  Calibrates camera using multiple views of calibration pattern

  CV_EXPORTS void cvCalibrateCamera( int image_count, int* _point_counts,
  CvSize image_size, CvPoint2D32f* _image_points, CvPoint3D32f* _object_points,
  float* _distortion_coeffs, float* _camera_matrix, float* _translation_vectors,
  float* _rotation_matrices, int flags );
*)
{$IFDEF SAFELOADLIB}

type
  TcvCalibrateCamera = procedure(image_count: Integer; Var _point_counts: Integer; image_size: TCvSize; _image_points: PCvPoint2D32f;
    _object_points: PCvPoint3D32f; _distortion_coeffs: PSingle; _camera_matrix: PSingle; _translation_vectors: PSingle;
    _rotation_matrices: PSingle; flags: Integer); cdecl;

var
  cvCalibrateCamera: TcvCalibrateCamera;
{$ELSE}
procedure cvCalibrateCamera(image_count: Integer; Var _point_counts: Integer; image_size: TCvSize; _image_points: PCvPoint2D32f;
  _object_points: PCvPoint3D32f; _distortion_coeffs: PSingle; _camera_matrix: PSingle; _translation_vectors: PSingle;
  _rotation_matrices: PSingle; flags: Integer); cdecl;
{$ENDIF}
(*
  CV_EXPORTS void cvCalibrateCamera_64d( int image_count, int* _point_counts,
  CvSize image_size, CvPoint2D64f* _image_points, CvPoint3D64f* _object_points,
  double* _distortion_coeffs, double* _camera_matrix, double* _translation_vectors,
  double* _rotation_matrices, int flags );
*)
{$IFDEF SAFELOADLIB}

type
  TcvCalibrateCamera_64d = procedure(image_count: Integer; Var _point_counts: Integer; image_size: TCvSize; _image_points: PCvPoint2D64f;
    _object_points: PCvPoint3D64f; _distortion_coeffs: PDouble; _camera_matrix: PDouble; _translation_vectors: PDouble;
    _rotation_matrices: PDouble; flags: Integer); cdecl;

var
  cvCalibrateCamera_64d: TcvCalibrateCamera_64d;
{$ELSE}
procedure cvCalibrateCamera_64d(image_count: Integer; Var _point_counts: Integer; image_size: TCvSize; _image_points: PCvPoint2D64f;
  _object_points: PCvPoint3D64f; _distortion_coeffs: PDouble; _camera_matrix: PDouble; _translation_vectors: PDouble;
  _rotation_matrices: PDouble; flags: Integer); cdecl;
{$ENDIF}
(*
  Find 3d position of object given intrinsic camera parameters,
  3d model of the object and projection of the object into view plane

  CV_EXPORTS void cvFindExtrinsicCameraParams( int point_count,
  CvSize image_size, CvPoint2D32f* _image_points,
  CvPoint3D32f* _object_points, float* focal_length,
  CvPoint2D32f principal_point, float* _distortion_coeffs,
  float* _rotation_vector, float* _translation_vector );
*)
{$IFDEF SAFELOADLIB}

type
  TcvFindExtrinsicCameraParams = procedure(point_count: Integer; image_size: TCvSize; _image_points: PCvPoint2D32f;
    _object_points: PCvPoint3D32f; focal_length: PSingle; principal_point: TCvPoint2D32f; _distortion_coeffs: PSingle;
    _rotation_vector: PSingle; _translation_vector: PSingle); cdecl;

var
  cvFindExtrinsicCameraParams: TcvFindExtrinsicCameraParams;

{$ELSE}
procedure cvFindExtrinsicCameraParams(point_count: Integer; image_size: TCvSize; _image_points: PCvPoint2D32f;
  _object_points: PCvPoint3D32f; focal_length: PSingle; principal_point: TCvPoint2D32f; _distortion_coeffs: PSingle;
  _rotation_vector: PSingle; _translation_vector: PSingle); cdecl;
{$ENDIF}
(*
  Variant of the previous function that takes double-precision parameters

  CV_EXPORTS void cvFindExtrinsicCameraParams_64d( int point_count,
  CvSize image_size, CvPoint2D64f* _image_points,
  CvPoint3D64f* _object_points, double* focal_length,
  CvPoint2D64f principal_point, double* _distortion_coeffs,
  double* _rotation_vector, double* _translation_vector );
*)
{$IFDEF SAFELOADLIB}

type
  TcvFindExtrinsicCameraParams_64d = procedure(point_count: Integer; image_size: TCvSize; _image_points: PCvPoint2D64f;
    _object_points: PCvPoint3D64f; focal_length: PDouble; principal_point: TCvPoint2D64f; _distortion_coeffs: PDouble;
    _rotation_vector: PDouble; _translation_vector: PDouble); cdecl;

var
  cvFindExtrinsicCameraParams_64d: TcvFindExtrinsicCameraParams_64d;
{$ELSE}
procedure cvFindExtrinsicCameraParams_64d(point_count: Integer; image_size: TCvSize; _image_points: PCvPoint2D64f;
  _object_points: PCvPoint3D64f; focal_length: PDouble; principal_point: TCvPoint2D64f; _distortion_coeffs: PDouble;
  _rotation_vector: PDouble; _translation_vector: PDouble); cdecl;
{$ENDIF}

const
  (* Rodrigues transform *)
  CV_RODRIGUES_M2V = 0;
  CV_RODRIGUES_V2M = 1;

  (*
    Converts rotation_matrix matrix to rotation_matrix vector or vice versa

    CV_EXPORTS void  cvRodrigues( CvMat* rotation_matrix, CvMat* rotation_vector,
    CvMat* jacobian, int conv_type );
  *)
{$IFDEF SAFELOADLIB}

type
  TcvRodrigues = procedure(rotation_matrix: PCvMat; rotation_vector: PCvMat; jacobian: PCvMat; conv_type: Integer); cdecl;

var
  cvRodrigues: TcvRodrigues;
{$ELSE}
procedure cvRodrigues(rotation_matrix: PCvMat; rotation_vector: PCvMat; jacobian: PCvMat; conv_type: Integer); cdecl;
{$ENDIF}
(*
  Does reprojection of 3d object points to the view plane

  CV_EXPORTS void  cvProjectPoints( int point_count, CvPoint3D64f* _object_points,
  double* _rotation_vector, double*  _translation_vector,
  double* focal_length, CvPoint2D64f principal_point,
  double* _distortion, CvPoint2D64f* _image_points,
  double* _deriv_points_rotation_matrix,
  double* _deriv_points_translation_vect,
  double* _deriv_points_focal,
  double* _deriv_points_principal_point,
  double* _deriv_points_distortion_coeffs );
*)
{$IFDEF SAFELOADLIB}

type
  TcvProjectPoints = procedure(point_count: Integer; _object_points: PCvPoint3D64f; _rotation_vector: PDouble; _translation_vector: PDouble;
    focal_length: PDouble; principal_point: TCvPoint2D64f; _distortion: PDouble; _image_points: PCvPoint2D64f;
    _deriv_points_rotation_matrix: PDouble; _deriv_points_translation_vect: PDouble; _deriv_points_focal: PDouble;
    _deriv_points_principal_point: PDouble; _deriv_points_distortion_coeffs: PDouble); cdecl;

var
  cvProjectPoints: TcvProjectPoints;
{$ELSE}
procedure cvProjectPoints(point_count: Integer; _object_points: PCvPoint3D64f; _rotation_vector: PDouble; _translation_vector: PDouble;
  focal_length: PDouble; principal_point: TCvPoint2D64f; _distortion: PDouble; _image_points: PCvPoint2D64f;
  _deriv_points_rotation_matrix: PDouble; _deriv_points_translation_vect: PDouble; _deriv_points_focal: PDouble;
  _deriv_points_principal_point: PDouble; _deriv_points_distortion_coeffs: PDouble); cdecl;
{$ENDIF}
(*
  Simpler version of the previous function

  CV_EXPORTS void  cvProjectPointsSimple( int point_count, CvPoint3D64f* _object_points,
  double* _rotation_matrix, double*  _translation_vector,
  double* _camera_matrix, double* _distortion, CvPoint2D64f* _image_points );
*)
{$IFDEF SAFELOADLIB}

type
  TcvProjectPointsSimple = procedure(point_count: Integer; _object_points: PCvPoint3D64f; _rotation_matrix: PDouble;
    _translation_vector: PDouble; _camera_matrix: PDouble; _distortion: PDouble; _image_points: PCvPoint2D64f); cdecl;

var
  cvProjectPointsSimple: TcvProjectPointsSimple;
{$ELSE}
procedure cvProjectPointsSimple(point_count: Integer; _object_points: PCvPoint3D64f; _rotation_matrix: PDouble;
  _translation_vector: PDouble; _camera_matrix: PDouble; _distortion: PDouble; _image_points: PCvPoint2D64f); cdecl;
{$ENDIF}
// #define cvMake2DPoints cvConvertPointsHomogeneous
// #define cvMake3DPoints cvConvertPointsHomogeneous
//
// #define cvWarpPerspectiveQMatrix cvGetPerspectiveTransform
//
// #define cvConvertPointsHomogenious cvConvertPointsHomogeneous

/// /////////////////////////////////// feature extractors: obsolete API //////////////////////////////////

type
  pCvSURFPoint = ^TCvSURFPoint;

  TCvSURFPoint = record
    pt: TCvPoint2D32f;
    laplacian: Integer;
    size: Integer;
    dir: Single;
    hessian: Single;
  end;

  (*
    // CV_INLINE CvSURFPoint cvSURFPoint( CvPoint2D32f pt, int laplacian,
    // int size, float dir CV_DEFAULT(0),
    // float hessian CV_DEFAULT(0))
    // {
    // CvSURFPoint kp;
    //
    // kp.pt        = pt;
    // kp.laplacian = laplacian;
    // kp.size      = size;
    // kp.dir       = dir;
    // kp.hessian   = hessian;
    //
    // return kp;
    // }
  *)

function cvSURFPoint(pt: TCvPoint2D32f; laplacian: Integer; size: Integer; dir: Single = 0; hessian: Single = 0): TCvSURFPoint; inline;

type
  pCvSURFParams = ^TCvSURFParams;

  TCvSURFParams = record
    extended: Integer;
    upright: Integer;
    hessianThreshold: Double;

    nOctaves: Integer;
    nOctaveLayers: Integer;
  end;

  // CVAPI(CvSURFParams) cvSURFParams( double hessianThreshold, int extended CV_DEFAULT(0) );
{$IFDEF SAFELOADLIB}

type
  TcvSURFParams_fun = function(hessianThreshold: Double; _extended: Integer = 0): TCvSURFParams; cdecl;

var
  cvSURFParams: TcvSURFParams_fun;

{$ELSE}
function cvSURFParams(hessianThreshold: Double; _extended: Integer = 0): TCvSURFParams; cdecl;
{$ENDIF}
(*
  If useProvidedKeyPts!=0, keypoints are not detected, but descriptors are computed
  at the locations provided in keypoints (a CvSeq of CvSURFPoint).

  CVAPI(void) cvExtractSURF( const CvArr* img, const CvArr* mask,
  CvSeq** keypoints, CvSeq** descriptors,
  CvMemStorage* storage, CvSURFParams params,
  int useProvidedKeyPts CV_DEFAULT(0)  );
*)
{$IFDEF SAFELOADLIB}

type
  TcvExtractSURF = procedure(const img: PCvArr; const mask: PCvArr; keypoints: ppCvSeq; descriptors: ppCvSeq; storage: PCvMemStorage;
    params: TCvSURFParams; useProvidedKeyPts: Integer = 0); cdecl;

var
  cvExtractSURF: TcvExtractSURF;
{$ELSE}
procedure cvExtractSURF(const img: PCvArr; const mask: PCvArr; keypoints: ppCvSeq; descriptors: ppCvSeq; storage: PCvMemStorage;
  params: TCvSURFParams; useProvidedKeyPts: Integer = 0); cdecl;
{$ENDIF}

Type
  (* Maximal Stable Regions Parameters *)
  (* typedef struct CvMSERParams
    {
    //! delta, in the code, it compares (size_{i}-size_{i-delta})/size_{i-delta}
    int delta;
    //! prune the area which bigger than maxArea
    int maxArea;
    //! prune the area which smaller than minArea
    int minArea;
    //! prune the area have simliar size to its children
    float maxVariation;
    //! trace back to cut off mser with diversity < min_diversity
    float minDiversity;

    /////// the next few params for MSER of color image

    //! for color image, the evolution steps
    int maxEvolution;
    //! the area threshold to cause re-initialize
    double areaThreshold;
    //! ignore too small margin
    double minMargin;
    //! the aperture size for edge blur
    int edgeBlurSize;
    } CvMSERParams;
  *)
  pCvMSERParams = ^TCvMSERParams;

  TCvMSERParams = record
    // ! delta, in the code, it compares (size_{i}-size_{i-delta})/size_{i-delta}
    delta: Integer;
    // ! prune the area which bigger than maxArea
    maxArea: Integer;
    // ! prune the area which smaller than minArea
    minArea: Integer;
    // ! prune the area have simliar size to its children
    maxVariation: Single;
    // ! trace back to cut off mser with diversity < min_diversity
    minDiversity: Single;
    /// //// the next few params for MSER of color image
    // ! for color image, the evolution steps
    maxEvolution: Integer;
    // ! the area threshold to cause re-initialize
    areaThreshold: Double;
    // ! ignore too small margin
    minMargin: Double;
    // ! the aperture size for edge blur
    edgeBlurSize: Integer;
  end;

  (*
    CVAPI(CvMSERParams) cvMSERParams( int delta CV_DEFAULT(5), int min_area CV_DEFAULT(60),
    int max_area CV_DEFAULT(14400), float max_variation CV_DEFAULT(.25f),
    float min_diversity CV_DEFAULT(.2f), int max_evolution CV_DEFAULT(200),
    double area_threshold CV_DEFAULT(1.01),
    double min_margin CV_DEFAULT(.003),
    int edge_blur_size CV_DEFAULT(5) );
  *)
{$IFDEF SAFELOADLIB}

type
  TCvMSERParams_fun = function(delta: Integer = 5; min_area: Integer = 60; max_area: Integer = 14400; max_variation: float = 0.25;
    min_diversity: float = 0.2; max_evolution: Integer = 200; area_threshold: Double = 1.01; min_margin: Double = 0.003;
    edge_blur_size: Integer = 5): TCvMSERParams; cdecl;

var
  cvMSERParams: TCvMSERParams_fun;
{$ELSE}
  // function cvMSERParams(delta: Integer = 5; min_area: Integer = 60; max_area: Integer = 14400; max_variation: float = 0.25; min_diversity: float = 0.2;
  // max_evolution: Integer = 200; area_threshold: Double = 1.01; min_margin: Double = 0.003; edge_blur_size: Integer = 5): TCvMSERParams; cdecl;
{$ENDIF}
  (*
    Extracts the contours of Maximally Stable Extremal Regions

    CVAPI(void) cvExtractMSER( CvArr* _img, CvArr* _mask, CvSeq** contours, CvMemStorage* storage, CvMSERParams params );
  *)
{$IFDEF SAFELOADLIB}

type
  TcvExtractMSER = procedure(_img: PCvArr; _mask: PCvArr; Var contours: pCvSeq; storage: PCvMemStorage; params: TCvMSERParams); cdecl;

var
  cvExtractMSER: TcvExtractMSER;
{$ELSE}
  // procedure cvExtractMSER(_img: PCvArr; _mask: PCvArr; Var contours: pCvSeq; storage: PCvMemStorage; params: TCvMSERParams); cdecl;
{$ENDIF}

Type
  (*
    typedef struct CvStarKeypoint
    {
    CvPoint pt;
    int size;
    float response;
    } CvStarKeypoint;
  *)
  TCvStarKeypoint = record
    pt: TCvPoint;
    size: Integer;
    response: Single;
  end;

  (*
    CV_INLINE CvStarKeypoint cvStarKeypoint(CvPoint pt, int size, float response)
    {
    CvStarKeypoint kpt;
    kpt.pt = pt;
    kpt.size = size;
    kpt.response = response;
    return kpt;
    }
  *)
function cvStarKeypoint(pt: TCvPoint; size: Integer; response: Single): TCvStarKeypoint; inline;

(* typedef struct CvStarDetectorParams
  {
  int maxSize;
  int responseThreshold;
  int lineThresholdProjected;
  int lineThresholdBinarized;
  int suppressNonmaxSize;
  } CvStarDetectorParams;
*)

Type
  pCvStarDetectorParams = ^TCvStarDetectorParams;

  TCvStarDetectorParams = record
    maxSize: Integer;
    responseThreshold: Integer;
    lineThresholdProjected: Integer;
    lineThresholdBinarized: Integer;
    suppressNonmaxSize: Integer;
  end;

  (*
    CV_INLINE CvStarDetectorParams cvStarDetectorParams(
    int maxSize CV_DEFAULT(45),
    int responseThreshold CV_DEFAULT(30),
    int lineThresholdProjected CV_DEFAULT(10),
    int lineThresholdBinarized CV_DEFAULT(8),
    int suppressNonmaxSize CV_DEFAULT(5))
    {
    CvStarDetectorParams params;
    params.maxSize = maxSize;
    params.responseThreshold = responseThreshold;
    params.lineThresholdProjected = lineThresholdProjected;
    params.lineThresholdBinarized = lineThresholdBinarized;
    params.suppressNonmaxSize = suppressNonmaxSize;

    return params;
    }
  *)
function cvStarDetectorParams(maxSize: Integer = 45; responseThreshold: Integer = 30; lineThresholdProjected: Integer = 10;
  lineThresholdBinarized: Integer = 8; suppressNonmaxSize: Integer = 5): TCvStarDetectorParams; inline;

(*
  CVAPI(CvSeq* ) cvGetStarKeypoints( const CvArr* img, CvMemStorage* storage,
  CvStarDetectorParams params CV_DEFAULT(cvStarDetectorParams()));
*)
{$IFDEF SAFELOADLIB}

type
  TcvGetStarKeypoints = function(const img: PCvArr; storage: PCvMemStorage;
    params: TCvStarDetectorParams { = CV_DEFAULT(cvStarDetectorParams()) } ): pCvSeq; cdecl;

var
  cvGetStarKeypoints: TcvGetStarKeypoints;
{$ELSE}
function cvGetStarKeypoints(const img: PCvArr; storage: PCvMemStorage;
  params: TCvStarDetectorParams { = CV_DEFAULT(cvStarDetectorParams()) } ): pCvSeq; cdecl;
{$ENDIF}
{$IF DEFINED(SAFELOADLIB) AND DEFINED(DEBUG)}
procedure Init_opencv_legacy_lib;
{$ENDIF}

implementation

uses ocv.lib;

const
  compat_lib = legacy_lib;

procedure CV_INIT_PIXEL_POS(var pos: TCvPixelPosition8u; origin, _step: Integer; roi: TIplROI; _x, _y, orientation: Integer);
begin
  pos.step := _step div sizeof(pos.currline[0]);
  if orientation <> 0 then
    pos.step := -pos.step;
  pos.width := roi.width;
  pos.height := roi.height;
  pos.bottomline := PUChar(origin + pos.step * pos.height);
  pos.topline := PUChar(origin - pos.step);
  pos.step_arr[0] := 0;
  pos.step_arr[1] := -pos.step;
  pos.step_arr[2] := pos.step;
  pos.x := _x;
  pos.currline := PUChar(origin + pos.step * _y);
end;

procedure CV_INIT_PIXEL_POS(var pos: TCvPixelPosition8s; origin, _step: Integer; roi: TIplROI; _x, _y, orientation: Integer);
begin
  pos.step := _step div sizeof(pos.currline[0]);
  if orientation <> 0 then
    pos.step := -pos.step;
  pos.width := roi.width;
  pos.height := roi.height;
  pos.bottomline := Pschar(origin + pos.step * pos.height);
  pos.topline := Pschar(origin - pos.step);
  pos.step_arr[0] := 0;
  pos.step_arr[1] := -pos.step;
  pos.step_arr[2] := pos.step;
  pos.x := _x;
  pos.currline := Pschar(origin + pos.step * _y);
end;

procedure CV_INIT_PIXEL_POS(var pos: TCvPixelPosition32f; origin, _step: Integer; roi: TIplROI; _x, _y, orientation: Integer);
begin
  pos.step := _step div sizeof(pos.currline[0]);
  if orientation <> 0 then
    pos.step := -pos.step;
  pos.width := roi.width;
  pos.height := roi.height;
  pos.bottomline := PSingle(origin + pos.step * pos.height);
  pos.topline := PSingle(origin - pos.step);
  pos.step_arr[0] := 0;
  pos.step_arr[1] := -pos.step;
  pos.step_arr[2] := pos.step;
  pos.x := _x;
  pos.currline := PSingle(origin + pos.step * _y);
end;

function cvSURFPoint(pt: TCvPoint2D32f; laplacian: Integer; size: Integer; dir: Single = 0; hessian: Single = 0): TCvSURFPoint; inline;
begin
  Result.pt := pt;
  Result.laplacian := laplacian;
  Result.size := size;
  Result.dir := dir;
  Result.hessian := hessian;
end;

function cvStarKeypoint(pt: TCvPoint; size: Integer; response: Single): TCvStarKeypoint;
begin
  Result.pt := pt;
  Result.size := size;
  Result.response := response;
end;

function cvStarDetectorParams(maxSize: Integer = 45; responseThreshold: Integer = 30; lineThresholdProjected: Integer = 10;
  lineThresholdBinarized: Integer = 8; suppressNonmaxSize: Integer = 5): TCvStarDetectorParams; inline;
begin
  Result.maxSize := maxSize;
  Result.responseThreshold := responseThreshold;
  Result.lineThresholdProjected := lineThresholdProjected;
  Result.lineThresholdBinarized := lineThresholdBinarized;
  Result.suppressNonmaxSize := suppressNonmaxSize;
end;

{$IFDEF SAFELOADLIB}

Var
  compatDLL: Cardinal;

procedure Init_opencv_legacy_lib;
begin
  compatDLL := ocvLoadLibrary(compat_lib);
  Assert(compatDLL <> 0, 'Can not init ' + compat_lib);

  cvMatArray := ocvGetProcAddress('cvMatArray', compatDLL);
  cvMean := ocvGetProcAddress('cvMean', compatDLL);
  cvSumPixels := ocvGetProcAddress('cvSumPixels', compatDLL);
  cvMean_StdDev := ocvGetProcAddress('cvMean_StdDev', compatDLL);
  cvmPerspectiveProject := ocvGetProcAddress('cvmPerspectiveProject', compatDLL);
  cvFillImage := ocvGetProcAddress('cvFillImage', compatDLL);
  cvRandSetRange := ocvGetProcAddress('cvRandSetRange', compatDLL);
  cvRandInit := ocvGetProcAddress('cvRandInit', compatDLL);
  cvRand := ocvGetProcAddress('cvRand', compatDLL);
  cvbRand := ocvGetProcAddress('cvbRand', compatDLL);
  cvbCartToPolar := ocvGetProcAddress('cvbCartToPolar', compatDLL);
  cvbFastArctan := ocvGetProcAddress('cvbFastArctan', compatDLL);
  cvbSqrt := ocvGetProcAddress('cvbSqrt', compatDLL);
  cvbInvSqrt := ocvGetProcAddress('cvbInvSqrt', compatDLL);
  cvbReciprocal := ocvGetProcAddress('cvbReciprocal', compatDLL);
  cvbFastExp := ocvGetProcAddress('cvbFastExp', compatDLL);
  cvbFastLog := ocvGetProcAddress('cvbFastLog', compatDLL);
  cvContourBoundingRect := ocvGetProcAddress('cvContourBoundingRect', compatDLL);
  cvPseudoInverse := ocvGetProcAddress('cvPseudoInverse', compatDLL);
  cvConvexHull := ocvGetProcAddress('cvConvexHull', compatDLL);
  cvMinAreaRect := ocvGetProcAddress('cvMinAreaRect', compatDLL);
  cvFitLine3D := ocvGetProcAddress('cvFitLine3D', compatDLL);
  cvFitLine2D := ocvGetProcAddress('cvFitLine2D', compatDLL);
  cvFitEllipse := ocvGetProcAddress('cvFitEllipse', compatDLL);
  cvProject3D := ocvGetProcAddress('cvProject3D', compatDLL);
  cvHoughLines := ocvGetProcAddress('cvHoughLines', compatDLL);
  cvHoughLinesP := ocvGetProcAddress('cvHoughLinesP', compatDLL);
  cvHoughLinesSDiv := ocvGetProcAddress('cvHoughLinesSDiv', compatDLL);
  cvCalcEMD := ocvGetProcAddress('cvCalcEMD', compatDLL);
  cvKMeans := ocvGetProcAddress('cvKMeans', compatDLL);
  cvStartScanGraph := ocvGetProcAddress('cvStartScanGraph', compatDLL);
  cvEndScanGraph := ocvGetProcAddress('cvEndScanGraph', compatDLL);
  cvLineAA := ocvGetProcAddress('cvLineAA', compatDLL);
  cvCircleAA := ocvGetProcAddress('cvCircleAA', compatDLL);
  cvEllipseAA := ocvGetProcAddress('cvEllipseAA', compatDLL);
  cvPolyLineAA := ocvGetProcAddress('cvPolyLineAA', compatDLL);
  cvUnDistortOnce := ocvGetProcAddress('cvUnDistortOnce', compatDLL);
  cvUnDistortInit := ocvGetProcAddress('cvUnDistortInit', compatDLL);
  cvUnDistort := ocvGetProcAddress('cvUnDistort', compatDLL);
  cvFindFundamentalMatrix := ocvGetProcAddress('cvFindFundamentalMatrix', compatDLL);
  cvFindChessBoardCornerGuesses := ocvGetProcAddress('cvFindChessBoardCornerGuesses', compatDLL);
  cvCalibrateCamera := ocvGetProcAddress('cvCalibrateCamera', compatDLL);
  cvCalibrateCamera_64d := ocvGetProcAddress('cvCalibrateCamera_64d', compatDLL);
  cvFindExtrinsicCameraParams := ocvGetProcAddress('cvFindExtrinsicCameraParams', compatDLL);
  cvFindExtrinsicCameraParams_64d := ocvGetProcAddress('cvFindExtrinsicCameraParams_64d', compatDLL);
  cvRodrigues := ocvGetProcAddress('cvRodrigues', compatDLL);
  cvProjectPoints := ocvGetProcAddress('cvProjectPoints', compatDLL);
  cvProjectPointsSimple := ocvGetProcAddress('cvProjectPointsSimple', compatDLL);
  cvSURFParams := ocvGetProcAddress('cvSURFParams', compatDLL);
  cvExtractSURF := ocvGetProcAddress('cvExtractSURF', compatDLL);
  // cvMSERParams := ocvGetProcAddress('cvMSERParams', compatDLL);
  // cvExtractMSER := ocvGetProcAddress('cvExtractMSER', compatDLL);
  cvGetStarKeypoints := ocvGetProcAddress('cvGetStarKeypoints', compatDLL);

end;
{$ELSE}
function cvMatArray; external legacy_lib;
function cvMean; external legacy_lib;
function cvSumPixels; external legacy_lib;
procedure cvMean_StdDev; external legacy_lib;
procedure cvmPerspectiveProject; external legacy_lib;
procedure cvFillImage; external legacy_lib;
procedure cvRandSetRange; external legacy_lib;
procedure cvRandInit; external legacy_lib;
procedure cvRand; external legacy_lib;
procedure cvbRand; external legacy_lib;
procedure cvbCartToPolar; external legacy_lib;
procedure cvbFastArctan; external legacy_lib;
procedure cvbSqrt; external legacy_lib;
procedure cvbInvSqrt; external legacy_lib;
procedure cvbReciprocal; external legacy_lib;
procedure cvbFastExp; external legacy_lib;
procedure cvbFastLog; external legacy_lib;
function cvContourBoundingRect; external legacy_lib;
function cvPseudoInverse; external legacy_lib;
procedure cvConvexHull; external legacy_lib;
procedure cvMinAreaRect; external legacy_lib;
procedure cvFitLine3D; external legacy_lib;
procedure cvFitLine2D; external legacy_lib;
procedure cvFitEllipse; external legacy_lib;
procedure cvProject3D; external legacy_lib;
function cvHoughLines; external legacy_lib;
function cvHoughLinesP; external legacy_lib;
function cvHoughLinesSDiv; external legacy_lib;
function cvCalcEMD; external legacy_lib;
procedure cvKMeans; external legacy_lib;
procedure cvStartScanGraph; external legacy_lib;
procedure cvEndScanGraph; external legacy_lib;
procedure cvLineAA; external legacy_lib;
procedure cvCircleAA; external legacy_lib;
procedure cvEllipseAA; external legacy_lib;
procedure cvPolyLineAA; external legacy_lib;
procedure cvUnDistortOnce; external legacy_lib;
procedure cvUnDistortInit; external legacy_lib;
procedure cvUnDistort; external legacy_lib;
procedure cvFindFundamentalMatrix; external legacy_lib;
function cvFindChessBoardCornerGuesses; external legacy_lib;
procedure cvCalibrateCamera; external legacy_lib;
procedure cvCalibrateCamera_64d; external legacy_lib;
procedure cvFindExtrinsicCameraParams; external legacy_lib;
procedure cvFindExtrinsicCameraParams_64d; external legacy_lib;
procedure cvRodrigues; external legacy_lib;
procedure cvProjectPoints; external legacy_lib;
procedure cvProjectPointsSimple; external legacy_lib;
function cvSURFParams; external legacy_lib;
procedure cvExtractSURF; external legacy_lib;
// function cvMSERParams; external legacy_lib;
// procedure cvExtractMSER; external legacy_lib;
function cvGetStarKeypoints; external legacy_lib;
{$ENDIF}

initialization

CV_MAT32F := CV_32FC1;
CV_MAT3x1_32F := CV_32FC1;
CV_MAT4x1_32F := CV_32FC1;
CV_MAT3x3_32F := CV_32FC1;
CV_MAT4x4_32F := CV_32FC1;

CV_MAT64D := CV_64FC1;
CV_MAT3x1_64D := CV_64FC1;
CV_MAT4x1_64D := CV_64FC1;
CV_MAT3x3_64D := CV_64FC1;
CV_MAT4x4_64D := CV_64FC1;

{$IFDEF SAFELOADLIB}
Init_opencv_legacy_lib;
{$ENDIF}

end.
