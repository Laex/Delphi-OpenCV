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
  opencv\modules\calib3d\include\opencv2\calib3d\calib3d_c.h
  *************************************************************************************************
*)

unit ocv.calib3d_c;

{$I OpenCV.inc}

interface

uses
  ocv.core.types_c,
  ocv.compat;

(* ***************************************************************************************
  *                      Camera Calibration, Pose Estimation and Stereo                  *
  **************************************************************************************** *)
Type
  // typedef struct CvPOSITObject CvPOSITObject;
  PCvPOSITObject = ^TCvPOSITObject;

  TCvPOSITObject = record
    N: Integer;
    inv_matr: PSingle;
    obj_vecs: PSingle;
    img_vecs: PSingle;
  end;

  (*
    Allocates and initializes CvPOSITObject structure before doing cvPOSIT
    CVAPI(CvPOSITObject* )  cvCreatePOSITObject( CvPoint3D32f* points, int point_count );
  *)

{$IFDEF SAFELOADLIB}

Type
  TcvCreatePOSITObject = function(points: pCvPoint3D32f; point_count: Integer): PCvPOSITObject; cdecl;

var
  cvCreatePOSITObject: TcvCreatePOSITObject;
{$ELSE}
function cvCreatePOSITObject(points: pCvPoint3D32f; point_count: Integer): PCvPOSITObject; cdecl;
{$ENDIF}
(* Runs POSIT (POSe from ITeration) algorithm for determining 3d position of
  an object given its model and projection in a weak-perspective case

  CVAPI(void)  cvPOSIT(  CvPOSITObject* posit_object, CvPoint2D32f* image_points,
  double focal_length, CvTermCriteria criteria,
  float* rotation_matrix, float* translation_vector);
*)
{$IFDEF SAFELOADLIB}

Type
  TcvPOSIT = procedure(posit_object: PCvPOSITObject; imagePoints: pCvPoint2D32f; focal_length: double; criteria: TCvTermCriteria;
    rotation_matrix: TCvMatr32f; translation_vector: TCvVect32f); cdecl;

var
  cvPOSIT: TcvPOSIT;
{$ELSE}
procedure cvPOSIT(posit_object: PCvPOSITObject; imagePoints: pCvPoint2D32f; focal_length: double; criteria: TCvTermCriteria;
  rotation_matrix: TCvMatr32f; translation_vector: TCvVect32f); cdecl;
{$ENDIF}
(*
  Releases CvPOSITObject structure

  CVAPI(void)  cvReleasePOSITObject( CvPOSITObject**  posit_object );
*)
{$IFDEF SAFELOADLIB}

Type
  TcvReleasePOSITObject = procedure(Var posit_object: PCvPOSITObject); cdecl;

var
  cvReleasePOSITObject: TcvReleasePOSITObject;
{$ELSE}
procedure cvReleasePOSITObject(Var posit_object: PCvPOSITObject); cdecl;
{$ENDIF}
(* updates the number of RANSAC iterations

  CVAPI(int) cvRANSACUpdateNumIters( double p, double err_prob,
  int model_points, int max_iters );
*)
{$IFDEF SAFELOADLIB}

Type
  TcvRANSACUpdateNumIters = function(p: double; err_prob: double; model_points: Integer; max_iters: Integer): Integer; cdecl;

var
  cvRANSACUpdateNumIters: TcvRANSACUpdateNumIters;
{$ELSE}
function cvRANSACUpdateNumIters(p: double; err_prob: double; model_points: Integer; max_iters: Integer): Integer; cdecl;
{$ENDIF}
(* CVAPI(void) cvConvertPointsHomogeneous( const CvMat* src, CvMat* dst ); *)
{$IFDEF SAFELOADLIB}

type
  TcvConvertPointsHomogeneous = procedure(const src: pCvMat; dst: pCvMat); cdecl;

var
  cvConvertPointsHomogeneous: TcvConvertPointsHomogeneous;
{$ELSE}
procedure cvConvertPointsHomogeneous(const src: pCvMat; dst: pCvMat); cdecl;
{$ENDIF}

const
  (* Calculates fundamental matrix given a set of corresponding points *)
  CV_FM_7POINT = 1;
  CV_FM_8POINT = 2;

  CV_LMEDS = 4;
  CV_RANSAC = 8;

  CV_FM_LMEDS_ONLY = CV_LMEDS;
  CV_FM_RANSAC_ONLY = CV_RANSAC;
  CV_FM_LMEDS = CV_LMEDS;
  CV_FM_RANSAC = CV_RANSAC;

  CV_ITERATIVE = 0;
  CV_EPNP = 1; // F.Moreno-Noguer, V.Lepetit and P.Fua "EPnP: Efficient Perspective-n-Point Camera Pose Estimation"
  CV_P3P = 2;
  // X.S. Gao, X.-R. Hou, J. Tang, H.-F. Chang; "Complete Solution Classification for the Perspective-Three-Point Problem"

  (*
    CVAPI(int) cvFindFundamentalMat( const CvMat* points1, const CvMat* points2,
    CvMat* fundamental_matrix,
    int method CV_DEFAULT(CV_FM_RANSAC),
    double param1 CV_DEFAULT(3.), double param2 CV_DEFAULT(0.99),
    CvMat* status CV_DEFAULT(NULL) );
  *)
{$IFDEF SAFELOADLIB}

Type
  TcvFindFundamentalMat = function(const points1: pCvMat; const points2: pCvMat; fundamental_matrix: pCvMat; method: Integer = CV_FM_RANSAC;
    param1: double = 3; param2: double = 0.99; status: pCvMat = nil): Integer; cdecl;

Var
  cvFindFundamentalMat: TcvFindFundamentalMat;
{$ELSE}
function cvFindFundamentalMat(const points1: pCvMat; const points2: pCvMat; fundamental_matrix: pCvMat; method: Integer = CV_FM_RANSAC;
  param1: double = 3; param2: double = 0.99; status: pCvMat = nil): Integer; cdecl;
{$ENDIF}
(*
  For each input point on one of images
  computes parameters of the corresponding
  epipolar line on the other image

  CVAPI(void) cvComputeCorrespondEpilines( const CvMat* points,
  int which_image,
  const CvMat* fundamental_matrix,
  CvMat* correspondent_lines );
*)
{$IFDEF SAFELOADLIB}

Type
  TcvComputeCorrespondEpilines = procedure(const points: pCvMat; which_image: Integer; const fundamental_matrix: pCvMat;
    correspondent_lines: pCvMat); cdecl;

var
  cvComputeCorrespondEpilines: TcvComputeCorrespondEpilines;
{$ELSE}
procedure cvComputeCorrespondEpilines(const points: pCvMat; which_image: Integer; const fundamental_matrix: pCvMat;
  correspondent_lines: pCvMat); cdecl;
{$ENDIF}
(*
  Triangulation functions

  CVAPI(void) cvTriangulatePoints(CvMat* projMatr1, CvMat* projMatr2,
  CvMat* projPoints1, CvMat* projPoints2,
  CvMat* points4D);
*)
{$IFDEF SAFELOADLIB}

Type
  TcvTriangulatePoints = procedure(projMatr1: pCvMat; projMatr2: pCvMat; projPoints1: pCvMat; projPoints2: pCvMat; points4D: pCvMat); cdecl;

var
  cvTriangulatePoints: TcvTriangulatePoints;
{$ELSE}
procedure cvTriangulatePoints(projMatr1: pCvMat; projMatr2: pCvMat; projPoints1: pCvMat; projPoints2: pCvMat; points4D: pCvMat); cdecl;
{$ENDIF}
(*
  CVAPI(void) cvCorrectMatches(CvMat* F, CvMat* points1, CvMat* points2,
  CvMat* new_points1, CvMat* new_points2);
*)
{$IFDEF SAFELOADLIB}

type
  TcvCorrectMatches = procedure(F: pCvMat; points1: pCvMat; points2: pCvMat; new_points1: pCvMat; new_points2: pCvMat); cdecl;

var
  cvCorrectMatches: TcvCorrectMatches;
{$ELSE}
procedure cvCorrectMatches(F: pCvMat; points1: pCvMat; points2: pCvMat; new_points1: pCvMat; new_points2: pCvMat); cdecl;
{$ENDIF}
(*
  Computes the optimal new camera matrix according to the free scaling parameter alpha:
  alpha=0 - only valid pixels will be retained in the undistorted image
  alpha=1 - all the source image pixels will be retained in the undistorted image

  CVAPI(void) cvGetOptimalNewCameraMatrix( const CvMat* camera_matrix,
  const CvMat* dist_coeffs,
  CvSize image_size, double alpha,
  CvMat* new_camera_matrix,
  CvSize new_imag_size CV_DEFAULT(cvSize(0,0)),
  CvRect* valid_pixel_ROI CV_DEFAULT(0),
  int center_principal_point CV_DEFAULT(0));
*)
{$IFDEF SAFELOADLIB}

Type
  TcvGetOptimalNewCameraMatrix = procedure(const camera_matrix: pCvMat; const dist_coeffs: pCvMat; image_size: TCvSize; alpha: double;
    new_camera_matrix: pCvMat; new_imag_size: TCvSize { = CV_DEFAULT(cvSize(0,0))) }; valid_pixel_ROI: PCvRect = nil;
    center_principal_point: Integer = 0); cdecl;

var
  cvGetOptimalNewCameraMatrix: TcvGetOptimalNewCameraMatrix;
{$ELSE}
procedure cvGetOptimalNewCameraMatrix(const camera_matrix: pCvMat; const dist_coeffs: pCvMat; image_size: TCvSize; alpha: double;
  new_camera_matrix: pCvMat; new_imag_size: TCvSize { = CV_DEFAULT(cvSize(0,0))) }; valid_pixel_ROI: PCvRect = nil;
  center_principal_point: Integer = 0); cdecl;
{$ENDIF}
(*
  Converts rotation vector to rotation matrix or vice versa

  CVAPI(int) cvRodrigues2( const CvMat* src, CvMat* dst,
  CvMat* jacobian CV_DEFAULT(0) );
*)
{$IFDEF SAFELOADLIB}

type
  TcvRodrigues2 = function(const src: pCvMat; dst: pCvMat; jacobian: pCvMat = nil): Integer; cdecl;

var
  cvRodrigues2: TcvRodrigues2;
{$ELSE}
function cvRodrigues2(const src: pCvMat; dst: pCvMat; jacobian: pCvMat = nil): Integer; cdecl;
{$ENDIF}
(*
  Finds perspective transformation between the object plane and image (view) plane

  CVAPI(int) cvFindHomography( const CvMat* src_points,
  const CvMat* dst_points,
  CvMat* homography,
  int method CV_DEFAULT(0),
  double ransacReprojThreshold CV_DEFAULT(3),
  CvMat* mask CV_DEFAULT(0));
*)
{$IFDEF SAFELOADLIB}

type
  TcvFindHomography = function(const src_points: pCvMat; const dst_points: pCvMat; homography: pCvMat; method: Integer = 0;
    ransacReprojThreshold: double = 3; mask: pCvMat = nil): Integer; cdecl;

Var
  cvFindHomography: TcvFindHomography;
{$ELSE}
function cvFindHomography(const src_points: pCvMat; const dst_points: pCvMat; homography: pCvMat; method: Integer = 0;
  ransacReprojThreshold: double = 3; mask: pCvMat = nil): Integer; cdecl;
{$ENDIF}
(*
  Computes RQ decomposition for 3x3 matrices

  CVAPI(void) cvRQDecomp3x3( const CvMat *matrixM, CvMat *matrixR, CvMat *matrixQ,
  CvMat *matrixQx CV_DEFAULT(NULL),
  CvMat *matrixQy CV_DEFAULT(NULL),
  CvMat *matrixQz CV_DEFAULT(NULL),
  CvPoint3D64f *eulerAngles CV_DEFAULT(NULL));
*)
{$IFDEF SAFELOADLIB}

type
  TcvRQDecomp3x3 = procedure(const matrixM: pCvMat; matrixR: pCvMat; matrixQ: pCvMat; matrixQx: pCvMat = nil; matrixQy: pCvMat = nil;
    matrixQz: pCvMat = nil; eulerAngles: PCvPoint3D64f = nil); cdecl;

Var
  cvRQDecomp3x3: TcvRQDecomp3x3;
{$ELSE}
procedure cvRQDecomp3x3(const matrixM: pCvMat; matrixR: pCvMat; matrixQ: pCvMat; matrixQx: pCvMat = nil; matrixQy: pCvMat = nil;
  matrixQz: pCvMat = nil; eulerAngles: PCvPoint3D64f = nil); cdecl;
{$ENDIF}
(*
  Computes projection matrix decomposition

  CVAPI(void) cvDecomposeProjectionMatrix( const CvMat *projMatr, CvMat *calibMatr,
  CvMat *rotMatr, CvMat *posVect,
  CvMat *rotMatrX CV_DEFAULT(NULL),
  CvMat *rotMatrY CV_DEFAULT(NULL),
  CvMat *rotMatrZ CV_DEFAULT(NULL),
  CvPoint3D64f *eulerAngles CV_DEFAULT(NULL));
*)
{$IFDEF SAFELOADLIB}

Type
  TcvDecomposeProjectionMatrix = procedure(const projMatr: pCvMat; calibMatr: pCvMat; rotMatr: pCvMat; posVect: pCvMat; rotMatrX: pCvMat = nil;
    rotMatrY: pCvMat = nil; rotMatrZ: pCvMat = nil; eulerAngles: PCvPoint3D64f = nil); cdecl;

var
  cvDecomposeProjectionMatrix: TcvDecomposeProjectionMatrix;
{$ELSE}
procedure cvDecomposeProjectionMatrix(const projMatr: pCvMat; calibMatr: pCvMat; rotMatr: pCvMat; posVect: pCvMat; rotMatrX: pCvMat = nil;
  rotMatrY: pCvMat = nil; rotMatrZ: pCvMat = nil; eulerAngles: PCvPoint3D64f = nil); cdecl;
{$ENDIF}
(*
  Computes d(AB)/dA and d(AB)/dB

  CVAPI(void) cvCalcMatMulDeriv( const CvMat* A, const CvMat* B, CvMat* dABdA, CvMat* dABdB );
*)
{$IFDEF SAFELOADLIB}

type
  TcvCalcMatMulDeriv = procedure(const A: pCvMat; const B: pCvMat; dABdA: pCvMat; dABdB: pCvMat); cdecl;

var
  cvCalcMatMulDeriv: TcvCalcMatMulDeriv;
{$ELSE}
procedure cvCalcMatMulDeriv(const A: pCvMat; const B: pCvMat; dABdA: pCvMat; dABdB: pCvMat); cdecl;
{$ENDIF}
(*
  Computes r3 = rodrigues(rodrigues(r2)*rodrigues(r1)),
  t3 = rodrigues(r2)*t1 + t2 and the respective derivatives

  CVAPI(void) cvComposeRT( const CvMat* _rvec1, const CvMat* _tvec1,
  const CvMat* _rvec2, const CvMat* _tvec2,
  CvMat* _rvec3, CvMat* _tvec3,
  CvMat* dr3dr1 CV_DEFAULT(0), CvMat* dr3dt1 CV_DEFAULT(0),
  CvMat* dr3dr2 CV_DEFAULT(0), CvMat* dr3dt2 CV_DEFAULT(0),
  CvMat* dt3dr1 CV_DEFAULT(0), CvMat* dt3dt1 CV_DEFAULT(0),
  CvMat* dt3dr2 CV_DEFAULT(0), CvMat* dt3dt2 CV_DEFAULT(0) );
*)
{$IFDEF SAFELOADLIB}

type
  TcvComposeRT = procedure(const _rvec1: pCvMat; const _tvec1: pCvMat; const _rvec2: pCvMat; const _tvec2: pCvMat; _rvec3: pCvMat; _tvec3: pCvMat;
    dr3dr1: pCvMat = nil; dr3dt1: pCvMat = nil; dr3dr2: pCvMat = nil; dr3dt2: pCvMat = nil; dt3dr1: pCvMat = nil; dt3dt1: pCvMat = nil;
    dt3dr2: pCvMat = nil; dt3dt2: pCvMat = nil); cdecl;

var
  cvComposeRT: TcvComposeRT;
{$ELSE}
procedure cvComposeRT(const _rvec1: pCvMat; const _tvec1: pCvMat; const _rvec2: pCvMat; const _tvec2: pCvMat; _rvec3: pCvMat; _tvec3: pCvMat;
  dr3dr1: pCvMat = nil; dr3dt1: pCvMat = nil; dr3dr2: pCvMat = nil; dr3dt2: pCvMat = nil; dt3dr1: pCvMat = nil; dt3dt1: pCvMat = nil;
  dt3dr2: pCvMat = nil; dt3dt2: pCvMat = nil); cdecl;
{$ENDIF}
(*
  Projects object points to the view plane using
  the specified extrinsic and intrinsic camera parameters

  CVAPI(void) cvProjectPoints2( const CvMat* object_points, const CvMat* rotation_vector,
  const CvMat* translation_vector, const CvMat* camera_matrix,
  const CvMat* distortion_coeffs, CvMat* image_points,
  CvMat* dpdrot CV_DEFAULT(NULL), CvMat* dpdt CV_DEFAULT(NULL),
  CvMat* dpdf CV_DEFAULT(NULL), CvMat* dpdc CV_DEFAULT(NULL),
  CvMat* dpddist CV_DEFAULT(NULL),
  double aspect_ratio CV_DEFAULT(0));
*)
{$IFDEF SAFELOADLIB}

type
  TcvProjectPoints2 = procedure(const object_points: pCvMat; const rotation_vector: pCvMat; const translation_vector: pCvMat;
    const camera_matrix: pCvMat; const distortion_coeffs: pCvMat; image_points: pCvMat; dpdrot: pCvMat = nil; dpdt: pCvMat = nil; dpdf: pCvMat = nil;
    dpdc: pCvMat = nil; dpddist: pCvMat = nil; aspect_ratio: double = 0); cdecl;

var
  cvProjectPoints2: TcvProjectPoints2;
{$ELSE}
procedure cvProjectPoints2(const object_points: pCvMat; const rotation_vector: pCvMat; const translation_vector: pCvMat; const camera_matrix: pCvMat;
  const distortion_coeffs: pCvMat; image_points: pCvMat; dpdrot: pCvMat = nil; dpdt: pCvMat = nil; dpdf: pCvMat = nil; dpdc: pCvMat = nil;
  dpddist: pCvMat = nil; aspect_ratio: double = 0); cdecl;
{$ENDIF}
(* Finds extrinsic camera parameters from
  a few known corresponding point pairs and intrinsic parameters

  CVAPI(void) cvFindExtrinsicCameraParams2( const CvMat* object_points,
  const CvMat* image_points,
  const CvMat* camera_matrix,
  const CvMat* distortion_coeffs,
  CvMat* rotation_vector,
  CvMat* translation_vector,
  int use_extrinsic_guess CV_DEFAULT(0) );
*)
{$IFDEF SAFELOADLIB}

type
  TcvFindExtrinsicCameraParams2 = procedure(const object_points: pCvMat; const image_points: pCvMat; const camera_matrix: pCvMat;
    const distortion_coeffs: pCvMat; rotation_vector: pCvMat; translation_vector: pCvMat; use_extrinsic_guess: Integer = 0); cdecl;

var
  cvFindExtrinsicCameraParams2: TcvFindExtrinsicCameraParams2;
{$ELSE}
procedure cvFindExtrinsicCameraParams2(const object_points: pCvMat; const image_points: pCvMat; const camera_matrix: pCvMat;
  const distortion_coeffs: pCvMat; rotation_vector: pCvMat; translation_vector: pCvMat; use_extrinsic_guess: Integer = 0); cdecl;
{$ENDIF}
(* Computes initial estimate of the intrinsic camera parameters
  in case of planar calibration target (e.g. chessboard)

  CVAPI(void) cvInitIntrinsicParams2D( const CvMat* object_points,
  const CvMat* image_points,
  const CvMat* npoints, CvSize image_size,
  CvMat* camera_matrix,
  double aspect_ratio CV_DEFAULT(1.));
*)

{$IFDEF SAFELOADLIB}

Type
  TcvInitIntrinsicParams2D = procedure(const object_points: pCvMat; const image_points: pCvMat; const npoints: pCvMat; image_size: TCvSize;
    camera_matrix: pCvMat; aspect_ratio: double = 1); cdecl;

var
  cvInitIntrinsicParams2D: TcvInitIntrinsicParams2D;
{$ELSE}
procedure cvInitIntrinsicParams2D(const object_points: pCvMat; const image_points: pCvMat; const npoints: pCvMat; image_size: TCvSize;
  camera_matrix: pCvMat; aspect_ratio: double = 1); cdecl;
{$ENDIF}

const
  CV_CALIB_CB_ADAPTIVE_THRESH = 1;
  CV_CALIB_CB_NORMALIZE_IMAGE = 2;
  CV_CALIB_CB_FILTER_QUADS = 4;
  CV_CALIB_CB_FAST_CHECK = 8;

  (* Performs a fast check if a chessboard is in the input image. This is a workaround to
    a problem of cvFindChessboardCorners being slow on images with no chessboard
    - src: input image
    - size: chessboard size
    Returns 1 if a chessboard can be in this image and findChessboardCorners should be called,
    0 if there is no chessboard, -1 in case of error

    CVAPI(int) cvCheckChessboard(IplImage* src, CvSize size);
  *)
{$IFDEF SAFELOADLIB}

Type
  TcvCheckChessboard = function(const image: pCvArr; size: TCvSize): Integer; cdecl;

var
  cvCheckChessboard: TcvCheckChessboard;
{$ELSE}
function cvCheckChessboard(const image: pCvArr; size: TCvSize): Integer; cdecl;
{$ENDIF}
(*
  Detects corners on a chessboard calibration pattern

  CVAPI(int) cvFindChessboardCorners(
  const void* image,
  CvSize pattern_size,
  CvPoint2D32f* corners,
  int* corner_count CV_DEFAULT(NULL),
  int flags CV_DEFAULT(CV_CALIB_CB_ADAPTIVE_THRESH+CV_CALIB_CB_NORMALIZE_IMAGE) );
*)
{$IFDEF SAFELOADLIB}

type
  TcvFindChessboardCorners = function(const image: Pointer; pattern_size: TCvSize; corners: pCvPoint2D32f; corner_count: pInteger = nil;
    flags: Integer = CV_CALIB_CB_ADAPTIVE_THRESH + CV_CALIB_CB_NORMALIZE_IMAGE): Integer; cdecl;

var
  cvFindChessboardCorners: TcvFindChessboardCorners;
{$ELSE}
function cvFindChessboardCorners(const image: Pointer; pattern_size: TCvSize; corners: pCvPoint2D32f; corner_count: pInteger = nil;
  flags: Integer = CV_CALIB_CB_ADAPTIVE_THRESH + CV_CALIB_CB_NORMALIZE_IMAGE): Integer; cdecl;
{$ENDIF}

const
  CV_CALIB_USE_INTRINSIC_GUESS = 1;
  CV_CALIB_FIX_ASPECT_RATIO = 2;
  CV_CALIB_FIX_PRINCIPAL_POINT = 4;
  CV_CALIB_ZERO_TANGENT_DIST = 8;
  CV_CALIB_FIX_FOCAL_LENGTH = 16;
  CV_CALIB_FIX_K1 = 32;
  CV_CALIB_FIX_K2 = 64;
  CV_CALIB_FIX_K3 = 128;
  CV_CALIB_FIX_K4 = 2048;
  CV_CALIB_FIX_K5 = 4096;
  CV_CALIB_FIX_K6 = 8192;
  CV_CALIB_RATIONAL_MODEL = 16384;
  CV_CALIB_THIN_PRISM_MODEL = 32768;
  CV_CALIB_FIX_S1_S2_S3_S4 = 65536;

  (*
    Draws individual chessboard corners or the whole chessboard detected

    CVAPI(void) cvDrawChessboardCorners(
    CvArr* image,
    CvSize pattern_size,
    CvPoint2D32f* corners,
    int count,
    int pattern_was_found );
  *)
{$IFDEF SAFELOADLIB}

type
  TcvDrawChessboardCorners = procedure(image: pIplImage; pattern_size: TCvSize; corners: pCvPoint2D32f; count: Integer;
    pattern_was_found: Integer); cdecl;

var
  cvDrawChessboardCorners: TcvDrawChessboardCorners;
{$ELSE}
procedure cvDrawChessboardCorners(image: pIplImage; pattern_size: TCvSize; corners: pCvPoint2D32f; count: Integer; pattern_was_found: Integer); cdecl;
{$ENDIF}
{
  /* Finds intrinsic and extrinsic camera parameters
  from a few views of known calibration pattern *)
  CVAPI(double) cvCalibrateCamera2(
  const CvMat* object_points,
  const CvMat* image_points,
  const CvMat* point_counts,
  CvSize image_size,
  CvMat* camera_matrix,
  CvMat* distortion_coeffs,
  CvMat* rotation_vectors CV_DEFAULT(NULL),
  CvMat* translation_vectors CV_DEFAULT(NULL),
  int flags CV_DEFAULT(0),
  CvTermCriteria term_crit CV_DEFAULT(cvTermCriteria(CV_TERMCRIT_ITER+CV_TERMCRIT_EPS,30,DBL_EPSILON)) );
}
{$IFDEF SAFELOADLIB}

type
  TcvCalibrateCamera2 = function(
    { } const object_points: pCvMat;
    { } const image_points: pCvMat;
    { } const point_counts: pCvMat;
    { } image_size: TCvSize;
    { } camera_matrix: pCvMat;
    { } distortion_coeffs: pCvMat;
    { } rotation_vectors: pCvMat { =nil };
    { } translation_vectors: pCvMat { =nil };
    { } flags: Integer { =0 };
    { } term_crit: TCvTermCriteria { =cvTermCriteria(CV_TERMCRIT_ITER+CV_TERMCRIT_EPS,30,DBL_EPSILON) }
    ): double; cdecl;

var
  cvCalibrateCamera2: TcvCalibrateCamera2;
{$ELSE}
function cvCalibrateCamera2(
  { } const object_points: pCvMat;
  { } const image_points: pCvMat;
  { } const point_counts: pCvMat;
  { } image_size: TCvSize;
  { } camera_matrix: pCvMat;
  { } distortion_coeffs: pCvMat;
  { } rotation_vectors: pCvMat { =nil };
  { } translation_vectors: pCvMat { =nil };
  { } flags: Integer { =0 };
  { } term_crit: TCvTermCriteria { =cvTermCriteria(CV_TERMCRIT_ITER+CV_TERMCRIT_EPS,30,DBL_EPSILON) }
  ): double; cdecl;
{$ENDIF}
(* Computes various useful characteristics of the camera from the data computed by
  cvCalibrateCamera2

  CVAPI(void) cvCalibrationMatrixValues( const CvMat *camera_matrix,
  CvSize image_size,
  double aperture_width CV_DEFAULT(0),
  double aperture_height CV_DEFAULT(0),
  double *fovx CV_DEFAULT(NULL),
  double *fovy CV_DEFAULT(NULL),
  double *focal_length CV_DEFAULT(NULL),
  CvPoint2D64f *principal_point CV_DEFAULT(NULL),
  double *pixel_aspect_ratio CV_DEFAULT(NULL));
*)
{$IFDEF SAFELOADLIB}

type
  TcvCalibrationMatrixValues = procedure(const camera_matrix: pCvMat; image_size: TCvSize; aperture_width: double = 0; aperture_height: double = 0;
    fovx: PDouble = nil; fovy: PDouble = nil; focal_length: PDouble = nil; principal_point: PCvPoint2D64f = nil;
    pixel_aspect_ratio: PDouble = nil); cdecl;

Var
  cvCalibrationMatrixValues: TcvCalibrationMatrixValues;
{$ELSE}
procedure cvCalibrationMatrixValues(const camera_matrix: pCvMat; image_size: TCvSize; aperture_width: double = 0; aperture_height: double = 0;
  fovx: PDouble = nil; fovy: PDouble = nil; focal_length: PDouble = nil; principal_point: PCvPoint2D64f = nil;
  pixel_aspect_ratio: PDouble = nil); cdecl;
{$ENDIF}

const
  CV_CALIB_FIX_INTRINSIC = 256;
  CV_CALIB_SAME_FOCAL_LENGTH = 512;

  (* Computes the transformation from one camera coordinate system to another one
    from a few correspondent views of the same calibration target. Optionally, calibrates
    both cameras

    CVAPI(double) cvStereoCalibrate( const CvMat* object_points, const CvMat* image_points1,
    const CvMat* image_points2, const CvMat* npoints,
    CvMat* camera_matrix1, CvMat* dist_coeffs1,
    CvMat* camera_matrix2, CvMat* dist_coeffs2,
    CvSize image_size, CvMat* R, CvMat* T,
    CvMat* E CV_DEFAULT(0), CvMat* F CV_DEFAULT(0),
    CvTermCriteria term_crit CV_DEFAULT(cvTermCriteria(
    CV_TERMCRIT_ITER+CV_TERMCRIT_EPS,30,1e-6)),
    int flags CV_DEFAULT(CV_CALIB_FIX_INTRINSIC));
  *)
{$IFDEF SAFELOADLIB}

type
  TcvStereoCalibrate = function(const object_points: pCvMat; const image_points1: pCvMat; const image_points2: pCvMat; const npoints: pCvMat;
    camera_matrix1: pCvMat; dist_coeffs1: pCvMat; camera_matrix2: pCvMat; dist_coeffs2: pCvMat; image_size: TCvSize; R: pCvMat; T: pCvMat;
    E: pCvMat { = nil }; F: pCvMat { = nil };
    term_crit: TCvTermCriteria { = CV_DEFAULT(cvTermCriteria(CV_TERMCRIT_ITER + CV_TERMCRIT_EPS, 30, 1E-6)) };
    flags: Integer { = CV_DEFAULT(CV_CALIB_FIX_INTRINSIC) } ): double; cdecl;

var
  cvStereoCalibrate: TcvStereoCalibrate;
{$ELSE}
function cvStereoCalibrate(const object_points: pCvMat; const image_points1: pCvMat; const image_points2: pCvMat; const npoints: pCvMat;
  camera_matrix1: pCvMat; dist_coeffs1: pCvMat; camera_matrix2: pCvMat; dist_coeffs2: pCvMat; image_size: TCvSize; R: pCvMat; T: pCvMat;
  E: pCvMat { = nil }; F: pCvMat { = nil }; term_crit: TCvTermCriteria { = CV_DEFAULT(cvTermCriteria(CV_TERMCRIT_ITER + CV_TERMCRIT_EPS, 30, 1E-6)) };
  flags: Integer { = CV_DEFAULT(CV_CALIB_FIX_INTRINSIC) } ): double; cdecl;
{$ENDIF}

const
  CV_CALIB_ZERO_DISPARITY = 1024;

  (*
    Computes 3D rotations (+ optional shift) for each camera coordinate system to make both
    views parallel (=> to make all the epipolar lines horizontal or vertical)

    CVAPI(void) cvStereoRectify( const CvMat* camera_matrix1, const CvMat* camera_matrix2,
    const CvMat* dist_coeffs1, const CvMat* dist_coeffs2,
    CvSize image_size, const CvMat* R, const CvMat* T,
    CvMat* R1, CvMat* R2, CvMat* P1, CvMat* P2,
    CvMat* Q CV_DEFAULT(0),
    int flags CV_DEFAULT(CV_CALIB_ZERO_DISPARITY),
    double alpha CV_DEFAULT(-1),
    CvSize new_image_size CV_DEFAULT(cvSize(0,0)),
    CvRect* valid_pix_ROI1 CV_DEFAULT(0),
    CvRect* valid_pix_ROI2 CV_DEFAULT(0));
  *)
{$IFDEF SAFELOADLIB}

type
  TcvStereoRectify = procedure(const camera_matrix1: pCvMat; const camera_matrix2: pCvMat; const dist_coeffs1: pCvMat; const dist_coeffs2: pCvMat;
    image_size: TCvSize; const R: pCvMat; const T: pCvMat; R1: pCvMat; R2: pCvMat; P1: pCvMat; P2: pCvMat; Q: pCvMat { = nil };
    flags: Integer { = CV_CALIB_ZERO_DISPARITY }; alpha: double { = -1 }; new_image_size: TCvSize { =CV_DEFAULT(cvSize(0,0)) };
    valid_pix_ROI1: PCvRect { =nil }; valid_pix_ROI2: PCvRect { =nil } ); cdecl;

var
  cvStereoRectify: TcvStereoRectify;
{$ELSE}
procedure cvStereoRectify(const camera_matrix1: pCvMat; const camera_matrix2: pCvMat; const dist_coeffs1: pCvMat; const dist_coeffs2: pCvMat;
  image_size: TCvSize; const R: pCvMat; const T: pCvMat; R1: pCvMat; R2: pCvMat; P1: pCvMat; P2: pCvMat; Q: pCvMat { = nil };
  flags: Integer { = CV_CALIB_ZERO_DISPARITY }; alpha: double { = -1 }; new_image_size: TCvSize { =CV_DEFAULT(cvSize(0,0)) };
  valid_pix_ROI1: PCvRect { =nil }; valid_pix_ROI2: PCvRect { =nil } ); cdecl;
{$ENDIF}
(*
  Computes rectification transformations for uncalibrated pair of images using a set
  of point correspondences

  CVAPI(int) cvStereoRectifyUncalibrated( const CvMat* points1, const CvMat* points2,
  const CvMat* F, CvSize img_size,
  CvMat* H1, CvMat* H2,
  double threshold CV_DEFAULT(5));
*)
{$IFDEF SAFELOADLIB}

Type
  TcvStereoRectifyUncalibrated = function(const points1: pCvMat; const points2: pCvMat; const F: pCvMat; img_size: TCvSize; H1: pCvMat; H2: pCvMat;
    threshold: double = 5): Integer; cdecl;

var
  cvStereoRectifyUncalibrated: TcvStereoRectifyUncalibrated;
{$ELSE}
function cvStereoRectifyUncalibrated(const points1: pCvMat; const points2: pCvMat; const F: pCvMat; img_size: TCvSize; H1: pCvMat; H2: pCvMat;
  threshold: double = 5): Integer; cdecl;
{$ENDIF}

(* stereo correspondence parameters and functions *)
const
  CV_STEREO_BM_NORMALIZED_RESPONSE = 0;
  CV_STEREO_BM_XSOBEL = 1;

Type
  (* Block matching algorithm structure *)
  (* typedef struct CvStereoBMState
    {
    // pre-filtering (normalization of input images)
    int preFilterType; // =CV_STEREO_BM_NORMALIZED_RESPONSE now
    int preFilterSize; // averaging window size: ~5x5..21x21
    int preFilterCap; // the output of pre-filtering is clipped by [-preFilterCap,preFilterCap]

    // correspondence using Sum of Absolute Difference (SAD)
    int SADWindowSize; // ~5x5..21x21
    int minDisparity;  // minimum disparity (can be negative)
    int numberOfDisparities; // maximum disparity - minimum disparity (> 0)

    // post-filtering
    int textureThreshold;  // the disparity is only computed for pixels
    // with textured enough neighborhood
    int uniquenessRatio;   // accept the computed disparity d* only if
    // SAD(d) >= SAD(d* )*(1 + uniquenessRatio/100.)
    // for any d != d*+/-1 within the search range.
    int speckleWindowSize; // disparity variation window
    int speckleRange; // acceptable range of variation in window

    int trySmallerWindows; // if 1, the results may be more accurate,
    // at the expense of slower processing
    CvRect roi1, roi2;
    int disp12MaxDiff;

    // temporary buffers
    CvMat* preFilteredImg0;
    CvMat* preFilteredImg1;
    CvMat* slidingSumBuf;
    CvMat* cost;
    CvMat* disp;
    } CvStereoBMState;
  *)

  pCvStereoBMState = ^TCvStereoBMState;

  TCvStereoBMState = record
    // pre-filtering (normalization of input images)
    preFilterType: Integer; // =CV_STEREO_BM_NORMALIZED_RESPONSE now
    preFilterSize: Integer; // averaging window size: ~5x5..21x21
    preFilterCap: Integer; // the output of pre-filtering is clipped by [-preFilterCap,preFilterCap]

    // correspondence using Sum of Absolute Difference (SAD)
    SADWindowSize: Integer; // ~5x5..21x21
    minDisparity: Integer; // minimum disparity (can be negative)
    numberOfDisparities: Integer; // maximum disparity - minimum disparity (> 0)

    // post-filtering
    textureThreshold: Integer; // the disparity is only computed for pixels
    // with textured enough neighborhood
    uniquenessRatio: Integer; // accept the computed disparity d* only if
    // SAD(d) >= SAD(d*)*(1 + uniquenessRatio/100.)
    // for any d != d*+/-1 within the search range.
    speckleWindowSize: Integer; // disparity variation window
    speckleRange: Integer; // acceptable range of variation in window

    trySmallerWindows: Integer; // if 1, the results may be more accurate,
    // at the expense of slower processing
    roi1, roi2: TCvRect;
    disp12MaxDiff: Integer;

    // temporary buffers
    preFilteredImg0: pCvMat;
    preFilteredImg1: pCvMat;
    slidingSumBuf: pCvMat;
    cost: pCvMat;
    disp: pCvMat;
  end;

const
  CV_STEREO_BM_BASIC = 0;
  CV_STEREO_BM_FISH_EYE = 1;
  CV_STEREO_BM_NARROW = 2;

  (*
    CVAPI(CvStereoBMState* ) cvCreateStereoBMState(int preset CV_DEFAULT(CV_STEREO_BM_BASIC),
    int numberOfDisparities CV_DEFAULT(0));
  *)
{$IFDEF SAFELOADLIB}

type
  TcvCreateStereoBMState = function(preset: Integer = CV_STEREO_BM_BASIC; numberOfDisparities: Integer = 0): pCvStereoBMState; cdecl;

var
  cvCreateStereoBMState: TcvCreateStereoBMState;
{$ELSE}
function cvCreateStereoBMState(preset: Integer = CV_STEREO_BM_BASIC; numberOfDisparities: Integer = 0): pCvStereoBMState; cdecl;
{$ENDIF}
(*
  CVAPI(void) cvReleaseStereoBMState( CvStereoBMState** state );
*)
{$IFDEF SAFELOADLIB}

type
  TcvReleaseStereoBMState = procedure(Var state: pCvStereoBMState); cdecl;

var
  cvReleaseStereoBMState: TcvReleaseStereoBMState;
{$ELSE}
procedure cvReleaseStereoBMState(Var state: pCvStereoBMState); cdecl;
{$ENDIF}
(*
  CVAPI(void) cvFindStereoCorrespondenceBM( const CvArr* left, const CvArr* right,
  CvArr* disparity, CvStereoBMState* state );
*)
{$IFDEF SAFELOADLIB}

type
  TcvFindStereoCorrespondenceBM = procedure(const left: pCvArr; const right: pCvArr; disparity: pCvArr; state: pCvStereoBMState); cdecl;

var
  cvFindStereoCorrespondenceBM: TcvFindStereoCorrespondenceBM;
{$ELSE}
procedure cvFindStereoCorrespondenceBM(const left: pCvArr; const right: pCvArr; disparity: pCvArr; state: pCvStereoBMState); cdecl;
{$ENDIF}
(*
  CVAPI(CvRect) cvGetValidDisparityROI( CvRect roi1, CvRect roi2, int minDisparity,
  int numberOfDisparities, int SADWindowSize );
*)
{$IFDEF SAFELOADLIB}

type
  TcvGetValidDisparityROI = function(roi1: TCvRect; roi2: TCvRect; minDisparity: Integer; numberOfDisparities: Integer; SADWindowSize: Integer)
    : TCvRect; cdecl;

var
  cvGetValidDisparityROI: TcvGetValidDisparityROI;
{$ELSE}
function cvGetValidDisparityROI(roi1: TCvRect; roi2: TCvRect; minDisparity: Integer; numberOfDisparities: Integer; SADWindowSize: Integer)
  : TCvRect; cdecl;
{$ENDIF}
(*
  CVAPI(void) cvValidateDisparity( CvArr* disparity, const CvArr* cost,
  int minDisparity, int numberOfDisparities,
  int disp12MaxDiff CV_DEFAULT(1) );
*)
{$IFDEF SAFELOADLIB}

type
  TcvValidateDisparity = procedure(disparity: pCvArr; const cost: pCvArr; minDisparity: Integer; numberOfDisparities: Integer;
    disp12MaxDiff: Integer = 1); cdecl;

var
  cvValidateDisparity: TcvValidateDisparity;
{$ELSE}
procedure cvValidateDisparity(disparity: pCvArr; const cost: pCvArr; minDisparity: Integer; numberOfDisparities: Integer;
  disp12MaxDiff: Integer = 1); cdecl;
{$ENDIF}
(*
  Reprojects the computed disparity image to the 3D space using the specified 4x4 matrix

  CVAPI(void)  cvReprojectImageTo3D(
  const CvArr* disparityImage,
  CvArr* _3dImage,
  const CvMat* Q,
  int handleMissingValues CV_DEFAULT(0) );
*)
{$IFDEF SAFELOADLIB}

Type
  TcvReprojectImageTo3D = procedure(
    { } const disparityImage: pCvMat;
    { } _3dImage: pIplImage;
    { } const Q: pCvMat;
    { } handleMissingValues: Integer = 0); cdecl;

var
  cvReprojectImageTo3D: TcvReprojectImageTo3D;
{$ELSE}
procedure cvReprojectImageTo3D(
  { } const disparityImage: pCvMat;
  { } _3dImage: pIplImage;
  { } const Q: pCvMat;
  { } handleMissingValues: Integer = 0); cdecl;
{$ENDIF}
// ------------------------------------

{$IF DEFINED(SAFELOADLIB) AND DEFINED(DEBUG)}
procedure Init_opencv_calib3d_lib;
{$ENDIF}

implementation

uses ocv.lib;

{$IFDEF SAFELOADLIB}

Var
  calib3dDLL: Cardinal;

procedure Init_opencv_calib3d_lib;
begin
  calib3dDLL := ocvLoadLibrary(calib3d_lib);
  Assert(calib3dDLL <> 0, 'Can not init ' + calib3d_lib);

  cvCreatePOSITObject := ocvGetProcAddress('cvCreatePOSITObject', calib3dDLL);
  cvPOSIT := ocvGetProcAddress('cvPOSIT', calib3dDLL);
  cvReleasePOSITObject := ocvGetProcAddress('cvReleasePOSITObject', calib3dDLL);
  cvRANSACUpdateNumIters := ocvGetProcAddress('cvRANSACUpdateNumIters', calib3dDLL);
  cvConvertPointsHomogeneous := ocvGetProcAddress('cvConvertPointsHomogeneous', calib3dDLL);
  cvFindFundamentalMat := ocvGetProcAddress('cvFindFundamentalMat', calib3dDLL);
  cvComputeCorrespondEpilines := ocvGetProcAddress('cvComputeCorrespondEpilines', calib3dDLL);
  cvTriangulatePoints := ocvGetProcAddress('cvTriangulatePoints', calib3dDLL);
  cvCorrectMatches := ocvGetProcAddress('cvCorrectMatches', calib3dDLL);
  cvGetOptimalNewCameraMatrix := ocvGetProcAddress('cvGetOptimalNewCameraMatrix', calib3dDLL);
  cvRodrigues2 := ocvGetProcAddress('cvRodrigues2', calib3dDLL);
  cvFindHomography := ocvGetProcAddress('cvFindHomography', calib3dDLL);
  cvRQDecomp3x3 := ocvGetProcAddress('cvRQDecomp3x3', calib3dDLL);
  cvDecomposeProjectionMatrix := ocvGetProcAddress('cvDecomposeProjectionMatrix', calib3dDLL);
  cvCalcMatMulDeriv := ocvGetProcAddress('cvCalcMatMulDeriv', calib3dDLL);
  cvComposeRT := ocvGetProcAddress('cvComposeRT', calib3dDLL);
  cvProjectPoints2 := ocvGetProcAddress('cvProjectPoints2', calib3dDLL);
  cvFindExtrinsicCameraParams2 := ocvGetProcAddress('cvFindExtrinsicCameraParams2', calib3dDLL);
  cvInitIntrinsicParams2D := ocvGetProcAddress('cvInitIntrinsicParams2D', calib3dDLL);
  cvCheckChessboard := ocvGetProcAddress('cvCheckChessboard', calib3dDLL);
  cvFindChessboardCorners := ocvGetProcAddress('cvFindChessboardCorners', calib3dDLL);
  cvDrawChessboardCorners := ocvGetProcAddress('cvDrawChessboardCorners', calib3dDLL);
  cvCalibrateCamera2 := ocvGetProcAddress('cvCalibrateCamera2', calib3dDLL);
  cvCalibrationMatrixValues := ocvGetProcAddress('cvCalibrationMatrixValues', calib3dDLL);
  cvStereoCalibrate := ocvGetProcAddress('cvStereoCalibrate', calib3dDLL);
  cvStereoRectify := ocvGetProcAddress('cvStereoRectify', calib3dDLL);
  cvStereoRectifyUncalibrated := ocvGetProcAddress('cvStereoRectifyUncalibrated', calib3dDLL);
  cvCreateStereoBMState := ocvGetProcAddress('cvCreateStereoBMState', calib3dDLL);
  cvReleaseStereoBMState := ocvGetProcAddress('cvReleaseStereoBMState', calib3dDLL);
  cvFindStereoCorrespondenceBM := ocvGetProcAddress('cvFindStereoCorrespondenceBM', calib3dDLL);
  cvGetValidDisparityROI := ocvGetProcAddress('cvGetValidDisparityROI', calib3dDLL);
  cvValidateDisparity := ocvGetProcAddress('cvValidateDisparity', calib3dDLL);
  cvReprojectImageTo3D := ocvGetProcAddress('cvReprojectImageTo3D', calib3dDLL);

end;

initialization

Init_opencv_calib3d_lib;

{$ELSE}
function cvCreatePOSITObject; external calib3d_lib;
procedure cvPOSIT; external calib3d_lib;
procedure cvReleasePOSITObject; external calib3d_lib;
function cvRANSACUpdateNumIters; external calib3d_lib;
procedure cvConvertPointsHomogeneous; external calib3d_lib;
function cvFindFundamentalMat; external calib3d_lib;
procedure cvComputeCorrespondEpilines; external calib3d_lib;
procedure cvTriangulatePoints; external calib3d_lib;
procedure cvCorrectMatches; external calib3d_lib;
procedure cvGetOptimalNewCameraMatrix; external calib3d_lib;
function cvRodrigues2; external calib3d_lib;
function cvFindHomography; external calib3d_lib;
procedure cvRQDecomp3x3; external calib3d_lib;
procedure cvDecomposeProjectionMatrix; external calib3d_lib;
procedure cvCalcMatMulDeriv; external calib3d_lib;
procedure cvComposeRT; external calib3d_lib;
procedure cvProjectPoints2; external calib3d_lib;
procedure cvFindExtrinsicCameraParams2; external calib3d_lib;
procedure cvInitIntrinsicParams2D; external calib3d_lib;
function cvCheckChessboard; external calib3d_lib;
function cvFindChessboardCorners; external calib3d_lib;
procedure cvDrawChessboardCorners; external calib3d_lib;
function cvCalibrateCamera2; external calib3d_lib;
procedure cvCalibrationMatrixValues; external calib3d_lib;
function cvStereoCalibrate; external calib3d_lib;
procedure cvStereoRectify; external calib3d_lib;
function cvStereoRectifyUncalibrated; external calib3d_lib;
function cvCreateStereoBMState; external calib3d_lib;
procedure cvReleaseStereoBMState; external calib3d_lib;
procedure cvFindStereoCorrespondenceBM; external calib3d_lib;
function cvGetValidDisparityROI; external calib3d_lib;
procedure cvValidateDisparity; external calib3d_lib;
procedure cvReprojectImageTo3D; external calib3d_lib;
{$ENDIF}

end.
