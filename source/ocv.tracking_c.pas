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
  opencv\modules\video\include\opencv2\video\tracking_c.h
  *************************************************************************************************
*)

unit ocv.tracking_c;

{$I OpenCV.inc}

interface

uses ocv.core.types_c, ocv.imgproc.types_c;

(* ****************************************************************************************
  *                                  Motion Analysis                                      *
  *************************************************************************************** *)

// ************************************ optical flow ***************************************
const
  CV_LKFLOW_PYR_A_READY = 1;
  CV_LKFLOW_PYR_B_READY = 2;
  CV_LKFLOW_INITIAL_GUESSES = 4;
  CV_LKFLOW_GET_MIN_EIGENVALS = 8;

  (*
    It is Lucas & Kanade method, modified to use pyramids.
    Also it does several iterations to get optical flow for
    every point at every pyramid level.
    Calculates optical flow between two images for certain set of points (i.e.
    it is a "sparse" optical flow, which is opposite to the previous 3 methods)

    CVAPI(void)  cvCalcOpticalFlowPyrLK( const CvArr*  prev, const CvArr*  curr,
    CvArr*  prev_pyr, CvArr*  curr_pyr,
    const CvPoint2D32f* prev_features,
    CvPoint2D32f* curr_features,
    int       count,
    CvSize    win_size,
    int       level,
    char*     status,
    float*    track_error,
    CvTermCriteria criteria,
    int       flags );
  *)

{$IFDEF SAFELOADLIB}

Type
  TcvCalcOpticalFlowPyrLK = procedure(const prev: pIplImage; const curr: pIplImage; prev_pyr: pIplImage; curr_pyr: pIplImage;
    const prev_features: pCvPoint2D32f; curr_features: pCvPoint2D32f; count: Integer; win_size: TCvSize; level: Integer; status: pCVChar;
    track_error: PSingle; criteria: TCvTermCriteria; flags: Integer); cdecl;

Var
  cvCalcOpticalFlowPyrLK: TcvCalcOpticalFlowPyrLK;
{$ELSE}
{$EXTERNALSYM cvCalcOpticalFlowPyrLK}
procedure cvCalcOpticalFlowPyrLK(const prev: pIplImage; const curr: pIplImage; prev_pyr: pIplImage; curr_pyr: pIplImage;
  const prev_features: pCvPoint2D32f; curr_features: pCvPoint2D32f; count: Integer; win_size: TCvSize; level: Integer; status: pCVChar;
  track_error: PSingle; criteria: TCvTermCriteria; flags: Integer); cdecl;
{$ENDIF}
(* Modification of a previous sparse optical flow algorithm to calculate
  affine flow

  CVAPI(void)  cvCalcAffineFlowPyrLK( const CvArr*  prev, const CvArr*  curr,
  CvArr*  prev_pyr, CvArr*  curr_pyr,
  const CvPoint2D32f* prev_features,
  CvPoint2D32f* curr_features,
  float* matrices, int  count,
  CvSize win_size, int  level,
  char* status, float* track_error,
  CvTermCriteria criteria, int flags );
*)
{$IFDEF SAFELOADLIB}

Type
  TcvCalcAffineFlowPyrLK = procedure(const prev: pCvArr; const curr: pCvArr; prev_pyr: pCvArr; curr_pyr: pCvArr; const prev_features: pCvPoint2D32f;
    var curr_features: TCvPoint2D32f; var matrices: Single; count: Integer; win_size: TCvSize; level: Integer; status: pCVChar;
    var track_error: Single; criteria: TCvTermCriteria; flags: Integer); cdecl;

Var
  cvCalcAffineFlowPyrLK: TcvCalcAffineFlowPyrLK;
{$ELSE}
{$EXTERNALSYM cvCalcAffineFlowPyrLK}
procedure cvCalcAffineFlowPyrLK(const prev: pCvArr; const curr: pCvArr; prev_pyr: pCvArr; curr_pyr: pCvArr; const prev_features: pCvPoint2D32f;
  var curr_features: TCvPoint2D32f; var matrices: Single; count: Integer; win_size: TCvSize; level: Integer; status: pCVChar; var track_error: Single;
  criteria: TCvTermCriteria; flags: Integer); cdecl;
{$ENDIF}
(*
  Estimate rigid transformation between 2 images or 2 point sets

  CVAPI(int)  cvEstimateRigidTransform( const CvArr* A, const CvArr* B,
  CvMat* M, int full_affine );
*)
{$IFDEF SAFELOADLIB}

Type
  TcvEstimateRigidTransform = function(const A: pCvArr; const B: pCvArr; var M: TCvMat; full_affine: Integer): Integer; cdecl;

var
  cvEstimateRigidTransform: TcvEstimateRigidTransform;
{$ELSE}
{$EXTERNALSYM cvEstimateRigidTransform}
function cvEstimateRigidTransform(const A: pCvArr; const B: pCvArr; var M: TCvMat; full_affine: Integer): Integer; cdecl;
{$ENDIF}
(*
  Estimate optical flow for each pixel using the two-frame G. Farneback algorithm

  CVAPI(void) cvCalcOpticalFlowFarneback( const CvArr* prev, const CvArr* next,
  CvArr* flow, double pyr_scale, int levels,
  int winsize, int iterations, int poly_n,
  double poly_sigma, int flags );
*)
{$IFDEF SAFELOADLIB}

type
  TcvCalcOpticalFlowFarneback = procedure(const prev: pCvMat; const next: pCvMat; flow: pCvMat; pyr_scale: double; levels: Integer; winsize: Integer;
    iterations: Integer; poly_n: Integer; poly_sigma: double; flags: Integer); cdecl;

var
  cvCalcOpticalFlowFarneback: TcvCalcOpticalFlowFarneback;
{$ELSE}
{$EXTERNALSYM cvCalcOpticalFlowFarneback}
procedure cvCalcOpticalFlowFarneback(const prev: pCvMat; const next: pCvMat; flow: pCvMat; pyr_scale: double; levels: Integer; winsize: Integer;
  iterations: Integer; poly_n: Integer; poly_sigma: double; flags: Integer); cdecl;
{$ENDIF}
(* ******************************** motion templates ************************************ *)

(* *****************************************************************************************
  *        All the motion template functions work only with single channel images.         *
  *        Silhouette image must have depth IPL_DEPTH_8U or IPL_DEPTH_8S                   *
  *        Motion history image must have depth IPL_DEPTH_32F,                             *
  *        Gradient mask - IPL_DEPTH_8U or IPL_DEPTH_8S,                                   *
  *        Motion orientation image - IPL_DEPTH_32F                                        *
  *        Segmentation mask - IPL_DEPTH_32F                                               *
  *        All the angles are in degrees, all the times are in milliseconds                *
  *************************************************************************************** *)

(*
  Updates motion history image given motion silhouette

  CVAPI(void)    cvUpdateMotionHistory( const CvArr* silhouette, CvArr* mhi,
  double timestamp, double duration );
*)
{$IFDEF SAFELOADLIB}

Type
  TcvUpdateMotionHistory = procedure(const silhouette: pCvArr; mhi: pCvArr; timestamp: double; duration: double); cdecl;

var
  cvUpdateMotionHistory: TcvUpdateMotionHistory;
{$ELSE}
{$EXTERNALSYM cvUpdateMotionHistory}
procedure cvUpdateMotionHistory(const silhouette: pCvArr; mhi: pCvArr; timestamp: double; duration: double); cdecl;
{$ENDIF}
(*
  Calculates gradient of the motion history image and fills
  a mask indicating where the gradient is valid

  CVAPI(void)    cvCalcMotionGradient( const CvArr* mhi, CvArr* mask, CvArr* orientation,
  double delta1, double delta2,
  int aperture_size CV_DEFAULT(3));
*)
{$IFDEF SAFELOADLIB}

Type
  TcvCalcMotionGradient = procedure(const mhi: pCvArr; mask: pCvArr; orientation: pCvArr; delta1: double; delta2: double;
    aperture_size: Integer = 3); cdecl;

var
  cvCalcMotionGradient: TcvCalcMotionGradient;
{$ELSE}
{$EXTERNALSYM cvCalcMotionGradient}
procedure cvCalcMotionGradient(const mhi: pCvArr; mask: pCvArr; orientation: pCvArr; delta1: double; delta2: double;
  aperture_size: Integer = 3); cdecl;
{$ENDIF}
(* Calculates average motion direction within a selected motion region
  (region can be selected by setting ROIs and/or by composing a valid gradient mask
  with the region mask)

  CVAPI(double)  cvCalcGlobalOrientation( const CvArr* orientation, const CvArr* mask,
  const CvArr* mhi, double timestamp,
  double duration );
*)
{$IFDEF SAFELOADLIB}

type
  TcvCalcGlobalOrientation = function(const orientation: pCvArr; const mask: pCvArr; const mhi: pCvArr; timestamp: double; duration: double)
    : double; cdecl;

var
  cvCalcGlobalOrientation: TcvCalcGlobalOrientation;
{$ELSE}
{$EXTERNALSYM cvCalcGlobalOrientation}
function cvCalcGlobalOrientation(const orientation: pCvArr; const mask: pCvArr; const mhi: pCvArr; timestamp: double; duration: double)
  : double; cdecl;
{$ENDIF}
(* Splits a motion history image into a few parts corresponding to separate independent motions
  (e.g. left hand, right hand)

  CVAPI(CvSeq* )  cvSegmentMotion( const CvArr* mhi, CvArr* seg_mask,
  CvMemStorage* storage,
  double timestamp, double seg_thresh );
*)
{$IFDEF SAFELOADLIB}

type
  TcvSegmentMotion = function(const mhi: pCvArr; seg_mask: pCvArr; storage: pCvMemStorage; timestamp: double; seg_thresh: double): pCvSeq; cdecl;

var
  cvSegmentMotion: TcvSegmentMotion;
{$ELSE}
{$EXTERNALSYM cvSegmentMotion}
function cvSegmentMotion(const mhi: pCvArr; seg_mask: pCvArr; storage: pCvMemStorage; timestamp: double; seg_thresh: double): pCvSeq; cdecl;
{$ENDIF}
(* ****************************************************************************************
  *                                       Tracking                                        *
  *************************************************************************************** *)

(*
  Implements CAMSHIFT algorithm - determines object position, size and orientation
  from the object histogram back project (extension of meanshift)

  CVAPI(int)  cvCamShift( const CvArr* prob_image, CvRect  window,
  CvTermCriteria criteria, CvConnectedComp* comp,
  CvBox2D* box CV_DEFAULT(NULL) );
*)
{$IFDEF SAFELOADLIB}

type
  TcvCamShift = function(const prob_image: pIplImage; window: TCvRect; criteria: TCvTermCriteria; comp: pCvConnectedComp; box: pCvBox2D = nil)
    : Integer; cdecl;

var
  cvCamShift: TcvCamShift;
{$ELSE}
{$EXTERNALSYM cvCamShift}
function cvCamShift(const prob_image: pIplImage; window: TCvRect; criteria: TCvTermCriteria; comp: pCvConnectedComp; box: pCvBox2D = nil)
  : Integer; cdecl;
{$ENDIF}
(* Implements MeanShift algorithm - determines object position
  from the object histogram back project

  CVAPI(int)  cvMeanShift( const CvArr* prob_image, CvRect  window,
  CvTermCriteria criteria, CvConnectedComp* comp );
*)
{$IFDEF SAFELOADLIB}

type
  TcvMeanShift = function(const prob_image: pCvArr; window: TCvRect; criteria: TCvTermCriteria; var comp: TCvConnectedComp): Integer; cdecl;

var
  cvMeanShift: TcvMeanShift;
{$ELSE}
{$EXTERNALSYM cvMeanShift}
function cvMeanShift(const prob_image: pCvArr; window: TCvRect; criteria: TCvTermCriteria; var comp: TCvConnectedComp): Integer; cdecl;
{$ENDIF}

(*
  standard Kalman filter (in G. Welch' and G. Bishop's notation):

  x(k)=A*x(k-1)+B*u(k)+w(k)  p(w)~N(0,Q)
  z(k)=H*x(k)+v(k),   p(v)~N(0,R)
*)
Type
  pCvKalman = ^TCvKalman;

{$EXTERNALSYM TCvKalman}

  TCvKalman = record
    MP: Integer; (* number of measurement vector dimensions *)
    DP: Integer; (* number of state vector dimensions *)
    CP: Integer; (* number of control vector dimensions *)

    (* backward compatibility fields *)
    // {$IFDEF 1}
    PosterState: PSingle; (* =state_pre->data.fl *)
    PriorState: PSingle; (* =state_post->data.fl *)
    DynamMatr: PSingle; (* =transition_matrix->data.fl *)
    MeasurementMatr: PSingle; (* =measurement_matrix->data.fl *)
    MNCovariance: PSingle; (* =measurement_noise_cov->data.fl *)
    PNCovariance: PSingle; (* =process_noise_cov->data.fl *)
    KalmGainMatr: PSingle; (* =gain->data.fl *)
    PriorErrorCovariance: PSingle; (* =error_cov_pre->data.fl *)
    PosterErrorCovariance: PSingle; (* =error_cov_post->data.fl *)
    _Temp1: PSingle; (* temp1->data.fl *)
    _Temp2: PSingle; (* temp2->data.fl *)
    // {$ENDIF}
    state_pre: pCvMat; (* predicted state (x'(k)):
      x(k)=A*x(k-1)+B*u(k) *)
    state_post: pCvMat; (* corrected state (x(k)):
      x(k)=x'(k)+K(k)*(z(k)-H*x'(k)) *)
    transition_matrix: pCvMat; (* state transition matrix (A) *)
    control_matrix: pCvMat; (* control matrix (B)
      (it is not used if there is no control) *)
    measurement_matrix: pCvMat; (* measurement matrix (H) *)
    process_noise_cov: pCvMat; (* process noise covariance matrix (Q) *)
    measurement_noise_cov: pCvMat; (* measurement noise covariance matrix (R) *)
    error_cov_pre: pCvMat; (* priori error estimate covariance matrix (P'(k)):
      P'(k)=A*P(k-1)*At + Q) *)
    gain: pCvMat; (* Kalman gain matrix (K(k)):
      K(k)=P'(k)*Ht*inv(H*P'(k)*Ht+R) *)
    error_cov_post: pCvMat; (* posteriori error estimate covariance matrix (P(k)):
      P(k)=(I-K(k)*H)*P'(k) *)
    Temp1: pCvMat; (* temporary matrices *)
    Temp2: pCvMat;
    temp3: pCvMat;
    temp4: pCvMat;
    temp5: pCvMat;
  end;
  //
  (*
    Creates Kalman filter and sets A, B, Q, R and state to some initial values

    CVAPI(CvKalman* ) cvCreateKalman( int dynam_params, int measure_params,
    int control_params CV_DEFAULT(0));
  *)
{$IFDEF SAFELOADLIB}

type
  TcvCreateKalman = function(dynam_params: Integer; measure_params: Integer; control_params: Integer = 0): pCvKalman; cdecl;

var
  cvCreateKalman: TcvCreateKalman;
{$ELSE}
{$EXTERNALSYM cvCreateKalman}
function cvCreateKalman(dynam_params: Integer; measure_params: Integer; control_params: Integer = 0): pCvKalman; cdecl;
{$ENDIF}
(*
  Releases Kalman filter state

  CVAPI(void)  cvReleaseKalman( CvKalman** kalman);
*)
{$IFDEF SAFELOADLIB}

type
  TcvReleaseKalman = procedure(var kalman: pCvKalman); cdecl;

var
  cvReleaseKalman: TcvReleaseKalman;
{$ELSE}
{$EXTERNALSYM cvReleaseKalman}
procedure cvReleaseKalman(var kalman: pCvKalman); cdecl;
{$ENDIF}
(*
  Updates Kalman filter by time (predicts future state of the system)
  CVAPI(const CvMat*  cvKalmanPredict( CvKalman* kalman,
  const CvMat* control CV_DEFAULT(NULL));

  Updates Kalman filter by measurement
  (corrects state of the system and internal matrices)
  CVAPI(const CvMat* )  cvKalmanCorrect( CvKalman* kalman, const CvMat* measurement );
*)

type
  TcvKalmanPredict = function(var kalman: TCvKalman; const control: pCvMat = nil): pCvMat; cdecl;
  TcvKalmanCorrect = function(var kalman: TCvKalman; const measurement: pCvMat): pCvMat; cdecl;

{$IFDEF SAFELOADLIB}

var
  cvKalmanPredict: TcvKalmanPredict;
  cvKalmanCorrect: TcvKalmanCorrect;
{$ELSE}
{$EXTERNALSYM cvKalmanPredict}
function cvKalmanPredict(var kalman: TCvKalman; const control: pCvMat = nil): pCvMat; cdecl;
{$EXTERNALSYM cvKalmanCorrect}
function cvKalmanCorrect(var kalman: TCvKalman; const measurement: pCvMat): pCvMat; cdecl;
{$ENDIF}

Var
{$EXTERNALSYM cvKalmanUpdateByTime}
  cvKalmanUpdateByTime: TcvKalmanPredict {$IFNDEF SAFELOADLIB} = cvKalmanPredict {$ENDIF};
{$EXTERNALSYM cvKalmanUpdateByMeasurement}
  cvKalmanUpdateByMeasurement: TcvKalmanCorrect{$IFNDEF SAFELOADLIB} = cvKalmanCorrect{$ENDIF};

{$IF DEFINED(SAFELOADLIB) AND DEFINED(DEBUG)}
procedure Init_opencv_Tracking_lib;
{$ENDIF}

implementation

uses
  ocv.lib;

{$IFDEF SAFELOADLIB}

Var
  TrackingDLL: Cardinal;

procedure Init_opencv_Tracking_lib;
begin
  TrackingDLL := ocvLoadLibrary(tracking_lib);
  Assert(TrackingDLL <> 0, 'Can not init ' + tracking_lib);

  cvCalcOpticalFlowPyrLK := ocvGetProcAddress('cvCalcOpticalFlowPyrLK', TrackingDLL);
  cvCalcAffineFlowPyrLK := ocvGetProcAddress('cvCalcAffineFlowPyrLK', TrackingDLL);
  cvEstimateRigidTransform := ocvGetProcAddress('cvEstimateRigidTransform', TrackingDLL);
  cvCalcOpticalFlowFarneback := ocvGetProcAddress('cvCalcOpticalFlowFarneback', TrackingDLL);
  cvUpdateMotionHistory := ocvGetProcAddress('cvUpdateMotionHistory', TrackingDLL);
  cvCalcMotionGradient := ocvGetProcAddress('cvCalcMotionGradient', TrackingDLL);
  cvCalcGlobalOrientation := ocvGetProcAddress('cvCalcGlobalOrientation', TrackingDLL);
  cvSegmentMotion := ocvGetProcAddress('cvSegmentMotion', TrackingDLL);
  cvCamShift := ocvGetProcAddress('cvCamShift', TrackingDLL);
  cvMeanShift := ocvGetProcAddress('cvMeanShift', TrackingDLL);
  cvCreateKalman := ocvGetProcAddress('cvCreateKalman', TrackingDLL);
  cvReleaseKalman := ocvGetProcAddress('cvReleaseKalman', TrackingDLL);
  cvKalmanPredict := ocvGetProcAddress('cvKalmanPredict', TrackingDLL);
  cvKalmanCorrect := ocvGetProcAddress('cvKalmanCorrect', TrackingDLL);
  cvKalmanUpdateByTime := cvKalmanPredict;
  cvKalmanUpdateByMeasurement := cvKalmanCorrect;
end;

initialization

Init_opencv_Tracking_lib;

{$ELSE}
function cvCamShift; external tracking_lib;
procedure cvCalcOpticalFlowPyrLK; external tracking_lib;
procedure cvCalcOpticalFlowFarneback; external tracking_lib;

procedure cvUpdateMotionHistory; external tracking_lib;
procedure cvCalcMotionGradient; external tracking_lib;
function cvSegmentMotion; external tracking_lib;
function cvCalcGlobalOrientation; external tracking_lib;
procedure cvCalcAffineFlowPyrLK; external tracking_lib;
function cvEstimateRigidTransform; external tracking_lib;
function cvMeanShift; external tracking_lib;
function cvCreateKalman; external tracking_lib;
function cvKalmanPredict; external tracking_lib;
function cvKalmanCorrect; external tracking_lib;
procedure cvReleaseKalman; external tracking_lib;
{$ENDIF}

end.
