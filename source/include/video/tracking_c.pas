// --------------------------------- OpenCV license.txt ---------------------------
(* //    IMPORTANT: READ BEFORE DOWNLOADING, COPYING, INSTALLING OR USING.
  //
  //    By downloading, copying, installing or using the software you agree to this license.
  //    If you do not agree to this license, do not download, install,
  //    copy or use the software.
  //
  //
  //                             License Agreement
  //                  For Open Source Computer Vision Library
  //
  //   Copyright (C) 2000-2008, Intel Corporation, all rights reserved.
  //   Copyright (C) 2009, Willow Garage Inc., all rights reserved.
  //   Third party copyrights are property of their respective owners.
  //
  //   Redistribution and use in source and binary forms, with or without modification,
  //   are permitted provided that the following conditions are met:
  //
  //     * Redistribution's of source code must retain the above copyright notice,
  //       this list of conditions and the following disclaimer.
  //
  //     * Redistribution's in binary form must reproduce the above copyright notice,
  //       this list of conditions and the following disclaimer in the documentation
  //       and/or other materials provided with the distribution.
  //
  //     * The name of the copyright holders may not be used to endorse or promote products
  //       derived from this software without specific prior written permission.
  //
  //   This software is provided by the copyright holders and contributors "as is" and
  //   any express or implied warranties, including, but not limited to, the implied
  //   warranties of merchantability and fitness for a particular purpose are disclaimed.
  //   In no event shall the Intel Corporation or contributors be liable for any direct,
  //   indirect, incidental, special, exemplary, or consequential damages
  //   (including, but not limited to, procurement of substitute goods or services;
  //   loss of use, data, or profits; or business interruption) however caused
  //   and on any theory of liability, whether in contract, strict liability,
  //   or tort (including negligence or otherwise) arising in any way out of
  //   the use of this software, even if advised of the possibility of such damage. *)

(* /  **************************************************************************************************
  //                                 Project Delphi-OpenCV
  //  **************************************************************************************************
  //  Contributor:
  //  laentir Valetov
  //  email:laex@bk.ru
  //  **************************************************************************************************
  //  You may retrieve the latest version of this file at the GitHub,
  //  located at git://github.com/Laex/Delphi-OpenCV.git
  //  **************************************************************************************************
  //  License:
  //  The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License");
  //  you may not use this file except in compliance with the License. You may obtain a copy of the
  //  License at http://www.mozilla.org/MPL/
  //
  //  Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF
  //  ANY KIND, either express or implied. See the License for the specific language governing rights
  //  and limitations under the License.
  //
  //  Alternatively, the contents of this file may be used under the terms of the
  //  GNU Lesser General Public License (the  "LGPL License"), in which case the
  //  provisions of the LGPL License are applicable instead of those above.
  //  If you wish to allow use of your version of this file only under the terms
  //  of the LGPL License and not to allow others to use your version of this file
  //  under the MPL, indicate your decision by deleting  the provisions above and
  //  replace  them with the notice and other provisions required by the LGPL
  //  License.  If you do not delete the provisions above, a recipient may use
  //  your version of this file under either the MPL or the LGPL License.
  //
  //  For more information about the LGPL: http://www.gnu.org/copyleft/lesser.html
  //  **************************************************************************************************
  //  Warning: Using Delphi XE3 syntax!
  //  **************************************************************************************************
  //  The Initial Developer of the Original Code:
  //  OpenCV: open source computer vision library
  //  Homepage:    http://opencv.org
  //  Online docs: http://docs.opencv.org
  //  Q&A forum:   http://answers.opencv.org
  //  Dev zone:    http://code.opencv.org
  //  **************************************************************************************************
  //  Original file:
  //  opencv\modules\video\include\opencv2\video\tracking_c.h
  //  ************************************************************************************************* *)

{$IFDEF DEBUG}
{$A8,B-,C+,D+,E-,F-,G+,H+,I+,J-,K-,L+,M-,N+,O-,P+,Q+,R+,S-,T-,U-,V+,W+,X+,Y+,Z1}
{$ELSE}
{$A8,B-,C-,D-,E-,F-,G+,H+,I+,J-,K-,L-,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y-,Z1}
{$ENDIF}
{$WARN SYMBOL_DEPRECATED OFF}
{$WARN SYMBOL_PLATFORM OFF}
{$WARN UNIT_PLATFORM OFF}
{$WARN UNSAFE_TYPE OFF}
{$WARN UNSAFE_CODE OFF}
{$WARN UNSAFE_CAST OFF}
unit tracking_c;

interface

Uses Core.types_c, imgproc.types_c;

/// ****************************************************************************************\
// *                                  Motion Analysis                                       *
// \****************************************************************************************/
//
/// ************************************ optical flow ***************************************/
const
  CV_LKFLOW_PYR_A_READY = 1;
  CV_LKFLOW_PYR_B_READY = 2;
  CV_LKFLOW_INITIAL_GUESSES = 4;
  CV_LKFLOW_GET_MIN_EIGENVALS = 8;

  /// * It is Lucas & Kanade method, modified to use pyramids.
  // Also it does several iterations to get optical flow for
  // every point at every pyramid level.
  // Calculates optical flow between two images for certain set of points (i.e.
  // it is a "sparse" optical flow, which is opposite to the previous 3 methods) */
  // CVAPI(void)  cvCalcOpticalFlowPyrLK( const CvArr*  prev, const CvArr*  curr,
  // CvArr*  prev_pyr, CvArr*  curr_pyr,
  // const CvPoint2D32f* prev_features,
  // CvPoint2D32f* curr_features,
  // int       count,
  // CvSize    win_size,
  // int       level,
  // char*     status,
  // float*    track_error,
  // CvTermCriteria criteria,
  // int       flags );
procedure cvCalcOpticalFlowPyrLK(const prev: pIplImage; const curr: pIplImage; prev_pyr: pIplImage; curr_pyr: pIplImage;
  const prev_features: pCvPoint2D32f; curr_features: pCvPoint2D32f; count: Integer; win_size: TCvSize; level: Integer;
  status: pCVChar; track_error: PSingle; criteria: TCvTermCriteria; flags: Integer); cdecl;

  /// * Modification of a previous sparse optical flow algorithm to calculate
  // affine flow */
  // CVAPI(void)  cvCalcAffineFlowPyrLK( const CvArr*  prev, const CvArr*  curr,
  // CvArr*  prev_pyr, CvArr*  curr_pyr,
  // const CvPoint2D32f* prev_features,
  // CvPoint2D32f* curr_features,
  // float* matrices, int  count,
  // CvSize win_size, int  level,
  // char* status, float* track_error,
  // CvTermCriteria criteria, int flags );
  //
  /// * Estimate rigid transformation between 2 images or 2 point sets */
  // CVAPI(int)  cvEstimateRigidTransform( const CvArr* A, const CvArr* B,
  // CvMat* M, int full_affine );
  //
  /// * Estimate optical flow for each pixel using the two-frame G. Farneback algorithm */
  // CVAPI(void) cvCalcOpticalFlowFarneback( const CvArr* prev, const CvArr* next,
  // CvArr* flow, double pyr_scale, int levels,
  // int winsize, int iterations, int poly_n,
  // double poly_sigma, int flags );
procedure cvCalcOpticalFlowFarneback(const prev: pCvMat; const next: pCvMat; flow: pCvMat; pyr_scale: double;
  levels: Integer; winsize: Integer; iterations: Integer; poly_n: Integer; poly_sigma: double; flags: Integer); cdecl;

  /// ********************************* motion templates *************************************/
  //
  /// ****************************************************************************************\
  // *        All the motion template functions work only with single channel images.         *
  // *        Silhouette image must have depth IPL_DEPTH_8U or IPL_DEPTH_8S                   *
  // *        Motion history image must have depth IPL_DEPTH_32F,                             *
  // *        Gradient mask - IPL_DEPTH_8U or IPL_DEPTH_8S,                                   *
  // *        Motion orientation image - IPL_DEPTH_32F                                        *
  // *        Segmentation mask - IPL_DEPTH_32F                                               *
  // *        All the angles are in degrees, all the times are in milliseconds                *
  // \****************************************************************************************/

  /// * Updates motion history image given motion silhouette */
  // CVAPI(void)    cvUpdateMotionHistory( const CvArr* silhouette, CvArr* mhi,
  // double timestamp, double duration );
procedure cvUpdateMotionHistory(const silhouette: pCvArr; mhi: pCvArr; timestamp: double; duration: double); cdecl;

/// * Calculates gradient of the motion history image and fills
// a mask indicating where the gradient is valid */
// CVAPI(void)    cvCalcMotionGradient( const CvArr* mhi, CvArr* mask, CvArr* orientation,
// double delta1, double delta2,
// int aperture_size CV_DEFAULT(3));
procedure cvCalcMotionGradient(const mhi: pCvArr; mask: pCvArr; orientation: pCvArr; delta1: double; delta2: double;
  aperture_size: Integer = 3); cdecl;

/// * Calculates average motion direction within a selected motion region
// (region can be selected by setting ROIs and/or by composing a valid gradient mask
// with the region mask) */
// CVAPI(double)  cvCalcGlobalOrientation( const CvArr* orientation, const CvArr* mask,
// const CvArr* mhi, double timestamp,
// double duration );
function cvCalcGlobalOrientation(const orientation: pCvArr; const mask: pCvArr; const mhi: pCvArr; timestamp: double;
  duration: double): double; cdecl;

/// * Splits a motion history image into a few parts corresponding to separate independent motions
// (e.g. left hand, right hand) */
// CVAPI(CvSeq*)  cvSegmentMotion( const CvArr* mhi, CvArr* seg_mask,
// CvMemStorage* storage,
// double timestamp, double seg_thresh );
function cvSegmentMotion(const mhi: pCvArr; seg_mask: pCvArr; storage: pCvMemStorage; timestamp: double;
  seg_thresh: double): pCvSeq; cdecl;

/// ****************************************************************************************\
// *                                       Tracking                                         *
// \****************************************************************************************/
//
/// * Implements CAMSHIFT algorithm - determines object position, size and orientation
// from the object histogram back project (extension of meanshift) */
// CVAPI(int)  cvCamShift( const CvArr* prob_image, CvRect  window,
// CvTermCriteria criteria, CvConnectedComp* comp,
// CvBox2D* box CV_DEFAULT(NULL) );
function cvCamShift(const prob_image: pIplImage; window: TCvRect; criteria: TCvTermCriteria; comp: pCvConnectedComp;
  box: pCvBox2D = nil): Integer; cdecl;

/// * Implements MeanShift algorithm - determines object position
// from the object histogram back project */
// CVAPI(int)  cvMeanShift( const CvArr* prob_image, CvRect  window,
// CvTermCriteria criteria, CvConnectedComp* comp );
//
/// *
// standard Kalman filter (in G. Welch' and G. Bishop's notation):
//
// x(k)=A*x(k-1)+B*u(k)+w(k)  p(w)~N(0,Q)
// z(k)=H*x(k)+v(k),   p(v)~N(0,R)
// */
// typedef struct CvKalman
// {
// int MP;                     /* number of measurement vector dimensions */
// int DP;                     /* number of state vector dimensions */
// int CP;                     /* number of control vector dimensions */
//
// /* backward compatibility fields */
// #if 1
// float* PosterState;         /* =state_pre->data.fl */
// float* PriorState;          /* =state_post->data.fl */
// float* DynamMatr;           /* =transition_matrix->data.fl */
// float* MeasurementMatr;     /* =measurement_matrix->data.fl */
// float* MNCovariance;        /* =measurement_noise_cov->data.fl */
// float* PNCovariance;        /* =process_noise_cov->data.fl */
// float* KalmGainMatr;        /* =gain->data.fl */
// float* PriorErrorCovariance;/* =error_cov_pre->data.fl */
// float* PosterErrorCovariance;/* =error_cov_post->data.fl */
// float* Temp1;               /* temp1->data.fl */
// float* Temp2;               /* temp2->data.fl */
// #endif
//
// CvMat* state_pre;           /* predicted state (x'(k)):
// x(k)=A*x(k-1)+B*u(k) */
// CvMat* state_post;          /* corrected state (x(k)):
// x(k)=x'(k)+K(k)*(z(k)-H*x'(k)) */
// CvMat* transition_matrix;   /* state transition matrix (A) */
// CvMat* control_matrix;      /* control matrix (B)
// (it is not used if there is no control)*/
// CvMat* measurement_matrix;  /* measurement matrix (H) */
// CvMat* process_noise_cov;   /* process noise covariance matrix (Q) */
// CvMat* measurement_noise_cov; /* measurement noise covariance matrix (R) */
// CvMat* error_cov_pre;       /* priori error estimate covariance matrix (P'(k)):
// P'(k)=A*P(k-1)*At + Q)*/
// CvMat* gain;                /* Kalman gain matrix (K(k)):
// K(k)=P'(k)*Ht*inv(H*P'(k)*Ht+R)*/
// CvMat* error_cov_post;      /* posteriori error estimate covariance matrix (P(k)):
// P(k)=(I-K(k)*H)*P'(k) */
// CvMat* temp1;               /* temporary matrices */
// CvMat* temp2;
// CvMat* temp3;
// CvMat* temp4;
// CvMat* temp5;
// } CvKalman;
//
/// * Creates Kalman filter and sets A, B, Q, R and state to some initial values */
// CVAPI(CvKalman*) cvCreateKalman( int dynam_params, int measure_params,
// int control_params CV_DEFAULT(0));
//
/// * Releases Kalman filter state */
// CVAPI(void)  cvReleaseKalman( CvKalman** kalman);
//
/// * Updates Kalman filter by time (predicts future state of the system) */
// CVAPI(const CvMat*)  cvKalmanPredict( CvKalman* kalman,
// const CvMat* control CV_DEFAULT(NULL));
//
/// * Updates Kalman filter by measurement
// (corrects state of the system and internal matrices) */
// CVAPI(const CvMat*)  cvKalmanCorrect( CvKalman* kalman, const CvMat* measurement );
//
// #define cvKalmanUpdateByTime  cvKalmanPredict
// #define cvKalmanUpdateByMeasurement cvKalmanCorrect
//
//
// #ifdef __cplusplus
// } // extern "C"
// #endif
//
//
// #endif // __OPENCV_TRACKING_C_H__

implementation

Uses
  uLibName;

function cvCamShift; external tracking_DLL;
procedure cvCalcOpticalFlowPyrLK; external tracking_DLL;
procedure cvCalcOpticalFlowFarneback; external tracking_DLL;

procedure cvUpdateMotionHistory; external tracking_DLL;
procedure cvCalcMotionGradient; external tracking_DLL;
function cvSegmentMotion; external tracking_DLL;
function cvCalcGlobalOrientation; external tracking_DLL;

end.
