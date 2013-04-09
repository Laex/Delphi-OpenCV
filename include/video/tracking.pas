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
  //  opencv\modules\video\include\opencv2\video\tracking.hpp
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
unit tracking;

interface

uses
  Core.types_c, imgproc.types_c;

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

  // CVAPI(void)  cvCalcOpticalFlowPyrLK(
  // const CvArr*  prev,
  // const CvArr*  curr,
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
//
/// * Updates motion history image given motion silhouette */
// CVAPI(void)    cvUpdateMotionHistory( const CvArr* silhouette, CvArr* mhi,
// double timestamp, double duration );
//
/// * Calculates gradient of the motion history image and fills
// a mask indicating where the gradient is valid */
// CVAPI(void)    cvCalcMotionGradient( const CvArr* mhi, CvArr* mask, CvArr* orientation,
// double delta1, double delta2,
// int aperture_size CV_DEFAULT(3));
//
/// * Calculates average motion direction within a selected motion region
// (region can be selected by setting ROIs and/or by composing a valid gradient mask
// with the region mask) */
// CVAPI(double)  cvCalcGlobalOrientation( const CvArr* orientation, const CvArr* mask,
// const CvArr* mhi, double timestamp,
// double duration );
//
/// * Splits a motion history image into a few parts corresponding to separate independent motions
// (e.g. left hand, right hand) */
// CVAPI(CvSeq*)  cvSegmentMotion( const CvArr* mhi, CvArr* seg_mask,
// CvMemStorage* storage,
// double timestamp, double seg_thresh );



// ****************************************************************************************\
// *                                       Tracking                                         *
// ****************************************************************************************/

{
  /* Implements CAMSHIFT algorithm - determines object position, size and orientation
  from the object histogram back project (extension of meanshift) */
  CVAPI(int)  cvCamShift( const CvArr* prob_image, CvRect  window,
  CvTermCriteria criteria, CvConnectedComp* comp,
  CvBox2D* box CV_DEFAULT(NULL) );
}

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
// }
//
// namespace cv
// {
//
/// /! updates motion history image using the current silhouette
// CV_EXPORTS_W void updateMotionHistory( InputArray silhouette, InputOutputArray mhi,
// double timestamp, double duration );
//
/// /! computes the motion gradient orientation image from the motion history image
// CV_EXPORTS_W void calcMotionGradient( InputArray mhi, OutputArray mask,
// OutputArray orientation,
// double delta1, double delta2,
// int apertureSize=3 );
//
/// /! computes the global orientation of the selected motion history image part
// CV_EXPORTS_W double calcGlobalOrientation( InputArray orientation, InputArray mask,
// InputArray mhi, double timestamp,
// double duration );
//
// CV_EXPORTS_W void segmentMotion(InputArray mhi, OutputArray segmask,
// CV_OUT std::vector<Rect>& boundingRects,
// double timestamp, double segThresh);
//
/// /! updates the object tracking window using CAMSHIFT algorithm
// CV_EXPORTS_W RotatedRect CamShift( InputArray probImage, CV_OUT CV_IN_OUT Rect& window,
// TermCriteria criteria );
//
/// /! updates the object tracking window using meanshift algorithm
// CV_EXPORTS_W int meanShift( InputArray probImage, CV_OUT CV_IN_OUT Rect& window,
// TermCriteria criteria );
//
/// *!
// Kalman filter.
//
// The class implements standard Kalman filter \url{http://en.wikipedia.org/wiki/Kalman_filter}.
// However, you can modify KalmanFilter::transitionMatrix, KalmanFilter::controlMatrix and
// KalmanFilter::measurementMatrix to get the extended Kalman filter functionality.
// */
// class CV_EXPORTS_W KalmanFilter
// {
// public:
// //! the default constructor
// CV_WRAP KalmanFilter();
// //! the full constructor taking the dimensionality of the state, of the measurement and of the control vector
// CV_WRAP KalmanFilter(int dynamParams, int measureParams, int controlParams=0, int type=CV_32F);
// //! re-initializes Kalman filter. The previous content is destroyed.
// void init(int dynamParams, int measureParams, int controlParams=0, int type=CV_32F);
//
// //! computes predicted state
// CV_WRAP const Mat& predict(const Mat& control=Mat());
// //! updates the predicted state from the measurement
// CV_WRAP const Mat& correct(const Mat& measurement);
//
// Mat statePre;           //!< predicted state (x'(k)): x(k)=A*x(k-1)+B*u(k)
// Mat statePost;          //!< corrected state (x(k)): x(k)=x'(k)+K(k)*(z(k)-H*x'(k))
// Mat transitionMatrix;   //!< state transition matrix (A)
// Mat controlMatrix;      //!< control matrix (B) (not used if there is no control)
// Mat measurementMatrix;  //!< measurement matrix (H)
// Mat processNoiseCov;    //!< process noise covariance matrix (Q)
// Mat measurementNoiseCov;//!< measurement noise covariance matrix (R)
// Mat errorCovPre;        //!< priori error estimate covariance matrix (P'(k)): P'(k)=A*P(k-1)*At + Q)*/
// Mat gain;               //!< Kalman gain matrix (K(k)): K(k)=P'(k)*Ht*inv(H*P'(k)*Ht+R)
// Mat errorCovPost;       //!< posteriori error estimate covariance matrix (P(k)): P(k)=(I-K(k)*H)*P'(k)
//
// // temporary matrices
// Mat temp1;
// Mat temp2;
// Mat temp3;
// Mat temp4;
// Mat temp5;
// };
//
// enum
// {
// OPTFLOW_USE_INITIAL_FLOW = CV_LKFLOW_INITIAL_GUESSES,
// OPTFLOW_LK_GET_MIN_EIGENVALS = CV_LKFLOW_GET_MIN_EIGENVALS,
// OPTFLOW_FARNEBACK_GAUSSIAN = 256
// };
//
/// /! constructs a pyramid which can be used as input for calcOpticalFlowPyrLK
// CV_EXPORTS_W int buildOpticalFlowPyramid(InputArray img, OutputArrayOfArrays pyramid,
// Size winSize, int maxLevel, bool withDerivatives = true,
// int pyrBorder = BORDER_REFLECT_101, int derivBorder = BORDER_CONSTANT,
// bool tryReuseInputImage = true);
//
/// /! computes sparse optical flow using multi-scale Lucas-Kanade algorithm
// CV_EXPORTS_W void calcOpticalFlowPyrLK( InputArray prevImg, InputArray nextImg,
// InputArray prevPts, InputOutputArray nextPts,
// OutputArray status, OutputArray err,
// Size winSize=Size(21,21), int maxLevel=3,
// TermCriteria criteria=TermCriteria(TermCriteria::COUNT+TermCriteria::EPS, 30, 0.01),
// int flags=0, double minEigThreshold=1e-4);
//
/// /! computes dense optical flow using Farneback algorithm
// CV_EXPORTS_W void calcOpticalFlowFarneback( InputArray prev, InputArray next,
// InputOutputArray flow, double pyr_scale, int levels, int winsize,
// int iterations, int poly_n, double poly_sigma, int flags );
//
/// /! estimates the best-fit Euqcidean, similarity, affine or perspective transformation
/// / that maps one 2D point set to another or one image to another.
// CV_EXPORTS_W Mat estimateRigidTransform( InputArray src, InputArray dst,
// bool fullAffine);
//
// enum
// {
// MOTION_TRANSLATION=0,
// MOTION_EUCLIDEAN=1,
// MOTION_AFFINE=2,
// MOTION_HOMOGRAPHY=3
// };
//
/// /! estimates the best-fit Translation, Euclidean, Affine or Perspective Transformation
/// / with respect to Enhanced Correlation Coefficient criterion that maps one image to
/// / another (area-based alignment)
/// /
/// / see reference:
/// / Evangelidis, G. E., Psarakis, E.Z., Parametric Image Alignment using
/// / Enhanced Correlation Coefficient Maximization, PAMI, 30(8), 2008
//
// CV_EXPORTS_W double findTransformECC(InputArray templateImage,
// InputArray inputImage,
// InputOutputArray warpMatrix,
// int motionType=MOTION_AFFINE,
// TermCriteria criteria=TermCriteria(TermCriteria::COUNT+TermCriteria::EPS, 50, 0.001));
//
//
/// /! computes dense optical flow using Simple Flow algorithm
// CV_EXPORTS_W void calcOpticalFlowSF(InputArray from,
// InputArray to,
// OutputArray flow,
// int layers,
// int averaging_block_size,
// int max_flow);
//
// CV_EXPORTS_W void calcOpticalFlowSF(InputArray from,
// InputArray to,
// OutputArray flow,
// int layers,
// int averaging_block_size,
// int max_flow,
// double sigma_dist,
// double sigma_color,
// int postprocess_window,
// double sigma_dist_fix,
// double sigma_color_fix,
// double occ_thr,
// int upscale_averaging_radius,
// double upscale_sigma_dist,
// double upscale_sigma_color,
// double speed_up_thr);
//
// class CV_EXPORTS DenseOpticalFlow : public Algorithm
// {
// public:
// virtual void calc(InputArray I0, InputArray I1, InputOutputArray flow) = 0;
// virtual void collectGarbage() = 0;
// };
//
/// / Implementation of the Zach, Pock and Bischof Dual TV-L1 Optical Flow method
/// /
/// / see reference:
/// /   [1] C. Zach, T. Pock and H. Bischof, "A Duality Based Approach for Realtime TV-L1 Optical Flow".
/// /   [2] Javier Sanchez, Enric Meinhardt-Llopis and Gabriele Facciolo. "TV-L1 Optical Flow Estimation".
// CV_EXPORTS Ptr<DenseOpticalFlow> createOptFlow_DualTVL1();
//
// }
//
// #endif
//
// #endif

implementation

Uses
  uLibName;

function cvCamShift; external tracking_DLL;
procedure cvCalcOpticalFlowPyrLK; external tracking_DLL;
procedure cvCalcOpticalFlowFarneback; external tracking_DLL;

end.
