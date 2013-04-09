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
  //  opencv\modules\imgproc\include\opencv2\imgproc\imgproc_c.h
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
unit imgproc_c;

interface

uses
  Core.types_c, imgproc.types_c;

(* ********************** Background statistics accumulation **************************** *)

(* Adds image to accumulator *)
// CVAPI(procedure)cvAcc(var Adds squared image to accumulator * )
// CVAPI(procedure)cvSquareAcc(CvArr * image: v1: 0)): CvArr; (var sqsum: CvArr; var Adds a product of two images to accumulator * )
// CVAPI(procedure)cvMultiplyAcc(CvArr * image1: unction mask CV_DEFAULT(v1: 0)): CvArr; (;
// var image2: CvArr; var acc: CvArr; var Adds image to accumulator with weights: acc = acc * (1 - alpha) + image * alpha * )
// CVAPI(procedure)cvRunningAvg(CvArr * image: unction mask CV_DEFAULT(v1: 0)): CvArr; (;
// var acc: CvArr;alpha: Double;

// *******************************  image Processing  *******************************

{
  /* Copies source 2D array inside of the larger destination array and
  makes a border of the specified type (IPL_BORDER_*) around the copied area. */
  CVAPI(void) cvCopyMakeBorder(
  const CvArr* src,
  CvArr* dst,
  CvPoint offset,
  int bordertype,
  CvScalar value CV_DEFAULT(cvScalarAll(0)));
}
procedure cvCopyMakeBorder(
  { } const src: pIplImage;
  { } dst: pIplImage;
  { } offset: TCvPoint;
  { } bordertype: Integer;
  { } value: TCvScalar { * cvScalarAll(0) * } ); cdecl;

{
  // Smoothes array (removes noise)
  CVAPI(void) cvSmooth(
  const CvArr* src,
  CvArr* dst,
  int smoothtype CV_DEFAULT(CV_GAUSSIAN),
  int size1 CV_DEFAULT(3),
  int size2 CV_DEFAULT(0),
  double sigma1 CV_DEFAULT(0),
  double sigma2 CV_DEFAULT(0));
}
procedure cvSmooth(
  { } const src: pIplImage;
  { } dst: pIplImage;
  { } smoothtype: Integer = CV_GAUSSIAN;
  { } size1: Integer = 3;
  { } size2: Integer = 0;
  { } sigma1: double = 0;
  { } sigma2: double = 0); cdecl;

// (* Convolves the image with the kernel *)
// CVAPI(
// procedure)cvFilter2D(v1: CvPoint(-1;
//

{
  Finds integral image: SUM(X,Y) = sum(x<X,y<Y)I(x,y)

  CVAPI(void) cvIntegral(
  const CvArr* image,
  CvArr* sum,
  CvArr* sqsum CV_DEFAULT(NULL),
  CvArr* tilted_sum CV_DEFAULT(NULL));
}
procedure cvIntegral(
  { } const image: pIplImage;
  { } sum: pIplImage;
  { } sqsum: pIplImage = NIL;
  { } tilted_sum: pIplImage = NIL); cdecl;

(*
  Smoothes the input image with gaussian kernel and then down-samples it.
  dst_width = floor(src_width/2)[+1],
  dst_height = floor(src_height/2)[+1]

  CVAPI(void)  cvPyrDown( const CvArr* src, CvArr* dst,
  int filter CV_DEFAULT(CV_GAUSSIAN_5x5) );
*)
procedure cvPyrDown(const src: pIplImage; dst: pIplImage; filter: Integer = CV_GAUSSIAN_5x5); cdecl;

(*

  Up-samples image and smoothes the result with gaussian kernel.
  dst_width = src_width*2,
  dst_height = src_height*2

  CVAPI(void)  cvPyrUp( const CvArr* src, CvArr* dst,
  int filter CV_DEFAULT(CV_GAUSSIAN_5x5) );
*)
procedure cvPyrUp(const src: pIplImage; dst: pIplImage; filter: Integer = CV_GAUSSIAN_5x5); cdecl;

// CVAPI(CvMat * )cvCreatePyramid(const CvArr * img
// : function filter CV_DEFAULT(v1: CV_GAUSSIAN_5x5)): Integer; (; extra_layers: int; rate: Double;
// var layer_sizes CV_DEFAULT(0): vSize; bufarr CV_DEFAULT(v1: 1: function);
// filter CV_DEFAULT(CV_GAUSSIAN_5x5): Integer): Integer;
//
// (* Releases pyramid *)
// CVAPI(procedure)cvReleasePyramid(v1: var Filters image using meanshift algorithm * )
// CVAPI(procedure)cvPyrMeanShiftFiltering(CvArr * src; var dst: CvArr; sp: function; sr: Double;
// var Segments image using seed " markers " * )CVAPI(procedure)cvWatershed(CvArr * image
// : function max_level CV_DEFAULT(v1: cvTermCriteria(CV_TERMCRIT_ITER + CV_TERMCRIT_EPS;
// :;
// v3: ))): Integer; (; var markers): Double;
// (* Calculates an image derivative using generalized Sobel   (aperture_size = 1: CvArr;
// : ;
// : ;
// var )

{
  /* Calculates an image derivative using generalized Sobel
  (aperture_size = 1,3,5,7) or Scharr (aperture_size = -1) operator.
  Scharr can be used only for the first dx or dy derivative */
  CVAPI(void) cvSobel(
  const CvArr* src,
  CvArr* dst,
  int xorder,
  int yorder,
  int aperture_size CV_DEFAULT(3));
}
procedure cvSobel(const src: pIplImage; dst: pIplImage; xorder: Integer; yorder: Integer;
  aperture_size: Integer = 3); cdecl;

{
  /* Calculates the image Laplacian: (d2/dx + d2/dy)I */
  CVAPI(void) cvLaplace(
  const CvArr* src,
  CvArr* dst,
  int aperture_size CV_DEFAULT(3) );
}
procedure cvLaplace(const src: pIplImage; dst: pIplImage; aperture_size: Integer = 3); cdecl;

(* Converts input array pixels from one color space to another *)
// CVAPI(void)  cvCvtColor( const CvArr* src, CvArr* dst, int code );
procedure cvCvtColor(const src: pIplImage; dst: pIplImage; code: Integer); cdecl; overload;
procedure cvCvtColor(const src: pCvMat; dst: pCvMat; code: Integer); cdecl; overload;
procedure cvCvtColor(const src: pIplImage; dst: pCvMat; code: Integer); cdecl; overload;

// (* Resizes image (input array is resized to fit the destination array) *)
// CVAPI(procedure)cvResize(var Warps image with affine transform * )
{
  CVAPI(void)  cvResize( const CvArr* src, CvArr* dst,
  int interpolation CV_DEFAULT( CV_INTER_LINEAR ));
}
procedure cvResize(const src: pIplImage; dst: pIplImage; interpolation: Integer = CV_INTER_LINEAR); cdecl;

{
  /* Warps image with affine transform */
  CVAPI(void)  cvWarpAffine( const CvArr* src, CvArr* dst, const CvMat* map_matrix,
  int flags CV_DEFAULT(CV_INTER_LINEAR+CV_WARP_FILL_OUTLIERS),
  CvScalar fillval CV_DEFAULT(cvScalarAll(0)) );
}
procedure cvWarpAffine(const src: pIplImage; dst: pIplImage; const map_matrix: pCvMat;
  flags: Integer { = CV_INTER_LINEAR+CV_WARP_FILL_OUTLIERS }; fillval: TCvScalar { = cvScalarAll(0) } ); cdecl;

// CVAPI(CvMat)cvGetAffineTransform(CvPoint2D32f * src: );
// var dst: vPoint2D32f; var map_matrix: CvMat);
//
{
  (* Computes rotation_matrix matrix *)
  CVAPI(CvMat)cv2DRotationMatrix(CvPoint2D32f center, Double angle, Double scale, CvMat * map_matrix);
}
function cv2DRotationMatrix(center: TCvPoint2D32f; angle: double; scale: double; map_matrix: pCvMat): pCvMat; cdecl;

{
  /* Warps image with perspective (projective) transform */
  CVAPI(void)  cvWarpPerspective( const CvArr* src, CvArr* dst, const CvMat* map_matrix,
  int flags CV_DEFAULT(CV_INTER_LINEAR+CV_WARP_FILL_OUTLIERS),
  CvScalar fillval CV_DEFAULT(cvScalarAll(0)) );
}
procedure cvWarpPerspective(const src: pIplImage; dst: pIplImage; const map_matrix: pCvMat;
  flags: Integer { =CV_INTER_LINEAR+CV_WARP_FILL_OUTLIERS }; fillval: TCvScalar { =cvScalarAll(0) } ); cdecl;
{
  /* Computes perspective transform matrix for mapping src[i] to dst[i] (i=0,1,2,3) */
  CVAPI(CvMat*) cvGetPerspectiveTransform( const CvPoint2D32f* src,
  const CvPoint2D32f* dst,
  CvMat* map_matrix );

}
function cvGetPerspectiveTransform(const src: pCvPoint2D32f; const dst: pCvPoint2D32f; map_matrix: pCvMat)
  : pCvMat; cdecl;
{
  /* Performs generic geometric transformation using the specified coordinate maps */
  CVAPI(void)  cvRemap(
  const CvArr* src,
  CvArr* dst,
  const CvArr* mapx,
  const CvArr* mapy,
  int flags CV_DEFAULT(CV_INTER_LINEAR+CV_WARP_FILL_OUTLIERS),
  CvScalar fillval CV_DEFAULT(cvScalarAll(0)) );
}
procedure cvRemap(const src: pIplImage; dst: pIplImage; const mapx: pIplImage; const mapy: pIplImage;
  flags: Integer { =CV_INTER_LINEAR+CV_WARP_FILL_OUTLIERS }; fillval: TCvScalar { =cvScalarAll(0) }
  ); cdecl;


// (* Performs forward or inverse log-polar image transform *)
// CVAPI(
// procedure)cvLogPolar(var Performs forward or inverse linear - polar image transform * )CVAPI(
// procedure)cvLinearPolar(CvArr * src: v1: CV_INTER_LINEAR + CV_WARP_FILL_OUTLIERS)): Integer; (;
// var dst: CvArr; center: CvPoint2D32f; maxRadius: Double;
// var Transforms the input image to compensate lens distortion * )CVAPI(
// procedure)cvUndistort2(CvArr * src:
// function flags CV_DEFAULT(v1: CV_INTER_LINEAR + CV_WARP_FILL_OUTLIERS)): Integer; (; var dst: CvArr;
// var camera_matrix: vMat; var distortion_coeffs: vMat; var new_camera_matrix CV_DEFAULT(0): vMat);
//

{
  /* Computes transformation map from intrinsic camera parameters
  that can used by cvRemap */
  CVAPI(void) cvInitUndistortMap(
  const CvMat* camera_matrix,
  const CvMat* distortion_coeffs,
  CvArr* mapx,
  CvArr* mapy );
}
procedure cvInitUndistortMap(const camera_matrix: pCvMat; const distortion_coeffs: pCvMat; mapx: pIplImage;
  mapy: pIplImage); cdecl;


// (* Computes undistortion+rectification map for a head of stereo camera *)
// CVAPI(
// procedure)cvInitUndistortRectifyMap(var camera_matrix: CvMat; var dist_coeffs: vMat;
// var = new_camera_matrix: onst CvMat; var } CvArr * mapx: {$EXTERNALSYM CvMat;
// var mapy: CvArr);
//
// (* Computes the original (undistorted) feature coordinates
// from the observed (distorted) coordinates *)
// CVAPI(procedure) cvUndistortPoints(
// v1: 0);
// var P CV_DEFAULT(0): vMat);
//
// (* creates structuring element used for morphological operations *)
// CVAPI(IplConvKernel)  cvCreateStructuringElementEx(
// Integer cols, Integer  rows, Integer  anchor_x, Integer  anchor_y,
// function shape, Integer values CV_DEFAULT(v1: 0)): Integer;

// CVAPI(IplConvKernel*)  cvCreateStructuringElementEx(
// int cols, int  rows, int  anchor_x, int  anchor_y,
// int shape, int* values CV_DEFAULT(NULL) );
function cvCreateStructuringElementEx(cols: Integer; rows: Integer; anchor_x: Integer; anchor_y: Integer;
  shape: Integer; values: pInteger = nil): pIplConvKernel; cdecl;

// (* releases structuring element *)
// CVAPI(procedure)  cvReleaseStructuringElement( element: array of IplConvKernel);
// CVAPI(void)  cvReleaseStructuringElement( IplConvKernel** element );
procedure cvReleaseStructuringElement(Var element: pIplConvKernel); cdecl;


//
// (* erodes input image (applies minimum filter) one or more times.
// If element cPointer is 0, 3x3 rectangular element is used *)
// CVAPI(procedure)  cvErode(
// v1: 0);
// var dilates input image (applies maximum filter) one or more times.   If element cPointer is 0: function iterations CV_DEFAULT(v1: 1)): Integer;(;
// var )

// CVAPI(procedure)  cvDilate(  CvArr* src: 3x3 rectangular element is used;
// var dst: CvArr;
// var element CV_DEFAULT(0): IplConvKernel;
// var Performs complex morphological transformation *)

// CVAPI(procedure)  cvMorphologyEx(  CvArr* src: function iterations CV_DEFAULT(v1: 1)): Integer;(;
// var dst: CvArr;
// var temp: CvArr;
// var element: IplConvKernel;
// operation: function;
// var Calculates all spatial and central moments up to the 3rd order *)

{ Performs complex morphological transformation }
// CVAPI(void)  cvMorphologyEx( const CvArr* src, CvArr* dst,
// CvArr* temp, IplConvKernel* element,
// int operation, int iterations CV_DEFAULT(1) );
procedure cvMorphologyEx(const src: pIplImage; dst: pIplImage; temp: pIplImage; element: pIplConvKernel;
  operation: Integer; iterations: Integer = 1); cdecl;


// CVAPI(procedure) cvMoments(  CvArr* arr: Integer iterations CV_DEFAULT(v1: 1)): Integer;(;
// var moments: CvMoments;
// binary CV_DEFAULT(0): Integer);

{ erodes input image (applies minimum filter) one or more times.
  If element pointer is NULL, 3x3 rectangular element is used }
// CVAPI(void)  cvErode( const CvArr* src, CvArr* dst,
// IplConvKernel* element CV_DEFAULT(NULL),
// int iterations CV_DEFAULT(1) );
procedure cvErode(const src: pIplImage; dst: pIplImage; element: pIplConvKernel = nil; iterations: Integer = 1); cdecl;

{ dilates input image (applies maximum filter) one or more times.
  If element pointer is NULL, 3x3 rectangular element is used }
// CVAPI(void)  cvDilate( const CvArr* src, CvArr* dst,
// IplConvKernel* element CV_DEFAULT(NULL),
// int iterations CV_DEFAULT(1) );
procedure cvDilate(const src: pIplImage; dst: pIplImage; element: pIplConvKernel = nil; iterations: Integer = 1); cdecl;

//
// (* Retrieve particular spatial, central or normalized central moments *)
// CVAPI(Double)  cvGetSpatialMoment( CvMoments* moments, Integer x_order, Integer y_order );
// CVAPI(Double)  cvGetCentralMoment( CvMoments* moments, Integer x_order, Integer y_order );
// CVAPI(Double)  cvGetNormalizedCentralMoment( CvMoments* moments,
// Integer x_order, Integer y_order );
//
// (* Calculates 7 Hu's invariants from precalculated spatial and central moments */
// CVAPI(procedure) cvGetHuMoments(var moments: CvMoments; var hu_moments: CvHuMoments);
//
// (*********************************** data sampling **************************************)
//
// (* Fetches pixels that belong to the specified line segment and stores them to the buffer.
// Returns the number of retrieved points. *)
// CVAPI(Integer)  cvSampleLine(  CvArr* image, CvPoint pt1, CvPoint pt2, Pointer  buffer,
// function connectivity CV_DEFAULT(v1: 8)): Integer;
//
// (* Retrieves the rectangular image region with specified center from the input array.
// dst(x,y) <- src(x + center.x - dst_width/2, y + center.y - dst_height/2).
// Values of pixels with fractional coordinates are retrieved using bilinear interpolation*)
// CVAPI(procedure)  cvGetRectSubPix(var src: CvArr; var dst: CvArr; center: CvPoint2D32f);
//
//
// (* Retrieves quadrangle from the input array.
// = ( a11  a12 or b1 )   dst(x,y) <- src(A : array[0..x y-1] of matrixarr' + b)
// ( a21  a22 or b2 )   (bilinear interpolation is used to retrieve pixels
// with fractional coordinates)
// *)
// CVAPI(procedure)  cvGetQuadrangleSubPix(
// var src: CvArr;
// var dst: CvArr;
// var map_matrix: vMat);
//
// (* Measures similarity between template and overlapped windows in the source image
// and fills the resultant image with the measurements *)
// CVAPI(procedure)  cvMatchTemplate(
// var image: CvArr;
// var templ: CvArr;
// var cResult: CvArr;
// method: Integer);
//
// (* Computes earth mover distance between
// two weighted point sets (called signatures) *)
// CVAPI(Single)  cvCalcEMD2(  CvArr* signature1,
// CvArr* signature2,
// Integer distance_type,
// CvDistanceFunction distance_func CV_DEFAULT(0),
// function  cost_matrix CV_DEFAULT(
// v1: 0);
// lower_bound CV_DEFAULT(0): function;
// userdata CV_DEFAULT(0): function): Single;
//
// (****************************************************************************************\
// *                              Contours retrieving                                       *
// ****************************************************************************************)

Type
  PCvContour = ^TCvContour;

  TCvContour = packed record
    flags: Integer; // * micsellaneous flags */          \
    header_size: Integer; // * size of sequence header */      \
    h_prev: PCvSeq; // * previous sequence */        \
    h_next: PCvSeq; // * next sequence */            \
    v_prev: PCvSeq; // * 2nd previous sequence */    \
    v_next: PCvSeq; // * 2nd next sequence */
    total: Integer; // * total number of elements */            \
    elem_size: Integer; // * size of sequence element in bytes */   \
    block_max: PAnsiChar; // * maximal bound of the last block */     \
    ptr: PAnsiChar; // * current write pointer */               \
    delta_elems: Integer; // * how many elements allocated when the seq grows */  \
    storage: PCvMemStorage; // * where the seq is stored */             \
    free_blocks: PCvSeqBlock; // * free blocks list */                    \
    first: PCvSeqBlock; // * pointer to the first sequence block */
    rect: TCvRect;
    color: Integer;
    reserved: array [0 .. 2] of Integer;
  end;

Const
  // * contour retrieval mode */
  CV_RETR_EXTERNAL = 0;
  CV_RETR_LIST = 1;
  CV_RETR_CCOMP = 2;
  CV_RETR_TREE = 3;

  // * contour approximation method */
  CV_CHAIN_CODE = 0;
  CV_CHAIN_APPROX_NONE = 1;
  CV_CHAIN_APPROX_SIMPLE = 2;
  CV_CHAIN_APPROX_TC89_L1 = 3;
  CV_CHAIN_APPROX_TC89_KCOS = 4;
  CV_LINK_RUNS = 5;

  {
    /* Retrieves outer and optionally inner boundaries of white (non-zero) connected
    components in the black (zero) background */
    CVAPI(int)  cvFindContours(
    CvArr* image,
    CvMemStorage* storage,
    CvSeq** first_contour,
    int header_size CV_DEFAULT(sizeof(CvContour)),
    int mode CV_DEFAULT(CV_RETR_LIST),
    int method CV_DEFAULT(CV_CHAIN_APPROX_SIMPLE),
    CvPoint offset CV_DEFAULT(cvPoint(0,0)));
  }

function cvFindContours(
  { } image: pIplImage;
  { } storage: PCvMemStorage;
  { } first_contour: PCvSeq;
  { } header_size: Integer { = SizeOf(TCvContour) };
  { } mode: Integer { = CV_RETR_LIST };
  { } method: Integer { = CV_CHAIN_APPROX_SIMPLE };
  { } offset: TCvPoint { =cvPoint(0,0) } ): Integer; cdecl;

//
// (* Initalizes contour retrieving process.
// Calls cvStartFindContours.
// Calls cvFindNextContour until null cPointer is returned
// or some other condition becomes true.
// Calls cvEndFindContours at the end. *)
// CVAPI(CvContourScanner)  cvStartFindContours( CvArr* image, CvMemStorage* storage,
// function header_size CV_DEFAULT(
// v1: CvContour));
// mode CV_DEFAULT(CV_RETR_LIST): Integer;
// method CV_DEFAULT(CV_CHAIN_APPROX_SIMPLE): Integer;
// offset CV_DEFAULT(cvPoint(0: CvPoint;
// v5: ))): Integer;
//
// (* Retrieves next contour *)
// CVAPI(CvSeq)  cvFindNextContour( CvContourScanner scanner ): Pointer;
//
//
// (* Substitutes the last retrieved contour with the new one
// (if the substitutor is null, the last retrieved contour is removed from the tree) *) then
// CVAPI(procedure)   cvSubstituteContour(
// v1: var Releases contour scanner and returns pointer to the first outer contour *)CVAPI(CvSeq)  cvEndFindContours( CvContourScanner* scanner);
//
// (* Approximates a single Freeman chain or a tree of chains to polygonal curves *)
// CVAPI(CvSeq) cvApproxChains( CvSeq* src_seq, CvMemStorage* storage,
// function method CV_DEFAULT(
// v1: 0);
// minimal_perimeter CV_DEFAULT(0): Integer;
// recursive CV_DEFAULT(0): Integer): Integer;
//
// (* Initalizes Freeman chain reader.
// The reader is used to iteratively get coordinates of all the chain points.
// If the Freeman codes should be read as is, a simple sequence reader should be used *)
// CVAPI(procedure) cvStartReadChainPoints(
// v1: var Retrieves the next chain point *)CVAPI(CvPoint) cvReadChainPoint( CvChainPtReader* reader);
//
//
// (****************************************************************************************\
// *                            Contour Processing and Shape Analysis                       *
// ****************************************************************************************)
{
  /* Approximates a single polygonal curve (contour) or
  a tree of polygonal curves (contours) */
  CVAPI(CvSeq*)  cvApproxPoly(
  const void* src_seq,
  int header_size,
  CvMemStorage* storage,
  int method,
  double eps,
  int recursive CV_DEFAULT(0));
}
function cvApproxPoly(
  { } const src_seq: PCvSeq;
  { } header_size: Integer;
  { } storage: PCvMemStorage;
  { } method: Integer;
  { } eps: double;
  { } recursive: Integer = 0): PCvSeq; cdecl;

(*
  /* Calculates perimeter of a contour or length of a part of contour */
  CVAPI(double)  cvArcLength( const void* curve,
  CvSlice slice CV_DEFAULT(CV_WHOLE_SEQ),
  int is_closed CV_DEFAULT(-1));
*)

function cvArcLength(const curve: Pointer; slice: TCvSlice { = CV_WHOLE_SEQ }; is_closed: Integer { = 1 } )
  : double; cdecl;

(*
  CV_INLINE double cvContourPerimeter( const void* contour )
  {
  return cvArcLength( contour, CV_WHOLE_SEQ, 1 );
  }
*)

function cvContourPerimeter(const contour: Pointer): double; inline;

// (* Calculates contour boundning rectangle (update=1) or
// just retrieves pre-calculated rectangle (update=0) *)
// CVAPI(CvRect)  cvBoundingRect( CvArr* points, Integer update CV_DEFAULT(0) );


// * Calculates area of a contour or contour segment */
// CVAPI(double)  cvContourArea( const CvArr* contour,
// CvSlice slice CV_DEFAULT(CV_WHOLE_SEQ),
// int oriented CV_DEFAULT(0));
function cvContourArea(const contour: PCvSeq; slice: TCvSlice { = CV_WHOLE_SEQ }; oriented: Integer = 0): double; cdecl;

// (* Finds minimum area rotated rectangle bounding a set of points *)
// CVAPI(CvBox2D)  cvMinAreaRect2( const CvArr* points, CvMemStorage* storage CV_DEFAULT(NULL));
function cvMinAreaRect2(points: PCvSeq; storage: PCvMemStorage = nil): TCvBox2D; cdecl;

// (* Finds minimum enclosing circle for a set of points *)
// CVAPI(int)  cvMinEnclosingCircle( const CvArr* points,CvPoint2D32f* center, float* radius );
function cvMinEnclosingCircle(points: PCvSeq; center: pCvPoint2D32f; radius: pSingle): Integer; cdecl;

{
  /* Compares two contours by matching their moments */
  CVAPI(double)  cvMatchShapes( const void* object1, const void* object2,
  int method, double parameter CV_DEFAULT(0));
}
function cvMatchShapes(const object1: Pointer; const object2: Pointer; method: Integer; parameter: double = 0)
  : double; cdecl;

{
  /* Calculates exact convex hull of 2d point set */
  CVAPI(CvSeq*) cvConvexHull2( const CvArr* input,
  void* hull_storage CV_DEFAULT(NULL),
  int orientation CV_DEFAULT(CV_CLOCKWISE),
  int return_points CV_DEFAULT(0));
}
function cvConvexHull2(const input: PCvSeq; hull_storage: Pointer = nil; orientation: Integer = CV_CLOCKWISE;
  return_points: Integer = 0): PCvSeq; cdecl;

{
  /* Checks whether the contour is convex or not (returns 1 if convex, 0 if not) */
  CVAPI(int)  cvCheckContourConvexity( const CvArr* contour );
}
function cvCheckContourConvexity(const contour: PCvSeq): Integer; cdecl;
{
  (* Finds convexity defects for the contour *)
  CVAPI(CvSeq)  cvConvexityDefects(  CvArr* contour,  CvArr* convexhull,
  CvMemStorage* storage CV_DEFAULT(0)): Pointer;
}
function cvConvexityDefects(contour: PCvSeq; convexhull: PCvSeq; storage: PCvMemStorage = nil): PCvSeq; cdecl;

// (* Fits ellipse into a set of 2d points *)
// CVAPI(CvBox2D) cvFitEllipse2(  CvArr* points );
//
// (* Finds minimum rectangle containing two given rectangles *)
// CVAPI(CvRect)  cvMaxRect(  CvRect* rect1,  CvRect* rect2 );

Type
  TBoxPoints = array [0 .. 3] of TCvPoint2D32f;
  // (* Finds coordinates of the box vertices *)
  // CVAPI(void) cvBoxPoints( CvBox2D box, CvPoint2D32f pt[4] );
procedure cvBoxPoints(box: TCvBox2D; pt: TBoxPoints); cdecl;


// (* Initializes sequence header for a matrix (column or row vector) of points -
// a wrapper for cvMakeSeqHeaderForArray (it does not initialize bounding rectangle not  not  not ) *)
// CVAPI(CvSeq) cvPointSeqFromMat( Integer seq_kind,  CvArr* mat,
// CvContour* contour_header,
// CvSeqBlock* block );
//
// (* Checks whether the point is inside polygon, outside, on an edge (at a vertex).
// Returns positive, negative or zero value, correspondingly.
// Optionally, measures a Integer distance between
// the point and the nearest polygon edge (measure_dist=1) *)
// CVAPI(Double) cvPointPolygonTest(  CvArr* contour,
// CvPoint2D32f pt, Integer measure_dist );
//
// (****************************************************************************************\
// *                                  Histogram functions                                   *
// ****************************************************************************************)

{
  /* Creates new histogram */
  CVAPI(CvHistogram*)  cvCreateHist( int dims, int* sizes, int type,
  float** ranges CV_DEFAULT(NULL),
  int uniform CV_DEFAULT(1));
}
function cvCreateHist(dims: Integer; sizes: pInteger; _type: Integer; ranges: pSingleArray2D = nil;
  uniform: Integer = 1): pCvHistogram; cdecl;

// (* Assignes histogram bin ranges *)
// CVAPI(procedure)  cvSetHistBinRanges(
// var Creates histogram header for array *)CVAPI(CvHistogram)  cvMakeHistHeaderForArray(                            Integer  dims: v1: 1)): Integer;(;
// var sizes: Integer;
// var hist: CvHistogram;
// var function: Single;
// var ranges CV_DEFAULT(v1: 1)): Integer;(* Releases histogram *)CVAPI(procedure)  cvReleaseHist( CvHistogram** hist ): Single;(* Clears all the histogram bins *)CVAPI(procedure)  cvClearHist( CvHistogram* hist ): data;(* Finds indices and values of minimum and maximum histogram bins *)

{
  /* Finds indices and values of minimum and maximum histogram bins */
  CVAPI(void)  cvGetMinMaxHistValue( const CvHistogram* hist,
  float* min_value, float* max_value,
  int* min_idx CV_DEFAULT(NULL),
  int* max_idx CV_DEFAULT(NULL));
}
procedure cvGetMinMaxHistValue(const hist: pCvHistogram; min_value: pSingle; max_value: pSingle;
  min_idx: pInteger = nil; max_idx: pInteger = nil); cdecl;

// (* Clear all histogram bins that are below the threshold *)
// CVAPI(procedure)  cvThreshHist(var hist: CvHistogram; threshold: Double);
//
//
// (* Compares two histogram *)
// CVAPI(Double)  cvCompareHist(  CvHistogram* hist1,
// CvHistogram* hist2,
// Integer method);
//
// (* Copies one histogram to another. Destination histogram is created if
// the destination cPointer is 0 *)
// CVAPI(procedure)  cvCopyHist(var src: CvHistogram;  dst: array of CvHistogram);
//
//
// (* Calculates bayesian probabilistic histograms
// (each or src and dst is an cArray of <number> histograms *)
// CVAPI(procedure)  cvCalcBayesianProb(
// src: array of CvHistogram;
// number: Integer;
// dst: array of CvHistogram);

{
  /* Calculates array histogram */
  CVAPI(void)  cvCalcArrHist( CvArr** arr, CvHistogram* hist,
  int accumulate CV_DEFAULT(0),
  const CvArr* mask CV_DEFAULT(NULL) );
}
procedure cvCalcArrHist(arr: ppIplImage; hist: pCvHistogram; accumulate: Integer = 0;
  const mask: pIplImage = nil); cdecl;

// CV_INLINE  void  cvCalcHist(
// IplImage** image,
// CvHistogram* hist,
// int accumulate CV_DEFAULT(0),
// const CvArr* mask CV_DEFAULT(NULL) )
// {
// cvCalcArrHist( (CvArr**)image, hist, accumulate, mask );
// }
procedure cvCalcHist(image: ppIplImage; hist: pCvHistogram; accumulate: Integer = 0;
  const mask: pIplImage = nil); inline;


// var mask CV_DEFAULT(0) )begin     cvCalcArrHist( (CvArr*)image: vArr;
// v5: hist;
// v6: accumulate;
// var Calculates back project *)

{
  /* Calculates back project */
  CVAPI(void)  cvCalcArrBackProject( CvArr** image, CvArr* dst,
  const CvHistogram* hist );
  #define  cvCalcBackProject(image, dst, hist) cvCalcArrBackProject((CvArr**)image, dst, hist)
}
procedure cvCalcArrBackProject(var image: pCvArr; dst: pCvArr; const hist: pCvHistogram); cdecl;
procedure cvCalcBackProject(var image: pIplImage; dst: pIplImage; const hist: pCvHistogram); cdecl;

// (* Does some sort of template matching but compares histograms of
// template and each window location *)
// CVAPI(procedure)  cvCalcArrBackProjectPatch(
// image: array of CvArr;
// var dst: CvArr;
// range: CvSize;
// var hist: CvHistogram;
// method: Integer;
// factor: Double);
/// / >> Following declaration is a macro definition!
// const cvCalcBackProjectPatch( image, dst, range, hist, method, factor )  cvCalcArrBackProjectPatch( (CvArr;
//



// (* calculates probabilistic density (divides one histogram by another) *)
// CVAPI(procedure)  cvCalcProbDensity(

{ /* equalizes histogram of 8-bit single-channel image */
  CVAPI(void)  cvEqualizeHist( const CvArr* src, CvArr* dst );
}

procedure cvEqualizeHist(const src, dst: pIplImage); cdecl;

//
//
// (* Applies distance transform to binary image *)
// CVAPI(procedure)  cvDistTransform(
// 3: v1:);
// mask CV_DEFAULT(0): unction;
// labels CV_DEFAULT(0): function;
// labelType CV_DEFAULT(CV_DIST_LABEL_CCOMP): Integer): Integer;
//
//
// (* Applies fixed-level threshold to grayscale image.
// This is a basic operation applied before retrieving contours *)
// CVAPI(double)  cvThreshold( const CvArr*  src, CvArr*  dst, double  threshold, double  max_value, int threshold_type );
function cvThreshold(const src, dst: pIplImage; threshold, max_value: double; threshold_type: Integer): double; cdecl;
{
  /* Applies adaptive threshold to grayscale image.
  The two parameters for methods CV_ADAPTIVE_THRESH_MEAN_C and
  CV_ADAPTIVE_THRESH_GAUSSIAN_C are:
  neighborhood size (3, 5, 7 etc.),
  and a constant subtracted from mean (...,-3,-2,-1,0,1,2,3,...) */
  CVAPI(void)  cvAdaptiveThreshold(
  const CvArr* src,
  CvArr* dst,
  double max_value,
  int adaptive_method CV_DEFAULT(CV_ADAPTIVE_THRESH_MEAN_C),
  int threshold_type CV_DEFAULT(CV_THRESH_BINARY),
  int block_size CV_DEFAULT(3),
  double param1 CV_DEFAULT(5));
}
procedure cvAdaptiveThreshold(
  { } const src: pIplImage;
  { } dst: pIplImage;
  { } max_value: double;
  { } adaptive_method: Integer = CV_ADAPTIVE_THRESH_MEAN_C;
  { } threshold_type: Integer = CV_THRESH_BINARY;
  { } block_size: Integer = 3;
  { } param1: double = 5); cdecl;

{
  /* Fills the connected component until the color difference gets large enough */
  CVAPI(void)  cvFloodFill(
  CvArr* image,
  CvPoint seed_point,
  CvScalar new_val,
  CvScalar lo_diff CV_DEFAULT(cvScalarAll(0)),
  CvScalar up_diff CV_DEFAULT(cvScalarAll(0)),
  CvConnectedComp* comp CV_DEFAULT(NULL),
  int flags CV_DEFAULT(4),
  CvArr* mask CV_DEFAULT(NULL));
}
procedure cvFloodFill(
  { } image: pIplImage;
  { } seed_point: TCvPoint;
  { } new_val: TCvScalar;
  { } lo_diff: TCvScalar { * cvScalarAll(0) * };
  { } up_diff: TCvScalar { * cvScalarAll(0) * };
  { } comp: pCvConnectedComp = NIL;
  { } flags: Integer = 4;
  { } mask: pCvArr = NIL); cdecl;

// ****************************************************************************************
// *                                  Feature detection                                   *
// ****************************************************************************************
{
  /* Runs canny edge detector */
  CVAPI(void)  cvCanny(
  const CvArr* image,
  CvArr* edges,
  double threshold1,
  double threshold2,
  int  aperture_size CV_DEFAULT(3) );
}
procedure cvCanny(const image: pIplImage; edges: pIplImage; threshold1: double; threshold2: double;
  aperture_size: Integer = 3); cdecl;

// (* Runs canny edge detector *) CVAPI(
// procedure)cvCanny(CvArr * image: array of
// function flags CV_DEFAULT(v1: 0)): Integer; (; var edges: CvArr; threshold1: Double;
// threshold2: Double; var Calculates constraint image for corner detection Dx xor 2 * Dyy + Dxx *
// Dy xor 2 - 2 * Dx * Dy * Dxy.Applying threshold to the cResult gives coordinates of
// corners * )
// CVAPI(
// procedure)cvPreCornerDetect(CvArr * image:
// function aperture_size CV_DEFAULT(v1: 3)): Integer; (; var corners: CvArr;
// var Calculates eigen values and vectors of 2 x2 gradient covariation matrix at every image
// pixel * )CVAPI(
// procedure)cvCornerEigenValsAndVecs(CvArr * image:
// function aperture_size CV_DEFAULT(v1: 3)): Integer; (; var eigenvv: CvArr; block_size:
// function; var Calculates minimal eigenvalue for 2 x2 gradient covariation matrix at every image
// pixel * )CVAPI(
// procedure)cvCornerMinEigenVal(CvArr * image: Integer aperture_size CV_DEFAULT(v1: 3)): Integer; (;
// var eigenval: CvArr; block_size:
// function; var Harris corner detector: Calculates det(M) - k * (trace(M) xor 2)
// : Integer aperture_size CV_DEFAULT(v1: 3)): Integer; (; var)CVAPI(
// procedure)cvCornerHarris(CvArr * image: where M is 2 x2 gradient covariation matrix for each pixel;
// var harris_responce: CvArr; block_size:

{
  /* Adjust corner position using some sort of gradient search */
  CVAPI(void)  cvFindCornerSubPix(
  const CvArr* image,
  CvPoint2D32f* corners,
  int count,
  CvSize win,
  CvSize zero_zone,
  CvTermCriteria  criteria );
}
procedure cvFindCornerSubPix(const image: pIplImage; corners: pCvPoint2D32f; count: Integer; win: TCvSize;
  zero_zone: TCvSize; criteria: TCvTermCriteria); cdecl;


// function; var Adjust corner position using some sort of gradient search * )CVAPI(
// procedure)cvFindCornerSubPix(CvArr * image: Integer aperture_size CV_DEFAULT(v1: 0.04)): Integer; (;
// var corners: CvPoint2D32f; count: Integer; win: CvSize; zero_zone: CvSize;
// var Finds a sparse set of points within the selected region that seem to be easy to track * )

{
  /* Finds a sparse set of points within the selected region
  that seem to be easy to track */
  CVAPI(void)  cvGoodFeaturesToTrack( const CvArr* image, CvArr* eig_image,
  CvArr* temp_image, CvPoint2D32f* corners,
  int* corner_count, double  quality_level,
  double  min_distance,
  const CvArr* mask CV_DEFAULT(NULL),
  int block_size CV_DEFAULT(3),
  int use_harris CV_DEFAULT(0),
  double k CV_DEFAULT(0.04) );
}
procedure cvGoodFeaturesToTrack(const image: pIplImage; eig_image: pIplImage; temp_image: pIplImage;
  corners: pCvPoint2D32f; corner_count: pInteger; quality_level: double; min_distance: double;
  const mask: pIplImage = nil; block_size: Integer = 3; use_harris: Integer = 0; k: double = 0.04); cdecl;


// (* Finds lines on binary image using one of several methods.
// line_storage is either memory storage or 1 x <max number of lines> CvMat, its
// number of columns is changed by the cFunction.
// method is one of CV_HOUGH_*;
// rho, theta and threshold are used for each of those methods;
// param1 ~ line length, param2 ~ line gap - for probabilistic,
// param1 ~ srn, param2 ~ stn - for multi-scale *)
// CVAPI(CvSeq)cvHoughLines2(CvArr * image, Pointer line_storage, Integer method, Double rho,
// Double theta, Integer threshold, Double param1 CV_DEFAULT(0), Double param2 CV_DEFAULT(0)
// ): Double;

{
  /* Finds lines on binary image using one of several methods.
  line_storage is either memory storage or 1 x <max number of lines> CvMat, its
  number of columns is changed by the function.
  method is one of CV_HOUGH_*;
  rho, theta and threshold are used for each of those methods;
  param1 ~ line length, param2 ~ line gap - for probabilistic,
  param1 ~ srn, param2 ~ stn - for multi-scale */

  CVAPI(CvSeq*)  cvHoughLines2(
  CvArr* image,
  void* line_storage,
  int method,
  double rho,
  double theta,
  int threshold,
  double param1 CV_DEFAULT(0),
  double param2 CV_DEFAULT(0));
}

function cvHoughLines2(
  { } image: pIplImage;
  { } line_storage: Pointer;
  { } method: Integer;
  { } rho: double;
  { } theta: double;
  { } threshold: Integer;
  { } param1: double = 0;
  { } param2: double = 0): PCvSeq; cdecl;

{
  /* Finds circles in the image */
  CVAPI(CvSeq*) cvHoughCircles(
  CvArr* image,
  void* circle_storage,
  int method,
  double dp,
  double min_dist,
  double param1 CV_DEFAULT(100),
  double param2 CV_DEFAULT(100),
  int min_radius CV_DEFAULT(0),
  int max_radius CV_DEFAULT(0));
}

function cvHoughCircles(
  { } image: pIplImage;
  { } circle_storage: Pointer;
  { } method: Integer;
  { } dp: double;
  { } min_dist: double;
  { } param1: double = 100;
  { } param2: double = 100;
  { } min_radius: Integer = 0;
  { } max_radius: Integer = 0): PCvSeq; cdecl;

// (* Fits a line into set of 2d or 3d points in a robust way (M-estimator technique) *)
// CVAPI(
// procedure)cvFitLine(CvArr * points, Integer dist_type, Double param, Double reps, Double aeps,
// Single * line): Double;
//
// {$IFDEF __cplusplus}
// end;
// {$ENDIF}
// {$ENDIF}
implementation

Uses uLibName;

// procedure cvCvtColor(const src: pIplImage; dst: pIplImage; code: Integer); external imgproc_Dll;
procedure cvCvtColor(const src: pIplImage; dst: pIplImage; code: Integer); external imgproc_Dll name 'cvCvtColor';
procedure cvCvtColor(const src: pCvMat; dst: pCvMat; code: Integer); external imgproc_Dll name 'cvCvtColor';
procedure cvCvtColor(const src: pIplImage; dst: pCvMat; code: Integer); external imgproc_Dll name 'cvCvtColor';

function cvThreshold; external imgproc_Dll;
procedure cvSmooth; external imgproc_Dll;
procedure cvResize; external imgproc_Dll;
function cvCreateStructuringElementEx; external imgproc_Dll;
procedure cvErode; external imgproc_Dll;
procedure cvDilate; external imgproc_Dll;
procedure cvReleaseStructuringElement; external imgproc_Dll;
procedure cvMorphologyEx; external imgproc_Dll;
procedure cvFloodFill; external imgproc_Dll;
procedure cvAdaptiveThreshold; external imgproc_Dll;
procedure cvCopyMakeBorder; external imgproc_Dll;
procedure cvSobel; external imgproc_Dll;
procedure cvLaplace; external imgproc_Dll;
procedure cvCanny; external imgproc_Dll;
function cvHoughLines2; external imgproc_Dll;
function cvHoughCircles; external imgproc_Dll;
procedure cvIntegral; external imgproc_Dll;
function cvFindContours; external imgproc_Dll;
function cvApproxPoly; external imgproc_Dll;
procedure cvEqualizeHist; external imgproc_Dll;
procedure cvFindCornerSubPix; external imgproc_Dll;
procedure cvInitUndistortMap; external imgproc_Dll;
procedure cvRemap; external imgproc_Dll;
function cvArcLength; external imgproc_Dll;

function cvContourPerimeter(const contour: Pointer): double; inline;
begin
  Result := cvArcLength(contour, CV_WHOLE_SEQ, 1);
end;

function cvMatchShapes; external imgproc_Dll;
function cv2DRotationMatrix; external imgproc_Dll;
procedure cvWarpAffine; external imgproc_Dll;
function cvGetPerspectiveTransform; external imgproc_Dll;
procedure cvWarpPerspective; external imgproc_Dll;
function cvContourArea; external imgproc_Dll;
function cvConvexHull2; external imgproc_Dll;
function cvConvexityDefects; external imgproc_Dll;
procedure cvPyrDown; external imgproc_Dll;
procedure cvPyrUp; external imgproc_Dll;
function cvCheckContourConvexity; external imgproc_Dll;

function cvCreateHist; external imgproc_Dll;

procedure cvCalcHist;
begin
  cvCalcArrHist(image, hist, accumulate, mask);
end;

procedure cvGetMinMaxHistValue; external imgproc_Dll;
procedure cvCalcArrHist; external imgproc_Dll;
procedure cvCalcArrBackProject; external imgproc_Dll;
procedure cvCalcBackProject; external imgproc_Dll name 'cvCalcArrBackProject';
procedure cvGoodFeaturesToTrack; external imgproc_Dll;
function cvMinAreaRect2; external imgproc_Dll;
function cvMinEnclosingCircle; external imgproc_Dll;
procedure cvBoxPoints; external imgproc_Dll;

end.
