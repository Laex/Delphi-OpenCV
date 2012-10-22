(* ************************************************************************

  ** Converted with C to Pascal Converter 2.0
  ** Release: 2.1.4.2012
  ** Email: al_gun@ncable.net.au
  ** Updates: http://cc.codegear.com/Author/302259
  ** Blogs: http://delphiprogrammingwithalgun.blogspot.com/
  ** Copyright (c) 2005, 2012 Ural Gunaydin (a.k.a. Al Gun)

  ************************************************************************* *)

unit imgproc_c;

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

uses
  Core.types_c, imgproc.types_c;

(* M///////////////////////////////////////////////////////////////////////////////////////
  //
  //  IMPORTANT: READ BEFORE DOWNLOADING, COPYING, INSTALLING OR USING.
  //
  //  By downloading, copying, installing or using the software you agree to this license.
  //  If you do not agree to this license, do not download, install,
  //  copy or use the software.
  //
  //
  //                           License Agreement
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
  //M*/

  {$ifndef __OPENCV_IMGPROC_IMGPROC_C_H__}
  {$define __OPENCV_IMGPROC_IMGPROC_C_H__}

  {$HPPEMIT '#include 'opencv2/core/core_c.h''}
  {$HPPEMIT '#include 'opencv2/imgproc/types_c.h''}

  {$ifdef __cplusplus}
  //extern "C" {
  {$endif}

  (*********************** Background statistics accumulation **************************** *)

(* Adds image to accumulator *)
// CVAPI(procedure)cvAcc(var Adds squared image to accumulator * )
// CVAPI(procedure)cvSquareAcc(CvArr * image: v1: 0)): CvArr; (var sqsum: CvArr; var Adds a product of two images to accumulator * )
// CVAPI(procedure)cvMultiplyAcc(CvArr * image1: unction mask CV_DEFAULT(v1: 0)): CvArr; (;
// var image2: CvArr; var acc: CvArr; var Adds image to accumulator with weights: acc = acc * (1 - alpha) + image * alpha * )
// CVAPI(procedure)cvRunningAvg(CvArr * image: unction mask CV_DEFAULT(v1: 0)): CvArr; (;
// var acc: CvArr;alpha: Double;
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * \ * image Processing * * *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * )
// (* Copies source 2D array inside of the larger destination array and   makes a border of the specified cType (IPL_BORDER_ *)
// around the copied area. * / CVAPI(
// procedure)cvCopyMakeBorder(CvArr * src: array of unction mask CV_DEFAULT(v1: 0)): CvArr; (;
//
// var
// dst: CvArr;
// offset: CvPoint;
// bordertype: function;
//
// var
// Smoothes array (removes noise) * )CVAPI(
// procedure)cvSmooth(CvArr * src: CvScalar value CV_DEFAULT(v1: 0))): Integer; (; var dst: CvArr;
// smoothtype CV_DEFAULT(v1: 3:
// function); size2 CV_DEFAULT(0): Integer; sigma1 CV_DEFAULT(0):
// function; sigma2 CV_DEFAULT(0): Double): Integer;
{
  CVAPI(void) cvSmooth( const CvArr* src, CvArr* dst,
  int smoothtype CV_DEFAULT(CV_GAUSSIAN),
  int size1 CV_DEFAULT(3),
  int size2 CV_DEFAULT(0),
  double sigma1 CV_DEFAULT(0),
  double sigma2 CV_DEFAULT(0));
}
procedure cvSmooth(const src: pIplImage; dst: pIplImage; smoothtype: integer = CV_GAUSSIAN;
  param1: integer = 3; param2: integer = 0; param3: double = 0; param4: double = 0); cdecl;


// (* Convolves the image with the kernel *)
// CVAPI(
// procedure)cvFilter2D(v1: CvPoint(-1;
//
// var
// Finds integral image: SUM(X: 1))): Double; (; = SUM(X < X: ); v4: < Y)I(X;
// var)CVAPI(procedure)cvIntegral(CvArr * image: ); var SUM: CvArr;
// Smoothes the input image with gaussian kernel and then down - samples it.dst_width = floor
// (src_width / 2): array [0 .. 0] of var function sqsum CV_DEFAULT(v1: 0)): CvArr; (;
// * src dst_height = floor(src_height / 2): array [0 .. 0] of var)CVAPI(procedure)cvPyrDown(CvArr;
// var dst: CvArr; var Up - samples image and Smoothes the cResult with gaussian kernel.dst_width =
// src_width * 2: function filter CV_DEFAULT(v1: CV_GAUSSIAN_5x5)): Integer; (;
// var 2 * )CVAPI(procedure)cvPyrUp(CvArr * src: dst_height = src_height; var dst: CvArr;
// var Builds pyramid for an image * )CVAPI(CvMat * )cvCreatePyramid(const CvArr * img
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
// var )CVAPI(procedure) cvSobel(  CvArr* src: ) or Scharr (aperture_size = -1) operator.   Scharr can be used only for the first dx or dy derivative;
// var dst: CvArr;
// xorder: Integer;
// yorder: Integer;
// var Calculates the image Laplacian: (d2/dx + d2/dy)I *) CVAPI(procedure)cvLaplace(CvArr * src
// : function aperture_size CV_DEFAULT(v1: 3)): Integer; (; var dst: CvArr;

(* Converts input array pixels from one color space to another *)
// CVAPI(void)  cvCvtColor( const CvArr* src, CvArr* dst, int code );
procedure cvCvtColor(const src: pIplImage; dst: pIplImage; code: integer); cdecl;
//
// (* Resizes image (input array is resized to fit the destination array) *)
// CVAPI(procedure)cvResize(var Warps image with affine transform * )
{
  CVAPI(void)  cvResize( const CvArr* src, CvArr* dst,
  int interpolation CV_DEFAULT( CV_INTER_LINEAR ));
}
procedure cvResize(const src: TCvArr; dst: TCvArr; interpolation: integer = CV_INTER_LINEAR); cdecl;

// CVAPI(procedure)cvWarpAffine(CvArr
// * src: ): CV_INTER_LINEAR): Integer; (; var dst: CvArr; var map_matrix: CvMat;
// Computes affine transform matrix for mapping src: array [0 .. I - 1] of var to dst[I]
// (I = 0 function flags CV_DEFAULT(v1: cvScalarAll(
// 0))): Integer; (; :; var)

// CVAPI(CvMat)cvGetAffineTransform(CvPoint2D32f * src: );
// var dst: vPoint2D32f; var map_matrix: CvMat);
//
// (* Computes rotation_matrix matrix *)
// CVAPI(CvMat)cv2DRotationMatrix(CvPoint2D32f center, Double angle, Double scale, CvMat * map_matrix);
//
// (* Warps image with perspective (projective) transform *)
// CVAPI(
// procedure)cvWarpPerspective(Computes perspective transform matrix for mapping src: array [0 .. I -
// 1] of var to dst[I](I = 0 v1: cvScalarAll(0))): Integer; (; :; :;
// var)CVAPI(CvMat)cvGetPerspectiveTransform(CvPoint2D32f * src: ); var dst: vPoint2D32f;
// var map_matrix: CvMat);
//
// (* Performs generic geometric transformation using the specified coordinate maps *)
// CVAPI(
// procedure)cvRemap(var Converts mapx & mapy from floating -
// point to Integer formats for cvRemap * )CVAPI(
// procedure)cvConvertMaps(CvArr * mapx: v1: cvScalarAll(0))): Integer; (; var mapy: CvArr;
// var mapxy: CvArr; var mapalpha: CvArr);
//
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
// (* Computes transformation map from intrinsic camera parameters
// that can used by cvRemap *)
// CVAPI(
// procedure)cvInitUndistortMap(var camera_matrix: CvMat; var distortion_coeffs: vMat; var mapx: CvArr;
// var mapy: CvArr);
//
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
//
// (* releases structuring element *)
// CVAPI(procedure)  cvReleaseStructuringElement( element: array of IplConvKernel);
//
// (* erodes input image (applies minimum filter) one or more times.
// If element cPointer is 0, 3x3 rectangular element is used *)
// CVAPI(procedure)  cvErode(
// v1: 0);
// var dilates input image (applies maximum filter) one or more times.   If element cPointer is 0: function iterations CV_DEFAULT(v1: 1)): Integer;(;
// var )CVAPI(procedure)  cvDilate(  CvArr* src: 3x3 rectangular element is used;
// var dst: CvArr;
// var element CV_DEFAULT(0): IplConvKernel;
// var Performs complex morphological transformation *)CVAPI(procedure)  cvMorphologyEx(  CvArr* src: function iterations CV_DEFAULT(v1: 1)): Integer;(;
// var dst: CvArr;
// var temp: CvArr;
// var element: IplConvKernel;
// operation: function;
// var Calculates all spatial and central moments up to the 3rd order *)CVAPI(procedure) cvMoments(  CvArr* arr: Integer iterations CV_DEFAULT(v1: 1)): Integer;(;
// var moments: CvMoments;
// binary CV_DEFAULT(0): Integer);
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
//
// (* Retrieves outer and optionally inner boundaries of white (non-zero) connected
// cComponents in the black (zero) background *)
// CVAPI(Integer)  cvFindContours( CvArr* image, CvMemStorage* storage, CvSeq** first_contour,
// function header_size CV_DEFAULT(
// v1: CvContour));
// mode CV_DEFAULT(CV_RETR_LIST): Integer;
// method CV_DEFAULT(CV_CHAIN_APPROX_SIMPLE): Integer;
// offset CV_DEFAULT(cvPoint(0: CvPoint;
// v5: ))): Integer;
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
//
// (* Approximates a single polygonal curve (contour) or
// a tree of polygonal curves (contours) *)
// CVAPI(CvSeq)  cvApproxPoly(  Pointer  src_seq,
// Integer header_size, CvMemStorage* storage,
// Integer method, Double eps,
// function recursive CV_DEFAULT(v1: 0)): Integer;
//
// (* Calculates perimeter of a contour or length of a part of contour *)
// CVAPI(Double)  cvArcLength(  Pointer  curve,
// CvSlice slice CV_DEFAULT(CV_WHOLE_SEQ),
// function is_closed CV_DEFAULT(v1: -1)): Integer;
//
// CV_INLINE function cvContourPerimeter(v1: contour; v2: CV_WHOLE_SEQ; : ): Double;
// end;
//
//
// (* Calculates contour boundning rectangle (update=1) or
// just retrieves pre-calculated rectangle (update=0) *)
// CVAPI(CvRect)  cvBoundingRect( CvArr* points, Integer update CV_DEFAULT(0) );
//
// (* Calculates area of a contour or contour segment *)
// CVAPI(Double)  cvContourArea(  CvArr* contour,
// CvSlice slice CV_DEFAULT(CV_WHOLE_SEQ),
// function oriented CV_DEFAULT(v1: 0)): Integer;
//
// (* Finds minimum area rotated rectangle bounding a set of points *)
// CVAPI(CvBox2D)  cvMinAreaRect2(  CvArr* points,
// CvMemStorage* storage CV_DEFAULT(0));
//
// (* Finds minimum enclosing circle for a set of points *)
// CVAPI(Integer)  cvMinEnclosingCircle(  CvArr* points,
// CvPoint2D32f* center, Single* radius );
//
// (* Compares two contours by matching their moments *)
// CVAPI(Double)  cvMatchShapes(  Pointer  object1,  Pointer  object2,
// function method, function parameter CV_DEFAULT(v1: 0)): Integer;
//
// (* Calculates exact convex hull of 2d point set *)
// CVAPI(CvSeq) cvConvexHull2(  CvArr* input,
// function hull_storage CV_DEFAULT(
// v1: CV_CLOCKWISE);
// return_points CV_DEFAULT(0): Integer): Integer;
//
// (* Checks whether the contour is convex or not (returns 1 if convex, 0 if not) *)
// CVAPI(Integer)  cvCheckContourConvexity(  CvArr* contour ): Double;
//
//
// (* Finds convexity defects for the contour *)
// CVAPI(CvSeq)  cvConvexityDefects(  CvArr* contour,  CvArr* convexhull,
// CvMemStorage* storage CV_DEFAULT(0)): Pointer;
//
// (* Fits ellipse into a set of 2d points *)
// CVAPI(CvBox2D) cvFitEllipse2(  CvArr* points );
//
// (* Finds minimum rectangle containing two given rectangles *)
// CVAPI(CvRect)  cvMaxRect(  CvRect* rect1,  CvRect* rect2 );
//
// (* Finds coordinates of the box vertices *)
// cvBoxPoints(box CvBox2D; pt : array[0..3] of CVAPI(procedure): CvPoint2D32f);
//
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
//
// (* Creates new histogram *)
// CVAPI(CvHistogram)  cvCreateHist( Integer dims, Integer* sizes, Integer cType,
// function * ranges CV_DEFAULT(v1: 1)): Integer;
//
// (* Assignes histogram bin ranges *)
// CVAPI(procedure)  cvSetHistBinRanges(
// var Creates histogram header for array *)CVAPI(CvHistogram)  cvMakeHistHeaderForArray(                            Integer  dims: v1: 1)): Integer;(;
// var sizes: Integer;
// var hist: CvHistogram;
// var function: Single;
// var ranges CV_DEFAULT(v1: 1)): Integer;(* Releases histogram *)CVAPI(procedure)  cvReleaseHist( CvHistogram** hist ): Single;(* Clears all the histogram bins *)CVAPI(procedure)  cvClearHist( CvHistogram* hist ): data;(* Finds indices and values of minimum and maximum histogram bins *)CVAPI(procedure)  cvGetMinMaxHistValue(  CvHistogram* hist: Single;
// var min_value: Single;
// var max_value: Single;
// var Normalizes histogram by dividing all bins by sum of the bins: function  min_idx CV_DEFAULT(v1: 0)): Integer;(;
// var )CVAPI(procedure)  cvNormalizeHist( CvHistogram* hist: multiplied by <factor>.   After that sum of histogram bins is equal to <factor>;
// factor: Double);
//
//
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
//
// (* Calculates array histogram *)
// CVAPI(procedure)  cvCalcArrHist(
// image: array of v1: 0)): Integer;CV_INLINE  CV_INLINE  procedure  cvCalcHist( IplImage;
// var hist: CvHistogram;
// accumulate CV_DEFAULT(0): Integer;
// var mask CV_DEFAULT(0) )begin     cvCalcArrHist( (CvArr*)image: vArr;
// v5: hist;
// v6: accumulate;
// var Calculates back project *)CVAPI(procedure)  cvCalcArrBackProject( CvArr** image: mask ): CvArr; end;(;
// var dst: CvArr;
// var hist: vHistogram);
/// / >> Following declaration is a macro definition!
// const cvCalcBackProject(image, dst, hist) cvCalcArrBackProject((CvArr;
//
//
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
//
// (* calculates probabilistic density (divides one histogram by another) *)
// CVAPI(procedure)  cvCalcProbDensity(
// var equalizes histogram of 8-bit single-channel image *)CVAPI(procedure)  cvEqualizeHist(  CvArr* src: v1: 255)): Double;(;
// var dst: CvArr);
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
function cvThreshold(const src, dst: pIplImage; threshold, max_value: double;
  threshold_type: integer): double; cdecl;

//
// (* Applies adaptive threshold to grayscale image.
// The two parameters for methods CV_ADAPTIVE_THRESH_MEAN_C and
// CV_ADAPTIVE_THRESH_GAUSSIAN_C are:
// neighborhood size (3, 5, 7 etc.),
// and a constant subtracted from mean (Args: const nt = subtracted from mean (Args: array of const,-3,-2,-1,0,1,2,3,Args: array of const): CvArr;
// {$EXTERNALSYM nt}r * src, CvArr * dst, Double max_value,
// function adaptive_method CV_DEFAULT(v1: CV_THRESH_BINARY); block_size CV_DEFAULT(3): Integer;
// param1 CV_DEFAULT(5):
// function): Integer;
//
// (* Fills the connected component until the color difference gets large enough *)
// CVAPI(
// procedure)cvFloodFill(v1: cvScalarAll(0)); up_diff CV_DEFAULT(cvScalarAll(0)): CvScalar;
// var comp CV_DEFAULT(0): CvConnectedComp; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// * * * * * * * \ * Feature detection * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// * * * * * * * * * ) (* Runs canny edge detector *) CVAPI(
// procedure)cvCanny(CvArr * image: array of
// function flags CV_DEFAULT(v1: 0)): Integer; (; var edges: CvArr; threshold1: Double;
// threshold2: Double; var Calculates constraint image for corner detection Dx xor 2 * Dyy + Dxx *
// Dy xor 2 - 2 * Dx * Dy * Dxy.Applying threshold to the cResult gives coordinates of
// corners * )CVAPI(
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
// function; var Adjust corner position using some sort of gradient search * )CVAPI(
// procedure)cvFindCornerSubPix(CvArr * image: Integer aperture_size CV_DEFAULT(v1: 0.04)): Integer; (;
// var corners: CvPoint2D32f; count: Integer; win: CvSize; zero_zone: CvSize;
// var Finds a sparse set of points within the selected region that seem to be easy to track * )CVAPI(
// procedure)cvGoodFeaturesToTrack(CvArr * image: cvTermCriteria criteria): Double; (;
// var eig_image: CvArr; var temp_image: CvArr; var corners: CvPoint2D32f; var corner_count: Integer;
// quality_level: Double; min_distance: Double; var mask CV_DEFAULT(0): vArr;
// block_size CV_DEFAULT(v1: 0:
// function); k CV_DEFAULT(0.04):
// function): Integer;
//
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
//
// (* Finds circles in the image *)
// CVAPI(CvSeq)cvHoughCircles(CvArr * image, Pointer circle_storage, Integer method, Double dp,
// Double min_dist,
// function param1 CV_DEFAULT(v1: 100); min_radius CV_DEFAULT(0):
// function; max_radius CV_DEFAULT(0): Integer): Integer;
//
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

const
{$IFDEF DEBUG}
  DllName = 'opencv_imgproc242d.dll';
{$ELSE}
  DllName = 'opencv_imgproc242.dll';
{$ENDIF}
procedure cvCvtColor; external DllName;
function cvThreshold; external DllName;
procedure cvSmooth; external DllName;
procedure cvResize; external DllName;

end.
