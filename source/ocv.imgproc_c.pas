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
  opencv\modules\imgproc\include\opencv2\imgproc\imgproc_c.h
  *************************************************************************************************
*)

unit ocv.imgproc_c;

{$I OpenCV.inc}

interface

uses
  ocv.core.types_c,
  ocv.imgproc.types_c;

(* ********************** Background statistics accumulation **************************** *)

(*
  /* Adds image to accumulator */
  CVAPI(void)  cvAcc( const CvArr* image, CvArr* sum,
  const CvArr* mask CV_DEFAULT(NULL) );
*)
procedure cvAcc(const image: PCvArr; sum: PCvArr; const mask: PCvArr = nil); cdecl;

(*
  /* Adds squared image to accumulator */
  CVAPI(void)  cvSquareAcc( const CvArr* image, CvArr* sqsum,
  const CvArr* mask CV_DEFAULT(NULL) );
*)
procedure cvSquareAcc(const image: PCvArr; sqsum: PCvArr; const mask: PCvArr = nil); cdecl;

(*
  /* Adds a product of two images to accumulator */
  CVAPI(void)  cvMultiplyAcc( const CvArr* image1, const CvArr* image2, CvArr* acc,
  const CvArr* mask CV_DEFAULT(NULL) );
*)
procedure cvMultiplyAcc(const image1: PCvArr; const image2: PCvArr; acc: PCvArr; const mask: PCvArr = nil); cdecl;

(*
  /* Adds image to accumulator with weights: acc = acc*(1-alpha) + image*alpha */
  CVAPI(void)  cvRunningAvg( const CvArr* image, CvArr* acc, double alpha,
  const CvArr* mask CV_DEFAULT(NULL) );
*)
procedure cvRunningAvg(const image: PCvArr; acc: PCvArr; alpha: Double; const mask: PCvArr = nil); cdecl;

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
  { } const src: PCvArr;
  { } dst: PCvArr;
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
  { } const src: PCvArr;
  { } dst: PCvArr;
  { } smoothtype: Integer = CV_GAUSSIAN;
  { } size1: Integer = 3;
  { } size2: Integer = 0;
  { } sigma1: Double = 0;
  { } sigma2: Double = 0); cdecl;

(*
  /* Convolves the image with the kernel */

  CVAPI(void) cvFilter2D( const CvArr* src, CvArr* dst, const CvMat* kernel,
  CvPoint anchor CV_DEFAULT(cvPoint(-1,-1)));
*)

procedure cvFilter2D(const src: PCvArr; dst: PCvArr; const kernel: pCvMat; anchor: TCvPoint { =CV_DEFAULT(cvPoint(-1,-1)) } ); cdecl; overload;
procedure cvFilter2D(const src: PCvArr; dst: PCvArr; const kernel: pCvMat); overload;

{
  Finds integral image: SUM(X,Y) = sum(x<X,y<Y)I(x,y)

  CVAPI(void) cvIntegral(
  const CvArr* image,
  CvArr* sum,
  CvArr* sqsum CV_DEFAULT(NULL),
  CvArr* tilted_sum CV_DEFAULT(NULL));
}
procedure cvIntegral(
  { } const image: PCvArr;
  { } sum: PCvArr;
  { } sqsum: PCvArr = NIL;
  { } tilted_sum: PCvArr = NIL); cdecl;

(*
  Smoothes the input image with gaussian kernel and then down-samples it.
  dst_width = floor(src_width/2)[+1],
  dst_height = floor(src_height/2)[+1]

  CVAPI(void)  cvPyrDown( const CvArr* src, CvArr* dst,
  int filter CV_DEFAULT(CV_GAUSSIAN_5x5) );
*)
procedure cvPyrDown(const src: PCvArr; dst: PCvArr; filter: Integer = CV_GAUSSIAN_5x5); cdecl;

(*

  Up-samples image and smoothes the result with gaussian kernel.
  dst_width = src_width*2,
  dst_height = src_height*2

  CVAPI(void)  cvPyrUp( const CvArr* src, CvArr* dst,
  int filter CV_DEFAULT(CV_GAUSSIAN_5x5) );
*)
procedure cvPyrUp(const src: PCvArr; dst: PCvArr; filter: Integer = CV_GAUSSIAN_5x5); cdecl;

(*
  Builds pyramid for an image

  CVAPI(CvMat** ) cvCreatePyramid( const CvArr* img, int extra_layers, double rate,
  const CvSize* layer_sizes CV_DEFAULT(0),
  CvArr* bufarr CV_DEFAULT(0),
  int calc CV_DEFAULT(1),
  int filter CV_DEFAULT(CV_GAUSSIAN_5x5) );
*)
function cvCreatePyramid(const img: PCvArr; extra_layers: Integer; rate: Double; const layer_sizes: pCvSize = nil; bufarr: PCvArr = nil; calc: Integer = 1;
  filter: Integer = CV_GAUSSIAN_5x5): ppCvMat; cdecl;
(*
  Releases pyramid

  CVAPI(void)  cvReleasePyramid( CvMat*** pyramid, int extra_layers );
*)
procedure cvReleasePyramid(var pyramid: ppCvMat; extra_layers: Integer); cdecl;

(*
  Filters image using meanshift algorithm

  CVAPI(void) cvPyrMeanShiftFiltering( const CvArr* src, CvArr* dst,
  double sp, double sr, int max_level CV_DEFAULT(1),
  CvTermCriteria termcrit CV_DEFAULT(cvTermCriteria(CV_TERMCRIT_ITER+CV_TERMCRIT_EPS,5,1)));
*)
procedure cvPyrMeanShiftFiltering(const src: PCvArr; dst: PCvArr; sp: Double; sr: Double; max_level: Integer { = 1 };
  termcrit: TCvTermCriteria { = CV_DEFAULT(cvTermCriteria(CV_TERMCRIT_ITER + CV_TERMCRIT_EPS, 5, 1)) } ); cdecl;

(*
  Segments image using seed "markers"

  CVAPI(void) cvWatershed( const CvArr* image, CvArr* markers );
*)
procedure cvWatershed(const image: PCvArr; markers: PCvArr); cdecl;

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
procedure cvSobel(const src: PCvArr; dst: PCvArr; xorder: Integer; yorder: Integer; aperture_size: Integer = 3); cdecl;

{
  /* Calculates the image Laplacian: (d2/dx + d2/dy)I */
  CVAPI(void) cvLaplace(
  const CvArr* src,
  CvArr* dst,
  int aperture_size CV_DEFAULT(3) );
}
procedure cvLaplace(const src: PCvArr; dst: PCvArr; aperture_size: Integer = 3); cdecl;

(* Converts input array pixels from one color space to another *)
// CVAPI(void)  cvCvtColor( const CvArr* src, CvArr* dst, int code );
procedure cvCvtColor(const src: PCvArr; dst: PCvArr; code: Integer); cdecl; overload;
procedure cvCvtColor(const src: pCvMat; dst: pCvMat; code: Integer); cdecl; overload;
// procedure cvCvtColor(const src: PCvArr; dst: pCvMat; code: Integer); cdecl; overload;

// (* Resizes image (input array is resized to fit the destination array) *)
// CVAPI(procedure)cvResize(var Warps image with affine transform * )
{
  CVAPI(void)  cvResize( const CvArr* src, CvArr* dst,
  int interpolation CV_DEFAULT( CV_INTER_LINEAR ));
}
procedure cvResize(const src: PCvArr; dst: PCvArr; interpolation: Integer = CV_INTER_LINEAR); cdecl;

{
  /* Warps image with affine transform */
  CVAPI(void)  cvWarpAffine( const CvArr* src, CvArr* dst, const CvMat* map_matrix,
  int flags CV_DEFAULT(CV_INTER_LINEAR+CV_WARP_FILL_OUTLIERS),
  CvScalar fillval CV_DEFAULT(cvScalarAll(0)) );
}
procedure cvWarpAffine(const src: PCvArr; dst: PCvArr; const map_matrix: pCvMat; flags: Integer { = CV_INTER_LINEAR+CV_WARP_FILL_OUTLIERS };
  fillval: TCvScalar { = cvScalarAll(0) } ); cdecl;

// * Computes affine transform matrix for mapping src[i] to dst[i] (i=0,1,2) */
// CVAPI(CvMat*) cvGetAffineTransform( const CvPoint2D32f * src,
// const CvPoint2D32f * dst,
// CvMat * map_matrix );
function cvGetAffineTransform(const src: pCvPoint2D32f; const dst: pCvPoint2D32f; map_matrix: pCvMat): pCvMat; cdecl;

{
  (* Computes rotation_matrix matrix *)
  CVAPI(CvMat)cv2DRotationMatrix(CvPoint2D32f center, Double angle, Double scale, CvMat * map_matrix);
}
function cv2DRotationMatrix(center: TCvPoint2D32f; angle: Double; scale: Double; map_matrix: pCvMat): pCvMat; cdecl;

{
  /* Warps image with perspective (projective) transform */
  CVAPI(void)  cvWarpPerspective( const CvArr* src, CvArr* dst, const CvMat* map_matrix,
  int flags CV_DEFAULT(CV_INTER_LINEAR+CV_WARP_FILL_OUTLIERS),
  CvScalar fillval CV_DEFAULT(cvScalarAll(0)) );
}
procedure cvWarpPerspective(const src: PCvArr; dst: PCvArr; const map_matrix: pCvMat; flags: Integer { =CV_INTER_LINEAR+CV_WARP_FILL_OUTLIERS };
  fillval: TCvScalar { =cvScalarAll(0) } ); cdecl;
{
  /* Computes perspective transform matrix for mapping src[i] to dst[i] (i=0,1,2,3) */
  CVAPI(CvMat*) cvGetPerspectiveTransform( const CvPoint2D32f* src,
  const CvPoint2D32f* dst,
  CvMat* map_matrix );

}
function cvGetPerspectiveTransform(const src: pCvPoint2D32f; const dst: pCvPoint2D32f; map_matrix: pCvMat): pCvMat; cdecl;
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
procedure cvRemap(const src: PCvArr; dst: PCvArr; const mapx: PCvArr; const mapy: PCvArr; flags: Integer { =CV_INTER_LINEAR+CV_WARP_FILL_OUTLIERS };
  fillval: TCvScalar { =cvScalarAll(0) }
  ); cdecl;

(*
  /* Converts mapx & mapy from floating-point to integer formats for cvRemap */
  CVAPI(void)  cvConvertMaps( const CvArr* mapx, const CvArr* mapy,
  CvArr* mapxy, CvArr* mapalpha );
*)
procedure cvConvertMaps(const mapx: PCvArr; const mapy: PCvArr; mapxy: PCvArr; mapalpha: PCvArr); cdecl;

/// * Performs forward or inverse log-polar image transform */
// CVAPI(void)  cvLogPolar( const CvArr* src, CvArr* dst,
// CvPoint2D32f center, double M,
// int flags CV_DEFAULT(CV_INTER_LINEAR+CV_WARP_FILL_OUTLIERS));
procedure cvLogPolar(const src: PCvArr; dst: PCvArr; center: TCvPoint2D32f; M: Double; flags: Integer = CV_INTER_LINEAR + CV_WARP_FILL_OUTLIERS); cdecl;

/// * Performs forward or inverse linear-polar image transform */
// CVAPI(void)  cvLinearPolar( const CvArr* src, CvArr* dst,
// CvPoint2D32f center, double maxRadius,
// int flags CV_DEFAULT(CV_INTER_LINEAR+CV_WARP_FILL_OUTLIERS));

procedure cvLinearPolar(const src: PCvArr; dst: PCvArr; center: TCvPoint2D32f; maxRadius: Double;
  flags: Integer = CV_INTER_LINEAR + CV_WARP_FILL_OUTLIERS); cdecl;

// * Transforms the input image to compensate lens distortion */
// CVAPI(void) cvUndistort2( const CvArr* src, CvArr* dst,
// const CvMat* camera_matrix,
// const CvMat* distortion_coeffs,
// const CvMat* new_camera_matrix CV_DEFAULT(0) );
procedure cvUndistort2(const src: PCvArr; dst: PCvArr; const camera_matrix: PCvArr; const distortion_coeffs: PCvArr;
  const new_camera_matrix: PCvArr = nil); cdecl;

(*
  /* Computes transformation map from intrinsic camera parameters
  that can used by cvRemap */
  CVAPI(void) cvInitUndistortMap(
  const CvMat* camera_matrix,
  const CvMat* distortion_coeffs,
  CvArr* mapx,
  CvArr* mapy );
*)
procedure cvInitUndistortMap(const camera_matrix: pCvMat; const distortion_coeffs: pCvMat; mapx: PCvArr; mapy: PCvArr); cdecl;

(*
  /* Computes undistortion+rectification map for a head of stereo camera */
  CVAPI(void) cvInitUndistortRectifyMap( const CvMat* camera_matrix,
  const CvMat* dist_coeffs,
  const CvMat *R, const CvMat* new_camera_matrix,
  CvArr* mapx, CvArr* mapy );
*)

procedure cvInitUndistortRectifyMap(const camera_matrix: pCvMat; const dist_coeffs: pCvMat; const R: pCvMat; const new_camera_matrix: pCvMat; mapx: PCvArr;
  mapy: PCvArr); cdecl;

(*
  /* Computes the original (undistorted) feature coordinates
  from the observed (distorted) coordinates */
  CVAPI(void) cvUndistortPoints( const CvMat* src, CvMat* dst,
  const CvMat* camera_matrix,
  const CvMat* dist_coeffs,
  const CvMat* R CV_DEFAULT(0),
  const CvMat* P CV_DEFAULT(0));
*)

procedure cvUndistortPoints(const src: pCvMat; dst: pCvMat; const camera_matrix: pCvMat; const dist_coeffs: pCvMat; const R: pCvMat = nil;
  const P: pCvMat = nil); cdecl;

// * creates structuring element used for morphological operations */
// CVAPI(IplConvKernel*)  cvCreateStructuringElementEx(
// int cols, int  rows, int  anchor_x, int  anchor_y,
// int shape, int* values CV_DEFAULT(NULL) );
function cvCreateStructuringElementEx(cols: Integer; rows: Integer; anchor_x: Integer; anchor_y: Integer; shape: Integer; values: PInteger = nil)
  : pIplConvKernel; cdecl;

// (* releases structuring element *)
// CVAPI(procedure)  cvReleaseStructuringElement( element: array of IplConvKernel);
// CVAPI(void)  cvReleaseStructuringElement( IplConvKernel** element );
procedure cvReleaseStructuringElement(Var element: pIplConvKernel); cdecl;

(*
  /* erodes input image (applies minimum filter) one or more times.
  If element pointer is NULL, 3x3 rectangular element is used */
  CVAPI(void)  cvErode( const CvArr* src, CvArr* dst,
  IplConvKernel* element CV_DEFAULT(NULL),
  int iterations CV_DEFAULT(1) );
*)
procedure cvErode(const src: PCvArr; dst: PCvArr; element: pIplConvKernel = nil; iterations: Integer = 1); cdecl;

(*
  /* dilates input image (applies maximum filter) one or more times.
  If element pointer is NULL, 3x3 rectangular element is used */
  CVAPI(void)  cvDilate( const CvArr* src, CvArr* dst,
  IplConvKernel* element CV_DEFAULT(NULL),
  int iterations CV_DEFAULT(1) );
*)
procedure cvDilate(const src: PCvArr; dst: PCvArr; element: pIplConvKernel = nil; iterations: Integer = 1); cdecl;

(*
  /* Performs complex morphological transformation */
  CVAPI(void)  cvMorphologyEx( const CvArr* src, CvArr* dst,
  CvArr* temp, IplConvKernel* element,
  int operation, int iterations CV_DEFAULT(1) );
*)
procedure cvMorphologyEx(const src: PCvArr; dst: PCvArr; temp: PCvArr; element: pIplConvKernel; operation: Integer; iterations: Integer = 1); cdecl;

(*
  /* Calculates all spatial and central moments up to the 3rd order */
  CVAPI(void) cvMoments( const CvArr* arr, CvMoments* moments, int binary CV_DEFAULT(0));
*)
procedure cvMoments(const arr: PCvArr; moments: pCvMoments; binary: Integer = 0); cdecl;

(* /* Retrieve particular spatial, central or normalized central moments */
  CVAPI(double)  cvGetSpatialMoment( CvMoments* moments, int x_order, int y_order ); *)
function cvGetSpatialMoment(moments: pCvMoments; x_order, y_order: Integer): Double; cdecl;
// CVAPI(double)  cvGetCentralMoment( CvMoments* moments, int x_order, int y_order );
function cvGetCentralMoment(moments: pCvMoments; x_order, y_order: Integer): Double; cdecl;
// CVAPI(double)  cvGetNormalizedCentralMoment( CvMoments* moments, int x_order, int y_order );
function cvGetNormalizedCentralMoment(moments: pCvMoments; x_order: Integer; y_order: Integer): Double; cdecl;
(*
  Calculates 7 Hu's invariants from precalculated spatial and central moments

  CVAPI(void) cvGetHuMoments( CvMoments*  moments, CvHuMoments*  hu_moments );
*)
procedure cvGetHuMoments(moments: pCvMoments; hu_moments: pCvHuMoments); cdecl;

// (*********************************** data sampling **************************************)
(*
  Fetches pixels that belong to the specified line segment and stores them to the buffer.
  Returns the number of retrieved points.

  CVAPI(int)  cvSampleLine( const CvArr* image, CvPoint pt1, CvPoint pt2, void* buffer,
  int connectivity CV_DEFAULT(8));
*)
function cvSampleLine(const image: PCvArr; pt1: TCvPoint; pt2: TCvPoint; buffer: Pointer; connectivity: Integer = 8): Integer; cdecl;
(*
  Retrieves the rectangular image region with specified center from the input array.
  dst(x,y) <- src(x + center.x - dst_width/2, y + center.y - dst_height/2).
  Values of pixels with fractional coordinates are retrieved using bilinear interpolation

  CVAPI(void)  cvGetRectSubPix( const CvArr* src, CvArr* dst, CvPoint2D32f center );
*)
procedure cvGetRectSubPix(const src: PCvArr; dst: PCvArr; center: TCvPoint2D32f); cdecl;
(*
  Retrieves quadrangle from the input array.
  matrixarr = ( a11  a12 | b1 )   dst(x,y) <- src(A[x y]' + b)
  ( a21  a22 | b2 )   (bilinear interpolation is used to retrieve pixels
  with fractional coordinates)

  CVAPI(void)  cvGetQuadrangleSubPix( const CvArr* src, CvArr* dst,
  const CvMat* map_matrix );
*)
procedure cvGetQuadrangleSubPix(const src: PCvArr; dst: PCvArr; const map_matrix: pCvMat); cdecl;
(*
  Measures similarity between template and overlapped windows in the source image
  and fills the resultant image with the measurements

  CVAPI(void)  cvMatchTemplate( const CvArr* image, const CvArr* templ,
  CvArr* result, int method );
*)
procedure cvMatchTemplate(const image: PCvArr; const templ: PCvArr; result: PCvArr; method: Integer); cdecl;

(*
  Computes earth mover distance between
  two weighted point sets (called signatures)

  CVAPI(float)  cvCalcEMD2( const CvArr* signature1,
  const CvArr* signature2,
  int distance_type,
  CvDistanceFunction distance_func CV_DEFAULT(NULL),
  const CvArr* cost_matrix CV_DEFAULT(NULL),
  CvArr* flow CV_DEFAULT(NULL),
  float* lower_bound CV_DEFAULT(NULL),
  void* userdata CV_DEFAULT(NULL));
*)
function cvCalcEMD2(const signature1: PCvArr; const signature2: PCvArr; distance_type: Integer; distance_func: TCvDistanceFunction = nil;
  const cost_matrix: PCvArr = nil; flow: PCvArr = nil; lower_bound: pfloat = nil; userdata: Pointer = nil): float; cdecl;

// ****************************************************************************************
// *                              Contours retrieving                                     *
// ****************************************************************************************
const
  // * contour retrieval mode */
  CV_RETR_EXTERNAL = 0;
  CV_RETR_LIST     = 1;
  CV_RETR_CCOMP    = 2;
  CV_RETR_TREE     = 3;

  // * contour approximation method */
  CV_CHAIN_CODE             = 0;
  CV_CHAIN_APPROX_NONE      = 1;
  CV_CHAIN_APPROX_SIMPLE    = 2;
  CV_CHAIN_APPROX_TC89_L1   = 3;
  CV_CHAIN_APPROX_TC89_KCOS = 4;
  CV_LINK_RUNS              = 5;

type

  pCvContourInfo = ^TCvContourInfo;

  TCvContourInfo = record
    flags: Integer;
    next: pCvContourInfo;   // next contour with the same mark value */
    parent: pCvContourInfo; // information about parent contour */
    contour: pCvSeq;        // corresponding contour (may be 0, if rejected) */
    rect: TCvRect;          // bounding rectangle */
    origin: TCvPoint;       // origin point (where the contour was traced from) */
    is_hole: Integer;       // hole flag */
  end;

  (*
    Structure that is used for sequental retrieving contours from the image.
    It supports both hierarchical and plane variants of Suzuki algorithm.
  *)
  pCvContourScanner = ^TCvContourScanner;

  TCvContourScanner = record
    storage1: pCvMemStorage; // contains fetched contours */
    storage2: pCvMemStorage; // contains approximated contours
    // (! = storage1 if approx_method2 ! = approx_method1) * / cinfo_storage: pCvMemStorage;
    // contains _CvContourInfo nodes */
    cinfo_set: pCvSet;             // set of _CvContourInfo nodes */
    initial_pos: TCvMemStoragePos; // starting storage pos */
    backup_pos: TCvMemStoragePos;  // beginning of the latest approx. contour */
    backup_pos2: TCvMemStoragePos; // ending of the latest approx. contour */
    img0: pByte;                   // image origin */
    img: pByte;                    // current image row */
    img_step: Integer;             // image step */
    img_size: TCvSize;             // ROI size */
    offset: TCvPoint;              // ROI offset: coordinates, added to each contour point */
    pt: TCvPoint;                  // current scanner position */
    lnbd: TCvPoint;                // position of the last met contour */
    nbd: Integer;                  // current mark val */
    l_cinfo: pCvContourInfo;       // information about latest approx. contour */
    cinfo_temp: TCvContourInfo;    // temporary var which is used in simple modes */
    frame_info: TCvContourInfo;    // information about frame */
    frame: TCvSeq;                 // frame itself */
    approx_method1: Integer;       // approx method when tracing */
    approx_method2: Integer;       // final approx method */
    mode: Integer;                 // contour scanning mode:
    // 0 - external only
    // 1 - all the contours w/o any hierarchy
    // 2 - connected components (i.e. two-level structure -
    // external contours and holes),
    // 3 - full hierarchy;
    // 4 - connected components of a multi-level image
    subst_flag: Integer;
    seq_type1: Integer;    // type of fetched contours */
    header_size1: Integer; // hdr size of fetched contours */
    elem_size1: Integer;   // elem size of fetched contours */
    seq_type2: Integer;    // */
    header_size2: Integer; // the same for approx. contours  */
    elem_size2: Integer;   // */
    cinfo_table: array [0 .. 127] of pCvContourInfo;
  end;

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
  { } image: PCvArr;
  { } storage: pCvMemStorage;
  { } first_contour: pCvSeq;
  { } header_size: Integer { = SizeOf(TCvContour) };
  { } mode: Integer { = CV_RETR_LIST };
  { } method: Integer { = CV_CHAIN_APPROX_SIMPLE };
  { } offset: TCvPoint { =cvPoint(0,0) } ): Integer; cdecl;

(* Initalizes contour retrieving process.
  Calls cvStartFindContours.
  Calls cvFindNextContour until null pointer is returned
  or some other condition becomes true.
  Calls cvEndFindContours at the end. *)
// CVAPI(CvContourScanner)  cvStartFindContours( CvArr* image, CvMemStorage* storage,
// int header_size CV_DEFAULT(sizeof(CvContour)),
// int mode CV_DEFAULT(CV_RETR_LIST),
// int method CV_DEFAULT(CV_CHAIN_APPROX_SIMPLE),
// CvPoint offset CV_DEFAULT(cvPoint(0,0)));
function cvStartFindContours(image: PCvArr; storage: pCvMemStorage; header_size: Integer { =sizeof(TCvContour)) }; mode: Integer { =  CV_RETR_LIST };
  method: Integer { =CV_CHAIN_APPROX_SIMPLE }; offset: TCvPoint { =cvPoint(0,0) } ): pCvContourScanner; cdecl;

// * Retrieves next contour */
// CVAPI(CvSeq*)  cvFindNextContour( CvContourScanner scanner );
function cvFindNextContour(scanner: pCvContourScanner): pCvSeq; cdecl;

(* Substitutes the last retrieved contour with the new one
  (if the substitutor is null, the last retrieved contour is removed from the tree) *)
// CVAPI(void)   cvSubstituteContour( CvContourScanner scanner, CvSeq* new_contour );
procedure cvSubstituteContour(scanner: pCvContourScanner; new_contour: pCvSeq); cdecl;

// * Releases contour scanner and returns pointer to the first outer contour */
// CVAPI(CvSeq*)  cvEndFindContours( CvContourScanner* scanner );
function cvEndFindContours(Var scanner: pCvContourScanner): pCvSeq; cdecl;
(*
  Approximates a single Freeman chain or a tree of chains to polygonal curves

  CVAPI(CvSeq* ) cvApproxChains( CvSeq* src_seq, CvMemStorage* storage,
  int method CV_DEFAULT(CV_CHAIN_APPROX_SIMPLE),
  double parameter CV_DEFAULT(0),
  int  minimal_perimeter CV_DEFAULT(0),
  int  recursive CV_DEFAULT(0));
*)
function cvApproxChains(src_seq: pCvSeq; storage: pCvMemStorage; method: Integer = CV_CHAIN_APPROX_SIMPLE; parameter: Double = 0;
  minimal_perimeter: Integer = 0; recursive: Integer = 0): pCvSeq; cdecl;
(*
  Initializes Freeman chain reader.
  The reader is used to iteratively get coordinates of all the chain points.
  If the Freeman codes should be read as is, a simple sequence reader should be used

  CVAPI(void) cvStartReadChainPoints( CvChain* chain, CvChainPtReader* reader );
*)
procedure cvStartReadChainPoints(chain: pCvChain; reader: pCvChainPtReader); cdecl;

(*
  Retrieves the next chain point
  CVAPI(CvPoint) cvReadChainPoint( CvChainPtReader* reader );
*)
function cvReadChainPoint(reader: pCvChainPtReader): TCvPoint; cdecl;

// (****************************************************************************************\
// *                            Contour Processing and Shape Analysis                       *
// *************************************************************************************** *)
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
  { } const src_seq: pCvSeq;
  { } header_size: Integer;
  { } storage: pCvMemStorage;
  { } method: Integer;
  { } eps: Double;
  { } recursive: Integer = 0): pCvSeq; cdecl;

(*
  /* Calculates perimeter of a contour or length of a part of contour */
  CVAPI(double)  cvArcLength( const void* curve,
  CvSlice slice CV_DEFAULT(CV_WHOLE_SEQ),
  int is_closed CV_DEFAULT(-1));
*)

function cvArcLength(const curve: Pointer; slice: TCvSlice { = CV_WHOLE_SEQ }; is_closed: Integer { = 1 } ): Double; cdecl;

(*
  CV_INLINE double cvContourPerimeter( const void* contour )
  {
  return cvArcLength( contour, CV_WHOLE_SEQ, 1 );
  }
*)
function cvContourPerimeter(const contour: Pointer): Double; {$IFDEF USE_INLINE}inline; {$ENDIF}
// * Calculates contour boundning rectangle (update=1) or
// just retrieves pre-calculated rectangle (update=0) */
// CVAPI(CvRect)  cvBoundingRect( CvArr* points, int update CV_DEFAULT(0) );
function cvBoundingRect(points: PCvArr; update: Integer = 0): TCvRect; cdecl;

// * Calculates area of a contour or contour segment */
// CVAPI(double)  cvContourArea( const CvArr* contour,
// CvSlice slice CV_DEFAULT(CV_WHOLE_SEQ),
// int oriented CV_DEFAULT(0));
function cvContourArea(const contour: PCvArr; slice: TCvSlice { = CV_WHOLE_SEQ }; oriented: Integer = 0): Double; cdecl;

// (* Finds minimum area rotated rectangle bounding a set of points *)
// CVAPI(CvBox2D)  cvMinAreaRect2( const CvArr* points, CvMemStorage* storage CV_DEFAULT(NULL));
function cvMinAreaRect2(points: PCvArr; storage: pCvMemStorage = nil): TCvBox2D; cdecl;

// (* Finds minimum enclosing circle for a set of points *)
// CVAPI(int)  cvMinEnclosingCircle( const CvArr* points,CvPoint2D32f* center, float* radius );
function cvMinEnclosingCircle(points: PCvArr; center: pCvPoint2D32f; radius: pSingle): Integer; cdecl;

{
  /* Compares two contours by matching their moments */
  CVAPI(double)  cvMatchShapes( const void* object1, const void* object2,
  int method, double parameter CV_DEFAULT(0));
}
function cvMatchShapes(const object1: Pointer; const object2: Pointer; method: Integer; parameter: Double = 0): Double; cdecl;

{
  /* Calculates exact convex hull of 2d point set */
  CVAPI(CvSeq*) cvConvexHull2( const CvArr* input,
  void* hull_storage CV_DEFAULT(NULL),
  int orientation CV_DEFAULT(CV_CLOCKWISE),
  int return_points CV_DEFAULT(0));
}
function cvConvexHull2(const input: pCvSeq; hull_storage: Pointer = nil; orientation: Integer = CV_CLOCKWISE; return_points: Integer = 0): pCvSeq; cdecl;

{
  /* Checks whether the contour is convex or not (returns 1 if convex, 0 if not) */
  CVAPI(int)  cvCheckContourConvexity( const CvArr* contour );
}
function cvCheckContourConvexity(const contour: pCvSeq): Integer; cdecl;
{
  (* Finds convexity defects for the contour *)
  CVAPI(CvSeq)  cvConvexityDefects(  CvArr* contour,  CvArr* convexhull,
  CvMemStorage* storage CV_DEFAULT(0)): Pointer;
}
function cvConvexityDefects(contour: pCvSeq; convexhull: pCvSeq; storage: pCvMemStorage = nil): pCvSeq; cdecl;

(*
  Fits ellipse into a set of 2d points

  CVAPI(CvBox2D) cvFitEllipse2( const CvArr* points );
*)
function cvFitEllipse2(const points: PCvArr): TCvBox2D; cdecl;

(*
  Finds minimum rectangle containing two given rectangles

  CVAPI(CvRect)  cvMaxRect( const CvRect* rect1, const CvRect* rect2 );
*)
function cvMaxRect(const rect1: pCvRect; const rect2: pCvRect): TCvRect; cdecl;

Type
  TBoxPoints = array [0 .. 3] of TCvPoint2D32f;
  // (* Finds coordinates of the box vertices *)
  // CVAPI(void) cvBoxPoints( CvBox2D box, CvPoint2D32f pt[4] );
procedure cvBoxPoints(box: TCvBox2D; pt: TBoxPoints); cdecl;

(*
  Initializes sequence header for a matrix (column or row vector) of points -
  a wrapper for cvMakeSeqHeaderForArray (it does not initialize bounding rectangle!!!)

  CVAPI(CvSeq* ) cvPointSeqFromMat( int seq_kind, const CvArr* mat,
  CvContour* contour_header,
  CvSeqBlock* block );
*)
function cvPointSeqFromMat(seq_kind: Integer; const mat: PCvArr; contour_header: pCvContour; block: pCvSeqBlock): pCvSeq; cdecl;
(*
  Checks whether the point is inside polygon, outside, on an edge (at a vertex).
  Returns positive, negative or zero value, correspondingly.
  Optionally, measures a signed distance between
  the point and the nearest polygon edge (measure_dist=1)

  CVAPI(double) cvPointPolygonTest( const CvArr* contour,
  CvPoint2D32f pt, int measure_dist );
*)
function cvPointPolygonTest(const contour: PCvArr; pt: TCvPoint2D32f; measure_dist: Integer): Double; cdecl;

// (****************************************************************************************\
// *                                  Histogram functions                                   *
// *************************************************************************************** *)

{
  /* Creates new histogram */
  CVAPI(CvHistogram*)  cvCreateHist( int dims, int* sizes, int type,
  float** ranges CV_DEFAULT(NULL),
  int uniform CV_DEFAULT(1));
}
function cvCreateHist(dims: Integer; sizes: PInteger; _type: Integer; ranges: Pointer = nil; uniform: Integer = 1): pCvHistogram; cdecl;

(*
  /* Assignes histogram bin ranges */
  CVAPI(void)  cvSetHistBinRanges( CvHistogram* hist, float** ranges,
  int uniform CV_DEFAULT(1));
*)
// WARNING! "float** ranges" is matrix
procedure cvSetHistBinRanges(hist: pCvHistogram; ranges: pSingle; uniform: Integer = 1); cdecl;

(*
  Creates histogram header for array

  CVAPI(CvHistogram* )  cvMakeHistHeaderForArray(
  int  dims, int* sizes, CvHistogram* hist,
  float* data, float** ranges CV_DEFAULT(NULL),
  int uniform CV_DEFAULT(1));
*)
function cvMakeHistHeaderForArray(dims: Integer; sizes: PInteger; hist: pCvHistogram; data: pfloat; ranges: ppfloat = nil; uniform: Integer = 1)
  : pCvHistogram; cdecl;

// * Releases histogram */
// CVAPI(void)  cvReleaseHist( CvHistogram** hist );
procedure cvReleaseHist(Var hist: pCvHistogram); cdecl;

// * Clears all the histogram bins */
// CVAPI(void)  cvClearHist( CvHistogram* hist );
procedure cvClearHist(hist: pCvHistogram); cdecl;

{
  /* Finds indices and values of minimum and maximum histogram bins */
  CVAPI(void)  cvGetMinMaxHistValue( const CvHistogram* hist,
  float* min_value, float* max_value,
  int* min_idx CV_DEFAULT(NULL),
  int* max_idx CV_DEFAULT(NULL));
}
procedure cvGetMinMaxHistValue(const hist: pCvHistogram; min_value: pSingle; max_value: pSingle; min_idx: PInteger = nil; max_idx: PInteger = nil); cdecl;

(*
  /* Normalizes histogram by dividing all bins by sum of the bins, multiplied by <factor>.
  After that sum of histogram bins is equal to <factor> */
  CVAPI(void)  cvNormalizeHist( CvHistogram* hist, double factor );
*)
procedure cvNormalizeHist(hist: pCvHistogram; factor: Double); cdecl;

(*
  /* Clear all histogram bins that are below the threshold */
  CVAPI(void)  cvThreshHist( CvHistogram* hist, double threshold );
*)
procedure cvThreshHist(hist: pCvHistogram; threshold: Double); cdecl;

(* /* Compares two histogram */
  CVAPI(Double)  cvCompareHist(  CvHistogram* hist1,
  CvHistogram* hist2,
  Integer method);
*)
function cvCompareHist(hist1: pCvHistogram; hist2: pCvHistogram; method: Integer): Double; cdecl;

(*
  /* Copies one histogram to another. Destination histogram is created if
  the destination pointer is NULL */
  CVAPI(void)  cvCopyHist( const CvHistogram* src, CvHistogram** dst );
*)
procedure cvCopyHist(const src: pCvHistogram; Var dst: pCvHistogram); cdecl;

(*
  /* Calculates bayesian probabilistic histograms
  (each or src and dst is an array of <number> histograms */
  CVAPI(void)  cvCalcBayesianProb( CvHistogram** src, int number,
  CvHistogram** dst);
*)
procedure cvCalcBayesianProb(Var src: pCvHistogram; number: Integer; Var dst: pCvHistogram); cdecl;

// * Does some sort of template matching but compares histograms of
// template and each window location */
// CVAPI(void)  cvCalcArrBackProjectPatch( CvArr** image, CvArr* dst, CvSize range,
// CvHistogram* hist, int method,
// double factor );
//procedure cvCalcArrBackProjectPatch(var image: PCvArr; dst: PCvArr; range: TCvSize; hist: pCvHistogram; method: Integer; factor: Double); cdecl;
// #define  cvCalcBackProjectPatch( image, dst, range, hist, method, factor ) cvCalcArrBackProjectPatch( (CvArr**)image, dst, range, hist, method, factor )

(*
  /* Calculates array histogram */
  CVAPI(void)  cvCalcArrHist( CvArr** arr, CvHistogram* hist,
  int accumulate CV_DEFAULT(0),
  const CvArr* mask CV_DEFAULT(NULL) );
*)
procedure cvCalcArrHist(var arr: PCvArr; hist: pCvHistogram; accumulate: Integer = 0; const mask: PCvArr = nil); cdecl; overload;
procedure cvCalcArrHist(var arr: pIplImage; hist: pCvHistogram; accumulate: Integer = 0; const mask: pIplImage = nil); cdecl; overload;
procedure cvCalcArrHist(var arr: pCvMat; hist: pCvHistogram; accumulate: Integer = 0; const mask: pCvMat = nil); cdecl; overload;

// CV_INLINE  void  cvCalcHist(
// IplImage** image,
// CvHistogram* hist,
// int accumulate CV_DEFAULT(0),
// const CvArr* mask CV_DEFAULT(NULL) )
// {
// cvCalcArrHist( (CvArr**)image, hist, accumulate, mask );
// }

procedure cvCalcHist(var image: PCvArr; hist: pCvHistogram; accumulate: Integer = 0; const mask: PCvArr = nil);
{$IFDEF USE_INLINE}inline; {$ENDIF} overload;
procedure cvCalcHist(var image: pIplImage; hist: pCvHistogram; accumulate: Integer = 0; const mask: PCvArr = nil);
{$IFDEF USE_INLINE}inline; {$ENDIF} overload;
procedure cvCalcHist(var image: pCvMat; hist: pCvHistogram; accumulate: Integer = 0; const mask: PCvArr = nil);
{$IFDEF USE_INLINE}inline; {$ENDIF} overload;
{
  /* Calculates back project */
  CVAPI(void)  cvCalcArrBackProject( CvArr** image, CvArr* dst,
  const CvHistogram* hist );
  #define  cvCalcBackProject(image, dst, hist) cvCalcArrBackProject((CvArr**)image, dst, hist)
}
procedure cvCalcArrBackProject(var image: PCvArr; dst: PCvArr; const hist: pCvHistogram); cdecl;

(*
  /* Does some sort of template matching but compares histograms of
  template and each window location */
*)
procedure cvCalcBackProject(var image: pIplImage; dst: PCvArr; const hist: pCvHistogram); cdecl; overload;
procedure cvCalcBackProject(var image: PCvArr; dst: PCvArr; const hist: pCvHistogram); cdecl; overload;

(*
  /* calculates probabilistic density (divides one histogram by another) */
  CVAPI(void)  cvCalcProbDensity( const CvHistogram* hist1, const CvHistogram* hist2,
  CvHistogram* dst_hist, double scale CV_DEFAULT(255) );
*)
procedure cvCalcProbDensity(const hist1: pCvHistogram; const hist2: pCvHistogram; dst_hist: pCvHistogram; scale: Double = 255); cdecl;

(*
  /* equalizes histogram of 8-bit single-channel image */
  CVAPI(void)  cvEqualizeHist( const CvArr* src, CvArr* dst );
*)
procedure cvEqualizeHist(const src, dst: PCvArr); cdecl;

(* Applies distance transform to binary image *)
{
  CVAPI(void)  cvDistTransform( const CvArr* src, CvArr* dst,
  int distance_type CV_DEFAULT(CV_DIST_L2),
  int mask_size CV_DEFAULT(3),
  const float* mask CV_DEFAULT(NULL),
  CvArr* labels CV_DEFAULT(NULL),
  int labelType CV_DEFAULT(CV_DIST_LABEL_CCOMP));
}

procedure cvDistTransform(const src: PCvArr; dst: PCvArr; distance_type: Integer = CV_DIST_L2; mask_size: Integer = 3; const mask: pfloat = nil;
  labels: PCvArr = nil; labelType: Integer = CV_DIST_LABEL_CCOMP); cdecl;

// (* Applies fixed-level threshold to grayscale image.
// This is a basic operation applied before retrieving contours *)
// CVAPI(double)  cvThreshold( const CvArr*  src, CvArr*  dst, double  threshold, double  max_value, int threshold_type );
function cvThreshold(const src, dst: PCvArr; threshold, max_value: Double; threshold_type: Integer): Double; cdecl;
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
  { } const src: PCvArr;
  { } dst: PCvArr;
  { } max_value: Double;
  { } adaptive_method: Integer = CV_ADAPTIVE_THRESH_MEAN_C;
  { } threshold_type: Integer = CV_THRESH_BINARY;
  { } block_size: Integer = 3;
  { } param1: Double = 5); cdecl;

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
  { } image: PCvArr;
  { } seed_point: TCvPoint;
  { } new_val: TCvScalar;
  { } lo_diff: TCvScalar { * cvScalarAll(0) * };
  { } up_diff: TCvScalar { * cvScalarAll(0) * };
  { } comp: pCvConnectedComp = NIL;
  { } flags: Integer = 4;
  { } mask: PCvArr = NIL); cdecl;

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
procedure cvCanny(const image: PCvArr; edges: PCvArr; threshold1: Double; threshold2: Double; aperture_size: Integer = 3); cdecl;

(*
  /* Calculates constraint image for corner detection
  Dx^2 * Dyy + Dxx * Dy^2 - 2 * Dx * Dy * Dxy.
  Applying threshold to the result gives coordinates of corners */
  CVAPI(void) cvPreCornerDetect( const CvArr* image, CvArr* corners,
  int aperture_size CV_DEFAULT(3) );
*)
procedure cvPreCornerDetect(const image: PCvArr; corners: PCvArr; aperture_size: Integer = 3); cdecl;
(*
  /* Calculates eigen values and vectors of 2x2
  gradient covariation matrix at every image pixel */
  CVAPI(void)  cvCornerEigenValsAndVecs( const CvArr* image, CvArr* eigenvv,
  int block_size, int aperture_size CV_DEFAULT(3) );
*)
procedure cvCornerEigenValsAndVecs(const image: PCvArr; eigenvv: PCvArr; block_size: Integer; aperture_size: Integer = 3); cdecl;

(*
  /* Calculates minimal eigenvalue for 2x2 gradient covariation matrix at
  every image pixel */
  CVAPI(void)  cvCornerMinEigenVal( const CvArr* image, CvArr* eigenval,
  int block_size, int aperture_size CV_DEFAULT(3) );
*)
procedure cvCornerMinEigenVal(const image: PCvArr; eigenval: PCvArr; block_size: Integer; aperture_size: Integer = 3); cdecl;

(*
  /* Harris corner detector:
  Calculates det(M) - k*(trace(M)^2), where M is 2x2 gradient covariation matrix for each pixel */
  CVAPI(void)  cvCornerHarris( const CvArr* image, CvArr* harris_response,
  int block_size, int aperture_size CV_DEFAULT(3),
  double k CV_DEFAULT(0.04) );
*)
procedure cvCornerHarris(const image: PCvArr; harris_response: PCvArr; block_size: Integer; aperture_size: Integer = 3; k: Double = 0.04); cdecl;

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
procedure cvFindCornerSubPix(const image: PCvArr; corners: pCvPoint2D32f; count: Integer; win: TCvSize; zero_zone: TCvSize; criteria: TCvTermCriteria); cdecl;

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
procedure cvGoodFeaturesToTrack(const image: PCvArr; eig_image: PCvArr; temp_image: PCvArr; corners: pCvPoint2D32f; corner_count: PInteger;
  quality_level: Double; min_distance: Double; const mask: PCvArr = nil; block_size: Integer = 3; use_harris: Integer = 0; k: Double = 0.04); cdecl;

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
  { } image: PCvArr;
  { } line_storage: Pointer;
  { } method: Integer;
  { } rho: Double;
  { } theta: Double;
  { } threshold: Integer;
  { } param1: Double = 0;
  { } param2: Double = 0): pCvSeq; cdecl;

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
  { } image: PCvArr;
  { } circle_storage: Pointer;
  { } method: Integer;
  { } dp: Double;
  { } min_dist: Double;
  { } param1: Double = 100;
  { } param2: Double = 100;
  { } min_radius: Integer = 0;
  { } max_radius: Integer = 0): pCvSeq; cdecl;

(*
  /* Fits a line into set of 2d or 3d points in a robust way (M-estimator technique) */
  CVAPI(void)  cvFitLine( const CvArr* points, int dist_type, double param,
  double reps, double aeps, float* line );
*)
procedure cvFitLine(const points: PCvArr; dist_type: Integer; param: Double; reps: Double; aeps: Double; Var line: Single); cdecl;

implementation

uses ocv.lib;

// procedure cvCvtColor(const src: pIplImage; dst: pIplImage; code: Integer); external imgproc_lib;
procedure cvCvtColor(const src: PCvArr; dst: PCvArr; code: Integer); cdecl; external imgproc_lib name 'cvCvtColor';
procedure cvCvtColor(const src: pCvMat; dst: pCvMat; code: Integer); cdecl; external imgproc_lib name 'cvCvtColor';
// procedure cvCvtColor(const src: PCvArr; dst: pCvMat; code: Integer); external imgproc_lib name 'cvCvtColor';

function cvThreshold(const src, dst: PCvArr; threshold, max_value: Double; threshold_type: Integer): Double; cdecl; external imgproc_lib;
procedure cvSmooth(const src: PCvArr; dst: PCvArr; smoothtype: Integer = CV_GAUSSIAN; size1: Integer = 3; size2: Integer = 0; sigma1: Double = 0;
  sigma2: Double = 0); cdecl; external imgproc_lib;
procedure cvResize(const src: PCvArr; dst: PCvArr; interpolation: Integer = CV_INTER_LINEAR); cdecl; external imgproc_lib;
function cvCreateStructuringElementEx(cols: Integer; rows: Integer; anchor_x: Integer; anchor_y: Integer; shape: Integer; values: PInteger = nil)
  : pIplConvKernel; cdecl; external imgproc_lib;
procedure cvErode(const src: PCvArr; dst: PCvArr; element: pIplConvKernel = nil; iterations: Integer = 1); cdecl; external imgproc_lib;
procedure cvDilate(const src: PCvArr; dst: PCvArr; element: pIplConvKernel = nil; iterations: Integer = 1); cdecl; external imgproc_lib;
procedure cvReleaseStructuringElement(Var element: pIplConvKernel); cdecl; external imgproc_lib;
procedure cvMorphologyEx(const src: PCvArr; dst: PCvArr; temp: PCvArr; element: pIplConvKernel; operation: Integer; iterations: Integer = 1); cdecl;
  external imgproc_lib;
procedure cvFloodFill(image: PCvArr; seed_point: TCvPoint; new_val: TCvScalar; lo_diff: TCvScalar { * cvScalarAll(0) * };
  up_diff: TCvScalar { * cvScalarAll(0) * }; comp: pCvConnectedComp = NIL; flags: Integer = 4; mask: PCvArr = NIL); cdecl; external imgproc_lib;
procedure cvAdaptiveThreshold(const src: PCvArr; dst: PCvArr; max_value: Double; adaptive_method: Integer = CV_ADAPTIVE_THRESH_MEAN_C;
  threshold_type: Integer = CV_THRESH_BINARY; block_size: Integer = 3; param1: Double = 5); cdecl; external imgproc_lib;
procedure cvCopyMakeBorder(const src: PCvArr; dst: PCvArr; offset: TCvPoint; bordertype: Integer; value: TCvScalar { * cvScalarAll(0) * } ); cdecl;
  external imgproc_lib;
procedure cvSobel(const src: PCvArr; dst: PCvArr; xorder: Integer; yorder: Integer; aperture_size: Integer = 3); cdecl; external imgproc_lib;
procedure cvLaplace(const src: PCvArr; dst: PCvArr; aperture_size: Integer = 3); cdecl; external imgproc_lib;
procedure cvCanny(const image: PCvArr; edges: PCvArr; threshold1: Double; threshold2: Double; aperture_size: Integer = 3); cdecl; external imgproc_lib;
function cvHoughLines2(image: PCvArr; line_storage: Pointer; method: Integer; rho: Double; theta: Double; threshold: Integer; param1: Double = 0;
  param2: Double = 0): pCvSeq; cdecl; external imgproc_lib;
function cvHoughCircles(image: PCvArr; circle_storage: Pointer; method: Integer; dp: Double; min_dist: Double; param1: Double = 100; param2: Double = 100;
  min_radius: Integer = 0; max_radius: Integer = 0): pCvSeq; cdecl; external imgproc_lib;
procedure cvIntegral(const image: PCvArr; sum: PCvArr; sqsum: PCvArr = NIL; tilted_sum: PCvArr = NIL); cdecl; external imgproc_lib;
function cvFindContours(image: PCvArr; storage: pCvMemStorage; first_contour: pCvSeq; header_size: Integer { = SizeOf(TCvContour) };
  mode: Integer { = CV_RETR_LIST }; method: Integer { = CV_CHAIN_APPROX_SIMPLE }; offset: TCvPoint { =cvPoint(0,0) } ): Integer; cdecl; external imgproc_lib;
function cvApproxPoly(const src_seq: pCvSeq; header_size: Integer; storage: pCvMemStorage; method: Integer; eps: Double; recursive: Integer = 0): pCvSeq; cdecl;
  external imgproc_lib;
procedure cvEqualizeHist(const src, dst: PCvArr); cdecl; external imgproc_lib;
procedure cvFindCornerSubPix(const image: PCvArr; corners: pCvPoint2D32f; count: Integer; win: TCvSize; zero_zone: TCvSize; criteria: TCvTermCriteria); cdecl;
  external imgproc_lib;
procedure cvInitUndistortMap(const camera_matrix: pCvMat; const distortion_coeffs: pCvMat; mapx: PCvArr; mapy: PCvArr); cdecl; external imgproc_lib;
procedure cvRemap(const src: PCvArr; dst: PCvArr; const mapx: PCvArr; const mapy: PCvArr; flags: Integer { =CV_INTER_LINEAR+CV_WARP_FILL_OUTLIERS };
  fillval: TCvScalar { =cvScalarAll(0) }
  ); cdecl; external imgproc_lib;
function cvArcLength(const curve: Pointer; slice: TCvSlice { = CV_WHOLE_SEQ }; is_closed: Integer { = 1 } ): Double; cdecl; external imgproc_lib;

function cvContourPerimeter(const contour: Pointer): Double; {$IFDEF USE_INLINE}inline; {$ENDIF}
begin
  result := cvArcLength(contour, CV_WHOLE_SEQ, 1);
end;

function cvMatchShapes(const object1: Pointer; const object2: Pointer; method: Integer; parameter: Double = 0): Double; cdecl; external imgproc_lib;
function cv2DRotationMatrix(center: TCvPoint2D32f; angle: Double; scale: Double; map_matrix: pCvMat): pCvMat; cdecl; external imgproc_lib;
procedure cvWarpAffine(const src: PCvArr; dst: PCvArr; const map_matrix: pCvMat; flags: Integer { = CV_INTER_LINEAR+CV_WARP_FILL_OUTLIERS };
  fillval: TCvScalar { = cvScalarAll(0) } ); cdecl; external imgproc_lib;
function cvGetPerspectiveTransform(const src: pCvPoint2D32f; const dst: pCvPoint2D32f; map_matrix: pCvMat): pCvMat; cdecl; external imgproc_lib;
procedure cvWarpPerspective(const src: PCvArr; dst: PCvArr; const map_matrix: pCvMat; flags: Integer { =CV_INTER_LINEAR+CV_WARP_FILL_OUTLIERS };
  fillval: TCvScalar { =cvScalarAll(0) } ); cdecl; external imgproc_lib;
function cvBoundingRect(points: PCvArr; update: Integer = 0): TCvRect; cdecl; external imgproc_lib;
function cvContourArea(const contour: PCvArr; slice: TCvSlice { = CV_WHOLE_SEQ }; oriented: Integer = 0): Double; cdecl; external imgproc_lib;
function cvConvexHull2(const input: pCvSeq; hull_storage: Pointer = nil; orientation: Integer = CV_CLOCKWISE; return_points: Integer = 0): pCvSeq; cdecl;
  external imgproc_lib;
function cvConvexityDefects(contour: pCvSeq; convexhull: pCvSeq; storage: pCvMemStorage = nil): pCvSeq; cdecl; external imgproc_lib;
procedure cvPyrDown(const src: PCvArr; dst: PCvArr; filter: Integer = CV_GAUSSIAN_5x5); cdecl; external imgproc_lib;
procedure cvPyrUp(const src: PCvArr; dst: PCvArr; filter: Integer = CV_GAUSSIAN_5x5); cdecl; external imgproc_lib;
function cvCheckContourConvexity(const contour: pCvSeq): Integer; cdecl; external imgproc_lib;
function cvCreateHist(dims: Integer; sizes: PInteger; _type: Integer; ranges: Pointer = nil; uniform: Integer = 1): pCvHistogram; cdecl; external imgproc_lib;

procedure cvCalcHist(var image: PCvArr; hist: pCvHistogram; accumulate: Integer = 0; const mask: PCvArr = nil);
{$IFDEF USE_INLINE}inline; {$ENDIF} overload;
begin
  cvCalcArrHist(image, hist, accumulate, mask);
end;

procedure cvCalcHist(var image: pIplImage; hist: pCvHistogram; accumulate: Integer = 0; const mask: PCvArr = nil);
{$IFDEF USE_INLINE}inline; {$ENDIF} overload;
begin
  cvCalcArrHist(PCvArr(image), hist, accumulate, mask);
end;

procedure cvCalcHist(var image: pCvMat; hist: pCvHistogram; accumulate: Integer = 0; const mask: PCvArr = nil);
{$IFDEF USE_INLINE}inline; {$ENDIF} overload;
begin
  cvCalcArrHist(PCvArr(image), hist, accumulate, mask);
end;

// procedure cvCalcHist;
// begin
// cvCalcArrHist(image, hist, accumulate, mask);
// end;

procedure cvGetMinMaxHistValue(const hist: pCvHistogram; min_value: pSingle; max_value: pSingle; min_idx: PInteger = nil; max_idx: PInteger = nil); cdecl;
  external imgproc_lib;
// procedure cvCalcArrHist; external imgproc_lib;
procedure cvCalcArrHist(var arr: PCvArr; hist: pCvHistogram; accumulate: Integer = 0; const mask: PCvArr = nil); cdecl; external imgproc_lib; overload;
procedure cvCalcArrHist(var arr: pIplImage; hist: pCvHistogram; accumulate: Integer = 0; const mask: pIplImage = nil); cdecl; external imgproc_lib; overload;
procedure cvCalcArrHist(var arr: pCvMat; hist: pCvHistogram; accumulate: Integer = 0; const mask: pCvMat = nil); cdecl; external imgproc_lib; overload;
procedure cvCalcArrBackProject(var image: PCvArr; dst: PCvArr; const hist: pCvHistogram); cdecl; external imgproc_lib;
//procedure cvCalcBackProject; external imgproc_lib name 'cvCalcArrBackProject';
procedure cvCalcBackProject(var image: pIplImage; dst: PCvArr; const hist: pCvHistogram); cdecl; external imgproc_lib name 'cvCalcArrBackProject'; overload;
procedure cvCalcBackProject(var image: PCvArr; dst: PCvArr; const hist: pCvHistogram); cdecl; external imgproc_lib name 'cvCalcArrBackProject'; overload;
procedure cvGoodFeaturesToTrack(const image: PCvArr; eig_image: PCvArr; temp_image: PCvArr; corners: pCvPoint2D32f; corner_count: PInteger;
  quality_level: Double; min_distance: Double; const mask: PCvArr = nil; block_size: Integer = 3; use_harris: Integer = 0; k: Double = 0.04); cdecl;
  external imgproc_lib;
function cvMinAreaRect2(points: PCvArr; storage: pCvMemStorage = nil): TCvBox2D; cdecl; external imgproc_lib;
function cvMinEnclosingCircle(points: PCvArr; center: pCvPoint2D32f; radius: pSingle): Integer; cdecl; external imgproc_lib;
procedure cvBoxPoints(box: TCvBox2D; pt: TBoxPoints); cdecl; external imgproc_lib;
procedure cvLogPolar(const src: PCvArr; dst: PCvArr; center: TCvPoint2D32f; M: Double; flags: Integer = CV_INTER_LINEAR + CV_WARP_FILL_OUTLIERS); cdecl;
  external imgproc_lib;
procedure cvLinearPolar(const src: PCvArr; dst: PCvArr; center: TCvPoint2D32f; maxRadius: Double; flags: Integer = CV_INTER_LINEAR + CV_WARP_FILL_OUTLIERS);
  cdecl; external imgproc_lib;
procedure cvReleaseHist(Var hist: pCvHistogram); cdecl; external imgproc_lib;
procedure cvClearHist(hist: pCvHistogram); cdecl; external imgproc_lib;
procedure cvMoments(const arr: PCvArr; moments: pCvMoments; binary: Integer = 0); cdecl; external imgproc_lib;
function cvGetSpatialMoment(moments: pCvMoments; x_order, y_order: Integer): Double; cdecl; external imgproc_lib;
procedure cvMatchTemplate(const image: PCvArr; const templ: PCvArr; result: PCvArr; method: Integer); cdecl; external imgproc_lib;
function cvGetCentralMoment(moments: pCvMoments; x_order, y_order: Integer): Double; cdecl; external imgproc_lib;
procedure cvUndistort2(const src: PCvArr; dst: PCvArr; const camera_matrix: PCvArr; const distortion_coeffs: PCvArr; const new_camera_matrix: PCvArr = nil);
  cdecl; external imgproc_lib;
function cvGetAffineTransform(const src: pCvPoint2D32f; const dst: pCvPoint2D32f; map_matrix: pCvMat): pCvMat; cdecl; external imgproc_lib;
procedure cvUndistortPoints(const src: pCvMat; dst: pCvMat; const camera_matrix: pCvMat; const dist_coeffs: pCvMat; const R: pCvMat = nil;
  const P: pCvMat = nil); cdecl; external imgproc_lib;

function cvStartFindContours(image: PCvArr; storage: pCvMemStorage; header_size: Integer { =sizeof(TCvContour)) }; mode: Integer { =  CV_RETR_LIST };
  method: Integer { =CV_CHAIN_APPROX_SIMPLE }; offset: TCvPoint { =cvPoint(0,0) } ): pCvContourScanner; cdecl; external imgproc_lib;
function cvFindNextContour(scanner: pCvContourScanner): pCvSeq; cdecl; external imgproc_lib;
procedure cvSubstituteContour(scanner: pCvContourScanner; new_contour: pCvSeq); cdecl; external imgproc_lib;
function cvEndFindContours(Var scanner: pCvContourScanner): pCvSeq; cdecl; external imgproc_lib;

function cvCompareHist(hist1: pCvHistogram; hist2: pCvHistogram; method: Integer): Double; cdecl; external imgproc_lib;

procedure cvAcc(const image: PCvArr; sum: PCvArr; const mask: PCvArr = nil); cdecl; external imgproc_lib;
procedure cvSquareAcc(const image: PCvArr; sqsum: PCvArr; const mask: PCvArr = nil); cdecl; external imgproc_lib;
procedure cvMultiplyAcc(const image1: PCvArr; const image2: PCvArr; acc: PCvArr; const mask: PCvArr = nil); cdecl; external imgproc_lib;
procedure cvRunningAvg(const image: PCvArr; acc: PCvArr; alpha: Double; const mask: PCvArr = nil); cdecl; external imgproc_lib;

procedure cvFilter2D(const src: PCvArr; dst: PCvArr; const kernel: pCvMat; anchor: TCvPoint); cdecl; external imgproc_lib;

procedure cvFilter2D(const src: PCvArr; dst: PCvArr; const kernel: pCvMat);
begin
  cvFilter2D(src, dst, kernel, CvPoint(-1, -1));
end;

function cvCreatePyramid(const img: PCvArr; extra_layers: Integer; rate: Double; const layer_sizes: pCvSize = nil; bufarr: PCvArr = nil; calc: Integer = 1;
  filter: Integer = CV_GAUSSIAN_5x5): ppCvMat; cdecl; external imgproc_lib;
procedure cvReleasePyramid(var pyramid: ppCvMat; extra_layers: Integer); cdecl; external imgproc_lib;
procedure cvPyrMeanShiftFiltering(const src: PCvArr; dst: PCvArr; sp: Double; sr: Double; max_level: Integer { = 1 };
  termcrit: TCvTermCriteria { = CV_DEFAULT(cvTermCriteria(CV_TERMCRIT_ITER + CV_TERMCRIT_EPS, 5, 1)) } ); cdecl; external imgproc_lib;
procedure cvWatershed(const image: PCvArr; markers: PCvArr); cdecl; external imgproc_lib;
procedure cvConvertMaps(const mapx: PCvArr; const mapy: PCvArr; mapxy: PCvArr; mapalpha: PCvArr); cdecl; external imgproc_lib;
procedure cvInitUndistortRectifyMap(const camera_matrix: pCvMat; const dist_coeffs: pCvMat; const R: pCvMat; const new_camera_matrix: pCvMat; mapx: PCvArr;
  mapy: PCvArr); cdecl; external imgproc_lib;
function cvGetNormalizedCentralMoment(moments: pCvMoments; x_order: Integer; y_order: Integer): Double; cdecl; external imgproc_lib;
procedure cvGetHuMoments(moments: pCvMoments; hu_moments: pCvHuMoments); cdecl; external imgproc_lib;
function cvSampleLine(const image: PCvArr; pt1: TCvPoint; pt2: TCvPoint; buffer: Pointer; connectivity: Integer = 8): Integer; cdecl; external imgproc_lib;
procedure cvGetRectSubPix(const src: PCvArr; dst: PCvArr; center: TCvPoint2D32f); cdecl; external imgproc_lib;
procedure cvGetQuadrangleSubPix(const src: PCvArr; dst: PCvArr; const map_matrix: pCvMat); cdecl; external imgproc_lib;
function cvCalcEMD2(const signature1: PCvArr; const signature2: PCvArr; distance_type: Integer; distance_func: TCvDistanceFunction = nil;
  const cost_matrix: PCvArr = nil; flow: PCvArr = nil; lower_bound: pfloat = nil; userdata: Pointer = nil): float; cdecl; external imgproc_lib;
function cvApproxChains(src_seq: pCvSeq; storage: pCvMemStorage; method: Integer = CV_CHAIN_APPROX_SIMPLE; parameter: Double = 0;
  minimal_perimeter: Integer = 0; recursive: Integer = 0): pCvSeq; cdecl; external imgproc_lib;
procedure cvStartReadChainPoints(chain: pCvChain; reader: pCvChainPtReader); cdecl; external imgproc_lib;
function cvReadChainPoint(reader: pCvChainPtReader): TCvPoint; cdecl; external imgproc_lib;
function cvFitEllipse2(const points: PCvArr): TCvBox2D; cdecl; external imgproc_lib;
function cvMaxRect(const rect1: pCvRect; const rect2: pCvRect): TCvRect; cdecl; external imgproc_lib;
function cvPointSeqFromMat(seq_kind: Integer; const mat: PCvArr; contour_header: pCvContour; block: pCvSeqBlock): pCvSeq; cdecl; external imgproc_lib;
function cvPointPolygonTest(const contour: PCvArr; pt: TCvPoint2D32f; measure_dist: Integer): Double; cdecl; external imgproc_lib;
procedure cvSetHistBinRanges(hist: pCvHistogram; ranges: pSingle; uniform: Integer = 1); cdecl; external imgproc_lib;
function cvMakeHistHeaderForArray(dims: Integer; sizes: PInteger; hist: pCvHistogram; data: pfloat; ranges: ppfloat = nil; uniform: Integer = 1): pCvHistogram;
  cdecl; external imgproc_lib;
procedure cvNormalizeHist(hist: pCvHistogram; factor: Double); cdecl; external imgproc_lib;
procedure cvThreshHist(hist: pCvHistogram; threshold: Double); cdecl; external imgproc_lib;
procedure cvCopyHist(const src: pCvHistogram; Var dst: pCvHistogram); cdecl; external imgproc_lib;
procedure cvCalcBayesianProb(Var src: pCvHistogram; number: Integer; Var dst: pCvHistogram); cdecl; external imgproc_lib;
procedure cvCalcArrBackProjectPatch(var image: PCvArr; dst: PCvArr; range: TCvSize; hist: pCvHistogram; method: Integer; factor: Double); cdecl;
  external imgproc_lib;
procedure cvCalcProbDensity(const hist1: pCvHistogram; const hist2: pCvHistogram; dst_hist: pCvHistogram; scale: Double = 255); cdecl; external imgproc_lib;
procedure cvDistTransform(const src: PCvArr; dst: PCvArr; distance_type: Integer = CV_DIST_L2; mask_size: Integer = 3; const mask: pfloat = nil;
  labels: PCvArr = nil; labelType: Integer = CV_DIST_LABEL_CCOMP); cdecl; external imgproc_lib;
procedure cvPreCornerDetect(const image: PCvArr; corners: PCvArr; aperture_size: Integer = 3); cdecl; external imgproc_lib;
procedure cvCornerEigenValsAndVecs(const image: PCvArr; eigenvv: PCvArr; block_size: Integer; aperture_size: Integer = 3); cdecl; external imgproc_lib;
procedure cvCornerMinEigenVal(const image: PCvArr; eigenval: PCvArr; block_size: Integer; aperture_size: Integer = 3); cdecl; external imgproc_lib;
procedure cvCornerHarris(const image: PCvArr; harris_response: PCvArr; block_size: Integer; aperture_size: Integer = 3; k: Double = 0.04); cdecl;
  external imgproc_lib;
procedure cvFitLine(const points: PCvArr; dist_type: Integer; param: Double; reps: Double; aeps: Double; Var line: Single); cdecl; external imgproc_lib;

end.
