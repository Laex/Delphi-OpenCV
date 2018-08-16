(* **************************************************************************************************
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
  opencv\modules\imgproc\include\opencv2\imgproc\types_c.h
  ************************************************************************************************* *)

{$I OpenCV.inc}
unit ocv.imgproc.types_c;

interface

uses
  ocv.core.types_c;

(* Connected component structure *)
type
  pCvConnectedComp = ^TCvConnectedComp;

  TCvConnectedComp = record
    area: Double; (* area of the connected component *)
    value: TCvScalar; (* average color of the connected component *)
    rect: TCvRect; (* ROI of the component *)
    contour: ^TCvSeq; (* optional component boundary *)
  end;

  (* Image smooth methods *)
const
  CV_BLUR_NO_SCALE = 0;
  CV_BLUR = 1;
  CV_GAUSSIAN = 2;
  CV_MEDIAN = 3;
  CV_BILATERAL = 4;

  (* Filters used in pyramid decomposition *)
  CV_GAUSSIAN_5x5 = 7;

  (* Special filters *)
  CV_SCHARR = -1;
  CV_MAX_SOBEL_KSIZE = 7;

  (* Constants for color conversion *)
  CV_BGR2BGRA = 0;
  CV_RGB2RGBA = CV_BGR2BGRA;
  CV_BGRA2BGR = 1;
  CV_RGBA2RGB = CV_BGRA2BGR;
  CV_BGR2RGBA = 2;
  CV_RGB2BGRA = CV_BGR2RGBA;
  CV_RGBA2BGR = 3;
  CV_BGRA2RGB = CV_RGBA2BGR;
  CV_BGR2RGB = 4;
  CV_RGB2BGR = CV_BGR2RGB;
  CV_BGRA2RGBA = 5;
  CV_RGBA2BGRA = CV_BGRA2RGBA;
  CV_BGR2GRAY = 6;
  CV_RGB2GRAY = 7;
  CV_GRAY2BGR = 8;
  CV_GRAY2RGB = CV_GRAY2BGR;
  CV_GRAY2BGRA = 9;
  CV_GRAY2RGBA = CV_GRAY2BGRA;
  CV_BGRA2GRAY = 10;
  CV_RGBA2GRAY = 11;
  CV_BGR2BGR565 = 12;
  CV_RGB2BGR565 = 13;
  CV_BGR5652BGR = 14;
  CV_BGR5652RGB = 15;
  CV_BGRA2BGR565 = 16;
  CV_RGBA2BGR565 = 17;
  CV_BGR5652BGRA = 18;
  CV_BGR5652RGBA = 19;
  CV_GRAY2BGR565 = 20;
  CV_BGR5652GRAY = 21;
  CV_BGR2BGR555 = 22;
  CV_RGB2BGR555 = 23;
  CV_BGR5552BGR = 24;
  CV_BGR5552RGB = 25;
  CV_BGRA2BGR555 = 26;
  CV_RGBA2BGR555 = 27;
  CV_BGR5552BGRA = 28;
  CV_BGR5552RGBA = 29;
  CV_GRAY2BGR555 = 30;
  CV_BGR5552GRAY = 31;
  CV_BGR2XYZ = 32;
  CV_RGB2XYZ = 33;
  CV_XYZ2BGR = 34;
  CV_XYZ2RGB = 35;
  CV_BGR2YCrCb = 36;
  CV_RGB2YCrCb = 37;
  CV_YCrCb2BGR = 38;
  CV_YCrCb2RGB = 39;
  CV_BGR2HSV = 40;
  CV_RGB2HSV = 41;
  CV_BGR2Lab = 44;
  CV_RGB2Lab = 45;
  CV_BayerBG2BGR = 46;
  CV_BayerGB2BGR = 47;
  CV_BayerRG2BGR = 48;
  CV_BayerGR2BGR = 49;
  CV_BayerBG2RGB = CV_BayerRG2BGR;
  CV_BayerGB2RGB = CV_BayerGR2BGR;
  CV_BayerRG2RGB = CV_BayerBG2BGR;
  CV_BayerGR2RGB = CV_BayerGB2BGR;
  CV_BGR2Luv = 50;
  CV_RGB2Luv = 51;
  CV_BGR2HLS = 52;
  CV_RGB2HLS = 53;
  CV_HSV2BGR = 54;
  CV_HSV2RGB = 55;
  CV_Lab2BGR = 56;
  CV_Lab2RGB = 57;
  CV_Luv2BGR = 58;
  CV_Luv2RGB = 59;
  CV_HLS2BGR = 60;
  CV_HLS2RGB = 61;
  CV_BayerBG2BGR_VNG = 62;
  CV_BayerGB2BGR_VNG = 63;
  CV_BayerRG2BGR_VNG = 64;
  CV_BayerGR2BGR_VNG = 65;
  CV_BayerBG2RGB_VNG = CV_BayerRG2BGR_VNG;
  CV_BayerGB2RGB_VNG = CV_BayerGR2BGR_VNG;
  CV_BayerRG2RGB_VNG = CV_BayerBG2BGR_VNG;
  CV_BayerGR2RGB_VNG = CV_BayerGB2BGR_VNG;
  CV_BGR2HSV_FULL = 66;
  CV_RGB2HSV_FULL = 67;
  CV_BGR2HLS_FULL = 68;
  CV_RGB2HLS_FULL = 69;
  CV_HSV2BGR_FULL = 70;
  CV_HSV2RGB_FULL = 71;
  CV_HLS2BGR_FULL = 72;
  CV_HLS2RGB_FULL = 73;
  CV_LBGR2Lab = 74;
  CV_LRGB2Lab = 75;
  CV_LBGR2Luv = 76;
  CV_LRGB2Luv = 77;
  CV_Lab2LBGR = 78;
  CV_Lab2LRGB = 79;
  CV_Luv2LBGR = 80;
  CV_Luv2LRGB = 81;
  CV_BGR2YUV = 82;
  CV_RGB2YUV = 83;
  CV_YUV2BGR = 84;
  CV_YUV2RGB = 85;
  CV_BayerBG2GRAY = 86;
  CV_BayerGB2GRAY = 87;
  CV_BayerRG2GRAY = 88;
  CV_BayerGR2GRAY = 89;
  // YUV 4:2:0 formats family;
  CV_YUV2RGB_NV12 = 90;
  CV_YUV2BGR_NV12 = 91;
  CV_YUV2RGB_NV21 = 92;
  CV_YUV2BGR_NV21 = 93;
  CV_YUV420sp2RGB = CV_YUV2RGB_NV21;
  CV_YUV420sp2BGR = CV_YUV2BGR_NV21;
  CV_YUV2RGBA_NV12 = 94;
  CV_YUV2BGRA_NV12 = 95;
  CV_YUV2RGBA_NV21 = 96;
  CV_YUV2BGRA_NV21 = 97;
  CV_YUV420sp2RGBA = CV_YUV2RGBA_NV21;
  CV_YUV420sp2BGRA = CV_YUV2BGRA_NV21;
  CV_YUV2RGB_YV12 = 98;
  CV_YUV2BGR_YV12 = 99;
  CV_YUV2RGB_IYUV = 100;
  CV_YUV2BGR_IYUV = 101;
  CV_YUV2RGB_I420 = CV_YUV2RGB_IYUV;
  CV_YUV2BGR_I420 = CV_YUV2BGR_IYUV;
  CV_YUV420p2RGB = CV_YUV2RGB_YV12;
  CV_YUV420p2BGR = CV_YUV2BGR_YV12;
  CV_YUV2RGBA_YV12 = 102;
  CV_YUV2BGRA_YV12 = 103;
  CV_YUV2RGBA_IYUV = 104;
  CV_YUV2BGRA_IYUV = 105;
  CV_YUV2RGBA_I420 = CV_YUV2RGBA_IYUV;
  CV_YUV2BGRA_I420 = CV_YUV2BGRA_IYUV;
  CV_YUV420p2RGBA = CV_YUV2RGBA_YV12;
  CV_YUV420p2BGRA = CV_YUV2BGRA_YV12;
  CV_YUV2GRAY_420 = 106;
  CV_YUV2GRAY_NV21 = CV_YUV2GRAY_420;
  CV_YUV2GRAY_NV12 = CV_YUV2GRAY_420;
  CV_YUV2GRAY_YV12 = CV_YUV2GRAY_420;
  CV_YUV2GRAY_IYUV = CV_YUV2GRAY_420;
  CV_YUV2GRAY_I420 = CV_YUV2GRAY_420;
  CV_YUV420sp2GRAY = CV_YUV2GRAY_420;
  CV_YUV420p2GRAY = CV_YUV2GRAY_420;
  // YUV 4:2:2 formats family;
  CV_YUV2RGB_UYVY = 107;
  CV_YUV2BGR_UYVY = 108;
  // CV_YUV2RGB_VYUY = 109;
  // CV_YUV2BGR_VYUY = 110;
  CV_YUV2RGB_Y422 = CV_YUV2RGB_UYVY;
  CV_YUV2BGR_Y422 = CV_YUV2BGR_UYVY;
  CV_YUV2RGB_UYNV = CV_YUV2RGB_UYVY;
  CV_YUV2BGR_UYNV = CV_YUV2BGR_UYVY;
  CV_YUV2RGBA_UYVY = 111;
  CV_YUV2BGRA_UYVY = 112;
  // CV_YUV2RGBA_VYUY = 113;
  // CV_YUV2BGRA_VYUY = 114;
  CV_YUV2RGBA_Y422 = CV_YUV2RGBA_UYVY;
  CV_YUV2BGRA_Y422 = CV_YUV2BGRA_UYVY;
  CV_YUV2RGBA_UYNV = CV_YUV2RGBA_UYVY;
  CV_YUV2BGRA_UYNV = CV_YUV2BGRA_UYVY;
  CV_YUV2RGB_YUY2 = 115;
  CV_YUV2BGR_YUY2 = 116;
  CV_YUV2RGB_YVYU = 117;
  CV_YUV2BGR_YVYU = 118;
  CV_YUV2RGB_YUYV = CV_YUV2RGB_YUY2;
  CV_YUV2BGR_YUYV = CV_YUV2BGR_YUY2;
  CV_YUV2RGB_YUNV = CV_YUV2RGB_YUY2;
  CV_YUV2BGR_YUNV = CV_YUV2BGR_YUY2;
  CV_YUV2RGBA_YUY2 = 119;
  CV_YUV2BGRA_YUY2 = 120;
  CV_YUV2RGBA_YVYU = 121;
  CV_YUV2BGRA_YVYU = 122;
  CV_YUV2RGBA_YUYV = CV_YUV2RGBA_YUY2;
  CV_YUV2BGRA_YUYV = CV_YUV2BGRA_YUY2;
  CV_YUV2RGBA_YUNV = CV_YUV2RGBA_YUY2;
  CV_YUV2BGRA_YUNV = CV_YUV2BGRA_YUY2;
  CV_YUV2GRAY_UYVY = 123;
  CV_YUV2GRAY_YUY2 = 124;
  // CV_YUV2GRAY_VYUY = CV_YUV2GRAY_UYVY;
  CV_YUV2GRAY_Y422 = CV_YUV2GRAY_UYVY;
  CV_YUV2GRAY_UYNV = CV_YUV2GRAY_UYVY;
  CV_YUV2GRAY_YVYU = CV_YUV2GRAY_YUY2;
  CV_YUV2GRAY_YUYV = CV_YUV2GRAY_YUY2;
  CV_YUV2GRAY_YUNV = CV_YUV2GRAY_YUY2;
  // alpha premultiplication;
  CV_RGBA2mRGBA = 125;
  CV_mRGBA2RGBA = 126;
  CV_COLORCVT_MAX = 127;

  (* Sub-pixel interpolation methods *)
  CV_INTER_NN = 0;
  CV_INTER_LINEAR = 1;
  CV_INTER_CUBIC = 2;
  CV_INTER_AREA = 3;
  CV_INTER_LANCZOS4 = 4;

  (* ... and other image warping flags *)
  CV_WARP_FILL_OUTLIERS = 8;
  CV_WARP_INVERSE_MAP = 16;

  (* Shapes of a structuring element for morphological operations *)
  CV_SHAPE_RECT = 0;
  CV_SHAPE_CROSS = 1;
  CV_SHAPE_ELLIPSE = 2;
  CV_SHAPE_CUSTOM = 100;

  (* Morphological operations *)
  CV_MOP_ERODE = 0;
  CV_MOP_DILATE = 1;
  CV_MOP_OPEN = 2;
  CV_MOP_CLOSE = 3;
  CV_MOP_GRADIENT = 4;
  CV_MOP_TOPHAT = 5;
  CV_MOP_BLACKHAT = 6;

  (* Spatial and central moments *)
type
  pCvMoments = ^TCvMoments;

  TCvMoments = record
    m00, m10, m01, m20, m11, m02, m30, m21, m12, m03: Double; (* spatial moments *)
    mu20, mu11, mu02, mu30, mu21, mu12, mu03: Double; (* central moments *)
    inv_sqrt_m00: Double; (* m00 != 0 ? 1/sqrt(m00) : 0 *)
  end;

  (* Hu invariants *)
type
  pCvHuMoments = ^TCvHuMoments;

  TCvHuMoments = record
    hu1, hu2, hu3, hu4, hu5, hu6, hu7: Double; (* Hu invariants *)
  end;

  (* Template matching methods *)
const
  CV_TM_SQDIFF = 0;
  CV_TM_SQDIFF_NORMED = 1;
  CV_TM_CCORR = 2;
  CV_TM_CCORR_NORMED = 3;
  CV_TM_CCOEFF = 4;
  CV_TM_CCOEFF_NORMED = 5;

type
  TCvDistanceFunction = function(var a: Single; var b: Single; user_param: Pointer): Single; CDECL;

const
  (* Contour retrieval modes *)
  CV_RETR_EXTERNAL = 0;
  CV_RETR_LIST = 1;
  CV_RETR_CCOMP = 2;
  CV_RETR_TREE = 3;
  CV_RETR_FLOODFILL = 4;
  (* Contour approximation methods *)
  CV_CHAIN_CODE = 0;
  CV_CHAIN_APPROX_NONE = 1;
  CV_CHAIN_APPROX_SIMPLE = 2;
  CV_CHAIN_APPROX_TC89_L1 = 3;
  CV_CHAIN_APPROX_TC89_KCOS = 4;
  CV_LINK_RUNS = 5;

  (*
    Internal structure that is used for sequental retrieving contours from the image.
    It supports both hierarchical and plane variants of Suzuki algorithm.
  *)
type
  // CvContourScanner = ^_CvContourScanner;
  (* Freeman chain reader state *)
  pCvChainPtReader = ^TCvChainPtReader;

  TCvChainPtReader = record
    code: char;
    pt: TCvPoint;
    deltas: array [0 .. 7, 0 .. 1] of schar;
  end;

  (* initializes 8-element array for fast access to 3x3 neighborhood of a pixel *)
  // CV_INIT_3X3_DELTAS(deltas, step, nch)((deltas): array [0 .. -1] of const = (nch),
  // (deltas)[1] = -(step) + (nch), (deltas)[2] = -(step), (deltas)[3] = -(step) - (nch),
  // (deltas)[4] = -(nch), (deltas)[5] = (step) - (nch), (deltas)[6] = (step),
  // (deltas)[7] = (step) + (nch))

  (* ***************************************************************************************\
    *                              Planar subdivisions                                       *
    *************************************************************************************** *)

type
  TCvSubdiv2DEdge = size_t;
  { EXTERNALSYM CvSubdiv2DEdge }

  pCvSubdiv2DPoint = ^TCvSubdiv2DPoint;
  TCvSubdiv2DPointArray = array [0 .. 0] of pCvSubdiv2DPoint;
  pCvSubdiv2DPointArray = ^TCvSubdiv2DPointArray;

  TCvSubdiv2DPoint = record
    flags: Integer;
    first: TCvSubdiv2DEdge;
    pt: TCvPoint2D32f;
    id: Integer;
  end;

const
  CV_SUBDIV2D_VIRTUAL_POINT_FLAG = (1 shl 30);
{$EXTERNALSYM CV_SUBDIV2D_VIRTUAL_POINT_FLAG}

Type

  pCvQuadEdge2D = ^TCvQuadEdge2D;

  TCvQuadEdge2D = record
    flags: Integer;
    pt: array [0 .. 3] of pCvSubdiv2DPoint;
    next: array [0 .. 3] of TCvSubdiv2DEdge;
  end;

  pCvSubdiv2D = ^TCvSubdiv2D;

  TCvSubdiv2D = record
    // CV_SUBDIV2D_FIELDS()
    // -CV_GRAPH_FIELDS()
    // --CV_SET_FIELDS()
    // --CV_SEQUENCE_FIELDS()
    // ---CV_TREE_NODE_FIELDS(CvSeq);
    flags: Integer; // * Miscellaneous flags.
    eader_size: Integer; // * Size of sequence header.
    h_prev: pCvSeq; // * Previous sequence.
    h_next: pCvSeq; // * Next sequence.
    v_prev: pCvSeq; // * 2nd previous sequence.
    v_next: pCvSeq; // * 2nd next sequence.
    total: Integer; // * Total number of elements.
    elem_size: Integer; // * Size of sequence element in bytes.
    block_max: pShortInt; // * Maximal bound of the last block.
    ptr: pShortInt; // * Current write pointer.
    delta_elems: Integer; // * Grow seq this many at a time.
    storage: pCvMemStorage; // * Where the seq is stored.
    free_blocks: pCvSeqBlock; // * Free blocks list.
    first: pCvSeqBlock; // * Pointer to the first sequence block.
    free_elems: pCvSetElem;
    active_count: Integer;
    edges: pCvSet;
    quad_edges: Integer;
    is_geometry_valid: Integer;
    recent_edge: TCvSubdiv2DEdge;
    topleft: TCvPoint2D32f;
    bottomright: TCvPoint2D32f;
  end;

const
  // Type CvSubdiv2DPointLocation
  CV_PTLOC_ERROR = -2;
  CV_PTLOC_OUTSIDE_RECT = -1;
  CV_PTLOC_INSIDE = 0;
  CV_PTLOC_VERTEX = 1;
  CV_PTLOC_ON_EDGE = 2;

  // Type  CvNextEdgeType
  CV_NEXT_AROUND_ORG = $00;
  CV_NEXT_AROUND_DST = $22;
  CV_PREV_AROUND_ORG = $11;
  CV_PREV_AROUND_DST = $33;
  CV_NEXT_AROUND_LEFT = $13;
  CV_NEXT_AROUND_RIGHT = $31;
  CV_PREV_AROUND_LEFT = $20;
  CV_PREV_AROUND_RIGHT = $02;

  (* get the next edge with the same origin point (counterwise) *)
  // >> Following declaration is a macro definition!
  // CV_SUBDIV2D_NEXT_EDGE(edge)(((CvQuadEdge2D(edge) and ~ 3))^.next: array [0 .. 2] of const);

  (* Contour approximation algorithms *)
  CV_POLY_APPROX_DP = 0;
  (* Shape matching methods *)
  CV_CONTOURS_MATCH_I1 = 1;
  CV_CONTOURS_MATCH_I2 = 2;
  CV_CONTOURS_MATCH_I3 = 3;
  (* Shape orientation *)
  CV_CLOCKWISE = 1;
  CV_COUNTER_CLOCKWISE = 2;

  (* Convexity defect *)
type
  pCvConvexityDefect = ^TCvConvexityDefect;

  TCvConvexityDefect = record
    start: PCvPoint; (* point of the contour where the defect begins *)
    _end: PCvPoint; (* point of the contour where the defect ends *)
    depth_point: PCvPoint; (* the farthest from the convex hull point within the defect *)
    depth: Single; (* distance between the farthest point and the convex hull *)
  end;

  (* Histogram comparison methods *)
const
  CV_COMP_CORREL = 0;

  CV_COMP_CHISQR = 1;

  CV_COMP_INTERSECT = 2;

  CV_COMP_BHATTACHARYYA = 3;

  CV_COMP_HELLINGER = CV_COMP_BHATTACHARYYA;

  (* Mask size for distance transform *)

  CV_DIST_MASK_3 = 3;

  CV_DIST_MASK_5 = 5;

  CV_DIST_MASK_PRECISE = 0;

  (* Content of output label array: connected components or pixels *)

  CV_DIST_LABEL_CCOMP = 0;

  CV_DIST_LABEL_PIXEL = 1;

  (* Distance types for Distance Transform and M-estimators *)

  CV_DIST_USER = -1; (* User defined distance *)

  CV_DIST_L1 = 1; (* distance = |x1-x2| + |y1-y2| *)

  CV_DIST_L2 = 2; (* the simple euclidean distance *)

  CV_DIST_C = 3; (* distance = max(|x1-x2|;|y1-y2|) *)

  CV_DIST_L12 = 4; (* L1-L2 metric: distance = 2(sqrt(1+x*x/2) - 1)) *)

  CV_DIST_FAIR = 5; (* distance = c^2(|x|/c-log(1+|x|/c)); c = 1.3998 *)

  CV_DIST_WELSCH = 6; (* distance = c^2/2(1-exp(-(x/c)^2)); c = 2.9846 *)

  CV_DIST_HUBER = 7; (* distance = |x|<c ? x^2/2 : c(|x|-c/2); c=1.345 *)

  (* Threshold types *)

  CV_THRESH_BINARY = 0; (* value = value > threshold ? max_value : 0 *)

  CV_THRESH_BINARY_INV = 1; (* value = value > threshold ? 0 : max_value *)

  CV_THRESH_TRUNC = 2; (* value = value > threshold ? threshold : value *)

  CV_THRESH_TOZERO = 3; (* value = value > threshold ? value : 0 *)

  CV_THRESH_TOZERO_INV = 4; (* value = value > threshold ? 0 : value *)

  CV_THRESH_MASK = 7;

  CV_THRESH_OTSU = 8;
  (* use Otsu algorithm to choose the optimal threshold value;
    combine = the flag with one of the above CV_THRESH_* values;
    {$EXTERNALSYM combine}


    (* Adaptive threshold methods *)
  CV_ADAPTIVE_THRESH_MEAN_C = 0;

  CV_ADAPTIVE_THRESH_GAUSSIAN_C = 1;

  (* FloodFill flags *)

  CV_FLOODFILL_FIXED_RANGE = (1 shl 16);

  CV_FLOODFILL_MASK_ONLY = (1 shl 17);

  (* Canny edge detector flags *)

  CV_CANNY_L2_GRADIENT = (1 shl 31);

  (* Variants of a Hough transform *)

  CV_HOUGH_STANDARD = 0;

  CV_HOUGH_PROBABILISTIC = 1;

  CV_HOUGH_MULTI_SCALE = 2;

  CV_HOUGH_GRADIENT = 3;

  (* Fast search data structures *)
Type
  pCvFeatureTree = ^TCvFeatureTree;

  TCvFeatureTree = record
  end;

  pCvLSH = ^TCvLSH;

  TCvLSH = record
  end;

  pCvLSHOperations = ^TCvLSHOperations;

  TCvLSHOperations = record
  end;

implementation

end.
