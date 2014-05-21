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
  //   the use of this software, even if advised of the possibility of such damage.*)

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
  //  opencv\modules\objdetect\include\opencv2\objdetect_c.h
  //  **************************************************************************************************)

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
unit objdetect_c;

interface

Uses core_c, Core.types_c;

/// ****************************************************************************************\
// *                         Haar-like Object Detection functions                           *
// \****************************************************************************************/
//
// #define CV_HAAR_MAGIC_VAL    0x42500000
// #define CV_TYPE_NAME_HAAR    "opencv-haar-classifier"
//
// #define CV_IS_HAAR_CLASSIFIER( haar )                                                    \
// ((haar) != NULL &&                                                                   \
// (((const CvHaarClassifierCascade*)(haar))->flags & CV_MAGIC_MASK)==CV_HAAR_MAGIC_VAL)
const
  // #define CV_HAAR_FEATURE_MAX  3
  CV_HAAR_FEATURE_MAX = 3;

Type
  pCvHaarFeature = ^TCvHaarFeature;

  TCvHaarFeatureRect = record
    r: TCvRect;
    weight: Float;
  end;

  TCvHaarFeature = record
    tilted: Integer; // int tilted;
    // struct
    // {
    // CvRect r;
    // float weight;
    // } rect[CV_HAAR_FEATURE_MAX];
    rect: array [0 .. CV_HAAR_FEATURE_MAX - 1] of TCvHaarFeatureRect;
  end;

  pCvHaarClassifier = ^TCvHaarClassifier;

  TCvHaarClassifier = record
    count: Integer; // int count;
    haar_feature: pCvHaarFeature; // CvHaarFeature* haar_feature;
    threshold: pFloat; // float* threshold;
    left: pInteger; // int* left;
    right: pInteger; // int* right;
    alpha: pFloat; // float* alpha;
  end;

  pCvHaarStageClassifier = ^TCvHaarStageClassifier;

  TCvHaarStageClassifier = record
    count: Integer; // int  count;
    threshold: Float; // float threshold;
    classifier: pCvHaarClassifier; // CvHaarClassifier* classifier;
    next: Integer; // int next;
    child: Integer; // int child;
    parent: Integer; // int parent;
  end;

  // typedef struct CvHidHaarClassifierCascade CvHidHaarClassifierCascade;
  TCvHidHaarClassifierCascade = record

  end;

  pCvHidHaarClassifierCascade = ^TCvHidHaarClassifierCascade;

  pCvHaarClassifierCascade = ^TCvHaarClassifierCascade;

  // typedef struct CvHaarClassifierCascade
  TCvHaarClassifierCascade = record
    flags: Integer; // int  flags;
    count: Integer; // int  count;
    orig_window_size: TCvSize; // CvSize orig_window_size;
    real_window_size: TCvSize; // CvSize real_window_size;
    scale: Double; // double scale;
    stage_classifier: pCvHaarStageClassifier; // CvHaarStageClassifier* stage_classifier;
    hid_cascade: pCvHidHaarClassifierCascade; // CvHidHaarClassifierCascade* hid_cascade;
  end;

  // typedef struct CvAvgComp
  // {
  // CvRect rect;
  // int neighbors;
  // } CvAvgComp;

  // Loads haar classifier cascade from a directory.
  // It is obsolete: convert your cascade to xml and use cvLoad instead
  // CVAPI(CvHaarClassifierCascade*) cvLoadHaarClassifierCascade(
  // const char* directory, CvSize orig_window_size);
function cvLoadHaarClassifierCascade(const directory: PAnsiChar; orig_window_size: TCvSize): pCvHaarClassifierCascade; cdecl;

// CVAPI(void) cvReleaseHaarClassifierCascade( CvHaarClassifierCascade** cascade );
procedure cvReleaseHaarClassifierCascade(Var cascade: pCvHaarClassifierCascade); cdecl;

Const
  CV_HAAR_DO_CANNY_PRUNING = 1;
  CV_HAAR_SCALE_IMAGE = 2;
  CV_HAAR_FIND_BIGGEST_OBJECT = 4;
  CV_HAAR_DO_ROUGH_SEARCH = 8;

  // CVAPI(CvSeq*) cvHaarDetectObjects( const CvArr* image,
  // CvHaarClassifierCascade* cascade, CvMemStorage* storage,
  // double scale_factor CV_DEFAULT(1.1),
  // int min_neighbors CV_DEFAULT(3), int flags CV_DEFAULT(0),
  // CvSize min_size CV_DEFAULT(cvSize(0,0)), CvSize max_size CV_DEFAULT(cvSize(0,0)));

  // CVAPI(CvSeq*)
function cvHaarDetectObjects(const image: pCvArr; cascade: pCvHaarClassifierCascade; storage: pCvMemStorage;
  scale_factor: Double {1.1}; min_neighbors: Integer {3}; flags: Integer {0}; min_size: TCvSize {CV_DEFAULT(cvSize(0,0))};
  max_size: TCvSize {CV_DEFAULT(cvSize(0,0))} ): pCvSeq; cdecl;

/// * sets images for haar classifier cascade */
// CVAPI(void) cvSetImagesForHaarClassifierCascade( CvHaarClassifierCascade* cascade,
// const CvArr* sum, const CvArr* sqsum,
// const CvArr* tilted_sum, double scale );
//
/// * runs the cascade on the specified window */
// CVAPI(int) cvRunHaarClassifierCascade( const CvHaarClassifierCascade* cascade,
// CvPoint pt, int start_stage CV_DEFAULT(0));
//
//
/// ****************************************************************************************\
// *                         Latent SVM Object Detection functions                          *
// \****************************************************************************************/

// DataType: STRUCT position
/// / Structure describes the position of the filter in the feature pyramid
/// / l - level in the feature pyramid
/// / (x, y) - coordinate in level l
type
  pCvLSVMFilterPosition = ^TCvLSVMFilterPosition;

  TCvLSVMFilterPosition = record
    x: Integer;
    y: Integer;
    l: Integer;
  end;

  // DataType: STRUCT filterObject
  // Description of the filter, which corresponds to the part of the object
  // V               - ideal (penalty = 0) position of the partial filter
  // from the root filter position (V_i in the paper)
  // penaltyFunction - vector describes penalty function (d_i in the paper)
  // pf[0] * x + pf[1] * y + pf[2] * x^2 + pf[3] * y^2
  // FILTER DESCRIPTION
  // Rectangular map (sizeX x sizeY),
  // every cell stores feature vector (dimension = p)
  // H               - matrix of feature vectors
  // to set and get feature vectors (i,j)
  // used formula H[(j * sizeX + i) * p + k], where
  // k - component of feature vector in cell (i, j)
  // END OF FILTER DESCRIPTION
Type
  pCvLSVMFilterObject = ^TCvLSVMFilterObject;

  TpCvLSVMFilterObject = array [0 .. 1] of pCvLSVMFilterObject;
  ppCvLSVMFilterObject = ^TpCvLSVMFilterObject;

  TCvLSVMFilterObject = record
    V: TCvLSVMFilterPosition;
    fineFunction: array [0 .. 3] of single;
    sizeX: Integer;
    sizeY: Integer;
    numFeatures: Integer;
    H: pSingle;
  end;

  // data type: STRUCT CvLatentSvmDetector
  // structure contains internal representation of trained Latent SVM detector
  // num_filters          - total number of filters (root plus part) in model
  // num_components       - number of components in model
  // num_part_filters     - array containing number of part filters for each component
  // filters              - root and part filters for all model components
  // b                    - biases for all model components
  // score_threshold      - confidence level threshold
Type
  pCvLatentSvmDetector = ^TCvLatentSvmDetector;

  TCvLatentSvmDetector = record
    num_filters: Integer;
    num_components: Integer;
    num_part_filters: pInteger;
    filters: ppCvLSVMFilterObject;
    b: pSingle;
    score_threshold: single;
  end;

  // data type: STRUCT CvObjectDetection
  // structure contains the bounding box and confidence level for detected object
  // rect                 - bounding box for a detected object
  // score                - confidence level
  pCvObjectDetection = ^TCvObjectDetection;

  TCvObjectDetection = record
    rect: TCvRect;
    score: single;
  end;

  /// /////////////// Object Detection using Latent SVM //////////////
  // load trained detector from a file
  //
  // API
  // CvLatentSvmDetector* cvLoadLatentSvmDetector(const char* filename);
  // INPUT
  // filename             - path to the file containing the parameters of
  // - trained Latent SVM detector
  // OUTPUT
  // trained Latent SVM detector in internal representation

  // CVAPI(CvLatentSvmDetector*) cvLoadLatentSvmDetector(const char* filename);
function cvLoadLatentSvmDetector(const filename: pCVChar): pCvLatentSvmDetector; stdcall;

// release memory allocated for CvLatentSvmDetector structure
//
// API
// void cvReleaseLatentSvmDetector(CvLatentSvmDetector** detector);
// INPUT
// detector             - CvLatentSvmDetector structure to be released
// OUTPUT

// CVAPI(void) cvReleaseLatentSvmDetector(CvLatentSvmDetector** detector);
procedure cvReleaseLatentSvmDetector(Var detector: pCvLatentSvmDetector); stdcall;


// find rectangular regions in the given image that are likely
// to contain objects and corresponding confidence levels
//
// CvSeq* cvLatentSvmDetectObjects(const IplImage* image,
// CvLatentSvmDetector* detector,
// CvMemStorage* storage,
// float overlap_threshold = 0.5f,
// int numThreads = -1);
// INPUT
// image                - image to detect objects in
// detector             - Latent SVM detector in internal representation
// storage              - memory storage to store the resultant sequence
// of the object candidate rectangles
// overlap_threshold    - threshold for the non-maximum suppression algorithm
// = 0.5f [here will be the reference to original paper]
// OUTPUT
// sequence of detected objects (bounding boxes and confidence levels stored in CvObjectDetection structures)

// CVAPI(CvSeq*) cvLatentSvmDetectObjects(IplImage* image,
// CvLatentSvmDetector* detector,
// CvMemStorage* storage,
// float overlap_threshold CV_DEFAULT(0.5f),
// int numThreads CV_DEFAULT(-1));
function cvLatentSvmDetectObjects(image: pIplImage; detector: pCvLatentSvmDetector; storage: pCvMemStorage;
  overlap_threshold: single = 0.5; numThreads: Integer = -1): pCvSeq; stdcall;

// #ifdef __cplusplus
// }
//
// CV_EXPORTS CvSeq* cvHaarDetectObjectsForROC( const CvArr* image,
// CvHaarClassifierCascade* cascade, CvMemStorage* storage,
// std::vector<int>& rejectLevels, std::vector<double>& levelWeightds,
// double scale_factor = 1.1,
// int min_neighbors = 3, int flags = 0,
// CvSize min_size = cvSize(0, 0), CvSize max_size = cvSize(0, 0),
// bool outputRejectLevels = false );
//
// struct CvDataMatrixCode
// {
// char msg[4];
// CvMat* original;
// CvMat* corners;
// };
//
// CV_EXPORTS std::deque<CvDataMatrixCode> cvFindDataMatrix(CvMat *im);
//
// #endif
//
//
// #endif /* __OPENCV_OBJDETECT_C_H__ */

implementation

Uses uLibName;

function cvLatentSvmDetectObjects; external objdetect_dll;
function cvLoadLatentSvmDetector; external objdetect_dll;
procedure cvReleaseLatentSvmDetector; external objdetect_dll;
function cvHaarDetectObjects; external objdetect_dll;
function cvLoadHaarClassifierCascade; external objdetect_dll;
procedure cvReleaseHaarClassifierCascade; external objdetect_dll;

end.
