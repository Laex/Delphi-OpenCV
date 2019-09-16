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
  opencv\modules\objdetect\include\opencv2\objdetect_c.h
  *************************************************************************************************
*)

//
{$I OpenCV.inc}
//
unit ocv.objdetect_c;

interface

uses ocv.core_c, ocv.core.types_c;

/// ****************************************************************************************\
// *                         Haar-like Object Detection functions                           *
// \****************************************************************************************/
const
  // #define CV_HAAR_MAGIC_VAL    0x42500000
  CV_HAAR_MAGIC_VAL = $42500000;
  // #define CV_TYPE_NAME_HAAR    "opencv-haar-classifier"
  CV_TYPE_NAME_HAAR = 'opencv-haar-classifier';

  // #define CV_IS_HAAR_CLASSIFIER( haar )                                                    \
  // ((haar) != NULL &&                                                                   \
  // (((const CvHaarClassifierCascade*)(haar))->flags & CV_MAGIC_MASK)==CV_HAAR_MAGIC_VAL)
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
  TCvAvgComp = record
    rect: TCvRect;
    neighbors: Integer;
  end;

  // Loads haar classifier cascade from a directory.
  // It is obsolete: convert your cascade to xml and use cvLoad instead
  // CVAPI(CvHaarClassifierCascade*) cvLoadHaarClassifierCascade(
  // const char* directory, CvSize orig_window_size);
{$IFDEF SAFELOADLIB}

type
  TcvLoadHaarClassifierCascade = function(const directory: PAnsiChar; orig_window_size: TCvSize): pCvHaarClassifierCascade; cdecl;

var
  cvLoadHaarClassifierCascade: TcvLoadHaarClassifierCascade;
{$ELSE}
function cvLoadHaarClassifierCascade(const directory: PAnsiChar; orig_window_size: TCvSize): pCvHaarClassifierCascade; cdecl;
{$ENDIF}
// CVAPI(void) cvReleaseHaarClassifierCascade( CvHaarClassifierCascade** cascade );
{$IFDEF SAFELOADLIB}

type
  TcvReleaseHaarClassifierCascade = procedure(Var cascade: pCvHaarClassifierCascade); cdecl;

var
  cvReleaseHaarClassifierCascade: TcvReleaseHaarClassifierCascade;
{$ELSE}
procedure cvReleaseHaarClassifierCascade(Var cascade: pCvHaarClassifierCascade); cdecl;
{$ENDIF}

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

{$IFDEF SAFELOADLIB}

type
  TcvHaarDetectObjects = function(const image: pCvArr; cascade: pCvHaarClassifierCascade; storage: pCvMemStorage; scale_factor: Double { 1.1 };
    min_neighbors: Integer { 3 }; flags: Integer { 0 }; min_size: TCvSize { CV_DEFAULT(cvSize(0,0)) }; max_size: TCvSize { CV_DEFAULT(cvSize(0,0)) } )
    : pCvSeq; cdecl;

var
  cvHaarDetectObjects: TcvHaarDetectObjects;
{$ELSE}
function cvHaarDetectObjects(const image: pCvArr; cascade: pCvHaarClassifierCascade; storage: pCvMemStorage; scale_factor: Double { 1.1 };
  min_neighbors: Integer { 3 }; flags: Integer { 0 }; min_size: TCvSize { CV_DEFAULT(cvSize(0,0)) }; max_size: TCvSize { CV_DEFAULT(cvSize(0,0)) } )
  : pCvSeq; cdecl;
{$ENDIF}
(*
  sets images for haar classifier cascade

  CVAPI(void) cvSetImagesForHaarClassifierCascade( CvHaarClassifierCascade* cascade,
  const CvArr* sum, const CvArr* sqsum,
  const CvArr* tilted_sum, double scale );
*)
{$IFDEF SAFELOADLIB}

type
  TcvSetImagesForHaarClassifierCascade = procedure(cascade: pCvHaarClassifierCascade; const sum: pCvArr; const sqsum: pCvArr;
    const tilted_sum: pCvArr; scale: Double); cdecl;

var
  cvSetImagesForHaarClassifierCascade: TcvSetImagesForHaarClassifierCascade;
{$ELSE}
procedure cvSetImagesForHaarClassifierCascade(cascade: pCvHaarClassifierCascade; const sum: pCvArr; const sqsum: pCvArr; const tilted_sum: pCvArr;
  scale: Double); cdecl;
{$ENDIF}
(*
  runs the cascade on the specified window

  CVAPI(int) cvRunHaarClassifierCascade( const CvHaarClassifierCascade* cascade,
  CvPoint pt, int start_stage CV_DEFAULT(0));
*)
{$IFDEF SAFELOADLIB}

type
  TcvRunHaarClassifierCascade = function(const cascade: pCvHaarClassifierCascade; pt: TCvPoint; start_stage: Integer = 0): Integer; cdecl;

var
  cvRunHaarClassifierCascade: TcvRunHaarClassifierCascade;
{$ELSE}
function cvRunHaarClassifierCascade(const cascade: pCvHaarClassifierCascade; pt: TCvPoint; start_stage: Integer = 0): Integer; cdecl;
{$ENDIF}
// ****************************************************************************************
// *                         Latent SVM Object Detection functions                        *
// ****************************************************************************************

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
{$IFDEF SAFELOADLIB}

type
  TcvLoadLatentSvmDetector = function(const filename: pCVChar): pCvLatentSvmDetector; cdecl;

var
  cvLoadLatentSvmDetector: TcvLoadLatentSvmDetector;
{$ELSE}
function cvLoadLatentSvmDetector(const filename: pCVChar): pCvLatentSvmDetector; cdecl;
{$ENDIF}
(*
  release memory allocated for CvLatentSvmDetector structure

  API
  void cvReleaseLatentSvmDetector(CvLatentSvmDetector** detector);
  INPUT
  detector             - CvLatentSvmDetector structure to be released
  OUTPUT

  CVAPI(void) cvReleaseLatentSvmDetector(CvLatentSvmDetector** detector);
*)
{$IFDEF SAFELOADLIB}

type
  TcvReleaseLatentSvmDetector = procedure(Var detector: pCvLatentSvmDetector); cdecl;

var
  cvReleaseLatentSvmDetector: TcvReleaseLatentSvmDetector;
{$ELSE}
procedure cvReleaseLatentSvmDetector(Var detector: pCvLatentSvmDetector); cdecl;
{$ENDIF}
(*
  find rectangular regions in the given image that are likely
  to contain objects and corresponding confidence levels

  CvSeq* cvLatentSvmDetectObjects(const IplImage* image,
  CvLatentSvmDetector* detector,
  CvMemStorage* storage,
  float overlap_threshold = 0.5f,
  int numThreads = -1);
  INPUT
  image                - image to detect objects in
  detector             - Latent SVM detector in internal representation
  storage              - memory storage to store the resultant sequence
  of the object candidate rectangles
  overlap_threshold    - threshold for the non-maximum suppression algorithm
  = 0.5f [here will be the reference to original paper]
  OUTPUT
  sequence of detected objects (bounding boxes and confidence levels stored in CvObjectDetection structures)

  CVAPI(CvSeq* ) cvLatentSvmDetectObjects(IplImage* image,
  CvLatentSvmDetector* detector,
  CvMemStorage* storage,
  float overlap_threshold CV_DEFAULT(0.5f),
  int numThreads CV_DEFAULT(-1));
*)
{$IFDEF SAFELOADLIB}

type
  TcvLatentSvmDetectObjects = function(image: pIplImage; detector: pCvLatentSvmDetector; storage: pCvMemStorage; overlap_threshold: single = 0.5;
    numThreads: Integer = -1): pCvSeq; cdecl;

var
  cvLatentSvmDetectObjects: TcvLatentSvmDetectObjects;
{$ELSE}
function cvLatentSvmDetectObjects(image: pIplImage; detector: pCvLatentSvmDetector; storage: pCvMemStorage; overlap_threshold: single = 0.5;
  numThreads: Integer = -1): pCvSeq; cdecl;
{$ENDIF}
(*
  CV_EXPORTS CvSeq* cvHaarDetectObjectsForROC( const CvArr* image,
  CvHaarClassifierCascade* cascade, CvMemStorage* storage,
  std::vector<int>& rejectLevels, std::vector<double>& levelWeightds,
  double scale_factor = 1.1,
  int min_neighbors = 3, int flags = 0,
  CvSize min_size = cvSize(0, 0), CvSize max_size = cvSize(0, 0),
  bool outputRejectLevels = false );
*)

(*
  struct CvDataMatrixCode
  {
  char msg[4];
  CvMat* original;
  CvMat* corners;
  };

  // CV_EXPORTS std::deque<CvDataMatrixCode> cvFindDataMatrix(CvMat *im);
*)
type
  TCvDataMatrixCode = record
    msg: array [0 .. 3] of cvChar;
    original: PCvMat;
    corners: PCvMat;
  end;

{$IF DEFINED(SAFELOADLIB) AND DEFINED(DEBUG)}
procedure Init_opencv_objdetect_lib;
{$IFEND}

implementation

uses ocv.lib;

{$IFDEF SAFELOADLIB}

Var
  objdetectDLL: Cardinal;

procedure Init_opencv_objdetect_lib;
begin
  objdetectDLL := ocvLoadLibrary(objdetect_lib);
  Assert(objdetectDLL <> 0, 'Can not init ' + objdetect_lib);

  cvLatentSvmDetectObjects := ocvGetProcAddress('cvLatentSvmDetectObjects', objdetectDLL);
  cvLoadLatentSvmDetector := ocvGetProcAddress('cvLoadLatentSvmDetector', objdetectDLL);
  cvReleaseLatentSvmDetector := ocvGetProcAddress('cvReleaseLatentSvmDetector', objdetectDLL);
  cvHaarDetectObjects := ocvGetProcAddress('cvHaarDetectObjects', objdetectDLL);
  cvLoadHaarClassifierCascade := ocvGetProcAddress('cvLoadHaarClassifierCascade', objdetectDLL);
  cvReleaseHaarClassifierCascade := ocvGetProcAddress('cvReleaseHaarClassifierCascade', objdetectDLL);
  cvSetImagesForHaarClassifierCascade := ocvGetProcAddress('cvSetImagesForHaarClassifierCascade', objdetectDLL);
  cvRunHaarClassifierCascade := ocvGetProcAddress('cvRunHaarClassifierCascade', objdetectDLL);

end;
{$ELSE}
function cvLatentSvmDetectObjects(image: pIplImage; detector: pCvLatentSvmDetector; storage: pCvMemStorage; overlap_threshold: single = 0.5;
  numThreads: Integer = -1): pCvSeq; cdecl; external objdetect_lib;
function cvLoadLatentSvmDetector(const filename: pCVChar): pCvLatentSvmDetector; cdecl; external objdetect_lib;
procedure cvReleaseLatentSvmDetector(Var detector: pCvLatentSvmDetector); cdecl; external objdetect_lib;
function cvHaarDetectObjects(const image: pCvArr; cascade: pCvHaarClassifierCascade; storage: pCvMemStorage; scale_factor: Double { 1.1 };
  min_neighbors: Integer { 3 }; flags: Integer { 0 }; min_size: TCvSize { CV_DEFAULT(cvSize(0,0)) }; max_size: TCvSize { CV_DEFAULT(cvSize(0,0)) } )
  : pCvSeq; cdecl; external objdetect_lib;
function cvLoadHaarClassifierCascade(const directory: PAnsiChar; orig_window_size: TCvSize): pCvHaarClassifierCascade; cdecl; external objdetect_lib;
procedure cvReleaseHaarClassifierCascade(Var cascade: pCvHaarClassifierCascade); cdecl; external objdetect_lib;
procedure cvSetImagesForHaarClassifierCascade(cascade: pCvHaarClassifierCascade; const sum: pCvArr; const sqsum: pCvArr; const tilted_sum: pCvArr;
  scale: Double); cdecl; external objdetect_lib;
function cvRunHaarClassifierCascade(const cascade: pCvHaarClassifierCascade; pt: TCvPoint; start_stage: Integer = 0): Integer; cdecl; external objdetect_lib;
{$ENDIF}

end.
