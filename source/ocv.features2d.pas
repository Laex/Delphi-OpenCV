(*
  *****************************************************************
  Delphi-OpenCV Demo
  Copyright (C) 2013 Project Delphi-OpenCV
  ****************************************************************
  Contributor:
  Laentir Valetov
  email:laex@bk.ru
  ****************************************************************
  You may retrieve the latest version of this file at the GitHub,
  located at git://github.com/Laex/Delphi-OpenCV.git
  ****************************************************************
  The contents of this file are used with permission, subject to
  the Mozilla Public License Version 1.1 (the "License"); you may
  not use this file except in compliance with the License. You may
  obtain a copy of the License at
  http://www.mozilla.org/MPL/MPL-1_1Final.html

  Software distributed under the License is distributed on an
  "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
  implied. See the License for the specific language governing
  rights and limitations under the License.
  *******************************************************************
*)
unit ocv.features2d;

{$I OpenCV.inc}

interface

Uses
{$IFDEF MSWINDOWS}
  Winapi.Windows,
{$ENDIF MSWINDOWS}
  ocv.cclasses,
  ocv.mat;

// nonfree
Type
  pKeyPoint = ^TKeyPoint;

  TKeyPoint = record
    x: Single;
    y: Single;
    size: Single; // !< diameter of the meaningful keypoint neighborhood
    angle: Single; // !< computed orientation of the keypoint (-1 if not applicable);
    // !< it's in [0,360) degrees and measured relative to
    // !< image coordinate system, ie in clockwise.
    response: Single;
    // !< the response by which the most strong keypoints have been selected. Can be used for the further sorting or subsampling
    octave: Integer; // !< octave (pyramid layer) from which the keypoint has been extracted
    class_id: Integer; // !< object class (if the keypoints need to be clustered by an object they belong to)
  end;

  TCVectorKeyPoint = class
  public
    function size(): size_t; virtual; stdcall; abstract;
    procedure push_back(const Val: TKeyPoint); virtual; stdcall; abstract;
    function at(i: Integer): pKeyPoint; virtual; stdcall; abstract;
    function Vector(): Pointer; virtual; stdcall; abstract;
    // ---------------------
    class function Create: TCVectorKeyPoint;
    procedure Free; reintroduce;
  end;

  TSURF = class
  public
    procedure detect(image: TccvMat; keypoints: TCVectorKeyPoint; mask: TccvMat = nil); virtual; stdcall; abstract;
    procedure compute(image: TccvMat; keypoints: TCVectorKeyPoint; Var descriptors: TccvMat); virtual; stdcall; abstract;
    // ------------------------------------------------
    class function Create(hessianThreshold: double = 400; nOctaves: Integer = 4; nOctaveLayers: Integer = 2; extended: BOOL = true;
      upright: BOOL = false): TSURF; overload;
    procedure Free; reintroduce;
  end;

  TSurfFeatureDetector = TSURF;
  TSurfDescriptorExtractor = TSURF;

  TSIFT = class
  public
    procedure detect(image: TccvMat; keypoints: TCVectorKeyPoint; mask: TccvMat = nil); virtual; stdcall; abstract;
    procedure compute(image: TccvMat; keypoints: TCVectorKeyPoint; Var descriptors: TccvMat); virtual; stdcall; abstract;
    // ------------------------------------------------
    class function Create(nfeatures: Integer = 0; nOctaveLayers: Integer = 3; contrastThreshold: double = 0.04; edgeThreshold: double = 10;
      sigma: double = 1.6): TSIFT; overload;
    procedure Free; reintroduce;
  end;

  TSiftFeatureDetector = TSIFT;
  TSiftDescriptorExtractor = TSIFT;

  // features2d

const
  kBytes       = 32;
  HARRIS_SCORE = 0;
  FAST_SCORE   = 1;

Type
  TBRISK = class
  public
    procedure detect(image: TccvMat; keypoints: TCVectorKeyPoint; mask: TccvMat = nil); virtual; stdcall; abstract;
    procedure compute(image: TccvMat; keypoints: TCVectorKeyPoint; Var descriptors: TccvMat); virtual; stdcall; abstract;
    // ------------------------------------------------
    class function Create(thresh: Integer = 30; octaves: Integer = 3; patternScale: Single = 1.0): TBRISK; overload;
    procedure Free; reintroduce;
  end;

  TBRISKFeatureDetector = TBRISK;
  TBRISKDescriptorExtractor = TBRISK;

  TORB = class
  public
    procedure detect(image: TccvMat; keypoints: TCVectorKeyPoint; mask: TccvMat = nil); virtual; stdcall; abstract;
    procedure compute(image: TccvMat; keypoints: TCVectorKeyPoint; Var descriptors: TccvMat); virtual; stdcall; abstract;
    // ------------------------------------------------
    class function Create(nfeatures: Integer = 500; scaleFactor: Single = 1.2; nlevels: Integer = 8; edgeThreshold: Integer = 31;
      firstLevel: Integer = 0; WTA_K: Integer = 2; scoreType: Integer = HARRIS_SCORE; patchSize: Integer = 31): TORB; overload;
    procedure Free; reintroduce;
  end;

  TORBFeatureDetector = TORB;
  TORBDescriptorExtractor = TORB;

  TFREAK = class
  public
    procedure compute(image: TccvMat; keypoints: TCVectorKeyPoint; Var descriptors: TccvMat); virtual; stdcall; abstract;
    // ------------------------------------------------
    class function Create(orientationNormalized: BOOL = true; scaleNormalized: BOOL = true; patternScale: Single = 22.0;
      nOctaves: Integer = 4): TFREAK; overload;
    procedure Free; reintroduce;
  end;

  TFREAKDescriptorExtractor = TFREAK;

  TMSER = class
  public
    procedure detect(image: TccvMat; keypoints: TCVectorKeyPoint; mask: TccvMat = nil); virtual; stdcall; abstract;
    // ------------------------------------------------
    class function Create(_delta: Integer = 5; _min_area: Integer = 60; _max_area: Integer = 14400; _max_variation: double = 0.25;
      _min_diversity: double = 0.2; _max_evolution: Integer = 200; _area_threshold: double = 1.01; _min_margin: double = 0.003;
      _edge_blur_size: Integer = 5): TMSER; overload;
    procedure Free; reintroduce;
  end;

  TMSERFeatureDetector = TMSER;

  TStarDetector = class
  public
    procedure detect(image: TccvMat; keypoints: TCVectorKeyPoint; mask: TccvMat = nil); virtual; stdcall; abstract;
    // ------------------------------------------------
    class function Create(_maxSize: Integer = 45; _responseThreshold: Integer = 30; _lineThresholdProjected: Integer = 10;
      _lineThresholdBinarized: Integer = 8; _suppressNonmaxSize: Integer = 5): TStarDetector; overload;
    procedure Free; reintroduce;
  end;

  TStarFeatureDetector = TStarDetector;

  TFastFeatureDetector = class
  public
    procedure detect(image: TccvMat; keypoints: TCVectorKeyPoint; mask: TccvMat = nil); virtual; stdcall; abstract;
    // ------------------------------------------------
    class function Create(threshold: Integer = 10; nonmaxSuppression: BOOL = true): TFastFeatureDetector; overload;
    procedure Free; reintroduce;
  end;

  TGFTTDetector = class
  public
    procedure detect(image: TccvMat; keypoints: TCVectorKeyPoint; mask: TccvMat = nil); virtual; stdcall; abstract;
    // ------------------------------------------------
    class function Create(maxCorners: Integer = 1000; qualityLevel: double = 0.01; minDistance: double = 1; blockSize: Integer = 3;
      useHarrisDetector: BOOL = false; k: double = 0.04): TGFTTDetector; overload;
    procedure Free; reintroduce;
  end;

  TGFTTFeatureDetector = TGFTTDetector;
  TGoodFeaturesToTrackDetector = TGFTTDetector;

  TSimpleBlobDetectorParams = record
    thresholdStep: Single;
    minThreshold: Single;
    maxThreshold: Single;
    minRepeatability: size_t;
    minDistBetweenBlobs: Single;

    filterByColor: Boolean;
    blobColor: uchar;

    filterByArea: BOOL;
    minArea: Single;
    maxArea: Single;

    filterByCircularity: BOOL;
    minCircularity: Single;
    maxCircularity: Single;

    filterByInertia: BOOL;
    minInertiaRatio: Single;
    maxInertiaRatio: Single;

    filterByConvexity: BOOL;
    minConvexity: Single;
    maxConvexity: Single;
    // -------------------------------
    procedure SetDefault;
  end;

  TSimpleBlobDetector = class
  public
    procedure detect(image: TccvMat; keypoints: TCVectorKeyPoint; mask: TccvMat = nil); virtual; stdcall; abstract;
    // ------------------------------------------------
    class function Create: TSimpleBlobDetector; overload;
    class function Create(SimpleBlobDetectorParams: TSimpleBlobDetectorParams): TSimpleBlobDetector; overload;
    procedure Free; reintroduce;
  end;

  TDenseFeatureDetector = class
  public
    procedure detect(image: TccvMat; keypoints: TCVectorKeyPoint; mask: TccvMat = nil); virtual; stdcall; abstract;
    // ------------------------------------------------
    class function Create(initFeatureScale: Single = 1; featureScaleLevels: Integer = 1; featureScaleMul: Single = 0.1;
      initXyStep: Integer = 6; initImgBound: Integer = 0; varyXyStepWithScale: BOOL = true; varyImgBoundWithScale: BOOL = false)
      : TDenseFeatureDetector; overload;
    procedure Free; reintroduce;
  end;

  TBriefDescriptorExtractor = class
  public
    procedure detect(image: TccvMat; keypoints: TCVectorKeyPoint; mask: TccvMat = nil); virtual; stdcall; abstract;
    // ------------------------------------------------
    class function Create(bytes: Integer = 32): TBriefDescriptorExtractor; overload;
    procedure Free; reintroduce;
  end;

  pDMatch = ^TDMatch;

  TDMatch = record
    queryIdx: Integer; // query descriptor index
    trainIdx: Integer; // train descriptor index
    imgIdx: Integer; // train image index
    distance: Single;
  end;

  TCVectorDMatch = class
  public
    function size(): size_t; virtual; stdcall; abstract;
    procedure push_back(const Val: TDMatch); virtual; stdcall; abstract;
    function at(i: Integer): pDMatch; virtual; stdcall; abstract;
    function Vector(): Pointer; virtual; stdcall; abstract;
    // ---------------------
    class function Create: TCVectorDMatch;
    procedure Free; reintroduce;
  end;

const
  NORM_INF       = 1;
  NORM_L1        = 2;
  NORM_L2        = 4;
  NORM_L2SQR     = 5;
  NORM_HAMMING   = 6;
  NORM_HAMMING2  = 7;
  NORM_TYPE_MASK = 7;
  NORM_RELATIVE  = 8;
  NORM_MINMAX    = 32;

type
  TBFMatcher = class
  public
    procedure match(queryDescriptors: TccvMat; trainDescriptors: TccvMat; matces: TCVectorDMatch; mask: TccvMat = Nil); virtual;
      stdcall; abstract;
    // ---------------------
    class function Create(normType: Integer = NORM_L2; crossCheck: BOOL = false): TBFMatcher;
    procedure Free; reintroduce;
  end;

  TFlannBasedMatcher = class
  public
    procedure match(queryDescriptors: TccvMat; trainDescriptors: TccvMat; matces: TCVectorDMatch; mask: TccvMat = Nil); virtual;
      stdcall; abstract;
    // ---------------------
    class function Create: TFlannBasedMatcher;
    procedure Free; reintroduce;
  end;

procedure DrawMatches(img1: TccvMat; keypoints1: TCVectorKeyPoint; img2: TccvMat; keypoints2: TCVectorKeyPoint; matches1to2: TCVectorDMatch;
  Var outImg: TccvMat); stdcall;

implementation

uses
  ocv.lib, System.Math;

function CreateSURF(hessianThreshold: double; nOctaves: Integer = 4; nOctaveLayers: Integer = 2; extended: BOOL = true;
  upright: BOOL = false): TSURF; stdcall; external opencv_classes_lib;
procedure ReleaseSURF(ex: TSURF); stdcall; external opencv_classes_lib;

function CreateSIFT(nfeatures: Integer = 0; nOctaveLayers: Integer = 3; contrastThreshold: double = 0.04; edgeThreshold: double = 10;
  sigma: double = 1.6): TSIFT; stdcall; external opencv_classes_lib;
procedure ReleaseSIFT(ex: TSIFT); stdcall; external opencv_classes_lib;

function CreateCVectorKeyPoint: TCVectorKeyPoint; stdcall; external opencv_classes_lib;
procedure ReleaseCVectorKeyPoint(ex: TCVectorKeyPoint); stdcall; external opencv_classes_lib;

function CreateCVectorDMatch(): TCVectorDMatch; stdcall; external opencv_classes_lib;
procedure ReleaseCVectorDMatch(ex: TCVectorDMatch); stdcall; external opencv_classes_lib;

function CreateBFMatcher(normType: Integer = NORM_L2; crossCheck: BOOL = false): TBFMatcher; stdcall; external opencv_classes_lib;
procedure ReleaseBFMatcher(ex: TBFMatcher); stdcall; external opencv_classes_lib;

procedure DrawMatches; stdcall; external opencv_classes_lib;

function CreateBRISK(thresh: Integer = 30; octaves: Integer = 3; patternScale: Single = 1.0): TBRISK; stdcall; external opencv_classes_lib;
procedure ReleaseBRISK(ex: TBRISK); stdcall; external opencv_classes_lib;
function CreateORB(nfeatures: Integer = 500; scaleFactor: Single = 1.2; nlevels: Integer = 8; edgeThreshold: Integer = 31;
  firstLevel: Integer = 0; WTA_K: Integer = 2; scoreType: Integer = HARRIS_SCORE; patchSize: Integer = 31): TORB; stdcall;
  external opencv_classes_lib;
procedure ReleaseORB(ex: TORB); stdcall; external opencv_classes_lib;
function CreateFREAK(orientationNormalized: BOOL = true; scaleNormalized: BOOL = true; patternScale: Single = 22.0; nOctaves: Integer = 4)
  : TFREAK; stdcall; external opencv_classes_lib;
procedure ReleaseFREAK(ex: TFREAK); stdcall; external opencv_classes_lib;
function CreateMSER(_delta: Integer = 5; _min_area: Integer = 60; _max_area: Integer = 14400; _max_variation: double = 0.25;
  _min_diversity: double = 0.2; _max_evolution: Integer = 200; _area_threshold: double = 1.01; _min_margin: double = 0.003;
  _edge_blur_size: Integer = 5): TMSER; stdcall; external opencv_classes_lib;
procedure ReleaseMSER(ex: TMSER); stdcall; external opencv_classes_lib;
function CreateStarDetector(_maxSize: Integer = 45; _responseThreshold: Integer = 30; _lineThresholdProjected: Integer = 10;
  _lineThresholdBinarized: Integer = 8; _suppressNonmaxSize: Integer = 5): TStarDetector; stdcall; external opencv_classes_lib;
procedure ReleaseStarDetector(ex: TStarDetector); stdcall; external opencv_classes_lib;
function CreateFastFeatureDetector(threshold: Integer = 10; nonmaxSuppression: BOOL = true): TFastFeatureDetector; stdcall;
  external opencv_classes_lib;
procedure ReleaseFastFeatureDetector(ex: TFastFeatureDetector); stdcall; external opencv_classes_lib;
function CreateGFTTDetector(maxCorners: Integer = 1000; qualityLevel: double = 0.01; minDistance: double = 1; blockSize: Integer = 3;
  useHarrisDetector: BOOL = false; k: double = 0.04): TGFTTDetector; stdcall; external opencv_classes_lib;
procedure ReleaseGFTTDetector(ex: TGFTTDetector); stdcall; external opencv_classes_lib;
function CreateSimpleBlobDetector(SimpleBlobDetectorParams: TSimpleBlobDetectorParams): TSimpleBlobDetector; stdcall;
  external opencv_classes_lib;
function CreateSimpleBlobDetectorDefault(): TSimpleBlobDetector; stdcall; external opencv_classes_lib;
procedure ReleaseSimpleBlobDetector(ex: TSimpleBlobDetector); stdcall; external opencv_classes_lib;
function CreateDenseFeatureDetector(initFeatureScale: Single = 1; featureScaleLevels: Integer = 1; featureScaleMul: Single = 0.1;
  initXyStep: Integer = 6; initImgBound: Integer = 0; varyXyStepWithScale: BOOL = true; varyImgBoundWithScale: BOOL = false)
  : TDenseFeatureDetector; stdcall; external opencv_classes_lib;
procedure ReleaseDenseFeatureDetector(ex: TDenseFeatureDetector); stdcall; external opencv_classes_lib;
function CreateBriefDescriptorExtractor(bytes: Integer = 32): TBriefDescriptorExtractor; stdcall; external opencv_classes_lib;
procedure ReleaseBriefDescriptorExtractor(ex: TBriefDescriptorExtractor); stdcall; external opencv_classes_lib;
function CreateFlannBasedMatcher(): TFlannBasedMatcher; stdcall; external opencv_classes_lib;
procedure ReleaseFlannBasedMatcher(ex: TFlannBasedMatcher); stdcall; external opencv_classes_lib;

{ TSURF }

class function TSURF.Create(hessianThreshold: double; nOctaves, nOctaveLayers: Integer; extended, upright: BOOL): TSURF;
begin
  Result := CreateSURF(hessianThreshold, nOctaves, nOctaveLayers, extended, upright);
end;

procedure TSURF.Free;
begin
  ReleaseSURF(Self);
end;

{ TCVectorKeyPoint }

class function TCVectorKeyPoint.Create: TCVectorKeyPoint;
begin
  Result := CreateCVectorKeyPoint;
end;

procedure TCVectorKeyPoint.Free;
begin
  ReleaseCVectorKeyPoint(Self);
end;

{ TCVectorDMatch }

class function TCVectorDMatch.Create: TCVectorDMatch;
begin
  Result := CreateCVectorDMatch;
end;

procedure TCVectorDMatch.Free;
begin
  ReleaseCVectorDMatch(Self);
end;

{ TBFMatcher }

class function TBFMatcher.Create(normType: Integer = NORM_L2; crossCheck: BOOL = false): TBFMatcher;
begin
  Result := CreateBFMatcher(normType, crossCheck);
end;

procedure TBFMatcher.Free;
begin
  ReleaseBFMatcher(Self);
end;

{ TSIFT }

class function TSIFT.Create(nfeatures, nOctaveLayers: Integer; contrastThreshold, edgeThreshold, sigma: double): TSIFT;
begin
  Result := CreateSIFT(nfeatures, nOctaveLayers, contrastThreshold, edgeThreshold, sigma);
end;

procedure TSIFT.Free;
begin
  ReleaseSIFT(Self);
end;

{ TBRISK }

class function TBRISK.Create(thresh, octaves: Integer; patternScale: Single): TBRISK;
begin
  Result := CreateBRISK(thresh, octaves, patternScale);
end;

procedure TBRISK.Free;
begin
  ReleaseBRISK(Self);
end;

{ TORB }

class function TORB.Create(nfeatures: Integer; scaleFactor: Single; nlevels, edgeThreshold, firstLevel, WTA_K, scoreType,
  patchSize: Integer): TORB;
begin
  Result := CreateORB(nfeatures, scaleFactor, nlevels, edgeThreshold, firstLevel, WTA_K, scoreType, patchSize);
end;

procedure TORB.Free;
begin
  ReleaseORB(Self);
end;

{ TFREAK }

class function TFREAK.Create(orientationNormalized, scaleNormalized: BOOL; patternScale: Single; nOctaves: Integer): TFREAK;
begin
  Result := CreateFREAK(orientationNormalized, scaleNormalized, patternScale, nOctaves);
end;

procedure TFREAK.Free;
begin
  ReleaseFREAK(Self);
end;

{ TMSER }

class function TMSER.Create(_delta, _min_area, _max_area: Integer; _max_variation, _min_diversity: double; _max_evolution: Integer;
  _area_threshold, _min_margin: double; _edge_blur_size: Integer): TMSER;
begin
  Result := CreateMSER(_delta, _min_area, _max_area, _max_variation, _min_diversity, _max_evolution, _area_threshold, _min_margin,
    _edge_blur_size);
end;

procedure TMSER.Free;
begin
  ReleaseMSER(Self);
end;

{ TStarDetector }

class function TStarDetector.Create(_maxSize, _responseThreshold, _lineThresholdProjected, _lineThresholdBinarized,
  _suppressNonmaxSize: Integer): TStarDetector;
begin
  Result := CreateStarDetector(_maxSize, _responseThreshold, _lineThresholdProjected, _lineThresholdBinarized, _suppressNonmaxSize);
end;

procedure TStarDetector.Free;
begin
  ReleaseStarDetector(Self);
end;

{ TFastFeatureDetector }

class function TFastFeatureDetector.Create(threshold: Integer; nonmaxSuppression: BOOL): TFastFeatureDetector;
begin
  Result := CreateFastFeatureDetector(threshold, nonmaxSuppression);
end;

procedure TFastFeatureDetector.Free;
begin
  ReleaseFastFeatureDetector(Self);
end;

{ TGFTTDetector }

class function TGFTTDetector.Create(maxCorners: Integer; qualityLevel, minDistance: double; blockSize: Integer; useHarrisDetector: BOOL;
  k: double): TGFTTDetector;
begin
  Result := CreateGFTTDetector(maxCorners, qualityLevel, minDistance, blockSize, useHarrisDetector, k);
end;

procedure TGFTTDetector.Free;
begin
  ReleaseGFTTDetector(Self);
end;

{ TSimpleBlobDetectorParams }

procedure TSimpleBlobDetectorParams.SetDefault;
begin
  thresholdStep := 10;
  minThreshold := 50;
  maxThreshold := 220;
  minRepeatability := 2;
  minDistBetweenBlobs := 10;

  filterByColor := true;
  blobColor := 0;

  filterByArea := true;
  minArea := 25;
  maxArea := 5000;

  filterByCircularity := false;
  minCircularity := 0.8;
  maxCircularity := MaxSingle;

  filterByInertia := true;
  // minInertiaRatio := 0.6;
  minInertiaRatio := 0.1;
  maxInertiaRatio := MaxSingle;

  filterByConvexity := true;
  // minConvexity := 0.8;
  minConvexity := 0.95;
  maxConvexity := MaxSingle;
end;

{ TSimpleBlobDetector }

class function TSimpleBlobDetector.Create: TSimpleBlobDetector;
begin
  Result := CreateSimpleBlobDetectorDefault;
end;

class function TSimpleBlobDetector.Create(SimpleBlobDetectorParams: TSimpleBlobDetectorParams): TSimpleBlobDetector;
begin
  Result := CreateSimpleBlobDetector(SimpleBlobDetectorParams);
end;

procedure TSimpleBlobDetector.Free;
begin
  ReleaseSimpleBlobDetector(Self);
end;

{ TDenseFeatureDetector }

class function TDenseFeatureDetector.Create(initFeatureScale: Single; featureScaleLevels: Integer; featureScaleMul: Single;
  initXyStep, initImgBound: Integer; varyXyStepWithScale, varyImgBoundWithScale: BOOL): TDenseFeatureDetector;
begin
  Result := CreateDenseFeatureDetector(initFeatureScale, featureScaleLevels, featureScaleMul, initXyStep, initImgBound, varyXyStepWithScale,
    varyImgBoundWithScale);
end;

procedure TDenseFeatureDetector.Free;
begin
  ReleaseDenseFeatureDetector(Self);
end;

{ TBriefDescriptorExtractor }

class function TBriefDescriptorExtractor.Create(bytes: Integer): TBriefDescriptorExtractor;
begin
  Result := CreateBriefDescriptorExtractor(bytes);
end;

procedure TBriefDescriptorExtractor.Free;
begin
  ReleaseBriefDescriptorExtractor(Self);
end;

{ TFlannBasedMatcher }

class function TFlannBasedMatcher.Create: TFlannBasedMatcher;
begin
  Result := CreateFlannBasedMatcher;
end;

procedure TFlannBasedMatcher.Free;
begin
  ReleaseFlannBasedMatcher(Self);
end;

end.
