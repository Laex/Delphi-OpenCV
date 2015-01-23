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

interface

Uses
  WinApi.Windows,
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
    class function Create: TSURF; overload;
    class function Create(hessianThreshold: double; nOctaves: Integer = 4; nOctaveLayers: Integer = 2; extended: BOOL = true;
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
    procedure match(queryDescriptors: TccvMat; trainDescriptors: TccvMat; matches: TCVectorDMatch; mask: TccvMat = Nil); virtual;
      stdcall; abstract;
    // ---------------------
    class function Create(normType: Integer = NORM_L2; crossCheck: BOOL = false): TBFMatcher;
    procedure Free; reintroduce;
  end;

procedure DrawMatches(img1: TccvMat; keypoints1: TCVectorKeyPoint; img2: TccvMat; keypoints2: TCVectorKeyPoint; matches1to2: TCVectorDMatch;
  Var outImg: TccvMat); stdcall;

implementation

uses
  ocv.lib;

function CreateSURF: TSURF; stdcall; external opencv_classes_lib;
function CreateSURFFromValue(hessianThreshold: double; nOctaves: Integer = 4; nOctaveLayers: Integer = 2; extended: BOOL = true;
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

{ TSURF }

class function TSURF.Create: TSURF;
begin
  Result := CreateSURF;
end;

class function TSURF.Create(hessianThreshold: double; nOctaves, nOctaveLayers: Integer; extended, upright: BOOL): TSURF;
begin
  Result := CreateSURFFromValue(hessianThreshold, nOctaves, nOctaveLayers, extended, upright);
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

end.
