(*
  *****************************************************************
  Delphi-OpenCV
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

unit ocv.cls.features2d;

{$I OpenCV.inc}

interface

Uses
  ocv.cls.types,
  ocv.cls.core,
  ocv.core_c,
  ocv.core.types_c;

// CV_EXPORTS bool initModule_features2d();
function initModule_features2d: cbool; cdecl;

type

  // ---------------------------- KeyPoint --------------------------
  (*
    pKeyPoint = ^TKeyPoint;
    TKeyPointArray = TArray<TKeyPoint>;
    TKeyPointArrayOfArray = TArray<TKeyPointArray>;

    TKeyPoint = record
    pt: TcvPoint2f; // !< coordinates of the keypoints
    size: Float; // !< diameter of the meaningful keypoint neighborhood
    angle: Float; // !< computed orientation of the keypoint (-1 if not applicable);
    // !< it's in [0,360) degrees and measured relative to
    // !< image coordinate system, ie in clockwise.
    response: Float; // !< the response by which the most strong keypoints have been selected. Can be used for the further sorting or subsampling
    octave: Integer; // !< octave (pyramid layer) from which the keypoint has been extracted
    class_id: Integer; // !< object class (if the keypoints need to be clustered by an object they belong to)
    procedure KeyPoint(_pt: TcvPoint2f; _size: Float; _angle: Float = -1; _response: Float = 0; _octave: Integer = 0;
    _class_id: Integer = -1); overload;
    procedure KeyPoint(x: Float; y: Float; _size: Float; _angle: Float = -1; _response: Float = 0; _octave: Integer = 0;
    _class_id: Integer = -1); overload;
    function hash: size_t;
    procedure convert(const keypoints: TKeyPointArray; Var points2f: TArrayOfcvPoint2f); overload;
    procedure convert(const points2f: TArrayOfcvPoint2f; Var keypoints: TKeyPointArray); overload;
    function overlap(const kp1, kp2: TKeyPoint): Float;
    end;
  *)
  // by ertankucukoglu
  pKeyPoint = ^TKeyPoint;

  TKeyPoint = record
  public type
    TKeyPointArray = TArray<TKeyPoint>;
    TKeyPointArrayOfArray = TArray<TKeyPointArray>;
  public
    pt: TcvPoint2f; // !< coordinates of the keypoints
    size: Float; // !< diameter of the meaningful keypoint neighborhood
    angle: Float; // !< computed orientation of the keypoint (-1 if not applicable);
    // !< it's in [0,360) degrees and measured relative to
    // !< image coordinate system, ie in clockwise.
    response: Float;
    // !< the response by which the most strong keypoints have been selected. Can be used for the further sorting or subsampling
    octave: Integer; // !< octave (pyramid layer) from which the keypoint has been extracted
    class_id: Integer; // !< object class (if the keypoints need to be clustered by an object they belong to)
    procedure KeyPoint(_pt: TcvPoint2f; _size: Float; _angle: Float = -1; _response: Float = 0; _octave: Integer = 0;
      _class_id: Integer = -1); overload;
    procedure KeyPoint(x: Float; y: Float; _size: Float; _angle: Float = -1; _response: Float = 0; _octave: Integer = 0;
      _class_id: Integer = -1); overload;
    function hash: size_t;
    procedure convert(const keypoints: TKeyPointArray; Var points2f: TArrayOfcvPoint2f); overload;
    procedure convert(const points2f: TArrayOfcvPoint2f; Var keypoints: TKeyPointArray); overload;
    function overlap(const kp1, kp2: TKeyPoint): Float;
  end;

  TKeyPointArray = TKeyPoint.TKeyPointArray;
  TKeyPointArrayOfArray = TKeyPoint.TKeyPointArrayOfArray;

  // ! writes vector of keypoints to the file storage
  // CV_EXPORTS void write(FileStorage& fs, const string& name, const vector<KeyPoint>& keypoints);
  // ! reads vector of keypoints from the specified file storage node
  // CV_EXPORTS void read(const FileNode& node, CV_OUT vector<KeyPoint>& keypoints);

  // ---------------------------- KeyPointsFilter --------------------------
  // class CV_EXPORTS KeyPointsFilter

  // ---------------------------- FeatureDetector --------------------------

const
  // feature detector adapter name
  fanGridAdapter = 'Grid'; // GridAdaptedFeatureDetector
  fanPyramidAdapter = 'Pyramid'; // PyramidAdaptedFeatureDetector
  fanDynamicAdapter = 'Dynamic';

  // feature detector name
  fdtFAST = 'FAST'; // FastFeatureDetector
  fdtSTAR = 'STAR'; // StarFeatureDetector
  fdtSIFT = 'SIFT'; // SIFT (nonfree module)
  fdtSURF = 'SURF'; // SURF (nonfree module)
  fdtORB = 'ORB'; // ORB
  fdtBRISK = 'BRISK'; // BRISK
  fdtMSER = 'MSER'; // MSER
  fdtGFTT = 'GFTT'; // GoodFeaturesToTrackDetector
  fdtHARRIS = 'HARRIS'; // GoodFeaturesToTrackDetector with Harris detector enabled
  fdtDense = 'Dense'; // DenseFeatureDetector
  fdtSimpleBlob = 'SimpleBlob'; // SimpleBlobDetector

  // combined format  "adapter name"+"detector name"
  // for example: "GridFAST", "PyramidSTAR" .
  cfdGridFAST = 'GridFAST';
  cfdPyramidSTAR = 'PyramidSTAR';

Type
  IFeatureDetector = interface(IOCVCommon)
    ['{1BA6C295-0C46-4C64-AB37-1073F969830C}']
    procedure detect(const image: PIplImage; Var keypoints: TKeyPointArray; const mask: PIplImage); overload;
    procedure detect(const images: TArrayofPIplImage; Var keypoints: TKeyPointArrayOfArray; masks: TArrayofPIplImage); overload;
    function empty(): boolean;
  end;

  TFeatureDetector = class(TOCVCommon, IFeatureDetector)
  public
    (*
      * Detect keypoints in an image.
      * image        The image.
      * keypoints    The detected keypoints.
      * mask         Mask specifying where to look for keypoints (optional). Must be a char
      *              matrix with non-zero values in the region of interest.
    *)
    // CV_WRAP void detect( const Mat& image, CV_OUT vector<KeyPoint>& keypoints, const Mat& mask=Mat() ) const;
    procedure detect(const image: PIplImage; Var keypoints: TKeyPointArray; const mask: PIplImage); overload;

    (*
      * Detect keypoints in an image set.
      * images       Image collection.
      * keypoints    Collection of keypoints detected in an input images. keypoints[i] is a set of keypoints detected in an images[i].
      * masks        Masks for image set. masks[i] is a mask for images[i].
    *)
    // void detect( const vector<Mat>& images, vector<vector<KeyPoint> >& keypoints, const vector<Mat>& masks=vector<Mat>() ) const;
    procedure detect(const images: TArrayofPIplImage; Var keypoints: TKeyPointArrayOfArray; masks: TArrayofPIplImage); overload;

    // Return true if detector object is empty
    // CV_WRAP virtual bool empty() const;
    function empty(): boolean;
    // Create feature detector by detector name.
    constructor Create(const detectorType: String);
  end;


  // ---------------------------- DescriptorExtractor --------------------------

const
  // extractor adapter name
  eanOpponent = 'Opponent'; // OpponentColorDescriptorExtractor

  // descriptor extractor name
  denSIFT = 'SIFT'; // SIFT
  denSURF = 'SURF'; // SURF
  denBRIEF = 'BRIEF'; // BriefDescriptorExtractor
  denBRISK = 'BRISK'; // BRISK
  denORB = 'ORB'; // ORB
  denFREAK = 'FREAK'; // FREAK

  // A combined format is also supported:
  // descriptor extractor adapter name ( "Opponent" – OpponentColorDescriptorExtractor ) + descriptor extractor name,
  // for example: "OpponentSIFT"
  ceOpponentSIFT = 'OpponentSIFT';

Type

  IDescriptorExtractor = interface(IOCVCommon)
    ['{E9FA5428-0592-487B-9003-EB36B1212050}']
    procedure compute(const image: PIplImage; keypoints: TKeyPointArray; Var descriptors: TMat); overload;
    procedure compute(const images: TArrayofPIplImage; keypoints: TKeyPointArrayOfArray; Var descriptors: TArrayOfTMat); overload;
    function descriptorSize: Integer;
    function descriptorType: Integer;
    function empty: cbool;
  end;

  TDescriptorExtractor = class(TOCVCommon, IDescriptorExtractor)
  public
    (*
      * Compute the descriptors for a set of keypoints in an image.
      * image        The image.
      * keypoints    The input keypoints. Keypoints for which a descriptor cannot be computed are removed.
      * descriptors  Copmputed descriptors. Row i is the descriptor for keypoint i.
    *)
    // CV_WRAP void compute( const Mat& image, CV_OUT CV_IN_OUT vector<KeyPoint>& keypoints, CV_OUT Mat& descriptors ) const;
    procedure compute(const image: PIplImage; keypoints: TKeyPointArray; Var descriptors: TMat); overload;

    (*
      * Compute the descriptors for a keypoints collection detected in image collection.
      * images       Image collection.
      * keypoints    Input keypoints collection. keypoints[i] is keypoints detected in images[i].
      *              Keypoints for which a descriptor cannot be computed are removed.
      * descriptors  Descriptor collection. descriptors[i] are descriptors computed for set keypoints[i].
    *)
    // void compute( const vector<Mat>& images, vector<vector<KeyPoint> >& keypoints, vector<Mat>& descriptors ) const;
    procedure compute(const images: TArrayofPIplImage; keypoints: TKeyPointArrayOfArray; Var descriptors: TArrayOfTMat); overload;

    // CV_WRAP virtual int descriptorSize() const = 0;
    function descriptorSize: Integer;
    // CV_WRAP virtual int descriptorType() const = 0;
    function descriptorType: Integer;

    // CV_WRAP virtual bool empty() const;
    function empty: cbool;

    // CV_WRAP static Ptr<DescriptorExtractor> create( const string& descriptorExtractorType );
    constructor Create(const descriptorExtractorType: String);
  end;

implementation

uses
  ocv.nonfree,
  ocv.utils,
  ocv.lib;

function initModule_features2d; external features2d_lib name '?initModule_features2d@cv@@YA_NXZ';

{ TFeatureDetector }

function Create_FeatureDetector(const detectorType: pAnsiChar): TOpenCVClass; stdcall;
  external opencv_classes_lib name '_Create_FeatureDetector@4';
function Empty_FeatureDetector(const F: TOpenCVClass): cbool; stdcall; external opencv_classes_lib name '_Empty_FeatureDetector@4';
procedure detect_FeatureDetector(const F: TOpenCVClass; image: PIplImage; Var keypointcount: Integer; Var keypoints: pKeyPoint;
  mask: PIplImage); stdcall; external opencv_classes_lib name '_detect_FeatureDetector@20';

constructor TFeatureDetector.Create(const detectorType: String);
begin
  inherited Create(Create_FeatureDetector(detectorType.AsPAnsiChar));
end;

procedure TFeatureDetector.detect(const image: PIplImage; Var keypoints: TKeyPointArray; const mask: PIplImage);
Var
  kpcount: Integer;
  kp: pKeyPoint;
  i: Integer;
begin
  detect_FeatureDetector(FData, image, kpcount, kp, mask);
  SetLength(keypoints, kpcount);
  for i := 0 to kpcount - 1 do
    keypoints[i] := kp[i];
  cvFree_(kp);
end;

procedure TFeatureDetector.detect(const images: TArrayofPIplImage; Var keypoints: TKeyPointArrayOfArray; masks: TArrayofPIplImage);
var
  i: Integer;
begin
  SetLength(keypoints, Length(images));
  SetLength(masks, Length(images));
  for i := 0 to High(images) do
    detect(images[i], keypoints[i], masks[i]);
end;

function TFeatureDetector.empty: boolean;
begin
  Result := Empty_FeatureDetector(FData);
end;

{ TDescriptorExtractor }

function Create_DescriptorExtractor(const descriptorExtractorType: pAnsiChar): TOpenCVClass; stdcall;
  external opencv_classes_lib name '_Create_DescriptorExtractor@4';
function Empty_DescriptorExtractor(const F: TOpenCVClass): cbool; stdcall; external opencv_classes_lib name '_Empty_DescriptorExtractor@4';
procedure compute_DescriptorExtractor(const F: TOpenCVClass; image: PIplImage; keypointcount: Integer; keypoints: pKeyPoint;
  Var mask: TOpenCVClass); stdcall; external opencv_classes_lib name '_compute_DescriptorExtractor@20';
function descriptorSize_DescriptorExtractor(const F: TOpenCVClass): Integer; stdcall;
  external opencv_classes_lib name '_descriptorSize_DescriptorExtractor@4';
function descriptorType_DescriptorExtractor(const F: TOpenCVClass): Integer; stdcall;
  external opencv_classes_lib name '_descriptorType_DescriptorExtractor@4';

procedure TDescriptorExtractor.compute(const image: PIplImage; keypoints: TKeyPointArray; Var descriptors: TMat);
Var
  desc: TOpenCVClass;
begin
  if Length(keypoints) > 0 then
  begin
    compute_DescriptorExtractor(FData, image, Length(keypoints), @keypoints[0], desc);
    descriptors := TMat.Create(desc);
  end
  else
    descriptors := TMat.Create;
end;

procedure TDescriptorExtractor.compute(const images: TArrayofPIplImage; keypoints: TKeyPointArrayOfArray; var descriptors: TArrayOfTMat);
var
  i: Integer;
begin
  SetLength(descriptors, Length(images));
  for i := 0 to High(images) do
    compute(images[i], keypoints[i], descriptors[i]);
end;

constructor TDescriptorExtractor.Create(const descriptorExtractorType: String);
begin
  inherited Create(Create_DescriptorExtractor(descriptorExtractorType.AsPAnsiChar));
end;

function TDescriptorExtractor.descriptorSize: Integer;
begin
  Result := descriptorSize_DescriptorExtractor(FData);
end;

function TDescriptorExtractor.descriptorType: Integer;
begin
  Result := descriptorType_DescriptorExtractor(FData);
end;

function TDescriptorExtractor.empty: cbool;
begin
  Result := Empty_DescriptorExtractor(FData);
end;

{ TKeyPoint }

procedure TKeyPoint.KeyPoint(_pt: TcvPoint2f; _size, _angle, _response: Float; _octave, _class_id: Integer);
begin
  pt := _pt;
  size := _size;
  angle := _angle;
  response := _response;
  octave := _octave;
  class_id := _class_id;
end;

procedure TKeyPoint.convert(const keypoints: TKeyPointArray; var points2f: TArrayOfcvPoint2f);
begin

end;

procedure TKeyPoint.convert(const points2f: TArrayOfcvPoint2f; var keypoints: TKeyPointArray);
begin

end;

function TKeyPoint.hash: size_t;
begin

end;

procedure TKeyPoint.KeyPoint(x, y, _size, _angle, _response: Float; _octave, _class_id: Integer);
begin
  KeyPoint(CvPoint2f(x, y), _size, _angle, _response, _octave, _class_id);
end;

function TKeyPoint.overlap(const kp1, kp2: TKeyPoint): Float;
begin

end;

initialization

initModule_features2d;

end.
