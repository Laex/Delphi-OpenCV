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

unit ocv.cls.objdetect;

{$I OpenCV.inc}

interface

Uses
  Winapi.Windows,
  ocv.cls.types,
  ocv.core.types_c,
  ocv.cls.core;

Type
  ICascadeClassifier = interface(IOCVCommon)
    ['{700C3DEC-F156-4014-9676-5BD9ECC3B01B}']
    function empty(): cbool;
    function load(const FileName: String): cbool;
    // function Read(const node: IFileNode): cbool;
    procedure detectMultiScale(Image: IMat; var objects: TArray<TCvRect>; scaleFactor: double { = 1.1 };
      minNeighbors: integer { = 3 }; flags: integer { = 0 }; minSize: ISize { = Size() };
      maxSize: ISize { = Size() } ); overload;
    procedure detectMultiScale(Image: IMat; Var objects: TVectorRect; var numDetections: TVectorInt;
      scaleFactor: double { = 1.1 }; minNeighbors: integer { = 3 }; flags: integer { = 0 }; minSize: ISize { = Size() };
      maxSize: ISize { = Size() } ); overload;
    procedure detectMultiScale(Image: IMat; Var objects: TVectorRect; var rejectLevels: TVectorInt;
      Var levelWeights: TVectorDouble; scaleFactor: double { = 1.1 }; minNeighbors: integer { = 3 };
      flags: integer { = 0 }; minSize: ISize { = Size() }; maxSize: ISize { = Size() };
      outputRejectLevels: cbool = false); overload;
    function isOldFormatCascade(): cbool;
    function getOriginalWindowSize(): ISize;
    function getFeatureType(): integer;
    function setImage(Image: IMat): cbool;
    // function convert(const oldcascade: String; const newcascade: String): cbool;
  end;

  TCascadeClassifier = class(TOCVCommon, ICascadeClassifier)
  private
  public
    constructor Create; overload;
    (* * @brief Loads a classifier from a file.

      @param filename Name of the file from which the classifier is loaded.
    *)
    constructor Create(const FileName: String); overload;
    destructor Destroy; override;
    (* * @brief Checks whether the classifier has been loaded.
    *)
    function empty(): cbool;
    (* * @brief Loads a classifier from a file.

      @param filename Name of the file from which the classifier is loaded. The file may contain an old
      HAAR classifier trained by the haartraining application or a new cascade classifier trained by the
      traincascade application.
    *)
    function load(const FileName: String): cbool;
    (* * @brief Reads a classifier from a FileStorage node.

      @note The file may contain a new cascade classifier (trained traincascade application) only.
    *)
    // function Read(const node: IFileNode): cbool;

    (* * @brief Detects objects of different sizes in the input image. The detected objects are returned as a list
      of rectangles.

      @param image Matrix of the type CV_8U containing an image where objects are detected.
      @param objects Vector of rectangles where each rectangle contains the detected object, the
      rectangles may be partially outside the original image.
      @param scaleFactor Parameter specifying how much the image size is reduced at each image scale.
      @param minNeighbors Parameter specifying how many neighbors each candidate rectangle should have
      to retain it.
      @param flags Parameter with the same meaning for an old cascade as in the function
      cvHaarDetectObjects. It is not used for a new cascade.
      @param minSize Minimum possible object size. Objects smaller than that are ignored.
      @param maxSize Maximum possible object size. Objects larger than that are ignored.

      The function is parallelized with the TBB library.

      @note
      -   (Python) A face detection example using cascade classifiers can be found at
      opencv_source_code/samples/python2/facedetect.py
    *)
    procedure detectMultiScale(Image: IMat; var objects: TArray<TCvRect>; scaleFactor: double { = 1.1 };
      minNeighbors: integer { = 3 }; flags: integer { = 0 }; minSize: ISize { = Size() };
      maxSize: ISize { = Size() } ); overload;

    (* * @overload
      @param image Matrix of the type CV_8U containing an image where objects are detected.
      @param objects Vector of rectangles where each rectangle contains the detected object, the
      rectangles may be partially outside the original image.
      @param numDetections Vector of detection numbers for the corresponding objects. An object's number
      of detections is the number of neighboring positively classified rectangles that were joined
      together to form the object.
      @param scaleFactor Parameter specifying how much the image size is reduced at each image scale.
      @param minNeighbors Parameter specifying how many neighbors each candidate rectangle should have
      to retain it.
      @param flags Parameter with the same meaning for an old cascade as in the function
      cvHaarDetectObjects. It is not used for a new cascade.
      @param minSize Minimum possible object size. Objects smaller than that are ignored.
      @param maxSize Maximum possible object size. Objects larger than that are ignored.
    *)
    procedure detectMultiScale(Image: IMat; Var objects: TVectorRect; var numDetections: TVectorInt;
      scaleFactor: double { = 1.1 }; minNeighbors: integer { = 3 }; flags: integer { = 0 }; minSize: ISize { = Size() };
      maxSize: ISize { = Size() } ); overload;

    (* * @overload
      if `outputRejectLevels` is `true` returns `rejectLevels` and `levelWeights`
    *)

    procedure detectMultiScale(Image: IMat; Var objects: TVectorRect; var rejectLevels: TVectorInt;
      Var levelWeights: TVectorDouble; scaleFactor: double { = 1.1 }; minNeighbors: integer { = 3 };
      flags: integer { = 0 }; minSize: ISize { = Size() }; maxSize: ISize { = Size() };
      outputRejectLevels: cbool = false); overload;

    function isOldFormatCascade(): cbool;
    function getOriginalWindowSize(): ISize;
    function getFeatureType(): integer;
    function setImage(Image: IMat): cbool;
    // function convert(const oldcascade: String; const newcascade: String): cbool;
  end;

implementation

Uses
  ocv.core_c, ocv.utils, ocv.lib, SysUtils;

// ------------------------------ CascadeClassifier ------------------------------
function _CreateCascadeClassifier: TOpenCVClass; stdcall; external opencv_classes_lib name '_CreateCascadeClassifier@0';
procedure _DestroyCascadeClassifier(const CascadeClassifier: TOpenCVClass); stdcall;
  external opencv_classes_lib name '_DestroyCascadeClassifier@4';
function _get_CascadeClassifier_empty(CascadeClassifier: TOpenCVClass): cbool; stdcall;
  external opencv_classes_lib name '_get_CascadeClassifier_empty@4';
function _get_CascadeClassifier_load(CascadeClassifier: TOpenCVClass; FileName: PAnsiChar): cbool; stdcall;
  external opencv_classes_lib name '_get_CascadeClassifier_load@8';
function _CascadeClassifier_isOldFormatCascade(CascadeClassifier: TOpenCVClass): cbool; stdcall;
  external opencv_classes_lib name '_CascadeClassifier_isOldFormatCascade@4';
function _CascadeClassifier_getOriginalWindowSize(CascadeClassifier: TOpenCVClass): TOpenCVClass; stdcall;
  external opencv_classes_lib name '_CascadeClassifier_getOriginalWindowSize@4';
function _CascadeClassifier_getFeatureType(CascadeClassifier: TOpenCVClass): integer; stdcall;
  external opencv_classes_lib name '_CascadeClassifier_getFeatureType@4';
function _CascadeClassifier_setImage(CascadeClassifier: TOpenCVClass; m: TOpenCVClass): cbool; stdcall;
  external opencv_classes_lib name '_CascadeClassifier_setImage@8';
// function _CascadeClassifier_convert(CascadeClassifier: TOpenCVClass; oldcascade, newcascade: PAnsiChar): cbool; stdcall;
// external opencv_classes_lib name '_CascadeClassifier_convert@12';
procedure _CascadeClassifier_detectMultiScale(CascadeClassifier: TOpenCVClass;
  Image: TOpenCVClass; Var rects: TOpenCVClass; var rectCount: NativeUInt; scaleFactor: double;
  minNeighbors: integer; flags: integer; minSize: TOpenCVClass; maxSize: TOpenCVClass); stdcall;
  external opencv_classes_lib name '_CascadeClassifier_detectMultiScale@40';
procedure _CascadeClassifier_copyAndDestroyRects(rects: TOpenCVClass; objects: Pointer); stdcall;
  external opencv_classes_lib name '_CascadeClassifier_copyAndDestroyRects@8';

{ ------------------------------ TCascadeClassifier ------------------------------ }

// function TCascadeClassifier.convert(const oldcascade, newcascade: String): cbool;
// begin
// Result := _CascadeClassifier_convert(FData, oldcascade.AsPAnsiChar, newcascade.AsPAnsiChar);
// end;

constructor TCascadeClassifier.Create;
begin
  inherited Create(_CreateCascadeClassifier);
end;

constructor TCascadeClassifier.Create(const FileName: String);
begin
  inherited Create(_CreateCascadeClassifier);
  load(FileName);
end;

destructor TCascadeClassifier.Destroy;
begin
  if Assigned(FData) then
    _DestroyCascadeClassifier(FData);
  inherited;
end;

procedure TCascadeClassifier.detectMultiScale(Image: IMat; var objects: TVectorRect; var numDetections: TVectorInt;
  scaleFactor: double; minNeighbors, flags: integer; minSize, maxSize: ISize);
begin
  raise Exception.Create('Not implemented');
end;

procedure TCascadeClassifier.detectMultiScale(Image: IMat; var objects: TVectorRect; var rejectLevels: TVectorInt;
  var levelWeights: TVectorDouble; scaleFactor: double; minNeighbors, flags: integer; minSize, maxSize: ISize;
  outputRejectLevels: cbool);
begin
  raise Exception.Create('Not implemented');
end;

procedure TCascadeClassifier.detectMultiScale(Image: IMat; var objects: TArray<TCvRect>; scaleFactor: double;
  minNeighbors, flags: integer; minSize, maxSize: ISize);
var rects: TOpenCVClass; rectCount: NativeUInt;
begin
  if Not Assigned(FData) then raise Exception.Create('TCascadeClassifier.detectMultiScale: FData is null');
  _CascadeClassifier_detectMultiScale(FData, Image._InternalData, rects, rectCount,
    scaleFactor, minNeighbors, flags, minSize._InternalData, maxSize._InternalData);
	SetLength(objects, rectCount);
  _CascadeClassifier_copyAndDestroyRects(rects, objects);
end;

function TCascadeClassifier.empty: cbool;
begin
  Result := _get_CascadeClassifier_empty(FData);
end;

function TCascadeClassifier.getFeatureType: integer;
begin
  Result := _CascadeClassifier_getFeatureType(FData);
end;

function TCascadeClassifier.getOriginalWindowSize: ISize;
begin
  Result := TSize.Create(_CascadeClassifier_getOriginalWindowSize(FData));
end;

function TCascadeClassifier.isOldFormatCascade: cbool;
begin
  Result := _CascadeClassifier_isOldFormatCascade(FData);
end;

function TCascadeClassifier.load(const FileName: String): cbool;
begin
  Result := _get_CascadeClassifier_load(FData, c_str(FileName));
end;

function TCascadeClassifier.setImage(Image: IMat): cbool;
begin
  Result := _CascadeClassifier_setImage(FData, Image._InternalData);
end;

end.
