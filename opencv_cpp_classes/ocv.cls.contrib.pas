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

unit ocv.cls.contrib;

{$I OpenCV.inc}

interface

Uses
  ocv.core.types_c,
  ocv.cls.core,
  ocv.cls.types;

Type
  TInputArrayOfIplImage = TArrayOfpIplImage; // InputArrayOfArrays
  TInputArrayOfMat = TArrayOfIMat; // InputArrayOfArrays
  TInputArrayOfInteger = {$IFDEF FPC}specialize {$ENDIF}TArray<Integer>; // InputArray

  IFaceRecognizer = interface(IOCVCommon)
    ['{199DE478-2C78-4347-B553-C062290C78D2}']
    // Trains a FaceRecognizer.
    // CV_WRAP virtual void train(InputArrayOfArrays src, InputArray labels) = 0;
    procedure train(src: TInputArrayOfIplImage; labels: TInputArrayOfInteger); overload;
    procedure train(src: TInputArrayOfMat; labels: TInputArrayOfInteger); overload;

    // Updates a FaceRecognizer.
    // CV_WRAP void update(InputArrayOfArrays src, InputArray labels);
    procedure update(src: TInputArrayOfIplImage; labels: TInputArrayOfInteger);

    // Gets a prediction from a FaceRecognizer.
    // virtual int predict(InputArray src) const = 0;
    function predict(src: pIplImage): Integer; overload;

    // Predicts the label and confidence for a given sample.
    // CV_WRAP virtual void predict(InputArray src, CV_OUT int &label, CV_OUT double &confidence) const = 0;
    procedure predict(src: pIplImage; Var lab: Integer; var confidence: double); overload;

    // Serializes this object to a given filename.
    // CV_WRAP virtual void save(const string& filename) const;
    procedure save(const filename: string);

    // Deserializes this object from a given filename.
    // CV_WRAP virtual void load(const string& filename);
    procedure load(const filename: string);

    // Serializes this object to a given cv::FileStorage.
    // virtual void save(FileStorage& fs) const = 0;

    // Deserializes this object from a given cv::FileStorage.
    // virtual void load(const FileStorage& fs) = 0;

    // Sets additional information as pairs label - info.
    // void setLabelsInfo(const std::map<int, string>& labelsInfo);

    // Gets string information by label
    // string getLabelInfo(const int &label);

    // Gets labels by string
    // vector<int> getLabelsByString(const string& str);
  end;

  TFaceRecognizer = class(TOCVCommon, IFaceRecognizer)
  public
    // CV_EXPORTS_W Ptr<FaceRecognizer> createEigenFaceRecognizer(int num_components = 0, double threshold = DBL_MAX);
    constructor createEigenFaceRecognizer(num_components: Integer = 0; threshold: double = DBL_MAX);
    // CV_EXPORTS_W Ptr<FaceRecognizer> createFisherFaceRecognizer(int num_components = 0, double threshold = DBL_MAX);
    constructor createFisherFaceRecognizer(num_components: Integer = 0; threshold: double = DBL_MAX);
    // CV_EXPORTS_W Ptr<FaceRecognizer> createLBPHFaceRecognizer(int radius=1, int neighbors=8,int grid_x=8, int grid_y=8, double threshold = DBL_MAX);
    constructor createLBPHFaceRecognizer(radius: Integer = 1; neighbors: Integer = 8; grid_x: Integer = 8; grid_y: Integer = 8;
      threshold: double = DBL_MAX);
    destructor Destroy; override;
    procedure train(src: TInputArrayOfIplImage; labels: TInputArrayOfInteger); overload;
    procedure train(src: TInputArrayOfMat; labels: TInputArrayOfInteger); overload;
    procedure update(src: TInputArrayOfIplImage; labels: TInputArrayOfInteger);
    function predict(src: pIplImage): Integer; overload;
    procedure predict(src: pIplImage; Var lab: Integer; var confidence: double); overload;
    procedure save(const filename: string);
    procedure load(const filename: string);
  end;

  (*
    CV_EXPORTS bool initModule_contrib();
  *)
{$IFDEF SAFELOADLIB}

type
  TInitModule_contrib = function(): cbool; cdecl;

var
  InitModule_contrib: TInitModule_contrib;
{$ELSE}
function InitModule_contrib(): cbool; cdecl;
{$ENDIF}

implementation

uses
  ocv.utils,
  ocv.lib;

// CV_EXPORTS_W Ptr<FaceRecognizer> createEigenFaceRecognizer(int num_components = 0, double threshold = DBL_MAX);
function Create_EigenFaceRecognizer(num_components: Integer = 0; threshold: double = DBL_MAX): TOpenCVClass; stdcall;
  external opencv_classes_lib name '_Create_EigenFaceRecognizer@12';
// CV_EXPORTS_W Ptr<FaceRecognizer> createFisherFaceRecognizer(int num_components = 0, double threshold = DBL_MAX);
function Create_FisherFaceRecognizer(num_components: Integer = 0; threshold: double = DBL_MAX): TOpenCVClass; stdcall;
  external opencv_classes_lib name '_Create_FisherFaceRecognizer@12';
// CV_EXPORTS_W Ptr<FaceRecognizer> createLBPHFaceRecognizer(int radius=1, int neighbors=8,int grid_x=8, int grid_y=8, double threshold = DBL_MAX);
function Create_LBPHFaceRecognizer(radius: Integer = 1; neighbors: Integer = 8; grid_x: Integer = 8; grid_y: Integer = 8; threshold: double = DBL_MAX)
  : TOpenCVClass; stdcall; external opencv_classes_lib name '_Create_LBPHFaceRecognizer@24';

procedure DestroyFaceRecognizer(const M: TOpenCVClass); stdcall; external opencv_classes_lib name '_DestroyFaceRecognizer@4';
procedure FaceRecognizerLoad(const M: TOpenCVClass; filename: PAnsiChar); stdcall; external opencv_classes_lib name '_FaceRecognizerLoad@8';
procedure FaceRecognizerSave(const M: TOpenCVClass; filename: PAnsiChar); stdcall; external opencv_classes_lib name '_FaceRecognizerSave@8';

function FaceRecognizerPredict(const M: TOpenCVClass; scr: pIplImage): Integer; stdcall;
  external opencv_classes_lib name '_FaceRecognizerPredict1@8'; overload;
procedure FaceRecognizerPredict(const M: TOpenCVClass; scr: pIplImage; var _label: Integer; var confidence: double); stdcall;
  external opencv_classes_lib name '_FaceRecognizerPredict2@16'; overload;

procedure FaceRecognizerTrain(const M: TOpenCVClass; const n: Integer; scr: Pointer; labels: Pointer); stdcall;
  external opencv_classes_lib name '_FaceRecognizerTrain@16'; overload;
procedure FaceRecognizerTrainMat(const M: TOpenCVClass; const n: Integer; scr: Pointer; labels: Pointer); stdcall;
  external opencv_classes_lib name '_FaceRecognizerTrain@16'; overload;
procedure FaceRecognizerUpdate(const M: TOpenCVClass; const n: Integer; scr: Pointer; labels: Pointer); stdcall;
  external opencv_classes_lib name '_FaceRecognizerUpdate@16';

procedure _DestroyFaceRecognizer(const E: TOpenCVClass); stdcall; external opencv_classes_lib name '_DestroyFaceRecognizer@4';

{ TFaceRecognizer }

constructor TFaceRecognizer.createEigenFaceRecognizer(num_components: Integer; threshold: double);
begin
  inherited Create(Create_EigenFaceRecognizer(num_components, threshold));
end;

constructor TFaceRecognizer.createFisherFaceRecognizer(num_components: Integer; threshold: double);
begin
  inherited Create(Create_FisherFaceRecognizer(num_components, threshold));
end;

constructor TFaceRecognizer.createLBPHFaceRecognizer(radius, neighbors, grid_x, grid_y: Integer; threshold: double);
begin
  inherited Create(Create_LBPHFaceRecognizer(radius, neighbors, grid_x, grid_y, threshold));
end;

destructor TFaceRecognizer.Destroy;
begin
  if Assigned(FData) then
  begin
    _DestroyFaceRecognizer(FData);
    FData := nil;
  end;
  inherited;
end;

procedure TFaceRecognizer.load(const filename: string);
begin
  FaceRecognizerLoad(FData, c_str(filename));
end;

procedure TFaceRecognizer.predict(src: pIplImage; var lab: Integer; var confidence: double);
begin
  FaceRecognizerPredict(FData, src, lab, confidence);
end;

function TFaceRecognizer.predict(src: pIplImage): Integer;
var
  confidence: double;
begin
  FaceRecognizerPredict(FData, src, Result, confidence);
end;

procedure TFaceRecognizer.save(const filename: string);
begin
  FaceRecognizerSave(FData, c_str(filename));
end;

procedure TFaceRecognizer.train(src: TInputArrayOfMat; labels: TInputArrayOfInteger);
Var
  src_mat: {$IFDEF FPC}specialize {$ENDIF}TArray<TOpenCVClass>;
  i: Integer;
begin
  SetLength(src_mat, Length(src));
  for i := 0 to High(src_mat) do
    src_mat[i] := src[i]._InternalData;
  FaceRecognizerTrainMat(FData, Length(src), @src_mat[0], @labels[0]);
end;

procedure TFaceRecognizer.train(src: TInputArrayOfIplImage; labels: TInputArrayOfInteger);
begin
  FaceRecognizerTrain(FData, Length(src), @src[0], @labels[0]);
end;

procedure TFaceRecognizer.update(src: TInputArrayOfIplImage; labels: TInputArrayOfInteger);
begin
  FaceRecognizerUpdate(FData, Length(src), @src[0], @labels[0]);
end;

{$IFDEF SAFELOADLIB}

Var
  contribDLL: Cardinal;

procedure Init_opencv_contrib;
begin
  contribDLL := ocvLoadLibrary(opencv_contrib_lib);
  Assert(contribDLL <> 0, 'Can not init ' + opencv_contrib_lib);

  InitModule_contrib := ocvGetProcAddress('?initModule_contrib@cv@@YA_NXZ', contribDLL);
end;

initialization

Init_opencv_contrib;

{$ELSE}
function InitModule_contrib(): cbool; cdecl; external opencv_contrib_lib name '?initModule_contrib@cv@@YA_NXZ';
{$ENDIF}

end.
