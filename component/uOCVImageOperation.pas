// *****************************************************************
// Delphi-OpenCV Demo
// Copyright (C) 2013 Project Delphi-OpenCV
// ****************************************************************
// Contributor:
// Laentir Valetov
// email:laex@bk.ru
// ****************************************************************
// You may retrieve the latest version of this file at the GitHub,
// located at git://github.com/Laex/Delphi-OpenCV.git
// ****************************************************************
// The contents of this file are used with permission, subject to
// the Mozilla Public License Version 1.1 (the "License"); you may
// not use this file except in compliance with the License. You may
// obtain a copy of the License at
// http://www.mozilla.org/MPL/MPL-1_1Final.html
//
// Software distributed under the License is distributed on an
// "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
// implied. See the License for the specific language governing
// rights and limitations under the License.
// *******************************************************************

unit uOCVImageOperation;

interface

uses
  Winapi.Windows,
  System.SysUtils,
  System.Classes,
  System.SyncObjs,
  System.Types,
  uOCVTypes,
  core.types_c, Vcl.Graphics;

type

  TocvCustomImageOperation = class(TPersistent)
  protected
    procedure AssignTo(Dest: TPersistent); override;
  private
    CS: TCriticalSection;
    FOwner: TComponent;
    FFloatParams: TArray<Double>;
    FIntParams: TArray<Integer>;
    FBoolParams: TArray<Boolean>;
    FOnGetParams: TNotifyEvent;
    function GetFloatParam(const index: Integer): Double;
    function GetIntParam(const index: Integer): Integer;
    procedure SetFloatParam(const index: Integer; const Value: Double);
    procedure SetIntParam(const index: Integer; const Value: Integer);
    procedure NotifyGetParams;
    function GetBoolParam(const index: Integer): Boolean;
    procedure SetBoolParam(const index: Integer; const Value: Boolean);
  protected
    function LockTransform: Boolean;
    procedure UnlockTransform;
    property FloatParams[const index: Integer]: Double Read GetFloatParam write SetFloatParam;
    property IntParams[const index: Integer]: Integer Read GetIntParam write SetIntParam;
    property BoolParams[const index: Integer]: Boolean Read GetBoolParam write SetBoolParam;
  public
    constructor Create(AOwner: TComponent); virtual;
    destructor Destroy; override;
    function Transform(const Source: pIplImage; var Destanation: pIplImage): Boolean; virtual; abstract;
  published
    property OnGetParams: TNotifyEvent read FOnGetParams write FOnGetParams;
  end;

  TocvImageOperationClass = class of TocvCustomImageOperation;

  TocvNoneOperation = class(TocvCustomImageOperation)
  public
    function Transform(const Source: pIplImage; var Destanation: pIplImage): Boolean; override;
  end;

  TocvGrayScaleOperation = class(TocvCustomImageOperation)
  public
    function Transform(const Source: pIplImage; var Destanation: pIplImage): Boolean; override;
  end;

  TovcCannyOperation = class(TocvCustomImageOperation)
  protected
    procedure AssignTo(Dest: TPersistent); override;
  private
    FThreshold2: Double;
    FThreshold1: Double;
    FApertureSize: Integer;
    procedure SetApertureSize(const Value: Integer);
    procedure SetThreshold1(const Value: Double);
    procedure SetThreshold2(const Value: Double);
  protected
  public
    constructor Create(AOwner: TComponent); override;
    function Transform(const Source: pIplImage; var Destanation: pIplImage): Boolean; override;
  published
    property Threshold1: Double Read FThreshold1 write SetThreshold1;
    property Threshold2: Double Read FThreshold2 write SetThreshold2;
    property ApertureSize: Integer Read FApertureSize write SetApertureSize default 100;
  end;

  TocvErodeDilateMode = (SHAPE_RECT, SHAPE_CROSS, SHAPE_ELLIPSE, SHAPE_CUSTOM);

  TovcCustomErodeDilateOperation = class(TocvCustomImageOperation)
  protected
    procedure AssignTo(Dest: TPersistent); override;
  private
    FRadius: Integer;
    FIterations: Integer;
    FMorphOp: TocvErodeDilateMode;
    procedure SetRadius(const Value: Integer);
    procedure SetIterations(const Value: Integer);
    procedure SetMorphOp(const Value: TocvErodeDilateMode);
  protected
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Radius: Integer Read FRadius write SetRadius default 5;
    property Iterations: Integer Read FIterations write SetIterations default 5;
    property MorphOp: TocvErodeDilateMode read FMorphOp write SetMorphOp default SHAPE_RECT;
  end;

  TovcErodeOperation = class(TovcCustomErodeDilateOperation)
  public
    function Transform(const Source: pIplImage; var Destanation: pIplImage): Boolean; override;
  end;

  TovcDilateOperation = class(TovcCustomErodeDilateOperation)
  public
    function Transform(const Source: pIplImage; var Destanation: pIplImage): Boolean; override;
  end;

  TocvLaplaceOperation = class(TocvCustomImageOperation)
  protected
    procedure AssignTo(Dest: TPersistent); override;
  private
    FAperture: Integer;
    procedure SetAperture(const Value: Integer);
  public
    constructor Create(AOwner: TComponent); override;
    function Transform(const Source: pIplImage; var Destanation: pIplImage): Boolean; override;
  published
    property Aperture: Integer read FAperture write SetAperture default 3;
  end;

  TovcSobelOperation = class(TocvCustomImageOperation)
  protected
    procedure AssignTo(Dest: TPersistent); override;
  private
    FXOrder: Integer;
    FYOrder: Integer;
    FAperture: Integer;
    procedure SetAperture(const Value: Integer);
    procedure SetXOrder(const Value: Integer);
    procedure SetYOrder(const Value: Integer);
  protected
  public
    constructor Create(AOwner: TComponent); override;
    function Transform(const Source: pIplImage; var Destanation: pIplImage): Boolean; override;
  published
    property XOrder: Integer read FXOrder write SetXOrder default 1;
    property YOrder: Integer read FYOrder write SetYOrder default 1;
    property Aperture: Integer read FAperture write SetAperture default 3;
  end;

  TocvSmoothOperations = (BLUR_NO_SCALE, BLUR, GAUSSIAN, MEDIAN, BILATERAL);

  TovcSmoothOperation = class(TocvCustomImageOperation)
  protected
    procedure AssignTo(Dest: TPersistent); override;
  private
    FSigma2: Double;
    FSize2: Integer;
    FSigma1: Double;
    FSize1: Integer;
    FSmoothOperation: TocvSmoothOperations;
    procedure SetSigma1(const Value: Double);
    procedure Setsigma2(const Value: Double);
    procedure SetSize1(const Value: Integer);
    procedure SetSize2(const Value: Integer);
    procedure SetSmoothOperation(const Value: TocvSmoothOperations);
  public
    constructor Create(AOwner: TComponent); override;
    function Transform(const Source: pIplImage; var Destanation: pIplImage): Boolean; override;
  published
    property sigma1: Double read FSigma1 write SetSigma1;
    property sigma2: Double read FSigma2 write Setsigma2;
    property size1: Integer read FSize1 write SetSize1 default 3;
    property size2: Integer read FSize2 write SetSize2 default 3;
    property SmoothOperation: TocvSmoothOperations read FSmoothOperation write SetSmoothOperation default GAUSSIAN;
  end;

  TocvThresholdType = (THRESH_BINARY, THRESH_BINARY_INV, THRESH_TRUNC, THRESH_TOZERO, THRESH_TOZERO_INV, THRESH_MASK,
    THRESH_OTSU);

  TocvCustomThresholdOperation = class(TocvCustomImageOperation)
  private
    function GetThresholdType: TocvThresholdType;
    procedure SetThresholdType(const Value: TocvThresholdType);
  public
    constructor Create(AOwner: TComponent); override;
  published
    property MaxValue: Double index 0 Read GetFloatParam write SetFloatParam; // default 250;
    property ThresholdType: TocvThresholdType read GetThresholdType write SetThresholdType default THRESH_BINARY; // index 0
  end;

  TocvThresholdOperation = class(TocvCustomThresholdOperation)
  public
    constructor Create(AOwner: TComponent); override;
    function Transform(const Source: pIplImage; var Destanation: pIplImage): Boolean; override;
  published
    property Threshold: Double index 1 Read GetFloatParam write SetFloatParam; // default 50;
  end;

  TocvAdaptiveThresholdType = (ADAPTIVE_THRESH_MEAN_C, ADAPTIVE_THRESH_GAUSSIAN_C);

  TocvAdaptiveThresholdOperation = class(TocvCustomThresholdOperation)
  private
    function GetAdaptiveThresholdType: TocvAdaptiveThresholdType;
    procedure SetAdaptiveThresholdType(const Value: TocvAdaptiveThresholdType);
  public
    constructor Create(AOwner: TComponent); override;
    function Transform(const Source: pIplImage; var Destanation: pIplImage): Boolean; override;
  published
    property AdaptiveThresholdType: TocvAdaptiveThresholdType read GetAdaptiveThresholdType write SetAdaptiveThresholdType
      default ADAPTIVE_THRESH_MEAN_C; // index 1
    property BlockSize: Integer index 2 Read GetIntParam write SetIntParam; // 3
    property Param: Double index 1 Read GetFloatParam write SetFloatParam; // 5;
  end;

  TocvPoint = class(TPersistent)
  protected
    procedure AssignTo(Dest: TPersistent); override;
  private
    FPoint: TPoint;
  published
    property X: Integer read FPoint.X write FPoint.X;
    property Y: Integer read FPoint.Y write FPoint.Y;
  end;

  IocvEditorPropertiesContainer = interface
    ['{418F88DD-E35D-4425-BF24-E753E83D35D6}']
    function GetProperties: TocvCustomImageOperation;
    function GetPropertiesClass: TocvImageOperationClass;
    procedure SetPropertiesClass(Value: TocvImageOperationClass);
  end;

  TocvContourRetrievalModes = (RETR_EXTERNAL, RETR_LIST, RETR_CCOMP, RETR_TREE, RETR_FLOODFILL);

  TocvContourApproximationMethods = (CHAIN_CODE, CHAIN_APPROX_NONE, CHAIN_APPROX_SIMPLE, CHAIN_APPROX_TC89_L1,
    CHAIN_APPROX_TC89_KCOS, LINK_RUNS);

  TocvLineType = (LT_FILLED, LT_8, LT_AA);

  TocvContourDraw = class(TPersistent)
  protected
    procedure AssignTo(Dest: TPersistent); override;
    function GetOwner: TPersistent; override;
  private
    FOwner: TPersistent;
    FOffset: TocvPoint;
    FDrawContours: Boolean;
    FMaxLevel: Integer;
    FHoleColor: TColor;
    FThickness: Integer;
    FLineType: TocvLineType;
    FExternalColor: TColor;
  public
    constructor Create(AOwner: TPersistent);
    destructor Destroy; override;
  published
    property DrawContours: Boolean read FDrawContours write FDrawContours default false;
    property ExternalColor: TColor read FExternalColor write FExternalColor default clGreen;
    property HoleColor: TColor read FHoleColor write FHoleColor default clRed;
    property MaxLevel: Integer read FMaxLevel write FMaxLevel default 2;
    property Thickness: Integer read FThickness write FThickness default 2;
    property LineType: TocvLineType read FLineType write FLineType default LT_AA;
    property Offset: TocvPoint read FOffset write FOffset;
  end;

  TocvContoursOperation = class(TocvCustomImageOperation, IocvEditorPropertiesContainer)
  private
    CS: TCriticalSection;
    FOperation: TocvCustomImageOperation;
    FOperationClass: TocvImageOperationClass;
    FRetrievalMode: TocvContourRetrievalModes;
    FApproximationMethod: TocvContourApproximationMethods;
    FOffset: TocvPoint;
    FContourDraw: TocvContourDraw;
    function LockTransform: Boolean;
    procedure UnlockTransform;
    procedure CreateProperties;
    procedure DestroyProperties;
    procedure RecreateProperties;
    function GetPropertiesClassName: string;
    procedure SetProperties(const Value: TocvCustomImageOperation);
    procedure SetPropertiesClass(Value: TocvImageOperationClass);
    procedure SetPropertiesClassName(const Value: string);
  protected
    function GetProperties: TocvCustomImageOperation;
    function GetPropertiesClass: TocvImageOperationClass;
    {IInterface}
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Transform(const Source: pIplImage; var Destanation: pIplImage): Boolean; override;
    property OperationClass: TocvImageOperationClass read GetPropertiesClass write SetPropertiesClass;
  published
    property OperationClassName: string read GetPropertiesClassName write SetPropertiesClassName;
    property Preprocessing: TocvCustomImageOperation read GetProperties write SetProperties;
    property RetrievalMode: TocvContourRetrievalModes read FRetrievalMode write FRetrievalMode default RETR_LIST;
    property ApproximationMethod: TocvContourApproximationMethods read FApproximationMethod write FApproximationMethod
      default CHAIN_APPROX_SIMPLE;
    property Offset: TocvPoint read FOffset write FOffset;
    property ContourDraw: TocvContourDraw read FContourDraw write FContourDraw;
  end;

  TocvImageOperationCollectionItem = class(TCollectionItem, IocvEditorPropertiesContainer)
  private
    CS: TCriticalSection;
    FOperation: TocvCustomImageOperation;
    FOperationClass: TocvImageOperationClass;
    function LockTransform: Boolean;
    procedure UnlockTransform;
    procedure CreateProperties;
    procedure DestroyProperties;
    procedure RecreateProperties;
    function GetPropertiesClassName: string;
    procedure SetProperties(const Value: TocvCustomImageOperation);
    procedure SetPropertiesClass(Value: TocvImageOperationClass);
    procedure SetPropertiesClassName(const Value: string);
  protected
    function GetProperties: TocvCustomImageOperation;
    function GetPropertiesClass: TocvImageOperationClass;
    function GetDisplayName: string; override;
    {IInterface}
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    function Transform(const Source: pIplImage; var Destanation: pIplImage): Boolean;
    procedure Assign(Source: TPersistent); override;
    property OperationClass: TocvImageOperationClass read GetPropertiesClass write SetPropertiesClass;
  published
    property OperationClassName: string read GetPropertiesClassName write SetPropertiesClassName;
    property Operation: TocvCustomImageOperation read GetProperties write SetProperties;
  end;

  TocvImageOperationCollection = class(TOwnedCollection);

  TocvImageOperation = class(TocvDataSourceAndReceiver, IocvEditorPropertiesContainer)
  private
    CS: TCriticalSection;
    FOperation: TocvCustomImageOperation;
    FOperationClass: TocvImageOperationClass;
    FOperations: TocvImageOperationCollection;
    function LockTransform: Boolean;
    procedure UnlockTransform;
    procedure CreateProperties;
    procedure DestroyProperties;
    procedure RecreateProperties;
    function GetPropertiesClassName: string;
    procedure SetProperties(const Value: TocvCustomImageOperation);
    procedure SetPropertiesClass(Value: TocvImageOperationClass);
    procedure SetPropertiesClassName(const Value: string);
  protected
    procedure TakeImage(const IplImage: pIplImage); override;
    function GetProperties: TocvCustomImageOperation;
    function GetPropertiesClass: TocvImageOperationClass;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property OperationClass: TocvImageOperationClass read GetPropertiesClass write SetPropertiesClass;
  published
    property OperationClassName: string read GetPropertiesClassName write SetPropertiesClassName;
    property Operation: TocvCustomImageOperation read GetProperties write SetProperties;
    property Operations: TocvImageOperationCollection Read FOperations write FOperations;
  end;

  TRegisteredImageOperations = class(TStringList)
  public
    function FindByClassName(const ClassName: String): TocvImageOperationClass;
    function FindByName(const Name: String): TocvImageOperationClass;
    function GetNameByClass(const IOClass: TClass): String;
    procedure RegisterIOClass(const IOClass: TClass; const ClassName: String);
  end;

function GetRegisteredImageOperations: TRegisteredImageOperations;

implementation

Uses
  core_c,
  imgproc_c,
  imgproc.types_c;

Var
  _RegisteredImageOperations: TRegisteredImageOperations = nil;

function GetRegisteredImageOperations: TRegisteredImageOperations;
begin
  if not Assigned(_RegisteredImageOperations) then
    _RegisteredImageOperations := TRegisteredImageOperations.Create;
  Result := _RegisteredImageOperations;
end;

{TocvImageOperation}

procedure TocvImageOperation.SetProperties(const Value: TocvCustomImageOperation);
begin
  if (FOperation <> nil) and (Value <> nil) then
    FOperation.Assign(Value);
end;

procedure TocvImageOperation.SetPropertiesClass(Value: TocvImageOperationClass);
begin
  if FOperationClass <> Value then
  begin
    FOperationClass := Value;
    RecreateProperties;
  end;
end;

procedure TocvImageOperation.CreateProperties;
begin
  if FOperationClass <> nil then
    FOperation := FOperationClass.Create(Self);
end;

procedure TocvImageOperation.DestroyProperties;
begin
  FreeAndNil(FOperation);
end;

procedure TocvImageOperation.RecreateProperties;
begin
  DestroyProperties;
  CreateProperties;
end;

procedure TocvImageOperation.SetPropertiesClassName(const Value: string);
begin
  OperationClass := TocvImageOperationClass(GetRegisteredImageOperations.FindByClassName(Value));
end;

constructor TocvImageOperation.Create(AOwner: TComponent);
begin
  inherited;
  CS := TCriticalSection.Create;
  FOperations := TocvImageOperationCollection.Create(Self, TocvImageOperationCollectionItem);
end;

destructor TocvImageOperation.Destroy;
begin
  if LockTransform then
    if Assigned(FOperation) then
      FreeAndNil(FOperation);
  FOperations.Free;
  CS.Free;
  inherited;
end;

function TocvImageOperation.GetProperties: TocvCustomImageOperation;
begin
  if not Assigned(FOperation) then
    FOperation := TocvNoneOperation.Create(Self);
  Result := FOperation;
end;

function TocvImageOperation.GetPropertiesClass: TocvImageOperationClass;
begin
  Result := TocvImageOperationClass(Operation.ClassType);
end;

function TocvImageOperation.GetPropertiesClassName: string;
begin
  Result := Operation.ClassName;
end;

function TocvImageOperation.LockTransform: Boolean;
begin
  Result := CS.TryEnter;
end;

procedure TocvImageOperation.TakeImage(const IplImage: pIplImage);
var
  Destanation: pIplImage;
  SourceDestanation: pIplImage;
  i: Integer;
begin
  if LockTransform then
    try
      if FOperations.Count > 0 then
      begin
        SourceDestanation := IplImage;
        for i := 0 to FOperations.Count - 1 do
        begin
          if not(FOperations.Items[i] as TocvImageOperationCollectionItem).Transform(SourceDestanation, Destanation) then
            Exit;
          if SourceDestanation <> IplImage then
            cvReleaseImage(SourceDestanation);
          SourceDestanation := Destanation;
        end;
        NotifyReceiver(SourceDestanation);
      end
      else if Assigned(FOperation) and FOperation.Transform(IplImage, Destanation) then
        NotifyReceiver(Destanation);
    finally
      UnlockTransform;
    end;
end;

procedure TocvImageOperation.UnlockTransform;
begin
  CS.Leave;
end;

{TovcImageOperationCanny}

procedure TovcCannyOperation.AssignTo(Dest: TPersistent);
begin
  if Dest is TovcCannyOperation then
  begin
    FThreshold1 := (Dest as TovcCannyOperation).FThreshold1;
    FThreshold2 := (Dest as TovcCannyOperation).FThreshold2;
    FApertureSize := (Dest as TovcCannyOperation).FApertureSize;
  end
  else
    inherited;
end;

constructor TovcCannyOperation.Create {(AOwner: TPersistent)};
begin
  inherited;
  FThreshold1 := 10;
  FThreshold2 := 100;
  FApertureSize := 3;
end;

procedure TovcCannyOperation.SetApertureSize(const Value: Integer);
begin
  if LockTransform then
    try
      FApertureSize := Value;
    finally
      UnlockTransform;
    end;
end;

procedure TovcCannyOperation.SetThreshold1(const Value: Double);
begin
  if LockTransform then
    try
      FThreshold1 := Value;
    finally
      UnlockTransform;
    end;
end;

procedure TovcCannyOperation.SetThreshold2(const Value: Double);
begin
  if LockTransform then
    try
      FThreshold2 := Value;
    finally
      UnlockTransform;
    end;
end;

function TovcCannyOperation.Transform(const Source: pIplImage; var Destanation: pIplImage): Boolean;
Var
  gray: pIplImage;
begin
  Result := false;
  if LockTransform then
    try
      NotifyGetParams;
      // cоздаём одноканальные картинки
      gray := cvCreateImage(cvGetSize(Source), IPL_DEPTH_8U, 1);
      Destanation := cvCreateImage(cvGetSize(Source), IPL_DEPTH_8U, 1);
      // преобразуем в градации cерого
      cvCvtColor(Source, gray, CV_RGB2GRAY);
      // получаем границы
      cvCanny(gray, Destanation, Threshold1, Threshold2, ApertureSize);
      cvReleaseImage(gray);
      Result := true;
    finally
      UnlockTransform;
    end;
end;

{TocvImageOperationGrayScale}

function TocvGrayScaleOperation.Transform(const Source: pIplImage; var Destanation: pIplImage): Boolean;
begin
  Result := false;
  if LockTransform then
    try
      Destanation := cvCreateImage(cvGetSize(Source), IPL_DEPTH_8U, 1);
      cvCvtColor(Source, Destanation, CV_RGB2GRAY);
      Result := true;
    finally
      UnlockTransform;
    end;
end;

{TocvImageOperationNone}

function TocvNoneOperation.Transform(const Source: pIplImage; var Destanation: pIplImage): Boolean;
begin
  Result := false;
  if LockTransform then
    try
      Destanation := cvCloneImage(Source);
      Result := true;
    finally
      UnlockTransform;
    end;
end;

{TCustomOpenCVImgOperation}

procedure TocvCustomImageOperation.AssignTo(Dest: TPersistent);
begin
  if Dest is TocvCustomImageOperation then
  begin
    FFloatParams := (Dest as TocvCustomImageOperation).FFloatParams;
    FIntParams := (Dest as TocvCustomImageOperation).FIntParams;
    FBoolParams := (Dest as TocvCustomImageOperation).FBoolParams;
  end
  else
    inherited;
end;

constructor TocvCustomImageOperation.Create(AOwner: TComponent);
begin
  inherited Create;
  FOwner := AOwner;
  CS := TCriticalSection.Create;
end;

destructor TocvCustomImageOperation.Destroy;
begin
  CS.Free;
  inherited;
end;

function TocvCustomImageOperation.GetBoolParam(const index: Integer): Boolean;
begin
  if (index >= 0) and (index < Length(FBoolParams)) then
    Result := FBoolParams[index]
  else
    Result := false;
end;

function TocvCustomImageOperation.GetFloatParam(const index: Integer): Double;
begin
  if (index >= 0) and (index < Length(FFloatParams)) then
    Result := FFloatParams[index]
  else
    Result := 0;
end;

function TocvCustomImageOperation.GetIntParam(const index: Integer): Integer;
begin
  if (index >= 0) and (index < Length(FIntParams)) then
    Result := FIntParams[index]
  else
    Result := 0;
end;

function TocvCustomImageOperation.LockTransform: Boolean;
begin
  Result := CS.TryEnter;
end;

procedure TocvCustomImageOperation.NotifyGetParams;
begin
  if Assigned(OnGetParams) then
    OnGetParams(Self);
end;

procedure TocvCustomImageOperation.SetBoolParam(const index: Integer; const Value: Boolean);
begin
  if (index >= 0) and LockTransform then
    try
      if index > High(FBoolParams) then
        SetLength(FBoolParams, index + 1);
      FBoolParams[index] := Value;
    finally
      UnlockTransform;
    end;
end;

procedure TocvCustomImageOperation.SetFloatParam(const index: Integer; const Value: Double);
begin
  if (index >= 0) and LockTransform then
    try
      if index > High(FFloatParams) then
        SetLength(FFloatParams, index + 1);
      FFloatParams[index] := Value;
    finally
      UnlockTransform;
    end;
end;

procedure TocvCustomImageOperation.SetIntParam(const index: Integer; const Value: Integer);
begin
  if (index >= 0) and LockTransform then
    try
      if index > High(FIntParams) then
        SetLength(FIntParams, index + 1);
      FIntParams[index] := Value;
    finally
      UnlockTransform;
    end;
end;

procedure TocvCustomImageOperation.UnlockTransform;
begin
  CS.Leave;
end;

{TovcImageOperationSmooth}
Const
  ocvSmoothOperations: array [TocvSmoothOperations] of Integer = (CV_BLUR_NO_SCALE, CV_BLUR, CV_GAUSSIAN, CV_MEDIAN,
    CV_BILATERAL);

procedure TovcSmoothOperation.AssignTo(Dest: TPersistent);
begin
  if Dest is TovcSmoothOperation then
  begin
    FSmoothOperation := (Dest as TovcSmoothOperation).FSmoothOperation;
    FSize1 := (Dest as TovcSmoothOperation).FSize1;
    FSize2 := (Dest as TovcSmoothOperation).FSize2;
    FSigma1 := (Dest as TovcSmoothOperation).FSigma1;
    FSigma2 := (Dest as TovcSmoothOperation).FSigma2;
  end
  else
    inherited;
end;

constructor TovcSmoothOperation.Create {(AOwner: TPersistent)};
begin
  inherited;
  FSmoothOperation := GAUSSIAN;
  FSize1 := 3;
  FSize2 := 3;
  FSigma1 := 0;
  FSigma2 := 0;
end;

procedure TovcSmoothOperation.SetSigma1(const Value: Double);
begin
  if LockTransform then
    try
      FSigma1 := Value;
    finally
      UnlockTransform;
    end;
end;

//
procedure TovcSmoothOperation.Setsigma2(const Value: Double);
begin
  if LockTransform then
    try
      FSigma2 := Value;
    finally
      UnlockTransform;
    end;
end;

//
procedure TovcSmoothOperation.SetSize1(const Value: Integer);
begin
  if LockTransform then
    try
      FSize1 := Value;
    finally
      UnlockTransform;
    end;
end;

//
procedure TovcSmoothOperation.SetSize2(const Value: Integer);
begin
  if LockTransform then
    try
      FSize2 := Value;
    finally
      UnlockTransform;
    end;
end;

procedure TovcSmoothOperation.SetSmoothOperation(const Value: TocvSmoothOperations);
begin
  if LockTransform then
    try
      FSmoothOperation := Value;
    finally
      UnlockTransform;
    end;
end;

function TovcSmoothOperation.Transform(const Source: pIplImage; var Destanation: pIplImage): Boolean;
begin
  Result := false;
  if LockTransform then
    try
      NotifyGetParams;
      Destanation := cvCloneImage(Source);
      cvSmooth(Source, Destanation, ocvSmoothOperations[SmoothOperation], size1, size2, sigma1, sigma2);
      Result := true;
    finally
      UnlockTransform;
    end;
end;

{TRegisteredImageOperations}

function TRegisteredImageOperations.FindByClassName(const ClassName: String): TocvImageOperationClass;
Var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    if TocvImageOperationClass(Objects[i]).ClassName = ClassName then
      Exit(TocvImageOperationClass(Objects[i]));
end;

function TRegisteredImageOperations.FindByName(const Name: String): TocvImageOperationClass;
Var
  i: Integer;
begin
  i := IndexOf(Name);
  if i <> -1 then
    Result := TocvImageOperationClass(Objects[i])
  else
    Result := Nil;
end;

function TRegisteredImageOperations.GetNameByClass(const IOClass: TClass): String;
Var
  i: Integer;
begin
  Result := '';
  for i := 0 to Count - 1 do
    if Integer(Objects[i]) = Integer(IOClass) then
    begin
      Result := Self[i];
      Break;
    end;
end;

procedure TRegisteredImageOperations.RegisterIOClass(const IOClass: TClass; const ClassName: String);
begin
  AddObject(ClassName, TObject(IOClass));
  RegisterClass(TPersistentClass(IOClass));
end;

{TovcCustomErodeDilate}

procedure TovcCustomErodeDilateOperation.AssignTo(Dest: TPersistent);
begin
  if Dest is TovcCustomErodeDilateOperation then
  begin
    FRadius := (Dest as TovcCustomErodeDilateOperation).Radius;
    FIterations := (Dest as TovcCustomErodeDilateOperation).Iterations;
    FMorphOp := (Dest as TovcCustomErodeDilateOperation).MorphOp;
  end
  else
    inherited;
end;

constructor TovcCustomErodeDilateOperation.Create(AOwner: TComponent);
begin
  inherited;
  FRadius := 5;
  FIterations := 5;
end;

procedure TovcCustomErodeDilateOperation.SetIterations(const Value: Integer);
begin
  if LockTransform then
    try
      FIterations := Value;
    finally
      UnlockTransform;
    end;
end;

procedure TovcCustomErodeDilateOperation.SetMorphOp(const Value: TocvErodeDilateMode);
begin
  if LockTransform then
    try
      FMorphOp := Value;
    finally
      UnlockTransform;
    end;
end;

procedure TovcCustomErodeDilateOperation.SetRadius(const Value: Integer);
begin
  if LockTransform then
    try
      FRadius := Value;
    finally
      UnlockTransform;
    end;
end;

const
  EDMorpgOp: array [TocvErodeDilateMode] of Integer = (CV_SHAPE_RECT, CV_SHAPE_CROSS, CV_SHAPE_ELLIPSE, CV_SHAPE_CUSTOM);

  {TovcErode}

function TovcErodeOperation.Transform(const Source: pIplImage; var Destanation: pIplImage): Boolean;
Var
  Kern: pIplConvKernel;
begin
  Result := false;
  if LockTransform then
    try
      NotifyGetParams;
      Destanation := cvCloneImage(Source);
      Kern := cvCreateStructuringElementEx(Radius * 2 + 1, Radius * 2 + 1, Radius, Radius, EDMorpgOp[FMorphOp]);
      cvErode(Source, Destanation, Kern, Iterations);
      cvReleaseStructuringElement(Kern);
      Result := true;
    finally
      UnlockTransform;
    end;
end;

{TovcDilate}

function TovcDilateOperation.Transform(const Source: pIplImage; var Destanation: pIplImage): Boolean;
Var
  Kern: pIplConvKernel;
begin
  Result := false;
  if LockTransform then
    try
      NotifyGetParams;
      Destanation := cvCloneImage(Source);
      Kern := cvCreateStructuringElementEx(Radius * 2 + 1, Radius * 2 + 1, Radius, Radius, EDMorpgOp[FMorphOp]);
      cvDilate(Source, Destanation, Kern, Iterations);
      cvReleaseStructuringElement(Kern);
      Result := true;
    finally
      UnlockTransform;
    end;
end;

{TocvLaplace}

procedure TocvLaplaceOperation.AssignTo(Dest: TPersistent);
begin
  if Dest is TocvLaplaceOperation then
  begin
    FAperture := (Dest as TocvLaplaceOperation).Aperture;
  end
  else
    inherited;
end;

constructor TocvLaplaceOperation.Create(AOwner: TComponent);
begin
  inherited;
  FAperture := 3;
end;

procedure TocvLaplaceOperation.SetAperture(const Value: Integer);
begin
  if LockTransform then
    try
      FAperture := Value;
    finally
      UnlockTransform;
    end;
end;

function TocvLaplaceOperation.Transform(const Source: pIplImage; var Destanation: pIplImage): Boolean;
Var
  D: pIplImage;
begin
  Result := false;
  if LockTransform then
    try
      NotifyGetParams;
      D := cvCreateImage(cvGetSize(Source), IPL_DEPTH_16S, Source^.nChannels);
      Destanation := cvCreateImage(cvGetSize(Source), Source^.depth, Source^.nChannels);
      cvLaplace(Source, D, Aperture);
      cvConvertScale(D, Destanation);
      cvReleaseImage(D);
      Result := true;
    finally
      UnlockTransform;
    end;
end;

{TovcSobel}

procedure TovcSobelOperation.AssignTo(Dest: TPersistent);
begin
  if Dest is TovcSobelOperation then
  begin
    FXOrder := (Dest as TovcSobelOperation).XOrder;
    FYOrder := (Dest as TovcSobelOperation).YOrder;
    FAperture := (Dest as TovcSobelOperation).Aperture;
  end
  else
    inherited;
end;

constructor TovcSobelOperation.Create(AOwner: TComponent);
begin
  inherited;
  FXOrder := 1;
  FYOrder := 1;
  FAperture := 3;
end;

procedure TovcSobelOperation.SetAperture(const Value: Integer);
begin
  if LockTransform then
    try
      FAperture := Value;
    finally
      UnlockTransform;
    end;
end;

procedure TovcSobelOperation.SetXOrder(const Value: Integer);
begin
  if LockTransform then
    try
      if (YOrder <> 0) and (Value <> 0) then
        FXOrder := Value;
    finally
      UnlockTransform;
    end;
end;

procedure TovcSobelOperation.SetYOrder(const Value: Integer);
begin
  if LockTransform then
    try
      if (XOrder <> 0) and (Value <> 0) then
        FYOrder := Value;
    finally
      UnlockTransform;
    end;
end;

function TovcSobelOperation.Transform(const Source: pIplImage; var Destanation: pIplImage): Boolean;
Var
  D: pIplImage;
begin
  Result := false;
  if LockTransform then
    try
      NotifyGetParams;
      D := cvCreateImage(cvGetSize(Source), IPL_DEPTH_16S, Source^.nChannels);
      Destanation := cvCreateImage(cvGetSize(Source), Source^.depth, Source^.nChannels);
      cvSobel(Source, D, XOrder, YOrder, Aperture);
      cvConvertScale(D, Destanation);
      cvReleaseImage(D);
      Result := true;
    finally
      UnlockTransform;
    end;
end;

{TocvImageOperationCollectionItem}

procedure TocvImageOperationCollectionItem.Assign(Source: TPersistent);
begin
  if Source is TocvImageOperationCollectionItem then
    Operation.Assign(TocvImageOperationCollectionItem(Source).Operation)
  else
    inherited;
end;

constructor TocvImageOperationCollectionItem.Create(Collection: TCollection);
begin
  inherited;
  CS := TCriticalSection.Create;
end;

procedure TocvImageOperationCollectionItem.CreateProperties;
begin
  if FOperationClass <> nil then
    FOperation := FOperationClass.Create(nil);
end;

destructor TocvImageOperationCollectionItem.Destroy;
begin
  if Assigned(FOperation) then
    FOperation.Free;
  CS.Free;
  inherited;
end;

procedure TocvImageOperationCollectionItem.DestroyProperties;
begin
  FreeAndNil(FOperation);
end;

function TocvImageOperationCollectionItem.GetDisplayName: string;
begin
  Result := GetRegisteredImageOperations.GetNameByClass(FOperation.ClassType);
end;

function TocvImageOperationCollectionItem.GetProperties: TocvCustomImageOperation;
begin
  if not Assigned(FOperation) then
    FOperation := TocvNoneOperation.Create(nil);
  Result := FOperation;
end;

function TocvImageOperationCollectionItem.GetPropertiesClass: TocvImageOperationClass;
begin
  Result := TocvImageOperationClass(Operation.ClassType);
end;

function TocvImageOperationCollectionItem.GetPropertiesClassName: string;
begin
  Result := Operation.ClassName;
end;

function TocvImageOperationCollectionItem.LockTransform: Boolean;
begin
  Result := CS.TryEnter;
end;

function TocvImageOperationCollectionItem.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
end;

procedure TocvImageOperationCollectionItem.RecreateProperties;
begin
  DestroyProperties;
  CreateProperties;
end;

procedure TocvImageOperationCollectionItem.SetProperties(const Value: TocvCustomImageOperation);
begin
  if (FOperation <> nil) and (Value <> nil) then
    FOperation.Assign(Value);
end;

procedure TocvImageOperationCollectionItem.SetPropertiesClass(Value: TocvImageOperationClass);
begin
  if FOperationClass <> Value then
  begin
    FOperationClass := Value;
    RecreateProperties;
  end;
end;

procedure TocvImageOperationCollectionItem.SetPropertiesClassName(const Value: string);
begin
  OperationClass := TocvImageOperationClass(GetRegisteredImageOperations.FindByClassName(Value));
end;

function TocvImageOperationCollectionItem.Transform(const Source: pIplImage; var Destanation: pIplImage): Boolean;
begin
  if LockTransform then
    try
      Result := Assigned(FOperation) and FOperation.Transform(Source, Destanation)
    finally
      UnlockTransform;
    end;
end;

procedure TocvImageOperationCollectionItem.UnlockTransform;
begin
  CS.Leave;
end;

function TocvImageOperationCollectionItem._AddRef: Integer;
begin
  Result := -1;
end;

function TocvImageOperationCollectionItem._Release: Integer;
begin
  Result := -1;
end;

{TocvThresholdOperation}

Const
  cThreshold: array [TocvThresholdType] of Integer = (CV_THRESH_BINARY, CV_THRESH_BINARY_INV, CV_THRESH_TRUNC, CV_THRESH_TOZERO,
    CV_THRESH_TOZERO_INV, CV_THRESH_MASK, CV_THRESH_OTSU);

constructor TocvThresholdOperation.Create(AOwner: TComponent);
begin
  inherited;
  Threshold := 50;
end;

constructor TocvCustomThresholdOperation.Create(AOwner: TComponent);
begin
  inherited;
  MaxValue := 250;
  ThresholdType := THRESH_BINARY;
end;

function TocvCustomThresholdOperation.GetThresholdType: TocvThresholdType;
begin
  Result := TocvThresholdType(GetIntParam(0));
end;

procedure TocvCustomThresholdOperation.SetThresholdType(const Value: TocvThresholdType);
begin
  SetIntParam(0, Integer(Value));
end;

function TocvThresholdOperation.Transform(const Source: pIplImage; var Destanation: pIplImage): Boolean;
Var
  D: pIplImage;
begin
  Result := false;
  if LockTransform then
    try
      NotifyGetParams;
      D := cvCreateImage(cvGetSize(Source), IPL_DEPTH_8U, 1);
      cvCvtColor(Source, D, CV_RGB2GRAY);
      Destanation := cvCreateImage(cvGetSize(Source), IPL_DEPTH_8U, 1);
      cvThreshold(D, Destanation, Threshold, MaxValue, cThreshold[ThresholdType]);
      cvReleaseImage(D);
      Result := true;
    finally
      UnlockTransform;
    end;
end;

{TocvAdaptiveThresholdOperation}

const
  cAdaptiveThresholdType: array [TocvAdaptiveThresholdType] of Integer = (CV_ADAPTIVE_THRESH_MEAN_C,
    CV_ADAPTIVE_THRESH_GAUSSIAN_C);

constructor TocvAdaptiveThresholdOperation.Create(AOwner: TComponent);
begin
  inherited;
  AdaptiveThresholdType := ADAPTIVE_THRESH_MEAN_C;
  BlockSize := 3;
  Param := 5;
end;

function TocvAdaptiveThresholdOperation.GetAdaptiveThresholdType: TocvAdaptiveThresholdType;
begin
  Result := TocvAdaptiveThresholdType(GetIntParam(1));
end;

procedure TocvAdaptiveThresholdOperation.SetAdaptiveThresholdType(const Value: TocvAdaptiveThresholdType);
begin
  SetIntParam(1, Integer(Value));
end;

function TocvAdaptiveThresholdOperation.Transform(const Source: pIplImage; var Destanation: pIplImage): Boolean;
Var
  D: pIplImage;
begin
  Result := false;
  if LockTransform then
    try
      NotifyGetParams;
      D := cvCreateImage(cvGetSize(Source), IPL_DEPTH_8U, 1);
      cvCvtColor(Source, D, CV_RGB2GRAY);
      Destanation := cvCreateImage(cvGetSize(Source), IPL_DEPTH_8U, 1);
      cvAdaptiveThreshold(D, Destanation, MaxValue, cAdaptiveThresholdType[AdaptiveThresholdType], cThreshold[ThresholdType],
        BlockSize, Param);
      cvReleaseImage(D);
      Result := true;
    finally
      UnlockTransform;
    end;
end;

{TocvContoursOperation}

constructor TocvContoursOperation.Create(AOwner: TComponent);
begin
  inherited;
  CS := TCriticalSection.Create;
  FOffset := TocvPoint.Create;
  FContourDraw := TocvContourDraw.Create(Self);
  OperationClass := TocvThresholdOperation;
  With Preprocessing as TocvThresholdOperation do
  begin
    Threshold := 128;
    MaxValue := 255;
    ThresholdType := THRESH_BINARY_INV;
  end;
  RetrievalMode := RETR_LIST;
  ApproximationMethod := CHAIN_APPROX_SIMPLE;
end;

procedure TocvContoursOperation.CreateProperties;
begin
  if FOperationClass <> nil then
    FOperation := FOperationClass.Create(nil);
end;

destructor TocvContoursOperation.Destroy;
begin
  if Assigned(FOperation) then
    FOperation.Free;
  CS.Free;
  FOffset.Free;
  FContourDraw.Free;
  inherited;
end;

procedure TocvContoursOperation.DestroyProperties;
begin
  FreeAndNil(FOperation);
end;

function TocvContoursOperation.GetProperties: TocvCustomImageOperation;
begin
  if not Assigned(FOperation) then
    FOperation := TocvNoneOperation.Create(nil);
  Result := FOperation;
end;

function TocvContoursOperation.GetPropertiesClass: TocvImageOperationClass;
begin
  Result := TocvImageOperationClass(Preprocessing.ClassType);
end;

function TocvContoursOperation.GetPropertiesClassName: string;
begin
  Result := Preprocessing.ClassName;
end;

function TocvContoursOperation.LockTransform: Boolean;
begin
  Result := CS.TryEnter;
end;

function TocvContoursOperation.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
end;

procedure TocvContoursOperation.RecreateProperties;
begin
  DestroyProperties;
  CreateProperties;
end;

procedure TocvContoursOperation.SetProperties(const Value: TocvCustomImageOperation);
begin
  if (FOperation <> nil) and (Value <> nil) then
    FOperation.Assign(Value);
end;

procedure TocvContoursOperation.SetPropertiesClass(Value: TocvImageOperationClass);
begin
  if FOperationClass <> Value then
  begin
    FOperationClass := Value;
    RecreateProperties;
  end;
end;

procedure TocvContoursOperation.SetPropertiesClassName(const Value: string);
begin
  OperationClass := TocvImageOperationClass(GetRegisteredImageOperations.FindByClassName(Value));
end;

function TocvContoursOperation.Transform(const Source: pIplImage; var Destanation: pIplImage): Boolean;

const
  cLineType:array[TocvLineType] of Integer= (CV_FILLED,8,CV_AA);

Var
  th_image: pIplImage;
  contours: pCvSeq;
  storage: pCvMemStorage;
  contoursCont: Integer;
  RGBColor: TColor;
  er, eg, eb: Byte;
  hr, hg, hb: Byte;
begin
  Result := false;
  if LockTransform then
    try
      Destanation := cvCloneImage(Source);

      storage := cvCreateMemStorage(0);
      contours := nil;
      th_image := nil;
      try
        if not Preprocessing.Transform(Source, th_image) then
          Exit;

        contoursCont := cvFindContours(th_image, storage, @contours, SizeOf(TCvContour), Integer(RetrievalMode),
          Integer(ApproximationMethod), cvPoint(Offset.X, Offset.Y));

        if contoursCont > 0 then
        begin
          if ContourDraw.DrawContours then
          begin
            RGBColor := ColorToRGB(ContourDraw.ExternalColor);
            er := GetRValue(RGBColor);
            eg := GetGValue(RGBColor);
            eb := GetBValue(RGBColor);
            RGBColor := ColorToRGB(ContourDraw.HoleColor);
            hr := GetRValue(RGBColor);
            hg := GetGValue(RGBColor);
            hb := GetBValue(RGBColor);
            cvDrawContours( Destanation, contours, CV_RGB(er, eg, eb), CV_RGB(hr, hg, hb),
                            ContourDraw.MaxLevel, ContourDraw.Thickness, cLineType[ContourDraw.LineType], cvPoint(ContourDraw.Offset.X,
                            ContourDraw.Offset.Y));
          end;
        end;
        Result := true;
      finally
        if Assigned(th_image) then
          cvReleaseImage(th_image);
        cvReleaseMemStorage(storage)
      end;

    finally
      UnlockTransform;
    end;
end;

procedure TocvContoursOperation.UnlockTransform;
begin
  CS.Leave;
end;

function TocvContoursOperation._AddRef: Integer;
begin
  Result := -1;
end;

function TocvContoursOperation._Release: Integer;
begin
  Result := -1;
end;

{TPersistentPoint}

procedure TocvPoint.AssignTo(Dest: TPersistent);
begin
  if Dest is TocvPoint then
  begin
    FPoint := (Dest as TocvPoint).FPoint;
  end
  else
    inherited;
end;

{TocvCountourDraw}

procedure TocvContourDraw.AssignTo(Dest: TPersistent);
begin
  if Dest is TocvContourDraw then
  begin
    FOffset.FPoint := (Dest as TocvContourDraw).FOffset.FPoint;
    FDrawContours := (Dest as TocvContourDraw).FDrawContours;
    FMaxLevel := (Dest as TocvContourDraw).FMaxLevel;
    FHoleColor := (Dest as TocvContourDraw).FHoleColor;
    FThickness := (Dest as TocvContourDraw).FThickness;
    FLineType := (Dest as TocvContourDraw).FLineType;
    FExternalColor := (Dest as TocvContourDraw).FExternalColor;
  end
  else
    inherited;
end;

constructor TocvContourDraw.Create(AOwner: TPersistent);
begin
  inherited Create;
  FOwner := AOwner;
  FOffset := TocvPoint.Create;
  FDrawContours := false;
  FMaxLevel := 2;
  FHoleColor := clRed;
  FThickness := 2;
  FLineType := LT_AA;
  FExternalColor := clGreen;
end;

destructor TocvContourDraw.Destroy;
begin
  FOffset.Free;
  inherited;
end;

function TocvContourDraw.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

initialization

GetRegisteredImageOperations.RegisterIOClass(TocvNoneOperation, 'None');
GetRegisteredImageOperations.RegisterIOClass(TocvGrayScaleOperation, 'GrayScale');
GetRegisteredImageOperations.RegisterIOClass(TovcCannyOperation, 'Canny');
GetRegisteredImageOperations.RegisterIOClass(TovcSmoothOperation, 'Smooth');
GetRegisteredImageOperations.RegisterIOClass(TovcErodeOperation, 'Erode');
GetRegisteredImageOperations.RegisterIOClass(TovcDilateOperation, 'Dilate');
GetRegisteredImageOperations.RegisterIOClass(TocvLaplaceOperation, 'Laplace');
GetRegisteredImageOperations.RegisterIOClass(TovcSobelOperation, 'Sobel');
GetRegisteredImageOperations.RegisterIOClass(TocvThresholdOperation, 'Threshold');
GetRegisteredImageOperations.RegisterIOClass(TocvAdaptiveThresholdOperation, 'AdaptiveThreshold');
GetRegisteredImageOperations.RegisterIOClass(TocvContoursOperation, 'Contours');

finalization

if Assigned(_RegisteredImageOperations) then
  FreeAndNil(_RegisteredImageOperations);

end.
