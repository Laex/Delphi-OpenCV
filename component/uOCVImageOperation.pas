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
  core.types_c,
  Vcl.Graphics;

type

  TocvCustomImageOperation = class(TPersistent)
  protected
    procedure AssignTo(Dest: TPersistent); override;
  private
    CS: TCriticalSection;
    FOwner: TPersistent; // TComponent;
    FFloatParams: TArray<Double>;
    FIntParams: TArray<Integer>;
    FBoolParams: TArray<Boolean>;
    function GetFloatParam(const index: Integer): Double;
    function GetIntParam(const index: Integer): Integer;
    procedure SetFloatParam(const index: Integer; const Value: Double);
    procedure SetIntParam(const index: Integer; const Value: Integer);
    function GetBoolParam(const index: Integer): Boolean;
    procedure SetBoolParam(const index: Integer; const Value: Boolean);
  protected
    function LockTransform: Boolean;
    procedure UnlockTransform;
    function DoTransform(const Source: IocvImage; var Destanation: IocvImage): Boolean; virtual;
    property FloatParams[const index: Integer]: Double Read GetFloatParam write SetFloatParam;
    property IntParams[const index: Integer]: Integer Read GetIntParam write SetIntParam;
    property BoolParams[const index: Integer]: Boolean Read GetBoolParam write SetBoolParam;
  public
    constructor Create(AOwner: TPersistent); virtual;
    destructor Destroy; override;
    function Transform(const Source: IocvImage; var Destanation: IocvImage): Boolean; virtual;
  end;

  TocvImageOperationClass = class of TocvCustomImageOperation;

  TocvNoneOperation = class(TocvCustomImageOperation)
  public
    function DoTransform(const Source: IocvImage; var Destanation: IocvImage): Boolean; override;
  end;

  TocvGrayScaleOperation = class(TocvCustomImageOperation)
  public
    function DoTransform(const Source: IocvImage; var Destanation: IocvImage): Boolean; override;
  end;

  TovcCannyOperation = class(TocvCustomImageOperation)
  public
    constructor Create(AOwner: TPersistent); override;
    function DoTransform(const Source: IocvImage; var Destanation: IocvImage): Boolean; override;
  published
    property Threshold1: Double index 0 Read GetFloatParam write SetFloatParam;
    property Threshold2: Double index 1 Read GetFloatParam write SetFloatParam;
    property ApertureSize: Integer index 0 Read GetIntParam write SetIntParam;
  end;

  TocvErodeDilateMode = (SHAPE_RECT, SHAPE_CROSS, SHAPE_ELLIPSE, SHAPE_CUSTOM);

  TovcCustomErodeDilateOperation = class(TocvCustomImageOperation)
  protected
    procedure AssignTo(Dest: TPersistent); override;
  private
    FMorphOp: TocvErodeDilateMode;
    procedure SetMorphOp(const Value: TocvErodeDilateMode);
  protected
  public
    constructor Create(AOwner: TPersistent); override;
  published
    property Radius: Integer index 0 Read GetIntParam write SetIntParam;
    property Iterations: Integer index 1 Read GetIntParam write SetIntParam;
    property MorphOp: TocvErodeDilateMode read FMorphOp write SetMorphOp default SHAPE_RECT;
  end;

  TovcErodeOperation = class(TovcCustomErodeDilateOperation)
  public
    function DoTransform(const Source: IocvImage; var Destanation: IocvImage): Boolean; override;
  end;

  TovcDilateOperation = class(TovcCustomErodeDilateOperation)
  public
    function DoTransform(const Source: IocvImage; var Destanation: IocvImage): Boolean; override;
  end;

  TocvLaplaceOperation = class(TocvCustomImageOperation)
  public
    constructor Create(AOwner: TPersistent); override;
    function DoTransform(const Source: IocvImage; var Destanation: IocvImage): Boolean; override;
  published
    property Aperture: Integer index 0 Read GetIntParam write SetIntParam;
  end;

  TovcSobelOperation = class(TocvCustomImageOperation)
  public
    constructor Create(AOwner: TPersistent); override;
    function DoTransform(const Source: IocvImage; var Destanation: IocvImage): Boolean; override;
  published
    property XOrder: Integer index 0 Read GetIntParam write SetIntParam;
    property YOrder: Integer index 1 Read GetIntParam write SetIntParam;
    property Aperture: Integer index 2 Read GetIntParam write SetIntParam;
  end;

  TocvSmoothOperations = (BLUR_NO_SCALE, BLUR, GAUSSIAN, MEDIAN, BILATERAL);

  TovcSmoothOperation = class(TocvCustomImageOperation)
  protected
    procedure AssignTo(Dest: TPersistent); override;
  private
    FSmoothOperation: TocvSmoothOperations;
    procedure SetSmoothOperation(const Value: TocvSmoothOperations);
  public
    constructor Create(AOwner: TPersistent); override;
    function DoTransform(const Source: IocvImage; var Destanation: IocvImage): Boolean; override;
  published
    property sigma1: Double index 0 Read GetFloatParam write SetFloatParam;
    property sigma2: Double index 1 Read GetFloatParam write SetFloatParam;
    property size1: Integer index 0 Read GetIntParam write SetIntParam;
    property size2: Integer index 1 Read GetIntParam write SetIntParam;
    property SmoothOperation: TocvSmoothOperations read FSmoothOperation write SetSmoothOperation default GAUSSIAN;
  end;

  TocvThresholdType = (THRESH_BINARY, THRESH_BINARY_INV, THRESH_TRUNC, THRESH_TOZERO, THRESH_TOZERO_INV, THRESH_MASK,
    THRESH_OTSU);

  TocvCustomThresholdOperation = class(TocvCustomImageOperation)
  private
    function GetThresholdType: TocvThresholdType;
    procedure SetThresholdType(const Value: TocvThresholdType);
  public
    constructor Create(AOwner: TPersistent); override;
  published
    property MaxValue: Double index 0 Read GetFloatParam write SetFloatParam; // default 250;
    property ThresholdType: TocvThresholdType read GetThresholdType write SetThresholdType default THRESH_BINARY; // index 0
  end;

  TocvThresholdOperation = class(TocvCustomThresholdOperation)
  public
    constructor Create(AOwner: TPersistent); override;
    function DoTransform(const Source: IocvImage; var Destanation: IocvImage): Boolean; override;
  published
    property Threshold: Double index 1 Read GetFloatParam write SetFloatParam; // default 50;
  end;

  TocvAdaptiveThresholdType = (ADAPTIVE_THRESH_MEAN_C, ADAPTIVE_THRESH_GAUSSIAN_C);

  TocvAdaptiveThresholdOperation = class(TocvCustomThresholdOperation)
  private
    function GetAdaptiveThresholdType: TocvAdaptiveThresholdType;
    procedure SetAdaptiveThresholdType(const Value: TocvAdaptiveThresholdType);
  public
    constructor Create(AOwner: TPersistent); override;
    function DoTransform(const Source: IocvImage; var Destanation: IocvImage): Boolean; override;
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
    property Enabled: Boolean read FDrawContours write FDrawContours default True;
    property ExternalColor: TColor read FExternalColor write FExternalColor default clGreen;
    property HoleColor: TColor read FHoleColor write FHoleColor default clRed;
    property MaxLevel: Integer read FMaxLevel write FMaxLevel default 2;
    property Thickness: Integer read FThickness write FThickness default 2;
    property LineType: TocvLineType read FLineType write FLineType default LT_AA;
    property Offset: TocvPoint read FOffset write FOffset;
  end;

  TocvContourApprox = class(TPersistent)
  protected
    procedure AssignTo(Dest: TPersistent); override;
    function GetOwner: TPersistent; override;
  private
    FOwner: TPersistent;
    FEnabled: Boolean;
    FRecursive: Boolean;
    FEps: Double;
  public
    constructor Create(AOwner: TPersistent);
  published
    property Enabled: Boolean read FEnabled write FEnabled default True;
    property Eps: Double read FEps write FEps;
    property Recursive: Boolean read FRecursive write FRecursive default True;
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
    FApprox: TocvContourApprox;
    function LockTransform: Boolean;
    procedure UnlockTransform;
    procedure CreateProperties;
    procedure DestroyProperties;
    procedure RecreateProperties;
    function GetPropertiesClassName: string;
    procedure SetProperties(const Value: TocvCustomImageOperation);
    procedure SetPropertiesClass(Value: TocvImageOperationClass);
    procedure SetPropertiesClassName(const Value: string);
    procedure DoNotifyContours(const Image: IocvImage; const ContourCount: Integer; const Contours: pCvSeq);
  protected
    function GetProperties: TocvCustomImageOperation;
    function GetPropertiesClass: TocvImageOperationClass;
    {IInterface}
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  public
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;
    function DoTransform(const Source: IocvImage; var Destanation: IocvImage): Boolean; override;
    property OperationClass: TocvImageOperationClass read GetPropertiesClass write SetPropertiesClass;
  published
    property OperationClassName: string read GetPropertiesClassName write SetPropertiesClassName;
    property Preprocessing: TocvCustomImageOperation read GetProperties write SetProperties;
    property RetrievalMode: TocvContourRetrievalModes read FRetrievalMode write FRetrievalMode default RETR_LIST;
    property ApproximationMethod: TocvContourApproximationMethods read FApproximationMethod write FApproximationMethod
      default CHAIN_APPROX_SIMPLE;
    property Offset: TocvPoint read FOffset write FOffset;
    property ContourDraw: TocvContourDraw read FContourDraw write FContourDraw;
    property ApproxPoly: TocvContourApprox read FApprox write FApprox;
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
    function DoTransform(const Source: IocvImage; var Destanation: IocvImage): Boolean;
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
    FOnBeforeTransorm: TOnOcvNotify;
    FOnAfterTransorm: TOnOcvNotify;
    FOnContour: TOnOcvContour;
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
    procedure TakeImage(const IplImage: IocvImage); override;
    function GetProperties: TocvCustomImageOperation;
    function GetPropertiesClass: TocvImageOperationClass;
    procedure DoNotifyAfterTransform(Sender: TObject; const IplImage: IocvImage);
    procedure DoNotifyBeforeTransform(Sender: TObject; const IplImage: IocvImage);
    procedure DoNotifyContour(Sender: TObject; const IplImage: IocvImage; const ContourCount: Integer; const Contours: pCvSeq);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property OperationClass: TocvImageOperationClass read GetPropertiesClass write SetPropertiesClass;
  published
    property OperationClassName: string read GetPropertiesClassName write SetPropertiesClassName;
    property Operation: TocvCustomImageOperation read GetProperties write SetProperties;
    property Operations: TocvImageOperationCollection Read FOperations write FOperations;
    property OnBeforeTransorm: TOnOcvNotify read FOnBeforeTransorm write FOnBeforeTransorm;
    property OnAfterTransorm: TOnOcvNotify read FOnAfterTransorm write FOnAfterTransorm;
    property OnContour: TOnOcvContour read FOnContour write FOnContour;
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

procedure TocvImageOperation.TakeImage(const IplImage: IocvImage);
var
  Destanation: IocvImage;
  i: Integer;
begin
  if LockTransform then
    try
      if FOperations.Count > 0 then
      begin
        Destanation := IplImage;
        for i := 0 to FOperations.Count - 1 do
        begin
          DoNotifyBeforeTransform(FOperations.Items[i], IplImage);
          if not(FOperations.Items[i] as TocvImageOperationCollectionItem).DoTransform(Destanation, Destanation) then
            Exit;
          DoNotifyAfterTransform(FOperation, Destanation);
        end;
        NotifyReceiver(Destanation);
      end
      else
      begin
        if Assigned(FOperation) then
        begin
          DoNotifyBeforeTransform(FOperation, IplImage);
          if FOperation.DoTransform(IplImage, Destanation) then
          begin
            DoNotifyAfterTransform(FOperation, Destanation);
            NotifyReceiver(Destanation);
          end;
        end;
      end;
    finally
      Destanation := nil;
      UnlockTransform;
    end;
end;

procedure TocvImageOperation.DoNotifyBeforeTransform(Sender: TObject; const IplImage: IocvImage);
begin
  if Assigned(OnBeforeTransorm) then
    OnBeforeTransorm(Sender, IplImage);
end;

procedure TocvImageOperation.DoNotifyContour(Sender: TObject; const IplImage: IocvImage; const ContourCount: Integer;
  const Contours: pCvSeq);
begin
  if Assigned(OnContour) then
    OnContour(Sender, IplImage, ContourCount, Contours);
end;

procedure TocvImageOperation.DoNotifyAfterTransform(Sender: TObject; const IplImage: IocvImage);
begin
  if Assigned(OnAfterTransorm) then
    OnAfterTransorm(Sender, IplImage);
end;

procedure TocvImageOperation.UnlockTransform;
begin
  CS.Leave;
end;

{TovcImageOperationCanny}

constructor TovcCannyOperation.Create {(AOwner: TPersistent)};
begin
  inherited;
  Threshold1 := 10;
  Threshold2 := 100;
  ApertureSize := 3;
end;

function TovcCannyOperation.DoTransform(const Source: IocvImage; var Destanation: IocvImage): Boolean;
begin
  Destanation := TocvImage.Create(cvCreateImage(cvGetSize(Source.IpImage), IPL_DEPTH_8U, 1));
  cvCanny(Source.GrayImage.IpImage, Destanation.IpImage, Threshold1, Threshold2, ApertureSize);
  Result := True;
end;

{TocvImageOperationGrayScale}

function TocvGrayScaleOperation.DoTransform(const Source: IocvImage; var Destanation: IocvImage): Boolean;
begin
  Destanation := Source.GrayImage;
  Result := True;
end;

{TocvImageOperationNone}

function TocvNoneOperation.DoTransform(const Source: IocvImage; var Destanation: IocvImage): Boolean;
begin
  Destanation := Source;
  Result := True;
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

constructor TocvCustomImageOperation.Create(AOwner: TPersistent);
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

function TocvCustomImageOperation.DoTransform(const Source: IocvImage; var Destanation: IocvImage): Boolean;
begin

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

function TocvCustomImageOperation.Transform(const Source: IocvImage; var Destanation: IocvImage): Boolean;
begin
  Result := LockTransform;
  if Result then
    try
      Result := DoTransform(Source, Destanation);
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
  inherited;
  if Dest is TovcSmoothOperation then
  begin
    FSmoothOperation := (Dest as TovcSmoothOperation).FSmoothOperation;
  end
  else
    inherited;
end;

constructor TovcSmoothOperation.Create {(AOwner: TPersistent)};
begin
  inherited;
  FSmoothOperation := GAUSSIAN;
  size1 := 3;
  size2 := 3;
  sigma1 := 0;
  sigma2 := 0;
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

function TovcSmoothOperation.DoTransform(const Source: IocvImage; var Destanation: IocvImage): Boolean;
Var
  Image: pIplImage;
begin
  Image := cvCloneImage(Source.IpImage);
  cvSmooth(Source.IpImage, Image, ocvSmoothOperations[SmoothOperation], size1, size2, sigma1, sigma2);
  Destanation := TocvImage.Create(Image);
  Result := True;
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
  inherited;
  if Dest is TovcCustomErodeDilateOperation then
    FMorphOp := (Dest as TovcCustomErodeDilateOperation).MorphOp;
end;

constructor TovcCustomErodeDilateOperation.Create {(AOwner: TComponent)};
begin
  inherited;
  Radius := 5;
  Iterations := 5;
  FMorphOp := SHAPE_RECT;
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

const
  EDMorpgOp: array [TocvErodeDilateMode] of Integer = (CV_SHAPE_RECT, CV_SHAPE_CROSS, CV_SHAPE_ELLIPSE, CV_SHAPE_CUSTOM);

  {TovcErode}

function TovcErodeOperation.DoTransform(const Source: IocvImage; var Destanation: IocvImage): Boolean;
Var
  Kern: pIplConvKernel;
begin
  Destanation := TocvImage.Create(cvCloneImage(Source.IpImage));
  Kern := cvCreateStructuringElementEx(Radius * 2 + 1, Radius * 2 + 1, Radius, Radius, EDMorpgOp[FMorphOp]);
  cvErode(Source.IpImage, Destanation.IpImage, Kern, Iterations);
  cvReleaseStructuringElement(Kern);
  Result := True;
end;

{TovcDilate}

function TovcDilateOperation.DoTransform(const Source: IocvImage; var Destanation: IocvImage): Boolean;
Var
  Kern: pIplConvKernel;
begin
  Destanation := TocvImage.Create(cvCloneImage(Source.IpImage));
  Kern := cvCreateStructuringElementEx(Radius * 2 + 1, Radius * 2 + 1, Radius, Radius, EDMorpgOp[FMorphOp]);
  cvDilate(Source.IpImage, Destanation.IpImage, Kern, Iterations);
  cvReleaseStructuringElement(Kern);
  Result := True;
end;

{TocvLaplace}

constructor TocvLaplaceOperation.Create {(AOwner: TComponent)};
begin
  inherited;
  Aperture := 3;
end;

function TocvLaplaceOperation.DoTransform(const Source: IocvImage; var Destanation: IocvImage): Boolean;
Var
  TempImg: pIplImage;
begin
  TempImg := cvCreateImage(cvGetSize(Source.IpImage), IPL_DEPTH_16S, Source.IpImage^.nChannels);
  Destanation := TocvImage.Create(cvCreateImage(cvGetSize(Source.IpImage), Source.IpImage^.depth, Source.IpImage^.nChannels));
  cvLaplace(Source.IpImage, TempImg, Aperture);
  cvConvertScale(TempImg, Destanation.IpImage);
  cvReleaseImage(TempImg);
  Result := True;
end;

{TovcSobel}

constructor TovcSobelOperation.Create {(AOwner: TComponent)};
begin
  inherited;
  XOrder := 1;
  YOrder := 1;
  Aperture := 3;
end;

function TovcSobelOperation.DoTransform(const Source: IocvImage; var Destanation: IocvImage): Boolean;
Var
  TmpImg: pIplImage;
begin
  TmpImg := cvCreateImage(cvGetSize(Source.IpImage), IPL_DEPTH_16S, Source.IpImage^.nChannels);
  Destanation := TocvImage.Create(cvCreateImage(cvGetSize(Source.IpImage), Source.IpImage^.depth, Source.IpImage^.nChannels));
  cvSobel(Source.IpImage, TmpImg, XOrder, YOrder, Aperture);
  cvConvertScale(TmpImg, Destanation.IpImage);
  cvReleaseImage(TmpImg);
  Result := True;
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
    FOperation := FOperationClass.Create(Self);
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
    FOperation := TocvNoneOperation.Create(Self);
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

function TocvImageOperationCollectionItem.DoTransform(const Source: IocvImage; var Destanation: IocvImage): Boolean;
begin
  if LockTransform then
    try
      Result := Assigned(FOperation) and FOperation.DoTransform(Source, Destanation)
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

constructor TocvThresholdOperation.Create {(AOwner: TComponent)};
begin
  inherited;
  Threshold := 50;
end;

constructor TocvCustomThresholdOperation.Create {(AOwner: TComponent)};
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

function TocvThresholdOperation.DoTransform(const Source: IocvImage; var Destanation: IocvImage): Boolean;
begin
  Destanation := TocvImage.Create(cvCreateImage(cvGetSize(Source.IpImage), IPL_DEPTH_8U, 1));
  cvThreshold(Source.GrayImage.IpImage, Destanation.IpImage, Threshold, MaxValue, cThreshold[ThresholdType]);
  Result := True;
end;

{TocvAdaptiveThresholdOperation}

const
  cAdaptiveThresholdType: array [TocvAdaptiveThresholdType] of Integer = (CV_ADAPTIVE_THRESH_MEAN_C,
    CV_ADAPTIVE_THRESH_GAUSSIAN_C);

constructor TocvAdaptiveThresholdOperation.Create {(AOwner: TComponent)};
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

function TocvAdaptiveThresholdOperation.DoTransform(const Source: IocvImage; var Destanation: IocvImage): Boolean;
begin
  Destanation := TocvImage.Create(cvCreateImage(cvGetSize(Source.IpImage), IPL_DEPTH_8U, 1));
  cvAdaptiveThreshold(Source.GrayImage.IpImage, Destanation.IpImage, MaxValue, cAdaptiveThresholdType[AdaptiveThresholdType],
    cThreshold[ThresholdType], BlockSize, Param);
  Result := True;
end;

{TocvContoursOperation}

constructor TocvContoursOperation.Create {(AOwner: TComponent)};
begin
  inherited;
  CS := TCriticalSection.Create;
  FOffset := TocvPoint.Create;
  FContourDraw := TocvContourDraw.Create(Self);
  FApprox := TocvContourApprox.Create(Self);
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
  FApprox.Free;
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

function TocvContoursOperation.DoTransform(const Source: IocvImage; var Destanation: IocvImage): Boolean;

const
  cLineType: array [TocvLineType] of Integer = (CV_FILLED, 8, CV_AA);

  procedure GetRGBValue(const AColor: TColor; var r, g, b: byte);
  Var
    RGBColor: TColor;
  begin
    RGBColor := ColorToRGB(AColor);
    r := GetRValue(RGBColor);
    g := GetGValue(RGBColor);
    b := GetBValue(RGBColor);
  end;

Var
  th_image: IocvImage;
  Contours: pCvSeq;
  storage: pCvMemStorage;
  contoursCont: Integer;
  RGBColor: TColor;
  er, eg, eb: byte;
  hr, hg, hb: byte;
begin
  Result := false;
  Contours := nil;
  th_image := nil;
  storage := cvCreateMemStorage(0);
  try
    Destanation := Source; // .Clone;
    if Preprocessing.Transform(Source, th_image) then
    begin
      contoursCont := cvFindContours(th_image.IpImage, storage, @Contours, SizeOf(TCvContour), Integer(RetrievalMode),
        Integer(ApproximationMethod), cvPoint(Offset.X, Offset.Y));
      if ApproxPoly.Enabled then
        Contours := cvApproxPoly(Contours, SizeOf(TCvContour), storage, CV_POLY_APPROX_DP, ApproxPoly.Eps,
          Integer(ApproxPoly.Recursive));
      DoNotifyContours(Destanation, contoursCont, Contours);
      if (contoursCont > 0) and ContourDraw.Enabled then
      begin
        GetRGBValue(ContourDraw.ExternalColor, er, eg, eb);
        GetRGBValue(ContourDraw.HoleColor, hr, hg, hb);
        cvDrawContours(Destanation.IpImage, Contours, CV_RGB(er, eg, eb), CV_RGB(hr, hg, hb), ContourDraw.MaxLevel,
          ContourDraw.Thickness, cLineType[ContourDraw.LineType], cvPoint(ContourDraw.Offset.X, ContourDraw.Offset.Y));
      end;
      Result := True;
    end;
  finally
    cvReleaseMemStorage(storage)
  end;
end;

procedure TocvContoursOperation.DoNotifyContours(const Image: IocvImage; const ContourCount: Integer; const Contours: pCvSeq);
Var
  NotifyTarget: TocvImageOperation;
begin
  if FOwner is TocvImageOperation then
    NotifyTarget := FOwner as TocvImageOperation
  else {}
    if
    {} (FOwner is TocvImageOperationCollectionItem) and
    {} ((FOwner as TocvImageOperationCollectionItem).GetOwner is TocvImageOperationCollection) and
    {} (((FOwner as TocvImageOperationCollectionItem).GetOwner as TocvImageOperationCollection).Owner is TocvImageOperation) then
      NotifyTarget := ((FOwner as TocvImageOperationCollectionItem).GetOwner as TocvImageOperationCollection)
        .Owner as TocvImageOperation
    else
      NotifyTarget := nil;
  if Assigned(NotifyTarget) then
    NotifyTarget.DoNotifyContour(Self, Image, ContourCount, Contours);
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
  FDrawContours := True;
  FMaxLevel := 2;
  FHoleColor := clRed;
  FThickness := 1;
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

{TocvContourApprox}

procedure TocvContourApprox.AssignTo(Dest: TPersistent);
begin
  if Dest is TocvContourApprox then
  begin
    FEnabled := (Dest as TocvContourApprox).Enabled;
    FEps := (Dest as TocvContourApprox).Eps;
    FRecursive := (Dest as TocvContourApprox).Recursive;
  end
  else
    inherited;
end;

constructor TocvContourApprox.Create(AOwner: TPersistent);
begin
  inherited Create;
  FOwner := AOwner;
  FEnabled := True;
  FEps := 3;
  FRecursive := True;
end;

function TocvContourApprox.GetOwner: TPersistent;
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
