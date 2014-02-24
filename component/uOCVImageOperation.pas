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
  System.SysUtils,
  System.Classes,
  System.SyncObjs,
  uOCVTypes,
  core.types_c;

type

  TocvCustomImageOperation = class(TPersistent)
  private
    CS: TCriticalSection;
    FOwner: TComponent;
  protected
    procedure LockTransform;
    procedure UnlockTransform;
  public
    constructor Create(AOwner: TComponent); virtual;
    destructor Destroy; override;
    function Transform(const Source: pIplImage; var Destanation: pIplImage): Boolean; virtual; abstract;
  end;

  TocvImageOperationClass = class of TocvCustomImageOperation;

  TocvImageOperation_None = class(TocvCustomImageOperation)
  public
    function Transform(const Source: pIplImage; var Destanation: pIplImage): Boolean; override;
  end;

  TocvImageOperation_GrayScale = class(TocvCustomImageOperation)
  public
    function Transform(const Source: pIplImage; var Destanation: pIplImage): Boolean; override;
  end;

  TovcImageOperation_Canny = class(TocvCustomImageOperation)
  protected
    procedure AssignTo(Dest: TPersistent); override;
  private
    FThreshold2: double;
    FThreshold1: double;
    FApertureSize: Integer;
    procedure SetApertureSize(const Value: Integer);
    procedure SetThreshold1(const Value: double);
    procedure SetThreshold2(const Value: double);
  protected
  public
    constructor Create(AOwner: TComponent); override;
    function Transform(const Source: pIplImage; var Destanation: pIplImage): Boolean; override;
  published
    property Threshold1: double Read FThreshold1 write SetThreshold1;
    property Threshold2: double Read FThreshold2 write SetThreshold2;
    property ApertureSize: Integer Read FApertureSize write SetApertureSize default 100;
  end;

  TocvSmoothOperations = (BLUR_NO_SCALE, BLUR, GAUSSIAN, MEDIAN, BILATERAL);

  TovcImageOperation_Smooth = class(TocvCustomImageOperation)
  protected
    procedure AssignTo(Dest: TPersistent); override;
  private
    FSigma2: double;
    FSize2: Integer;
    FSigma1: double;
    FSize1: Integer;
    FSmoothOperation: TocvSmoothOperations;
    procedure SetSigma1(const Value: double);
    procedure Setsigma2(const Value: double);
    procedure SetSize1(const Value: Integer);
    procedure SetSize2(const Value: Integer);
    procedure SetSmoothOperation(const Value: TocvSmoothOperations);
  public
    constructor Create(AOwner: TComponent); override;
    function Transform(const Source: pIplImage; var Destanation: pIplImage): Boolean; override;
  published
    property sigma1: double read FSigma1 write SetSigma1;
    property sigma2: double read FSigma2 write Setsigma2;
    property size1: Integer read FSize1 write SetSize1 default 3;
    property size2: Integer read FSize2 write SetSize2 default 3;
    property SmoothOperation: TocvSmoothOperations read FSmoothOperation write SetSmoothOperation default GAUSSIAN;
  end;

  IocvEditorPropertiesContainer = interface
    ['{418F88DD-E35D-4425-BF24-E753E83D35D6}']
    function GetProperties: TocvCustomImageOperation;
    function GetPropertiesClass: TocvImageOperationClass;
    procedure SetPropertiesClass(Value: TocvImageOperationClass);
  end;

  TocvImageOperation = class(TocvDataSourceAndReceiver, IocvEditorPropertiesContainer)
  private
    CS: TCriticalSection;
    FProperties: TocvCustomImageOperation;
    FPropertiesClass: TocvImageOperationClass;
    procedure LockTransform;
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
    property PropertiesClass: TocvImageOperationClass read GetPropertiesClass write SetPropertiesClass;
  published
    property PropertiesClassName: string read GetPropertiesClassName write SetPropertiesClassName;
    property Properties: TocvCustomImageOperation read GetProperties write SetProperties;
  end;

  TRegisteredImageOperations = class(TStringList)
  public
    function FindByClassName(const ClassName: String): TocvImageOperationClass;
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

{ TocvImageOperation }

procedure TocvImageOperation.SetProperties(const Value: TocvCustomImageOperation);
begin
  if (FProperties <> nil) and (Value <> nil) then
    FProperties.Assign(Value);
end;

procedure TocvImageOperation.SetPropertiesClass(Value: TocvImageOperationClass);
begin
  if FPropertiesClass <> Value then
  begin
    FPropertiesClass := Value;
    RecreateProperties;
  end;
end;

procedure TocvImageOperation.CreateProperties;
begin
  if FPropertiesClass <> nil then
    FProperties := FPropertiesClass.Create(Self);
end;

procedure TocvImageOperation.DestroyProperties;
begin
  FreeAndNil(FProperties);
end;

procedure TocvImageOperation.RecreateProperties;
begin
  DestroyProperties;
  CreateProperties;
end;

procedure TocvImageOperation.SetPropertiesClassName(const Value: string);
begin
  PropertiesClass := TocvImageOperationClass(GetRegisteredImageOperations.FindByClassName(Value));
end;

constructor TocvImageOperation.Create(AOwner: TComponent);
begin
  inherited;
  CS := TCriticalSection.Create;
end;

destructor TocvImageOperation.Destroy;
begin
  LockTransform;
  if Assigned(FProperties) then
    FreeAndNil(FProperties);
  CS.Free;
  inherited;
end;

function TocvImageOperation.GetProperties: TocvCustomImageOperation;
begin
  if not Assigned(FProperties) then
    FProperties := TocvImageOperation_None.Create(Self);
  Result := FProperties;
end;

function TocvImageOperation.GetPropertiesClass: TocvImageOperationClass;
begin
  Result := TocvImageOperationClass(Properties.ClassType);
end;

function TocvImageOperation.GetPropertiesClassName: string;
begin
  Result := Properties.ClassName;
end;

procedure TocvImageOperation.LockTransform;
begin
  CS.Enter;
end;

procedure TocvImageOperation.TakeImage(const IplImage: pIplImage);
var
  Destanation: pIplImage;
begin
  if Assigned(FProperties) and FProperties.Transform(IplImage, Destanation) then
  begin
    LockTransform;
    try
      NotifyReceiver(Destanation);
    finally
      UnlockTransform;
    end;
  end;
end;

procedure TocvImageOperation.UnlockTransform;
begin
  CS.Leave;
end;

{ TovcImageOperationCanny }

procedure TovcImageOperation_Canny.AssignTo(Dest: TPersistent);
begin
  if Dest is TovcImageOperation_Canny then
  begin
    FThreshold1 := (Dest as TovcImageOperation_Canny).FThreshold1;
    FThreshold2 := (Dest as TovcImageOperation_Canny).FThreshold2;
    FApertureSize := (Dest as TovcImageOperation_Canny).FApertureSize;
  end
  else
    inherited;
end;

constructor TovcImageOperation_Canny.Create { (AOwner: TPersistent) };
begin
  inherited;
  FThreshold1 := 10;
  FThreshold2 := 100;
  FApertureSize := 3;
end;

procedure TovcImageOperation_Canny.SetApertureSize(const Value: Integer);
begin
  LockTransform;
  try
    FApertureSize := Value;
  finally
    UnlockTransform;
  end;
end;

procedure TovcImageOperation_Canny.SetThreshold1(const Value: double);
begin
  LockTransform;
  try
    FThreshold1 := Value;
  finally
    UnlockTransform;
  end;
end;

procedure TovcImageOperation_Canny.SetThreshold2(const Value: double);
begin
  LockTransform;
  try
    FThreshold2 := Value;
  finally
    UnlockTransform;
  end;
end;

function TovcImageOperation_Canny.Transform(const Source: pIplImage; var Destanation: pIplImage): Boolean;
Var
  gray: pIplImage;
begin
  LockTransform;
  try
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

{ TocvImageOperationGrayScale }

function TocvImageOperation_GrayScale.Transform(const Source: pIplImage; var Destanation: pIplImage): Boolean;
begin
  Destanation := cvCreateImage(cvGetSize(Source), IPL_DEPTH_8U, 1);
  // преобразуем в градации cерого
  cvCvtColor(Source, Destanation, CV_RGB2GRAY);
  Result := true;
end;

{ TocvImageOperationNone }

function TocvImageOperation_None.Transform(const Source: pIplImage; var Destanation: pIplImage): Boolean;
begin
  Destanation := cvCloneImage(Source);
  Result := true;
end;

{ TCustomOpenCVImgOperation }

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

procedure TocvCustomImageOperation.LockTransform;
begin
  CS.Enter;
end;

procedure TocvCustomImageOperation.UnlockTransform;
begin
  CS.Leave;
end;

{ TovcImageOperationSmooth }
Const
  ocvSmoothOperations: array [TocvSmoothOperations] of Integer = (CV_BLUR_NO_SCALE, CV_BLUR, CV_GAUSSIAN, CV_MEDIAN, CV_BILATERAL);

procedure TovcImageOperation_Smooth.AssignTo(Dest: TPersistent);
begin
  if Dest is TovcImageOperation_Smooth then
  begin
    FSmoothOperation := (Dest as TovcImageOperation_Smooth).FSmoothOperation;
    FSize1 := (Dest as TovcImageOperation_Smooth).FSize1;
    FSize2 := (Dest as TovcImageOperation_Smooth).FSize2;
    FSigma1 := (Dest as TovcImageOperation_Smooth).FSigma1;
    FSigma2 := (Dest as TovcImageOperation_Smooth).FSigma2;
  end
  else
    inherited;
end;

constructor TovcImageOperation_Smooth.Create { (AOwner: TPersistent) };
begin
  inherited;
  FSmoothOperation := GAUSSIAN;
  FSize1 := 3;
  FSize2 := 3;
  FSigma1 := 0;
  FSigma2 := 0;
end;

procedure TovcImageOperation_Smooth.SetSigma1(const Value: double);
begin
  LockTransform;
  try
    FSigma1 := Value;
  finally
    UnlockTransform;
  end;
end;

//
procedure TovcImageOperation_Smooth.Setsigma2(const Value: double);
begin
  LockTransform;
  try
    FSigma2 := Value;
  finally
    UnlockTransform;
  end;
end;

//
procedure TovcImageOperation_Smooth.SetSize1(const Value: Integer);
begin
  LockTransform;
  try
    FSize1 := Value;
  finally
    UnlockTransform;
  end;
end;

//
procedure TovcImageOperation_Smooth.SetSize2(const Value: Integer);
begin
  LockTransform;
  try
    FSize2 := Value;
  finally
    UnlockTransform;
  end;
end;

procedure TovcImageOperation_Smooth.SetSmoothOperation(const Value: TocvSmoothOperations);
begin
  LockTransform;
  try
    FSmoothOperation := Value;
  finally
    UnlockTransform;
  end;
end;

function TovcImageOperation_Smooth.Transform(const Source: pIplImage; var Destanation: pIplImage): Boolean;
begin
  LockTransform;
  try
    Destanation := cvCloneImage(Source);
    cvSmooth(Source, Destanation, ocvSmoothOperations[SmoothOperation], size1, size2, sigma1, sigma2);
    Result := true;
  finally
    UnlockTransform;
  end;
end;

{ TRegisteredImageOperations }

function TRegisteredImageOperations.FindByClassName(const ClassName: String): TocvImageOperationClass;
Var
  i: Integer;
begin
  i := IndexOf(ClassName);
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

initialization

GetRegisteredImageOperations.RegisterIOClass(TocvImageOperation_None, 'None');
GetRegisteredImageOperations.RegisterIOClass(TocvImageOperation_GrayScale, 'GrayScale');
GetRegisteredImageOperations.RegisterIOClass(TovcImageOperation_Canny, 'Canny');
GetRegisteredImageOperations.RegisterIOClass(TovcImageOperation_Smooth, 'Smooth');

finalization

if Assigned(_RegisteredImageOperations) then
  FreeAndNil(_RegisteredImageOperations);

end.
