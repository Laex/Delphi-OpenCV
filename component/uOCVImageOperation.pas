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
    function LockTransform: Boolean;
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

  TocvErodeDilateMode = (SHAPE_RECT, SHAPE_CROSS, SHAPE_ELLIPSE, SHAPE_CUSTOM);

  TovcCustomErodeDilate = class(TocvCustomImageOperation)
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

  TovcErode = class(TovcCustomErodeDilate)
  public
    function Transform(const Source: pIplImage; var Destanation: pIplImage): Boolean; override;
  end;

  TovcDilate = class(TovcCustomErodeDilate)
  public
    function Transform(const Source: pIplImage; var Destanation: pIplImage): Boolean; override;
  end;

  TocvLaplace = class(TocvCustomImageOperation)
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

  TovcSobel = class(TocvCustomImageOperation)
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
    // function GetOwner: TComponent;
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
    FOperation := TocvImageOperation_None.Create(Self);
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

constructor TovcImageOperation_Canny.Create {(AOwner: TPersistent)};
begin
  inherited;
  FThreshold1 := 10;
  FThreshold2 := 100;
  FApertureSize := 3;
end;

procedure TovcImageOperation_Canny.SetApertureSize(const Value: Integer);
begin
  if LockTransform then
    try
      FApertureSize := Value;
    finally
      UnlockTransform;
    end;
end;

procedure TovcImageOperation_Canny.SetThreshold1(const Value: double);
begin
  if LockTransform then
    try
      FThreshold1 := Value;
    finally
      UnlockTransform;
    end;
end;

procedure TovcImageOperation_Canny.SetThreshold2(const Value: double);
begin
  if LockTransform then
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
  Result := False;
  if LockTransform then
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

{TocvImageOperationGrayScale}

function TocvImageOperation_GrayScale.Transform(const Source: pIplImage; var Destanation: pIplImage): Boolean;
begin
  Result := False;
  if LockTransform then
    try
      Destanation := cvCreateImage(cvGetSize(Source), IPL_DEPTH_8U, 1);
      // преобразуем в градации cерого
      cvCvtColor(Source, Destanation, CV_RGB2GRAY);
      Result := true;
    finally
      UnlockTransform;
    end;
end;

{TocvImageOperationNone}

function TocvImageOperation_None.Transform(const Source: pIplImage; var Destanation: pIplImage): Boolean;
begin
  Result := False;
  if LockTransform then
    try
      Destanation := cvCloneImage(Source);
      Result := true;
    finally
      UnlockTransform;
    end;
end;

{TCustomOpenCVImgOperation}

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

function TocvCustomImageOperation.LockTransform: Boolean;
begin
  Result := CS.TryEnter;
end;

procedure TocvCustomImageOperation.UnlockTransform;
begin
  CS.Leave;
end;

{TovcImageOperationSmooth}
Const
  ocvSmoothOperations: array [TocvSmoothOperations] of Integer = (CV_BLUR_NO_SCALE, CV_BLUR, CV_GAUSSIAN, CV_MEDIAN,
    CV_BILATERAL);

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

constructor TovcImageOperation_Smooth.Create {(AOwner: TPersistent)};
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
  if LockTransform then
    try
      FSigma1 := Value;
    finally
      UnlockTransform;
    end;
end;

//
procedure TovcImageOperation_Smooth.Setsigma2(const Value: double);
begin
  if LockTransform then
    try
      FSigma2 := Value;
    finally
      UnlockTransform;
    end;
end;

//
procedure TovcImageOperation_Smooth.SetSize1(const Value: Integer);
begin
  if LockTransform then
    try
      FSize1 := Value;
    finally
      UnlockTransform;
    end;
end;

//
procedure TovcImageOperation_Smooth.SetSize2(const Value: Integer);
begin
  if LockTransform then
    try
      FSize2 := Value;
    finally
      UnlockTransform;
    end;
end;

procedure TovcImageOperation_Smooth.SetSmoothOperation(const Value: TocvSmoothOperations);
begin
  if LockTransform then
    try
      FSmoothOperation := Value;
    finally
      UnlockTransform;
    end;
end;

function TovcImageOperation_Smooth.Transform(const Source: pIplImage; var Destanation: pIplImage): Boolean;
begin
  Result := False;
  if LockTransform then
    try
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

procedure TovcCustomErodeDilate.AssignTo(Dest: TPersistent);
begin
  if Dest is TovcCustomErodeDilate then
  begin
    FRadius := (Dest as TovcCustomErodeDilate).Radius;
    FIterations := (Dest as TovcCustomErodeDilate).Iterations;
    FMorphOp := (Dest as TovcCustomErodeDilate).MorphOp;
  end
  else
    inherited;
end;

constructor TovcCustomErodeDilate.Create(AOwner: TComponent);
begin
  inherited;
  FRadius := 5;
  FIterations := 5;
end;

procedure TovcCustomErodeDilate.SetIterations(const Value: Integer);
begin
  if LockTransform then
    try
      FIterations := Value;
    finally
      UnlockTransform;
    end;
end;

procedure TovcCustomErodeDilate.SetMorphOp(const Value: TocvErodeDilateMode);
begin
  if LockTransform then
    try
      FMorphOp := Value;
    finally
      UnlockTransform;
    end;
end;

procedure TovcCustomErodeDilate.SetRadius(const Value: Integer);
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

function TovcErode.Transform(const Source: pIplImage; var Destanation: pIplImage): Boolean;
Var
  Kern: pIplConvKernel;
begin
  Result := False;
  if LockTransform then
    try
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

function TovcDilate.Transform(const Source: pIplImage; var Destanation: pIplImage): Boolean;
Var
  Kern: pIplConvKernel;
begin
  Result := False;
  if LockTransform then
    try
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

procedure TocvLaplace.AssignTo(Dest: TPersistent);
begin
  if Dest is TocvLaplace then
  begin
    FAperture := (Dest as TocvLaplace).Aperture;
  end
  else
    inherited;
end;

constructor TocvLaplace.Create(AOwner: TComponent);
begin
  inherited;
  FAperture := 3;
end;

procedure TocvLaplace.SetAperture(const Value: Integer);
begin
  if LockTransform then
    try
      FAperture := Value;
    finally
      UnlockTransform;
    end;
end;

function TocvLaplace.Transform(const Source: pIplImage; var Destanation: pIplImage): Boolean;
Var
  D: pIplImage;
begin
  Result := False;
  if LockTransform then
    try
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

procedure TovcSobel.AssignTo(Dest: TPersistent);
begin
  if Dest is TovcSobel then
  begin
    FXOrder := (Dest as TovcSobel).XOrder;
    FYOrder := (Dest as TovcSobel).YOrder;
    FAperture := (Dest as TovcSobel).Aperture;
  end
  else
    inherited;
end;

constructor TovcSobel.Create(AOwner: TComponent);
begin
  inherited;
  FXOrder := 1;
  FYOrder := 1;
  FAperture := 3;
end;

procedure TovcSobel.SetAperture(const Value: Integer);
begin
  if LockTransform then
    try
      FAperture := Value;
    finally
      UnlockTransform;
    end;
end;

procedure TovcSobel.SetXOrder(const Value: Integer);
begin
  if LockTransform then
    try
      if (YOrder <> 0) and (Value <> 0) then
        FXOrder := Value;
    finally
      UnlockTransform;
    end;
end;

procedure TovcSobel.SetYOrder(const Value: Integer);
begin
  if LockTransform then
    try
      if (XOrder <> 0) and (Value <> 0) then
        FYOrder := Value;
    finally
      UnlockTransform;
    end;
end;

function TovcSobel.Transform(const Source: pIplImage; var Destanation: pIplImage): Boolean;
Var
  D: pIplImage;
begin
  Result := False;
  if LockTransform then
    try
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

// function TocvImageOperationCollectionItem.GetOwner: TComponent;
// begin
// Result := (Collection as TOwnedCollection).Owner as TComponent;
// end;

function TocvImageOperationCollectionItem.GetProperties: TocvCustomImageOperation;
begin
  if not Assigned(FOperation) then
    FOperation := TocvImageOperation_None.Create(nil);
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

initialization

GetRegisteredImageOperations.RegisterIOClass(TocvImageOperation_None, 'None');
GetRegisteredImageOperations.RegisterIOClass(TocvImageOperation_GrayScale, 'GrayScale');
GetRegisteredImageOperations.RegisterIOClass(TovcImageOperation_Canny, 'Canny');
GetRegisteredImageOperations.RegisterIOClass(TovcImageOperation_Smooth, 'Smooth');
GetRegisteredImageOperations.RegisterIOClass(TovcErode, 'Erode');
GetRegisteredImageOperations.RegisterIOClass(TovcDilate, 'Dilate');
GetRegisteredImageOperations.RegisterIOClass(TocvLaplace, 'Laplace');
GetRegisteredImageOperations.RegisterIOClass(TovcSobel, 'Sobel');

finalization

if Assigned(_RegisteredImageOperations) then
  FreeAndNil(_RegisteredImageOperations);

end.
