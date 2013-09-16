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
    CS    : TCriticalSection;
    FOwner: TPersistent;
  protected
    procedure LockTransform;
    procedure UnlockTransform;
    function GetOwner: TPersistent; override;
  public
    constructor Create(AOwner: TPersistent); virtual;
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
  private
    FThreshold2  : double;
    FThreshold1  : double;
    FApertureSize: Integer;
    procedure SetApertureSize(const Value: Integer);
    procedure SetThreshold1(const Value: double);
    procedure SetThreshold2(const Value: double);
    procedure ReadParamsThreshold1(Reader: TReader);
    procedure WriteParamsThreshold1(Writer: TWriter);
    procedure ReadParamsThreshold2(Reader: TReader);
    procedure WriteParamsThreshold2(Writer: TWriter);
    procedure ReadParamsApertureSize(Reader: TReader);
    procedure WriteParamsApertureSize(Writer: TWriter);
  protected
    procedure DefineProperties(Filer: TFiler); override;
  public
    constructor Create(AOwner: TPersistent); override;
    procedure Assign(Source: TPersistent); override;
    function Transform(const Source: pIplImage; var Destanation: pIplImage): Boolean; override;
  published
    property Threshold1  : double Read FThreshold1 write SetThreshold1;
    property Threshold2  : double Read FThreshold2 write SetThreshold2;
    property ApertureSize: Integer Read FApertureSize write SetApertureSize default 100;
  end;

  TocvSmoothOperations = (BLUR_NO_SCALE, BLUR, GAUSSIAN, MEDIAN, BILATERAL);

  TovcImageOperation_Smooth = class(TocvCustomImageOperation)
  private
    FSigma2         : double;
    FSize2          : Integer;
    FSigma1         : double;
    FSize1          : Integer;
    FSmoothOperation: TocvSmoothOperations;
    procedure SetSigma1(const Value: double);
    procedure Setsigma2(const Value: double);
    procedure SetSize1(const Value: Integer);
    procedure SetSize2(const Value: Integer);
    procedure SetSmoothOperation(const Value: TocvSmoothOperations);
  protected
  public
    constructor Create(AOwner: TPersistent); override;
    function Transform(const Source: pIplImage; var Destanation: pIplImage): Boolean; override;
    procedure Assign(Source: TPersistent); override;
  published
    property sigma1         : double read FSigma1 write SetSigma1;
    property sigma2         : double read FSigma2 write Setsigma2;
    property size1          : Integer read FSize1 write SetSize1 default 3;
    property size2          : Integer read FSize2 write SetSize2 default 3;
    property SmoothOperation: TocvSmoothOperations read FSmoothOperation write SetSmoothOperation default GAUSSIAN;
  end;

  TcvImageOperations = (ioNone, ioGrayScale, ioCanny, ioSmooth);

Const
  cvImageOperation: array [TcvImageOperations] of TocvImageOperationClass = (TocvImageOperation_None,
    TocvImageOperation_GrayScale, TovcImageOperation_Canny, TovcImageOperation_Smooth);

Type

  TocvImageOperation = class(TocvDataSourceAndReceiver)
  private
    CS                   : TCriticalSection;
    FImageOperation      : TcvImageOperations;
    FImageOperationParams: TocvCustomImageOperation;
    procedure CreateImageOperation;
    procedure LockTransform;
    procedure UnlockTransform;
    function GetImageOperationParams: TocvCustomImageOperation;
    procedure SetImageOperations(const Value: TcvImageOperations);
  protected
    procedure TakeImage(const IplImage: pIplImage); override;
  public
    constructor Create(AOwner: TComponent); //override;
    destructor Destroy; override;
  published
    property ImageOperation      : TcvImageOperations Read FImageOperation write SetImageOperations;
    property ImageOperationParams: TocvCustomImageOperation Read GetImageOperationParams;
  end;

implementation

Uses
  core_c,
  imgproc_c,
  imgproc.types_c;

{ TocvImageOperation }

procedure TocvImageOperation.SetImageOperations(const Value: TcvImageOperations);
begin
  if FImageOperation <> Value then
  begin
    LockTransform;
    try
      FImageOperation := Value;
      CreateImageOperation;
    finally
      UnlockTransform;
    end;
  end;
end;

constructor TocvImageOperation.Create(AOwner: TComponent);
begin
  inherited;
  CS := TCriticalSection.Create;
  CreateImageOperation;
end;

procedure TocvImageOperation.CreateImageOperation;
begin
  if Assigned(FImageOperationParams) then
    FreeAndNil(FImageOperationParams);
  FImageOperationParams := cvImageOperation[FImageOperation].Create(Self);
end;

destructor TocvImageOperation.Destroy;
begin
  LockTransform;
  if Assigned(FImageOperationParams) then
    FreeAndNil(FImageOperationParams);
  CS.Free;
  inherited;
end;

function TocvImageOperation.GetImageOperationParams: TocvCustomImageOperation;
begin
  if not Assigned(FImageOperationParams) then
    CreateImageOperation;
  Result := FImageOperationParams;
end;

procedure TocvImageOperation.LockTransform;
begin
  CS.Enter;
end;

procedure TocvImageOperation.TakeImage(const IplImage: pIplImage);
var
  Destanation: pIplImage;
begin
  if Assigned(FImageOperationParams) then
  begin
    if ImageOperation <> ioNone then
      Destanation := cvCreateImage(
        cvGetSize(IplImage),
        IPL_DEPTH_8U,
        1);
    try
      if FImageOperationParams.Transform(IplImage, Destanation) then
      begin
        LockTransform;
        try
          NotifyReceiver(Destanation);
        finally
          UnlockTransform;
        end;
      end;
    finally
      if ImageOperation <> ioNone then
        cvReleaseImage(Destanation);
    end;
  end;
end;

procedure TocvImageOperation.UnlockTransform;
begin
  CS.Leave;
end;

{ TovcImageOperationCanny }

procedure TovcImageOperation_Canny.Assign(Source: TPersistent);
Var
  imC: TovcImageOperation_Canny;
begin
  inherited;
  if Source is TovcImageOperation_Canny then
  begin
    imC           := Source as TovcImageOperation_Canny;
    FThreshold1   := imC.Threshold1;
    FThreshold2   := imC.Threshold2;
    FApertureSize := imC.ApertureSize;
  end;
end;

constructor TovcImageOperation_Canny.Create { (AOwner: TPersistent) };
begin
  inherited;
  FThreshold1   := 10;
  FThreshold2   := 100;
  FApertureSize := 3;
end;
procedure TovcImageOperation_Canny.ReadParamsThreshold1(Reader: TReader);
begin
  // Найти маркер начала списка
  Reader.ReadListBegin;
  FThreshold1 := Reader.ReadFloat;
  // Прочитать маркер окончания списка
  Reader.ReadListEnd;
end;

procedure TovcImageOperation_Canny.WriteParamsThreshold1(Writer: TWriter);
begin
  // Найти маркер начала списка
  Writer.WriteListBegin;
  Writer.WriteFloat(FThreshold1);
  // Прочитать маркер окончания списка
  Writer.WriteListEnd;
end;

procedure TovcImageOperation_Canny.ReadParamsThreshold2(Reader: TReader);
begin
  // Найти маркер начала списка
  Reader.ReadListBegin;
  FThreshold2 := Reader.ReadFloat;
  // Прочитать маркер окончания списка
  Reader.ReadListEnd;
end;

procedure TovcImageOperation_Canny.WriteParamsThreshold2(Writer: TWriter);
begin
  // Найти маркер начала списка
  Writer.WriteListBegin;
  Writer.WriteFloat(FThreshold2);
  // Прочитать маркер окончания списка
  Writer.WriteListEnd;
end;

procedure TovcImageOperation_Canny.ReadParamsApertureSize(Reader: TReader);
begin
  // Найти маркер начала списка
  Reader.ReadListBegin;
  FApertureSize := Reader.ReadInteger;
  // Прочитать маркер окончания списка
  Reader.ReadListEnd;
end;

procedure TovcImageOperation_Canny.WriteParamsApertureSize(Writer: TWriter);
begin
  // Найти маркер начала списка
  Writer.WriteListBegin;
  Writer.WriteInteger(FApertureSize);
  // Прочитать маркер окончания списка
  Writer.WriteListEnd;
end;

procedure TovcImageOperation_Canny.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineProperty(
    'Threshold1',
    ReadParamsThreshold1,
    WriteParamsThreshold1,
    true);
  Filer.DefineProperty(
    'Threshold2',
    ReadParamsThreshold2,
    WriteParamsThreshold2,
    true);
  Filer.DefineProperty(
    'ApertureSize',
    ReadParamsApertureSize,
    WriteParamsApertureSize,
    true);
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
    gray := cvCreateImage(
      cvGetSize(Source),
      IPL_DEPTH_8U,
      1);
    Destanation := cvCreateImage(
      cvGetSize(Source),
      IPL_DEPTH_8U,
      1);
    // преобразуем в градации cерого
    cvCvtColor(
      Source,
      gray,
      CV_RGB2GRAY);
    // получаем границы
    cvCanny(
      gray,
      Destanation,
      Threshold1,
      Threshold2,
      ApertureSize);
    cvReleaseImage(gray);
    Result := true;
  finally
    UnlockTransform;
  end;
end;

{ TocvImageOperationGrayScale }

function TocvImageOperation_GrayScale.Transform(const Source: pIplImage; var Destanation: pIplImage): Boolean;
begin
  Destanation := cvCreateImage(
    cvGetSize(Source),
    IPL_DEPTH_8U,
    1);
  // преобразуем в градации cерого
  cvCvtColor(
    Source,
    Destanation,
    CV_RGB2GRAY);
  Result := true;
end;

{ TocvImageOperationNone }

function TocvImageOperation_None.Transform(const Source: pIplImage; var Destanation: pIplImage): Boolean;
begin
  Destanation := Source;
  Result      := true;
end;

{ TCustomOpenCVImgOperation }

constructor TocvCustomImageOperation.Create { (AOwner: TPersistent) };
begin
  inherited Create;
  FOwner := AOwner;
  CS     := TCriticalSection.Create;
  // SetLength(
  // FValues,
  // 10);
  // FOwner := AOwner;
end;

destructor TocvCustomImageOperation.Destroy;
begin
  CS.Free;
  inherited;
end;

function TocvCustomImageOperation.GetOwner: TPersistent;
begin
  Result := FOwner;
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
  ocvSmoothOperations: array [TocvSmoothOperations] of Integer = (CV_BLUR_NO_SCALE, CV_BLUR, CV_GAUSSIAN, CV_MEDIAN,
    CV_BILATERAL);

procedure TovcImageOperation_Smooth.Assign(Source: TPersistent);
Var
  imS: TovcImageOperation_Smooth;
begin
  inherited;
  if Source is TovcImageOperation_Smooth then
  begin
    imS              := Source as TovcImageOperation_Smooth;
    FSmoothOperation := imS.SmoothOperation;
    FSize1           := imS.size1;
    FSize2           := imS.size2;
    FSigma1          := imS.sigma1;
    FSigma2          := imS.sigma2;
  end;
end;

constructor TovcImageOperation_Smooth.Create { (AOwner: TPersistent) };
begin
  inherited;
  FSmoothOperation := GAUSSIAN;
  FSize1           := 3;
  FSize2           := 3;
  FSigma1          := 0;
  FSigma2          := 0;
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
    cvSmooth(
      Source,
      Destanation,
      ocvSmoothOperations[SmoothOperation],
      size1,
      size2,
      sigma1,
      sigma2);
    Result := true;
  finally
    UnlockTransform;
  end;
end;

initialization

RegisterClasses([TocvImageOperation_None, TocvImageOperation_GrayScale, TovcImageOperation_Canny,
  TovcImageOperation_Smooth]);

end.
