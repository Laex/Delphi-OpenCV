(* /*****************************************************************
  //                       Delphi-OpenCV Demo
  //               Copyright (C) 2013 Project Delphi-OpenCV
  // ****************************************************************
  // Contributor:
  // laentir Valetov
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
  ******************************************************************* *)

unit uOCVImageOperation;

interface

uses
  System.SysUtils,
  System.Classes,
  System.SyncObjs,
  uOCVTypes,
  core.types_c;

type

  TocvCustomImageOperations = class(TComponent)
  private
    CS: TCriticalSection;
  protected
    procedure LockTransform;
    procedure UnlockTransform;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Transform(const Source: pIplImage; var Destanation: pIplImage): Boolean; virtual; abstract;
  end;

  TocvImageOperationClass = class of TocvCustomImageOperations;

  TocvImageOperationNone = class(TocvCustomImageOperations)
  public
    function Transform(const Source: pIplImage; var Destanation: pIplImage): Boolean; override;
  end;

  TocvImageOperationGrayScale = class(TocvCustomImageOperations)
  public
    function Transform(const Source: pIplImage; var Destanation: pIplImage): Boolean; override;
  end;

  TovcImageOperationCanny = class(TocvCustomImageOperations)
  private
    FThreshold2: double;
    FThreshold1: double;
    FApertureSize: Integer;
    procedure SetApertureSize(const Value: Integer);
    procedure SetThreshold1(const Value: double);
    procedure SetThreshold2(const Value: double);
  public
    constructor Create(AOwner: TComponent); override;
    function Transform(const Source: pIplImage; var Destanation: pIplImage): Boolean; override;
  published
    property Threshold1: double Read FThreshold1 write SetThreshold1;
    property Threshold2: double Read FThreshold2 write SetThreshold2;
    property ApertureSize: Integer Read FApertureSize write SetApertureSize;
  end;

  TocvSmoothOperations = (BLUR_NO_SCALE, BLUR, GAUSSIAN, MEDIAN, BILATERAL);

  TovcImageOperationSmooth = class(TocvCustomImageOperations)
  private
    Fsize1: Integer;
    Fsize2: Integer;
    Fsigma1: double;
    Fsigma2: double;
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
    property sigma1: double read Fsigma1 write SetSigma1;
    property sigma2: double read Fsigma2 write Setsigma2;
    property size1: Integer read Fsize1 write SetSize1 default 3;
    property size2: Integer read Fsize2 write SetSize2 default 0;
    property SmoothOperation: TocvSmoothOperations read FSmoothOperation write SetSmoothOperation default GAUSSIAN;
  end;

  TcvImageOperation = (ioNone, ioGrayScale, ioCanny, ioSmooth);

Const
  cvImageOperation: array [TcvImageOperation] of TocvImageOperationClass = (TocvImageOperationNone,
    TocvImageOperationGrayScale, TovcImageOperationCanny, TovcImageOperationSmooth);

Type
  TocvImageOperation = class(TocvDataSourceAndReceiver)
  private
    CS: TCriticalSection;
    FImageOperation: TcvImageOperation;
    FImageOperationParams: TocvCustomImageOperations;
    procedure SetImageOperation(const Value: TcvImageOperation);
    procedure CreateImageOperation;
    function GetImageOperationParams: TocvCustomImageOperations;
    procedure LockTransform;
    procedure UnlockTransform;
  protected
    procedure TakeImage(const IplImage: pIplImage); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property ImageOperation: TcvImageOperation Read FImageOperation write SetImageOperation;
    property ImageOperationParams: TocvCustomImageOperations Read GetImageOperationParams;
  end;

implementation

Uses
  core_c,
  imgproc_c,
  imgproc.types_c;

{ TocvImageOperation }

procedure TocvImageOperation.SetImageOperation(const Value: TcvImageOperation);
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

function TocvImageOperation.GetImageOperationParams: TocvCustomImageOperations;
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
      Destanation := cvCreateImage(cvGetSize(IplImage), IPL_DEPTH_8U, 1);
    try
      if FImageOperationParams.Transform(IplImage, Destanation) then
      begin
        LockTransform;
        try
          NotifyRecipients(Destanation);
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

constructor TovcImageOperationCanny.Create(AOwner: TComponent);
begin
  inherited;
  FThreshold1 := 10;
  FThreshold2 := 100;
  FApertureSize := 3;
end;

procedure TovcImageOperationCanny.SetApertureSize(const Value: Integer);
begin
  LockTransform;
  try
    FApertureSize := Value;
  finally
    UnlockTransform;
  end;
end;

procedure TovcImageOperationCanny.SetThreshold1(const Value: double);
begin
  LockTransform;
  try
    FThreshold1 := Value;
  finally
    UnlockTransform;
  end;
end;

procedure TovcImageOperationCanny.SetThreshold2(const Value: double);
begin
  LockTransform;
  try
    FThreshold2 := Value;
  finally
    UnlockTransform;
  end;
end;

function TovcImageOperationCanny.Transform(const Source: pIplImage; var Destanation: pIplImage): Boolean;
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
    Result := True;
  finally
    UnlockTransform;
  end;
end;

{ TocvImageOperationGrayScale }

function TocvImageOperationGrayScale.Transform(const Source: pIplImage; var Destanation: pIplImage): Boolean;
begin
  Destanation := cvCreateImage(cvGetSize(Source), IPL_DEPTH_8U, 1);
  // преобразуем в градации cерого
  cvCvtColor(Source, Destanation, CV_RGB2GRAY);
  Result := True;
end;

{ TocvImageOperationNone }

function TocvImageOperationNone.Transform(const Source: pIplImage; var Destanation: pIplImage): Boolean;
begin
  Destanation := Source;
  Result := True;
end;

{ TCustomOpenCVImgOperation }

constructor TocvCustomImageOperations.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  CS := TCriticalSection.Create;
  SetSubComponent(True);
end;

destructor TocvCustomImageOperations.Destroy;
begin
  CS.Free;
  inherited;
end;

procedure TocvCustomImageOperations.LockTransform;
begin
  CS.Enter;
end;

procedure TocvCustomImageOperations.UnlockTransform;
begin
  CS.Leave;
end;

{ TovcImageOperationSmooth }
Const
  ocvSmoothOperations: array [TocvSmoothOperations] of Integer = (CV_BLUR_NO_SCALE, CV_BLUR, CV_GAUSSIAN, CV_MEDIAN,
    CV_BILATERAL);

constructor TovcImageOperationSmooth.Create(AOwner: TComponent);
begin
  inherited;
//  FSmoothOperation := GAUSSIAN;
//  Fsize1 := 3;
//  Fsize2 := 3;
  Fsigma1 := 0;
  Fsigma2 := 0;
end;

procedure TovcImageOperationSmooth.SetSigma1(const Value: double);
begin
  LockTransform;
  try
    Fsigma1 := Value;
  finally
    UnlockTransform;
  end;
end;

procedure TovcImageOperationSmooth.Setsigma2(const Value: double);
begin
  LockTransform;
  try
    Fsigma2 := Value;
  finally
    UnlockTransform;
  end;
end;

procedure TovcImageOperationSmooth.SetSize1(const Value: Integer);
begin
  LockTransform;
  try
    Fsize1 := Value;
  finally
    UnlockTransform;
  end;
end;

procedure TovcImageOperationSmooth.SetSize2(const Value: Integer);
begin
  LockTransform;
  try
    Fsize2 := Value;
  finally
    UnlockTransform;
  end;
end;

procedure TovcImageOperationSmooth.SetSmoothOperation(const Value: TocvSmoothOperations);
begin
  LockTransform;
  try
    FSmoothOperation := Value;
  finally
    UnlockTransform
  end;
end;

function TovcImageOperationSmooth.Transform(const Source: pIplImage; var Destanation: pIplImage): Boolean;
begin
  LockTransform;
  try
    Destanation := cvCloneImage(Source);
    cvSmooth(Source, Destanation, ocvSmoothOperations[SmoothOperation], Fsize1, Fsize2, Fsigma1, Fsigma2);
    Result := True;
  finally
    UnlockTransform;
  end;
end;

initialization

RegisterClasses([TocvImageOperationNone, TocvImageOperationGrayScale, TovcImageOperationCanny,
  TovcImageOperationSmooth]);

end.
