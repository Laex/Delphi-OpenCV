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

{$IFNDEF CLR}
{$I OpenCV.inc}
unit ocv.comp.Types;
{$ENDIF}

interface

uses
{$IFDEF HAS_UNITSCOPE}
{$IFDEF MSWINDOWS}
  Winapi.Windows,
{$ENDIF MSWINDOWS}
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  System.Types,

{$IFDEF HAS_FMX}
  FMX.Graphics,
{$ELSE}
  Vcl.Graphics,
{$ENDIF HAS_FMX}
{$ELSE}
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF MSWINDOWS}
  SysUtils,
  Classes,
  Graphics,
{$IFNDEF DELPHI5}Types, {$ENDIF DELPHI5}
{$ENDIF HAS_UNITSCOPE}
  ocv.core_c,
  ocv.core.types_c;

type
  TocvRect = type TRect;

{$IFDEF DELPHI2009_UP}

  TocvRectHelper = record helper for TocvRect
    function cvRect: TcvRect;
  end;
{$ENDIF}

  TocvLine = record
    S, E: TCvPoint;
  end;

  TocvCircle = record
    cX, cY: Integer;
    Radius: Integer;
  end;

  TocvColorEncode = (ceRGB, ceGRAY);

  TocvPixel = record
    case c: TocvColorEncode of
      ceRGB:
        (R, G, B: byte);
      ceGRAY:
        (GV: byte);
  end;

const
  cmRGB: TA4CVChar = 'RGB'#0;
  cmBGR: TA4CVChar = 'BGR'#0;
  cmGRAY: TA4CVChar = 'GRAY';

Type
{$IFDEF DELPHIXE_UP}
  TocvRects = {$IFDEF FPC}specialize {$ENDIF}TArray<TocvRect>;
  TocvCircles = {$IFDEF FPC}specialize {$ENDIF}TArray<TocvCircle>;
  TocvLines = {$IFDEF FPC}specialize {$ENDIF}TArray<TocvLine>;
{$ELSE}
  TocvRects = Array of TocvRect;
  TocvCircles = Array of TocvCircle;
  TocvLines = array of TocvLine;
{$ENDIF}
  TocvLineType = (LT_FILLED, LT_8, LT_AA);

  IocvFont = interface
    ['{3EAFF1CE-7C65-4138-829F-329C81DDED8F}']
    function GetFontName: string;
    procedure SetFontName(const Value: string);
    function GetFontColor: TColor;
    procedure SetFontColor(const Value: TColor);
    function GetFontThickness: Integer;
    procedure SetFontThickness(const Value: Integer);
    function GetFontLineType: TocvLineType;
    procedure SetFontLineType(const Value: TocvLineType);
    function GetFontHScale: Single;
    procedure SetFontHScale(const Value: Single);
    function GetFontVScale: Single;
    procedure SetFontVScale(const Value: Single);
    function GetCvFont: TCvFont;
    property Name: string read GetFontName write SetFontName;
    property Color: TColor read GetFontColor write SetFontColor;
    property Thickness: Integer read GetFontThickness write SetFontThickness;
    property LineType: TocvLineType read GetFontLineType write SetFontLineType;
    property HScale: Single read GetFontHScale write SetFontHScale;
    property VScale: Single read GetFontVScale write SetFontVScale;
    property cvFont: TCvFont Read GetCvFont;
  end;

  IocvCanvas = interface
    ['{D5BCBC44-8139-42A7-A97A-0A5AD33C6526}']
    function GetOcvFont: IocvFont;
    property ocvFont: IocvFont read GetOcvFont;

    procedure Rectangle(const x1, y1, x2, y2: Integer; const Color: TColor = clRed; const Thickness: Integer = 1;
      const LineType: TocvLineType = LT_8; const Shift: Integer = 0); overload;
    procedure Rectangle(const ARect: TocvRect; const Color: TColor = clRed; const Thickness: Integer = 1;
      const LineType: TocvLineType = LT_8; const Shift: Integer = 0); overload;
    procedure Circle(const x, y, R: Integer; const Color: TColor = clRed; const Thickness: Integer = 1; const LineType: TocvLineType = LT_8;
      const Shift: Integer = 0);
    procedure Ellipse(const CenterX, CenterY: Integer; const Axes: TocvRect; const Angle: double; const start_angle: double;
      const nd_angle: double; const Color: TColor = clRed; const Thickness: Integer = 1; const LineType: TocvLineType = LT_8;
      const Shift: Integer = 0);
    procedure EllipseBox(const Box: TocvRect; const Angle: Single; const Color: TColor = clRed; const Thickness: Integer = 1;
      Const LineType: TocvLineType = LT_8; const Shift: Integer = 0); overload;
    procedure EllipseBox(const Box: TCvBox2D; const Color: TColor = clRed; const Thickness: Integer = 1;
      Const LineType: TocvLineType = LT_8; const Shift: Integer = 0); overload;
    procedure TextOut(const x, y: Integer; const Text: String; const Shadow: Boolean = False);
  end;

  IocvImage = interface
    ['{84567F57-A399-4179-AA0F-6F8A2788F89B}']
    function GetIplImage: pIplImage;
    function GetisGray: Boolean;
    function GrayImage: IocvImage;
    function Clone: IocvImage;
    function Same: IocvImage;
    function AsBitmap: TBitmap;
    function Crop(const roi: TcvRect): IocvImage;
    function GetWidth: Integer;
    function GetHeight: Integer;
    function GetCanvas: IocvCanvas;
    function Resize(const nW, nH: Integer; const interpolation: Integer = CV_INTER_LINEAR): IocvImage;
    function Scale(const dw, dh: Single; const interpolation: Integer = CV_INTER_LINEAR): IocvImage;
    function GetPixel(const x, y: Integer): TocvPixel;
    procedure SetPixel(const x, y: Integer; const P: TocvPixel);
    // -------------------------------------------
    property IpImage: pIplImage Read GetIplImage;
    property isGray: Boolean Read GetisGray;
    property Width: Integer Read GetWidth;
    property Height: Integer Read GetHeight;
    property Canvas: IocvCanvas Read GetCanvas;
    property Pixel[const x, y: Integer]: TocvPixel read GetPixel write SetPixel;
  end;

  TocvFont = class(TInterfacedObject, IocvFont)
  private
    FCvFont: TCvFont;
    FFontColor: TColor;
    FFontLineType: TocvLineType;
    procedure CreateOcvFont;
  protected
    function GetFontName: string;
    procedure SetFontName(const Value: string);
    function GetFontColor: TColor;
    procedure SetFontColor(const Value: TColor);
    function GetFontThickness: Integer;
    procedure SetFontThickness(const Value: Integer);
    function GetFontLineType: TocvLineType;
    procedure SetFontLineType(const Value: TocvLineType);
    function GetFontHScale: Single;
    procedure SetFontHScale(const Value: Single);
    function GetFontVScale: Single;
    procedure SetFontVScale(const Value: Single);
    function GetCvFont: TCvFont;
  public
    constructor Create;
    property Name: string read GetFontName write SetFontName;
    property Color: TColor read GetFontColor write SetFontColor;
    property Thickness: Integer read GetFontThickness write SetFontThickness;
    property LineType: TocvLineType read GetFontLineType write SetFontLineType;
    property HScale: Single read GetFontHScale write SetFontHScale;
    property VScale: Single read GetFontVScale write SetFontVScale;
    property cvFont: TCvFont Read GetCvFont;
  end;

  TocvImage = class;

  TocvCanvas = class(TInterfacedObject, IocvCanvas)
  private
    FOwner: TocvImage;
    FocvFont: IocvFont;
  protected
    function GetOcvFont: IocvFont;
  public
    constructor Create(AOwner: TocvImage);
    destructor Destroy; override;
    procedure Rectangle(const x1, y1, x2, y2: Integer; const Color: TColor = clRed; const Thickness: Integer = 1;
      const LineType: TocvLineType = LT_8; const Shift: Integer = 0); overload;
    procedure Rectangle(const ARect: TocvRect; const Color: TColor = clRed; const Thickness: Integer = 1;
      const LineType: TocvLineType = LT_8; const Shift: Integer = 0); overload;
    procedure Circle(const CenterX, CenterY, Radius: Integer; const Color: TColor = clRed; const Thickness: Integer = 1;
      const LineType: TocvLineType = LT_8; const Shift: Integer = 0);
    procedure Ellipse(const CenterX, CenterY: Integer; const Axes: TocvRect; const Angle: double; const start_angle: double;
      const nd_angle: double; const Color: TColor = clRed; const Thickness: Integer = 1; const LineType: TocvLineType = LT_8;
      const Shift: Integer = 0);
    procedure EllipseBox(const Box: TocvRect; const Angle: Single; const Color: TColor = clRed; const Thickness: Integer = 1;
      Const LineType: TocvLineType = LT_8; const Shift: Integer = 0); overload;
    procedure EllipseBox(const Box: TCvBox2D; const Color: TColor = clRed; const Thickness: Integer = 1;
      Const LineType: TocvLineType = LT_8; const Shift: Integer = 0); overload;
    procedure TextOut(const x, y: Integer; const Text: String; const Shadow: Boolean = False);
    property ocvFont: IocvFont read GetOcvFont;
  end;

  TocvImage = class(TInterfacedObject, IocvImage)
  private
    FImage: pIplImage;
    FocvCanvas: IocvCanvas;
  protected
    function GetIplImage: pIplImage;
    function GetisGray: Boolean;
    function GetWidth: Integer;
    function GetHeight: Integer;
    function GetCanvas: IocvCanvas;
    function GetPixel(const x, y: Integer): TocvPixel;
    procedure SetPixel(const x, y: Integer; const P: TocvPixel);
  public
    constructor Create; overload;
    constructor Create(const AImage: pIplImage); overload;
    constructor Create(const Bitmap: TBitmap); overload;
    constructor CreateClone(const AImage: pIplImage);
    constructor LoadFormFile(const FileName: String);
    destructor Destroy; override;
    function GrayImage: IocvImage;
    function Clone: IocvImage;
    function Same: IocvImage;
    function AsBitmap: TBitmap;
    function Resize(const nW, nH: Integer; const interpolation: Integer = CV_INTER_LINEAR): IocvImage;
    function Scale(const dw, dh: Single; const interpolation: Integer = CV_INTER_LINEAR): IocvImage;
    function Crop(const roi: TcvRect): IocvImage;
    property IplImage: pIplImage Read GetIplImage;
    property isGray: Boolean Read GetisGray;
    property IpImage: pIplImage Read GetIplImage;
    property Width: Integer Read GetWidth;
    property Height: Integer Read GetHeight;
    property Canvas: IocvCanvas Read GetCanvas;
    property Pixel[const x, y: Integer]: TocvPixel read GetPixel write SetPixel;
  end;

  TOnOcvNotifyCollectionItem = procedure(PrevOperation, Operation, NextOperation: TObject; const IplImage: IocvImage;
    Var ContinueTransform: Boolean) of object;

  TOnOcvNotify = procedure(Sender: TObject; Var IplImage: IocvImage) of object;
  TOnOcvAfterViewPaint = procedure(Sender: TObject; const IplImage: IocvImage) of object;
  TOnOcvAfterTransform = TOnOcvNotify;
  TOnOcvBeforeTransform = procedure(Sender: TObject; const IplImage: IocvImage; Var ContinueTransform: Boolean) of object;
  TOnOcvContour = procedure(Sender: TObject; const IplImage: IocvImage; const ContourCount: Integer; const Contours: pCvSeq) of object;
  TOnOcvHaarCascade = procedure(Sender: TObject; const IplImage: IocvImage; const HaarRects: TocvRects) of object;
  TOnOcvRect = procedure(Sender: TObject; const IplImage: IocvImage; const Rect: TocvRect) of object;
  TOnOcvRects = procedure(Sender: TObject; const IplImage: IocvImage; const Rects: TocvRects) of object;
  TOnOcvCircles = procedure(Sender: TObject; const IplImage: IocvImage; const Circles: TocvCircles) of object;
  TOnOcvLines = procedure(Sender: TObject; const IplImage: IocvImage; const Lines: TocvLines) of object;

  IocvDataReceiver = interface
    ['{F67DEC9E-CCE0-49D2-AB9B-AD7E1020C5DC}']
    procedure TakeImage(const IplImage: IocvImage);
    procedure SetVideoSource(const Value: TObject);
  end;

  IocvDataSource = interface
    ['{80640C0A-6828-42F8-83E7-DA5FD9036DFF}']
    procedure AddReceiver(const OpenCVVideoReceiver: IocvDataReceiver);
    procedure RemoveReceiver(const OpenCVVideoReceiver: IocvDataReceiver);

    function GetName: string;
    function GetImage: IocvImage;
    function GetEnabled: Boolean;
    function GetHeight: Integer;
    function GetWidth: Integer;
    function GetFPS: double;

    property Enabled: Boolean Read GetEnabled;
    property Image: IocvImage read GetImage;
    property Name: String read GetName;
    property Width: Integer Read GetWidth;
    property Height: Integer Read GetHeight;
    property FPS: double read GetFPS;
  end;

  TocvReceiverList = class(TThreadList) // <IocvDataReceiver>;
  public
    procedure Add(Item: IocvDataReceiver);
    procedure Remove(Item: IocvDataReceiver); {$IFDEF USE_INLINE}inline; {$ENDIF}
  end;

  TocvDataSource = class(TComponent, IocvDataSource)
  protected
    FOpenCVVideoReceivers: TocvReceiverList;
    FImage: IocvImage;
    FWidth, FHeight: Integer;
    FFPS: double;
    function GetName: string; virtual;
    procedure NotifyReceiver(const IplImage: IocvImage); virtual;
    function GetImage: IocvImage;
    function GetEnabled: Boolean; virtual;
    function GetHeight: Integer; virtual;
    function GetWidth: Integer; virtual;
    function GetFPS: double; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AddReceiver(const OpenCVVideoReceiver: IocvDataReceiver); virtual;
    procedure RemoveReceiver(const OpenCVVideoReceiver: IocvDataReceiver); virtual;
    property Image: IocvImage Read GetImage;
  end;

  TocvDataReceiver = class(TComponent, IocvDataReceiver)
  private
    [weak] FocvVideoSource: IocvDataSource;
  protected
    procedure SetVideoSource(const Value: TObject); virtual;
    procedure SetOpenCVVideoSource(const Value: IocvDataSource); virtual;
  public
    procedure TakeImage(const IplImage: IocvImage); virtual;
    destructor Destroy; override;
    function isSourceEnabled: Boolean; virtual;
  published
    property VideoSource: IocvDataSource Read FocvVideoSource write SetOpenCVVideoSource;
  end;

  TocvDataSourceAndReceiver = class(TocvDataSource, IocvDataReceiver)
  private
    [weak] FocvVideoSource: IocvDataSource;
  protected
    procedure SetVideoSource(const Value: TObject); virtual;
    procedure SetOpenCVVideoSource(const Value: IocvDataSource); virtual;
  public
    procedure TakeImage(const IplImage: IocvImage); virtual;
    destructor Destroy; override;
  published
    property VideoSource: IocvDataSource Read FocvVideoSource write SetOpenCVVideoSource;
  end;

  // Haar cascade types
{$I ocvHaarCascadeType.inc}

  // TocvHaarCascadeType =
  // (
  // hcEye,
  // hcEyeTreeEyeGlasses,
  // hcFrontalCatFace,
  // hcFrontalCatFaceExtended,
  // hcFrontalFaceAlt,
  // hcFrontalFaceAlt2,
  // hcFrontalFaceAltTree,
  // hcFrontalFaceDefaut,
  // hcFullBody,
  // hcLeftEye2Splits,
  // hcLowerBody,
  // hcMcsEyePairBig,
  // hcMcsEyePairSmall,
  // hcMcsLeftEar,
  // hcMcsLeftEye,
  // hcMcsMouth,
  // hcMcsNose,
  // hcMcsRightEar,
  // hcMcsRightEye,
  // hcMcsUpperBody,
  // hcProfileFace,
  // hcRightEye2Splits,
  // hcSmile,
  // hcUpperBody,
  // hcPlateNumberRus,
  // hcLicencePlateRus16stages
  // );
  TocvHaarCascadeFlag = (HAAR_DO_CANNY_PRUNING, HAAR_SCALE_IMAGE, HAAR_FIND_BIGGEST_OBJECT, HAAR_DO_ROUGH_SEARCH);
  TocvHaarCascadeFlagSet = set of TocvHaarCascadeFlag;

function HaarSetToFlag(const CascadeFlags: TocvHaarCascadeFlagSet): Integer;

function ocvRect(Left, Top, Right, Bottom: Integer): TocvRect;
function ocvRectCenter(cX, cY, Width, Height: Integer): TocvRect;
function cvRect(const oRect: TocvRect): TcvRect;

procedure GetRGBValue(const AColor: TColor; var R, G, B: byte);
function ColorToCvRGB(const Color: TColor): TCvScalar;

function ocvPixel(const R, G, B: byte): TocvPixel;

const
  cLineType: array [TocvLineType] of Integer = (CV_FILLED, 8, CV_AA);

  ///
  // Run utils\CompressHaar\uCompressHaar.dpr
  // Add to search path \Delphi-OpenCV\resource\facedetectxml\
  ///
{$I haarcascade.inc}
{$R haarcascade.res haarcascade.rc}
{$R haarcascade.res}

implementation

uses
  ocv.imgproc_c,
  ocv.imgproc.types_c,
  ocv.highgui_c, ocv.utils;

function ocvPixel(const R, G, B: byte): TocvPixel;
begin
  Result.c := ceRGB;
  Result.R := R;
  Result.G := G;
  Result.B := B;
end;

function cvRect(const oRect: TocvRect): TcvRect;
begin
  Result := ocv.core.types_c.cvRect(oRect.Left, oRect.Top,
{$IFDEF DELPHIXE2_UP}oRect.Width{$ELSE}oRect.Right - oRect.Left{$ENDIF},
{$IFDEF DELPHIXE2_UP}oRect.Height{$ELSE}oRect.Bottom - oRect.Top{$ENDIF});
end;

function HaarSetToFlag(const CascadeFlags: TocvHaarCascadeFlagSet): Integer;
Var
  i: TocvHaarCascadeFlag;
  j: Integer;
begin
  Result := 0;
  j := 1;
  for i := HAAR_DO_CANNY_PRUNING to HAAR_DO_ROUGH_SEARCH do
  begin
    if i in CascadeFlags then
      Result := Result or j;
    j := j * 2;
  end;
end;

function ocvRect(Left, Top, Right, Bottom: Integer): TocvRect;
begin
  Result.Left := Left;
  Result.Top := Top;
  Result.Bottom := Bottom;
  Result.Right := Right;
end;

function ocvRectCenter(cX, cY, Width, Height: Integer): TocvRect;
begin
  Result.Left := cX - (Width div 2);
  Result.Right := cX + (Width div 2);
  Result.Top := cY - (Height div 2);
  Result.Bottom := cY + (Height div 2);
end;

{ TOpenCVDataSource }

procedure TocvDataSource.AddReceiver(const OpenCVVideoReceiver: IocvDataReceiver);
begin
  FOpenCVVideoReceivers.Add(OpenCVVideoReceiver);
end;

constructor TocvDataSource.Create(AOwner: TComponent);
begin
  inherited;
  FOpenCVVideoReceivers := TocvReceiverList.Create;
end;

destructor TocvDataSource.Destroy;
begin
  FOpenCVVideoReceivers.Free;
  FImage := nil;
  inherited;
end;

function TocvDataSource.GetEnabled: Boolean;
begin
  Result := False;
end;

function TocvDataSource.GetFPS: double;
begin
  Result := FFPS;
end;

function TocvDataSource.GetHeight: Integer;
begin
  Result := FHeight;
end;

function TocvDataSource.GetImage: IocvImage;
begin
  Result := FImage;
end;

function TocvDataSource.GetName: string;
begin
  Result := Name;
end;

function TocvDataSource.GetWidth: Integer;
begin
  Result := FWidth;
end;

procedure TocvDataSource.NotifyReceiver(const IplImage: IocvImage);
Var
  R: Pointer; // IocvDataReceiver;
  LockList: TList; // <IocvDataReceiver>;
begin
  LockList := FOpenCVVideoReceivers.LockList;
  try
    for R in LockList do
      IocvDataReceiver(R).TakeImage(IplImage);
  finally
    FOpenCVVideoReceivers.UnlockList;
  end;
end;

procedure TocvDataSource.RemoveReceiver(const OpenCVVideoReceiver: IocvDataReceiver);
begin
  FOpenCVVideoReceivers.Remove(OpenCVVideoReceiver);
end;

{ TOpenCVDataSourceAndReceiver }

destructor TocvDataSourceAndReceiver.Destroy;
begin
  if Assigned(FocvVideoSource) then
    FocvVideoSource.RemoveReceiver(Self);
  inherited;
end;

procedure TocvDataSourceAndReceiver.SetOpenCVVideoSource(const Value: IocvDataSource);
begin
  if (FocvVideoSource <> Value) then
  begin
    if Assigned(FocvVideoSource) then
      FocvVideoSource.RemoveReceiver(Self);
    FocvVideoSource := Value;
    if Assigned(FocvVideoSource) then
      FocvVideoSource.AddReceiver(Self);
  end;
end;

procedure TocvDataSourceAndReceiver.SetVideoSource(const Value: TObject);
begin
  VideoSource := Value as TocvDataSource;
end;

procedure TocvDataSourceAndReceiver.TakeImage(const IplImage: IocvImage);
begin

end;

{ TocvDataReceiver }

destructor TocvDataReceiver.Destroy;
begin
  if Assigned(FocvVideoSource) then
    FocvVideoSource.RemoveReceiver(Self);
  inherited;
end;

function TocvDataReceiver.isSourceEnabled: Boolean;
begin
  Result := Assigned(VideoSource) and VideoSource.Enabled;
end;

procedure TocvDataReceiver.SetOpenCVVideoSource(const Value: IocvDataSource);
begin
  if (FocvVideoSource <> Value) then
  begin
    if Assigned(FocvVideoSource) then
      FocvVideoSource.RemoveReceiver(Self);
    FocvVideoSource := Value;
    if Assigned(FocvVideoSource) then
      FocvVideoSource.AddReceiver(Self);
  end;
end;

procedure TocvDataReceiver.SetVideoSource(const Value: TObject);
begin
  if (Value <> Self) then
    VideoSource := Value as TocvDataSource;
end;

procedure TocvDataReceiver.TakeImage(const IplImage: IocvImage);
begin

end;

{ TocvImage }

function TocvImage.AsBitmap: TBitmap;
var
  deep: Integer;
  // i, j, K, wStep, Channels: Integer;
  // data: PByteArray;
  // pb: PByteArray;
begin
  if (FImage <> NIL) then
  begin
    Result := TBitmap.Create;
    Result.Width := FImage^.Width;
    Result.Height := FImage^.Height;
    deep := FImage^.nChannels * FImage^.depth;
    case deep of
      8:
        Result.PixelFormat := pf8bit;
      16:
        Result.PixelFormat := pf16bit;
      24:
        Result.PixelFormat := pf24bit;
      32:
        Result.PixelFormat := pf32bit;
    End;
    if not ipDraw(Result.Canvas.Handle, FImage, Rect(0, 0, FImage^.Width - 1, FImage^.Height - 1), False) then
      FreeAndNil(Result);
    {
      wStep := FImage^.WidthStep;
      Channels := FImage^.nChannels;
      data := Pointer(FImage^.imageData);
      for i := 0 to FImage^.Height - 1 do
      begin
      pb := Result.Scanline[i];
      for j := 0 to FImage^.Width - 1 do
      for K := 0 to Channels - 1 do
      pb[Channels * j + K] := data[i * wStep + j * Channels + K]
      End; }
  End
  else
    Result := NIL;
end;

function TocvImage.Clone: IocvImage;
begin
  Result := TocvImage.CreateClone(FImage);
end;

constructor TocvImage.Create(const AImage: pIplImage);
begin
  Create;
  FImage := AImage;
end;

constructor TocvImage.Create(const Bitmap: TBitmap);
begin
  Create(BitmapToIplImage(Bitmap));
end;

constructor TocvImage.Create;
begin
  FocvCanvas := TocvCanvas.Create(Self);
end;

constructor TocvImage.CreateClone(const AImage: pIplImage);
begin
  Create;
  FImage := cvCloneImage(AImage);
end;

function TocvImage.Crop(const roi: TcvRect): IocvImage;
Var
  CropIplImage: pIplImage;
begin
  CropIplImage := cvCreateImage(cvSize(roi.Width, roi.Height), FImage^.depth, FImage^.nChannels);
  cvSetImageROI(FImage, roi);
  cvCopy(FImage, CropIplImage);
  cvResetImageROI(FImage);
  Result := TocvImage.Create(CropIplImage);
end;

destructor TocvImage.Destroy;
begin
  // FocvCanvas.Free;
  cvReleaseImage(FImage);
  inherited;
end;

function TocvImage.GetCanvas: IocvCanvas;
begin
  Result := FocvCanvas as IocvCanvas;
end;

function TocvImage.GetPixel(const x, y: Integer): TocvPixel;
begin
  FillChar(Result, SizeOf(Result), 0);
  if FImage.colorModel = cmRGB then
  begin
    Result.c := ceRGB;
    if FImage.channelSeq = cmRGB then
    begin
      Result.R := byte(CV_IMAGE_ELEM(FImage, SizeOf(byte), y, (x * FImage^.nChannels) + 0)^);
      Result.G := byte(CV_IMAGE_ELEM(FImage, SizeOf(byte), y, (x * FImage^.nChannels) + 1)^);
      Result.B := byte(CV_IMAGE_ELEM(FImage, SizeOf(byte), y, (x * FImage^.nChannels) + 2)^);
    end
    else
    begin
      Result.B := byte(CV_IMAGE_ELEM(FImage, SizeOf(byte), y, (x * FImage^.nChannels) + 0)^);
      Result.G := byte(CV_IMAGE_ELEM(FImage, SizeOf(byte), y, (x * FImage^.nChannels) + 1)^);
      Result.R := byte(CV_IMAGE_ELEM(FImage, SizeOf(byte), y, (x * FImage^.nChannels) + 2)^);
    end;
  end
  else if FImage.colorModel = cmGRAY then
  begin
    Result.c := ceGRAY;
    Result.GV := byte(CV_IMAGE_ELEM(FImage, SizeOf(byte), y, (x * FImage^.nChannels))^);
  end;
end;

procedure TocvImage.SetPixel(const x, y: Integer; const P: TocvPixel);
begin
  case P.c of
    ceRGB:
      begin
        if FImage.channelSeq = cmRGB then
        begin
          byte(CV_IMAGE_ELEM(FImage, SizeOf(byte), y, (x * FImage^.nChannels) + 0)^) := P.R;
          byte(CV_IMAGE_ELEM(FImage, SizeOf(byte), y, (x * FImage^.nChannels) + 1)^) := P.G;
          byte(CV_IMAGE_ELEM(FImage, SizeOf(byte), y, (x * FImage^.nChannels) + 2)^) := P.B;
        end
        else
        begin
          byte(CV_IMAGE_ELEM(FImage, SizeOf(byte), y, (x * FImage^.nChannels) + 0)^) := P.B;
          byte(CV_IMAGE_ELEM(FImage, SizeOf(byte), y, (x * FImage^.nChannels) + 1)^) := P.G;
          byte(CV_IMAGE_ELEM(FImage, SizeOf(byte), y, (x * FImage^.nChannels) + 2)^) := P.R;
        end;
      end;
    ceGRAY:
      byte(CV_IMAGE_ELEM(FImage, SizeOf(byte), y, (x * FImage^.nChannels))^) := P.GV;
  end;
end;

function TocvImage.GetHeight: Integer;
begin
  if Assigned(FImage) then
    Result := FImage^.Height
  else
    Result := 0;
end;

function TocvImage.GetIplImage: pIplImage;
begin
  Result := FImage;
end;

function TocvImage.GetisGray: Boolean;
begin
  Result := FImage^.nChannels = 1;
end;

function TocvImage.GetWidth: Integer;
begin
  if Assigned(FImage) then
    Result := FImage^.Width
  else
    Result := 0;
end;

function TocvImage.GrayImage: IocvImage;
Var
  iImage: pIplImage;
begin
  if isGray then
    Result := Self
  else
  begin
    iImage := cvCreateImage(cvGetSize(FImage), IPL_DEPTH_8U, 1);
    cvCvtColor(FImage, iImage, CV_RGB2GRAY);
    Result := TocvImage.Create(iImage);
  end;
end;

constructor TocvImage.LoadFormFile(const FileName: String);
begin
  FImage := cvLoadImage(PAnsiChar(AnsiString(FileName)));
end;

function TocvImage.Resize(const nW, nH: Integer; const interpolation: Integer = CV_INTER_LINEAR): IocvImage;
Var
  R: pIplImage;
begin
  R := cvCreateImage(cvSize(nW, nH), FImage^.depth, FImage^.nChannels);
  cvResize(FImage, R, interpolation);
  Result := TocvImage.Create(R);
end;

function TocvImage.Same: IocvImage;
begin
  Result := TocvImage.Create(cvCreateImage(cvGetSize(FImage), FImage^.depth, FImage^.nChannels));
end;

function TocvImage.Scale(const dw, dh: Single; const interpolation: Integer = CV_INTER_LINEAR): IocvImage;
begin
  Result := Resize(Trunc(Width * dw), Trunc(Height * dh), interpolation);
end;

{ TocvReceiverList }

procedure TocvReceiverList.Add(Item: IocvDataReceiver);
begin
  inherited Add(Pointer(Item));
end;

procedure TocvReceiverList.Remove(Item: IocvDataReceiver);
begin
  inherited Remove(Pointer(Item));
end;

procedure GetRGBValue(const AColor: TColor; var R, G, B: byte);
Var
  RGBColor: TColor;
begin
  RGBColor := ColorToRGB(AColor);
  R := GetRValue(RGBColor);
  G := GetGValue(RGBColor);
  B := GetBValue(RGBColor);
end;

function ColorToCvRGB(const Color: TColor): TCvScalar;
var
  R, G, B: byte;
begin
  GetRGBValue(Color, R, G, B);
  Result := CV_RGB(R, G, B);
end;

{ TocvCanvas }

procedure TocvCanvas.Circle(const CenterX, CenterY, Radius: Integer; const Color: TColor; const Thickness: Integer;
  const LineType: TocvLineType; const Shift: Integer);
begin
  if Assigned(FOwner) and Assigned(FOwner.FImage) then
    cvCircle(FOwner.FImage, cvPoint(CenterX, CenterY), Radius, ColorToCvRGB(Color), Thickness, cLineType[LineType], Shift);
end;

constructor TocvCanvas.Create(AOwner: TocvImage);
begin
  FOwner := AOwner;
  FocvFont := TocvFont.Create;
end;

destructor TocvCanvas.Destroy;
begin
  // FocvFont.Free;
  inherited;
end;

procedure TocvCanvas.Ellipse(const CenterX, CenterY: Integer; const Axes: TocvRect; const Angle, start_angle, nd_angle: double;
  const Color: TColor; const Thickness: Integer; const LineType: TocvLineType; const Shift: Integer);
begin
  if Assigned(FOwner) and Assigned(FOwner.FImage) then
    cvEllipse(FOwner.FImage, cvPoint(CenterX, CenterY), cvSize({$IFDEF DELPHIXE2_UP}Axes.Width{$ELSE}Axes.Right - Axes.Left{$ENDIF},
{$IFDEF DELPHIXE2_UP}Axes.Height{$ELSE}Axes.Bottom - Axes.Top{$ENDIF}), Angle, start_angle, nd_angle, ColorToCvRGB(Color), Thickness,
      cLineType[LineType], Shift);
end;

procedure TocvCanvas.EllipseBox(const Box: TCvBox2D; const Color: TColor; const Thickness: Integer; const LineType: TocvLineType;
  const Shift: Integer);
begin
  if Assigned(FOwner) and Assigned(FOwner.FImage) then
    cvEllipseBox(FOwner.FImage, Box, ColorToCvRGB(Color), Thickness, cLineType[LineType], Shift);
end;

function TocvCanvas.GetOcvFont: IocvFont;
begin
  Result := FocvFont as IocvFont;
end;

procedure TocvCanvas.Rectangle(const ARect: TocvRect; const Color: TColor; const Thickness: Integer; const LineType: TocvLineType;
  const Shift: Integer);
begin
  Rectangle(ARect.Left, ARect.Top, ARect.Right, ARect.Bottom, Color, Thickness, LineType, Shift);
end;

procedure TocvCanvas.EllipseBox(const Box: TocvRect; const Angle: Single; const Color: TColor; const Thickness: Integer;
  const LineType: TocvLineType; const Shift: Integer);
begin
  EllipseBox(CvBox2D(Box.Left, Box.Top, {$IFDEF DELPHIXE2_UP}Box.Width{$ELSE}Box.Right - Box.Left{$ENDIF},
{$IFDEF DELPHIXE2_UP}Box.Height{$ELSE}Box.Bottom - Box.Top{$ENDIF}, Angle), Color, Thickness, LineType, Shift);
end;

procedure TocvCanvas.Rectangle(const x1, y1, x2, y2: Integer; const Color: TColor; const Thickness: Integer; const LineType: TocvLineType;
  const Shift: Integer);
begin
  if Assigned(FOwner) and Assigned(FOwner.FImage) then
    cvRectangle(FOwner.FImage, cvPoint(x1, y1), cvPoint(x2, y2), ColorToCvRGB(Color), Thickness, cLineType[LineType], Shift);
end;

procedure TocvCanvas.TextOut(const x, y: Integer; const Text: String; const Shadow: Boolean);
Var
  str: pCVChar;
  Font: TCvFont;
begin
  if Assigned(FOwner) and Assigned(FOwner.FImage) then
  begin
    str := @(AnsiString(Text)[1]);
    Font := ocvFont.cvFont;
    if Shadow then
      cvPutText(FOwner.FImage, str, cvPoint(x - 1, y - 1), @Font, CV_RGB(0, 0, 0));
    cvPutText(FOwner.FImage, str, cvPoint(x, y), @Font, ColorToCvRGB(ocvFont.Color));
  end;
end;

{ TocvFont }

constructor TocvFont.Create;
begin
  inherited;
  FillChar(FCvFont, SizeOf(FCvFont), 0);
  FCvFont.HScale := 0.5;
  FCvFont.VScale := 0.5;
  FCvFont.Thickness := 1;
  FFontLineType := LT_8;
  FFontColor := clRed;
  CreateOcvFont;
end;

procedure TocvFont.CreateOcvFont;
begin
  cvInitFont(@FCvFont, CV_FONT_VECTOR0, HScale, VScale, 0, Thickness, cLineType[LineType]);
end;

function TocvFont.GetCvFont: TCvFont;
begin
  Result := FCvFont;
end;

function TocvFont.GetFontColor: TColor;
begin
  Result := FFontColor;
end;

function TocvFont.GetFontHScale: Single;
begin
  Result := FCvFont.HScale;
end;

function TocvFont.GetFontLineType: TocvLineType;
begin
  Result := FFontLineType;
end;

function TocvFont.GetFontName: string;
begin
  Result := String(FCvFont.nameFont);
end;

function TocvFont.GetFontThickness: Integer;
begin
  Result := FCvFont.Thickness;
end;

function TocvFont.GetFontVScale: Single;
begin
  Result := FCvFont.VScale;
end;

procedure TocvFont.SetFontColor(const Value: TColor);
begin
  FFontColor := Value;
  FCvFont.Color := ColorToCvRGB(Value);
  CreateOcvFont;
end;

procedure TocvFont.SetFontHScale(const Value: Single);
begin
  FCvFont.HScale := Value;
  CreateOcvFont;
end;

procedure TocvFont.SetFontLineType(const Value: TocvLineType);
begin
  FFontLineType := Value;
  CreateOcvFont;
end;

procedure TocvFont.SetFontName(const Value: string);
begin
  FCvFont.nameFont := pCVChar(AnsiString(Value));
  CreateOcvFont;
end;

procedure TocvFont.SetFontThickness(const Value: Integer);
begin
  FCvFont.Thickness := Value;
  CreateOcvFont;
end;

procedure TocvFont.SetFontVScale(const Value: Single);
begin
  FCvFont.VScale := Value;
  CreateOcvFont;
end;

{ TocvRectHelper }

function TocvRectHelper.cvRect: TcvRect;
// Var
// R: TcvRect;
begin
  Result.x := Left;
  Result.y := Top;
  Result.Width := {$IFDEF DELPHIXE2_UP}Width{$ELSE}Right - Left{$ENDIF};
  Result.Height := {$IFDEF DELPHIXE2_UP}Height{$ELSE}Bottom - Top{$ENDIF};
end;

end.
