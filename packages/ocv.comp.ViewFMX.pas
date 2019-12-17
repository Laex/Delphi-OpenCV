(*
  ****************************************************************
  Delphi-OpenCV Demo
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
  ****************************************************************
*)

unit ocv.comp.ViewFMX;

{$I OpenCV.inc}

interface

uses
  System.Classes,
  System.Types,
  System.SyncObjs,
  FMX.Types,
{$IFDEF DELPHIXE2_UP} // Delphi XE6 and above
  FMX.Graphics,
{$ELSE}
  FMX.PixelFormats,
{$ENDIF}
  FMX.Controls, ocv.comp.Types;

{$IFNDEF DELPHIXE5_UP}

// Delphi XE5 and below
const
  PixelFormatBytes: array [TPixelFormat] of Integer = (
    { pfUnknown } 0,
    { pfA16B16G16R16 } 8,
    { pfA2R10G10B10 } 4,
    { pfA2B10G10R10 } 4,
    { pfA8R8G8B8 } 4,
    { pfX8R8G8B8 } 4,
    { pfA8B8G8R8 } 4,
    { pfX8B8G8R8 } 4,
    { pfR5G6B5 } 2,
    { pfA4R4G4B4 } 2,
    { pfA1R5G5B5 } 2,
    { pfX1R5G5B5 } 2,
    { pfG16R16 } 4,
    { pfA8L8 } 2,
    { pfA4L4 } 1,
    { pfL16 } 2,
    { pfL8 } 1,
    { pfR16F } 2,
    { pfG16R16F } 4,
    { pfA16B16G16R16F } 8,
    { pfR32F } 4,
    { pfG32R32F } 8,
    { pfA32B32G32R32F } 16,
    { pfA8 } 1,
    { pfV8U8 } 2,
    { pfL6V5U5 } 2,
    { pfX8L8V8U8 } 4,
    { pfQ8W8V8U8 } 4,
    { pfV16U16 } 4,
    { pfA2W10V10U10 } 4,
    { pfU8Y8_V8Y8 } 4,
    { pfR8G8_B8G8 } 4,
    { pfY8U8_Y8V8 } 4,
    { pfG8R8_G8B8 } 4,
    { pfQ16W16V16U16 } 8,
    { pfCxV8U8 } 2,
    { pfDXT1 } 8,
    { pfDXT2 } 16,
    { pfDXT3 } 16,
    { pfDXT4 } 16,
    { pfDXT5 } 16,
    { pfA32B32G32R32 } 16,
    { pfB10G11R11F } 4);
{$ENDIF}

type
{$IFNDEF DELPHIXE5_UP} // Delphi XE5 and below
  TBitmapHack = class helper for TBitmap
  public
    procedure SetPixelFormat(Value: TPixelFormat);
  end;
{$ENDIF}

  TocvViewFMX = class(TControl, IocvDataReceiver)
  private
    [weak]
    FocvVideoSource: IocvDataSource;
    FStretch: Boolean;
    FCenter: Boolean;
    FProportional: Boolean;
    FImage: IocvImage;
    FOnAfterPaint: TOnOcvAfterViewPaint;
    FOnBeforePaint: TOnOcvNotify;
    FCS: TCriticalSection;
    procedure SetOpenCVVideoSource(const Value: IocvDataSource);
    function isSourceEnabled: Boolean;
    function PaintRect: TRectF;
  protected
    BackBuffer: TBitmap;
    procedure Paint; override;
    procedure TakeImage(const IplImage: IocvImage);
    procedure SetVideoSource(const Value: TObject);
    procedure ImageLock;
    procedure ImageUnLock;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DrawImage(const IplImage: IocvImage);
  published
    property VideoSource: IocvDataSource Read FocvVideoSource
      write SetOpenCVVideoSource;
    property Proportional: Boolean read FProportional write FProportional
      default false;
    property Stretch: Boolean read FStretch write FStretch default True;
    property Center: Boolean read FCenter write FCenter default false;
    property Left;
    property Top;
    property Width Stored True;
    property Height Stored True;
    property Align;
    property OnAfterPaint: TOnOcvAfterViewPaint read FOnAfterPaint
      write FOnAfterPaint;
    property OnBeforePaint: TOnOcvNotify read FOnBeforePaint
      write FOnBeforePaint;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseUp;
    property OnMouseMove;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseWheel;
    property HitTest;
    property PopupMenu;
    property TabOrder;
    property Visible;
{$IF CompilerVersion<21.0}
    property DesignVisible;
{$IFEND}
    property Opacity;
    property Margins;
    property Padding;
    property Position;
  end;

implementation

uses
  System.UITypes,
  ocv.core.types_c,
  ocv.fmxutils;

{$IFNDEF DELPHIXE5_UP}

// Delphi XE5 and below
procedure TBitmapHack.SetPixelFormat(Value: TPixelFormat);
begin
  Self.FPixelFormat := Value;
end;
{$ENDIF}
{ TocvVewFMX }

constructor TocvViewFMX.Create(AOwner: TComponent);
begin
  inherited;
{$IFDEF DELPHIXE6_UP} // Delphi XE6 and above
  BackBuffer := TBitmap.Create;
{$ENDIF}
{$IFDEF DELPHIXE6} // Delphi XE6
  BackBuffer.PixelFormat := TPixelFormat.RGB;
{$ENDIF}
{$IFNDEF DELPHIXE6_UP} // Delphi XE5 and below
  BackBuffer := TBitmap.Create(0, 0);
  BackBuffer.SetPixelFormat(TPixelFormat.pfX8R8G8B8);
{$ENDIF}
  FCS := TCriticalSection.Create;
  Stretch := True;
  Proportional := false;
  Center := false;
end;

destructor TocvViewFMX.Destroy;
begin
  FCS.Free;
  BackBuffer.Free;
  inherited;
end;

procedure TocvViewFMX.DrawImage(const IplImage: IocvImage);
begin
  ImageLock;
  try
    FImage := IplImage;
  finally
    ImageUnLock;
  end;
  Repaint;
end;

procedure TocvViewFMX.ImageLock;
begin
  // Canvas.Lock;
  FCS.Enter;
end;

procedure TocvViewFMX.ImageUnLock;
begin
  FCS.Leave;
  // Canvas.UnLock;
end;

function TocvViewFMX.isSourceEnabled: Boolean;
begin
  Result := (Assigned(VideoSource) and (VideoSource.Enabled)) or
    Assigned(FImage);
end;

procedure TocvViewFMX.Paint;
begin
  if (csDesigning in ComponentState) then
  begin
    Canvas.Stroke.Thickness := 1;
    Canvas.Stroke.Kind := TBrushKind.bkSolid;
    Canvas.Stroke.Color := TAlphaColorRec.Black;
    Canvas.DrawRect(RectF(0, 0, Width, Height), 0, 0, AllCorners, 1);
  end
  else if (not isSourceEnabled) or (not Assigned(FImage)) then
    inherited
  else
  begin
    if Assigned(OnBeforePaint) then
      OnBeforePaint(Self, FImage);
    ImageLock;
    try
{$IFDEF DELPHIXE5_UP}
      IPLImageToFMXBitmap(FImage.IpImage, BackBuffer);
{$ENDIF}
      Canvas.DrawBitmap(BackBuffer, RectF(0, 0, BackBuffer.Width,
        BackBuffer.Height), PaintRect, 1, True);
    finally
      ImageUnLock;
    end;

    if Assigned(OnAfterPaint) then
      OnAfterPaint(Self, FImage);
  end;
end;

function TocvViewFMX.PaintRect: TRectF;
var
  ViewWidth, ViewHeight, CliWidth, CliHeight: Integer;
  AspectRatio: Double;
begin
  ViewWidth := FImage.IpImage^.Width;
  ViewHeight := FImage.IpImage^.Height;
  CliWidth := Trunc(Width);
  CliHeight := Trunc(Height);
  if (Proportional and ((ViewWidth > CliWidth) or (ViewHeight > CliHeight))) or Stretch
  then
  begin
    if Proportional and (ViewWidth > 0) and (ViewHeight > 0) then
    begin
      AspectRatio := ViewWidth / ViewHeight;
      if ViewWidth > ViewHeight then
      begin
        ViewWidth := CliWidth;
        ViewHeight := Trunc(CliWidth / AspectRatio);
        if ViewHeight > CliHeight then
        begin
          ViewHeight := CliHeight;
          ViewWidth := Trunc(CliHeight * AspectRatio);
        end;
      end
      else
      begin
        ViewHeight := CliHeight;
        ViewWidth := Trunc(CliHeight * AspectRatio);
        if ViewWidth > CliWidth then
        begin
          ViewWidth := CliWidth;
          ViewHeight := Trunc(CliWidth / AspectRatio);
        end;
      end;
    end
    else
    begin
      ViewWidth := CliWidth;
      ViewHeight := CliHeight;
    end;
  end;

  With Result do
  begin
    Left := 0;
    Top := 0;
    Right := ViewWidth;
    Bottom := ViewHeight;
  end;

  if Center then
    OffsetRect(Result, (CliWidth - ViewWidth) div 2,
      (CliHeight - ViewHeight) div 2);
end;

procedure TocvViewFMX.SetOpenCVVideoSource(const Value: IocvDataSource);
begin
  if FocvVideoSource <> Value then
  begin
    if Assigned(FocvVideoSource) then
      FocvVideoSource.RemoveReceiver(Self);
    FocvVideoSource := Value;
    if Assigned(FocvVideoSource) then
      FocvVideoSource.AddReceiver(Self);
  end;
end;

procedure TocvViewFMX.SetVideoSource(const Value: TObject);
begin
  VideoSource := Value as TocvDataSource;
end;

procedure TocvViewFMX.TakeImage(const IplImage: IocvImage);
begin
  if not(csDestroying in ComponentState) then
    DrawImage(IplImage);
end;

end.

