// ****************************************************************
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
// ****************************************************************
unit ocv.comp.ViewFMX;

{$I OpenCV.inc}

interface

Uses
  System.Classes,
  System.Types,
  FMX.Graphics,
  FMX.Controls,
  ocv.comp.Types;

Type
  TocvViewFMX = class(TControl, IocvDataReceiver)
  private
    FStretch: Boolean;
    FocvVideoSource: IocvDataSource;
    FCenter: Boolean;
    FProportional: Boolean;
    FImage: IocvImage;
    FOnAfterPaint: TOnOcvAfterViewPaint;
    FOnBeforePaint: TOnOcvNotify;
    procedure SetOpenCVVideoSource(const Value: IocvDataSource);
    function isSourceEnabled: Boolean;
    function PaintRect: TRectF;
  protected
    BackBuffer: TBitmap;
    procedure Paint; override;
    procedure TakeImage(const IplImage: IocvImage);
    procedure SetVideoSource(const Value: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DrawImage(const IplImage: IocvImage);
  published
    property VideoSource: IocvDataSource Read FocvVideoSource write SetOpenCVVideoSource;
    property Proportional: Boolean read FProportional write FProportional default false;
    property Stretch: Boolean read FStretch write FStretch default True;
    property Center: Boolean read FCenter write FCenter default false;

    property Left;
    property Top;
    property Width;
    property Height;
    property Align;
    property OnAfterPaint: TOnOcvAfterViewPaint read FOnAfterPaint write FOnAfterPaint;
    property OnBeforePaint: TOnOcvNotify read FOnBeforePaint write FOnBeforePaint;
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
    property DesignVisible;
    property Opacity;
    property Margins;
    property Padding;
    property Position;
  end;

implementation

Uses
  System.UITypes,
  FMX.Types;

{ TocvVewFMX }

constructor TocvViewFMX.Create(AOwner: TComponent);
begin
  inherited;
  BackBuffer := TBitmap.Create;
  BackBuffer.PixelFormat := TPixelFormat.RGB;
  Stretch := True;
  Proportional := false;
  Center := false;
end;

destructor TocvViewFMX.Destroy;
begin
  BackBuffer.Free;
  inherited;
end;

procedure TocvViewFMX.DrawImage(const IplImage: IocvImage);
begin
  FImage := IplImage;
  Repaint;
end;

function TocvViewFMX.isSourceEnabled: Boolean;
begin
  Result := (Assigned(VideoSource) and (VideoSource.Enabled)) or Assigned(FImage);
end;

procedure TocvViewFMX.Paint;
var
  M: TBitmapData;
  i: integer;
  SrcData, DestData: pByte;
  nC: integer;
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

    BackBuffer.Canvas.BeginScene;
    if (BackBuffer.Width <> FImage.Width) or (BackBuffer.Height <> FImage.Height) then
      BackBuffer.SetSize(FImage.Width, FImage.Height);
    if BackBuffer.Map(TMapAccess.maWrite, M) then
      try
        SrcData := pByte(FImage.IpImage^.imageData);
        DestData := pByte(M.Data);
        nC := FImage.IpImage^.nChannels;
        for i := 0 to M.Width * M.Height - 1 do
        begin
          DestData[i * PixelFormatBytes[BackBuffer.PixelFormat] + 0] := SrcData[i * nC + 0];
          DestData[i * PixelFormatBytes[BackBuffer.PixelFormat] + 1] := SrcData[i * nC + 1];
          DestData[i * PixelFormatBytes[BackBuffer.PixelFormat] + 2] := SrcData[i * nC + 2];
          DestData[i * PixelFormatBytes[BackBuffer.PixelFormat] + 3] := $FF;
        end;
      finally
        BackBuffer.Unmap(M);
      end;
    BackBuffer.Canvas.EndScene;
    Canvas.DrawBitmap(BackBuffer, RectF(0, 0, BackBuffer.Width, BackBuffer.Height), PaintRect, 1, True);

    // Canvas.Stroke.Thickness := 1;
    // Canvas.Stroke.Kind := TBrushKind.bkSolid;
    // Canvas.Stroke.Color := TAlphaColorRec.Black;
    // Canvas.DrawRect(RectF(0, 0, Width, Height), 0, 0, AllCorners, 1);

    if Assigned(OnAfterPaint) then
      OnAfterPaint(Self, FImage);

  end;
end;

function TocvViewFMX.PaintRect: TRectF;
var
  ViewWidth, ViewHeight, CliWidth, CliHeight: integer;
  AspectRatio: Double;
begin
  ViewWidth := FImage.IpImage^.Width;
  ViewHeight := FImage.IpImage^.Height;
  CliWidth := Trunc(Width);
  CliHeight := Trunc(Height);
  if (Proportional and ((ViewWidth > CliWidth) or (ViewHeight > CliHeight))) or Stretch then
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
    OffsetRect(Result, (CliWidth - ViewWidth) div 2, (CliHeight - ViewHeight) div 2);
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
