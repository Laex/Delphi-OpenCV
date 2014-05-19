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

{$IFNDEF CLR}

{$I OpenCV.inc}

unit uOCVView;
{$ENDIF}

interface

uses
  {$IFDEF VER6P}
  WinApi.Windows,
  WinApi.Messages,
  System.SysUtils,
  System.Classes,
  Vcl.Controls,
  {$ELSE}
  Windows,
  Messages,
  SysUtils,
  Classes,
  Controls,
  {$ENDIF VER6P}
  uOCVTypes,
  core.types_c, Vcl.Graphics;

type

  TocvView = class(TWinControl, IocvDataReceiver)
  private
    FocvVideoSource: IocvDataSource;
    FImage: IocvImage;
    FOnAfterPaint: TOnOcvNotify;
    FOnBeforePaint: TOnOcvNotify;
    FCanvas: TCanvas;
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure SetOpenCVVideoSource(const Value: IocvDataSource);
    function isSourceEnabled: Boolean;
  protected
    procedure TakeImage(const IplImage: IocvImage);
    procedure SetVideoSource(const Value: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DrawImage(const IplImage: IocvImage);
    property Canvas: TCanvas read FCanvas;
  published
    property VideoSource: IocvDataSource Read FocvVideoSource write SetOpenCVVideoSource;
    property Align;
    property OnAfterPaint: TOnOcvNotify read FOnAfterPaint write FOnAfterPaint;
    property OnBeforePaint: TOnOcvNotify read FOnBeforePaint write FOnBeforePaint;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseUp;
    property OnMouseMove;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseWheel;
    property OnMouseWheelUp;
    property OnMouseWheelDown;
  end;

implementation

Uses
  cvUtils;

{TOpenCVView}

constructor TocvView.Create(AOwner: TComponent);
begin
  inherited;
  FCanvas := TControlCanvas.Create;
  TControlCanvas(FCanvas).Control := Self;
end;

destructor TocvView.Destroy;
begin
  if Assigned(FocvVideoSource) then
    FocvVideoSource.RemoveReceiver(Self);
  FCanvas.Free;
  inherited;
end;

procedure TocvView.SetOpenCVVideoSource(const Value: IocvDataSource);
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

procedure TocvView.SetVideoSource(const Value: TObject);
begin
  VideoSource := Value as TocvDataSource;
end;

procedure TocvView.DrawImage(const IplImage: IocvImage);
begin
  FImage := IplImage;
  Invalidate;
end;

procedure TocvView.TakeImage(const IplImage: IocvImage);
begin
  if not(csDestroying in ComponentState) then
  begin
    // if Assigned(OnBeforePaint) then
    // OnBeforePaint(Self, IplImage);
    DrawImage(IplImage);
    // if Assigned(OnAfterPaint) then
    // OnAfterPaint(Self, IplImage);
  end;
end;

function TocvView.isSourceEnabled: Boolean;
begin
  Result := (Assigned(VideoSource) and (VideoSource.Enabled)) or Assigned(FImage);
end;

procedure TocvView.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
  if (csDesigning in ComponentState) or (not isSourceEnabled) then
    inherited;
end;

procedure TocvView.WMPaint(var Message: TWMPaint);
Var
  DC: HDC;
  lpPaint: TPaintStruct;
begin
  if (csDesigning in ComponentState) or (not isSourceEnabled) then
    inherited
  else
  begin
    if Assigned(FImage) then
    begin
      Canvas.Lock;
      DC := BeginPaint(Handle, lpPaint);
      try
        Canvas.Handle := DC;
        try
          if Assigned(OnBeforePaint) then
            OnBeforePaint(Self, FImage);

          ipDraw(DC, FImage.IpImage, ClientRect);

          if Assigned(OnAfterPaint) then
            OnAfterPaint(Self, FImage);

        finally
          Canvas.Handle := 0;
        end;
      finally
        EndPaint(Handle, lpPaint);
        Canvas.Unlock;
      end;
    end
    else
      DefaultHandler(Message);
  end;
end;

end.
