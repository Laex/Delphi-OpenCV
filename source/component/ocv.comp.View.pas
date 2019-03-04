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

{$IFNDEF CLR}
{$I OpenCV.inc}
unit ocv.comp.View;
{$ENDIF}

interface

uses
{$IFDEF HAS_UNITSCOPE}
{$IFDEF MSWINDOWS}
  Winapi.Windows, Winapi.Messages,
{$ENDIF MSWINDOWS}
  System.SysUtils,
  System.Classes,
  Vcl.Controls,
  Vcl.Graphics,
  System.SyncObjs,
{$ELSE}
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF MSWINDOWS}
{$IFNDEF FPC}Messages, {$ENDIF FPC}
  SysUtils,
  Classes,
  Controls,
  Graphics,
  SyncObjs,
{$ENDIF}
  ocv.comp.Types,
  ocv.core.types_c;

type

  TocvViewFrames = class(TOwnedCollection)

  end;

  TocvPersistentRect = class(TPersistent)
  private
    FRect: TRect;
    FOnChange: TNotifyEvent;
    function GetRect: TRect;
    procedure SetRect(const Value: TRect);
    procedure SetRectBottom(const Value: integer);
    procedure SetRectLeft(const Value: integer);
    procedure SetRectRight(const Value: integer);
    procedure SetRectTop(const Value: integer);
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    property AsRect: TRect read GetRect Write SetRect;
    constructor Create; virtual;
  published
    property Left: integer read FRect.Left write SetRectLeft;
    property Top: integer read FRect.Top write SetRectTop;
    property Right: integer read FRect.Right write SetRectRight;
    property Bottom: integer read FRect.Bottom write SetRectBottom;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TocvViewFrame = class(TCollectionItem, IocvDataReceiver)
  private
    [weak] FocvVideoSource: IocvDataSource;
    FImage: IocvImage;
    FLock: TCriticalSection;
    FDrawRect: TocvPersistentRect;
    FEnabled: Boolean;
    procedure SetOpenCVVideoSource(const Value: IocvDataSource);
    function GetImage: IocvImage;
    function Lock: Boolean;
    procedure Unlock;
  protected
    { IInterface }
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: integer; stdcall;
    function _Release: integer; stdcall;
    procedure TakeImage(const IplImage: IocvImage);
    procedure SetVideoSource(const Value: TObject);
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    property Image: IocvImage read GetImage;
  published
    property VideoSource: IocvDataSource Read FocvVideoSource write SetOpenCVVideoSource;
    property DrawRect: TocvPersistentRect read FDrawRect write FDrawRect;
    property Enabled: Boolean read FEnabled Write FEnabled default false;
  end;

  TocvView = class(TWinControl, IocvDataReceiver)
  private
    [weak] FocvVideoSource: IocvDataSource;
    FImage: IocvImage;
    FOnAfterPaint: TOnOcvAfterViewPaint;
    FOnBeforePaint: TOnOcvNotify;
    FCanvas: TCanvas;
    FStretch: Boolean;
    FProportional: Boolean;
    FCenter: Boolean;
    FFrames: TocvViewFrames;
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure SetOpenCVVideoSource(const Value: IocvDataSource);
    function isSourceEnabled: Boolean;
    function PaintRect: TRect;
  protected
    procedure SetVideoSource(const Value: TObject);
    procedure TakeImage(const IplImage: IocvImage);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DrawImage(const IplImage: IocvImage);
    property Canvas: TCanvas read FCanvas;
    property Image: IocvImage read FImage;
  published
    property VideoSource: IocvDataSource Read FocvVideoSource write SetOpenCVVideoSource;
    property Proportional: Boolean read FProportional write FProportional default false;
    property Stretch: Boolean read FStretch write FStretch default True;
    property Center: Boolean read FCenter write FCenter default false;
    property Frames: TocvViewFrames Read FFrames Write FFrames;
    property Align;
    property OnAfterPaint: TOnOcvAfterViewPaint read FOnAfterPaint write FOnAfterPaint;
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

uses
  ocv.utils;

{ TOpenCVView }

constructor TocvView.Create(AOwner: TComponent);
begin
  inherited;
  FCanvas := TControlCanvas.Create;
  TControlCanvas(FCanvas).Control := Self;
  Stretch := True;
  Proportional := false;
  Center := false;
  FFrames := TocvViewFrames.Create(Self, TocvViewFrame);
end;

destructor TocvView.Destroy;
begin
  // if Assigned(FocvVideoSource) then
  // FocvVideoSource.RemoveReceiver(Self);
  VideoSource := nil;
  FCanvas.Free;
  FFrames.Free;
  inherited;
end;

procedure TocvView.SetOpenCVVideoSource(const Value: IocvDataSource);
begin
  if FocvVideoSource <> Value then
  begin
    if Assigned(FocvVideoSource) and (not(csDesigning in ComponentState)) then
      FocvVideoSource.RemoveReceiver(Self);
    FocvVideoSource := Value;
    if Assigned(FocvVideoSource) and (not(csDesigning in ComponentState)) then
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
    DrawImage(IplImage);
end;

function TocvView.PaintRect: TRect;
var
  ViewWidth, ViewHeight, CliWidth, CliHeight: integer;
  AspectRatio: Double;
begin
  ViewWidth := FImage.IpImage^.Width;
  ViewHeight := FImage.IpImage^.Height;
  CliWidth := ClientWidth;
  CliHeight := ClientHeight;
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

  with Result do
  begin
    Left := 0;
    Top := 0;
    Right := ViewWidth;
    Bottom := ViewHeight;
  end;

  if Center then
    OffsetRect(Result, (CliWidth - ViewWidth) div 2, (CliHeight - ViewHeight) div 2);
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
  i: integer;
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
          if ipDraw(DC, FImage.IpImage, PaintRect) then
          begin
            for i := 0 to FFrames.Count - 1 do
              With (FFrames.Items[i] as TocvViewFrame) do
{$IFDEF DELPHIXE2_UP}
                if Enabled and (not DrawRect.AsRect.isEmpty) and Assigned(Image) then
{$ELSE}
                if Enabled and IsRectEmpty(DrawRect.AsRect) and Assigned(Image) then
{$ENDIF}
                  ipDraw(DC, Image.IpImage, DrawRect.AsRect);
            if Assigned(OnAfterPaint) then
              OnAfterPaint(Self, FImage);
          end;
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

{ TocvViewFrame }

constructor TocvViewFrame.Create(Collection: TCollection);
begin
  inherited;
  FLock := TCriticalSection.Create;
  FDrawRect := TocvPersistentRect.Create;
{$IFDEF DELPHIXE2_UP}
  FDrawRect.FRect.Width := 50;
  FDrawRect.FRect.Height := 50;
{$ENDIF}
  FEnabled := false;
end;

destructor TocvViewFrame.Destroy;
begin
  FImage := Nil;
  FLock.Free;
  FDrawRect.Free;
  inherited;
end;

function TocvViewFrame.GetImage: IocvImage;
begin
  FLock.Enter;
  try
    Result := FImage;
  finally
    Unlock;
  end;
end;

function TocvViewFrame.Lock: Boolean;
begin
  Result := FLock.TryEnter;
end;

function TocvViewFrame.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
end;

procedure TocvViewFrame.SetOpenCVVideoSource(const Value: IocvDataSource);
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

procedure TocvViewFrame.SetVideoSource(const Value: TObject);
begin
  VideoSource := Value as TocvDataSource;
end;

procedure TocvViewFrame.TakeImage(const IplImage: IocvImage);
begin
  if Lock then
    try
      FImage := IplImage;
    finally
      Unlock;
    end;
end;

procedure TocvViewFrame.Unlock;
begin
  FLock.Leave;
end;

function TocvViewFrame._AddRef: integer;
begin
  Result := -1;
end;

function TocvViewFrame._Release: integer;
begin
  Result := -1;
end;

{ TPersistentRect }

procedure TocvPersistentRect.AssignTo(Dest: TPersistent);
begin
  if Dest is TocvPersistentRect then
    with TocvPersistentRect(Dest) do
    begin
      AsRect := Self.AsRect;
    end
  else
    inherited AssignTo(Dest);
end;

constructor TocvPersistentRect.Create;
begin
  inherited;
  FOnChange := nil;
end;

function TocvPersistentRect.GetRect: TRect;
begin
  Result := FRect;
end;

procedure TocvPersistentRect.SetRect(const Value: TRect);
begin
  FRect.Left := Value.Left;
  FRect.Top := Value.Top;
  FRect.Right := Value.Right;
  FRect.Bottom := Value.Bottom;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TocvPersistentRect.SetRectBottom(const Value: integer);
begin
  FRect.Bottom := Value;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TocvPersistentRect.SetRectLeft(const Value: integer);
begin
  FRect.Left := Value;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TocvPersistentRect.SetRectRight(const Value: integer);
begin
  FRect.Right := Value;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TocvPersistentRect.SetRectTop(const Value: integer);
begin
  FRect.Top := Value;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

end.
