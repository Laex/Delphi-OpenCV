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

unit uOCVView;

interface

uses
  WinApi.Windows,
  WinApi.Messages,
  System.SysUtils,
  System.Classes,
  Vcl.Controls,
  uOCVTypes,
  core.types_c;

type
  TocvView = class(TWinControl, IocvDataReceiver)
  private
    FocvVideoSource: IocvDataSource;
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure SetOpenCVVideoSource(const Value: IocvDataSource);
  protected
    procedure TakeImage(const IplImage: pIplImage);
    procedure SetVideoSource(const Value: TObject);
  public
    destructor Destroy; override;
  published
    property VideoSource: IocvDataSource Read FocvVideoSource write SetOpenCVVideoSource;
    property Align;
  end;

implementation

Uses
  cvUtils;

{ TOpenCVView }

destructor TocvView.Destroy;
begin
  if Assigned(FocvVideoSource) then
    FocvVideoSource.SetReceiver(nil);
  inherited;
end;

procedure TocvView.SetOpenCVVideoSource(const Value: IocvDataSource);
begin
  if FocvVideoSource <> Value then
  begin
    if Assigned(FocvVideoSource) then
      FocvVideoSource.SetReceiver(nil);
    FocvVideoSource := Value;
    if Assigned(FocvVideoSource) then
      FocvVideoSource.SetReceiver(Self);
  end;
end;

procedure TocvView.SetVideoSource(const Value: TObject);
begin
  VideoSource := Value as TocvDataSource;
end;

procedure TocvView.TakeImage(const IplImage: pIplImage);
begin
  if not(csDestroying in ComponentState) then
    ipDraw(
      GetDC(Handle),
      IplImage,
      ClientRect);
end;

procedure TocvView.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
  if (csDesigning in ComponentState) then
    inherited;
end;

procedure TocvView.WMPaint(var Message: TWMPaint);
begin
  if (csDesigning in ComponentState) then
    inherited
  else
    DefaultHandler(Message);
end;

end.
