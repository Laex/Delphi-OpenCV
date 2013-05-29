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

unit uOpenCVView;

interface

uses
  WinApi.Windows,
  WinApi.Messages,
  System.SysUtils,
  System.Classes,
  Vcl.Controls,
  uOpenCVVideoSource,
  uOpenCVVideoReceiver,
  core.types_c;

type
  TOpenCVView = class(TWinControl, IOpenCVVideoReceiver)
  private
    FOpenCVVideoSource: TOpenCVVideoSource;
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure SetOpenCVVideoSource(const Value: TOpenCVVideoSource);
  protected
    procedure TakeImage(const IplImage: pIplImage);
    procedure SetVideoSource(const Value: TObject);
    procedure SetEnabled(Value: Boolean); override;
  public
    destructor Destroy; override;
  published
    property VideoSource: TOpenCVVideoSource Read FOpenCVVideoSource write SetOpenCVVideoSource;
    property Align;
  end;

procedure Register;

implementation

Uses cvUtils;

procedure Register;
begin
  RegisterComponents('OpenCV', [TOpenCVView]);
end;

{ TOpenCVView }

destructor TOpenCVView.Destroy;
begin
  if Assigned(FOpenCVVideoSource) then
    FOpenCVVideoSource.RemoveReceiver(Self);
  inherited;
end;

procedure TOpenCVView.SetEnabled(Value: Boolean);
begin
  inherited;
end;

procedure TOpenCVView.SetOpenCVVideoSource(const Value: TOpenCVVideoSource);
begin
  if FOpenCVVideoSource <> Value then
  begin
    if Assigned(FOpenCVVideoSource) then
    begin
      FOpenCVVideoSource.RemoveReceiver(Self);
    end;
    FOpenCVVideoSource := Value;
    if Assigned(FOpenCVVideoSource) then
    begin
      FOpenCVVideoSource.AddReceiver(Self);
    end;
  end;
end;

procedure TOpenCVView.SetVideoSource(const Value: TObject);
begin
  VideoSource := Value as TOpenCVVideoSource;
end;

procedure TOpenCVView.TakeImage(const IplImage: pIplImage);
begin
  if not(csDestroying in ComponentState) then
    ipDraw(GetDC(Handle), IplImage, ClientRect);
end;

procedure TOpenCVView.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
  // if Enabled then
  // DefaultHandler(Message)
  // else
  if (csDesigning in ComponentState) then
    inherited;
end;

procedure TOpenCVView.WMPaint(var Message: TWMPaint);
begin
  // if Enabled then
  // DefaultHandler(Message)
  // else
  if (csDesigning in ComponentState) then
    inherited;
end;

end.
