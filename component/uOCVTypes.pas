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

unit uOCVTypes;

interface

Uses
  System.SysUtils,
  System.Classes,
  System.SyncObjs,
  System.Generics.Collections,
  core.types_c;

Type
  IocvDataReceiver = interface
    ['{F67DEC9E-CCE0-49D2-AB9B-AD7E1020C5DC}']
    procedure TakeImage(const IplImage: pIplImage);
    procedure SetVideoSource(const Value: TObject);
  end;

  IocvDataSource = interface
    ['{80640C0A-6828-42F8-83E7-DA5FD9036DFF}']
    procedure SetReceiver(const OpenCVVideoReceiver: IocvDataReceiver);
    function GetName: string;
  end;

  TocvReceiverList = TList<IocvDataReceiver>;

  TocvDataSource = class(TComponent, IocvDataSource)
  private
    ReceiverCS: TCriticalSection;
  protected
    FOpenCVVideoReceiver: IocvDataReceiver;
    procedure NotifyReceiver(const IplImage: pIplImage); virtual;
    function GetName: string; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetReceiver(const OpenCVVideoReceiver: IocvDataReceiver); virtual;
  end;

  TocvDataReceiver = class(TComponent, IocvDataReceiver)
  private
    FocvVideoSource: IocvDataSource;
  protected
    procedure TakeImage(const IplImage: pIplImage); virtual;
    procedure SetVideoSource(const Value: TObject); virtual;
    procedure SetOpenCVVideoSource(const Value: IocvDataSource); virtual;
  public
    destructor Destroy; override;
  published
    property VideoSource: IocvDataSource Read FocvVideoSource write SetOpenCVVideoSource;
  end;

  TocvDataSourceAndReceiver = class(TocvDataSource, IocvDataReceiver)
  private
    FocvVideoSource: IocvDataSource;
  protected
    procedure TakeImage(const IplImage: pIplImage); virtual;
    procedure SetVideoSource(const Value: TObject); virtual;
    procedure SetOpenCVVideoSource(const Value: IocvDataSource); virtual;
  public
    destructor Destroy; override;
  published
    property VideoSource: IocvDataSource Read FocvVideoSource write SetOpenCVVideoSource;
  end;

implementation

{ TOpenCVDataSource }

procedure TocvDataSource.SetReceiver(const OpenCVVideoReceiver: IocvDataReceiver);
begin
  ReceiverCS.Enter;
  try
    FOpenCVVideoReceiver := OpenCVVideoReceiver;
  finally
    ReceiverCS.Leave;
  end;
end;

constructor TocvDataSource.Create(AOwner: TComponent);
begin
  inherited;
  ReceiverCS := TCriticalSection.Create;
end;

destructor TocvDataSource.Destroy;
begin
  ReceiverCS.Free;
  inherited;
end;

function TocvDataSource.GetName: string;
begin
  Result := Name;
end;

procedure TocvDataSource.NotifyReceiver(const IplImage: pIplImage);
begin
  if Assigned(FOpenCVVideoReceiver) then
    FOpenCVVideoReceiver.TakeImage(IplImage);
end;

{ TOpenCVDataSourceAndReceiver }

destructor TocvDataSourceAndReceiver.Destroy;
begin
  if Assigned(FocvVideoSource) then
    FocvVideoSource.SetReceiver(nil);
  inherited;
end;

procedure TocvDataSourceAndReceiver.SetOpenCVVideoSource(const Value: IocvDataSource);
begin
  if (FocvVideoSource <> Value) then
  begin
    if Assigned(FocvVideoSource) then
      FocvVideoSource.SetReceiver(nil);
    FocvVideoSource := Value;
    if Assigned(FocvVideoSource) then
      FocvVideoSource.SetReceiver(Self);
  end;
end;

procedure TocvDataSourceAndReceiver.SetVideoSource(const Value: TObject);
begin
  VideoSource := Value as TocvDataSource;
end;

procedure TocvDataSourceAndReceiver.TakeImage(const IplImage: pIplImage);
begin

end;

{ TocvDataReceiver }

destructor TocvDataReceiver.Destroy;
begin
  if Assigned(FocvVideoSource) then
    FocvVideoSource.SetReceiver(nil);
  inherited;
end;

procedure TocvDataReceiver.SetOpenCVVideoSource(const Value: IocvDataSource);
begin
  if (FocvVideoSource <> Value) then
  begin
    if Assigned(FocvVideoSource) then
      FocvVideoSource.SetReceiver(nil);
    FocvVideoSource := Value;
    if Assigned(FocvVideoSource) then
      FocvVideoSource.SetReceiver(Self);
  end;
end;

procedure TocvDataReceiver.SetVideoSource(const Value: TObject);
begin
  VideoSource := Value as TocvDataSource;
end;

procedure TocvDataReceiver.TakeImage(const IplImage: pIplImage);
begin

end;

end.
