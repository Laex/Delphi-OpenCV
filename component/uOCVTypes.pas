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
  System.Generics.Collections,
  core.types_c;

Type
  IocvImage = interface
    ['{84567F57-A399-4179-AA0F-6F8A2788F89B}']
    function GetIplImage: pIplImage;
    function GetisGray: Boolean;
    function GrayImage: IocvImage;
    function Clone: IocvImage;
    property IpImage: pIplImage Read GetIplImage;
    property isGray: Boolean read GetisGray;
  end;

  TocvImage = class(TInterfacedObject, IocvImage)
  private
    FImage: pIplImage;
  protected
    function GetIplImage: pIplImage;
    function GetisGray: Boolean;
  public
    constructor Create(const AImage: pIplImage);
    constructor CreateCopy(const AImage: pIplImage);
    destructor Destroy; override;
    function GrayImage: IocvImage;
    function Clone: IocvImage;
    property IplImage: pIplImage Read GetIplImage;
    property isGray: Boolean read GetisGray;
  end;

  TOnOcvNotify = procedure(Sender: TObject; const IplImage: IocvImage) of object;
  TOnOcvContour = procedure(Sender: TObject; const IplImage: IocvImage; const ContourCount: Integer; const Contours: pCvSeq)
    of object;

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
  end;

  TocvReceiverList = TThreadList<IocvDataReceiver>;

  TocvDataSource = class(TComponent, IocvDataSource)
  protected
    FOpenCVVideoReceivers: TocvReceiverList;
    FImage: IocvImage;
    function GetName: string; virtual;
    procedure NotifyReceiver(const IplImage: IocvImage); virtual;
    function GetImage: IocvImage;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AddReceiver(const OpenCVVideoReceiver: IocvDataReceiver); virtual;
    procedure RemoveReceiver(const OpenCVVideoReceiver: IocvDataReceiver); virtual;
    property Image: IocvImage read GetImage;
  end;

  TocvDataReceiver = class(TComponent, IocvDataReceiver)
  private
    FocvVideoSource: IocvDataSource;
  protected
    procedure TakeImage(const IplImage: IocvImage); virtual;
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
    procedure TakeImage(const IplImage: IocvImage); virtual;
    procedure SetVideoSource(const Value: TObject); virtual;
    procedure SetOpenCVVideoSource(const Value: IocvDataSource); virtual;
  public
    destructor Destroy; override;
  published
    property VideoSource: IocvDataSource Read FocvVideoSource write SetOpenCVVideoSource;
  end;

implementation

uses
  core_c, imgproc_c, imgproc.types_c;

{TOpenCVDataSource}

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

function TocvDataSource.GetImage: IocvImage;
begin
  Result := FImage;
end;

function TocvDataSource.GetName: string;
begin
  Result := Name;
end;

procedure TocvDataSource.NotifyReceiver(const IplImage: IocvImage);
Var
  R: IocvDataReceiver;
  LockList: TList<IocvDataReceiver>;
begin
  LockList := FOpenCVVideoReceivers.LockList;
  try
    FImage := IplImage;
    for R in LockList do
      R.TakeImage(IplImage);
  finally
    FOpenCVVideoReceivers.UnlockList;
  end;
end;

procedure TocvDataSource.RemoveReceiver(const OpenCVVideoReceiver: IocvDataReceiver);
begin
  FOpenCVVideoReceivers.Remove(OpenCVVideoReceiver);
end;

{TOpenCVDataSourceAndReceiver}

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

{TocvDataReceiver}

destructor TocvDataReceiver.Destroy;
begin
  if Assigned(FocvVideoSource) then
    FocvVideoSource.RemoveReceiver(Self);
  inherited;
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

{TocvImage}

function TocvImage.Clone: IocvImage;
begin
  Result := TocvImage.CreateCopy(FImage);
end;

constructor TocvImage.Create(const AImage: pIplImage);
begin
  FImage := AImage;
end;

constructor TocvImage.CreateCopy(const AImage: pIplImage);
begin
  FImage := cvCloneImage(AImage);
end;

destructor TocvImage.Destroy;
begin
  cvReleaseImage(FImage);
  inherited;
end;

function TocvImage.GetIplImage: pIplImage;
begin
  Result := FImage;
end;

function TocvImage.GetisGray: Boolean;
begin
  Result := FImage^.nChannels = 1;
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

end.
