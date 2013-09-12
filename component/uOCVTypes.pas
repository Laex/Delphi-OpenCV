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
    procedure AddReceiver(const OpenCVVideoReceiver: IocvDataReceiver);
    procedure RemoveReceiver(const OpenCVVideoReceiver: IocvDataReceiver);
  end;

  TocvReceiverList = TList<IocvDataReceiver>;

  TocvDataSource = class(TComponent, IocvDataSource)
  private
    ReceiverCS: TCriticalSection;
  protected
    FReceiverList: TocvReceiverList;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AddReceiver(const OpenCVVideoReceiver: IocvDataReceiver); virtual;
    procedure RemoveReceiver(const OpenCVVideoReceiver: IocvDataReceiver); virtual;
    procedure DisconnectRecipients; virtual;
    procedure NotifyRecipients(const IplImage: pIplImage); virtual;
  end;

  TocvDataSourceAndReceiver = class(TocvDataSource, IocvDataReceiver)
  private
    FocvVideoSource: TocvDataSource;
    procedure SetOpenCVVideoSource(const Value: TocvDataSource); virtual;
  protected
    procedure TakeImage(const IplImage: pIplImage); virtual;
    procedure SetVideoSource(const Value: TObject); virtual;
    destructor Destroy; override;
  published
    property VideoSource: TocvDataSource Read FocvVideoSource write SetOpenCVVideoSource;
  end;

implementation

{ TOpenCVDataSource }

procedure TocvDataSource.AddReceiver(const OpenCVVideoReceiver: IocvDataReceiver);
begin
  ReceiverCS.Enter;
  try
    if FReceiverList.IndexOf(OpenCVVideoReceiver) = -1 then
      FReceiverList.Add(OpenCVVideoReceiver);
  finally
    ReceiverCS.Leave;
  end;
end;

constructor TocvDataSource.Create(AOwner: TComponent);
begin
  inherited;
  FReceiverList := TocvReceiverList.Create;
  ReceiverCS := TCriticalSection.Create;
end;

destructor TocvDataSource.Destroy;
begin
  DisconnectRecipients;
  FReceiverList.Free;
  ReceiverCS.Free;
  inherited;
end;

procedure TocvDataSource.DisconnectRecipients;
Var
  I: IocvDataReceiver;
begin
  ReceiverCS.Enter;
  try
    for I in FReceiverList do
      I.SetVideoSource(nil);
  finally
    ReceiverCS.Leave;
  end;
end;

procedure TocvDataSource.NotifyRecipients(const IplImage: pIplImage);
Var
  I: Integer;
begin
  for I := 0 to FReceiverList.Count - 1 do
    FReceiverList[I].TakeImage(IplImage);
end;

procedure TocvDataSource.RemoveReceiver(const OpenCVVideoReceiver: IocvDataReceiver);
begin
  ReceiverCS.Enter;
  try
    if FReceiverList.IndexOf(OpenCVVideoReceiver) <> -1 then
      FReceiverList.Remove(OpenCVVideoReceiver);
  finally
    ReceiverCS.Leave;
  end;
end;

{ TOpenCVDataSourceAndReceiver }

destructor TocvDataSourceAndReceiver.Destroy;
begin
if Assigned(FocvVideoSource) then
    FocvVideoSource.RemoveReceiver(Self);
  inherited;
end;

procedure TocvDataSourceAndReceiver.SetOpenCVVideoSource(const Value: TocvDataSource);
begin
  if (FocvVideoSource <> Value) and (Value <> Self) then
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

procedure TocvDataSourceAndReceiver.TakeImage(const IplImage: pIplImage);
begin

end;

end.
