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

unit uOpenCVCamera;

interface

uses
  System.SysUtils,
  System.Classes,
  System.SyncObjs,
  System.Generics.Collections,
  core.types_c,
  highgui_c,
  uOpenCVVideoReceiver,
  uOpenCVVideoSource;

type
  TOpenCVCameraCaptureSource = //
    (CAP_ANY { = 0 } , // autodetect
    CAP_MIL { = 100 } , // MIL proprietary drivers
    CAP_VFW { = 200 } , // platform native
    CAP_V4L { = 200 } , //
    CAP_V4L2 { = 200 } , //
    CAP_FIREWARE { = 300 } , // IEEE 1394 drivers
    CAP_FIREWIRE { = 300 } , //
    CAP_IEEE1394 { = 300 } , //
    CAP_DC1394 { = 300 } , //
    CAP_CMU1394 { = 300 } , //
    CAP_STEREO { = 400 } , // TYZX proprietary drivers
    CAP_TYZX { = 400 } , //
    TYZX_LEFT { = 400 } , //
    TYZX_RIGHT { = 401 } , //
    TYZX_COLOR { = 402 } , //
    TYZX_Z { = 403 } , //
    CAP_QT { = 500 } , // QuickTime
    CAP_UNICAP { = 600 } , // Unicap drivers
    CAP_DSHOW { = 700 } , // DirectShow (via videoInput)
    CAP_PVAPI { = 800 } , // PvAPI, Prosilica GigE SDK
    CAP_OPENNI { = 900 } , // OpenNI (for Kinect)
    CAP_OPENNI_ASUS { = 910 } , // OpenNI (for Asus Xtion)
    CAP_ANDROID { = 1000 } , // Android
    CAP_XIAPI { = 1100 } , // XIMEA Camera API
    CAP_AVFOUNDATION { = 1200 } );

type

  TReceiverList = TList<IOpenCVVideoReceiver>;

  TOpenCVCameraThread = class(TThread)
  private
    FReceiverList: TReceiverList;
    ReceiverCS: TCriticalSection;
  protected
    FCapture: pCvCapture;
    procedure Execute; override;
    procedure SendVideoData(const IplImage: pIplImage);
    procedure DisconnectRecipients;
  public
    constructor Create(CreateSuspended: Boolean); overload;
    destructor Destroy; override;
    procedure AddReceiver(const OpenCVVideoReceiver: IOpenCVVideoReceiver);
    procedure RemoveReceiver(const OpenCVVideoReceiver: IOpenCVVideoReceiver);
  end;

  TOpenCVCamera = class(TOpenCVVideoSource)
  private
    FEnabled: Boolean;
    FCameraCaptureSource: TOpenCVCameraCaptureSource;
    procedure SetEnabled(const Value: Boolean);
    procedure SetCameraCaptureSource(const Value: TOpenCVCameraCaptureSource);
  protected
    FCapture: pCvCapture;
    FOpenCVCameraThread: TOpenCVCameraThread;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AddReceiver(const OpenCVVideoReceiver: IOpenCVVideoReceiver); override;
    procedure RemoveReceiver(const OpenCVVideoReceiver: IOpenCVVideoReceiver); override;
  published
    property Enabled: Boolean Read FEnabled write SetEnabled default False;
    property CameraCaptureSource: TOpenCVCameraCaptureSource read FCameraCaptureSource write SetCameraCaptureSource
      default CAP_ANY;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('OpenCV', [TOpenCVCamera]);
end;

const
  OpenCVCameraCaptureSource: array [TOpenCVCameraCaptureSource] of Longint = //
    (CV_CAP_ANY, // autodetect
    CV_CAP_MIL, // MIL proprietary drivers
    CV_CAP_VFW, // platform native
    CV_CAP_V4L, //
    CV_CAP_V4L2, //
    CV_CAP_FIREWARE, // IEEE 1394 drivers
    CV_CAP_FIREWIRE, //
    CV_CAP_IEEE1394, //
    CV_CAP_DC1394, //
    CV_CAP_CMU1394, //
    CV_CAP_STEREO, // TYZX proprietary drivers
    CV_CAP_TYZX, //
    CV_TYZX_LEFT, //
    CV_TYZX_RIGHT, //
    CV_TYZX_COLOR, //
    CV_TYZX_Z, //
    CV_CAP_QT, // QuickTime
    CV_CAP_UNICAP, // Unicap drivers
    CV_CAP_DSHOW, // DirectShow (via videoInput)
    CV_CAP_PVAPI, // PvAPI; Prosilica GigE SDK
    CV_CAP_OPENNI, // OpenNI (for Kinect)
    CV_CAP_OPENNI_ASUS, // OpenNI (for Asus Xtion)
    CV_CAP_ANDROID, // Android
    CV_CAP_XIAPI, // XIMEA Camera API
    CV_CAP_AVFOUNDATION);

  ThreadSleepConst = 20;

  { TOpenCVCameraThread }

procedure TOpenCVCameraThread.AddReceiver(const OpenCVVideoReceiver: IOpenCVVideoReceiver);
begin
  if FReceiverList.IndexOf(OpenCVVideoReceiver) = -1 then
    FReceiverList.Add(OpenCVVideoReceiver);
end;

constructor TOpenCVCameraThread.Create(CreateSuspended: Boolean);
begin
  inherited Create(CreateSuspended);
  FReceiverList := TReceiverList.Create;
  ReceiverCS := TCriticalSection.Create;
end;

destructor TOpenCVCameraThread.Destroy;
begin
  DisconnectRecipients;
  FReceiverList.Free;
  ReceiverCS.Free;
  inherited;
end;

procedure TOpenCVCameraThread.DisconnectRecipients;
Var
  I: IOpenCVVideoReceiver;
begin
  ReceiverCS.Enter;
  for I in FReceiverList do
    I.SetVideoSource(nil);
  ReceiverCS.Leave;
end;

procedure TOpenCVCameraThread.Execute;
Var
  frame: pIplImage;
begin
  while not Terminated do
  begin
    if Assigned(FCapture) then
    begin
      frame := cvQueryFrame(FCapture);
      if Assigned(frame) then
      begin
        SendVideoData(frame);
      end;
    end;
  end;
end;

procedure TOpenCVCameraThread.RemoveReceiver(const OpenCVVideoReceiver: IOpenCVVideoReceiver);
begin
  ReceiverCS.Enter;
  if FReceiverList.IndexOf(OpenCVVideoReceiver) <> -1 then
    FReceiverList.Remove(OpenCVVideoReceiver);
  ReceiverCS.Leave;
end;

procedure TOpenCVCameraThread.SendVideoData(const IplImage: pIplImage);
var
  I: Integer;
begin
  ReceiverCS.Enter;
  for I := 0 to FReceiverList.Count - 1 do
    FReceiverList[I].TakeImage(IplImage);
  ReceiverCS.Leave;
end;

{ TOpenCVCamera }

procedure TOpenCVCamera.AddReceiver(const OpenCVVideoReceiver: IOpenCVVideoReceiver);
begin
  FOpenCVCameraThread.AddReceiver(OpenCVVideoReceiver);
end;

constructor TOpenCVCamera.Create(AOwner: TComponent);
begin
  inherited;
  FOpenCVCameraThread := TOpenCVCameraThread.Create(True);
end;

destructor TOpenCVCamera.Destroy;
begin
  FOpenCVCameraThread.DisconnectRecipients;
  FOpenCVCameraThread.FCapture := nil;
  if Assigned(FCapture) then
  cvReleaseCapture(FCapture);
  FOpenCVCameraThread.Terminate;
  FOpenCVCameraThread.WaitFor;
  FOpenCVCameraThread.Free;
  inherited;
end;

procedure TOpenCVCamera.RemoveReceiver(const OpenCVVideoReceiver: IOpenCVVideoReceiver);
begin
  FOpenCVCameraThread.RemoveReceiver(OpenCVVideoReceiver);
end;

procedure TOpenCVCamera.SetCameraCaptureSource(const Value: TOpenCVCameraCaptureSource);
Var
  isEnabled: Boolean;
begin
  if FCameraCaptureSource <> Value then
  begin
    isEnabled := Enabled;
    if Assigned(FCapture) and FEnabled then
      Enabled := False;
    FCameraCaptureSource := Value;
    Enabled := isEnabled;
  end;
end;

procedure TOpenCVCamera.SetEnabled(const Value: Boolean);
begin
  if FEnabled <> Value then
  begin
    if not(csDesigning in ComponentState) then
    begin
      if Assigned(FCapture) and FEnabled then
      begin
        FOpenCVCameraThread.Suspend;
        FOpenCVCameraThread.FCapture := nil;
        cvReleaseCapture(FCapture);
        FCapture := Nil;
      end;
      if Value then
      begin
        FCapture := cvCreateCameraCapture(OpenCVCameraCaptureSource[FCameraCaptureSource]);
        FOpenCVCameraThread.FCapture := FCapture;
        FOpenCVCameraThread.Resume;
      end;
    end;
    FEnabled := Value;
  end;
end;

end.
