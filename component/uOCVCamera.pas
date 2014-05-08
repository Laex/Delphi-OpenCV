(* /*****************************************************************
  //                       Delphi-OpenCV Demo
  //               Copyright (C) 2013 Project Delphi-OpenCV
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
  ********************************************************************)

unit uOCVCamera;

interface

uses
  System.SysUtils,
  System.Classes,
  core.types_c,
  highgui_c,
  uOCVTypes;

type
  TocvCameraCaptureSource =
  //
    (CAP_ANY {= 0} , // autodetect
    CAP_CAM_0 {=0} , //
    CAP_CAM_1 {=1} , //
    CAP_CAM_2 {=2} , //
    CAP_CAM_3 {=3} , //
    CAP_CAM_4 {=4} , //
    CAP_CAM_5 {=5} , //
    CAP_MIL {= 100} , // MIL proprietary drivers
    CAP_VFW {= 200} , // platform native
    CAP_V4L {= 200} , //
    CAP_V4L2 {= 200} , //
    CAP_FIREWARE {= 300} , // IEEE 1394 drivers
    CAP_FIREWIRE {= 300} , //
    CAP_IEEE1394 {= 300} , //
    CAP_DC1394 {= 300} , //
    CAP_CMU1394 {= 300} , //
    CAP_STEREO {= 400} , // TYZX proprietary drivers
    CAP_TYZX {= 400} , //
    TYZX_LEFT {= 400} , //
    TYZX_RIGHT {= 401} , //
    TYZX_COLOR {= 402} , //
    TYZX_Z {= 403} , //
    CAP_QT {= 500} , // QuickTime
    CAP_UNICAP {= 600} , // Unicap drivers
    CAP_DSHOW {= 700} , // DirectShow (via videoInput)
    CAP_PVAPI {= 800} , // PvAPI, Prosilica GigE SDK
    CAP_OPENNI {= 900} , // OpenNI (for Kinect)
    CAP_OPENNI_ASUS {= 910} , // OpenNI (for Asus Xtion)
    CAP_ANDROID {= 1000} , // Android
    CAP_XIAPI {= 1100} , // XIMEA Camera API
    CAP_AVFOUNDATION {= 1200} );

type
  TocvCameraThread = class(TThread)
  private const
    ThreadSleepConst = 10;
  private
    FOnNotifyData: TOnOcvNotify;
  protected
    FCapture: pCvCapture;
    procedure Execute; override;
  public
    property OnNotifyData: TOnOcvNotify Read FOnNotifyData write FOnNotifyData;
  end;

  TocvResolution = (r160x120, r320x240, r424x240, r640x360, r800x448, r960x544, r1280x720);

  TocvCamera = class(TocvDataSource)
  private
    FEnabled: Boolean;
    FCameraCaptureSource: TocvCameraCaptureSource;
    FResolution: TocvResolution;
    FOnImage: TOnOcvNotify;
    procedure SetEnabled(const Value: Boolean);
    procedure SetCameraCaptureSource(const Value: TocvCameraCaptureSource);
    procedure SetResolution(const Value: TocvResolution);
    procedure TerminateCameraThread;
    procedure ReleaseCamera;
    procedure SetCameraResolution;
  protected
    FCapture: pCvCapture;
    FOpenCVCameraThread: TocvCameraThread;
    procedure OnNotifyData(Sender: TObject; const IplImage: IocvImage);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Enabled: Boolean Read FEnabled write SetEnabled default False;
    property CameraCaptureSource: TocvCameraCaptureSource read FCameraCaptureSource write SetCameraCaptureSource default CAP_ANY;
    property Resolution: TocvResolution read FResolution write setResolution;
    property OnImage: TOnOcvNotify read FOnImage write FOnImage;
  end;

implementation

uses
  core_c;

const
  ocvCameraCaptureSource: array [TocvCameraCaptureSource] of Longint =
  //
    (CV_CAP_ANY, // autodetect
    CV_CAP_CAM_0, //
    CV_CAP_CAM_1, //
    CV_CAP_CAM_2, //
    CV_CAP_CAM_3, //
    CV_CAP_CAM_4, //
    CV_CAP_CAM_5, //
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

Type
  TCameraResolution = record
    cWidth, cHeight: Integer;
  end;

Const
  CameraResolution: array [TocvResolution] of TCameraResolution = ((cWidth: 160; cHeight: 120), (cWidth: 320; cHeight: 240),
    (cWidth: 424; cHeight: 240), (cWidth: 640; cHeight: 360), (cWidth: 800; cHeight: 448), (cWidth: 960; cHeight: 544),
    (cWidth: 1280; cHeight: 720));

  {TOpenCVCameraThread}

procedure TocvCameraThread.Execute;
Var
  frame: pIplImage;
begin
  while not Terminated do
    if Assigned(FCapture) then
    begin
      try
        frame := cvQueryFrame(FCapture);
        if Assigned(frame) then
        begin
          if Assigned(OnNotifyData) then
            Synchronize(
              procedure
              begin
                OnNotifyData(Self, TocvImage.CreateCopy(frame));
              end);
          Sleep(ThreadSleepConst);
        end;
      except
      end;
    end
    else
      Suspend;
end;

{TOpenCVCamera}

constructor TocvCamera.Create(AOwner: TComponent);
begin
  inherited;
  if not(csDesigning in ComponentState) then
  begin
    FOpenCVCameraThread := TocvCameraThread.Create(True);
    FOpenCVCameraThread.OnNotifyData := OnNotifyData;
    FEnabled := False;
    FResolution := r160x120;
  end;
end;

destructor TocvCamera.Destroy;
begin
  TerminateCameraThread;
  ReleaseCamera;
  inherited;
end;

procedure TocvCamera.ReleaseCamera;
begin
  if Assigned(FCapture) then
  begin
    cvReleaseCapture(FCapture);
    FCapture := nil;
  end;
end;

procedure TocvCamera.TerminateCameraThread;
begin
  if Assigned(FOpenCVCameraThread) then
  begin
    FOpenCVCameraThread.Terminate;
    FOpenCVCameraThread.Resume;
    FOpenCVCameraThread.WaitFor;
    FOpenCVCameraThread.Free;
    FOpenCVCameraThread := Nil;
  end;
end;

procedure TocvCamera.OnNotifyData(Sender: TObject; const IplImage: IocvImage);
begin
  if Assigned(OnImage) then
    OnImage(Self, IplImage);
  NotifyReceiver(IplImage);
end;

procedure TocvCamera.SetCameraCaptureSource(const Value: TocvCameraCaptureSource);
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

procedure TocvCamera.SetEnabled(const Value: Boolean);
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
        FCapture := cvCreateCameraCapture(ocvCameraCaptureSource[FCameraCaptureSource]);
        SetCameraResolution;
        FOpenCVCameraThread.FCapture := FCapture;
        FOpenCVCameraThread.Resume;
      end;

    end;
    FEnabled := Value;
  end;
end;

procedure TocvCamera.SetCameraResolution;
begin
  cvSetCaptureProperty(FCapture, CV_CAP_PROP_FRAME_WIDTH, CameraResolution[FResolution].cWidth);
  cvSetCaptureProperty(FCapture, CV_CAP_PROP_FRAME_HEIGHT, CameraResolution[FResolution].cHeight);
end;

procedure TocvCamera.SetResolution(const Value: TocvResolution);
begin
  if FResolution <> Value then
  begin
    FResolution := Value;
    if Enabled then
    begin
      Enabled := False;
      Enabled := True;
    end;
  end;
end;

end.
