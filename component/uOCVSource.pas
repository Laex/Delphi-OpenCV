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

unit uOCVSource;

interface

uses
  System.SysUtils,
  System.Classes,
  System.SyncObjs,
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

  TocvCustomSourceThread = class(TThread)
  private
    FOnNotifyData: TOnOcvNotify;
    FOnNoData: TNotifyEvent;
    FThreadDelay: Integer;
    FLock: TCriticalSection;
    procedure SetCapture(const Value: pCvCapture); virtual;
  protected
    FCapture: pCvCapture;
  public
    constructor Create(CreateSuspended: Boolean);
    destructor Destroy; override;
    property OnNoData: TNotifyEvent Read FOnNoData write FOnNoData;
    property OnNotifyData: TOnOcvNotify Read FOnNotifyData write FOnNotifyData;
    property Capture: pCvCapture read FCapture write SetCapture;
  end;

  TocvCaptureThread = class(TocvCustomSourceThread)
  protected
    procedure Execute; override;
  end;

  TocvCustomSource = class(TocvDataSource)
  protected
    FCapture: pCvCapture;
    FSourceThread: TocvCustomSourceThread;
    FThreadDelay: Integer;
    procedure OnNotifyData(Sender: TObject; const IplImage: IocvImage);
    procedure SetEnabled(Value: Boolean); virtual;
    procedure Loaded; override;
    function GetEnabled: Boolean; override;
  private
    FEnabled: Boolean;
    FOnImage: TOnOcvNotify;
    procedure TerminateSourceThread;
    procedure ReleaseSource;
    function GetHeight: Integer; virtual;
    function GetWidth: Integer; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Enabled: Boolean Read GetEnabled write SetEnabled default False;
    property OnImage: TOnOcvNotify read FOnImage write FOnImage;
    property Width: Integer Read GetWidth;
    property Height: Integer Read GetHeight;
  end;

  TocvResolution = (r160x120, r320x240, r424x240, r640x360, r800x448, r960x544, r1280x720);

  TocvCameraSource = class(TocvCustomSource)
  protected
    procedure SetEnabled(Value: Boolean); override;
  private
    FCaptureSource: TocvCameraCaptureSource;
    FResolution: TocvResolution;
    procedure SetCameraSource(const Value: TocvCameraCaptureSource);
    procedure SetResolution(const Value: TocvResolution);
    procedure SetCameraResolution;
    function GetHeight: Integer; override;
    function GetWidth: Integer; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Camera: TocvCameraCaptureSource read FCaptureSource write SetCameraSource default CAP_ANY;
    property Resolution: TocvResolution read FResolution write SetResolution;
  end;

  TocvFileSource = class(TocvCustomSource)
  protected
    procedure SetEnabled(Value: Boolean); override;
    procedure OnNoData(Sender: TObject);
  private
    FFileName: TFileName;
    FLoop: Boolean;
    FOnEndOfFile: TNotifyEvent;
    FDelay: Integer;
    procedure SetFileName(const Value: TFileName);
    procedure SetDelay(const Value: Integer);
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Delay: Integer read FDelay write SetDelay default (1000 div 25);
    property FileName: TFileName read FFileName write SetFileName;
    property Loop: Boolean read FLoop write FLoop default True;
    property OnEndOfFile: TNotifyEvent read FOnEndOfFile Write FOnEndOfFile;
  end;

  TocvIPCamSource = class(TocvCustomSource)
  private
    FPort: Word;
    FPassword: string;
    FIP: string;
    FUserName: String;
    FPostfix: string;
  protected
    procedure SetEnabled(Value: Boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property UserName: String read FUserName write FUserName;
    property Password: string read FPassword write FPassword;
    property IP: string read FIP write FIP;
    property Postfix: string read FPostfix write FPostfix; {TODO: Need rename}
    property Port: Word read FPort write FPort default 554;
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

procedure TocvCaptureThread.Execute;
Var
  frame: pIplImage;
begin
  while not Terminated do
    if Assigned(FCapture) then
    begin
      try
        FLock.Enter;
        try
          frame := cvQueryFrame(FCapture);
        finally
          FLock.Leave;
        end;
        if not Terminated then
        begin
          if Assigned(frame) then
          begin
            if Assigned(OnNotifyData) then
              Synchronize(
                procedure
                begin
                  OnNotifyData(Self, TocvImage.CreateClone(frame));
                end);
            Sleep(FThreadDelay);
          end
          else if Assigned(OnNoData) then
            OnNoData(Self);
        end;
      except
      end;
    end
    else
      Suspend;
end;

{TOpenCVCamera}

constructor TocvCameraSource.Create(AOwner: TComponent);
begin
  inherited;
  FEnabled := False;
  FResolution := r160x120;
end;

procedure TocvCameraSource.SetCameraSource(const Value: TocvCameraCaptureSource);
Var
  isEnabled: Boolean;
begin
  if FCaptureSource <> Value then
  begin
    isEnabled := Enabled;
    if Assigned(FCapture) and FEnabled then
      Enabled := False;
    FCaptureSource := Value;
    Enabled := isEnabled;
  end;
end;

procedure TocvCameraSource.SetEnabled(Value: Boolean);
begin
  if FEnabled <> Value then
  begin
    if not(csDesigning in ComponentState) then
    begin
      if Assigned(FCapture) and FEnabled then
      begin
        FSourceThread.Capture := nil;
        cvReleaseCapture(FCapture);
        FCapture := Nil;
      end;
      if Value then
      begin
        FCapture := cvCreateCameraCapture(ocvCameraCaptureSource[FCaptureSource]);
        if Assigned(FCapture) then
        begin
          SetCameraResolution;
          FSourceThread.Capture := FCapture;
          FSourceThread.Resume;
        end;
      end;
    end;
    FEnabled := Value;
  end;
end;

function TocvCameraSource.GetHeight: Integer;
begin
  Result := CameraResolution[FResolution].cHeight;
end;

function TocvCameraSource.GetWidth: Integer;
begin
  Result := CameraResolution[FResolution].cWidth;
end;

procedure TocvCameraSource.SetCameraResolution;
begin
  cvSetCaptureProperty(FCapture, CV_CAP_PROP_FRAME_WIDTH, CameraResolution[FResolution].cWidth);
  cvSetCaptureProperty(FCapture, CV_CAP_PROP_FRAME_HEIGHT, CameraResolution[FResolution].cHeight);
end;

procedure TocvCameraSource.SetResolution(const Value: TocvResolution);
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

{TocvCustomSource}

constructor TocvCustomSource.Create(AOwner: TComponent);
begin
  inherited;
  if not(csDesigning in ComponentState) then
  begin
    FSourceThread := TocvCaptureThread.Create(True);
    FSourceThread.OnNotifyData := OnNotifyData;
    FSourceThread.FThreadDelay := FThreadDelay;
    FSourceThread.FreeOnTerminate := True;
  end;
  FThreadDelay := 10;
  FEnabled := False;
end;

destructor TocvCustomSource.Destroy;
begin
  TerminateSourceThread;
  ReleaseSource;
  inherited;
end;

function TocvCustomSource.GetEnabled: Boolean;
begin
  Result := FEnabled;
end;

function TocvCustomSource.GetHeight: Integer;
begin
  Result := 0;
end;

function TocvCustomSource.GetWidth: Integer;
begin
  Result := 0;
end;

procedure TocvCustomSource.Loaded;
begin
  inherited;
  if Enabled and (not Assigned(FCapture)) then
  begin
    // Hack
    FEnabled := False;
    Enabled := True;
  end;
end;

procedure TocvCustomSource.OnNotifyData(Sender: TObject; const IplImage: IocvImage);
begin
  FImage := IplImage.Clone;
  if Assigned(OnImage) then
    OnImage(Self, IplImage);
  NotifyReceiver(IplImage);
end;

procedure TocvCustomSource.SetEnabled(Value: Boolean);
begin

end;

procedure TocvCustomSource.TerminateSourceThread;
begin
  if Assigned(FSourceThread) then
  begin
    FSourceThread.Terminate;
    if FSourceThread.Suspended then
      FSourceThread.Resume;
    FSourceThread := Nil;
  end;
end;

procedure TocvCustomSource.ReleaseSource;
begin
  if Assigned(FCapture) then
  begin
    cvReleaseCapture(FCapture);
    FCapture := nil;
  end;
end;

{TocvFileSourceclass}

constructor TocvFileSource.Create(AOwner: TComponent);
begin
  inherited;
  FLoop := True;
  FDelay := (1000 div 25);
end;

procedure TocvFileSource.OnNoData(Sender: TObject);
begin
  if Assigned(FOnEndOfFile) then
    FOnEndOfFile(Self);
  if Loop then
  begin
    Enabled := False;
    Enabled := True;
  end;
end;

procedure TocvFileSource.SetDelay(const Value: Integer);
begin
  if FDelay <> Value then
  begin
    FDelay := Value;
    if Assigned(FSourceThread) then
      FSourceThread.FThreadDelay := FDelay;
  end;
end;

procedure TocvFileSource.SetEnabled(Value: Boolean);
begin
  if FEnabled <> Value then
  begin
    if not(csDesigning in ComponentState) then
    begin
      if Assigned(FCapture) and FEnabled then
      begin
        FSourceThread.Capture := nil;
        cvReleaseCapture(FCapture);
        FCapture := Nil;
      end;
      if Value then
      begin
        FCapture := cvCreateFileCapture(PAnsiChar(AnsiString(FileName)));
        if Assigned(FCapture) then
        begin
          FSourceThread.Capture := FCapture;
          FSourceThread.Resume;
        end;
      end;
    end;
    FEnabled := Value;
  end;
end;

procedure TocvFileSource.SetFileName(const Value: TFileName);
Var
  _Enabled: Boolean;
begin
  if FFileName <> Value then
  begin
    _Enabled := Enabled;
    Enabled := False;
    FFileName := Value;
    Enabled := _Enabled;
  end;
end;

{TocvCustomSourceThread}

constructor TocvCustomSourceThread.Create(CreateSuspended: Boolean);
begin
  inherited;
  FThreadDelay := 10;
  FLock := TCriticalSection.Create;
end;

destructor TocvCustomSourceThread.Destroy;
begin
  FLock.Free;
  inherited;
end;

procedure TocvCustomSourceThread.SetCapture(const Value: pCvCapture);
begin
  FLock.Enter;
  try
    FCapture := Value;
  finally
    FLock.Leave;
  end;
end;

{TocvIPCamSource}

constructor TocvIPCamSource.Create(AOwner: TComponent);
begin
  inherited;
  FPort := 554;
end;

procedure TocvIPCamSource.SetEnabled(Value: Boolean);
Var
  IPCam: AnsiString;
begin
  if FEnabled <> Value then
  begin
    if not(csDesigning in ComponentState) then
    begin
      if Assigned(FCapture) and FEnabled then
      begin
        FSourceThread.Capture := nil;
        cvReleaseCapture(FCapture);
        FCapture := Nil;
      end;
      if Value then
      begin
        IPCam := 'rtsp://';
        if Length(Trim(UserName)) <> 0 then
          IPCam := IPCam + Trim(UserName) + ':' + Trim(Password) + '@';
        IPCam := IPCam + IP + ':' + Port.ToString;
        if Length(Trim(Postfix)) > 0 then
        begin
          if (IPCam[Length(IPCam)] <> '/') and (Postfix[1] <> '/') then
            IPCam := IPCam + '/';
          IPCam := IPCam + Postfix;
        end;
        FCapture := cvCreateFileCapture(PAnsiChar(IPCam));
        if Assigned(FCapture) then
        begin
          FSourceThread.Capture := FCapture;
          FSourceThread.Resume;
        end;
      end;
    end;
    FEnabled := Value;
  end;
end;

end.
