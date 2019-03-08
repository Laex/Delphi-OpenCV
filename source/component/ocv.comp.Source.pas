(*
  *****************************************************************
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
  *******************************************************************
*)

{$IFNDEF CLR }
{$I OpenCV.inc}
unit ocv.comp.Source;
{$ENDIF}

interface

uses
{$IFDEF HAS_UNITSCOPE}
  System.SysUtils,
  System.Classes,
  System.SyncObjs,
{$ELSE}
  SysUtils,
  Classes,
  SyncObjs,
{$ENDIF}
  ocv.core.types_c,
  ocv.highgui_c,
  ocv.comp.Types,
  ocv.lock;

type
  TocvCameraCaptureSource =
  //
    (CAP_ANY { = 0 } , // autodetect
    CAP_CAM_0 { =0 } , //
    CAP_CAM_1 { =1 } , //
    CAP_CAM_2 { =2 } , //
    CAP_CAM_3 { =3 } , //
    CAP_CAM_4 { =4 } , //
    CAP_CAM_5 { =5 } , //
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

  TocvCustomSourceThread = class(TThread)
  private
    FOnNotifyData: TOnOcvNotify;
    FOnNoData: TNotifyEvent;
    FThreadDelay: Integer;
    FLock: TOCVLock;
  public
    constructor Create(CreateSuspended: Boolean);
    destructor Destroy; override;
    property OnNoData: TNotifyEvent Read FOnNoData write FOnNoData;
    property OnNotifyData: TOnOcvNotify Read FOnNotifyData write FOnNotifyData;
    property ThreadDelay: Integer read FThreadDelay Write FThreadDelay;
  end;

  TocvCustomSource = class(TocvDataSource)
  protected
    FSourceThread: TocvCustomSourceThread;
    FThreadDelay: Integer;
    FEnabled: Boolean;
    procedure OnNotifyData(Sender: TObject; Var IplImage: IocvImage); virtual;
    procedure SetEnabled(Value: Boolean); virtual;
    function GetEnabled: Boolean; override;
    procedure TerminateSourceThread; virtual;
  private
    FOnImage: TOnOcvNotify;
    procedure ReleaseSource; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Enabled: Boolean Read GetEnabled write SetEnabled default False;
    property OnImage: TOnOcvNotify read FOnImage write FOnImage;
    property ImageWidth: Integer Read GetWidth;
    property ImageHeight: Integer Read GetHeight;
    property FPS: Double read GetFPS;
  end;

{$IFDEF DelphiOCVVersion_30}

  TcsVideoCapture = IVideoCapture;
{$ELSE}
  TcsVideoCapture = pCvCapture;
{$ENDIF}

  TocvCaptureSource = class(TocvCustomSource)
  protected
    FCapture: TcsVideoCapture;
    procedure Loaded; override;
  private
    procedure ReleaseSource; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TocvResolution = (r160x120, r320x240, r424x240, r640x360, r800x448, r960x544, r1280x720, rCustom);

  TocvCameraSource = class(TocvCaptureSource)
  protected
    procedure SetEnabled(Value: Boolean); override;
  private
    FCaptureSource: TocvCameraCaptureSource;
    FResolution: TocvResolution;
    FCustomHeight: Cardinal;
    FCustomWidth: Cardinal;
    procedure SetCameraSource(const Value: TocvCameraCaptureSource);
    procedure SetResolution(const Value: TocvResolution);
    procedure SetCameraResolution;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Camera: TocvCameraCaptureSource read FCaptureSource write SetCameraSource default CAP_ANY;
    property Resolution: TocvResolution read FResolution write SetResolution default r160x120;
    property CustomWidth: Cardinal read FCustomWidth write FCustomWidth default 0;
    property CustomHeight: Cardinal read FCustomHeight write FCustomHeight default 0;
  end;

  TocvFileSource = class(TocvCaptureSource)
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

  TocvIPProtocol = ( //
    ippHTTP, //
    ippHTTPS, //
    ippRTSP //
    ); //

  TocvIPCamSource = class(TocvCaptureSource)
  private
    FPort: Word;
    FPassword: string;
    FIP: string;
    FUserName: String;
    FURI: string;
    FProtocol: TocvIPProtocol;
  protected
    function GetIPCamTarget: AnsiString; overload;
    function GetIPCamTarget(var URL: AnsiString): Boolean; overload;
    procedure SetEnabled(Value: Boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property UserName: String read FUserName write FUserName;
    property Password: string read FPassword write FPassword;
    property IP: string read FIP write FIP;
    property URI: string read FURI write FURI; { TODO: Need rename }
    property Port: Word read FPort write FPort default 554;
    property Protocol: TocvIPProtocol read FProtocol write FProtocol default ippRTSP;
  end;

const
  IPProtocolString: array [TocvIPProtocol] of AnsiString = ( //
    'http://', //
    'https://', //
    'rtsp://' //
    );

implementation

uses
  ocv.core_c,
  ocv.utils;

Type
  TocvCaptureThread = class(TocvCustomSourceThread)
  private
    procedure SetCapture(const Value: TcsVideoCapture); virtual;
  protected
    FCapture: TcsVideoCapture;
    procedure Execute; override;
  public
    property Capture: TcsVideoCapture read FCapture write SetCapture;
  end;

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
  CameraResolution: array [TocvResolution] of TCameraResolution = ((cWidth: 160; cHeight: 120), (cWidth: 320;
    cHeight: 240), (cWidth: 424; cHeight: 240), (cWidth: 640; cHeight: 360), (cWidth: 800; cHeight: 448), (cWidth: 960;
    cHeight: 544), (cWidth: 1280; cHeight: 720), (cWidth: 0; cHeight: 0));

  { TOpenCVCameraThread }

procedure TocvCaptureThread.Execute;
Var
{$IFDEF DelphiOCVVersion_30}
  frame: IMat;
  I: TIplImage;
{$ELSE}
  frame: pIplImage;
{$ENDIF}
  Image: IocvImage;
begin
  while not Terminated do
    if Assigned(FCapture) then
    begin
      try
        FLock.Enter;
        try
          frame := nil;
{$IFDEF DelphiOCVVersion_30}
          FCapture.Read(frame);
{$ELSE}
          frame := cvQueryFrame(FCapture);
{$ENDIF}
        finally
          FLock.Leave;
        end;
        if not Terminated then
        begin
          if Assigned(frame) then
          begin
            if Assigned(OnNotifyData) then
            begin
{$IFDEF DelphiOCVVersion_30}
              I.InitFromMat(frame);
              Image := TocvImage.CreateClone(@I);
{$ELSE}
              Image := TocvImage.CreateClone(frame);
{$ENDIF}
              OnNotifyData(Self, Image);
              Image := nil;
              Sleep(FThreadDelay);
            end;
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

{ TOpenCVCamera }

constructor TocvCameraSource.Create(AOwner: TComponent);
begin
  inherited;
  FEnabled := False;
  Resolution := r160x120;
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
    if csDesigning in ComponentState then
      FEnabled := Value
    else
    begin
      if Assigned(FCapture) and FEnabled then
      begin
        (FSourceThread as TocvCaptureThread).Capture := nil;
{$IFNDEF DelphiOCVVersion_30}
        cvReleaseCapture(FCapture);
{$ENDIF}
        FCapture := Nil;
        FEnabled := False;
      end;
      if Value then
      begin
{$IFDEF DelphiOCVVersion_30}
        FCapture := TVideoCapture.Create(ocvCameraCaptureSource[FCaptureSource]);
{$ELSE}
        FCapture := cvCreateCameraCapture(ocvCameraCaptureSource[FCaptureSource]);
{$ENDIF}
        if Assigned(FCapture) then
        begin
          SetCameraResolution;
{$IFDEF DelphiOCVVersion_30}
          FFPS := FCapture.Prop[CV_CAP_PROP_FPS];
{$ELSE}
          FFPS := cvGetCaptureProperty(FCapture, CV_CAP_PROP_FPS);
{$ENDIF}
          (FSourceThread as TocvCaptureThread).Capture := FCapture;
          FSourceThread.Resume;
          FEnabled := True;
        end;
      end;
    end;
  end;
end;

procedure TocvCameraSource.SetCameraResolution;
Var
  cR: TCameraResolution;
begin
  if (FResolution = rCustom) then
  begin
    if (FCustomWidth > 0) and (FCustomHeight > 0) then
    begin
      cR.cWidth := FCustomWidth;
      cR.cHeight := FCustomHeight;
    end
    else
      cR := CameraResolution[r160x120];
  end
  else
    cR := CameraResolution[FResolution];
{$IFDEF DelphiOCVVersion_30}
  FCapture.Prop[CV_CAP_PROP_FRAME_WIDTH] := cR.cWidth;
  FCapture.Prop[CV_CAP_PROP_FRAME_HEIGHT] := cR.cHeight;
{$ELSE}
  cvSetCaptureProperty(FCapture, CV_CAP_PROP_FRAME_WIDTH, cR.cWidth);
  cvSetCaptureProperty(FCapture, CV_CAP_PROP_FRAME_HEIGHT, cR.cHeight);
{$ENDIF}
end;

procedure TocvCameraSource.SetResolution(const Value: TocvResolution);
begin
  FWidth := CameraResolution[Value].cWidth;
  FHeight := CameraResolution[Value].cHeight;
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

{ TocvCustomSource }

constructor TocvCustomSource.Create(AOwner: TComponent);
begin
  inherited;
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

procedure TocvCustomSource.OnNotifyData(Sender: TObject; Var IplImage: IocvImage);
begin
  FWidth := IplImage.Width;
  FHeight := IplImage.Height;
  FImage := IplImage.Clone;
  if Assigned(OnImage) then
    OnImage(Self, IplImage);
  NotifyReceiver(IplImage);
end;

procedure TocvCustomSource.SetEnabled(Value: Boolean);
begin
  FEnabled := Value;
end;

procedure TocvCustomSource.TerminateSourceThread;
begin
  if Assigned(FSourceThread) then
  begin
    FSourceThread.Terminate;
    if FSourceThread.Suspended then
      FSourceThread.Resume;
    FSourceThread.WaitFor;
    FreeAndNil(FSourceThread);
  end;
end;

procedure TocvCustomSource.ReleaseSource;
begin

end;

{ TocvFileSourceclass }

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
{$IFNDEF DelphiOCVVersion_30}
Var
  pFileName: PAnsiChar;
{$ENDIF}
begin
  if FEnabled <> Value then
  begin
    if not(csDesigning in ComponentState) then
    begin
      if Assigned(FCapture) and FEnabled then
      begin
        (FSourceThread as TocvCaptureThread).Capture := nil;
{$IFNDEF DelphiOCVVersion_30}
        cvReleaseCapture(FCapture);
{$ENDIF}
        FCapture := Nil;
      end;
      if Value and FileExists(FileName) then
      begin
{$IFDEF DelphiOCVVersion_30}
        FCapture := TVideoCapture.Create(FileName);
{$ELSE}
        pFileName := PAnsiChar(@(AnsiString(FileName)[1]));
        FCapture := cvCreateFileCapture(pFileName);
{$ENDIF}
        if Assigned(FCapture) then
        begin
          (FSourceThread as TocvCaptureThread).Capture := FCapture;
          (FSourceThread as TocvCaptureThread).OnNoData := OnNoData; // Here is the addition
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

{ TocvCustomSourceThread }

constructor TocvCustomSourceThread.Create(CreateSuspended: Boolean);
begin
  inherited;
  FThreadDelay := 10;
  FLock := TOCVLock.Create;
end;

destructor TocvCustomSourceThread.Destroy;
begin
  FLock.Free;
  inherited;
end;

{ TocvIPCamSource }

constructor TocvIPCamSource.Create(AOwner: TComponent);
begin
  inherited;
  FPort := 554;
  FProtocol := ippRTSP;
end;

function TocvIPCamSource.GetIPCamTarget: AnsiString;
begin
  Result := IPProtocolString[FProtocol];
  if Length(Trim(UserName)) <> 0 then
    Result := Result + AnsiString(Trim(UserName)) + ':' + AnsiString(Trim(Password)) + '@';
  Result := Result + AnsiString(IP) + ':' + AnsiString(IntToStr(Port));
  if Length(Trim(URI)) > 0 then
  begin
    if (Result[Length(Result)] <> '/') and (URI[1] <> '/') then
      Result := Result + '/';
    Result := Result + AnsiString(URI);
  end;
end;

function TocvIPCamSource.GetIPCamTarget(var URL: AnsiString): Boolean;
begin
  URL := IPProtocolString[FProtocol];
  if Length(Trim(UserName)) <> 0 then
    URL := URL + AnsiString(Trim(UserName)) + ':' + AnsiString(Trim(Password)) + '@';
  URL := URL + AnsiString(IP) + ':' + AnsiString(IntToStr(Port));
  if Length(Trim(URI)) > 0 then
  begin
    if (URL[Length(URL)] <> '/') and (URI[1] <> '/') then
      URL := URL + '/';
    URL := URL + AnsiString(URI);
  end;
  Result := Length(IP) > 0;
end;

procedure TocvIPCamSource.SetEnabled(Value: Boolean);
Var
  IPCamURL: AnsiString;
begin
  if FEnabled <> Value then
  begin
    if not(csDesigning in ComponentState) then
    begin
      if Assigned(FCapture) and FEnabled then
      begin
        (FSourceThread as TocvCaptureThread).Capture := nil;
{$IFNDEF DelphiOCVVersion_30}
        cvReleaseCapture(FCapture);
{$ENDIF}
        FCapture := Nil;
      end;
      if Value then
      begin
        if GetIPCamTarget(IPCamURL) and (not(csLoading in ComponentState)) then
        begin
{$IFDEF DelphiOCVVersion_30}
          FCapture := TVideoCapture.Create(IPCamURL);
{$ELSE}
          FCapture := cvCreateFileCapture(PAnsiChar(IPCamURL));
{$ENDIF}
        end;
        if Assigned(FCapture) then
        begin
          (FSourceThread as TocvCaptureThread).Capture := FCapture;
          FSourceThread.Resume;
        end;
      end;
    end;
    FEnabled := Value;
  end;
end;

{ TocvCaptureSource }

constructor TocvCaptureSource.Create(AOwner: TComponent);
begin
  inherited;
  if not(csDesigning in ComponentState) then
  begin
    FSourceThread := TocvCaptureThread.Create(True);
    FSourceThread.OnNotifyData := OnNotifyData;
    FSourceThread.FThreadDelay := FThreadDelay;
    // FSourceThread.FreeOnTerminate := True;
  end;
end;

procedure TocvCaptureSource.Loaded;
begin
  inherited;
  if Enabled and (not Assigned(FCapture)) then
  begin
    // Hack
    FEnabled := False;
    Enabled := True;
  end;
end;

procedure TocvCaptureSource.ReleaseSource;
begin
  inherited;
  if Assigned(FCapture) then
  begin
{$IFNDEF DelphiOCVVersion_30}
    cvReleaseCapture(FCapture);
{$ENDIF}
    FCapture := nil;
  end;
end;

procedure TocvCaptureThread.SetCapture(const Value: TcsVideoCapture);
begin
  FLock.Enter;
  try
    FCapture := Value;
  finally
    FLock.Leave;
  end;
end;

end.
