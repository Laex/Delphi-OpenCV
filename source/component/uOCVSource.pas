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

{$IFNDEF CLR}
{$I OpenCV.inc}
unit uOCVSource;
{$ENDIF}

interface

uses
{$IFDEF VER6P}
  System.SysUtils,
  System.Classes,
  System.SyncObjs,
{$ELSE}
  SysUtils,
  Classes,
  SyncObjs,
{$ENDIF VER6P}
  ocv.core.types_c,
  ocv.highgui_c,
  ffm.libavcodec.avcodec,
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
  public
    constructor Create(CreateSuspended: Boolean);
    destructor Destroy; override;
    property OnNoData: TNotifyEvent Read FOnNoData write FOnNoData;
    property OnNotifyData: TOnOcvNotify Read FOnNotifyData write FOnNotifyData;
  end;

  TocvCustomSource = class(TocvDataSource)
  protected
    FSourceThread: TocvCustomSourceThread;
    FThreadDelay: Integer;
    procedure OnNotifyData(Sender: TObject; const IplImage: IocvImage);
    procedure SetEnabled(Value: Boolean); virtual;
    function GetEnabled: Boolean; override;
  private
    FEnabled: Boolean;
    FOnImage: TOnOcvNotify;
    procedure TerminateSourceThread;
    procedure ReleaseSource; virtual;
    function GetHeight: Integer; virtual;
    function GetWidth: Integer; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Enabled: Boolean Read GetEnabled write SetEnabled default False;
    property OnImage: TOnOcvNotify read FOnImage write FOnImage;
    property ImageWidth: Integer Read GetWidth;
    property ImageHeight: Integer Read GetHeight;
  end;

  TocvCaptureSource = class(TocvCustomSource)
  protected
    FCapture: pCvCapture;
    procedure Loaded; override;
  private
    procedure ReleaseSource; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TocvResolution = (r160x120, r320x240, r424x240, r640x360, r800x448, r960x544, r1280x720);

  TocvCameraSource = class(TocvCaptureSource)
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

  TocvIPCamSource = class(TocvCaptureSource)
  private
    FPort: Word;
    FPassword: string;
    FIP: string;
    FUserName: String;
    FURI: string;
  protected
    function GetIPCamTarget: AnsiString;
    procedure SetEnabled(Value: Boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property UserName: String read FUserName write FUserName;
    property Password: string read FPassword write FPassword;
    property IP: string read FIP write FIP;
    property URI: string read FURI write FURI; {TODO: Need rename}
    property Port: Word read FPort write FPort default 554;
  end;

  TOnNotifyFFMpegPacket = procedure(Sender: TObject; const packet: TAVPacket; const isKeyFrame: Boolean) of object;

  TocvFFMpegIPCamSource = class(TocvCustomSource)
  private
    FPort: Word;
    FPassword: string;
    FIP: string;
    FUserName: String;
    FURI: string;
    FOnNotifyFFMpegPacket: TOnNotifyFFMpegPacket;
  protected
    function GetIPCamTarget: AnsiString;
    procedure SetEnabled(Value: Boolean); override;
    procedure Loaded; override;
    procedure DoNotifyPacket(const packet: TAVPacket; const isKeyFrame: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
  published
    property UserName: String read FUserName write FUserName;
    property Password: string read FPassword write FPassword;
    property IP: string read FIP write FIP;
    property URI: string read FURI write FURI; {TODO: Need rename}
    property Port: Word read FPort write FPort default 554;
    property OnFFMpegPacket: TOnNotifyFFMpegPacket read FOnNotifyFFMpegPacket write FOnNotifyFFMpegPacket;
  end;

implementation

uses
  ocv.core_c,
  ffm.avformat,
  ffm.dict,
  ffm.avutil,
  ffm.frame,
  ffm.swscale,
  ffm.pixfmt;

Type
  TocvCaptureThread = class(TocvCustomSourceThread)
  private
    procedure SetCapture(const Value: pCvCapture); virtual;
  protected
    FCapture: pCvCapture;
  protected
    procedure Execute; override;
  public
    property Capture: pCvCapture read FCapture write SetCapture;
  end;

  TocvFFMpegIPCamSourceThread = class(TocvCustomSourceThread)
  private
    FEnabled: Boolean;
    FIPCamURL: AnsiString;
    FSuspendEvent: TEvent;
    FOwner: TocvFFMpegIPCamSource;
    procedure TerminatedSet; override;
  protected
    procedure Execute; override;
  public
    constructor Create(AOwner: TocvFFMpegIPCamSource);
    destructor Destroy; override;
    procedure SetIPCamUrl(const AIPCam: AnsiString; const AEnabled: Boolean);
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
        (FSourceThread as TocvCaptureThread).Capture := nil;
        cvReleaseCapture(FCapture);
        FCapture := Nil;
      end;
      if Value then
      begin
        FCapture := cvCreateCameraCapture(ocvCameraCaptureSource[FCaptureSource]);
        if Assigned(FCapture) then
        begin
          SetCameraResolution;
          (FSourceThread as TocvCaptureThread).Capture := FCapture;
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
        (FSourceThread as TocvCaptureThread).Capture := nil;
        cvReleaseCapture(FCapture);
        FCapture := Nil;
      end;
      if Value then
      begin
        FCapture := cvCreateFileCapture(PAnsiChar(AnsiString(FileName)));
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

{TocvIPCamSource}

constructor TocvIPCamSource.Create(AOwner: TComponent);
begin
  inherited;
  FPort := 554;
end;

function TocvIPCamSource.GetIPCamTarget: AnsiString;
begin
  Result := 'rtsp://';
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

procedure TocvIPCamSource.SetEnabled(Value: Boolean);
begin
  if FEnabled <> Value then
  begin
    if not(csDesigning in ComponentState) then
    begin
      if Assigned(FCapture) and FEnabled then
      begin
        (FSourceThread as TocvCaptureThread).Capture := nil;
        cvReleaseCapture(FCapture);
        FCapture := Nil;
      end;
      if Value then
      begin
        FCapture := cvCreateFileCapture(PAnsiChar(GetIPCamTarget));
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

{TocvCaptureSource}

constructor TocvCaptureSource.Create(AOwner: TComponent);
begin
  inherited;
  if not(csDesigning in ComponentState) then
  begin
    FSourceThread := TocvCaptureThread.Create(True);
    FSourceThread.OnNotifyData := OnNotifyData;
    FSourceThread.FThreadDelay := FThreadDelay;
    FSourceThread.FreeOnTerminate := True;
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
    cvReleaseCapture(FCapture);
    FCapture := nil;
  end;
end;

{TocvFFMpegIPCamSource}

constructor TocvFFMpegIPCamSource.Create(AOwner: TComponent);
begin
  inherited;
  FPort := 554;
  if not(csDesigning in ComponentState) then
  begin
    FSourceThread := TocvFFMpegIPCamSourceThread.Create(Self);
    FSourceThread.OnNotifyData := OnNotifyData;
    FSourceThread.FThreadDelay := FThreadDelay;
    FSourceThread.FreeOnTerminate := True;
  end;
end;

procedure TocvFFMpegIPCamSource.DoNotifyPacket(const packet: TAVPacket; const isKeyFrame: Boolean);
begin
  if Assigned(OnFFMpegPacket) then
    OnFFMpegPacket(Self, packet, isKeyFrame);
end;

function TocvFFMpegIPCamSource.GetIPCamTarget: AnsiString;
begin
  Result := 'rtsp://';
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

procedure TocvFFMpegIPCamSource.Loaded;
begin
  inherited;
  if Enabled then
  begin
    // Hack
    FEnabled := False;
    Enabled := True;
  end;
end;

procedure TocvFFMpegIPCamSource.SetEnabled(Value: Boolean);
begin
  if FEnabled <> Value then
  begin
    if not(csDesigning in ComponentState) then
      (FSourceThread as TocvFFMpegIPCamSourceThread).SetIPCamUrl(GetIPCamTarget, Value);
    FEnabled := Value;
  end;
end;

procedure TocvCaptureThread.SetCapture(const Value: pCvCapture);
begin
  FLock.Enter;
  try
    FCapture := Value;
  finally
    FLock.Leave;
  end;
end;

{TocvFFMpegIPCamSourceThread}

constructor TocvFFMpegIPCamSourceThread.Create(AOwner: TocvFFMpegIPCamSource);
begin
  inherited Create(False);
  FOwner := AOwner;
  FSuspendEvent := TEvent.Create;
  FSuspendEvent.ResetEvent;
end;

destructor TocvFFMpegIPCamSourceThread.Destroy;
begin
  FSuspendEvent.Free;
  inherited;
end;

procedure TocvFFMpegIPCamSourceThread.Execute;
Var
  optionsDict: pAVDictionary;
  pFormatCtx: pAVFormatContext;
  pCodecCtx: pAVCodecContext;
  pCodec: pAVCodec;
  packet: TAVPacket;
  img_convert_context: pSwsContext;
  frame: pAVFrame;
  iplframe: pIplImage;

  procedure ReleaseAllocatedData;
  begin
    if Assigned(pCodecCtx) then
    begin
      avcodec_close(pCodecCtx);
      pCodecCtx := nil;
    end;
    if Assigned(pFormatCtx) then
    begin
      avformat_close_input(pFormatCtx);
      pFormatCtx := nil;
    end;
    if Assigned(iplframe) then
    begin
      cvReleaseImage(iplframe);
      iplframe := nil;
    end;
    if Assigned(frame) then
    begin
      av_frame_free(frame);
      frame := nil;
    end;
    if Assigned(optionsDict) then
    begin
      av_dict_free(optionsDict);
      optionsDict := nil;
    end;
  end;

Var
  i, ret, videoStream: Integer;
  frame_finished: Integer;
  linesize: array [0 .. 3] of Integer;

  isReconnect: Boolean;
begin
  av_register_all();
  avformat_network_init();

  optionsDict := nil;
  pFormatCtx := nil;
  pCodecCtx := nil;
  iplframe := nil;
  frame := nil;
  isReconnect := False;

  While (not Terminated) do
  begin

    FSuspendEvent.WaitFor;
    if Terminated then
      Break;

    ReleaseAllocatedData;
    if isReconnect then
    begin
      i := 0;
      while (not Terminated) and (i < 10) do
      begin
        Sleep(100);
        Inc(i);
      end;
      if Terminated then
        Break;
      isReconnect := False;
    end;

    av_dict_set(optionsDict, 'rtsp_transport', 'tcp', 0);
    av_dict_set(optionsDict, 'stimeout', '1000000', 0);
    ret := avformat_open_input(pFormatCtx, PAnsiChar(FIPCamURL), nil, @optionsDict); // pFormatCtx
    if ret < 0 then
    begin
      isReconnect := True;
      Continue;
    end;

    av_dict_free(optionsDict);
    optionsDict := nil;
    if avformat_find_stream_info(pFormatCtx, nil) < 0 then
    begin
      isReconnect := True;
      Continue;
    end;

    // Dump information about file onto standard error
    av_dump_format(pFormatCtx, 0, PAnsiChar(FIPCamURL), 0);
    // Find the first video stream
    videoStream := -1;
    for i := 0 to pFormatCtx^.nb_streams - 1 do
      if (pFormatCtx^.streams[i]^.codec^.codec_type = AVMEDIA_TYPE_VIDEO) then
      begin
        videoStream := i;
        Break;
      end;

    if videoStream = -1 then
    begin
      isReconnect := True;
      Continue;
    end;

    // Get a pointer to the codec context for the video stream
    pCodecCtx := pFormatCtx^.streams[videoStream]^.codec; // pCodecCtx
    // Find the decoder for the video stream
    pCodec := avcodec_find_decoder(pCodecCtx^.codec_id);
    if not Assigned(pCodec) then
    begin
      isReconnect := True;
      Continue;
    end;

    if (pCodec^.capabilities and CODEC_CAP_TRUNCATED) = 0 then
      pCodecCtx^.flags := pCodecCtx^.flags or CODEC_FLAG_TRUNCATED; (*we dont send complete frames*)
    // Open codec
    if avcodec_open2(pCodecCtx, pCodec, nil) < 0 then
    begin
      isReconnect := True;
      Continue;
    end;

    img_convert_context := sws_getCachedContext(nil, pCodecCtx^.width, pCodecCtx^.height, pCodecCtx^.pix_fmt, pCodecCtx^.width,
      pCodecCtx^.height, AV_PIX_FMT_BGR24, SWS_BILINEAR, nil, nil, nil);
    if (img_convert_context = nil) then
    begin
      isReconnect := True;
      Continue;
    end;

    frame := av_frame_alloc();
    iplframe := cvCreateImage(CvSize(pCodecCtx^.width, pCodecCtx^.height), IPL_DEPTH_8U, 3); // iplframe
    FillChar(linesize, SizeOf(linesize), 0);
    linesize[0] := iplframe^.widthStep;

    while (not Terminated) and (FSuspendEvent.WaitFor(0) = wrSignaled) do
    begin
      if av_read_frame(pFormatCtx, packet) >= 0 then
      begin
        if (packet.stream_index = videoStream) then
        begin
          Synchronize(
            procedure
            begin
              FOwner.DoNotifyPacket(packet, (packet.flags and AV_PKT_FLAG_KEY) <> 0);
            end);
          // Video stream packet
          avcodec_decode_video2(pCodecCtx, frame, frame_finished, @packet);
          if (frame_finished <> 0) then
          begin
            sws_scale(img_convert_context, @frame^.data, @frame^.linesize, 0, pCodecCtx^.height, @iplframe^.imageData, @linesize);
            if Assigned(OnNotifyData) then
              Synchronize(
                procedure
                begin
                  OnNotifyData(Self, TocvImage.CreateClone(iplframe));
                end);
          end;
        end
        else
        begin
          isReconnect := True;
          Break;
        end;
      end;
    end;
  end;
  ReleaseAllocatedData;
  avformat_network_deinit;
end;

procedure TocvFFMpegIPCamSourceThread.SetIPCamUrl(const AIPCam: AnsiString; const AEnabled: Boolean);
begin
  if (FEnabled <> AEnabled) or (FIPCamURL <> AIPCam) then
  begin
    FSuspendEvent.ResetEvent;
    FIPCamURL := AIPCam;
    FEnabled := AEnabled;
    if FEnabled then
      FSuspendEvent.SetEvent;
  end;
end;

procedure TocvFFMpegIPCamSourceThread.TerminatedSet;
begin
  inherited;
  FSuspendEvent.ResetEvent;
end;

end.
