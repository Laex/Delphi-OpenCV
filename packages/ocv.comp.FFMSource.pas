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
{$I ..\..\source\OpenCV.inc}
unit ocv.comp.FFMSource;
{$ENDIF}

interface

Uses
{$IFDEF HAS_UNITSCOPE}
  System.SysUtils,
  System.Classes,
  System.SyncObjs,
{$ELSE}
  SysUtils,
  Classes,
  SyncObjs,
{$ENDIF}
  ocv.comp.Source,
  libavcodec;

Type
  TOnNotifyFFMpegPacket = procedure(Sender: TObject; const packet: AVPacket; const isKeyFrame: Boolean) of object;

  TocvFFMpegIPCamEvent = (ffocvTryConnect, ffocvConnected, ffocvLostConnection, ffocvReconnect, ffocvErrorGetStream);
  TOnocvFFMpegIPCamEvent = procedure(Sender: TObject; const Event: TocvFFMpegIPCamEvent) of object;

  TocvFFMpegIPCamSource = class(TocvCustomSource)
  private
    FPort: Word;
    FPassword: string;
    FIP: string;
    FUserName: String;
    FURI: string;
    FOnNotifyFFMpegPacket: TOnNotifyFFMpegPacket;
    FProtocol: TocvIPProtocol;
    FReconnectDelay: Cardinal;
    FOnIPCamEvent: TOnocvFFMpegIPCamEvent;
    procedure SetReconnectDelay(const Value: Cardinal);
  protected
    procedure TerminateSourceThread; override;
    function GetIPCamTarget: AnsiString;
    procedure SetEnabled(Value: Boolean); override;
    procedure Loaded; override;
    procedure DoNotifyPacket(const packet: AVPacket; const isKeyFrame: Boolean);
    procedure DoNotifyEvent(Event: TocvFFMpegIPCamEvent);
  public
    constructor Create(AOwner: TComponent); override;
  published
    property UserName: String read FUserName write FUserName;
    property Password: string read FPassword write FPassword;
    property IP: string read FIP write FIP;
    property URI: string read FURI write FURI; { TODO: Need rename }
    property Port: Word read FPort write FPort default 554;
    property Protocol: TocvIPProtocol read FProtocol write FProtocol default ippRTSP;
    property OnFFMpegPacket: TOnNotifyFFMpegPacket read FOnNotifyFFMpegPacket write FOnNotifyFFMpegPacket;
    property OnIPCamEvent: TOnocvFFMpegIPCamEvent read FOnIPCamEvent write FOnIPCamEvent;
    property ReconnectDelay: Cardinal Read FReconnectDelay write SetReconnectDelay default 1000;
  end;

implementation

Uses
  ocv.core_c,
  ocv.core.types_c,
  ocv.comp.Types,
  libavformat,
  libavutil,
  libswscale;

Type
  TocvFFMpegIPCamSourceThread = class(TocvCustomSourceThread)
  private
    FEnabled: Boolean;
    FIPCamURL: AnsiString;
    FSuspendEvent: TEvent;
    FOwner: TocvFFMpegIPCamSource;
    FReconnectDelay: Cardinal;
    FisReconnect: Boolean;
{$IFDEF DELPHIXE2_UP}
    procedure TerminatedSet; override;
{$ENDIF}
    procedure DoNotify(Event: TocvFFMpegIPCamEvent);
  protected
    procedure Execute; override;
  public
    constructor Create(AOwner: TocvFFMpegIPCamSource);
    destructor Destroy; override;
    procedure SetIPCamUrl(const AIPCam: AnsiString; const AEnabled: Boolean);
  end;

  { TocvFFMpegIPCamSource }

constructor TocvFFMpegIPCamSource.Create(AOwner: TComponent);
begin
  inherited;
  FPort := 554;
  FProtocol := ippRTSP;
  FReconnectDelay := 1000;
  if not(csDesigning in ComponentState) then
  begin
    FSourceThread := TocvFFMpegIPCamSourceThread.Create(Self);
    FSourceThread.OnNotifyData := OnNotifyData;
    FSourceThread.ThreadDelay := FThreadDelay;
//    FSourceThread.FreeOnTerminate := True;
  end;
end;

procedure TocvFFMpegIPCamSource.DoNotifyEvent(Event: TocvFFMpegIPCamEvent);
begin
  if Assigned(OnIPCamEvent) then
    OnIPCamEvent(Self, Event);
end;

procedure TocvFFMpegIPCamSource.DoNotifyPacket(const packet: AVPacket; const isKeyFrame: Boolean);
begin
  if Assigned(OnFFMpegPacket) then
    OnFFMpegPacket(Self, packet, isKeyFrame);
end;

function TocvFFMpegIPCamSource.GetIPCamTarget: AnsiString;
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

procedure TocvFFMpegIPCamSource.Loaded;
begin
  inherited;
  if Enabled then
  begin
    // Hack
    Enabled := False;
    Enabled := True;
  end;
end;

procedure TocvFFMpegIPCamSource.SetEnabled(Value: Boolean);
begin
  if Enabled <> Value then
  begin
    if not(csDesigning in ComponentState) then
    begin
      (FSourceThread as TocvFFMpegIPCamSourceThread).FReconnectDelay := ReconnectDelay;
      (FSourceThread as TocvFFMpegIPCamSourceThread).SetIPCamUrl(GetIPCamTarget, Value);
    end;
    FEnabled := Value;
  end;
end;

procedure TocvFFMpegIPCamSource.SetReconnectDelay(const Value: Cardinal);
begin
  if FReconnectDelay <> Value then
  begin
    FReconnectDelay := Value;
    if Assigned(FSourceThread) then
      (FSourceThread as TocvFFMpegIPCamSourceThread).FReconnectDelay := ReconnectDelay;
  end;
end;

procedure TocvFFMpegIPCamSource.TerminateSourceThread;
begin
  if Assigned(FSourceThread) then
    (FSourceThread as TocvFFMpegIPCamSourceThread).FSuspendEvent.SetEvent;
  inherited;
end;

{ TocvFFMpegIPCamSourceThread }

constructor TocvFFMpegIPCamSourceThread.Create(AOwner: TocvFFMpegIPCamSource);
begin
  inherited Create(False);
  FOwner := AOwner;
  FReconnectDelay := 1000;
  FSuspendEvent := TEvent.Create;
  FSuspendEvent.ResetEvent;
end;

destructor TocvFFMpegIPCamSourceThread.Destroy;
begin
  FSuspendEvent.Free;
  inherited;
end;

procedure TocvFFMpegIPCamSourceThread.DoNotify(Event: TocvFFMpegIPCamEvent);
begin
  Synchronize(
    procedure
    begin
      FOwner.DoNotifyEvent(Event);
    end);
end;

procedure TocvFFMpegIPCamSourceThread.Execute;
Var
  optionsDict: pAVDictionary;
  pFormatCtx: pAVFormatContext;
  pCodecCtx: pAVCodecContext;
  pCodec: pAVCodec;
  packet: AVPacket;
  img_convert_context: pSwsContext;
  frame: pAVFrame;
  iplframe: pIplImage;

  procedure ReleaseAllocatedData;
  begin
    if Assigned(pCodecCtx) then
    begin
      avcodec_free_context(pCodecCtx);
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
    if Assigned(img_convert_context) then
    begin
      sws_freeContext(img_convert_context);
      img_convert_context := nil;
    end;
  end;

Var
  i, ret, videoStream, recvRet: Integer;
  linesize: array [0 .. 3] of Integer;
  RDelay: Cardinal;
  Image: IocvImage;
begin
  avformat_network_init();

  optionsDict := nil;
  pFormatCtx := nil;
  pCodecCtx := nil;
  iplframe := nil;
  frame := nil;
  img_convert_context := nil;

  try
    While (not Terminated) do
    begin

      FisReconnect := False;
  {$IFDEF DELPHIXE_UP}
      FSuspendEvent.WaitFor;
  {$ELSE}
      FSuspendEvent.WaitFor(10000);
  {$ENDIF}
      if Terminated then
        Break;

      ReleaseAllocatedData;

      DoNotify(ffocvTryConnect);

      av_dict_set(optionsDict, 'rtsp_transport', 'tcp', 0);
      av_dict_set(optionsDict, 'rtsp_flags', 'prefer_tcp', 0);
      av_dict_set(optionsDict, 'allowed_media_types', 'video', 0);
      av_dict_set(optionsDict, 'reorder_queue_size', '10', 0);
      av_dict_set(optionsDict, 'max_delay', '500000', 0);
      av_dict_set(optionsDict, 'stimeout', '1000000', 0);

      ret := avformat_open_input(pFormatCtx, PAnsiChar(FIPCamURL), nil, @optionsDict); // pFormatCtx
      if ret < 0 then
      begin
        DoNotify(ffocvErrorGetStream);
        FisReconnect := True;
        Continue;
      end;

      av_dict_free(optionsDict);
      optionsDict := nil;
      if avformat_find_stream_info(pFormatCtx, nil) < 0 then
      begin
        DoNotify(ffocvErrorGetStream);
        FisReconnect := True;
        Continue;
      end;

      // Dump information about file onto standard error
      av_dump_format(pFormatCtx, 0, PAnsiChar(FIPCamURL), 0);
      // Find the first video stream
      videoStream := -1;
      for i := 0 to pFormatCtx^.nb_streams - 1 do
        if (pFormatCtx^.streams[i]^.codecpar^.codec_type = AVMEDIA_TYPE_VIDEO) then
        begin
          videoStream := i;
          Break;
        end;

      if videoStream = -1 then
      begin
        DoNotify(ffocvErrorGetStream);
        FisReconnect := True;
        Continue;
      end;

      pCodec := avcodec_find_decoder(pFormatCtx^.streams[videoStream]^.codecpar^.codec_id);
      if not Assigned(pCodec) then
      begin
        DoNotify(ffocvErrorGetStream);
        FisReconnect := True;
        Continue;
      end;

      pCodecCtx := avcodec_alloc_context3(pCodec);
      if not Assigned(pCodecCtx) then
      begin
        DoNotify(ffocvErrorGetStream);
        FisReconnect := True;
        Continue;
      end;

      if avcodec_parameters_to_context(pCodecCtx, pFormatCtx^.streams[videoStream]^.codecpar) < 0 then
      begin
        DoNotify(ffocvErrorGetStream);
        FisReconnect := True;
        Continue;
      end;

      if avcodec_open2(pCodecCtx, pCodec, nil) < 0 then
      begin
        DoNotify(ffocvErrorGetStream);
        FisReconnect := True;
        Continue;
      end;

      if pCodecCtx^.pix_fmt = AV_PIX_FMT_NONE then
      begin
        DoNotify(ffocvErrorGetStream);
        FisReconnect := True;
        Continue;
      end;

      img_convert_context := sws_getCachedContext(nil, pCodecCtx^.Width, pCodecCtx^.Height, pCodecCtx^.pix_fmt,
        pCodecCtx^.Width, pCodecCtx^.Height, AV_PIX_FMT_BGR24, SWS_BILINEAR, nil, nil, nil);
      if (img_convert_context = nil) then
      begin
        DoNotify(ffocvErrorGetStream);
        FisReconnect := True;
        Continue;
      end;

      frame := av_frame_alloc();
      iplframe := cvCreateImage(CvSize(pCodecCtx^.Width, pCodecCtx^.Height), IPL_DEPTH_8U, 3); // iplframe
      FillChar(linesize, SizeOf(linesize), 0);
      linesize[0] := iplframe^.widthStep;

      DoNotify(ffocvConnected);

      while (not Terminated) and (FSuspendEvent.WaitFor(0) = wrSignaled) and (not FisReconnect) do
      begin
        ret := av_read_frame(pFormatCtx, @packet);
        if ret >= 0 then
        begin
          try
            if (packet.stream_index = videoStream) then
            begin
              FOwner.DoNotifyPacket(packet, (packet.flags and AV_PKT_FLAG_KEY) <> 0);
              if avcodec_send_packet(pCodecCtx, @packet) >= 0 then
              begin
                repeat
                  recvRet := avcodec_receive_frame(pCodecCtx, frame);
                  if recvRet = 0 then
                  begin
                    sws_scale(img_convert_context, @frame^.data, @frame^.linesize, 0, pCodecCtx^.Height,
                      @iplframe^.imageData, @linesize);
                    if Assigned(OnNotifyData) then
                    begin
                      Image := TocvImage.CreateClone(iplframe);
                      OnNotifyData(FOwner, Image);
                      Image := nil;
                    end;
                  end;
                until recvRet <> 0;
              end;
            end;
          finally
            av_packet_unref(@packet);
          end;
        end
        else
        begin
          DoNotify(ffocvLostConnection);
          FisReconnect := True;
          Break;
        end;
      end;

      if (not Terminated) and FisReconnect and (FReconnectDelay > 0) and (FSuspendEvent.WaitFor(0) = wrSignaled) then
      begin

        DoNotify(ffocvReconnect);

        RDelay := 0;
        while (not Terminated) and (RDelay < FReconnectDelay) do
        begin
          Sleep(100);
          Inc(RDelay, 100);
        end;
        if Terminated then
          Break;
        FisReconnect := False;
      end;
    end;
  finally
    ReleaseAllocatedData;
    avformat_network_deinit;
  end;
end;

procedure TocvFFMpegIPCamSourceThread.SetIPCamUrl(const AIPCam: AnsiString; const AEnabled: Boolean);
begin
  if (FEnabled <> AEnabled) or (FIPCamURL <> AIPCam) then
  begin
    FSuspendEvent.ResetEvent;
    FisReconnect := True;
    FIPCamURL := AIPCam;
    FEnabled := AEnabled;
    if FEnabled then
      FSuspendEvent.SetEvent;
  end;
end;

{$IFDEF DELPHIXE2_UP}

procedure TocvFFMpegIPCamSourceThread.TerminatedSet;
begin
  inherited;
  FSuspendEvent.ResetEvent;
end;
{$ENDIF}

end.
