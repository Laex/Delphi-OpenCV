program filtering_video;

{$APPTYPE CONSOLE}
{$R *.res}
{$include ffmpeg.inc}

uses
  Winapi.Windows,
  System.SysUtils,
  ffmpeglib in '..\..\ffmpeglib.pas',
  ctypes in '..\..\..\ctypes\ctypes.pas',
  avformat in '..\..\libavformat\avformat.pas',
  avio in '..\..\libavformat\avio.pas',
  avutil in '..\..\libavutil\avutil.pas',
  buffer in '..\..\libavutil\buffer.pas',
  dict in '..\..\libavutil\dict.pas',
  frame in '..\..\libavutil\frame.pas',
  log in '..\..\libavutil\log.pas',
  opt in '..\..\libavutil\opt.pas',
  pixfmt in '..\..\libavutil\pixfmt.pas',
  rational in '..\..\libavutil\rational.pas',
  samplefmt in '..\..\libavutil\samplefmt.pas',
  parseutils in '..\..\libavutil\parseutils.pas',
  swscale in '..\..\libswscale\swscale.pas',
  pixdesc in '..\..\libavutil\pixdesc.pas',
  imgutils in '..\..\libavutil\imgutils.pas',
  mem in '..\..\libavutil\mem.pas',
  error in '..\..\libavutil\error.pas',
  avfilter in '..\..\libavfilter\avfilter.pas',
  buffersink in '..\..\libavfilter\buffersink.pas',
  mathematics in '..\..\libavutil\mathematics.pas',
  libavcodec.avcodec in '..\..\libavcodec\libavcodec.avcodec.pas',
  buffersrc in '..\..\libavfilter\buffersrc.pas',
  errno in '..\..\libavutil\errno.pas';

Const
  _XOPEN_SOURCE = 600; (* for usleep *)

Var
  filter_descr: pAnsiChar = 'scale=78:24';
  fmt_ctx: pAVFormatContext = nil;
  dec_ctx: pAVCodecContext = nil;
  buffersink_ctx: pAVFilterContext = nil;
  buffersrc_ctx: pAVFilterContext = nil;
  filter_graph: pAVFilterGraph = nil;
  video_stream_index: Integer = -1;
  last_pts: int64_t = AV_NOPTS_VALUE;

function open_input_file(const filename: pAnsiChar): Integer;
Var
  ret: Integer;
  dec: pAVCodec;
begin
  ret := avformat_open_input(fmt_ctx, filename, nil, nil);
  if (ret < 0) then
  begin
    // av_log(nil, AV_LOG_ERROR, 'Cannot open input file\n');
    Exit(ret);
  end;

  ret := avformat_find_stream_info(fmt_ctx, nil);
  if (ret < 0) then
  begin
    // av_log(nil, AV_LOG_ERROR, 'Cannot find stream information\n');
    Exit(ret);
  end;

  (* select the video stream *)
  ret := av_find_best_stream(fmt_ctx, AVMEDIA_TYPE_VIDEO, -1, -1, dec, 0);
  if (ret < 0) then
  begin
    // av_log(nil, AV_LOG_ERROR, 'Cannot find a video stream in the input file\n');
    Exit(ret);
  end;
  video_stream_index := ret;
  dec_ctx := fmt_ctx^.streams[video_stream_index]^.codec;

  (* init the video decoder *)
  ret := avcodec_open2(dec_ctx, dec, nil);
  if (ret < 0) then
  begin
    // av_log(nil, AV_LOG_ERROR, 'Cannot open video decoder\n');
    Exit(ret);
  end;

  Result := 0;
end;

function init_filters(const filters_descr: pAnsiChar): Integer;
Var
  args: AnsiString;
  ret: Integer;
  buffersrc: pAVFilter;
  buffersink: pAVFilter;
  outputs: pAVFilterInOut;
  inputs: pAVFilterInOut;
  pix_fmts: array [0 .. 1] of TAVPixelFormat;
  buffersink_params: pAVBufferSinkParams;
begin
  buffersrc := avfilter_get_by_name('buffer');
  buffersink := avfilter_get_by_name('buffersink');
  outputs := avfilter_inout_alloc();
  inputs := avfilter_inout_alloc();
  pix_fmts[0] := AV_PIX_FMT_GRAY8;
  pix_fmts[1] := AV_PIX_FMT_NONE;

  filter_graph := avfilter_graph_alloc();

  (* buffer video source: the decoded frames from the decoder will be inserted here. *)
  args := Format('video_size=%dx%d:pix_fmt=%d:time_base=%d/%d:pixel_aspect=%d/%d', [ //
    dec_ctx^.width, //
    dec_ctx^.height, //
    Integer(dec_ctx^.pix_fmt), //
    dec_ctx^.time_base.num, //
    dec_ctx^.time_base.den, //
    dec_ctx^.sample_aspect_ratio.num, //
    dec_ctx^.sample_aspect_ratio.den //
    ]);

  ret := avfilter_graph_create_filter(buffersrc_ctx, buffersrc, 'in', pAnsiChar(args), nil, filter_graph);
  if (ret < 0) then
  begin
    // av_log(nil, AV_LOG_ERROR, 'Cannot create buffer source\n');
    Exit(ret);
  end;

  (* buffer video sink: to terminate the filter chain. *)
  buffersink_params := av_buffersink_params_alloc();
  buffersink_params^.pixel_fmts := @pix_fmts;
  ret := avfilter_graph_create_filter(buffersink_ctx, buffersink, 'out', nil, buffersink_params, filter_graph);
  av_free(buffersink_params);
  if (ret < 0) then
  begin
    // av_log(nil, AV_LOG_ERROR, 'Cannot create buffer sink\n');
    Exit(ret);
  end;

  (* Endpoints for the filter graph. *)
  outputs^.name := av_strdup('in');
  outputs^.filter_ctx := buffersrc_ctx;
  outputs^.pad_idx := 0;
  outputs^.next := nil;

  inputs^.name := av_strdup('out');
  inputs^.filter_ctx := buffersink_ctx;
  inputs^.pad_idx := 0;
  inputs^.next := nil;

  ret := avfilter_graph_parse_ptr(filter_graph, filters_descr, inputs, outputs, nil);
  if (ret < 0) then
    Exit(ret);

  ret := avfilter_graph_config(filter_graph, nil);
  if (ret < 0) then
    Exit(ret);
  Result := 0;
end;

procedure display_frame(const vframe: pAVFrame; time_base: TAVRational);
Const
  ds: array[0..4] of char = ' .-+#';
Var
  x, y: Integer;
  p0, p: pByte;
  delay: int64;
  hConsole:THandle;
  coordScreen:TCOORD;
begin
  if (vframe^.pts <> AV_NOPTS_VALUE) then
  begin
    if (last_pts <> AV_NOPTS_VALUE) then
    begin
      (* sleep roughly the right amount of time;
        * usleep is in microseconds, just like AV_TIME_BASE. *)
      delay := av_rescale_q(vframe^.pts - last_pts, time_base, AV_TIME_BASE_Q);
      if (delay > 0) and (delay < 1000000) then
        sleep(delay);
    end;
    last_pts := vframe^.pts;
  end;

  (* Trivial ASCII grayscale display. *)
  p0 := vframe^.data[0];
//  Write(#33);
  hConsole := GetStdHandle(STD_OUTPUT_HANDLE);
  FillChar(coordScreen,SizeOf(coordScreen),0);
  SetConsoleCursorPosition(hConsole, coordScreen);
  for y := 0 to vframe^.height - 1 do
  begin
    p := p0;
    for x := 0 to vframe^.width - 1 do
    begin
      Write(ds[p^ div 52]);
      Inc(p);
    end;
    Writeln;
    p0 := p0 + vframe^.linesize[0];
  end;
end;

Var
  ret: Integer;
  packet: TAVPacket;
  vframe: pAVFrame = nil;
  filt_frame: pAVFrame = nil;
  got_frame: Integer;
  filename: AnsiString;
  buf: array [0 .. 1023] of ansichar;

begin
  try
    vframe := av_frame_alloc();
    filt_frame := av_frame_alloc();

    if (not Assigned(vframe)) or (not Assigned(filt_frame)) then
    begin
      Writeln('Could not allocate frame');
      Halt(1);
    end;
    if (ParamCount <> 1) then
    begin
      Writeln('Usage: ', ExtractFileName(ParamStr(0)), ' file');
      Halt(1);
    end;

    avcodec_register_all();
    av_register_all();
    avfilter_register_all();
    avformat_network_init;
    try
      filename := AnsiString(ParamStr(1));
      ret := open_input_file(pAnsiChar(filename));
      if (ret < 0) then
        Halt(1);
      ret := init_filters(pAnsiChar(filter_descr));
      if (ret < 0) then
        Halt(1);

      (* read all packets *)
      while True do
      begin
        ret := av_read_frame(fmt_ctx, packet);
        if (ret < 0) then
          break;

        if (packet.stream_index = video_stream_index) then
        begin
          avcodec_get_frame_defaults(vframe);
          got_frame := 0;
          ret := avcodec_decode_video2(dec_ctx, vframe, got_frame, @packet);
          if (ret < 0) then
          begin
            // av_log(nil, AV_LOG_ERROR, 'Error decoding video');
            break;
          end;

          if got_frame <> 0 then
          begin
            vframe^.pts := av_frame_get_best_effort_timestamp(vframe);

            (* push the decoded frame into the filtergraph *)
            if (av_buffersrc_add_frame_flags(buffersrc_ctx, vframe, AV_BUFFERSRC_FLAG_KEEP_REF) < 0) then
            begin
              // av_log(nil, AV_LOG_ERROR, 'Error while feeding the filtergraph\n');
              break;
            end;

            (* pull filtered frames from the filtergraph *)
            while True do
            begin
              ret := av_buffersink_get_frame(buffersink_ctx, filt_frame);
               if (ret = AVERROR(EAGAIN)) or (ret = AVERROR_EOF) then
                break;
              if (ret < 0) then
                Halt(1);
              display_frame(filt_frame, buffersink_ctx^.inputs[0]^.time_base);
              av_frame_unref(filt_frame);
            end;
          end;
        end;
        av_free_packet(packet);
      end;
    finally
      avformat_network_deinit;
      avfilter_graph_free(filter_graph);
      if Assigned(dec_ctx) then
        avcodec_close(dec_ctx);
      avformat_close_input(fmt_ctx);
      av_frame_free(vframe);
      av_frame_free(filt_frame);

      if (ret < 0)  and (ret <> AVERROR_EOF) then
      begin
        av_strerror(ret, buf, sizeof(buf));
        Writeln('Error occurred: ', buf);
        // Halt(1);
      end;
    end;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;

end.
