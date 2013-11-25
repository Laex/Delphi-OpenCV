program avcodec_sample;

{$APPTYPE CONSOLE}
{$R *.res}
{$include ffmpeg.inc}

uses
  Winapi.Windows,
  System.SysUtils,
  System.Classes,
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

function GetNextFrame(pFormatCtx: pAVFormatContext; pCodecCtx: pAVCodecContext; videoStream: Integer; pFrame: pAVFrame): Boolean;

label
  loop_exit;

Var
  packet: TAVPacket;
  bytesRemaining: Integer;
  rawData: pByte;
  fFirstTime: Boolean;
  bytesDecoded: Integer;
  frameFinished: Integer;
begin
  bytesRemaining := 0;
  fFirstTime := true;
  // First time we're called, set packet.data to nil to indicate it
  // doesn't have to be freed
  if (fFirstTime) then
  begin
    fFirstTime := false;
    packet.data := nil;
  end;

  // Decode packets until we have decoded a complete frame
  while (true) do
  begin
    // Work on the current packet until we have decoded all of it
    while (bytesRemaining > 0) do
    begin
      // Decode the next chunk of data
      bytesDecoded := avcodec_decode_video2(pCodecCtx, pFrame, frameFinished, @packet);

      // Was there an error?
      if (bytesDecoded < 0) then
      begin
        Writeln('Error while decoding frame');
        Exit(false);
      end;

      bytesRemaining := bytesRemaining - bytesDecoded;
      rawData := rawData + bytesDecoded;

      // Did we finish the current frame? Then we can return
      if (frameFinished = 0) then
        Exit(true);
    end;

    // Read the next packet, skipping all packets that aren't for this
    // stream
    repeat
      // Free old packet
      if Assigned(packet.data) then
        av_free_packet(packet);

      // Read new packet
      if (av_read_frame(pFormatCtx, packet) < 0) then
        goto loop_exit;
    until (packet.stream_index = videoStream);

    bytesRemaining := packet.size;
    rawData := packet.data;
  end;

loop_exit:

  // Decode the rest of the last frame
  bytesDecoded := avcodec_decode_video2(pCodecCtx, pFrame, frameFinished, @packet);

  // Free last packet
  if Assigned(packet.data) then
    av_free_packet(packet);

  Result := frameFinished <> 0;
end;

procedure SaveFrame(pFrame: pAVFrame; width: Integer; height: Integer; iFrame: Integer);
Var
  M: TFileStream;
  y: Integer;
  A: AnsiString;
begin
  // Open file
  M := TFileStream.Create(Format('frame%d.ppm', [iFrame]), fmCreate);
  // Write header
  A := Format('P6'#13#10'%d %d'#13#10'255'#13#10, [width, height]);
  M.WriteBuffer((@A[1])^, Length(A));

  // Write pixel data
  for y := 0 to height - 1 do
    M.Write(Pointer(Integer(@pFrame^.data[0]) + y * pFrame^.linesize[0])^, width * 3);
  // Close file
  M.Free;
end;

Var
  pFormatCtx: pAVFormatContext;
  i, videoStream: Integer;
  pCodecCtx: pAVCodecContext;
  pCodec: pAVCodec;
  pFrame: pAVFrame;
  pFrameRGB: pAVFrame;
  numBytes: Integer;
  buffer: pByte;

function open_input_file(const filename: pAnsiChar): Integer;
Var
  ret: Integer;
begin
  ret := avformat_open_input(pFormatCtx, filename, nil, nil);
  if (ret < 0) then
  begin
    // av_log(nil, AV_LOG_ERROR, 'Cannot open input file\n');
    Exit(ret);
  end;

  ret := avformat_find_stream_info(pFormatCtx, nil);
  if (ret < 0) then
  begin
    // av_log(nil, AV_LOG_ERROR, 'Cannot find stream information\n');
    Exit(ret);
  end;

  (* select the video stream *)
  ret := av_find_best_stream(pFormatCtx, AVMEDIA_TYPE_VIDEO, -1, -1, pCodec, 0);
  if (ret < 0) then
  begin
    // av_log(nil, AV_LOG_ERROR, 'Cannot find a video stream in the input file\n');
    Exit(ret);
  end;
  videoStream := ret;
  pCodecCtx := pFormatCtx^.streams[videoStream]^.codec;

  // Inform the codec that we can handle truncated bitstreams -- i.e.,
  // bitstreams where frame boundaries can fall in the middle of packets
  if (pCodec^.capabilities and CODEC_CAP_TRUNCATED) <> 0 then
    pCodecCtx^.flags := pCodecCtx^.flags or CODEC_FLAG_TRUNCATED;

  (* init the video decoder *)
  ret := avcodec_open2(pCodecCtx, pCodec, nil);
  if (ret < 0) then
  begin
    // av_log(nil, AV_LOG_ERROR, 'Cannot open video decoder\n');
    Exit(ret);
  end;

  Result := 0;
end;

begin
  try

    // Register all formats and codecs
    av_register_all();

    // Open video file
    if open_input_file(pAnsiChar(ParamStr(1))) <> 0 then
      Halt(1); // Couldn't open file

    // Allocate video frame
    pFrame := avcodec_alloc_frame();

    // Allocate an AVFrame structure
    pFrameRGB := avcodec_alloc_frame();
    if (pFrameRGB = nil) then
      Halt(1);

    // Determine required buffer size and allocate buffer
    numBytes := avpicture_get_size(AV_PIX_FMT_RGB24, pCodecCtx^.width, pCodecCtx^.height);
    buffer := AllocMem(numBytes);

    // Assign appropriate parts of buffer to image planes in pFrameRGB
    avpicture_fill(pAVPicture(pFrameRGB), buffer, AV_PIX_FMT_RGB24, pCodecCtx^.width, pCodecCtx^.height);

    // Read frames and save first five frames to disk
    i := 0;
    while GetNextFrame(pFormatCtx, pCodecCtx, videoStream, pFrame) do
    begin
      swr_convert(pAVPicture(pFrameRGB), AV_PIX_FMT_RGB24, pFrame, pCodecCtx^.pix_fmt, pCodecCtx^.width, pCodecCtx^.height);

      // Save the frame to disk
      Inc(i);
      if (i <= 5) then
        SaveFrame(pFrameRGB, pCodecCtx^.width, pCodecCtx^.height, i);
    end;

    // Free the RGB image
    FreeMem(buffer);
    av_free(pFrameRGB);

    // Free the YUV frame
    av_free(pFrame);

    // Close the codec
    avcodec_close(pCodecCtx);

    // Close the video file
    avformat_close_input(pFormatCtx);

  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;

end.
