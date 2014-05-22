program ffmpeg_sample_player;

{ .$APPTYPE CONSOLE }
{$R *.res}
{$i ffmpeg.inc}

uses
  Winapi.Windows,
  System.SysUtils,
  System.Classes,
  SDL,
  ffm.lib,
  ffm.ctypes,
  ffm.avformat,
  ffm.avio,
  ffm.avutil,
  ffm.buffer,
  ffm.dict,
  ffm.frame,
  ffm.log,
  ffm.opt,
  ffm.pixfmt,
  ffm.rational,
  ffm.samplefmt,
  ffm.parseutils,
  ffm.swscale,
  ffm.pixdesc,
  ffm.imgutils,
  ffm.mem,
  ffm.error,
  ffm.avfilter,
  ffm.buffersink,
  ffm.mathematics,
  ffm.libavcodec.avcodec,
  ffm.buffersrc,
  ffm.errno,
  uResourcePaths;

var
  err: Integer;
  filename: AnsiString;
  format_context: pAVFormatContext = nil;
  video_stream: Integer;
  codec_context: pAVCodecContext;
  codec: pAVCodec;
  screen: pSDL_Surface;
  bmp: pSDL_Overlay;
  img_convert_context: pSwsContext;
  frame: pAVFrame;
  packet: TAVPacket;
  frame_finished: Integer;
  pict: TAVPicture;
  rect: TSDL_Rect;
  event: TSDL_Event;

const
  std_filename = cResourceMedia + 'trailer.avi';

begin
  try
    if (ParamCount < 1) then
      filename := std_filename
    else
      filename := ParamStr(1);

    // Register all available file formats and codecs
    av_register_all();
    avformat_network_init();

    // Init SDL with video support
    err := SDL_Init(SDL_INIT_VIDEO);
    if (err < 0) then
    begin
      WriteLn(Format('Unable to init SDL: %s', [SDL_GetError()]));
      Halt(1);
    end;

    // Open video file
    err := avformat_open_input(format_context, PAnsiChar(filename), nil, nil);
    if (err < 0) then
    begin
      WriteLn('ffmpeg: Unable to open input file');
      Halt(1);
    end;

    // Retrieve stream information
    err := avformat_find_stream_info(format_context, nil);
    if (err < 0) then
    begin
      WriteLn('ffmpeg: Unable to find stream info');
      Halt(1);
    end;

    // Dump information about file onto standard error
    av_dump_format(format_context, 0, PAnsiChar(filename), 0);

    // Find the first video stream
    for video_stream := 0 to format_context^.nb_streams - 1 do
      if (format_context^.streams[video_stream]^.codec^.codec_type = AVMEDIA_TYPE_VIDEO) then
        break;
    if (video_stream = format_context^.nb_streams) then
    begin
      WriteLn('ffmpeg: Unable to find video stream');
      Halt(1);
    end;

    codec_context := format_context^.streams[video_stream]^.codec;
    codec := avcodec_find_decoder(codec_context^.codec_id);
    err := avcodec_open2(codec_context, codec, nil);
    if (err < 0) then
    begin
      WriteLn('ffmpeg: Unable to open codec');
      Halt(1);
    end;

    screen := SDL_SetVideoMode(codec_context^.width, codec_context^.height, 0, 0 { SDL_FULLSCREEN } );
    if (screen = nil) then
    begin
      WriteLn('Couldn''t set video mode');
      Halt(1);
    end;

    bmp := SDL_CreateYUVOverlay(codec_context^.width, codec_context^.height, SDL_YV12_OVERLAY, screen);

    img_convert_context := sws_getCachedContext(nil, codec_context^.width, codec_context^.height, codec_context^.pix_fmt, codec_context^.width, codec_context^.height, AV_PIX_FMT_YUV420P, SWS_BICUBIC,
      nil, nil, nil);
    if (img_convert_context = nil) then
    begin
      WriteLn('Cannot initialize the conversion context');
      Halt(1);
    end;

    frame := avcodec_alloc_frame();
    while (av_read_frame(format_context, packet) >= 0) do
    begin
      if (packet.stream_index = video_stream) then
      begin
        // Video stream packet
        avcodec_decode_video2(codec_context, frame, frame_finished, @packet);

        if (frame_finished <> 0) then
        begin
          SDL_LockYUVOverlay(bmp);

          // Convert frame to YV12 pixel format for display in SDL overlay
          pByte(pict.data[0]) := pByte(bmp^.pixels[0]);
          pByte(pict.data[1]) := pByte(bmp^.pixels[2]); // it's because YV12
          pByte(pict.data[2]) := pByte(bmp^.pixels[1]);

          pict.linesize[0] := bmp^.pitches[0];
          pict.linesize[1] := bmp^.pitches[2];
          pict.linesize[2] := bmp^.pitches[1];

          sws_scale(img_convert_context, @frame^.data, @frame^.linesize, 0, codec_context^.height, @pict.data, @pict.linesize);

          SDL_UnlockYUVOverlay(bmp);

          rect.x := 0;
          rect.y := 0;
          rect.w := codec_context^.width;
          rect.h := codec_context^.height;
          SDL_DisplayYUVOverlay(bmp, @rect);
        end;
      end;

      // Free the packet that was allocated by av_read_frame
      av_free_packet(packet);

      // Handling SDL events there
      if SDL_PollEvent(@event) <> 0 then
        if (event.type_ = SDL_QUITEV) then
          break;
    end;

    sws_freeContext(img_convert_context);

    // Free the YUV frame
    av_free(frame);

    // Close the codec
    avcodec_close(codec_context);

    // Close the video file
    avformat_close_input(&format_context);

    avformat_network_deinit;

    // Quit SDL
    SDL_QUIT();

  except
    on E: Exception do
      WriteLn(E.ClassName, ': ', E.Message);
  end;

end.
