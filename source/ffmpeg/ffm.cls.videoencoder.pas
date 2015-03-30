unit ffm.cls.videoencoder;

{$I OpenCV.inc}
{$POINTERMATH ON}

interface

Uses
  ffm.libavcodec.avcodec,
  ffm.pixfmt,
  ffm.avformat,
  ffm.swscale,
  ffm.avio,
  ffm.frame;

Type
  TFFMVideoEncoder = class
  private const
    MAX_AUDIO_PACKET_SIZE = (128 * 1024);
  private
    // output file name
    outputFilename: AnsiString;
    // output format.
    pOutFormat: pAVOutputFormat;
    // format context
    pFormatContext: pAVFormatContext;
    // video stream context
    pVideoStream: pAVStream;
    // audio streams context
    pAudioStream: pAVStream;
    // convert context context
    pImgConvertCtx: pSwsContext;
    // encode buffer and size
    pVideoEncodeBuffer: pByte;
    nSizeVideoEncodeBuffer: Integer;
    // audio buffer and size
    pAudioEncodeBuffer: pByte;
    nSizeAudioEncodeBuffer: Integer;
    // count of sample
    audioInputSampleSize: Integer;
    // current picture
    pCurrentPicture: pAVFrame;

    // audio buffer. Save rest samples in audioBuffer from previous audio frame.
    audioBuffer: pByte;
    nAudioBufferSize: Integer;
    nAudioBufferSizeCurrent: Integer;
    W_VIDEO: Integer;
    H_VIDEO: Integer;
    fbit_rate: Integer;
    ftime_base_den: Integer;
    function flush_encoder: Integer;
  protected
    // Add video stream
    function AddVideoStream(const pContext: pAVFormatContext; const codec_id: TAVCodecID): pAVStream;
    // Open Video Stream
    function OpenVideo(const oc: pAVFormatContext; const pStream: pAVStream): boolean;
    // Allocate memory
    function CreateFFmpegPicture(const pix_fmt: TAVPixelFormat; const nWidth, nHeight: Integer): pAVFrame;
    // Close video stream
    procedure CloseVideo(const pContext: pAVFormatContext; const pStream: pAVStream);
    // Add audio stream
    function AddAudioStream(const pContext: pAVFormatContext; const codec_id: TAVCodecID): pAVStream;
    // Open audio stream
    function OpenAudio(const pContext: pAVFormatContext; const pStream: pAVStream): boolean;
    // close audio stream
    procedure CloseAudio(const pContext: pAVFormatContext; const pStream: pAVStream);
    // Add video frame
    function AddVideoFrame(const pOutputFrame: pAVFrame; const pVideoCodec: pAVCodecContext): boolean;
    // Add audio samples
    function AddAudioSample(const pFormatContext: pAVFormatContext; const pStream: pAVStream; const soundBuffer: pByte;
      const soundBufferSize: Integer): boolean;
    // Free resourses.
    procedure Free;
    function NeedConvert(const framepixfmt: TAVPixelFormat): boolean;
  public
    constructor Create;
    destructor Destroy; override;
    // init output file
    function InitFile(const inputFile: AnsiString; const container: AnsiString; const AW_VIDEO: Integer = 320;
      const AH_VIDEO: Integer = 200): boolean;
    // Set video params
    procedure SetVideoParams(const atime_base_den: Integer = 25; const abit_rate: Integer = 2000000);
    // Add video and audio data
    function AddFrame(const frame: pAVFrame; const soundBuffer: pByte; const soundBufferSize: Integer;
      const framepixfmt: TAVPixelFormat = AV_PIX_FMT_RGB24): boolean;
    // end of output
    function Finish: boolean;
  end;

implementation

Uses
{$IFDEF MSWINDOWS}
  Winapi.Windows,
{$ENDIF MSWINDOWS}
  System.SysUtils,
  System.AnsiStrings,
  System.Math,
  ffm.mem,
  ffm.avutil,
  ffm.samplefmt,
  ffm.mathematics;

{ TNVRVideoEncoder }

function TFFMVideoEncoder.AddAudioSample(const pFormatContext: pAVFormatContext; const pStream: pAVStream; const soundBuffer: pByte;
  const soundBufferSize: Integer): boolean;
Var
  pCodecCxt: pAVCodecContext;
  packSizeInSize: Integer; // DWORD;
  nCountSamples: Integer;
  nCurrentSize: Integer;
  // nWriteSamples: Integer;
  pSoundBuffer: pByte;
  pAudioFrame: pAVFrame;
  // nBufferShift: Integer;
  nCurrentBufferSize: Integer;
  pkt: TAVPacket;
  nOutputSize, error: Integer;
begin

  // pCodecCxt := nil;
  Result := true;

  pCodecCxt := pStream^.codec;

  // Size of packet on bytes.
  // FORMAT s16
  packSizeInSize := soundBufferSize;

  nCountSamples := soundBufferSize div av_get_bytes_per_sample(AV_SAMPLE_FMT_S16);

  // Add current audio frame to previos.
  CopyMemory(@audioBuffer[nAudioBufferSizeCurrent], soundBuffer, soundBufferSize);
  nAudioBufferSizeCurrent := nAudioBufferSizeCurrent + soundBufferSize;

  nCurrentSize := nAudioBufferSizeCurrent;
  // nWriteSamples := 0;
  pSoundBuffer := audioBuffer;

  while (nCurrentSize >= packSizeInSize) do
  begin
    pAudioFrame := nil;

    pAudioFrame := avcodec_alloc_frame();

    // Audio frame should be equal or smaller pCodecCxt^.frame_size.
    pAudioFrame^.nb_samples := min(pCodecCxt^.frame_size div av_get_bytes_per_sample(AV_SAMPLE_FMT_S16), nCountSamples);
    // nBufferShift := nWriteSamples * av_get_bytes_per_sample(AV_SAMPLE_FMT_S16);
    nCurrentBufferSize := pAudioFrame^.nb_samples * av_get_bytes_per_sample(AV_SAMPLE_FMT_S16);

    if avcodec_fill_audio_frame(pAudioFrame, 1, AV_SAMPLE_FMT_S16, pSoundBuffer, nCurrentBufferSize, 1) <> 0 then
    begin
      Result := false;
      break;
    end;

    av_init_packet(@pkt);

    pkt.flags := pkt.flags or AV_PKT_FLAG_KEY;
    pkt.stream_index := pStream^.index;
    pkt.data := pAudioEncodeBuffer;
    pkt.size := nSizeAudioEncodeBuffer;

    nOutputSize := 0;
    // Create encoded packet from audio frame.
    error := avcodec_encode_audio2(pCodecCxt, @pkt, pAudioFrame, nOutputSize);

    if (error = 0) and (nOutputSize > 0) then
    begin
      if Assigned(pCodecCxt^.coded_frame) and (pCodecCxt^.coded_frame^.pts <> AV_NOPTS_VALUE) then
        pkt.pts := av_rescale_q(pCodecCxt^.coded_frame^.pts, pCodecCxt^.time_base, pStream^.time_base);

      pkt.stream_index := pStream^.index;

      // Write the compressed frame in the media file.
      if (av_interleaved_write_frame(pFormatContext, @pkt) <> 0) then
      begin
        Result := false;
        break;
      end;
    end;

    nCurrentSize := nCurrentSize - nCurrentBufferSize;
    pSoundBuffer := pSoundBuffer + nCurrentBufferSize;

    // nWriteSamples := nWriteSamples + pAudioFrame^.nb_samples;
    avcodec_free_frame(pAudioFrame);
  end;

  // save excess
  CopyMemory(audioBuffer, @audioBuffer[nAudioBufferSizeCurrent - nCurrentSize], nCurrentSize);
  nAudioBufferSizeCurrent := nCurrentSize;
end;

function TFFMVideoEncoder.AddAudioStream(const pContext: pAVFormatContext; const codec_id: TAVCodecID): pAVStream;
Var
  pCodecCxt: pAVCodecContext;
begin
  // pCodecCxt := nil;
  // Result := nil;
  // Try create stream.
  Result := avformat_new_stream(pContext, nil);
  if not Assigned(Result) then
  begin
    // printf("Cannot add new audio stream\n");
    Exit(nil);
  end;
  // Codec.
  pCodecCxt := Result^.codec;
  pCodecCxt^.codec_id := codec_id;
  pCodecCxt^.codec_type := AVMEDIA_TYPE_AUDIO;
  // Set format
  pCodecCxt^.bit_rate := 128000;
  pCodecCxt^.sample_rate := 44100;
  pCodecCxt^.channels := 1; // mono
  pCodecCxt^.sample_fmt := AV_SAMPLE_FMT_S16;

  nSizeAudioEncodeBuffer := 4 * MAX_AUDIO_PACKET_SIZE;
  if not Assigned(pAudioEncodeBuffer) then
    pAudioEncodeBuffer := av_malloc(nSizeAudioEncodeBuffer);

  // Some formats want stream headers to be separate.
  if (pContext^.oformat^.flags and AVFMT_GLOBALHEADER) <> 0 then
    pCodecCxt^.flags := pCodecCxt^.flags or CODEC_FLAG_GLOBAL_HEADER;
end;

function TFFMVideoEncoder.AddFrame(const frame: pAVFrame; const soundBuffer: pByte; const soundBufferSize: Integer;
  const framepixfmt: TAVPixelFormat): boolean;
Var
  // nOutputSize: Integer;
  pVideoCodec: pAVCodecContext;
begin
  Result := true;
  // nOutputSize := 0;
  // pVideoCodec := nil;

  if Assigned(pVideoStream) and Assigned(frame) and Assigned(frame^.data[0]) then
  begin
    pVideoCodec := pVideoStream^.codec;

    if NeedConvert(framepixfmt) then
    begin
      // RGB to YUV420P.
      if not Assigned(pImgConvertCtx) then
      begin
        pImgConvertCtx := sws_getContext(pVideoCodec^.width, pVideoCodec^.height, framepixfmt, pVideoCodec^.width, pVideoCodec^.height,
          pVideoCodec^.pix_fmt, SWS_BICUBLIN, nil, nil, nil);
      end;

      // Allocate picture.
      pCurrentPicture := CreateFFmpegPicture(pVideoCodec^.pix_fmt, pVideoCodec^.width, pVideoCodec^.height);

      // if NeedConvert(framepixfmt) and Assigned(pImgConvertCtx) then
      // begin
      // Convert RGB to YUV.
      sws_scale(pImgConvertCtx, @frame^.data, @frame^.linesize, 0, pVideoCodec^.height, @pCurrentPicture^.data, @pCurrentPicture^.linesize);
      // end;

      Result := AddVideoFrame(pCurrentPicture, pVideoStream^.codec);

      // Free temp frame
      av_free(pCurrentPicture^.data[0]);
      av_free(pCurrentPicture);
      pCurrentPicture := nil;
    end
    else
      Result := AddVideoFrame(frame, pVideoStream^.codec);
  end;

  // Add sound
  if Assigned(soundBuffer) and (soundBufferSize > 0) then
    Result := AddAudioSample(pFormatContext, pAudioStream, soundBuffer, soundBufferSize);
end;

function TFFMVideoEncoder.AddVideoFrame(const pOutputFrame: pAVFrame; const pVideoCodec: pAVCodecContext): boolean;
Var
  pkt: TAVPacket;
  packet: TAVPacket;
  nOutputSize: Integer;
  error: Integer;
begin

  // Result := false;

  if (pFormatContext^.oformat^.flags and AVFMT_RAWPICTURE) <> 0 then
  begin
    // Raw video case. The API will change slightly in the near
    // future for that.
    av_init_packet(@pkt);

    pkt.flags := pkt.flags or AV_PKT_FLAG_KEY;
    pkt.stream_index := pVideoStream^.index;
    pkt.data := pByte(pOutputFrame);
    pkt.size := sizeof(TAVPicture);

    av_interleaved_write_frame(pFormatContext, @pkt);
    Result := true;
  end
  else
  begin
    // Encode
    av_init_packet(@packet);
    packet.data := pVideoEncodeBuffer;
    packet.size := nSizeVideoEncodeBuffer;

    nOutputSize := 0;
    // Encode frame to packet.
    error := avcodec_encode_video2(pVideoCodec, @packet, pOutputFrame, nOutputSize);
    if (error = 0) and (nOutputSize > 0) then
    begin
      // AVPacket pkt;
      av_init_packet(@pkt);
      if Assigned(pVideoCodec^.coded_frame) and (pVideoCodec^.coded_frame^.pts <> AV_NOPTS_VALUE) then
        pkt.pts := AV_NOPTS_VALUE;

      if Assigned(pVideoCodec^.coded_frame) and (pVideoCodec^.coded_frame^.key_frame <> 0) then
        pkt.flags := pkt.flags or AV_PKT_FLAG_KEY;
      pkt.stream_index := pVideoStream^.index;
      pkt.data := pVideoEncodeBuffer;
      pkt.size := packet.size;

      // Write packet with frame.
      Result := av_interleaved_write_frame(pFormatContext, @pkt) = 0;
      // Result := av_write_frame(pFormatContext, @pkt) = 0;
    end
    else
      Result := false;
  end;
end;

function TFFMVideoEncoder.AddVideoStream(const pContext: pAVFormatContext; const codec_id: TAVCodecID): pAVStream;
Var
  pCodecCxt: pAVCodecContext;
begin
  // pCodecCxt := nil;
  // Result := nil;

  Result := avformat_new_stream(pContext, nil);
  if not Assigned(Result) then
  begin
    // printf("Cannot add new vidoe stream\n");
    Exit(nil);
  end;

  pCodecCxt := Result^.codec;
  pCodecCxt^.codec_id := codec_id;
  pCodecCxt^.codec_type := AVMEDIA_TYPE_VIDEO;
  pCodecCxt^.frame_number := 0;
  // Put sample parameters.
  pCodecCxt^.bit_rate := fbit_rate; // 2000000;
  // Resolution must be a multiple of two.
  pCodecCxt^.width := W_VIDEO;
  pCodecCxt^.height := H_VIDEO;
  (* time base: this is the fundamental unit of time (in seconds) in terms
    of which frame timestamps are represented. for fixed-fps content,
    timebase should be 1/framerate and timestamp increments should be
    identically 1. *)
  pCodecCxt^.time_base.den := ftime_base_den; // 25;
  pCodecCxt^.time_base.num := 1;
  pCodecCxt^.gop_size := 12; // emit one intra frame every twelve frames at most

  pCodecCxt^.pix_fmt := AV_PIX_FMT_YUV420P;
  if (pCodecCxt^.codec_id = AV_CODEC_ID_MPEG2VIDEO) then
  begin
    // Just for testing, we also add B frames
    pCodecCxt^.max_b_frames := 2;
  end;
  if (pCodecCxt^.codec_id = AV_CODEC_ID_MPEG1VIDEO) then
  begin
    (* Needed to avoid using macroblocks
      in which some coeffs overflow.This does not happen with normal video, it just happens here as the motion of
      the chroma plane does not match the luma plane. *)
    pCodecCxt^.mb_decision := 2;
  end;

  // Some formats want stream headers to be separate.
  if (pContext^.oformat^.flags and AVFMT_GLOBALHEADER) <> 0 then
  begin
    pCodecCxt^.flags := pCodecCxt^.flags or CODEC_FLAG_GLOBAL_HEADER;
  end;
end;

procedure TFFMVideoEncoder.CloseAudio(const pContext: pAVFormatContext; const pStream: pAVStream);
begin
  avcodec_close(pStream^.codec);
  if Assigned(pAudioEncodeBuffer) then
  begin
    av_free(pAudioEncodeBuffer);
    pAudioEncodeBuffer := Nil;
  end;
  nSizeAudioEncodeBuffer := 0;
end;

procedure TFFMVideoEncoder.CloseVideo(const pContext: pAVFormatContext; const pStream: pAVStream);
begin
  avcodec_close(pStream^.codec);
  if Assigned(pCurrentPicture) then
  begin
    if Assigned(pCurrentPicture^.data[0]) then
    begin
      av_free(pCurrentPicture^.data[0]);
      pCurrentPicture^.data[0] := nil;
    end;
    av_free(pCurrentPicture);
    pCurrentPicture := nil;
  end;

  if Assigned(pVideoEncodeBuffer) then
  begin
    av_free(pVideoEncodeBuffer);
    pVideoEncodeBuffer := nil;
  end;
  nSizeVideoEncodeBuffer := 0;
end;

constructor TFFMVideoEncoder.Create;
begin
  pOutFormat := nil;
  pFormatContext := nil;
  pVideoStream := nil;
  pImgConvertCtx := nil;
  pCurrentPicture := nil;
  pVideoEncodeBuffer := nil;
  nSizeVideoEncodeBuffer := 0;
  pAudioEncodeBuffer := nil;
  nSizeAudioEncodeBuffer := 0;
  nAudioBufferSize := 1024 * 1024 * 4;
  audioBuffer := AllocMem(nAudioBufferSize);
  nAudioBufferSizeCurrent := 0;
  SetVideoParams;
end;

function TFFMVideoEncoder.CreateFFmpegPicture(const pix_fmt: TAVPixelFormat; const nWidth, nHeight: Integer): pAVFrame;
Var
  picture_buf: pByte;
  size: Integer;
begin
  // picture_buf := nil;
  Result := avcodec_alloc_frame();
  if not Assigned(Result) then
  begin
    // printf("Cannot create frame\n");
    Exit(nil);
  end;

  size := avpicture_get_size(pix_fmt, nWidth, nHeight);

  picture_buf := av_malloc(size);

  if not Assigned(picture_buf) then
  begin
    av_free(Result);
    // printf("Cannot allocate buffer\n");
    Exit(nil);
  end;
  avpicture_fill(pAVPicture(Result), picture_buf, pix_fmt, nWidth, nHeight);
end;

destructor TFFMVideoEncoder.Destroy;
begin
  Finish;
  inherited;
end;

function TFFMVideoEncoder.Finish: boolean;
begin
  Result := true;
  // Todo: Maybe you need write audio samples from audioBuffer to file before cloasing.
  if Assigned(pFormatContext) then
  begin
    // flush_encoder;
    av_write_trailer(pFormatContext);
    Free;
  end;

  if Assigned(audioBuffer) then
  begin
    FreeMem(audioBuffer);
    audioBuffer := nil;
  end;
end;

function TFFMVideoEncoder.flush_encoder: Integer;
Var
  ret, got_output: Integer;
  pkt: TAVPacket;
begin
  Result := 0;
  (* get the delayed frames *)
  av_init_packet(@pkt);
  got_output := 1;
  While got_output <> 0 do
  begin
    ret := avcodec_encode_video2(pVideoStream^.codec, @pkt, nil, got_output);
    if (ret < 0) then
    begin
      // WriteLn('Error encoding frame');
      Exit(ret);
    end;
    if (got_output <> 0) then
    begin
      // WriteLn(format('Write frame %3d (size=%5d)', [i, pkt.size]));
      // BlockWrite(f, pkt.data^, pkt.size);
      av_free_packet(pkt);
    end;
    // Inc(i);
  end;
end;

procedure TFFMVideoEncoder.Free;
Var
  i: Integer;
begin
  if Assigned(pFormatContext) then
  begin
    // close video stream
    if Assigned(pVideoStream) then
      CloseVideo(pFormatContext, pVideoStream);

    // close audio stream.
    if Assigned(pAudioStream) then
      CloseAudio(pFormatContext, pAudioStream);

    // Free the streams.
    for i := 0 to pFormatContext^.nb_streams - 1 do
    begin
      av_freep(pFormatContext^.streams[i]^.codec);
      // av_freep(pFormatContext^.streams[i]);
    end;

    if ((pFormatContext^.flags and AVFMT_NOFILE) = 0) and Assigned(pFormatContext^.pb) then
      avio_close(pFormatContext^.pb);

    // Free the stream.
    av_free(pFormatContext);
    pFormatContext := nil;
  end;
end;

function TFFMVideoEncoder.InitFile(const inputFile, container: AnsiString; const AW_VIDEO, AH_VIDEO: Integer): boolean;
Var
  filename: PAnsiChar;
begin
  Result := false;
  W_VIDEO := AW_VIDEO;
  H_VIDEO := AH_VIDEO;
  filename := PAnsiChar(inputFile);
  outputFilename := inputFile;

  // Initialize libavcodec
  av_register_all();
  if System.AnsiStrings.SameText(container, 'auto') then
    // Create format
    pOutFormat := av_guess_format(nil, filename, nil)
  else
    // use contanier
    pOutFormat := av_guess_format(PAnsiChar(container), nil, nil);

  if Assigned(pOutFormat) then
  begin
    // allocate context
    pFormatContext := avformat_alloc_context();
    if Assigned(pFormatContext) then
    begin
      pFormatContext^.oformat := pOutFormat;
      CopyMemory(@pFormatContext^.filename, filename, min(System.AnsiStrings.strlen(filename), sizeof(pFormatContext^.filename)));

      // Add video and audio stream
      pVideoStream := AddVideoStream(pFormatContext, pOutFormat^.video_codec);
      pAudioStream := AddAudioStream(pFormatContext, pOutFormat^.audio_codec);

      // Set the output parameters (must be done even if no parameters).
      av_dump_format(pFormatContext, 0, filename, 1);

      // Open Video and Audio stream
      Result := false;
      if Assigned(pVideoStream) then
        Result := OpenVideo(pFormatContext, pVideoStream);

      if Assigned(pAudioStream) then
        Result := OpenAudio(pFormatContext, pAudioStream);

      if Result and ((pOutFormat^.flags and AVFMT_NOFILE) = 0) and (avio_open(pFormatContext^.pb, filename, AVIO_FLAG_WRITE) < 0) then
      begin
        Result := false;
        // printf(" Cannot open file\ n ");
      end;

      if Result then
        // Write file header.
        avformat_write_header(pFormatContext, nil);
    end;
  end;

  if not Result then
  begin
    Free();
    // printf(" Cannot init file\ n ");
  end;
end;

function TFFMVideoEncoder.NeedConvert(const framepixfmt: TAVPixelFormat): boolean;
begin
  if Assigned(pVideoStream) and Assigned(pVideoStream^.codec) then
    Result := pVideoStream^.codec^.pix_fmt <> framepixfmt // AV_PIX_FMT_RGB24
  else
    Result := false;
end;

function TFFMVideoEncoder.OpenAudio(const pContext: pAVFormatContext; const pStream: pAVStream): boolean;
Var
  pCodecCxt: pAVCodecContext;
  pCodec: pAVCodec;
begin
  // pCodecCxt := nil;
  // pCodec := nil;
  pCodecCxt := pStream^.codec;
  // Find the audio encoder.
  pCodec := avcodec_find_encoder(pCodecCxt^.codec_id);
  if not Assigned(pCodec) then
  begin
    // printf("Cannot open audio codec\n");
    Exit(false);
  end;
  // Open it.
  if (avcodec_open2(pCodecCxt, pCodec, nil) < 0) then
  begin
    // printf("Cannot open audio codec\n");
    Exit(false);
  end;

  if (pCodecCxt^.frame_size <= 1) then
  begin
    // Ugly hack for PCM codecs (will be removed ASAP with new PCM
    // support to compute the input frame size in samples.
    audioInputSampleSize := nSizeAudioEncodeBuffer div pCodecCxt^.channels;
    case pStream^.codec^.codec_id of
      AV_CODEC_ID_PCM_S16LE, AV_CODEC_ID_PCM_S16BE, AV_CODEC_ID_PCM_U16LE, AV_CODEC_ID_PCM_U16BE:
        audioInputSampleSize := audioInputSampleSize shr 1;
    end;
    pCodecCxt^.frame_size := audioInputSampleSize;
  end
  else
    audioInputSampleSize := pCodecCxt^.frame_size;
  Result := true;
end;

function TFFMVideoEncoder.OpenVideo(const oc: pAVFormatContext; const pStream: pAVStream): boolean;
Var
  pCodec: pAVCodec;
  pContext: pAVCodecContext;
begin
  pContext := pStream^.codec;
  // Find the video encoder.
  pCodec := avcodec_find_encoder(pContext^.codec_id);
  if not Assigned(pCodec) then
  begin
    // printf("Cannot found video codec\n");
    Exit(false);
  end;

  // Open the codec.
  if (avcodec_open2(pContext, pCodec, nil) < 0) then
  begin
    // printf("Cannot open video codec\n");
    Exit(false);
  end;

  pVideoEncodeBuffer := nil;
  if (pFormatContext^.oformat^.flags and AVFMT_RAWPICTURE) = 0 then
  begin
    // allocate output buffer
    nSizeVideoEncodeBuffer := 10000000;
    pVideoEncodeBuffer := av_malloc(nSizeVideoEncodeBuffer);
  end;
  Result := true;
end;

procedure TFFMVideoEncoder.SetVideoParams(const atime_base_den, abit_rate: Integer);
begin
  fbit_rate := abit_rate;
  ftime_base_den := atime_base_den;
end;

end.
