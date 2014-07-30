(*
  * Copyright (c) 2001 Fabrice Bellard
  *
  * Permission is hereby granted, free of charge, to any person obtaining a copy
  * of this software and associated documentation files (the "Software"), to deal
  * in the Software without restriction, including without limitation the rights
  * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
  * copies of the Software, and to permit persons to whom the Software is
  * furnished to do so, subject to the following conditions:
  *
  * The above copyright notice and this permission notice shall be included in
  * all copies or substantial portions of the Software.
  *
  * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
  * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
  * THE SOFTWARE.
*)
(* *
  * @file
  * libavcodec API use example.
  *
  * @example decoding_encoding.c
  * Note that libavcodec only handles codecs (mpeg, mpeg4, etc...),
  * not file formats (avi, vob, mp4, mov, mkv, mxf, flv, mpegts, mpegps, etc...). See library 'libavformat' for the
  * format handling
*)

program decoding_encoding;

{$APPTYPE CONSOLE}
{$POINTERMATH ON}
{$R *.res}

uses
  System.SysUtils,
  System.Math,
  ffm.libavcodec.avcodec,
  ffm.pixfmt,
  ffm.avformat,
  ffm.samplefmt,
  ffm.swscale,
  ffm.avio,
  ffm.frame,
  ffm.channel_layout,
  ffm.ctypes,
  ffm.mem,
  ffm.mathematics,
  ffm.avutil,
  ffm.opt,
  ffm.imgutils,
  ffm.log;

Const
  INBUF_SIZE = 4096;
  AUDIO_INBUF_SIZE = 20480;
  AUDIO_REFILL_THRESH = 4096;

  (* check that a given sample format is supported by the encoder *)
function check_sample_fmt(const codec: pAVCodec; const sample_fmt: TAVSampleFormat): Boolean;
Var
  p: pAVSampleFormat;
begin
  p := codec^.sample_fmts;
  while (p^ <> AV_SAMPLE_FMT_NONE) do
  begin
    if (p^ = sample_fmt) then
      Exit(True);
    inc(p);
  end;
  Result := False;
end;

(* just pick the highest supported samplerate *)
function select_sample_rate(const codec: pAVCodec): Integer;
Var
  p: pInteger;
  best_samplerate: Integer;
begin
  best_samplerate := 0;
  if not Assigned(codec^.supported_samplerates) then
    Exit(44100);
  p := codec^.supported_samplerates;
  while (p^ <> 0) do
  begin
    best_samplerate := MAX(p^, best_samplerate);
    inc(p);
  end;
  Result := best_samplerate;
end;

(* select layout with the highest channel count *)
function select_channel_layout(codec: pAVCodec): Integer;
Var
  p: PUInt64_t;
  best_ch_layout: UInt64;
  best_nb_channels: Integer;
  nb_channels: Integer;
begin
  best_ch_layout := 0;
  best_nb_channels := 0;
  if not Assigned(codec^.channel_layouts) then
    Exit(AV_CH_LAYOUT_STEREO);
  p := codec^.channel_layouts;
  while (p^ <> 0) do
  begin
    nb_channels := av_get_channel_layout_nb_channels(p^);
    if (nb_channels > best_nb_channels) then
    begin
      best_ch_layout := p^;
      best_nb_channels := nb_channels;
    end;
    inc(p);
  end;
  Result := best_ch_layout;
end;

(*
  * Audio encoding example
*)
procedure audio_encode_example(const filename: String);
Var
  codec: pAVCodec;
  c: pAVCodecContext;
  frame: pAVFrame;
  pkt: TAVPacket;
  i, j, k, ret, got_output: Integer;
  buffer_size: Integer;
  f: File;
  samples: ^Int16;
  t, tincr: Single;
begin
  c := nil;
  WriteLn('Encode audio file ', filename);
  (* find the MP2 encoder *)
  codec := avcodec_find_encoder(AV_CODEC_ID_MP2);
  if not Assigned(codec) then
  begin
    WriteLn('Codec not found');
    Exit;
  end;
  c := avcodec_alloc_context3(codec);
  if not Assigned(c) then
  begin
    WriteLn('Could not allocate audio codec context\n');
    Exit;
  end;
  (* put sample parameters *)
  c^.bit_rate := 64000;
  (* check that the encoder supports s16 pcm input *)
  c^.sample_fmt := AV_SAMPLE_FMT_S16;
  if not check_sample_fmt(codec, c^.sample_fmt) then
  begin
    WriteLn('Encoder does not support sample format ', av_get_sample_fmt_name(c^.sample_fmt));
    Exit;
  end;
  (* select other audio parameters supported by the encoder *)
  c^.sample_rate := select_sample_rate(codec);
  c^.channel_layout := select_channel_layout(codec);
  c^.channels := av_get_channel_layout_nb_channels(c^.channel_layout);
  (* open it *)
  if (avcodec_open2(c, codec, nil) < 0) then
  begin
    WriteLn('Could not open codec');
    Exit;
  end;
  AssignFile(f, filename);
  try
    Rewrite(f, 1);
  except
    WriteLn('Could not open', filename);
    Exit;
  end;
  (* frame containing input raw audio *)
  frame := av_frame_alloc();
  if not Assigned(frame) then
  begin
    WriteLn('Could not allocate audio frame');
    Exit;
  end;
  frame^.nb_samples := c^.frame_size;
  frame^.format := Integer(c^.sample_fmt);
  frame^.channel_layout := c^.channel_layout;
  (* the codec gives us the frame size, in samples,
    * we calculate the size of the samples buffer in bytes *)
  buffer_size := av_samples_get_buffer_size(nil, c^.channels, c^.frame_size, c^.sample_fmt, 0);
  if (buffer_size < 0) then
  begin
    WriteLn('Could not get sample buffer size');
    Exit;
  end;
  samples := av_malloc(buffer_size);
  if not Assigned(samples) then
  begin
    WriteLn('Could not allocate %d bytes for samples buffer\n', buffer_size);
    Exit;
  end;
  (* setup the data pointers in the AVFrame *)
  ret := avcodec_fill_audio_frame(frame, c^.channels, c^.sample_fmt, pByte(samples), buffer_size, 0);
  if (ret < 0) then
  begin
    WriteLn('Could not setup audio frame');
    Exit;
  end;
  (* encode a single tone sound *)
  t := 0;
  tincr := 2 * M_PI * 440.0 / c^.sample_rate;
  for i := 0 to 199 do
  begin
    av_init_packet(@pkt);
    pkt.data := nil; // packet data will be allocated by the encoder
    pkt.size := 0;
    for j := 0 to c^.frame_size - 1 do
    begin
      samples[2 * j] := Trunc((sin(t) * 10000));
      for k := 1 to c^.channels - 1 do
        samples[2 * j + k] := samples[2 * j];
      t := t + tincr;
    end;
    (* encode the samples *)
    ret := avcodec_encode_audio2(c, @pkt, frame, got_output);
    if (ret < 0) then
    begin
      WriteLn('Error encoding audio frame');
      Exit;
    end;
    if (got_output <> 0) then
    begin
      BlockWrite(f, pkt.data^, pkt.size);
      av_free_packet(pkt);
    end;
  end;
  (* get the delayed frames *)
  got_output := 1;
  while got_output <> 0 do
  // ; i++)
  begin
    ret := avcodec_encode_audio2(c, @pkt, nil, got_output);
    if (ret < 0) then
    begin
      WriteLn('Error encoding frame');
      Exit;
    end;
    if (got_output <> 0) then
    begin
      BlockWrite(f, pkt.data^, pkt.size);
      av_free_packet(pkt);
    end;
    inc(i);
  end;
  Close(f);
  av_freep(samples);
  av_frame_free(frame);
  avcodec_close(c);
  av_free(c);
end;

(*
  * Audio decoding.
*)
procedure audio_decode_example(const outfilename: String; const filename: String);
Var
  codec: pAVCodec;
  c: pAVCodecContext;
  len: Integer;
  f, outfile: File;
  inbuf: array [0 .. AUDIO_INBUF_SIZE + FF_INPUT_BUFFER_PADDING_SIZE - 1] of byte;
  avpkt: TAVPacket;
  decoded_frame: pAVFrame;
  got_frame: Integer;
  data_size: Integer;
begin
  c := nil;
  decoded_frame := nil;
  av_init_packet(@avpkt);
  WriteLn('Decode audio file %s to ', filename, outfilename);
  (* find the mpeg audio decoder *)
  codec := avcodec_find_decoder(AV_CODEC_ID_MP2);
  if not Assigned(codec) then
  begin
    WriteLn('Codec not found');
    Exit;
  end;
  c := avcodec_alloc_context3(codec);
  if not Assigned(c) then
  begin
    WriteLn('Could not allocate audio codec context');
    Exit;
  end;
  (* open it *)
  if (avcodec_open2(c, codec, nil) < 0) then
  begin
    WriteLn('Could not open codec');
    Exit;
  end;
  AssignFile(f, filename);
  try
    Reset(f, 1);
  except
    WriteLn('Could not open ', filename);
    Exit;
  end;
  AssignFile(outfile, outfilename);
  try
    Rewrite(outfile, 1);
  except
    av_free(c);
    Exit;
  end;
  (* decode until eof *)
  avpkt.data := @inbuf;
  BlockRead(f, inbuf, AUDIO_INBUF_SIZE, avpkt.size);
  while (avpkt.size > 0) do
  begin
    got_frame := 0;
    if not Assigned(decoded_frame) then
    begin
      decoded_frame := av_frame_alloc();
      if not Assigned(decoded_frame) then
      begin
        WriteLn('Could not allocate audio frame');
        Exit;
      end;
    end;
    len := avcodec_decode_audio4(c, decoded_frame, got_frame, @avpkt);
    if (len < 0) then
    begin
      WriteLn('Error while decoding');
      Exit;
    end;
    if (got_frame <> 0) then
    begin
      (* if a frame has been decoded, output it *)
      data_size := av_samples_get_buffer_size(nil, c^.channels, decoded_frame^.nb_samples, c^.sample_fmt, 1);
      if (data_size < 0) then
      begin
        (* This should not occur, checking just for paranoia *)
        WriteLn('Failed to calculate data size');
        Exit;
      end;
      BlockWrite(outfile, decoded_frame^.data[0]^, data_size);
    end;
    avpkt.size := avpkt.size - len;
    avpkt.data := avpkt.data + len;
    avpkt.dts := AV_NOPTS_VALUE;
    avpkt.pts := AV_NOPTS_VALUE;
    if (avpkt.size < AUDIO_REFILL_THRESH) then
    begin
      (* Refill the input buffer, to avoid trying to decode
        * incomplete frames. Instead of this, one could also use
        * a parser, or use a proper container format through
        * libavformat. *)
      Move(avpkt.data^, inbuf, avpkt.size);
      avpkt.data := @inbuf;
      BlockRead(f, avpkt.data[avpkt.size], AUDIO_INBUF_SIZE - avpkt.size, len);
      if (len > 0) then
        avpkt.size := avpkt.size + len;
    end;
  end;
  Close(outfile);
  Close(f);
  avcodec_close(c);
  av_free(c);
  av_frame_free(decoded_frame);
end;

(*
  * Video encoding example
*)
procedure video_encode_example(const filename: String; codec_id: TAVCodecID);
Var
  codec: pAVCodec;
  c: pAVCodecContext;
  i, ret, x, y, got_output: Integer;
  f: File;
  frame: pAVFrame;
  pkt: TAVPacket;
  endcode: array [0 .. 3] of byte;
begin
  c := nil;
  endcode[0] := 0;
  endcode[1] := 0;
  endcode[2] := 1;
  endcode[3] := $B7;
  WriteLn('Encode video file ', filename);
  (* find the mpeg1 video encoder *)
  codec := avcodec_find_encoder(codec_id);
  if not Assigned(codec) then
  begin
    WriteLn('Codec not found');
    Exit;
  end;
  c := avcodec_alloc_context3(codec);
  if not Assigned(c) then
  begin
    WriteLn('Could not allocate video codec context');
    Exit;
  end;
  (* put sample parameters *)
  c^.bit_rate := 400000;
  (* resolution must be a multiple of two *)
  c^.width := 352;
  c^.height := 288;
  (* frames per second *)
  c^.time_base.num := 1;
  c^.time_base.den := 25;
  (* emit one intra frame every ten frames
    * check frame pict_type before passing frame
    * to encoder, if frame^.pict_type is AV_PICTURE_TYPE_I
    * then gop_size is ignored and the output of encoder
    * will always be I frame irrespective to gop_size
  *)
  c^.gop_size := 10;
  c^.max_b_frames := 1;
  c^.pix_fmt := AV_PIX_FMT_YUV420P;
  if (codec_id = AV_CODEC_ID_H264) then
    av_opt_set(c^.priv_data, 'preset', 'slow', 0);
  (* open it *)
  if (avcodec_open2(c, codec, nil) < 0) then
  begin
    WriteLn('Could not open codec');
    Exit;
  end;
  AssignFile(f, filename);
  try
    Rewrite(f, 1);
  except
    WriteLn('Could not open ', filename);
    Exit;
  end;
  frame := av_frame_alloc();
  if not Assigned(frame) then
  begin
    WriteLn('Could not allocate video frame');
    Exit;
  end;
  frame^.format := Integer(c^.pix_fmt);
  frame^.width := c^.width;
  frame^.height := c^.height;
  (* the image can be allocated by any means and av_image_alloc() is
    * just the most convenient way if av_malloc() is to be used *)
  ret := av_image_alloc(frame^.data, frame^.linesize, c^.width, c^.height, c^.pix_fmt, 32);
  if (ret < 0) then
  begin
    WriteLn('Could not allocate raw picture buffer');
    Exit;
  end;
  // got_output:=0;
  (* encode 1 second of video *)
  for i := 0 to 24 do
  begin
    av_init_packet(@pkt);
    pkt.data := nil; // packet data will be allocated by the encoder
    pkt.size := 0;
    (* prepare a dummy image *)

    (* Y *)
    for y := 0 to c^.height - 1 do
      for x := 0 to c^.width - 1 do
        frame^.data[0][y * frame^.linesize[0] + x] := x + y + i * 3;

    (* Cb and Cr *)
    for y := 0 to (c^.height div 2) - 1 do
      for x := 0 to (c^.width div 2) - 1 do
      begin
        frame^.data[1][y * frame^.linesize[1] + x] := 128 + y + i * 2;
        frame^.data[2][y * frame^.linesize[2] + x] := 64 + x + i * 5;
      end;

    frame^.pts := i;
    (* encode the image *)
    ret := avcodec_encode_video2(c, @pkt, frame, got_output);
    if (ret < 0) then
    begin
      WriteLn('Error encoding frame');
      Exit;
    end;
    if (got_output <> 0) then
    begin
      WriteLn(format('Write frame %3d (size=%5d)', [i, pkt.size]));
      BlockWrite(f, pkt.data^, pkt.size);
      av_free_packet(pkt);
    end;
  end;
  (* get the delayed frames *)
  got_output := 1;
  While got_output <> 0 do
  begin
    ret := avcodec_encode_video2(c, @pkt, nil, got_output);
    if (ret < 0) then
    begin
      WriteLn('Error encoding frame');
      Exit;
    end;
    if (got_output <> 0) then
    begin
      WriteLn(format('Write frame %3d (size=%5d)', [i, pkt.size]));
      BlockWrite(f, pkt.data^, pkt.size);
      av_free_packet(pkt);
    end;
    inc(i);
  end;
  (* add sequence end code to have a real mpeg file *)
  BlockWrite(f, endcode, sizeof(endcode));
  Close(f);
  // avcodec_close(c);
  av_free(c);
  // av_freep(frame^.data[0]);
  av_frame_free(frame);
end;

(*
  * Video decoding example
*)
procedure pgm_save(buf: pByte; wrap, xsize, ysize: Integer; filename: String);
Var
  f: TextFile;
  fb: File;
  i: Integer;
begin
  AssignFile(f, filename);
  Rewrite(f);
  WriteLn(f, format('P5' + #13#10 + '%d %d' + #13#10 + '%d', [xsize, ysize, 255]));
  Close(f);
  AssignFile(fb, filename);
  Reset(fb, 1);
  Seek(fb, FileSize(fb));
  for i := 0 to ysize - 1 do
    BlockWrite(fb, buf[i * wrap], xsize);
  Close(fb);
end;

function decode_write_frame(const outfilename: String; avctx: pAVCodecContext; frame: pAVFrame;
  Var frame_count: Integer; pkt: pAVPacket; last: Integer): Integer;
Var
  len, got_frame: Integer;
  buf: array [0 .. 1023] of AnsiChar;

begin
  len := avcodec_decode_video2(avctx, frame, got_frame, pkt);
  if (len < 0) then
  begin
    WriteLn('Error while decoding frame ', frame_count);
    Exit(len);
  end;
  if (got_frame <> 0) then
  begin
    if last <> 0 then
      WriteLn(format('Saving last frame %3d', [frame_count]))
    else
      WriteLn(format('Saving frame %3d', [frame_count]));
    (* the picture is allocated by the decoder, no need to free it *)
    pgm_save(frame^.data[0], frame^.linesize[0], avctx^.width, avctx^.height, format(outfilename, [frame_count]));
    inc(frame_count);
  end;
  if Assigned(pkt^.data) then
  begin
    pkt^.size := pkt^.size - len;
    pkt^.data := pkt^.data + len;
  end;
  Result := 0;
end;

procedure video_decode_example(const outfilename: String; const filename: String);
Var
  codec: pAVCodec;
  c: pAVCodecContext;
  frame_count: Integer;
  f: File;
  frame: pAVFrame;
  inbuf: array [0 .. INBUF_SIZE + FF_INPUT_BUFFER_PADDING_SIZE - 1] of byte;
  avpkt: TAVPacket;
begin
  c := nil;
  av_init_packet(@avpkt);
  (* set end of buffer to 0 (this ensures that no overreading happens for damaged mpeg streams) *)
  FillChar(inbuf[INBUF_SIZE], FF_INPUT_BUFFER_PADDING_SIZE, 0);
  WriteLn(format('Decode video file %s to %s', [filename, outfilename]));
  (* find the mpeg1 video decoder *)
  codec := avcodec_find_decoder(AV_CODEC_ID_MPEG1VIDEO);
  if not Assigned(codec) then
  begin
    WriteLn('Codec not found');
    Exit;
  end;
  c := avcodec_alloc_context3(codec);
  if not Assigned(c) then
  begin
    WriteLn('Could not allocate video codec context');
    Exit;
  end;
  if (codec^.capabilities and CODEC_CAP_TRUNCATED) <> 0 then
    c^.flags := c^.flags or CODEC_FLAG_TRUNCATED; (* we do not send complete frames *)
  (* For some codecs, such as msmpeg4 and mpeg4, width and height
    MUST be initialized there because this information is not
    available in the bitstream. *)
  (* open it *)
  if (avcodec_open2(c, codec, nil) < 0) then
  begin
    WriteLn('Could not open codec');
    Exit;
  end;
  AssignFile(f, filename);
  try
    Reset(f, 1);
  except
    WriteLn('Could not open ', filename);
    Exit;
  end;
  frame := av_frame_alloc();
  if not Assigned(frame) then
  begin
    WriteLn('Could not allocate video frame');
    Exit;
  end;
  frame_count := 0;
  While True do
  begin
    BlockRead(f, inbuf, INBUF_SIZE, avpkt.size);
    if (avpkt.size = 0) then
      break;
    (* NOTE1: some codecs are stream based (mpegvideo, mpegaudio)
      and this is the only method to use them because you cannot
      know the compressed data size before analysing it.
      BUT some other codecs (msmpeg4, mpeg4) are inherently frame
      based, so you must call them with all the data for one
      frame exactly. You must also initialize 'width' and
      'height' before initializing them. *)
    (* NOTE2: some codecs allow the raw parameters (frame size,
      sample rate) to be changed at any frame. We handle this, so
      you should also take care of it *)
    (* here, we use a stream based decoder (mpeg1video), so we
      feed decoder and see if it could decode a frame *)
    avpkt.data := @inbuf;
    while (avpkt.size > 0) do
      if (decode_write_frame(outfilename, c, frame, frame_count, @avpkt, 0) < 0) then
        Exit;
  end;
  (* some codecs, such as MPEG, transmit the I and P frame with a
    latency of one frame. You must do the following to have a
    chance to get the last frame of the video *)
  avpkt.data := nil;
  avpkt.size := 0;
  decode_write_frame(outfilename, c, frame, frame_count, @avpkt, 1);
  Close(f);
  avcodec_close(c);
  av_free(c);
  av_frame_free(frame);
end;

procedure avlog(ptr: Pointer; level: Integer; fmt: PAnsiChar; vl: pva_list); cdecl;
Var
  line: array [0 .. 1023] of AnsiChar;
  print_prefix: Integer;
  A:AnsiString;
begin
  print_prefix := 1;
  av_log_format_line(ptr, level, fmt, vl, @line, sizeof(line), print_prefix);
  A:=Trim(AnsiString(line));
  Writeln(A);
end;

Var
  output_type: String;

begin
  try
    av_log_set_callback(avlog);
    (* register all the codecs *)
    avcodec_register_all();
    if ParamCount = 0 then
    begin
      WriteLn('usage: ' + ExtractFileName(ParamStr(0)) + ' output_type' + #13#10 +
        'API example program to decode/encode a media stream with libavcodec.' + #13#10 +
        'This program generates a synthetic stream and encodes it to a file' + #13#10 +
        'named test.h264, test.mp2 or test.mpg depending on output_type.' + #13#10 +
        'The encoded stream is then decoded and written to a raw data output.' + #13#10 +
        'output_type must be chosen between "h264", "mp2", "mpg"');
      Halt;
    end;
    output_type := ParamStr(1);
    if (SameText(output_type, 'h264')) then
      video_encode_example('test.h264', AV_CODEC_ID_H264)
    else if (SameText(output_type, 'mp2')) then
    begin
      audio_encode_example('test.mp2');
      audio_decode_example('test.sw', 'test.mp2');
    end
    else if (SameText(output_type, 'mpg')) then
    begin
      video_encode_example('test.mpg', AV_CODEC_ID_MPEG1VIDEO);
      video_decode_example('test%02d.pgm', 'test.mpg');
    end
    else
    begin
      WriteLn(format('Invalid output type "%s", choose between "h264", "mp2", or "mpg"', [output_type]));
      Halt;
    end;
  except
    on E: Exception do
      WriteLn(E.ClassName, ': ', E.Message);
  end;

end.
