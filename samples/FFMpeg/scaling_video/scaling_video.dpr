program scaling_video;

{$APPTYPE CONSOLE}
{$R *.res}
{$i ffmpeg.inc}

uses
  System.SysUtils,
  System.Classes,
  Winapi.Windows,
  ffm.lib,
  ffm.libavcodec.avcodec,
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
  uResourcePaths;

Var
  src_data: TPointers;
  dst_data: TPointers;
  src_linesize: TLinesizes;
  dst_linesize: TLinesizes;
  src_w: Integer = 320;
  src_h: Integer = 240;
  dst_w, dst_h: Integer;
  src_pix_fmt: TAVPixelFormat = AV_PIX_FMT_YUV420P;
  dst_pix_fmt: TAVPixelFormat = AV_PIX_FMT_RGB24;
  dst_size: AnsiString;
  dst_filename: AnsiString;
//  dst_file: File;
  dst_straem:TMemoryStream;
  dst_bufsize: Integer;
  sws_ctx: pSwsContext = nil;
  i, ret: Integer;

const
  in_filename = cResourceMedia + 'trailer.avi';
  out_filename = cResourceResult + 'trailer-out.avi';

procedure fill_yuv_image(data: TPointers; linesize: TLinesizes; width: Integer; height: Integer; frame_index: Integer);
Var
  x, y: Integer;
begin
  (* Y *)
  for y := 0 to height - 1 do
    for x := 0 to width - 1 do
      data[0][y * linesize[0] + x] := x + y + frame_index * 3;

  (* Cb and Cr *)
  for y := 0 to height div 2 - 1 do
  begin
    for x := 0 to width div 2 - 1 do
    begin
      data[1][y * linesize[1] + x] := 128 + y + frame_index * 2;
      data[2][y * linesize[2] + x] := 64 + x + frame_index * 5;
    end;
  end;
end;

Var
  Buf: array [0 .. 511] of AnsiChar;
  r:Integer;

begin
  try
    if (ParamCount <> 2) then
    begin
      WriteLn(Format('Usage: %s output_file output_size' + #13#10 + 'API example program to show how to scale an image with libswscale.' +
        #13#10 + 'This program generates a series of pictures, rescales them to the given ' +
        'output_size and saves them to an output file named output_file.' + #13#10, [ExtractFileName(ParamStr(0))]));
    end;
    if (ParamCount < 2) then
    begin
      if FileExists(in_filename) then
        if CopyFileEx(PChar(in_filename), PChar(out_filename), nil, nil, nil, COPY_FILE_RESTARTABLE) then
        begin
          dst_filename := out_filename;
          dst_size := '320x240';
        end
        else
          Halt(1);
    end
    else
    begin
      dst_filename := ParamStr(1);
      dst_size := ParamStr(2);
    end;

    if av_parse_video_size(dst_w, dst_h, PAnsiChar(dst_size)) < 0 then
    begin
      WriteLn(Format('Invalid size %s, must be in the form WxH or a valid size abbreviation', [dst_size]));
      Halt(1);
    end;

    (* create scaling context *)
    sws_ctx := sws_getContext(src_w, src_h, src_pix_fmt, dst_w, dst_h, dst_pix_fmt, SWS_BILINEAR, Nil, Nil, Nil);
    if not Assigned(sws_ctx) then
    begin
      WriteLn(Format('Impossible to create scale context for the conversion fmt:%s s:%dx%d -> fmt:%s s:%dx%d' + #13#10,
        [av_get_pix_fmt_name(src_pix_fmt), src_w, src_h, av_get_pix_fmt_name(dst_pix_fmt), dst_w, dst_h]));
      Halt(1);
      // ret := AVERROR(EINVAL);
      // goto
      // end;
    end;

    (* allocate source and destination image buffers *)
    ret := av_image_alloc(src_data, src_linesize, src_w, src_h, src_pix_fmt, 16);
    // av_strerror(ret,@buf,SizeOf(buf));
    if (ret < 0) then
    begin
      WriteLn('Could not allocate source image');
      Halt(1);
    end;

    (* buffer is going to be written to rawvideo file, no alignment *)
    ret := av_image_alloc(dst_data, dst_linesize, dst_w, dst_h, dst_pix_fmt, 1);
    if (ret < 0) then
    begin
      WriteLn('Could not allocate destination image');
    end;
    dst_bufsize := ret;

    dst_straem:=TMemoryStream.Create;

    for i := 0 to 99 do
    begin
      (* generate synthetic video *)
      fill_yuv_image(src_data, src_linesize, src_w, src_h, i);

      (* convert to destination format *)
      sws_scale(sws_ctx, @src_data, @src_linesize, 0, src_h, @dst_data, @dst_linesize);

      (* write scaled image to file *)
      dst_straem.Write((@dst_data[0][0])^, dst_bufsize);
    end;

    dst_straem.SaveToFile(dst_filename);
    dst_straem.Free;

    WriteLn(Format('Scaling succeeded. Play the output file with the command:' + #13#10 +
      'ffplay -f rawvideo -pixel_format %s -video_size %dx%d %s', [av_get_pix_fmt_name(dst_pix_fmt), dst_w, dst_h, dst_filename]));

    av_freep(@src_data[0]);
    av_freep(@dst_data[0]);
    sws_freeContext(sws_ctx);

  except
    on E: Exception do
      WriteLn(E.ClassName, ': ', E.Message);
  end;

end.
