program metadata;

{$APPTYPE CONSOLE}
{$R *.res}
{$include ffmpeg.inc}

uses
  System.SysUtils,
  ffmpeglib,
  ffmpeg.libavcodec.avcodec,
  ffmpeg.ctypes,
  ffmpeg.avformat,
  ffmpeg.avio,
  ffmpeg.avutil,
  ffmpeg.buffer,
  ffmpeg.dict,
  ffmpeg.frame,
  ffmpeg.log,
  ffmpeg.opt,
  ffmpeg.pixfmt,
  ffmpeg.rational,
  ffmpeg.samplefmt,
  ffmpeg.parseutils,
  ffmpeg.swscale,
  ffmpeg.pixdesc,
  ffmpeg.imgutils,
  ffmpeg.mem,
  ffmpeg.error,
  uResourcePaths;

Var
  fmt_ctx: pAVFormatContext = nil;
  tag: pAVDictionaryEntry = Nil;
  ret: Integer;
  inp : AnsiString;

const
  std_filename = cResourceMedia + 'trailer.avi';
	
begin
  try
     Writeln(Format('usage: %s <input_file>'#13#10 + 'example program to demonstrate the use of the libavformat metadata API.'#13#10,
        [ExtractFileName(ParamStr(0))]));
    if (ParamCount < 1) then
      inp := std_filename
    else
      inp := ParamStr(1);
    av_register_all();
    ret := avformat_open_input(fmt_ctx, PAnsiChar(inp), nil, nil);
    if ret < 0 then
      Halt(ret);

    tag := av_dict_get(fmt_ctx^.metadata, '', tag, AV_DICT_IGNORE_SUFFIX);
    while Assigned(tag) do
    begin
      Writeln(Format('%s = %s'#13#10, [tag^.key, tag^.value]));
      tag := av_dict_get(fmt_ctx^.metadata, '', tag, AV_DICT_IGNORE_SUFFIX);
    end;

    avformat_close_input(fmt_ctx);
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;

end.
