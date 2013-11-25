program metadata;

{$APPTYPE CONSOLE}
{$R *.res}
{$include ffmpeg.inc}

uses
  System.SysUtils,
  ffmpeglib in '..\..\ffmpeglib.pas',
  libavcodec.avcodec in '..\..\libavcodec\libavcodec.avcodec.pas',
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
  error in '..\..\libavutil\error.pas';

Var
  fmt_ctx: pAVFormatContext = nil;
  tag: pAVDictionaryEntry = Nil;
  ret: Integer;
  inp : AnsiString;

begin
  try
    if (ParamCount <> 1) then
    begin
      Writeln(Format('usage: %s <input_file>'#13#10 + 'example program to demonstrate the use of the libavformat metadata API.'#13#10,
        [ExtractFileName(ParamStr(0))]));
      Halt(1)
    end;
    av_register_all();
    inp:=ParamStr(1);
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
