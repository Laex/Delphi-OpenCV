program avcodec_sample2;

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

procedure SaveFrame(AVFrame *pFrame, int width, int height, int iFrame);
begin
    FILE *pFile;
    char szFilename[32];
    int  y;

    // Open file
    sprintf(szFilename, "frame%d.ppm", iFrame);
    pFile=fopen(szFilename, "wb");
    if(pFile==NULL)
	return;

    // Write header
    fprintf(pFile, "P6\n%d %d\n255\n", width, height);

    // Write pixel data
    for(y=0; y<height; y++)
	fwrite(pFrame->data[0]+y*pFrame->linesize[0], 1, width*3, pFile);

    // Close file
    fclose(pFile);
end;


begin
  try


  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;

end.
