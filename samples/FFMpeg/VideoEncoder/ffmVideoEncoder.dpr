program ffmVideoEncoder;

{$APPTYPE CONSOLE}
{$POINTERMATH ON}
{$R *.res}

uses
  System.SysUtils,
  ffm.cls.videoencoder,
  ffm.libavcodec.avcodec,
  ffm.pixfmt,
  ffm.mem,
  ffm.frame;

Var
  seed: Single = 1.0;

  // Create test video frame
procedure CreateFrame(const buffer: PByte; const w, h, bytespan: Integer);
Var
  wxh: Integer;
  i, j: Integer;
  line: PByte;
begin
  wxh := w * h;
  for i := 0 to h - 1 do
  begin
    line := @buffer[i * bytespan];
    for j := 0 to w - 1 do
    begin
      // RGB
      line[0] := Trunc(255 * sin((i / wxh * seed) * 3.14));
      line[1] := Trunc(255 * cos((j / wxh * seed) * 3.14));
      line[2] := Trunc(255 * sin(((i + j) / wxh * seed) * 3.14));
      line := line + 3;
    end;
  end;
  seed := seed + 2.2;
end;

Var
  shift: Single = 0.0;
  seconds: Single = 0.0;

  minNu: Single = 3.14 / (44100.0) * 700.0;
  maxNu: Single = 3.14 / (44100.0) * 1500.0;
  speedNu: Single = 3.14 / (44100.0) * 10.0;

  varNu: Single = 3.14 / (44100.0) * 700.0;

  // Create sample
procedure CreateSample(const buffer: PByte; const sampleCount: Integer);
Var
  shift, seconds, minNu, maxNu, speedNu, varNu: Single;
  i: Integer;
begin
  if (varNu > maxNu) then
    varNu := minNu;

  varNu := varNu + speedNu;

  for i := 0 to sampleCount - 1 do
  begin
    seconds := seconds + 1.0 / 44100.0;
    buffer[i] := Trunc(sin(i * varNu + shift) * $4FFF);
  end;
  shift := shift + varNu * sampleCount;
end;

const
  W_VIDEO = 640;
  H_VIDEO = 480;
  FILE_NAME = 'c:\temp\1.avi';
  FRAME_COUNT = 150;
  CONTAINER = 'auto';

Var
  encoder: TFFMVideoEncoder;
  w, h: Integer;
  frame: pAVFrame;
  nSampleSize: Integer;
  sample: PByte;
  bufferImgSize: Integer;
  buffer: PByte;
  i: Integer;

begin
  try
    encoder := TFFMVideoEncoder.Create;

    if encoder.InitFile(FILE_NAME, CONTAINER, W_VIDEO, H_VIDEO) then
    begin
      w := W_VIDEO;
      h := H_VIDEO;
      frame := av_frame_alloc();//avcodec_alloc_frame();
      nSampleSize := 2 * 44100 div 25; // 1 / 25 sec * FORMAT SIZE(S16)
      sample := AllocMem(nSampleSize);
      // Create frame
      bufferImgSize := avpicture_get_size(AV_PIX_FMT_BGR24, w, h);
      buffer := av_mallocz(bufferImgSize);
      avpicture_fill(pAVPicture(frame), buffer, AV_PIX_FMT_BGR24, w, h);

      for i := 0 to FRAME_COUNT - 1 do
      begin
        CreateFrame(frame^.data[0], w, h, frame^.linesize[0]);
        CreateSample(sample, nSampleSize div 2);
        if not encoder.AddFrame(frame, sample, nSampleSize) then
          Writeln('Cannot write frame');
      end;

      encoder.Finish();
      av_free(frame^.data[0]);
      av_free(frame);
      FreeMem(sample);
      sample := nil;
    end
    else
      Writeln('Cannot open file ' + FILE_NAME);
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;

end.
