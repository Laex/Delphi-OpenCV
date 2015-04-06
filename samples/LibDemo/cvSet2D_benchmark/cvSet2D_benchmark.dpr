program cvSet2D_benchmark;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  System.SysUtils,
  ocv.core_c,
  ocv.core.types_c,
  ocv.imgproc.types_c,
  ocv.imgproc_c;

Var
  img: pIplImage;
  _begin, _end: int64;
  time1, time2: int64;
  i, j: Integer;
  inn: TcvScalar;

begin
  try
    img := cvCreateImage(cvSize(5000, 5000), IPL_DEPTH_8U, 1);
    cvZero(img);
    WriteLn('Fill bytes...');
    _begin := cvGetTickCount;
    for i := 0 to 5000 - 1 do
      for j := 0 to 5000 - 1 do
        pByte(Integer(img^.imageData) + i * img.widthStep + j * img^.nChannels)^ := 100;
    _end := cvGetTickCount;
    time1 := _end - _begin;

    inn.val[0] := 100;
    WriteLn('Set2D...');
    _begin := cvGetTickCount;
    for i := 0 to 5000 - 1 do
      for j := 0 to 5000 - 1 do
        cvSet2D(img, i, j, inn);
    _end := cvGetTickCount;
    time2 := _end - _begin;

    WriteLn('Time 1: ', time1, ' ticks');
    WriteLn('Time 2: ', time2, ' ticks');

    cvReleaseImage(img);
    WriteLn('Press any key');
    Readln;
  except
    on E: Exception do
      WriteLn(E.ClassName, ': ', E.Message);
  end;

end.
