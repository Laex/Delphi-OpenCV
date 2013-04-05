// JCL_DEBUG_EXPERT_GENERATEJDBG OFF
// JCL_DEBUG_EXPERT_INSERTJDBG OFF
// JCL_DEBUG_EXPERT_DELETEMAPFILE OFF
program cv_CreateVideoWriter;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  System.SysUtils,
{$I ..\..\uses_include.inc}
  ;

Const
  filename = 'Result\capture.avi';

Var
  capture: pCvCapture;
  fps: Double;
  size: TCvSize;
  writer: pCvVideoWriter;
  frame: PIplImage;
  c: Integer;

begin
  try
    cvNamedWindow('capture');
    // получаем любую подключённую камеру
    capture := cvCreateCameraCapture(CV_CAP_ANY);
    if not Assigned(capture) then
      Halt;
    // частота кадров
    fps := cvGetCaptureProperty(capture, CV_CAP_PROP_FPS);
    if fps = 0 then
      fps := 15;
    // размер картинки
    size := cvSize(Trunc(cvGetCaptureProperty(capture, CV_CAP_PROP_FRAME_WIDTH)),
      Trunc(cvGetCaptureProperty(capture, CV_CAP_PROP_FRAME_HEIGHT)));
    if (size.width = 0) or (size.height = 0) then
      size := cvSize(640, 480);
    writer := cvCreateVideoWriter(filename, CV_FOURCC('X', 'V', 'I', 'D'), fps, size);
    if not Assigned(writer) then
      Halt;
    frame := nil;
    while true do
    begin
      // получаем кадр
      frame := cvQueryFrame(capture);
      if not Assigned(frame) then
        Break;
      // сохраняем в файл
      cvWriteFrame(writer, frame);
      // показываем
      cvShowImage('capture', frame);
      c := cvWaitKey(1);
      if (c = 27) then
        Break; // если нажата ESC - выходим
    end;
    // освобождаем ресурсы
    cvReleaseVideoWriter(writer);
    cvReleaseCapture(capture);
    cvDestroyWindow('capture');
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;

end.
