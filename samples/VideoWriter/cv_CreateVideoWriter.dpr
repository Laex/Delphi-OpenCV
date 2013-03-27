// JCL_DEBUG_EXPERT_GENERATEJDBG OFF
// JCL_DEBUG_EXPERT_INSERTJDBG OFF
// JCL_DEBUG_EXPERT_DELETEMAPFILE OFF
program cv_CreateVideoWriter;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  System.SysUtils,
  Core.types_c in '..\..\include\сore\Core.types_c.pas',
  core_c in '..\..\include\сore\core_c.pas',
  highgui_c in '..\..\include\highgui\highgui_c.pas',
  LibName in '..\..\include\LibName.pas';

Const
  filename = 'Resource\capture.avi';

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
    // double fps = cvGetCaptureProperty (capture, CV_CAP_PROP_FPS);
    fps := 15;
    // размер картинки
    // CvSize size = cvSize( (int)cvGetCaptureProperty( capture, CV_CAP_PROP_FRAME_WIDTH), (int)cvGetCaptureProperty( capture, CV_CAP_PROP_FRAME_HEIGHT));
    size := CvSize(640, 480);
    writer := cvCreateVideoWriter(filename, CV_FOURCC('X', 'V', 'I', 'D'), fps, size, 0);
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
      c := cvWaitKey(0);
      if (c = 27) then
        Break; // если нажата ESC - выходим
    end;
    // освобождаем ресурсы
    cvReleaseCapture(capture);
    cvReleaseVideoWriter(writer);
    cvDestroyWindow('capture');
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;

end.
