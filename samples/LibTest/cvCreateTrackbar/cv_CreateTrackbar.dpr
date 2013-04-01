// JCL_DEBUG_EXPERT_GENERATEJDBG OFF
// JCL_DEBUG_EXPERT_INSERTJDBG OFF
// JCL_DEBUG_EXPERT_DELETEMAPFILE OFF
program cv_CreateTrackbar;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  System.SysUtils,
  Core.types_c in '..\..\..\include\сore\Core.types_c.pas',
  core_c in '..\..\..\include\сore\core_c.pas',
  highgui_c in '..\..\..\include\highgui\highgui_c.pas',
  uLibName in '..\..\..\include\uLibName.pas',
  types_c in '..\..\..\include\сore\types_c.pas';

Const
  filename = 'Resource\768x576.avi';

Var
  capture: pCvCapture = nil;
  frame: pIplImage = nil;
  framesCount: Double;
  frames: Integer;
  currentPosition: Integer;
  c: Integer;

  // функция-обработчик ползунка -
  // перематывает на нужный кадр
procedure myTrackbarCallback(pos: Integer); cdecl;
begin
  cvSetCaptureProperty(capture, CV_CAP_PROP_POS_FRAMES, pos);
end;

begin
  try
    // окно для отображения картинки
    cvNamedWindow('original', CV_WINDOW_AUTOSIZE);
    // получаем информацию о видео-файле
    capture := cvCreateFileCapture(filename);
    // получаем число кадров
    framesCount := cvGetCaptureProperty(capture, CV_CAP_PROP_FRAME_COUNT);
    Writeln('[i] count: ', framesCount);
    frames := Trunc(framesCount);

    currentPosition := 0;
    if (frames <> 0) then
      // показываем ползунок
      cvCreateTrackbar('Position', 'original', @currentPosition, frames, myTrackbarCallback);

    while True do
    begin
      // получаем следующий кадр
      frame := cvQueryFrame(capture);
      if not Assigned(frame) then
        Break;
      // здесь можно вставить
      // процедуру обработки

      // показываем кадр
      cvShowImage('original', frame);

      c := cvWaitKey(33);
      if (c = 27) then
        Break; // если нажата ESC - выходим
    end;
    // освобождаем ресурсы
    cvReleaseCapture(capture);
    // удаляем окно
    cvDestroyWindow('original');
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;

end.
