// JCL_DEBUG_EXPERT_GENERATEJDBG OFF
// JCL_DEBUG_EXPERT_INSERTJDBG OFF
// JCL_DEBUG_EXPERT_DELETEMAPFILE OFF
program cv_Resize;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  System.SysUtils,
{$I ..\..\uses_include.inc}
  ;

Const
  // имя картинки
  filename = 'Resource\cat2.jpg';

Var
  // исходная
  image: PIplImage = nil;
  dst: array [0 .. 3] of PIplImage;
  i: Integer;

begin
  try
    image := cvLoadImage(filename, 1);
    i := 0;
    Writeln('[i] image: ', filename);
    if not Assigned(image) then
      Halt;

    // создание уменьшенных картинок (разный тип интерполяции)
    for i := 0 to 3 do
    begin
      dst[i] := cvCreateImage(cvSize(image^.width div 3, image^.height div 3), image^.depth,
        image^.nChannels);
      cvResize(image, dst[i], i);
    end;

    // окно для отображения картинки
    cvNamedWindow('original', CV_WINDOW_AUTOSIZE);
    cvShowImage('original', image);

    // показываем результат
    for i := 0 to 3 do
    begin
      cvNamedWindow(PCVChar(IntToStr(i)), CV_WINDOW_AUTOSIZE);
      cvShowImage(PCVChar(IntToStr(i)), dst[i]);
    end;

    // ждём нажатия клавиши
    cvWaitKey(0);
    // освобождаем ресурсы
    cvReleaseImage(image);
    // удаляем окна
    cvDestroyAllWindows();
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;

end.
