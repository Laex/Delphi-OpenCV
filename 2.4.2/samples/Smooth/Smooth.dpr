program Smooth;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  System.SysUtils,
  Core.types_c in '..\..\include\сore\Core.types_c.pas',
  core_c in '..\..\include\сore\core_c.pas',
  highgui_c in '..\..\include\highgui\highgui_c.pas',
  imgproc.types_c in '..\..\include\imgproc\imgproc.types_c.pas',
  imgproc_c in '..\..\include\imgproc\imgproc_c.pas';

Const
  // имя картинки
  filename = 'opencv_logo_with_text.png';

Var
  image: PIplImage = nil;
  dst: PIplImage = nil;

begin
  try
    // получаем картинку
    image := cvLoadImage(filename, 1);
    // клонируем картинку
    dst := cvCloneImage(image);
    Writeln('[i] image: ', filename);
    if not Assigned(image) then
      Halt;
    // окно для отображения картинки
    cvNamedWindow('original', CV_WINDOW_AUTOSIZE);
    cvNamedWindow('Smooth', CV_WINDOW_AUTOSIZE);
    // сглаживаем исходную картинку
//    cvSmooth(image, dst, CV_GAUSSIAN, 3, 3);
    cvSmooth(image, dst, CV_BLUR_NO_SCALE, 3, 3);
    // показываем картинку
    cvShowImage('original', image);
    cvShowImage('Smooth', dst);
    // ждём нажатия клавиши
    cvWaitKey(0);
    // освобождаем ресурсы
    cvReleaseImage(&image);
    cvReleaseImage(&dst);
    // удаляем окно
    cvDestroyWindow('original');
    cvDestroyWindow('Smooth');
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;

end.
