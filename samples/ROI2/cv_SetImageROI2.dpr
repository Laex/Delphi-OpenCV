// JCL_DEBUG_EXPERT_GENERATEJDBG OFF
// JCL_DEBUG_EXPERT_INSERTJDBG OFF
// JCL_DEBUG_EXPERT_DELETEMAPFILE OFF
program cv_SetImageROI2;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  System.SysUtils,
  Core.types_c in '..\..\include\сore\Core.types_c.pas',
  core_c in '..\..\include\сore\core_c.pas',
  highgui_c in '..\..\include\highgui\highgui_c.pas',
  LibName in '..\..\include\LibName.pas';

Const
  // имя картинки
  filename = 'Resource\opencv_logo_with_text.png';
  filename2 = 'Resource\cat2.jpg';

var
  image: PIplImage = nil;
  src: PIplImage = nil;
  x: Integer;
  y: Integer;
  width: Integer;
  height: Integer;

begin
  try
    image := cvLoadImage(filename, 1);

    Writeln('[i] image: ', filename);
    if not Assigned(image) then
      Halt;
    cvNamedWindow('origianl', CV_WINDOW_AUTOSIZE);
    cvNamedWindow('ROI', CV_WINDOW_AUTOSIZE);
    // задаём ROI
    x := 120;
    y := 50;
    // добавочное изображение
    src := cvLoadImage(filename2, 1);
    if not Assigned(src) then
      Halt;
    // размер ROI
    width := src^.width;
    height := src^.height;
    cvShowImage('origianl', image);
    // устанавливаем ROI
    cvSetImageROI(image, cvRect(x, y, width, height));
    // обнулим изображение
    cvZero(image);
    // копируем изображение
    cvCopy(src, image);
    // сбрасываем ROI
    cvResetImageROI(image);
    // показываем изображение
    cvShowImage('ROI', image);
    // ждём нажатия клавиши
    cvWaitKey(0);
    // освобождаем ресурсы
    cvReleaseImage(image);
    cvReleaseImage(src);
    cvDestroyAllWindows;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;

end.
