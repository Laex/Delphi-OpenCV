// JCL_DEBUG_EXPERT_GENERATEJDBG OFF
// JCL_DEBUG_EXPERT_INSERTJDBG OFF
// JCL_DEBUG_EXPERT_DELETEMAPFILE OFF
program cv_Sobel;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  System.SysUtils,
  core_c in '..\..\include\сore\core_c.pas',
  Core.types_c in '..\..\include\сore\Core.types_c.pas',
  highgui_c in '..\..\include\highgui\highgui_c.pas',
  imgproc.types_c in '..\..\include\imgproc\imgproc.types_c.pas',
  imgproc_c in '..\..\include\imgproc\imgproc_c.pas',
  LibName in '..\..\include\LibName.pas';

const
  filename = 'Resource\cat2.jpg';

Var
  xorder: Integer = 1;
  xorder_max: Integer = 2;

  yorder: Integer = 1;
  yorder_max: Integer = 2;

  // функция-обработчик ползунка -
  // порядок производной по X
procedure myTrackbarXorder(pos: Integer); cdecl;
begin
  xorder := pos;
end;

//
// функция-обработчик ползунка -
// порядок производной по Y
procedure myTrackbarYorder(pos: Integer); cdecl;
begin
  yorder := pos;
end;

Var
  image: pIplImage = nil;
  dst: pIplImage = nil;
  dst2: pIplImage = nil;
  aperture: Integer = 3;
  c: Integer;

begin
  // получаем картинку
  image := cvLoadImage(filename);
  WriteLn(Format('[i] image: %s', [filename]));
  // создаём картинки
  dst := cvCreateImage(cvSize(image^.width, image^.height), IPL_DEPTH_16S, image^.nChannels);
  dst2 := cvCreateImage(cvSize(image^.width, image^.height), image^.depth, image^.nChannels);

  // окно для отображения картинки
  cvNamedWindow('original', CV_WINDOW_AUTOSIZE);
  cvNamedWindow('sobel', CV_WINDOW_AUTOSIZE);

  cvCreateTrackbar('xorder', 'original', @xorder, xorder_max, myTrackbarXorder);
  cvCreateTrackbar('yorder', 'original', @yorder, yorder_max, myTrackbarYorder);

  while True do
  begin

    // проверяем, чтобы порядок производных по X и Y был отличен от 0
    if (xorder = 0) and (yorder = 0) then
    begin
      WriteLn('[i] Error: bad params for cvSobel() !');
      cvZero(dst2);
    end
    else
    begin
      // применяем оператор Собеля
      cvSobel(image, dst, xorder, yorder, aperture);
      // преобразуем изображение к 8-битному
      cvConvertScale(dst, dst2);
    end;

    // показываем картинку
    cvShowImage('original', image);
    cvShowImage('sobel', dst2);

    c := cvWaitKey(33);
    if (c = 27) then
      // если нажата ESC - выходим
      break;

  end;

  // освобождаем ресурсы
  cvReleaseImage(image);
  cvReleaseImage(dst);
  cvReleaseImage(dst2);
  // удаляем окна
  cvDestroyAllWindows;

end.
