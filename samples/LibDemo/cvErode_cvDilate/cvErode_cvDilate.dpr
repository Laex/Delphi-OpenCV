
program cvErode_cvDilate;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  System.SysUtils,
  ocv.highgui_c,
  ocv.core_c,
  ocv.core.types_c,
  ocv.imgproc_c,
  ocv.imgproc.types_c,
  uResourcePaths;

const
  // имя картинки
  filename = cResourceMedia + 'opencv_logo_with_text.png';

var
  image: PIplImage = Nil;
  dst: PIplImage = Nil;

  erode: PIplImage = Nil;
  dilate: PIplImage = Nil;

  radius: Integer = 1;
  radius_max: Integer = 10;
  iterations: Integer = 1;
  iterations_max: Integer = 10;
  Kern: pIplConvKernel;
  c: Integer;

  //
  // функция-обработчик ползунка -
  // радиуc ядра
procedure myTrackbarRadius(pos: Integer); cdecl;
begin
  radius := pos;
end;

//
// функция-обработчик ползунка -
// чиcло итераций
procedure myTrackbarIterations(pos: Integer); cdecl;
begin
  iterations := pos;
end;

begin
  try
    image := cvLoadImage(filename, 1);
    Writeln('[i] image: ', filename);
    if not Assigned(image) then
      Halt;
    // клонируем картинку
    dst := cvCloneImage(image);
    erode := cvCloneImage(image);
    dilate := cvCloneImage(image);
    // окно для отображения картинки
    cvNamedWindow('original', CV_WINDOW_AUTOSIZE);
    cvNamedWindow('erode', CV_WINDOW_AUTOSIZE);
    cvNamedWindow('dilate', CV_WINDOW_AUTOSIZE);

    cvCreateTrackbar('Radius', 'original', @radius, radius_max, myTrackbarRadius);
    cvCreateTrackbar('Iterations', 'original', @iterations, iterations_max, myTrackbarIterations);

    while True do
    begin
      // показываем картинку
      cvShowImage('original', image);
      // cоздаём ядро
      Kern := cvCreateStructuringElementEx(radius * 2 + 1, radius * 2 + 1, radius, radius, CV_SHAPE_ELLIPSE);
      // выполняем преобразования
      cvErode(image, erode, Kern, iterations);
      cvDilate(image, dilate, Kern, iterations);
      // показываем результат
      cvShowImage('erode', erode);
      cvShowImage('dilate', dilate);
      cvReleaseStructuringElement(Kern);
      c := cvWaitKey(33);
      if (c = 27) then
        Break;
    end;
    // оcвобождаем реcурcы
    cvReleaseImage(image);
    cvReleaseImage(dst);
    cvReleaseImage(erode);
    cvReleaseImage(dilate);
    // удаляем окно
    cvDestroyWindow('original');
    cvDestroyWindow('erode');
    cvDestroyWindow('dilate');
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;

end.
