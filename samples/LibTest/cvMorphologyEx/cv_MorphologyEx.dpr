// JCL_DEBUG_EXPERT_GENERATEJDBG OFF
// JCL_DEBUG_EXPERT_INSERTJDBG OFF
// JCL_DEBUG_EXPERT_DELETEMAPFILE OFF
program cv_MorphologyEx;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  System.SysUtils,
{$I ..\..\uses_include.inc}
  ;

Const
  // имя картинки
  filename = 'Resource\opencv_logo_with_text.png';

Var

  image: PIplImage = nil;
  open: PIplImage = nil;
  close: PIplImage = nil;
  gradient: PIplImage = nil;
  tophat: PIplImage = nil;
  blackhat: PIplImage = nil;

  radius: Integer = 1;
  radius_max: Integer = 10;
  iterations: Integer = 1;
  iterations_max: Integer = 10;

  Kern: PIplConvKernel;
  Temp: PIplImage;
  c:Integer;

  //
  // функция-обработчик ползунка -
  // радиус ядра
procedure myTrackbarRadius(pos: Integer); cdecl;
begin
  radius := pos;
end;

//
// функция-обработчик ползунка -
// число итераций
procedure myTrackbarIterations(pos: Integer); cdecl;
begin
  iterations := pos;
end;

begin
  try
    // получаем картинку
    image := cvLoadImage(filename, 1);
    Writeln('[i] image: ', filename);
    if not Assigned(image) then
      Halt;
    // клонируем картинку
    open := cvCloneImage(image);
    close := cvCloneImage(image);
    gradient := cvCloneImage(image);
    tophat := cvCloneImage(image);
    blackhat := cvCloneImage(image);

    // окно для отображения картинки
    cvNamedWindow('original', CV_WINDOW_AUTOSIZE);
    cvNamedWindow('CV_MOP_OPEN', CV_WINDOW_AUTOSIZE);
    cvNamedWindow('CV_MOP_CLOSE', CV_WINDOW_AUTOSIZE);
    cvNamedWindow('CV_MOP_GRADIENT', CV_WINDOW_AUTOSIZE);
    cvNamedWindow('CV_MOP_TOPHAT', CV_WINDOW_AUTOSIZE);
    cvNamedWindow('CV_MOP_BLACKHAT', CV_WINDOW_AUTOSIZE);

    cvCreateTrackbar('radius', 'original', @radius, radius_max, myTrackbarRadius);
    cvCreateTrackbar('iterations', 'original', @iterations, iterations_max, myTrackbarIterations);

    while True do
    begin
      // показываем картинку
      cvShowImage('original', image);
      // создаём ядро
      Kern := cvCreateStructuringElementEx(radius * 2 + 1, radius * 2 + 1, radius, radius,
        CV_SHAPE_ELLIPSE);

      // картинка для промежуточного хранения результатов cvCreateImage
      Temp := cvCreateImage(cvSize(image^.width, image^.height), IPL_DEPTH_8U, 1);
      // выолняем преобразования
      cvMorphologyEx(image, open, Temp, Kern, CV_MOP_OPEN, iterations);
      cvMorphologyEx(image, close, Temp, Kern, CV_MOP_CLOSE, iterations);
      cvMorphologyEx(image, gradient, Temp, Kern, CV_MOP_GRADIENT, iterations);
      cvMorphologyEx(image, tophat, Temp, Kern, CV_MOP_TOPHAT, iterations);
      cvMorphologyEx(image, blackhat, Temp, Kern, CV_MOP_BLACKHAT, iterations);

      // показываем результат
      cvShowImage('CV_MOP_OPEN', open);
      cvShowImage('CV_MOP_CLOSE', close);
      cvShowImage('CV_MOP_GRADIENT', gradient);
      cvShowImage('CV_MOP_TOPHAT', tophat);
      cvShowImage('CV_MOP_BLACKHAT', blackhat);

      cvReleaseStructuringElement(Kern);
      cvReleaseImage(Temp);

      c := cvWaitKey(33);
      if (c = 27) then
        Break;
    end;

    // освобождаем ресурсы
    cvReleaseImage(&image);
    cvReleaseImage(&open);
    cvReleaseImage(&close);
    cvReleaseImage(&gradient);
    cvReleaseImage(&tophat);
    cvReleaseImage(&blackhat);
    // удаляем окна
    cvDestroyAllWindows();
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;

end.
