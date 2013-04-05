// JCL_DEBUG_EXPERT_GENERATEJDBG OFF
// JCL_DEBUG_EXPERT_INSERTJDBG OFF
// JCL_DEBUG_EXPERT_DELETEMAPFILE OFF
program cv_Laplace;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  System.SysUtils,
{$I ..\..\uses_include.inc}
  ;

const
  filename = 'Resource\cat2.jpg';

Var
  image: pIplImage = Nil;
  dst: pIplImage = Nil;
  dst2: pIplImage = Nil;
  aperture: Integer = 3;

begin
  try
    // получаем картинку
    image := cvLoadImage(filename);
    WriteLn(Format('[i] image: %s', [filename]));
    // создаём картинки
    dst := cvCreateImage(cvGetSize(image), IPL_DEPTH_16S, image^.nChannels);
    dst2 := cvCreateImage(cvGetSize(image), image^.depth, image^.nChannels);

    // окно для отображения картинки
    cvNamedWindow('original', CV_WINDOW_AUTOSIZE);
    cvNamedWindow('cvLaplace', CV_WINDOW_AUTOSIZE);

    // применяем оператор Лапласа
    cvLaplace(image, dst, aperture);

    // преобразуем изображение к 8-битному
    cvConvertScale(dst, dst2);

    // показываем картинку
    cvShowImage('original', image);
    cvShowImage('cvLaplace', dst2);

    cvWaitKey(0);

    // освобождаем ресурсы
    cvReleaseImage(image);
    cvReleaseImage(dst);
    cvReleaseImage(dst2);
    // удаляем окна
    cvDestroyAllWindows();
  except
    on E: Exception do
      WriteLn(E.ClassName, ': ', E.Message);
  end;

end.
