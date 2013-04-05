// JCL_DEBUG_EXPERT_GENERATEJDBG OFF
// JCL_DEBUG_EXPERT_INSERTJDBG OFF
// JCL_DEBUG_EXPERT_DELETEMAPFILE OFF
program cv_Laplace;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  System.SysUtils,
uLibName in '..\..\..\include\uLibName.pas',
highgui_c in '..\..\..\include\highgui\highgui_c.pas',
core_c in '..\..\..\include\сore\core_c.pas',
Core.types_c in '..\..\..\include\сore\Core.types_c.pas',
imgproc.types_c in '..\..\..\include\imgproc\imgproc.types_c.pas',
imgproc_c in '..\..\..\include\imgproc\imgproc_c.pas',
legacy in '..\..\..\include\legacy\legacy.pas',
calib3d in '..\..\..\include\calib3d\calib3d.pas',
imgproc in '..\..\..\include\imgproc\imgproc.pas',
haar in '..\..\..\include\objdetect\haar.pas',
objdetect in '..\..\..\include\objdetect\objdetect.pas',
tracking in '..\..\..\include\video\tracking.pas',
Core in '..\..\..\include\сore\core.pas'
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
