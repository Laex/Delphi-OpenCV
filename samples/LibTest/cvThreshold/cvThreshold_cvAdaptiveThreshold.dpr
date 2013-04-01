// JCL_DEBUG_EXPERT_GENERATEJDBG OFF
// JCL_DEBUG_EXPERT_INSERTJDBG OFF
// JCL_DEBUG_EXPERT_DELETEMAPFILE OFF
program cvThreshold_cvAdaptiveThreshold;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  System.SysUtils,
  core_c in '..\..\..\include\сore\core_c.pas',
  Core.types_c in '..\..\..\include\сore\Core.types_c.pas',
  highgui_c in '..\..\..\include\highgui\highgui_c.pas',
  imgproc.types_c in '..\..\..\include\imgproc\imgproc.types_c.pas',
  imgproc_c in '..\..\..\include\imgproc\imgproc_c.pas',
  uLibName in '..\..\..\include\uLibName.pas',
  types_c in '..\..\..\include\сore\types_c.pas';

const
  filename = 'Resource\cat2.jpg';

var
  src: pIplImage = nil;
  dst: pIplImage = nil;
  dst2: pIplImage = nil;

begin
  // получаем картинку
  src := cvLoadImage(filename, CV_LOAD_IMAGE_GRAYSCALE);
  WriteLn(Format('[i] image: %s', [filename]));
  // покажем изображение
  cvNamedWindow('original', CV_WINDOW_AUTOSIZE);
  cvShowImage('original', src);

  dst := cvCreateImage(cvSize(src^.width, src^.height), IPL_DEPTH_8U, 1);
  dst2 := cvCreateImage(cvSize(src^.width, src^.height), IPL_DEPTH_8U, 1);

  cvThreshold(src, dst, 50, 250, CV_THRESH_BINARY);
  cvAdaptiveThreshold(src, dst2, 250, CV_ADAPTIVE_THRESH_GAUSSIAN_C, CV_THRESH_BINARY, 7, 1);

  // показываем результаты
  cvNamedWindow('cvThreshold', CV_WINDOW_AUTOSIZE);
  cvShowImage('cvThreshold', dst);
  cvNamedWindow('cvAdaptiveThreshold', CV_WINDOW_AUTOSIZE);
  cvShowImage('cvAdaptiveThreshold', dst2);

  // ждём нажатия клавиши
  cvWaitKey(0);

  // освобождаем ресурсы
  cvReleaseImage(src);
  cvReleaseImage(dst);
  cvReleaseImage(dst2);
  // удаляем окна
  cvDestroyAllWindows;

end.
