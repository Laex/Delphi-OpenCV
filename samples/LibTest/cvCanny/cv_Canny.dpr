// JCL_DEBUG_EXPERT_GENERATEJDBG OFF
// JCL_DEBUG_EXPERT_INSERTJDBG OFF
// JCL_DEBUG_EXPERT_DELETEMAPFILE OFF
program cv_Canny;

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

Var
  image: pIplImage = nil;
  gray: pIplImage = nil;
  dst: pIplImage = nil;

begin
  // получаем картинку
  image := cvLoadImage(filename);
  WriteLn(Format('[i] image: %s', [filename]));

  // создаём одноканальные картинки
  gray := cvCreateImage(cvGetSize(image), IPL_DEPTH_8U, 1);
  dst := cvCreateImage(cvGetSize(image), IPL_DEPTH_8U, 1);

  // окно для отображения картинки
  cvNamedWindow('original', CV_WINDOW_AUTOSIZE);
  cvNamedWindow('gray', CV_WINDOW_AUTOSIZE);
  cvNamedWindow('cvCanny', CV_WINDOW_AUTOSIZE);

  // преобразуем в градации серого
  cvCvtColor(image, gray, CV_RGB2GRAY);

  // получаем границы
  cvCanny(gray, dst, 10, 100, 3);

  // показываем картинки
  cvShowImage('original', image);
  cvShowImage('gray', gray);
  cvShowImage('cvCanny', dst);

  // ждём нажатия клавиши
  cvWaitKey(0);

  // освобождаем ресурсы
  cvReleaseImage(image);
  cvReleaseImage(gray);
  cvReleaseImage(dst);
  // удаляем окна
  cvDestroyAllWindows();

end.
