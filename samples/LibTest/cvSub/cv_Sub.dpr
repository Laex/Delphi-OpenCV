// JCL_DEBUG_EXPERT_GENERATEJDBG OFF
// JCL_DEBUG_EXPERT_INSERTJDBG OFF
// JCL_DEBUG_EXPERT_DELETEMAPFILE OFF
program cv_Sub;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  System.SysUtils,
{$I ..\..\uses_include.inc}
  ;

const
  filename = 'Resource\cat2.jpg';

Var
  src: pIplImage = nil;
  dst: pIplImage = nil;
  dst2: pIplImage = nil;

begin
try
  // получаем картинку в градациях серого
  src := cvLoadImage(filename, CV_LOAD_IMAGE_GRAYSCALE);
  WriteLn(Format('[i] image: %s', [filename]));

  // покажем изображение
  cvNamedWindow('original', 1);
  cvShowImage('original', src);

  // получим бинарное изображение
  dst2 := cvCreateImage(cvSize(src^.width, src^.height), IPL_DEPTH_8U, 1);
  cvCanny(src, dst2, 50, 200);

  cvNamedWindow('bin', 1);
  cvShowImage('bin', dst2);

  // cvScale(src, dst);
  cvSub(src, dst2, dst2);
  cvNamedWindow('sub', 1);
  cvShowImage('sub', dst2);

  // ждём нажатия клавиши
  cvWaitKey(0);

  // освобождаем ресурсы
  cvReleaseImage(src);
  cvReleaseImage(dst);
  cvReleaseImage(dst2);
  // удаляем окна
  cvDestroyAllWindows();
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
