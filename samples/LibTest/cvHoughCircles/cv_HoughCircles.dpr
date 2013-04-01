// JCL_DEBUG_EXPERT_GENERATEJDBG OFF
// JCL_DEBUG_EXPERT_INSERTJDBG OFF
// JCL_DEBUG_EXPERT_DELETEMAPFILE OFF
program cv_HoughCircles;

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
  filename = 'Resource\opencv_logo_with_text_sm.png';

Type
  TFloatArray = array [0 .. 10] of Single;
  pFloatArray = ^TFloatArray;

Var
  image: pIplImage = nil;
  src: pIplImage = nil;
  storage: pCvMemStorage;
  results: pCvSeq;
  i: Integer;
  p: pFloatArray;
  pt:TCvPoint;

begin
  // получаем картинку (в градациях серого)
  image := cvLoadImage(filename, CV_LOAD_IMAGE_GRAYSCALE);
  WriteLn(Format('[i] image: %s', [filename]));

  // загрузим оригинальное изображении
  src := cvLoadImage(filename);

  // хранилище памяти для кругов
  storage := cvCreateMemStorage(0);
  // сглаживаем изображение
  cvSmooth(image, image, CV_GAUSSIAN, 5, 5);

  // поиск кругов
  results := cvHoughCircles(image, storage, CV_HOUGH_GRADIENT, 10, image^.width / 5);

  // пробегаемся по кругам и рисуем их на оригинальном изображении
  for i := 0 to results^.total - 1 do
  begin
    p := pFloatArray(cvGetSeqElem(results, i));
    pt := CvPoint(cvRound(p^[0]), cvRound(p^[1]));
    cvCircle(src, pt, cvRound(p^[2]), CV_RGB(255, 0, 0));
  end;

  // показываем
  cvNamedWindow('cvHoughCircles', 1);
  cvShowImage('cvHoughCircles', src);

  // ждём нажатия клавиши
  cvWaitKey(0);

  // освобождаем ресурсы
  cvReleaseMemStorage(storage);
  cvReleaseImage(image);
  cvReleaseImage(src);
  cvDestroyAllWindows();

end.
