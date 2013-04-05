// JCL_DEBUG_EXPERT_GENERATEJDBG OFF
// JCL_DEBUG_EXPERT_INSERTJDBG OFF
// JCL_DEBUG_EXPERT_DELETEMAPFILE OFF
program cv_Integral;

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
  SourceImage: pIplImage = nil;
  SumImage: pIplImage = nil;
  SquareSumImage: pIplImage = nil;
  SquareSumImage32S: pIplImage = nil;
  TiltedSumImage: pIplImage = nil;

begin
  try
    // получаем картинку
    SourceImage := cvLoadImage(filename, CV_LOAD_IMAGE_GRAYSCALE);
    WriteLn(Format('[i] image: %s', [filename]));

    // покажем изображение
    cvNamedWindow('original', 1);
    cvShowImage('original', SourceImage);

    SumImage := cvCreateImage(cvSize(SourceImage^.width + 1, SourceImage^.height + 1), IPL_DEPTH_32S, 1);
    SquareSumImage := cvCreateImage(cvSize(SourceImage^.width + 1, SourceImage^.height + 1), IPL_DEPTH_64F, 1);
    SquareSumImage32S := cvCreateImage(cvSize(SourceImage^.width + 1, SourceImage^.height + 1), IPL_DEPTH_32S, 1);
    TiltedSumImage := cvCreateImage(cvSize(SourceImage^.width + 1, SourceImage^.height + 1), IPL_DEPTH_32S, 1);

    // получаем интегральное изображение
    {
      void cvIntegral( const CvArr* SourceImage, CvArr* SumImage, CvArr* SquareSumImage=0, CvArr* TiltedSumImage=0 );
      SourceImage:     The source image, wxh, single-channel, 8-bit, or floating-point (32f or 64f).
      SumImage:        The sum image, w+1xh+1, single-channel, 32-bit integer or double precision floating-point (64f).
      SquareSumImage:  The square sum image, w+1xh+1, single-channel, double precision floating-point (64f).
      TiltedSumImage:  The tilted sum image (sum of rotated by 45° image), w+1xh+1, single-channel, the same data type as sum.

      The function cvIntegral calculates one or more integral images for the source image as following:
      S(X,Y)=sumx<X,y<YI(x,y)
      Sq(X,Y)=sumx<X,y<YI(x,y)2
      T(X,Y)=sumy<Y,abs(x-X)<yI(x,y)

      After that the images are calculated, they can be used to calculate sums of pixels over an arbitrary rectangles, for example:
      sumx1<=x<x2,y1<=y<y2I(x,y)=S(x2,y2)-S(x1,y2)-S(x2,y1)+S(x1,x1)

      It makes possible to do a fast blurring or fast block correlation with variable window size etc.
    }
    cvIntegral(SourceImage, SumImage, SquareSumImage, TiltedSumImage);

    cvNamedWindow('cvIntegral', 1);
    cvShowImage('cvIntegral', SumImage);
    cvNamedWindow('cvIntegral 2', 1);
    cvConvert(SquareSumImage, SquareSumImage32S);
    cvShowImage('cvIntegral 2', SquareSumImage32S);
    cvNamedWindow('cvIntegral tilted');
    cvShowImage('cvIntegral tilted', TiltedSumImage);

    // ждём нажатия клавиши
    cvWaitKey(0);

    // освобождаем ресурсы
    cvReleaseImage(SourceImage);
    cvReleaseImage(SumImage);
    cvReleaseImage(SquareSumImage);
    cvReleaseImage(SquareSumImage32S);
    cvReleaseImage(TiltedSumImage);
    // удаляем окна
    cvDestroyAllWindows();
    except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;

end.
