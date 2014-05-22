// *****************************************************************
// Delphi-OpenCV Demo
// Copyright (C) 2013 Project Delphi-OpenCV
// ****************************************************************
// Contributor:
  // Laentir Valetov
// email:laex@bk.ru
// ****************************************************************
// You may retrieve the latest version of this file at the GitHub,
// located at git://github.com/Laex/Delphi-OpenCV.git
// ****************************************************************
// The contents of this file are used with permission, subject to
// the Mozilla Public License Version 1.1 (the "License"); you may
// not use this file except in compliance with the License. You may
// obtain a copy of the License at
// http://www.mozilla.org/MPL/MPL-1_1Final.html
//
// Software distributed under the License is distributed on an
// "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
// implied. See the License for the specific language governing
// rights and limitations under the License.
// *******************************************************************

program cv_Integral;

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
  filename = cResourceMedia + 'cat2.jpg';

var
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

    // оcвобождаем реcурcы
    cvReleaseImage(SourceImage);
    cvReleaseImage(SumImage);
    cvReleaseImage(SquareSumImage);
    cvReleaseImage(SquareSumImage32S);
    cvReleaseImage(TiltedSumImage);
    // удаляем окна
    cvDestroyAllWindows();
  except
    on E: Exception do
      WriteLn(E.ClassName, ': ', E.Message);
  end;

end.
