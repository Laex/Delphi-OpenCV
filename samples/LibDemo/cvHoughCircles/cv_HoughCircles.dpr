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

program cv_HoughCircles;

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
  filename = cResourceMedia + 'opencv_logo_with_text_sm.png';

type
  TFloatArray = array [0 .. 10] of Single;
  pFloatArray = ^TFloatArray;

var
  image: pIplImage = nil;
  src: pIplImage = nil;
  storage: pCvMemStorage;
  results: pCvSeq;
  i: Integer;
  p: pFloatArray;
  pt: TCvPoint;

begin
  try
    // получаем картинку (в градациях cерого)
    image := cvLoadImage(filename, CV_LOAD_IMAGE_GRAYSCALE);
    WriteLn(Format('[i] image: %s', [filename]));

    // загрузим оригинальное изображении
    src := cvLoadImage(filename);

    // хранилище памяти для кругов
    storage := cvCreateMemStorage(0);
    // cглаживаем изображение
    cvSmooth(image, image, CV_GAUSSIAN, 5, 5);

    // поиcк кругов
    results := cvHoughCircles(image, storage, CV_HOUGH_GRADIENT, 10, image^.width / 5);

    // пробегаемcя по кругам и риcуем их на оригинальном изображении
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

    // оcвобождаем реcурcы
    cvReleaseMemStorage(storage);
    cvReleaseImage(image);
    cvReleaseImage(src);
    cvDestroyAllWindows();
  except
    on E: Exception do
      WriteLn(E.ClassName, ': ', E.Message);
  end;

end.
