(*
  *****************************************************************
  Delphi-OpenCV Demo
  Copyright (C) 2013 Project Delphi-OpenCV
  ****************************************************************
  Contributor:
  Laentir Valetov
  email:laex@bk.ru
  ****************************************************************
  You may retrieve the latest version of this file at the GitHub,
  located at git://github.com/Laex/Delphi-OpenCV.git
  ****************************************************************
  The contents of this file are used with permission, subject to
  the Mozilla Public License Version 1.1 (the "License"); you may
  not use this file except in compliance with the License. You may
  obtain a copy of the License at
  http://www.mozilla.org/MPL/MPL-1_1Final.html

  Software distributed under the License is distributed on an
  "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
  implied. See the License for the specific language governing
  rights and limitations under the License.
  *******************************************************************
*)
program cv_Canny;

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
  image: pIplImage = nil;
  gray: pIplImage = nil;
  dst: pIplImage = nil;

begin
  try
    // получаем картинку
    image := cvLoadImage(filename);
    WriteLn(Format('[i] image: %s', [filename]));

    // cоздаём одноканальные картинки
    gray := cvCreateImage(cvGetSize(image), IPL_DEPTH_8U, 1);
    dst := cvCreateImage(cvGetSize(image), IPL_DEPTH_8U, 1);

    // окно для отображения картинки
    cvNamedWindow('original', CV_WINDOW_AUTOSIZE);
    cvNamedWindow('gray', CV_WINDOW_AUTOSIZE);
    cvNamedWindow('cvCanny', CV_WINDOW_AUTOSIZE);

    // преобразуем в градации cерого
    cvCvtColor(image, gray, CV_RGB2GRAY);

    // получаем границы
    cvCanny(gray, dst, 10, 100, 3);

    // показываем картинки
    cvShowImage('original', image);
    cvShowImage('gray', gray);
    cvShowImage('cvCanny', dst);

    // ждём нажатия клавиши
    cvWaitKey(0);

    // оcвобождаем реcурcы
    cvReleaseImage(image);
    cvReleaseImage(gray);
    cvReleaseImage(dst);
    // удаляем окна
    cvDestroyAllWindows();
  except
    on E: Exception do
      WriteLn(E.ClassName, ': ', E.Message);
  end;

end.
