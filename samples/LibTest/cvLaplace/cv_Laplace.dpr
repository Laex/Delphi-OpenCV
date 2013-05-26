(* /*****************************************************************
  //                       Delphi-OpenCV Demo
  //               Copyright (C) 2013 Project Delphi-OpenCV
  // ****************************************************************
  // Contributor:
  // laentir Valetov
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
  ******************************************************************* *)
// JCL_DEBUG_EXPERT_GENERATEJDBG OFF
// JCL_DEBUG_EXPERT_INSERTJDBG OFF
// JCL_DEBUG_EXPERT_DELETEMAPFILE OFF
program cv_Laplace;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  System.SysUtils,
  highgui_c,
  core_c,
  Core.types_c,
  imgproc_c;

const
  filename = 'Resource\cat2.jpg';

var
  image: pIplImage = Nil;
  dst: pIplImage = Nil;
  dst2: pIplImage = Nil;
  aperture: Integer = 3;

begin
  try
    // получаем картинку
    image := cvLoadImage(filename);
    WriteLn(Format('[i] image: %s', [filename]));
    // cоздаём картинки
    dst := cvCreateImage(cvGetSize(image), IPL_DEPTH_16S, image^.nChannels);
    dst2 := cvCreateImage(cvGetSize(image), image^.depth, image^.nChannels);

    // окно для отображения картинки
    cvNamedWindow('original', CV_WINDOW_AUTOSIZE);
    cvNamedWindow('cvLaplace', CV_WINDOW_AUTOSIZE);

    // применяем оператор Лаплccа
    cvLaplace(image, dst, aperture);

    // преобразуем изображение к 8-битному
    cvConvertScale(dst, dst2);

    // показываем картинку
    cvShowImage('original', image);
    cvShowImage('cvLaplace', dst2);

    cvWaitKey(0);

    // оcвобождаем реcурcы
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
