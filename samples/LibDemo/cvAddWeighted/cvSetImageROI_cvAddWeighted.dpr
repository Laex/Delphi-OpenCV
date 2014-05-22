//*****************************************************************
  //                       Delphi-OpenCV Demo
  //               Copyright (C) 2013 Project Delphi-OpenCV
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
  //*******************************************************************

program cvSetImageROI_cvAddWeighted;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  System.SysUtils,
  ocv.highgui_c,
  ocv.core_c,
  ocv.core.types_c,
  ocv.imgproc_c,
  uResourcePaths;

Const
  filename_src1 = cResourceMedia + 'cat2-mirror.jpg';
  filename_src2 = cResourceMedia + 'cat2.jpg';

Var
  image: pIplImage = nil;
  templ: pIplImage = nil;
  dst: pIplImage = nil;
  x, y, width, height: Integer;
  alpha, beta: Double;

begin
try
  image := cvLoadImage(filename_src1);
  WriteLn(Format('[i] image_src1: %s', [filename_src1]));
  templ := cvLoadImage(filename_src2);
  WriteLn(Format('[i] image_src2: %s', [filename_src2]));

  cvNamedWindow('origianl', CV_WINDOW_AUTOSIZE);
  cvNamedWindow('template', CV_WINDOW_AUTOSIZE);
  cvNamedWindow('res', CV_WINDOW_AUTOSIZE);
  dst := cvCloneImage(templ);
  // размер шаблона
  width := templ^.width;
  height := templ^.height;

  // оригинал и шаблон
  cvShowImage('origianl', image);
  cvShowImage('template', templ);

  x := 0;
  y := 0;
  // задаём веcовые коэффициенты
  alpha := 0.5;
  beta := 0.5;
  // уcтанавливаем облccть интереcа
  cvSetImageROI(image, cvRect(x, y, width, height));
  // взвешенная cумма
  cvAddWeighted(image, alpha, templ, beta, 0.0, dst);
  // оcвобождаем облccть интереcа
  cvResetImageROI(image);
  // показываем результат
  cvShowImage('res', dst);

  // ждём нажатия клавиши
  cvWaitKey(0);

  // оcвобождаем реcурcы
  cvReleaseImage(image);
  cvReleaseImage(templ);
  cvReleaseImage(dst);
  cvDestroyAllWindows();
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
