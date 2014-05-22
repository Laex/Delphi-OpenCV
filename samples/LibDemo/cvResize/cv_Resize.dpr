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

program cv_Resize;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  System.SysUtils,
  ocv.highgui_c,
  ocv.core_c,
  ocv.core.types_c,
  ocv.imgproc_c,
  uResourcePaths;

const
  // им€ картинки
  filename = cResourceMedia + 'cat2.jpg';

var
  // иcходна€
  image: PIplImage = nil;
  dst: array [0 .. 3] of PIplImage;
  i: Integer;

begin
  try
    image := cvLoadImage(filename, 1);
    i := 0;
    Writeln('[i] image: ', filename);
    if not Assigned(image) then
      Halt;

    // cоздание уменьшенных картинок (разный тип интерпол€ции)
    for i := 0 to 3 do
    begin
      dst[i] := cvCreateImage(cvSize(image^.width div 3, image^.height div 3), image^.depth, image^.nChannels);
      cvResize(image, dst[i], i);
    end;

    // окно дл€ отображени€ картинки
    cvNamedWindow('original', CV_WINDOW_AUTOSIZE);
    cvShowImage('original', image);

    // показываем результат
    for i := 0 to 3 do
    begin
      cvNamedWindow(PCVChar(IntToStr(i)), CV_WINDOW_AUTOSIZE);
      cvShowImage(PCVChar(IntToStr(i)), dst[i]);
    end;

    // ждЄм нажати€ клавиши
    cvWaitKey(0);
    // оcвобождаем реcурcы
    cvReleaseImage(image);
    // удал€ем окна
    cvDestroyAllWindows();
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;

end.
