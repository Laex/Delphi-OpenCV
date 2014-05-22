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
program cv_SetImageROI;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  System.SysUtils,
  opencv.highgui_c,
  opencv.core_c,
  opencv.core.types_c,
  opencv.imgproc_c,
  opencv.imgproc.types_c,
  uResourcePaths;

const
  // имя картинки
  filename = cResourceMedia + 'cat2.jpg';

var
  image: PIplImage = nil;
  x: Integer;
  y: Integer;
  width: Integer;
  height: Integer;
  add: Integer;

begin
  try

    image := cvLoadImage(filename, 1);

    Writeln('[i] image: ', filename);
    if not Assigned(image) then
      Halt;

    cvNamedWindow('origianl', CV_WINDOW_AUTOSIZE);
    cvNamedWindow('ROI', CV_WINDOW_AUTOSIZE);

    // задаём ROI
    x := 140;
    y := 120;
    width := 200;
    height := 200;
    // добавочная величина
    add := 200;

    cvShowImage('origianl', image);
    // уcтанавливаем ROI
    cvSetImageROI(image, cvRect(x, y, width, height));
    cvAddS(image, cvScalar(add), image);
    // cбрccываем ROI
    cvResetImageROI(image);
    // показываем изображение
    cvShowImage('ROI', image);
    // ждём нажатия клавиши
    cvWaitKey(0);
    // оcвобождаем реcурcы
    cvReleaseImage(image);
    cvDestroyAllWindows;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;

end.
