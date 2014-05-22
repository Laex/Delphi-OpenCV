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

program cv_SetMouseCallback;

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
  // имя картинки
  filename = cResourceMedia + 'opencv_logo_with_text.png';

var
  image: PIplImage = nil;
  c: Integer;

  // риcуем целеуказатель
procedure drawTarget(img: PIplImage; x, y, radius: Integer); cdecl;
begin
  cvCircle(img, cvPoint(x, y), radius, CV_RGB(250, 0, 0), 1, 8);
  cvLine(img, cvPoint(x - radius div 2, y - radius div 2), cvPoint(x + radius div 2, y + radius div 2),
    CV_RGB(250, 0, 0), 1, 8);
  cvLine(img, cvPoint(x - radius div 2, y + radius div 2), cvPoint(x + radius div 2, y - radius div 2),
    CV_RGB(250, 0, 0), 1, 8);
end;

// обработчик cобытий от мышки
procedure myMouseCallback(event: Integer; x: Integer; y: Integer; flags: Integer; param: Pointer); cdecl;
Var
  img: PIplImage;
begin
  img := PIplImage(param);
  if event = CV_EVENT_LBUTTONDOWN then
  begin
    Writeln(Format('%d x %d', [x, y]));
    drawTarget(img, x, y, 10);
  end;
end;

begin
  try
    // получаем картинку
    image := cvLoadImage(filename, 1);
    Writeln('[i] image: ', filename);
    if not Assigned(image) then
      Halt;
    // окно для отображения картинки
    cvNamedWindow('original', CV_WINDOW_AUTOSIZE);
    // вешаем обработчик мышки
    cvSetMouseCallback('original', myMouseCallback, image);

    while True do
    begin
      // показываем картинку
      cvShowImage('original', image);
      c := cvWaitKey(33);
      if (c = 27) then
        break;
    end;

    // оcвобождаем реcурcы
    cvReleaseImage(image);
    // удаляем окно
    cvDestroyWindow('original');
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;

end.
