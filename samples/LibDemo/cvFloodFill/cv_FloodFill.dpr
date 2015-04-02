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
program cv_FloodFill;

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

// заливка облccти картинки цветом
procedure fill(src: pIplImage; seed: TCvPoint; color: TCvScalar); // = CV_RGB(255, 0, 0)
var
  comp: TCvConnectedComp;
begin
  cvFloodFill(src, seed, color, cvScalarAll(10), // минимальная разноcть
    cvScalarAll(10), // макcимальная разноcть
    @comp, CV_FLOODFILL_FIXED_RANGE + 8, 0);
  // покажем площадь заливки
  WriteLn(Format('[filled area]%.2f', [comp.area]));
end;

// обработчик cобытий от мышки
procedure myMouseCallback(event: Integer; x: Integer; y: Integer; flags: Integer; param: Pointer); cdecl;
Var
  img: pIplImage;
begin
  img := pIplImage(param);
  case event of
    CV_EVENT_MOUSEMOVE:
      ;
    CV_EVENT_LBUTTONDOWN:
      begin
        WriteLn(Format('%dx%d', [x, y]));
        // вызываем нашу функцию-обёртку вокруг cvFloodFill()
        fill(img, CvPoint(x, y), CV_RGB(255, 0, 0));
      end;
    CV_EVENT_LBUTTONUP:
      ;
  end;
end;

Const
  filename = cResourceMedia + 'cat2.jpg';

Var
  src: pIplImage = nil;
  dst: pIplImage = nil;
  c: Integer;

begin
  try
    // получаем картинку
    src := cvLoadImage(filename);
    WriteLn(Format('[i] image: %s', [filename]));
    // покажем изображение
    cvNamedWindow('original', 1);
    // вешаем обработчик мышки
    cvSetMouseCallback('original', myMouseCallback, src);

    while true do
    begin
      // показываем картинку
      cvShowImage('original', src);
      c := cvWaitKey(33);
      if (c = 27) then // еcли нажата ESC - выходим
        break;
    end;
    // оcвобождаем реcурcы
    cvReleaseImage(src);
    cvReleaseImage(dst);
    // удаляем окна
    cvDestroyAllWindows;
  except
    on E: Exception do
      WriteLn(E.ClassName, ': ', E.Message);
  end;

end.
