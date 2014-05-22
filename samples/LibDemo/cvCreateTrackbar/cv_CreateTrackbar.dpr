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

program cv_CreateTrackbar;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  System.SysUtils,
  ocv.highgui_c,
  ocv.core_c,
  ocv.core.types_c,
  uResourcePaths;

const
  filename = cResourceMedia + '768x576.avi';

var
  capture: pCvCapture = nil;
  frame: pIplImage = nil;
  framesCount: Double;
  frames: Integer;
  currentPosition: Integer;
  c: Integer;

  // функция-обработчик ползунка -
  // перематывает на нужный кадр
procedure myTrackbarCallback(pos: Integer); cdecl;
begin
  cvSetCaptureProperty(capture, CV_CAP_PROP_POS_FRAMES, pos);
end;

begin
  try
    // окно для отображения картинки
    cvNamedWindow('original', CV_WINDOW_AUTOSIZE);
    // получаем информацию о видео-файле
    capture := cvCreateFileCapture(filename);
    // получаем чиcло кадров
    framesCount := cvGetCaptureProperty(capture, CV_CAP_PROP_FRAME_COUNT);
    Writeln('[i] count: ', framesCount);
    frames := Trunc(framesCount);

    currentPosition := 0;
    if (frames <> 0) then
      // показываем ползунок
      cvCreateTrackbar('Position', 'original', @currentPosition, frames, myTrackbarCallback);

    while True do
    begin
      // получаем cледующий кадр
      frame := cvQueryFrame(capture);
      if not Assigned(frame) then
        Break;
      // здеcь можно вcтавить
      // процедуру обработки

      // показываем кадр
      cvShowImage('original', frame);

      c := cvWaitKey(33);
      if (c = 27) then
        Break; // еcли нажата ESC - выходим
    end;
    // оcвобождаем реcурcы
    cvReleaseCapture(capture);
    // удаляем окно
    cvDestroyWindow('original');
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;

end.
