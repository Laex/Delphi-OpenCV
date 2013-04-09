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
program cv_CreateTrackbar;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  System.SysUtils,
uLibName in '..\..\..\include\uLibName.pas',
highgui_c in '..\..\..\include\highgui\highgui_c.pas',
core_c in '..\..\..\include\сore\core_c.pas',
Core.types_c in '..\..\..\include\сore\Core.types_c.pas',
imgproc.types_c in '..\..\..\include\imgproc\imgproc.types_c.pas',
imgproc_c in '..\..\..\include\imgproc\imgproc_c.pas',
legacy in '..\..\..\include\legacy\legacy.pas',
calib3d in '..\..\..\include\calib3d\calib3d.pas',
imgproc in '..\..\..\include\imgproc\imgproc.pas',
haar in '..\..\..\include\objdetect\haar.pas',
objdetect in '..\..\..\include\objdetect\objdetect.pas',
tracking in '..\..\..\include\video\tracking.pas',
Core in '..\..\..\include\сore\core.pas'
  ;

Const
  filename = 'Resource\768x576.avi';

Var
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
    // получаем число кадров
    framesCount := cvGetCaptureProperty(capture, CV_CAP_PROP_FRAME_COUNT);
    Writeln('[i] count: ', framesCount);
    frames := Trunc(framesCount);

    currentPosition := 0;
    if (frames <> 0) then
      // показываем ползунок
      cvCreateTrackbar('Position', 'original', @currentPosition, frames, myTrackbarCallback);

    while True do
    begin
      // получаем следующий кадр
      frame := cvQueryFrame(capture);
      if not Assigned(frame) then
        Break;
      // здесь можно вставить
      // процедуру обработки

      // показываем кадр
      cvShowImage('original', frame);

      c := cvWaitKey(33);
      if (c = 27) then
        Break; // если нажата ESC - выходим
    end;
    // освобождаем ресурсы
    cvReleaseCapture(capture);
    // удаляем окно
    cvDestroyWindow('original');
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;

end.
