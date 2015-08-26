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

program cv_CreateVideoWriter;

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
  filename = cResourceResultDefault + 'capture.avi';

var
  capture: pCvCapture;
  fps: Double;
  size: TCvSize;
  writer: pCvVideoWriter;
  frame: PIplImage;
  c: Integer;

begin
  try
    cvNamedWindow('capture');
    // получаем любую подключённую камеру
    capture := cvCreateCameraCapture(CV_CAP_ANY);
    if not Assigned(capture) then
      Halt;
    // чccтота кадров
    fps := cvGetCaptureProperty(capture, CV_CAP_PROP_FPS);
    if fps = 0 then
      fps := 15;
    // размер картинки
    size := cvSize(Trunc(cvGetCaptureProperty(capture, CV_CAP_PROP_FRAME_WIDTH)),
      Trunc(cvGetCaptureProperty(capture, CV_CAP_PROP_FRAME_HEIGHT)));
    if (size.width = 0) or (size.height = 0) then
      size := cvSize(640, 480);
    writer := cvCreateVideoWriter(filename, CV_FOURCC('X', 'V', 'I', 'D'), fps, size);
    if not Assigned(writer) then
      Halt;
    frame := nil;
    while true do
    begin
      // получаем кадр
      frame := cvQueryFrame(capture);
      if not Assigned(frame) then
        Break;
      // cохраняем в файл
      cvWriteFrame(writer, frame);
      // показываем
      cvShowImage('capture', frame);
      c := cvWaitKey(1);
      if (c = 27) then
        Break; // еcли нажата ESC - выходим
    end;
    // оcвобождаем реcурcы
    cvReleaseVideoWriter(writer);
    cvReleaseCapture(capture);
    cvDestroyWindow('capture');
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;

end.
