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
program cv_CreateCameraCapture;

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

Var
  capture: PCvCapture;
  width: Double;
  height: Double;
  frame: PIplImage;
  counter: Integer;
  filename: pCVChar;
  c: Integer;

begin
  try
    // получаем любую подключённую камеру
    capture := cvCreateCameraCapture(CV_CAP_ANY);
    if not Assigned(capture) then
      Halt;
    // узнаем ширину и высоту кадра
    width := cvGetCaptureProperty(capture, CV_CAP_PROP_FRAME_WIDTH);
    height := cvGetCaptureProperty(capture, CV_CAP_PROP_FRAME_HEIGHT);
    WriteLn(Format('[i] %.0f x %.0f', [width, height]));
    frame := Nil;
    cvNamedWindow('capture', CV_WINDOW_AUTOSIZE);
    WriteLn('[i] press Enter for capture image and Esc for quit!');
    counter := 0;
    filename := AllocMem(512);

    while true do
    begin
      // получаем кадр
      frame := cvQueryFrame(capture);
      // показываем
      cvShowImage('capture', frame);
      c := cvWaitKey(33);
      if (c = 27) then
        Break
      else if (c = 13) then
      begin
        // сохраняем кадр в файл
        filename := PCVChar(AnsiString(Format('Image %d.jpg'#0, [counter])));
        WriteLn('[i] capture - ', filename);
        cvSaveImage(filename, frame);
        Inc(counter);
      end;
    end;
    // освобождаем ресурсы
    cvReleaseCapture(capture);
    cvDestroyWindow('capture');
  except
    on E: Exception do
      WriteLn(E.ClassName, ': ', E.Message);
  end;

end.
