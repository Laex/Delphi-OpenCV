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

program TwoCameras_C;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  System.SysUtils,
  ocv.highgui_c,
  ocv.core_c,
  ocv.core.types_c,
  ocv.imgproc_c,
  ocv.imgproc.types_c;

var
  frame1, frame2: PIplImage;
  video1, video2: PCvCapture;
  c: integer;

begin
  try
    cvNamedWindow('Camera 1');
    cvNamedWindow('Camera 2');
    video1 := cvCreateCameraCapture(0);
    cvSetCaptureProperty(video1, CV_CAP_PROP_FRAME_WIDTH, 320);
    cvSetCaptureProperty(video1, CV_CAP_PROP_FRAME_HEIGHT, 240);

    video2 := cvCreateCameraCapture(1);
    cvSetCaptureProperty(video2, CV_CAP_PROP_FRAME_WIDTH, 320);
    cvSetCaptureProperty(video2, CV_CAP_PROP_FRAME_HEIGHT, 240);

    while True do
    begin
      frame1 := cvQueryFrame(video1);
      if Assigned(frame1) then
        cvShowImage('Camera 1', frame1);
      frame2 := cvQueryFrame(video2);
      if Assigned(frame2) then
        cvShowImage('Camera 2', frame2);
      if cvWaitKey(99) = 27 then
        Break;
    end;

    cvDestroyWindow('Camera 1');
    cvDestroyWindow('Camera 2');
    cvReleaseCapture(video1);
    cvReleaseCapture(video2);

  except
    on E: Exception do
      WriteLn(E.ClassName, ': ', E.Message);
  end;

end.
