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
program Class_TwoCameras;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  System.SysUtils,
  highgui_c,
  core_c,
  Core.types_c,
  imgproc_c,
  imgproc.types_c,
  highgui,
  Mat;

Const
  VK_ESCAPE = 27;

  DESIRED_CAMERA_WIDTH: Integer = 320;
  DESIRED_CAMERA_HEIGHT: Integer = 240;

  windowName1 = 'Left';
  windowName2 = 'Right';

var
  camera1, camera2: IVideoCapture;
  cameraFrame1: IMat = nil;
  cameraFrame2: IMat = nil;
  c: Integer;

begin
  try
    camera1 := CreateVideoCapture(0);
    camera1.setValue(CV_CAP_PROP_FRAME_WIDTH, DESIRED_CAMERA_WIDTH);
    camera1.setValue(CV_CAP_PROP_FRAME_HEIGHT, DESIRED_CAMERA_HEIGHT);
    camera2 := CreateVideoCapture(1);
    camera2.setValue(CV_CAP_PROP_FRAME_WIDTH, DESIRED_CAMERA_WIDTH);
    camera2.setValue(CV_CAP_PROP_FRAME_HEIGHT, DESIRED_CAMERA_HEIGHT);
    namedWindow(windowName1);
    namedWindow(windowName2);
    while true do
    begin
      camera1.read(cameraFrame1);
      if Assigned(cameraFrame1) then
      begin
        if (cameraFrame1.empty()) then
        begin
          Writeln('ERROR: Couldn''t grab the next camera frame.');
          Halt(1);
        end;
        imshow(windowName1, cameraFrame1);
      end;

      camera2.read(cameraFrame2);
      if Assigned(cameraFrame2) then
      begin
        if (cameraFrame2.empty()) then
        begin
          Writeln('ERROR: Couldn''t grab the next camera frame.');
          Halt(1);
        end;
        imshow(windowName2, cameraFrame2);
      end;

      // IMPORTANT: Wait for atleast 20 milliseconds, so that the image can be displayed on the screen!
      if (waitKey(20) = VK_ESCAPE) then // Escape Key
        break; // Quit the program!

    end;
    camera1.release;
    camera2.release;
    cvDestroyAllWindows;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;

end.
