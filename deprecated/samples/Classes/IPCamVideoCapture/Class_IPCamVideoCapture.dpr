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
program Class_IPCamVideoCapture;

{$APPTYPE CONSOLE}
{$POINTERMATH ON}
{$R *.res}

uses
  System.SysUtils,
  core.types_c,
  highgui,
  highgui_c,
  Mat;

Const
  VK_ESCAPE = 27;

  DESIRED_CAMERA_WIDTH: Integer = 640;
  DESIRED_CAMERA_HEIGHT: Integer = 480;

  windowName = 'Test IP camera VideoCapture'; // Name shown in the GUI window.

  // Get access to the webcam.
procedure initWebcam(videoCapture: IVideoCapture; filename: pCVChar);
begin
  // Get access to the default camera.
  // Surround the OpenCV call by a try/catch block so we can give a useful error message!
  videoCapture.openfilename(filename);
  if not videoCapture.isOpened() then
  begin
    Writeln('ERROR: Could not access the camera!');
    Halt(1);
  end;
  Writeln('Loaded camera ', filename);
end;

Var
  // See http://www.ispyconnect.com/
  filename: pCVChar ='rtsp://10.1.1.202:554/cam/realmonitor?channel=1&subtype=0&unicast=true&proto=Onvif';
  camera: IVideoCapture;
  cameraFrame: IMat = nil;

begin
  try
    camera := CreateVideoCapture;
    initWebcam(camera, filename);
    // Try to set the camera resolution. Note that this only works for some cameras on
    // some computers and only for some drivers, so don't rely on it to work!
    camera.setValue(CV_CAP_PROP_FRAME_WIDTH, DESIRED_CAMERA_WIDTH);
    camera.setValue(CV_CAP_PROP_FRAME_HEIGHT, DESIRED_CAMERA_HEIGHT);
    // Create a GUI window for display on the screen.
    namedWindow(windowName); // Resizable window, might not work on Windows.
    // Run forever, until the user hits Escape to "break" out of this loop.
    while true do
    begin
      // Grab the next camera frame. Note that you can't modify camera frames.
      camera.read(cameraFrame);
      if Assigned(cameraFrame) then
      begin
        if (cameraFrame.empty()) then
        begin
          Writeln('ERROR: Couldn''t grab the next camera frame.');
          Halt(1);
        end;
        imshow(windowName, cameraFrame);
      end;
      // IMPORTANT: Wait for atleast 20 milliseconds, so that the image can be displayed on the screen!
      if (waitKey(20) = VK_ESCAPE) then // Escape Key
        break; // Quit the program!
    end;

    camera.release;

    cvDestroyAllWindows;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;

end.
