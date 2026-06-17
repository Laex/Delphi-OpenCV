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

program ClassTest;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  WinApi.Windows,
  System.SysUtils,
  ocv.cls.core,
  ocv.highgui_c,
  ocv.cls.highgui,
  uResourcePaths;

procedure TestCameraCapture;

  procedure PrintCameraInfo(V: IVideoCapture);
  begin
    WriteLn('-------- Camera info --------');
    WriteLn('FRAME_WIDTH:               ', Trunc(V.Prop[CAP_PROP_FRAME_WIDTH]));
    WriteLn('FRAME_HEIGHT:              ', Trunc(V.Prop[CAP_PROP_FRAME_HEIGHT]));
    WriteLn('FPS:                       ', Trunc(V.Prop[CAP_PROP_FPS]));
    WriteLn('FRAME_COUNT:               ', Trunc(V.Prop[CAP_PROP_FRAME_COUNT]));
    WriteLn('PROP_FORMAT:               ', Trunc(V.Prop[CAP_PROP_FORMAT]));
    WriteLn('PROP_MODE:                 ', Trunc(V.Prop[CAP_PROP_MODE]));
    WriteLn('PROP_BRIGHTNESS:           ', Trunc(V.Prop[CAP_PROP_BRIGHTNESS]));
    WriteLn('PROP_CONTRAST:             ', Trunc(V.Prop[CAP_PROP_CONTRAST]));
    WriteLn('PROP_SATURATION:           ', Trunc(V.Prop[CAP_PROP_SATURATION]));
    WriteLn('PROP_HUE:                  ', Trunc(V.Prop[CAP_PROP_HUE]));
    WriteLn('PROP_GAIN:                 ', Trunc(V.Prop[CAP_PROP_GAIN]));
    WriteLn('PROP_EXPOSURE:             ', Trunc(V.Prop[CAP_PROP_EXPOSURE]));
    WriteLn('PROP_CONVERT_RGB:          ', Trunc(V.Prop[CAP_PROP_CONVERT_RGB]));
    WriteLn('PROP_WHITE_BALANCE_BLUE_U: ', Trunc(V.Prop[CAP_PROP_WHITE_BALANCE_BLUE_U]));
    WriteLn('PROP_RECTIFICATION:        ', Trunc(V.Prop[CAP_PROP_RECTIFICATION]));
    WriteLn('PROP_SHARPNESS:            ', Trunc(V.Prop[CAP_PROP_SHARPNESS]));
    WriteLn('PROP_AUTO_EXPOSURE:        ', Trunc(V.Prop[CAP_PROP_AUTO_EXPOSURE]));
    WriteLn('PROP_GAMMA:                ', Trunc(V.Prop[CAP_PROP_GAMMA]));
    WriteLn('PROP_TEMPERATURE:          ', Trunc(V.Prop[CAP_PROP_TEMPERATURE]));
    WriteLn('PROP_TRIGGER:              ', Trunc(V.Prop[CAP_PROP_TRIGGER]));
    WriteLn('PROP_TRIGGER_DELAY:        ', Trunc(V.Prop[CAP_PROP_TRIGGER_DELAY]));
    WriteLn('PROP_WHITE_BALANCE_RED_V:  ', Trunc(V.Prop[CAP_PROP_WHITE_BALANCE_RED_V]));
    WriteLn('PROP_ZOOM:                 ', Trunc(V.Prop[CAP_PROP_ZOOM]));
    WriteLn('PROP_FOCUS:                ', Trunc(V.Prop[CAP_PROP_FOCUS]));
    WriteLn('PROP_GUID:                 ', Trunc(V.Prop[CAP_PROP_GUID]));
    WriteLn('PROP_ISO_SPEED:            ', Trunc(V.Prop[CAP_PROP_ISO_SPEED]));
    WriteLn('PROP_BACKLIGHT:            ', Trunc(V.Prop[CAP_PROP_BACKLIGHT]));
    WriteLn('PROP_PAN:                  ', Trunc(V.Prop[CAP_PROP_PAN]));
    WriteLn('PROP_TILT:                 ', Trunc(V.Prop[CAP_PROP_TILT]));
    WriteLn('PROP_ROLL:                 ', Trunc(V.Prop[CAP_PROP_ROLL]));
    WriteLn('PROP_IRIS:                 ', Trunc(V.Prop[CAP_PROP_IRIS]));
    WriteLn('PROP_SETTINGS:             ', Trunc(V.Prop[CAP_PROP_SETTINGS]));
  end;

const
  windowName                     = 'Test VideoCapture'; // Name shown in the GUI window.
  VK_ESCAPE                      = 27;
  DESIRED_CAMERA_WIDTH: Integer  = 800;
  DESIRED_CAMERA_HEIGHT: Integer = 600;

Var
  M: IMat;
  V: IVideoCapture;
begin
  V := TVideoCapture.Create;
  if V.Open(CAP_ANY) then
  begin
    WriteLn('CAP_PROP_FRAME_WIDTH=', DESIRED_CAMERA_WIDTH, ' -> ',
      V.PropSet(CAP_PROP_FRAME_WIDTH, DESIRED_CAMERA_WIDTH));
    // V.Prop[CAP_PROP_FRAME_WIDTH] := DESIRED_CAMERA_WIDTH;
    WriteLn('CAP_PROP_FRAME_HEIGHT=', DESIRED_CAMERA_HEIGHT, ' -> ',
      V.PropSet(CAP_PROP_FRAME_HEIGHT, DESIRED_CAMERA_HEIGHT));
    // V.Prop[CAP_PROP_FRAME_HEIGHT] := DESIRED_CAMERA_HEIGHT;
    namedWindow(windowName);
    WriteLn('Press <ESC> to exit');
    WriteLn('Press <Space> to info');
    while true do
    begin
      // Grab the next camera frame. Note that you can't modify camera frames.
      if V.read(M) then
      begin
        if Assigned(M) and (M.empty()) then
        begin
          WriteLn('ERROR: Couldn''t grab the next camera frame.');
          Halt(1);
        end;
        imshow(windowName, M);
      end;
      // IMPORTANT: Wait for atleast 20 milliseconds, so that the image can be displayed on the screen!
      case waitKey(20) of
        VK_ESCAPE:
          break;
        32:
          PrintCameraInfo(V);
      end;
    end;
    destroyAllWindows;
  end;
end;

procedure TestCameraCaptureFileName;
const
  windowName      = 'Test VideoCapture'; // Name shown in the GUI window.
  VK_ESCAPE       = 27;
  VIDEO_FILE_NAME = cResourceMedia + '768x576.avi';

Var
  M: IMat;
  V: IVideoCapture;
begin
  V := TVideoCapture.Create(VIDEO_FILE_NAME);
  if V.isOpened then
  begin
    namedWindow(windowName);
    while true do
    begin
      // Grab the next camera frame. Note that you can't modify camera frames.
      if V.read(M) then
      begin
        if Assigned(M) and (M.empty()) then
        begin
          WriteLn('ERROR: Couldn''t grab the next camera frame.');
          Halt(1);
        end;
        imshow(windowName, M);
      end;
      // IMPORTANT: Wait for atleast 20 milliseconds, so that the image can be displayed on the screen!
      if (waitKey(20) = VK_ESCAPE) then // Escape Key
        break;                          // Quit the program!
    end;
    destroyAllWindows;
  end;
end;

begin
  try
    ReportMemoryLeaksOnShutdown:=True;
    TestCameraCapture;
    // TestCameraCaptureFileName
  except
    on E: Exception do
      WriteLn(E.ClassName, ': ', E.Message);
  end;

end.
