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
// Original file:
// opencv\samples\c\polar_transforms.c
// ***************************************************************

program cv_LinearPolar;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  System.SysUtils,
  System.Character,
  ocv.highgui_c,
  ocv.core_c,
  ocv.core.types_c,
  ocv.imgproc_c,
  ocv.imgproc.types_c;

procedure help;
begin
  WriteLn('This program illustrates Linear-Polar and Log-Polar image transforms');
  WriteLn('Usage :');
  WriteLn('polar_transforms [[camera number -- Default 0],[AVI path_filename]]');
end;

Var
  capture: pCvCapture = nil;
  log_polar_img: pIplImage = nil;
  lin_polar_img: pIplImage = nil;
  recovered_img: pIplImage = nil;
  frame: pIplImage;
  filename: AnsiString;

begin
  try
    help;

    if (ParamCount = 0) or ((ParamCount = 1) and (length(ParamStr(1)) = 1) and (isdigit(ParamStr(1)[1]))) then
      capture := cvCreateCameraCapture(iif(ParamCount = 1, ParamStr(1), CV_CAP_ANY))
    else if (ParamCount = 1) and FileExists(ParamStr(1)) then
    begin
      filename := ParamStr(1);
      capture := cvCreateFileCapture(pCvChar(@filename[1]))
    end
    else
      Halt;
    if not Assigned(capture) then
    begin
      WriteLn('Could not initialize capturing...');
      WriteLn('Usage: <CAMERA_NUMBER>, or <VIDEO_FILE>');
      Halt;
    end;

    cvNamedWindow('Linear-Polar', 0);
    cvNamedWindow('Log-Polar', 0);
    cvNamedWindow('Recovered image', 0);

    cvMoveWindow('Linear-Polar', 20, 20);
    cvMoveWindow('Log-Polar', 700, 20);
    cvMoveWindow('Recovered image', 20, 700);

    while True do
    begin
      frame := nil;

      frame := cvQueryFrame(capture);
      if not Assigned(frame) then
        break;

      if not Assigned(log_polar_img) then
      begin
        log_polar_img := cvCreateImage(cvSize(frame^.width, frame^.height), IPL_DEPTH_8U, frame^.nChannels);
        lin_polar_img := cvCreateImage(cvSize(frame^.width, frame^.height), IPL_DEPTH_8U, frame^.nChannels);
        recovered_img := cvCreateImage(cvSize(frame^.width, frame^.height), IPL_DEPTH_8U, frame^.nChannels);
      end;

      cvLogPolar(frame, log_polar_img, cvPoint2D32f(frame^.width shr 1, frame^.height shr 1), 70,
        CV_INTER_LINEAR + CV_WARP_FILL_OUTLIERS);
      cvLinearPolar(frame, lin_polar_img, cvPoint2D32f(frame^.width shr 1, frame^.height shr 1), 70,
        CV_INTER_LINEAR + CV_WARP_FILL_OUTLIERS);

{$IFDEF 0}
      cvLogPolar(log_polar_img, recovered_img, cvPoint2D32f(frame^.width shr 1, frame^.height shr 1), 70,
        CV_WARP_INVERSE_MAP + CV_INTER_LINEAR);
{$ELSE}
      cvLinearPolar(lin_polar_img, recovered_img, cvPoint2D32f(frame^.width shr 1, frame^.height shr 1), 70,
        CV_WARP_INVERSE_MAP + CV_INTER_LINEAR + CV_WARP_FILL_OUTLIERS);
{$ENDIF}
      cvShowImage('Log-Polar', log_polar_img);
      cvShowImage('Linear-Polar', lin_polar_img);
      cvShowImage('Recovered image', recovered_img);

      if cvWaitKey(10) >= 0 then
        break;
    end;

    cvReleaseCapture(capture);
    cvDestroyWindow('Linear-Polar');
    cvDestroyWindow('Log-Polar');
    cvDestroyWindow('Recovered image');

  except
    on E: Exception do
      WriteLn(E.ClassName, ': ', E.Message);
  end;

end.
