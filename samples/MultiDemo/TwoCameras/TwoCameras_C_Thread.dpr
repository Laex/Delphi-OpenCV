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

program TwoCameras_C_Thread;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  WinApi.Windows,
  System.SysUtils,
  ocv.highgui_c,
  ocv.core_c,
  ocv.core.types_c,
  ocv.imgproc_c,
  ocv.imgproc.types_c;

Const
  EXIT_SUCCESS = 1;

var
  crSec: TRTLCriticalSection;

Const
  VidLeft: pCVChar = 'WebCamLeft';
  VidRight: pCVChar = 'WebcamRight';

function threadFunc(param: LPVOID): DWORD; stdcall;
Var
  frm: pIplImage;
  capture: pCvCapture;
begin
  EnterCriticalSection(crSec);
  cvNamedWindow(VidLeft);
  capture := cvCreateCameraCapture(1);
  if Assigned(capture) then
  begin
    cvSetCaptureProperty(capture, CV_CAP_PROP_FRAME_WIDTH, 320);
    cvSetCaptureProperty(capture, CV_CAP_PROP_FRAME_HEIGHT, 240);
  end;
  LeaveCriticalSection(crSec);
  if Assigned(capture) then
    repeat
      EnterCriticalSection(crSec);
      frm := cvQueryFrame(capture);
      LeaveCriticalSection(crSec);
      if Assigned(frm) then
      begin
        EnterCriticalSection(crSec);
        cvShowImage(VidLeft, frm);
        LeaveCriticalSection(crSec);
      end;
      if cvWaitKey(1) = 27 then
        break;
    until not Assigned(frm);
  EnterCriticalSection(crSec);
  cvDestroyWindow(VidLeft);
  cvReleaseCapture(capture);
  LeaveCriticalSection(crSec);
  Result := EXIT_SUCCESS;
end;

function threadFunc2(param: LPVOID): DWORD; stdcall;
Var
  frm: pIplImage;
  capture: pCvCapture;
begin
  EnterCriticalSection(crSec);
  cvNamedWindow(VidRight);
  capture := cvCreateCameraCapture(0);
  if Assigned(capture) then
  begin
    cvSetCaptureProperty(capture, CV_CAP_PROP_FRAME_WIDTH, 320);
    cvSetCaptureProperty(capture, CV_CAP_PROP_FRAME_HEIGHT, 240);
  end;
  LeaveCriticalSection(crSec);
  if Assigned(capture) then
    repeat
      EnterCriticalSection(crSec);
      frm := cvQueryFrame(capture);
      LeaveCriticalSection(crSec);
      if Assigned(frm) then
      begin
        EnterCriticalSection(crSec);
        cvShowImage(VidRight, frm);
        LeaveCriticalSection(crSec);
      end;
      if cvWaitKey(1) = 27 then
        break;
    until not Assigned(frm);
  EnterCriticalSection(crSec);
  cvDestroyWindow(VidRight);
  cvReleaseCapture(capture);
  LeaveCriticalSection(crSec);
  Result := EXIT_SUCCESS;
end;

Var
  threadID, threadID2: DWORD;
  lpHandles: array [0 .. 1] of THandle;

begin
  try
    InitializeCriticalSection(crSec);
    lpHandles[0] := CreateThread(nil, 0, @threadFunc, nil, 0, threadID);
    lpHandles[1] := CreateThread(nil, 0, @threadFunc2, nil, 0, threadID2);
    WaitForMultipleObjects(Length(lpHandles), @lpHandles, TRUE, INFINITE);
    DeleteCriticalSection(crSec);
  except
    on E: Exception do
      WriteLn(E.ClassName, ': ', E.Message);
  end;

end.
