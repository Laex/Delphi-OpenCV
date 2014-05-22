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

program fback_c;

{$APPTYPE CONSOLE}
{$POINTERMATH ON}
{$R *.res}

uses
  System.SysUtils,
  ocv.highgui_c,
  ocv.core_c,
  ocv.core.types_c,
  ocv.imgproc_c,
  ocv.imgproc.types_c,
  ocv.tracking_c;

procedure drawOptFlowMap(const flow: pCvMat; cflowmap: pCvMat; step: Integer; scale: double; color: TCvScalar);
var
  x, y: Integer;
  fxy: TCvPoint2D32f;
begin
  y := 0;
  While y < cflowmap^.rows do
  begin
    x := 0;
    while x < cflowmap^.cols do
    begin
      fxy := pCvPoint2D32f(CV_MAT_ELEM(flow^, SizeOf(TCvPoint2D32f), y, x))^;
      cvLine(cflowmap, cvPoint(x, y), cvPoint(cvRound(x + fxy.x), cvRound(y + fxy.y)), color, 1, 8, 0);
      cvCircle(cflowmap, cvPoint(x, y), 2, color, -1, 8, 0);
      x := x + step;
    end;
    y := y + step;
  end;
end;

Var
  capture: pCvCapture;
  prevgray: pCvMat = nil;
  gray: pCvMat = nil;
  flow: pCvMat = nil;
  cflow: pCvMat = nil;
  firstFrame: boolean;
  frame: pIplImage;
  temp: pCvMat;

begin
  try
    Writeln(' This program demonstrate dense Farneback optical flow');
    Writeln('It read from camera 0, and shows how to use and display dense Franeback optical flow');
    Writeln('Usage: ');
    Writeln('fback_c ');

    capture := cvCreateCameraCapture(0);
    Assert(Assigned(capture));

    cvNamedWindow('flow', 1);
    while true do
    begin
      firstFrame := Assigned(gray);

      frame := cvQueryFrame(capture);
      Assert(Assigned(frame));

      if not Assigned(gray) then
      begin
        gray := cvCreateMat(frame^.height, frame^.width, CV_8UC1);
        prevgray := cvCreateMat(gray^.rows, gray^.cols, gray^._type);
        flow := cvCreateMat(gray^.rows, gray^.cols, CV_32FC2);
        cflow := cvCreateMat(gray^.rows, gray^.cols, CV_8UC3);
      end;
      cvCvtColor(frame, gray, CV_BGR2GRAY);

      if firstFrame then
      begin
        cvCalcOpticalFlowFarneback(prevgray, gray, flow, 0.5, 3, 15, 3, 5, 1.2, 0);
        cvCvtColor(prevgray, cflow, CV_GRAY2BGR);
        drawOptFlowMap(flow, cflow, 16, 1.5, CV_RGB(0, 255, 0));
        cvShowImage('flow', cflow);
      end;

      if (cvWaitKey(10) = 27) then
        break;

      CV_SWAP(prevgray, gray, temp);

    end;
    cvReleaseCapture(capture);

  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;

end.
