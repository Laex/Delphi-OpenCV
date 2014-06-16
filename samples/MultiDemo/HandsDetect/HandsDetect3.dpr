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
// Original: http://anikettatipamula.blogspot.ro/2012/02/hand-gesture-using-opencv.html
// *******************************************************************

program HandsDetect3;

{$APPTYPE CONSOLE}
{$POINTERMATH ON}
{$R *.res}

uses
  System.SysUtils,
  ocv.highgui_c,
  ocv.core_c,
  ocv.core.types_c,
  ocv.imgproc_c,
  ocv.imgproc.types_c;

Var
  capture: PCvCapture;
  frame: pIplImage;
  hsv_image: pIplImage = nil;
  hsv_mask: pIplImage;
  hsv_min, hsv_max: TCvScalar;

begin
  try

    hsv_min := CvScalar(0, 30, 80, 0);
    hsv_max := CvScalar(20, 150, 255, 0);

    capture := cvCreateCameraCapture(CV_CAP_ANY);
    cvNamedWindow('capture', CV_WINDOW_AUTOSIZE);
    cvNamedWindow('hsv-img', CV_WINDOW_AUTOSIZE);
    cvNamedWindow('hsv-msk', CV_WINDOW_AUTOSIZE);
    while true do
    begin
      frame := cvQueryFrame(capture);
      if not Assigned(hsv_image) then
      begin
        hsv_image := cvCreateImage(cvGetSize(frame), 8, 3);
        hsv_mask := cvCreateImage(cvGetSize(frame), 8, 1);
      end;
      cvCvtColor(frame, hsv_image, CV_BGR2HSV);
      cvShowImage('hsv-img', hsv_image);
      cvInRangeS(hsv_image, hsv_min, hsv_max, hsv_mask);
      cvShowImage('hsv-msk', hsv_mask);

      cvShowImage('capture', frame);
      if cvWaitKey(33) = 27 then
        Break;
    end;
    cvReleaseCapture(capture);
    cvReleaseImage(hsv_image);
    cvReleaseImage(hsv_mask);
    cvDestroyAllWindows;
  except
    on E: Exception do
      WriteLn(E.ClassName, ': ', E.Message);
  end;

end.
