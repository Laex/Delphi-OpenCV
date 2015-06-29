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

program cv_GoodFeaturesToTrack_Video;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  System.SysUtils,
  ocv.highgui_c,
  ocv.core_c,
  ocv.core.types_c,
  ocv.imgproc_c,
  ocv.imgproc.types_c;

type
  TPointsArr = array [0 .. 250] of TCvPoint2D32f;
  PPointsArr = ^TPointsArr;

var
  img1, img2, img3, eig, temp: PIplImage;

  points: array [0 .. 1] of PPointsArr;
  pointsRow1: TPointsArr;
  ipar1, count, j: integer;
  capture: PCvCapture;
  c: integer;

begin
  try
    capture := cvCreateCameraCapture(CV_CAP_ANY);
    if not Assigned(capture) then
      Halt;
    ipar1 := 10;
    while true do
    begin
      // получаем кадр
      img3 := cvQueryFrame(capture);
      img2 := cvCreateImage(cvGetSize(img3), IPL_DEPTH_8U, 1);
      img1 := cvCloneImage(img3);
      cvCvtColor(img3, img2, CV_RGB2GRAY);
      eig := cvCreateImage(cvGetSize(img2), 32, 1);
      temp := cvCreateImage(cvGetSize(img2), 32, 1);

      points[0] := @pointsRow1[0];
      count := 250;

      cvGoodFeaturesToTrack(img2, eig, temp, @points[0][0], @ipar1, 0.05, 10, nil, 5, 0, 0.09);
      cvFindCornerSubPix(img2, @points[0][0], count, cvSize(5, 5), cvSize(-1, -1),
        cvTermCriteria(CV_TERMCRIT_ITER or CV_TERMCRIT_EPS, 20, 0.03));

      for j := 0 to ipar1 - 1 do
      begin
        cvCircle(img1, cvPointFrom32f(points[0][j]), 2, CV_RGB(0, 255, 255), -1, 8, 0);
        cvCircle(img1, cvPointFrom32f(points[0][j]), 4, CV_RGB(255, 0, 0), -1, 8, 0);
      end;
      cvNamedWindow('bb', 1);
      cvShowImage('bb', img1);
      c := cvWaitKey(33);
      if (c = 27) then
        Break
    end;
    cvReleaseImage(img1);
    cvReleaseImage(img2);
    cvReleaseImage(eig);
    cvReleaseImage(temp);
    cvReleaseCapture(capture);
    cvDestroyAllWindows;
  except
    on E: Exception do
      WriteLn(E.ClassName, ': ', E.Message);
  end;

end.
