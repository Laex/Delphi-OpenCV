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

// Example 10-1. Pyramid Lucas-Kanade optical flow code
//
(* *************** License:**************************
  Oct. 3, 2008
  Right to use this code in any way you want without warrenty, support or any guarentee of it working.

  BOOK: It would be nice if you cited it:
  Learning OpenCV: Computer Vision with the OpenCV Library
  by Gary Bradski and Adrian Kaehler
  Published by O'Reilly Media, October 3, 2008

  AVAILABLE AT:
  http://www.amazon.com/Learning-OpenCV-Computer-Vision-Library/dp/0596516134
  Or: http://oreilly.com/catalog/9780596516130/
  ISBN-10: 0596516134 or: ISBN-13: 978-0596516130

  OTHER OPENCV SITES:
  * The source code is on sourceforge at:
  http://sourceforge.net/projects/opencvlibrary/
  * The OpenCV wiki page (As of Oct 1, 2008 this is down for changing over servers, but should come back):
  http://opencvlibrary.sourceforge.net/
  * An active user group is at:
  http://tech.groups.yahoo.com/group/OpenCV/
  * The minutes of weekly OpenCV development meetings are at:
  http://pr.willowgarage.com/wiki/OpenCV
  ************************************************** *)
program ch10_ex10_1;

{$APPTYPE CONSOLE}
{$POINTERMATH ON}
{$R *.res}

uses
  System.SysUtils,
  ocv.utils,
  ocv.core_c,
  ocv.highgui_c,
  ocv.core.types_c,
  ocv.imgproc.types_c,
  ocv.imgproc_c,
  ocv.tracking_c,
  uResourcePaths;

Const
  MAX_CORNERS = 500;

Var
  imgA: PIplImage;
  imgB: PIplImage;
  img_sz: TCvSize;
  win_size: Integer;
  imgC: PIplImage;
  eig_image: PIplImage;
  tmp_image: PIplImage;
  corner_count: Integer;
  cornersA: PCvPoint2D32f;
  features_found: array [0 .. MAX_CORNERS - 1] of byte;
  feature_errors: array [0 .. MAX_CORNERS - 1] of Single;
  pyr_sz: TCvSize;
  pyrA: PIplImage;
  pyrB: PIplImage;
  cornersB: PCvPoint2D32f;
  i: Integer;
  p0: TCvPoint;
  p1: TCvPoint;

begin
  try
    // Initialize, load two images from the file system, and
    // allocate the images and other structures we will need for
    // results.
    //
    imgA := cvLoadImage(cResourceMedia + 'OpticalFlow0.jpg', CV_LOAD_IMAGE_GRAYSCALE);
    imgB := cvLoadImage(cResourceMedia + 'OpticalFlow1.jpg', CV_LOAD_IMAGE_GRAYSCALE);
    img_sz := cvGetSize(imgA);
    win_size := 10;
    imgC := cvLoadImage(cResourceMedia + 'OpticalFlow1.jpg', CV_LOAD_IMAGE_UNCHANGED);

    // The first thing we need to do is get the features
    // we want to track.
    //
    eig_image := cvCreateImage(img_sz, IPL_DEPTH_32F, 1);
    tmp_image := cvCreateImage(img_sz, IPL_DEPTH_32F, 1);
    corner_count := MAX_CORNERS;
    cornersA := AllocMem(SizeOf(TCvPoint2D32f) * MAX_CORNERS);
    cvGoodFeaturesToTrack(imgA, eig_image, tmp_image, cornersA, @corner_count, 0.01, 5.0, 0, 3, 0, 0.04);
    cvFindCornerSubPix(imgA, cornersA, corner_count, cvSize(win_size, win_size), cvSize(-1, -1),
      cvTermCriteria(CV_TERMCRIT_ITER or CV_TERMCRIT_EPS, 20, 0.03));

    // Call the Lucas Kanade algorithm
    //
    pyr_sz := cvSize(imgA^.width + 8, imgB^.height div 3);
    pyrA := cvCreateImage(pyr_sz, IPL_DEPTH_32F, 1);
    pyrB := cvCreateImage(pyr_sz, IPL_DEPTH_32F, 1);
    cornersB := AllocMem(SizeOf(TCvPoint2D32f) * MAX_CORNERS);
    cvCalcOpticalFlowPyrLK(imgA, imgB, pyrA, pyrB, cornersA, cornersB, corner_count, cvSize(win_size, win_size), 5, @features_found,
      @feature_errors, cvTermCriteria(CV_TERMCRIT_ITER or CV_TERMCRIT_EPS, 20, 0.3), 0);
    // Now make some image of what we are looking at:
    //
    for i := 0 to corner_count - 1 do
    begin
      if (features_found[i] = 0) or (feature_errors[i] > 550) then
      begin
        // printf('Error is %f/n',feature_errors[i]);
        continue;
      end;
      // printf('Got it/n');
      p0 := cvPoint(cvRound(cornersA[i].x), cvRound(cornersA[i].y));
      p1 := cvPoint(cvRound(cornersB[i].x), cvRound(cornersB[i].y));
      cvLine(imgC, p0, p1, CV_RGB(255, 0, 0), 2);
    end;
    cvNamedWindow('ImageA', 0);
    cvNamedWindow('ImageB', 0);
    cvNamedWindow('LKpyr_OpticalFlow', 0);
    cvShowImage('ImageA', imgA);
    cvShowImage('ImageB', imgB);
    cvShowImage('LKpyr_OpticalFlow', imgC);
    cvWaitKey(0);
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;

end.
