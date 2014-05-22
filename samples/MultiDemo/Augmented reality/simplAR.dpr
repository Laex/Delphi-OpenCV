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
// Original:
// http://dsynflo.blogspot.ru/2010/06/simplar-augmented-reality-for-ocv.html
// *******************************************************************

program simplAR;

{$APPTYPE CONSOLE}
{$POINTERMATH ON}
{$R *.res}

uses
  System.SysUtils,
  ocv.highgui_c,
  ocv.core_c,
  ocv.core.types_c,
  ocv.calib3d_c,
  ocv.imgproc_c,
  ocv.imgproc.types_c,
  uResourcePaths;

const
  // Print pattern "chessboard 6x5.jpg"
  trailer_filename = cResourceMedia + 'trailer.avi';
  pic_filename = cResourceMedia + 'pic.jpg';

Var
  capture: pCvCapture = nil;
  image: pIplImage = nil;
  frame: pIplImage = nil;
  neg_img, cpy_img: pIplImage;
  key: Integer = 0;
  fcount: Integer = 0;
  option: Integer = 0;
  vid: pCvCapture = nil;
  pic: pIplImage;
  b_width: Integer = 5;
  b_height: Integer = 4;
  b_squares: Integer = 20;
  b_size: TCvSize;

  warp_matrix: pCvMat;
  corners: pCvPoint2D32f;
  corner_count: Integer;

  gray: pIplImage;
  found: Integer;

  p: array [0 .. 3] of TCvPoint2D32f;
  q: array [0 .. 3] of TCvPoint2D32f;

  blank: pIplImage;
  pp: array [0 .. 3] of TCvPoint;

begin
  try

    WriteLn('Select an option to run the program');
    WriteLn;
    WriteLn('1. Show an Image over the pattern.');
    WriteLn('2. Play a Clip over the pattern.');
    WriteLn('3. Mark the pattern.');
    Write('>');
    Readln(option);

    // Quit on invalid entry
    if (option < 1) or (option > 3) then
    begin
      WriteLn('Invalid selection.');
      Halt;
    end;

    capture := cvCreateCameraCapture(0);
    if not Assigned(capture) then
      Halt;

    // Use a video with aspect ratio 4:3
    vid := cvCreateFileCapture(trailer_filename);
    if not Assigned(vid) then
    begin
      cvReleaseCapture(capture);
      Halt;
    end;

    pic := cvLoadImage(pic_filename);
    cvFlip(pic, pic, 1);

    b_size := cvSize(b_width, b_height);
    // The pattern actually has 6 x 5 squares, but has 5 x 4 := 20 'ENCLOSED' corners

    warp_matrix := cvCreateMat(3, 3, CV_32FC1);
    corners := AllocMem(b_squares * SizeOf(TCvPoint2D32f));

    cvNamedWindow('Video', CV_WINDOW_AUTOSIZE);

    while (key <> 27) do
    begin
      image := cvQueryFrame(capture);
      if not Assigned(image) then
        break;
      cvFlip(image, image, 1);

      cpy_img := cvCreateImage(cvGetSize(image), 8, 3);
      neg_img := cvCreateImage(cvGetSize(image), 8, 3);

      gray := cvCreateImage(cvGetSize(image), image^.depth, 1);
      found := cvFindChessboardCorners(image, b_size, corners, @corner_count, CV_CALIB_CB_ADAPTIVE_THRESH or
        CV_CALIB_CB_FILTER_QUADS);

      cvCvtColor(image, gray, CV_BGR2GRAY);

      // This function identifies the pattern from the gray image, saves the valid group of corners
      cvFindCornerSubPix(gray, corners, corner_count, cvSize(11, 11), cvSize(-1, -1),
        cvTermCriteria(CV_TERMCRIT_EPS + CV_TERMCRIT_ITER, 30, 0.1));

      if (corner_count = b_squares) then
      begin
        if (option = 1) then
        begin
          blank := cvCreateImage(cvGetSize(pic), 8, 3);
          cvZero(blank);
          cvNot(blank, blank);

          // Set of source points to calculate Perspective matrix
          q[0].x := pic^.width * 0;
          q[0].y := pic^.height * 0;
          q[1].x := pic^.width;
          q[1].y := pic^.height * 0;

          q[2].x := pic^.width;
          q[2].y := pic^.height;
          q[3].x := pic^.width * 0;
          q[3].y := pic^.height;

          // Set of destination points to calculate Perspective matrix
          p[0].x := corners[0].x;
          p[0].y := corners[0].y;
          p[1].x := corners[4].x;
          p[1].y := corners[4].y;

          p[2].x := corners[19].x;
          p[2].y := corners[19].y;
          p[3].x := corners[15].x;
          p[3].y := corners[15].y;

          // Calculate Perspective matrix
          cvGetPerspectiveTransform(@q, @p, warp_matrix);

          // Boolean juggle to obtain 2D-Augmentation
          cvZero(neg_img);
          cvZero(cpy_img);

          cvWarpPerspective(pic, neg_img, warp_matrix, CV_INTER_LINEAR + CV_WARP_FILL_OUTLIERS, cvScalarAll(0));
          cvWarpPerspective(blank, cpy_img, warp_matrix, CV_INTER_LINEAR + CV_WARP_FILL_OUTLIERS, cvScalarAll(0));
          cvNot(cpy_img, cpy_img);

          cvAnd(cpy_img, image, cpy_img);
          cvOr(cpy_img, neg_img, image);

          cvShowImage('Video', image);
          cvReleaseImage(blank);
        end
        else if (option = 2) then
        begin
          frame := cvQueryFrame(vid);
          if not Assigned(frame) then
            WriteLn('error frame');

          blank := cvCreateImage(cvGetSize(frame), 8, 3);
          cvZero(blank);
          cvNot(blank, blank);

          q[0].x := frame^.width * 0;
          q[0].y := frame^.height * 0;
          q[1].x := frame^.width;
          q[1].y := frame^.height * 0;

          q[2].x := frame^.width;
          q[2].y := frame^.height;
          q[3].x := frame^.width * 0;
          q[3].y := frame^.height;

          p[0].x := corners[0].x;
          p[0].y := corners[0].y;
          p[1].x := corners[4].x;
          p[1].y := corners[4].y;

          p[2].x := corners[19].x;
          p[2].y := corners[19].y;
          p[3].x := corners[15].x;
          p[3].y := corners[15].y;

          cvGetPerspectiveTransform(@q, @p, warp_matrix);

          // Boolean juggle to obtain 2D-Augmentation
          cvZero(neg_img);
          cvZero(cpy_img);

          cvWarpPerspective(frame, neg_img, warp_matrix, CV_INTER_LINEAR + CV_WARP_FILL_OUTLIERS, cvScalarAll(0));
          cvWarpPerspective(blank, cpy_img, warp_matrix, CV_INTER_LINEAR + CV_WARP_FILL_OUTLIERS, cvScalarAll(0));
          cvNot(cpy_img, cpy_img);

          cvAnd(cpy_img, image, cpy_img);
          cvOr(cpy_img, neg_img, image);

          cvShowImage('Video', image);
          cvReleaseImage(blank);
        end
        else
        begin
          pp[0].x := Trunc(corners[0].x);
          pp[0].y := Trunc(corners[0].y);
          pp[1].x := Trunc(corners[4].x);
          pp[1].y := Trunc(corners[4].y);

          pp[2].x := Trunc(corners[19].x);
          pp[2].y := Trunc(corners[19].y);
          pp[3].x := Trunc(corners[15].x);
          pp[3].y := Trunc(corners[15].y);

          cvLine(image, pp[0], pp[1], CV_RGB(255, 0, 0), 2);
          cvLine(image, pp[1], pp[2], CV_RGB(0, 255, 0), 2);
          cvLine(image, pp[2], pp[3], CV_RGB(0, 0, 255), 2);
          cvLine(image, pp[3], pp[0], CV_RGB(255, 255, 0), 2);

          // or simply
          cvDrawChessboardCorners(image, b_size, corners, corner_count, found);

          cvShowImage('Video', image);
        end;
      end
      else
      begin
        // Show gray image when pattern is not detected
        cvFlip(gray, gray);
        cvShowImage('Video', gray);
      end;
      key := cvWaitKey(1);
      cvReleaseImage(cpy_img);
      cvReleaseImage(neg_img);
      cvReleaseImage(gray);
    end;

    cvDestroyWindow('Video');
    cvReleaseCapture(vid);
    cvReleaseMat(warp_matrix);
    cvReleaseCapture(capture);
    FreeMem(corners);

  except
    on E: Exception do
      WriteLn(E.ClassName, ': ', E.Message);
  end;

end.
