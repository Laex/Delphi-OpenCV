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

// This is the motion template code motempl.c from the OpenCV samples/c directory.
// It's just too fun not to also include here and is described in detail in Chapter 10.
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
  **************************************************
*)

program ch10_motempl;

{$APPTYPE CONSOLE}
{$POINTERMATH ON}
{$R *.res}

uses
  Winapi.Windows,
  System.SysUtils,
  System.Character,
  ocv.utils,
  ocv.core_c,
  ocv.highgui_c,
  ocv.core.types_c,
  ocv.imgproc.types_c,
  ocv.imgproc_c,
  ocv.tracking_c,
  ocv.compat;

procedure help();
begin
  WriteLn('This one uses a video camera to demonstrate motion templates in chapter 10.');
  WriteLn(' 1 Point the camera away from you');
  WriteLn(' 2 Start the program');
  WriteLn(' 3 Wait a few seconds until the video window goes black');
  WriteLn(' 4 Move in front of the camera and watch the templates being drawn along with');
  WriteLn('   their segmented motion vectors.');
  WriteLn('Ussage: ');
  WriteLn('./ch10_motempl');
end;

// various tracking parameters (in seconds)
const
  CLOCKS_PER_SEC = 1000;
  MHI_DURATION = 1;
  MAX_TIME_DELTA = 0.5;
  MIN_TIME_DELTA = 0.05;
  // number of cyclic frame buffer used for motion detection
  // (should, probably, depend on FPS)
  N = 4;

var
  // ring image buffer
  buf: array of pIplImage;
  last: Integer = 0;
  // temporary images
  mhi: pIplImage = nil; // MHI
  orient: pIplImage = nil; // orientation
  mask: pIplImage = nil; // valid orientation mask
  segmask: pIplImage = nil; // motion segmentation map
  storage: pCvMemStorage = nil; // temporary storage

  // parameters:
  // img - input video frame
  // dst - resultant motion picture
  // args - optional parameters
procedure update_mhi(img: pIplImage; dst: pIplImage; diff_threshold: Integer);
var
  timestamp: double;
  size: TCvSize;
  i, idx1, idx2: Integer;
  silh: pIplImage;
  seq: pCvSeq;
  comp_rect: TCvRect;
  count, angle: double;
  center: TCvPoint;
  magnitude: double;
  color: TCvScalar;
begin
  timestamp := GetTickCount div CLOCKS_PER_SEC; // get current time in seconds
  size := cvSize(img^.width, img^.height); // get current frame size
  idx1 := last;

  // allocate images at the beginning or
  // reallocate them if the frame size is changed
  if (not Assigned(mhi)) or (mhi^.width <> size.width) or (mhi^.height <> size.height) then
  begin
    if Length(buf) = 0 then
      SetLength(buf, N);

    for i := 0 to N - 1 do
    begin
      cvReleaseImage(buf[i]);
      buf[i] := cvCreateImage(size, IPL_DEPTH_8U, 1);
      cvZero(buf[i]);
    end;
    cvReleaseImage(mhi);
    cvReleaseImage(orient);
    cvReleaseImage(segmask);
    cvReleaseImage(mask);

    mhi := cvCreateImage(size, IPL_DEPTH_32F, 1);
    cvZero(mhi); // clear MHI at the beginning
    orient := cvCreateImage(size, IPL_DEPTH_32F, 1);
    segmask := cvCreateImage(size, IPL_DEPTH_32F, 1);
    mask := cvCreateImage(size, IPL_DEPTH_8U, 1);
  end;

  cvCvtColor(img, buf[last], CV_BGR2GRAY); // convert frame to grayscale

  idx2 := (last + 1) mod N; // index of (last - (N-1))th frame
  last := idx2;

  silh := buf[idx2];
  cvAbsDiff(buf[idx1], buf[idx2], silh); // get difference between frames

  cvThreshold(silh, silh, diff_threshold, 1, CV_THRESH_BINARY); // and threshold it
  cvUpdateMotionHistory(silh, mhi, timestamp, MHI_DURATION); // update MHI

  // convert MHI to blue 8u image
  cvCvtScale(mhi, mask, 255. / MHI_DURATION, (MHI_DURATION - timestamp) * 255. / MHI_DURATION);
  cvZero(dst);
  cvCvtPlaneToPix(mask, nil, nil, nil, dst);

  // calculate motion gradient orientation and valid orientation mask
  cvCalcMotionGradient(mhi, mask, orient, MAX_TIME_DELTA, MIN_TIME_DELTA, 3);

  if not Assigned(storage) then
    storage := cvCreateMemStorage(0)
  else
    cvClearMemStorage(storage);

  // segment motion: get sequence of motion components
  // segmask is marked motion components map. It is not used further
  seq := cvSegmentMotion(mhi, segmask, storage, timestamp, MAX_TIME_DELTA);

  // iterate through the motion components,
  // One more iteration (i == -1) corresponds to the whole image (global motion)
  for i := -1 to seq^.total - 1 do
  begin

    if (i < 0) then
    begin // case of the whole image
      comp_rect := cvRect(0, 0, size.width, size.height);
      color := CV_RGB(255, 255, 255);
      magnitude := 100;
    end
    else
    begin // i-th motion component
      comp_rect := pCvConnectedComp(cvGetSeqElem(seq, i))^.rect;
      if (comp_rect.width + comp_rect.height) < 100 then // reject very small components
        continue;
      color := CV_RGB(255, 0, 0);
      magnitude := 30;
    end;

    // select component ROI
    cvSetImageROI(silh, comp_rect);
    cvSetImageROI(mhi, comp_rect);
    cvSetImageROI(orient, comp_rect);
    cvSetImageROI(mask, comp_rect);

    // calculate orientation
    angle := cvCalcGlobalOrientation(orient, mask, mhi, timestamp, MHI_DURATION);
    angle := 360.0 - angle; // adjust for images with top-left origin

    count := cvNorm(silh, nil, CV_L1, nil); // calculate number of points within silhouette ROI

    cvResetImageROI(mhi);
    cvResetImageROI(orient);
    cvResetImageROI(mask);
    cvResetImageROI(silh);

    // check for the case of little motion
    if count < (comp_rect.width * comp_rect.height * 0.05) then
      continue;

    // draw a clock with arrow indicating the direction
    center := cvPoint((comp_rect.x + comp_rect.width div 2), (comp_rect.y + comp_rect.height div 2));

    cvCircle(dst, center, cvRound(magnitude * 1.2), color, 3, CV_AA, 0);
    cvLine(dst, center, cvPoint(cvRound(center.x + magnitude * cos(angle * CV_PI / 180)), cvRound(center.y - magnitude * sin(angle * CV_PI / 180))),
      color, 3, CV_AA, 0);
  end;
end;

Var
  motion: pIplImage = nil;
  capture: pCvCapture = nil;
  image: pIplImage;
  S:string;

begin
  try
    if (ParamCount = 1)and(FileExists(ParamStr(1))) then
        capture := cvCaptureFromFile(ParamStr(1).AsPAnsiChar)
    else
      capture := cvCaptureFromCAM(CV_CAP_ANY);

    if Assigned(capture) then
    begin
      help();
      cvNamedWindow('Motion', 1);

      While True do
      begin
        image := cvQueryFrame(capture);
        if Assigned(image) then
        begin
          if not Assigned(motion) then
          begin
            motion := cvCreateImage(cvSize(image^.width, image^.height), 8, 3);
            cvZero(motion);
            motion^.origin := image^.origin;
          end;
        end;

        update_mhi(image, motion, 30);
        cvShowImage('Motion', motion);

        if (cvWaitKey(10) >= 0) then
          break;
      end;
      cvReleaseCapture(capture);
      cvDestroyWindow('Motion');
    end
    else
    begin
      WriteLn('Failed to open camera or file');
      help();
    end;
  except
    on E: Exception do
      WriteLn(E.ClassName, ': ', E.Message);
  end;

end.
