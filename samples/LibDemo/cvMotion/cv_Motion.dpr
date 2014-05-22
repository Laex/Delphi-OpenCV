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
// opencv\samples\c\motempl.c
// ***************************************************************

program cv_Motion;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  WinApi.Windows,
  System.SysUtils,
  System.Character,
  ocv.highgui_c,
  ocv.core_c,
  ocv.core.types_c,
  ocv.imgproc_c,
  ocv.imgproc.types_c,
  ocv.tracking_c;

procedure help;
begin
  Writeln('This program demonstrated the use of motion templates -- basically using the gradients');
  Writeln('of thresholded layers of decaying frame differencing. New movements are stamped on top with floating system');
  Writeln('time code and motions too old are thresholded away. This is the ''motion history file''. The program reads from the camera of your choice or from');
  Writeln('a file. Gradients of motion history are used to detect direction of motoin etc');
  Writeln('Usage :');
  Writeln('motempl [camera number 0-n or file name, default is camera 0]');
end;

// various tracking parameters (in seconds)
const
  MHI_DURATION = 1;
  MAX_TIME_DELTA = 0.5;
  MIN_TIME_DELTA = 0.05;
  // number of cyclic frame buffer used for motion detection
  // (should, probably, depend on FPS)
  N: Integer = 4;
  CLOCKS_PER_SEC = 1000;

Var
  // ring image buffer
  buf: ppIplImage = nil;
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
Var
  timestamp: double;
  size: TCvSize;
  i, idx1: Integer;
  silh: pIplImage;
  seq: pCvSeq;
  comp_rect: TCvRect;
  count, angle: double;
  center: TCvPoint;
  magnitude: double;
  color: TCvScalar;
  idx2: Integer;

begin
  timestamp := GetTickCount / CLOCKS_PER_SEC; // get current time in seconds
  size := CvSize(img^.width, img^.height); // get current frame size
  idx1 := last;
  // allocate images at the beginning or
  // reallocate them if the frame size is changed
  if (not Assigned(mhi)) or (mhi^.width <> size.width) or (mhi^.height <> size.height) then
  begin
    if not Assigned(buf) then
    begin
      buf := AllocMem(N * sizeof(pIplImage));
      ZeroMemory(buf, N * sizeof(pIplImage));
    end;

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
  cvMerge(mask, 0, 0, 0, dst);

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
  // One more iteration (i = -1) corresponds to the whole image (global motion)
  for i := -1 to seq^.total - 1 do
  begin
    if (i < 0) then
    begin // case of the whole image
      comp_rect := CvRect(0, 0, size.width, size.height);
      color := CV_RGB(255, 255, 255);
      magnitude := 100;
    end
    else
    begin // i-th motion component
      comp_rect := pCvConnectedComp(cvGetSeqElem(seq, i))^.rect;
      if (comp_rect.width + comp_rect.height < 100) then // reject very small components
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

    count := cvNorm(silh, 0, CV_L1, 0); // calculate number of points within silhouette ROI

    cvResetImageROI(mhi);
    cvResetImageROI(orient);
    cvResetImageROI(mask);
    cvResetImageROI(silh);

    // check for the case of little motion
    if (count < comp_rect.width * comp_rect.height * 0.05) then
      continue;

    // draw a clock with arrow indicating the direction
    center := CvPoint((comp_rect.x + comp_rect.width div 2), (comp_rect.y + comp_rect.height div 2));

    cvCircle(dst, center, cvRound(magnitude * 1.2), color, 3, CV_AA, 0);
    cvLine(dst, center, CvPoint(cvRound(center.x + magnitude * cos(angle * CV_PI / 180)),
      cvRound(center.y - magnitude * sin(angle * CV_PI / 180))), color, 3, CV_AA, 0);
  end;
end;

Var
  motion: pIplImage = nil;
  capture: pCvCapture = nil;
  image: pIplImage;
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

    if Assigned(capture) then
    begin
      cvNamedWindow('Motion', 1);

      while True do
      begin
        image := cvQueryFrame(capture);
        if not Assigned(image) then
          break;

        if not Assigned(motion) then
        begin
          motion := cvCreateImage(CvSize(image^.width, image^.height), 8, 3);
          cvZero(motion);
          motion^.origin := image^.origin;
        end;

        update_mhi(image, motion, 30);
        cvShowImage('Motion', motion);

        if (cvWaitKey(10) >= 0) then
          break;
      end;
      cvReleaseCapture(capture);
      cvDestroyWindow('Motion');
    end;

  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;

end.
