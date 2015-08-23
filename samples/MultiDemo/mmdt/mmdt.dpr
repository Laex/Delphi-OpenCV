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
  ***************************************************************
  Original file:
  http://vidikon.com/download/multimd.zip
  http://blog.vidikon.com/?p=447
  ***************************************************************
*)
program mmdt;

{$APPTYPE CONSOLE}
{$POINTERMATH ON}
{$R *.res}

uses
  Winapi.Windows,
  System.SysUtils,
  System.Generics.Collections,
  ocv.highgui_c,
  ocv.core_c,
  ocv.core.types_c,
  ocv.imgproc_c,
  ocv.imgproc.types_c,
  ocv.tracking_c;

const
  MAX_CAMERA = 10;
  _MAX_TEXT = 100;
  MAX_JPG_CAM = 100000;
  CLOCKS_PER_SEC = 1000;

var
  all_camera: Integer = 0;
  capture: array [0 .. MAX_CAMERA - 1] of pCvCapture;
  allcam: Integer;
  threadvar
  // thread
    NumMultiThread: Integer;

var
  UnloadCapture, WindowCapture: array [0 .. MAX_CAMERA - 1] of Integer;

  MHI_DURATION: double = 1;
  MAX_TIME_DELTA: double = 0.5;
  MIN_TIME_DELTA: double = 0.05;
  // number of cyclic frame buffer used for motion detection
  // (should, probably, depend on FPS)
  N: Integer = 4;

  // ring image buffer
  buf: array [0 .. MAX_CAMERA - 1] of ppIplImage;
  last: array [0 .. MAX_CAMERA - 1] of Integer;
  // temporary images
  mhi: array [0 .. MAX_CAMERA - 1] of pIplImage; // MHI
  orient: array [0 .. MAX_CAMERA - 1] of pIplImage; // orientation
  mask: array [0 .. MAX_CAMERA - 1] of pIplImage; // valid orientation mask
  segmask: array [0 .. MAX_CAMERA - 1] of pIplImage; // motion segmentation map
  storage: array [0 .. MAX_CAMERA - 1] of pCvMemStorage; // temporary storage

  MINMSEC, MAXMSEC: Integer;
  PATHJPG: AnsiString;
  OUTJPG: Integer;
  PATHAVI: AnsiString;
  OUTAVI: Integer;
  PATHLOG: AnsiString;
  OUTLOG: Integer;

  WriteCam: array [0 .. MAX_CAMERA - 1] of Integer;

  ww: Integer = 0;
  i: Integer;
  lpT: DWORD;
  h: tHANDLE;
  str: String; // [21];
  k: Integer = 0;

  // parameters:
  // img - input video frame
  // dst - resultant motion picture
  // args - optional parameters
  // camera - num camera
function update_mhi(img: pIplImage; dst: pIplImage; diff_threshold, camera: Integer): Integer;
Var
  timestamp: double; // get current time in seconds
  size: TCvSize; // get current frame size
  i, idx1, idx2: Integer;
  silh: pIplImage;
  seq: pCvSeq;
  comp_rect: TCvRect;
  count, angle: double;
  center: TCvPoint;
  magnitude: double;
  color: TCvScalar;
  cv1, cv2: TCvPoint;
  DetectZone: Integer;
  jj, i1, j1, xx, yy: Integer;
begin
  timestamp := GetTickCount / CLOCKS_PER_SEC;
  // get current time in seconds
  size := CvSize(img^.width, img^.height); // get current frame size
  idx1 := last[camera];
  DetectZone := 0;
  // allocate images at the beginning or
  // reallocate them if the frame size is changed
  if not Assigned(mhi[camera]) or (mhi[camera].width <> size.width) or (mhi[camera].height <> size.height) then
  begin
    if (buf[camera] = nil) then
      buf[camera] := AllocMem(N * sizeof(pIplImage));

    for i := 0 to N - 1 do
    begin
      cvReleaseImage(buf[camera][i]);
      buf[camera][i] := cvCreateImage(size, IPL_DEPTH_8U, 1);
      cvZero(buf[camera][i]);
    end;
    cvReleaseImage(mhi[camera]);
    cvReleaseImage(orient[camera]);
    cvReleaseImage(segmask[camera]);
    cvReleaseImage(mask[camera]);

    mhi[camera] := cvCreateImage(size, IPL_DEPTH_32F, 1);
    cvZero(mhi[camera]); // clear MHI at the beginning
    orient[camera] := cvCreateImage(size, IPL_DEPTH_32F, 1);
    segmask[camera] := cvCreateImage(size, IPL_DEPTH_32F, 1);
    mask[camera] := cvCreateImage(size, IPL_DEPTH_8U, 1);
  end;

  cvCvtColor(img, buf[camera][last[camera]], CV_BGR2GRAY); // convert frame to grayscale

  idx2 := (last[camera] + 1) mod N; // index of (last - (N-1))th frame
  last[camera] := idx2;

  silh := buf[camera][idx2];
  cvAbsDiff(buf[camera][idx1], buf[camera][idx2], silh); // get difference between frames

  cvThreshold(silh, silh, diff_threshold, 1, CV_THRESH_BINARY); // and threshold it
  cvUpdateMotionHistory(silh, mhi[camera], timestamp, MHI_DURATION); // update MHI

  // convert MHI to blue 8u image
  cvCvtScale(mhi[camera], mask[camera], 255. / MHI_DURATION, (MHI_DURATION - timestamp) * 255. / MHI_DURATION);
  cvZero(dst);
  // cvCvtPlaneToPix( mask[camera], 0, 0, 0, dst );
  cvMerge(mask[camera], 0, 0, 0, dst);

  // calculate motion gradient orientation and valid orientation mask
  cvCalcMotionGradient(mhi[camera], mask[camera], orient[camera], MAX_TIME_DELTA, MIN_TIME_DELTA, 3);

  if not Assigned(storage[camera]) then
    storage[camera] := cvCreateMemStorage(0)
  else
    cvClearMemStorage(storage[camera]);

  // segment motion: get sequence of motion components
  // segmask is marked motion components map. It is not used further
  seq := cvSegmentMotion(mhi[camera], segmask[camera], storage[camera], timestamp, MAX_TIME_DELTA);

  // iterate through the motion components,
  // One more iteration (i = -1) corresponds to the whole image (global motion)
  // uchar* ptr;
  // ptr := (uchar*) (dst^.imageData );
  jj := 0;
  for i := -1 to seq^.total - 1 do
  begin
    if (i < 0) then
    begin // case of the whole image
      comp_rect := CvRect(0, 0, size.width, size.height);
      color := CV_RGB(255, 255, 255);
      magnitude := 100;
    end
    else
    begin
      // i-th motion component
      comp_rect := pCvConnectedComp(cvGetSeqElem(seq, i))^.rect;
      if (comp_rect.width + comp_rect.height < 100) then // reject very small components
        continue;

      color := CV_RGB(255, 0, 0);
      magnitude := 30;
    end;

    if (i = -1) then
      continue;
    Inc(jj);
    Inc(DetectZone);

    // select component ROI
    cvSetImageROI(silh, comp_rect);
    cvSetImageROI(mhi[camera], comp_rect);
    cvSetImageROI(orient[camera], comp_rect);
    cvSetImageROI(mask[camera], comp_rect);

    // calculate orientation
    angle := cvCalcGlobalOrientation(orient[camera], mask[camera], mhi[camera], timestamp, MHI_DURATION);
    angle := 360.0 - angle;
    // adjust for images with top-left origin

    count := cvNorm(silh, 0, CV_L1, 0); // calculate number of points within silhouette ROI

    cvResetImageROI(mhi[camera]);
    cvResetImageROI(orient[camera]);
    cvResetImageROI(mask[camera]);
    cvResetImageROI(silh);

    // check for the case of little motion
    if (count < comp_rect.width * comp_rect.height * 0.05) then
      continue;

    // draw a clock with arrow indicating the direction
    center := CvPoint((comp_rect.x + comp_rect.width div 2), (comp_rect.y + comp_rect.height div 2));

    cvCircle(dst, center, cvRound(magnitude * 1.2), color, 3, CV_AA, 0);
    cvLine(dst, center, CvPoint(cvRound(center.x + magnitude * cos(angle * CV_PI / 180)),
      cvRound(center.y - magnitude * sin(angle * CV_PI / 180))), color, 3, CV_AA, 0);

    xx := comp_rect.width;
    if (xx > comp_rect.height) then
      xx := comp_rect.height;

    cv1.x := comp_rect.x;
    cv1.y := comp_rect.y;
    cv2.x := comp_rect.x + comp_rect.width;
    cv2.y := comp_rect.y + comp_rect.height;
    cvRectangle(img, cv1, cv2, color, 3, CV_AA, 0);
    cvLine(img, center, CvPoint(cvRound(center.x + (xx / 2) * cos(angle * CV_PI / 180)),
      cvRound(center.y - (xx / 2) * sin(angle * CV_PI / 180))), color, 5, CV_AA, 0);
  end;
  Result := DetectZone;
end;

function MultiThread(lpParameter: LPVOID): DWORD; stdcall;
Var
  localNumMultiThread: Integer;
  localWindowCapture: Integer;
  JPG_CAM: Integer;

  motion: pIplImage;
  image1: pIplImage;
  // buf3: String;
  buf1, buf2, buf: AnsiString;
  cadr: Longint;

  detect: Integer;
  detect1: Integer;
  waitkey: Integer;
  cvVideoWriter: pCvVideoWriter;
  logpolar_frame: pIplImage;
  image: pIplImage;
  F: TextFile;
  ii: LONG;

begin
  localNumMultiThread := pInteger(lpParameter)^;
  localWindowCapture := WindowCapture[localNumMultiThread];
  JPG_CAM := 0;

  capture[localNumMultiThread] := cvCreateCameraCapture(localNumMultiThread);

  // Work with camera
  if Assigned(capture[localNumMultiThread]) then
  begin
    motion := nil;
    image1 := nil;
    cadr := 0;
    if (OUTLOG <> 0) then
    begin
      buf1 := PATHLOG + Format('cam%d.log', [localNumMultiThread]);
      AssignFile(F, buf1);
      Rewrite(F);
    end;
    buf := Format('Camera #%d', [localNumMultiThread]);
    cvNamedWindow(pCvChar(@buf[1]), 1);
    detect := 0;
    waitkey := MINMSEC;
    // Create avi file
    cvVideoWriter := nil;
    while True do
    begin
      if not Assigned(capture[localNumMultiThread]) then
        break;
      image := cvQueryFrame(capture[localNumMultiThread]);
      if not Assigned(image) then
        break;

      if not Assigned(motion) then
      begin
        motion := cvCreateImage(CvSize(image^.width, image^.height), 8, 3);
        cvZero(motion);
        motion^.origin := image^.origin;
      end;
      if not Assigned(image1) then
      begin
        image1 := cvCreateImage(CvSize(image^.width, image^.height), 8, 3);
        cvZero(image1);
        image1^.origin := image1^.origin;
      end;
      cvCopy(image, image1);
      if (OUTAVI <> 0) then
      begin
        if not Assigned(cvVideoWriter) then
        begin
          buf1 := PATHAVI + Format('cam%d.avi', [localNumMultiThread]);
          cvVideoWriter := cvCreateVideoWriter(pCvChar(@buf1[1]), CV_FOURCC('X', 'V', 'I', 'D'), 15,
            CvSize(image^.width div 2, image^.height div 2));
          logpolar_frame := cvCreateImage(CvSize(image^.width div 2, image^.height div 2), IPL_DEPTH_8U, 3);
          cvZero(logpolar_frame);
          logpolar_frame^.origin := logpolar_frame^.origin;
          // I don't why, but another's not work
          // my be cvLogPolar made some different param in IplImage
          cvLogPolar(image1, logpolar_frame, cvPoint2D32f(image1^.width div 2, image1^.height div 2), 40,
            CV_INTER_LINEAR + CV_WARP_FILL_OUTLIERS);
        end;
      end;
      detect1 := update_mhi(image, motion, 30, localNumMultiThread);
      if (detect > 0) then
        Dec(detect);
      if (detect1 > 0) then
        detect := 5;
      if (detect > 0) then
        waitkey := MINMSEC
      else
        waitkey := MAXMSEC;
      if (localWindowCapture <> WindowCapture[localNumMultiThread]) then
      begin
        if (localWindowCapture > 0) then
          cvDestroyWindow(pCvChar(@buf[1]))
        else
          cvNamedWindow(pCvChar(@buf[1]), 1);
        localWindowCapture := WindowCapture[localNumMultiThread];
      end;
      if (localWindowCapture <> 0) then
        cvShowImage(pCvChar(@buf[1]), image);
      if (detect > 0) and (OUTJPG <> 0) then
      begin
        buf1 := PATHJPG + Format('cam%dn', [localNumMultiThread]);
        ii := JPG_CAM + 1;
        repeat
          buf2 := buf1 + Format('%d.jpg', [ii]);
          ii := ii + 1;
        until not FileExists(buf2);
        cvSaveImage(pCvChar(@buf2[1]), image1);
      end;
      if (detect > 0) then
      begin
        if (OUTAVI <> 0) then
        begin
          if not Assigned(logpolar_frame) then
            logpolar_frame := cvCreateImage(CvSize(image1^.width div 2, image1^.height div 2), image1^.depth, image1^.nChannels);
          cvResize(image1, logpolar_frame, 2);
          cvWriteFrame(cvVideoWriter, logpolar_frame);
        end;
        if (OUTLOG <> 0) then
        begin
          WriteLn(F, Format('cadr #%d', [cadr]));
          cadr := cadr + 1;
        end;
      end;
      cvWaitKey(waitkey);
      if (UnloadCapture[localNumMultiThread] = 0) then
        break;
      // printf('%d...',localNumMultiThread);
      // WriteCam[localNumMultiThread]:=1;
    end;
    cvReleaseCapture(capture[localNumMultiThread]);
    if (localWindowCapture <> 0) then
      cvDestroyWindow(pCvChar(@buf[1]));
    cvReleaseVideoWriter(cvVideoWriter);
  end;
  if (OUTLOG <> 0) then
    CloseFile(F);
  UnloadCapture[localNumMultiThread] := -1;
  Result := 0;
end;

procedure help;
begin
  WriteLn;
  WriteLn('Commands:');
  WriteLn('exit - exit program');
  WriteLn('help - this help');
  WriteLn('winon - windows on');
  WriteLn('winoff - windows off');
end;

begin
  try
    // Params
    allcam := 2;
    MINMSEC := 200;
    MAXMSEC := 1000;
    PATHJPG := 'Result\';
    PATHAVI := 'Result\';
    PATHLOG := 'Result\';

    ww := 1; // WINDOWS ON
    OUTJPG := 1; // OUTJPG ON
    OUTAVI := 1; // OUTAVI ON
    OUTLOG := 0; // OUTLOG ON
    // init for detect
    FillChar(capture, sizeof(capture), 0);
    FillChar(buf, sizeof(capture), 0);
    FillChar(last, sizeof(last), 0);
    FillChar(mhi, sizeof(mhi), 0);
    FillChar(orient, sizeof(orient), 0);
    FillChar(mask, sizeof(mask), 0);
    FillChar(segmask, sizeof(segmask), 0);
    FillChar(storage, sizeof(storage), 0);
    FillChar(WriteCam, sizeof(WriteCam), 0);
    // Search camera divice
    WriteLn('Searching');
    for i := 0 to allcam - 1 do
    begin
      Write('.');
      capture[i] := nil;
      capture[i] := cvCreateCameraCapture(i);
      UnloadCapture[i] := 0;
      if not Assigned(capture[i]) then
        continue;
      UnloadCapture[i] := 1;
      // WindowCapture[i] := ww;
      Inc(all_camera);
      cvReleaseCapture(capture[i]);
    end;
    WriteLn(#13#10'All camera:=', all_camera);
    if (allcam = 0) then
    begin
      WriteLn('Error with camera! Sorry...');
      Sleep(5000);
      Halt;
    end;

    // Create threads for camera
    for i := 0 to all_camera - 1 do
    begin
      if (UnloadCapture[i] = 0) then
        continue;
      WriteLn('Create thread fo cam # ', i);
      NumMultiThread := i;
      h := CreateThread(NiL, 0, @MultiThread, @NumMultiThread, 0, lpT);
      Sleep(100);
      CloseHandle(h);
    end;
    help;
    // Go in to command interpretator
    Sleep(1000);
    while True do
    begin
      Write('MMDT# ');
      Readln(str);
      k := 0;
      if (SameText(str, 'exit')) then
        break;
      if (SameText(str, 'help')) then
        help;
      if (SameText(str, 'winon')) then
      begin
        for i := 0 to allcam - 1 do
          WindowCapture[i] := 1;
        k := 1;
      end;
      if (SameText(str, 'winoff')) then
      begin
        for i := 0 to allcam - 1 do
          WindowCapture[i] := 0;
        k := 1;
      end;
      if (k = 0) then
        WriteLn('Unknown command');
    end;

    // Delete Captures
    if (all_camera > 0) then
      for i := 0 to all_camera - 1 do
        if Assigned(capture[i]) then
        begin
          // cvReleaseCapture( &capture[i] );
          UnloadCapture[i] := 0;
          repeat
            Sleep(100);
          until (UnloadCapture[i] = -1);
        end;

  except
    on E: Exception do
      WriteLn(E.ClassName, ': ', E.Message);
  end;

end.
