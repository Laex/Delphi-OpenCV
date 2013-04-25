(* /*****************************************************************
  //                       Delphi-OpenCV Demo
  //               Copyright (C) 2013 Project Delphi-OpenCV
  // ****************************************************************
  // Contributor:
  // Mikhail Grigorev
  // email: sleuthhound@gmail.com
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
  ******************************************************************* *)

// JCL_DEBUG_EXPERT_GENERATEJDBG OFF
// JCL_DEBUG_EXPERT_INSERTJDBG OFF
// JCL_DEBUG_EXPERT_DELETEMAPFILE OFF
program MotionDetect;

{$APPTYPE CONSOLE}
{$POINTERMATH ON}
{$R *.res}

uses
  System.SysUtils,
  Math,
  uLibName in '..\..\..\include\uLibName.pas',
  highgui_c in '..\..\..\include\highgui\highgui_c.pas',
  imgproc.types_c in '..\..\..\include\imgproc\imgproc.types_c.pas',
  imgproc_c in '..\..\..\include\imgproc\imgproc_c.pas',
  imgproc in '..\..\..\include\imgproc\imgproc.pas',
  core in '..\..\..\include\core\core.pas',
  core.types_c in '..\..\..\include\core\Core.types_c.pas',
  core_c in '..\..\..\include\core\core_c.pas';

var
  storage: pCvMemStorage = nil;
  capture: pCvCapture = nil;
  frame: pIplImage = nil;
  frame_grey: pIplImage = nil;
  difference_img: pIplImage = nil;
  oldframe_grey: pIplImage = nil;
  contours: pCvSeq = nil;
  c: pCvSeq = nil;
  // rect: TCvRect;
  rect2d: TCvBox2D;
  key: integer;
  first: boolean = true;

begin
  try
    capture := cvCreateCameraCapture(0);
    storage := cvCreateMemStorage(0);
    frame := cvQueryFrame(capture);
    frame_grey := cvCreateImage(cvSize(frame^.width, frame^.height), IPL_DEPTH_8U, 1);

    while true do
    begin
      frame := cvQueryFrame(capture);
      if frame = nil then
        break;
      cvCvtColor(frame, frame_grey, CV_RGB2GRAY);
      if first then
      begin
        difference_img := cvCloneImage(frame_grey);
        oldframe_grey := cvCloneImage(frame_grey);
        cvConvertScale(frame_grey, oldframe_grey, 1.0, 0.0);
        first := false;
      end;
      cvAbsDiff(oldframe_grey, frame_grey, difference_img);
      cvSmooth(difference_img, difference_img, CV_BLUR);
      cvThreshold(difference_img, difference_img, 25, 255, CV_THRESH_BINARY);
      contours := AllocMem(SizeOf(TCvSeq));
      cvFindContours(difference_img, storage, @contours, SizeOf(TCvContour), CV_RETR_LIST, CV_CHAIN_APPROX_NONE,
        cvPoint(0, 0));
      c := contours;
      while (c <> nil) do
      begin
        // rect := cvBoundingRect(c, 0);
        rect2d := cvMinAreaRect2(c);
        { cvRectangle(frame, cvPoint(rect.x, rect.y),
          cvPoint(rect.x+rect.width, rect.y+rect.height),
          cvScalar(0, 0, 255, 0), 2, 8, 0); }
        cvRectangle(frame, cvPoint(Round(rect2d.center.x - rect2d.size.width / 2),
          Round(rect2d.center.y - rect2d.size.height / 2)), cvPoint(Round(rect2d.center.x + rect2d.size.width / 2),
          Round(rect2d.center.y + rect2d.size.height / 2)), cvScalar(0, 0, 255, 0), 2, 8, 0);
        c := c.h_next;
      end;
      cvShowImage('Output Image', frame);
      cvShowImage('Difference Image', difference_img);
      cvConvertScale(frame_grey, oldframe_grey, 1.0, 0.0);
      cvClearMemStorage(storage);
      contours := nil;
      c := nil;
      FreeMem(contours, SizeOf(TCvSeq));
      key := cvWaitKey(33);
      if (key = 27) then
        break;
    end;
    // Оcвобождаем реcурcы
    cvReleaseMemStorage(storage);
    cvReleaseCapture(capture);
    cvReleaseImage(oldframe_grey);
    cvReleaseImage(difference_img);
    cvReleaseImage(frame_grey);
    cvDestroyAllWindows();
  except
    on E: Exception do
      WriteLn(E.ClassName, ': ', E.Message);
  end;

end.
