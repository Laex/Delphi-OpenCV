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

//
// ch9_watershed image
// This is an exact copy of the watershed.cpp demo in the OpenCV ../samples/c directory
//
// Think about using a morphologically eroded forground and background segmented image as the template
// for the watershed algorithm to segment objects by color and edges for collecting
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

program ch9_watershed;

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
  ocv.compat,
  uResourcePaths;

Var
  marker_mask: PIplImage;
  markers: PIplImage;
  img0, img, img_gray, wshed: PIplImage;
  prev_pt: TCvPoint;

procedure on_mouse(event: Integer; x: Integer; y: Integer; flags: Integer; param: Pointer); cdecl;
Var
  pt: TCvPoint;
begin
  if not Assigned(img) then
    Exit;
  if (event = CV_EVENT_LBUTTONUP) or ((flags and CV_EVENT_FLAG_LBUTTON) = 0) then
    prev_pt := cvPoint(-1, -1)
  else if (event = CV_EVENT_LBUTTONDOWN) then
    prev_pt := cvPoint(x, y)
  else if (event = CV_EVENT_MOUSEMOVE) and ((flags and CV_EVENT_FLAG_LBUTTON) <> 0) then
  begin
    pt := cvPoint(x, y);
    if (prev_pt.x < 0) then
      prev_pt := pt;
    cvLine(marker_mask, prev_pt, pt, cvScalarAll(255), 5, 8, 0);
    cvLine(img, prev_pt, pt, cvScalarAll(255), 5, 8, 0);
    prev_pt := pt;
    cvShowImage('image', img);
  end;
end;

Var
  filename: AnsiString;
  rng: TCvRNG;
  c: Integer;
  storage: pCvMemStorage;
  contours: pCvSeq;
  color_tab: pCvMat;
  i, j, comp_count: Integer;
  ptr: pByte;
  t: double;
  idx: Integer;
  dst: pByte;

begin
  try
    prev_pt := cvPoint(-1, -1);

    if ParamCount > 0 then
      filename := ParamStr(1)
    else
      filename := cResourceMedia+'fruits.jpg';
    rng := CvRNG(-1);


    img0 := cvLoadImage(c_str(filename), 1);

    if not Assigned(img0) then
      Halt;

    WriteLn('Hot keys: '#13#10#9 + 'ESC - quit the program'#13#10#9 + 'r - restore the original image'#13#10#9 +
      'w or ENTER - run watershed algorithm'#13#10#13#10#9 + '(before running it, roughly mark the areas on the image)'#13#10#9 +
      '(before that, roughly outline several markers on the image)');

    cvNamedWindow('image', 1);
    cvNamedWindow('watershed transform', 1);

    img := cvCloneImage(img0);
    img_gray := cvCloneImage(img0);
    wshed := cvCloneImage(img0);
    marker_mask := cvCreateImage(cvGetSize(img), 8, 1);
    markers := cvCreateImage(cvGetSize(img), IPL_DEPTH_32S, 1);
    cvCvtColor(img, marker_mask, CV_BGR2GRAY);
    cvCvtColor(marker_mask, img_gray, CV_GRAY2BGR);

    cvZero(marker_mask);
    cvZero(wshed);
    cvShowImage('image', img);
    cvShowImage('watershed transform', wshed);
    cvSetMouseCallback('image', on_mouse, 0);

    While true do
    begin
      c := cvWaitKey(0);

      if c = 27 then
        break;

      if char(c) = 'r' then
      begin
        cvZero(marker_mask);
        cvCopy(img0, img);
        cvShowImage('image', img);
      end;

      if (chr(c) = 'w') or (c = 13) then
      begin
        storage := cvCreateMemStorage(0);
        contours := nil;
        comp_count := 0;
        // cvSaveImage( 'wshed_mask.png', marker_mask );
        // marker_mask = cvLoadImage( 'wshed_mask.png', 0 );
        cvFindContours(marker_mask, storage, @contours, sizeof(TCvContour), CV_RETR_CCOMP, CV_CHAIN_APPROX_SIMPLE,cvPoint(0,0));
        cvZero(markers);
        while (contours <> nil) do
        begin
          cvDrawContours(markers, contours, cvScalarAll(comp_count + 1), cvScalarAll(comp_count + 1), -1, -1, 8, cvPoint(0, 0));
          contours := contours^.h_next;
          comp_count := comp_count + 1;
        end;

        color_tab := cvCreateMat(1, comp_count, CV_8UC3);
        for i := 0 to comp_count - 1 do
        begin
          ptr := pByte(Integer(color_tab^.data) + i * 3);
          ptr[0] := (cvRandInt(rng) mod 180 + 50);
          ptr[1] := (cvRandInt(rng) mod 180 + 50);
          ptr[2] := (cvRandInt(rng) mod 180 + 50);
        end;

        begin
          t := cvGetTickCount();
          cvWatershed(img0, markers);
          t := cvGetTickCount() - t;
          WriteLn('exec time = ', (t / (cvGetTickFrequency() * 1000)):6:3,'ms');
        end;

        // paint the watershed image
        for i := 0 to markers^.height - 1 do
          for j := 0 to markers^.width - 1 do
          begin
            idx := Integer(CV_IMAGE_ELEM(markers, SizeOf(integer), i, j)^);
            dst := CV_IMAGE_ELEM(wshed, SizeOf(uchar), i, j * 3);
            if (idx = -1) then
            begin
              dst[0] := 255;
              dst[1] := 255;
              dst[2] := 255;
            end
            else if (idx <= 0) or (idx > comp_count) then
            begin
              dst[0] := 0;
              dst[1] := 0;
              dst[2] := 0; // should not get here
            end
            else
            begin
              ptr := pByte(Integer(color_tab^.data) + (idx - 1) * 3);
              dst[0] := ptr[0];
              dst[1] := ptr[1];
              dst[2] := ptr[2];
            end;
          end;

        cvAddWeighted(wshed, 0.5, img_gray, 0.5, 0, wshed);
        cvShowImage('watershed transform', wshed);
        cvReleaseMemStorage(storage);
        cvReleaseMat(color_tab);
      end;
    end;

  except
    on E: Exception do
      WriteLn(E.ClassName, ': ', E.Message);
  end;

end.
