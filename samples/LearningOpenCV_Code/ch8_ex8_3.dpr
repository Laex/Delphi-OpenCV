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
// Example 8-3. Finding and drawing contours on an input image
//
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

program ch8_ex8_3;

{$APPTYPE CONSOLE}
{$POINTERMATH ON}
{$R *.res}

uses
  System.SysUtils,
  ocv.core_c,
  ocv.highgui_c,
  ocv.core.types_c,
  ocv.imgproc.types_c,
  ocv.imgproc_c,
  ocv.utils,
  ocv.legacy,
  uResourcePaths;

const
  WndName = 'ch8_ex8_3';

Var
  img_8uc1          : pIplImage = nil;
  img_edge, img_8uc3: pIplImage;
  storage           : pCvMemStorage;
  first_contour     : pCvSeq;
  Nc, n, k, i       : Integer;
  p                 : pCvPoint;
  c                 : pCvSeq;

begin
  try
    // Changed this a little for safer image loading and help if not
    if ParamCount = 1 then
      img_8uc1 := cvLoadImage(c_str(ParamStr(1)), CV_LOAD_IMAGE_GRAYSCALE);
    if not Assigned(img_8uc1) then
    begin
      WriteLn('Example 8_3 Drawing Contours');
      WriteLn('Call is:');
      WriteLn('ch8_ex8_3 image');
      Halt;
    end;

    cvNamedWindow(WndName, 1);

    img_edge := cvCreateImage(cvGetSize(img_8uc1), 8, 1);
    img_8uc3 := cvCreateImage(cvGetSize(img_8uc1), 8, 3);
    cvThreshold(img_8uc1, img_edge, 128, 255, CV_THRESH_BINARY);
    storage       := cvCreateMemStorage();
    first_contour := nil;
    Nc            := cvFindContours(img_edge, storage, @first_contour, sizeof(TCvContour), CV_RETR_LIST,
      // Try all four values and see what happens
      CV_CHAIN_APPROX_SIMPLE, cvPoint(0, 0));
    n := 0;
    WriteLn('Hit any key to draw the next contour, ESC to quit');
    WriteLn('Total Contours Detected: ', Nc);
    c := first_contour;
    While c <> nil do
    begin
      cvCvtColor(img_8uc1, img_8uc3, CV_GRAY2BGR);
      cvDrawContours(img_8uc3, c, CV_RGB($FF, $00, $00), // Yarg, these are defined above, but not in the book.  Oops
        CV_RGB($00, $00, $FF), 0,                        // Try different values of max_level, and see what happens
        2, 8,                                            //
        cvPoint(0, 0));
      WriteLn('Contour ', n);
      cvShowImage(WndName, img_8uc3);
      WriteLn(c^.total, ' elements:');
      for i := 0 to c^.total - 1 do
      begin
        p := CV_GET_SEQ_ELEM(sizeof(TcvPoint), c, i);
        WriteLn('(', p^.x, ',', p^.y, ')');
      end;
      if cvWaitKey() = 27 then
        break;
      Inc(n);
      c := c^.h_next
    end;
    WriteLn('Finished all contours. Hit key to finish');
    cvCvtColor(img_8uc1, img_8uc3, CV_GRAY2BGR);
    cvShowImage(WndName, img_8uc3);
    cvWaitKey(0);
    cvDestroyWindow(WndName);
    cvReleaseImage(img_8uc1);
    cvReleaseImage(img_8uc3);
    cvReleaseImage(img_edge);

  except
    on E: Exception do
      WriteLn(E.ClassName, ': ', E.Message);
  end;

end.
