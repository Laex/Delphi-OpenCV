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
// ch10_ex10_1b_Horn_Schunck   Optical flow by the Horn-Schunck method
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
program ch10_ex10_1b_Horn_Schunck;

{$APPTYPE CONSOLE}
{$POINTERMATH ON}
{$R *.res}

uses
  System.SysUtils,
  ocv.core_c,
  ocv.highgui_c,
  ocv.core.types_c,
  ocv.legacy,
  uResourcePaths;

Var
  imgA, imgB, velx, vely, imgC: pIplImage;
  step, x, y                  : Integer;
  px, py                      : pFloat;

begin
  try
    // Initialize, load two images from the file system, and
    // allocate the images and other structures we will need for
    // results.

    // exit if no input images
    imgA := cvLoadImage(cResourceMedia + 'OpticalFlow0.jpg', 0);
    imgB := cvLoadImage(cResourceMedia + 'OpticalFlow1.jpg', 0);
    if (not Assigned(imgA)) or (not Assigned(imgB)) then
    begin
      Writeln('One of OpticalFlow0.jpg and/or OpticalFlow1.jpg didn''t load');
      Halt;
    end;

    velx := cvCreateImage(cvGetSize(imgA), IPL_DEPTH_32F, 1);
    vely := cvCreateImage(cvGetSize(imgA), IPL_DEPTH_32F, 1);

    imgC := cvCreateImage(cvGetSize(imgA), IPL_DEPTH_8U, 3);

    cvNamedWindow('OpticalFlow0');
    cvNamedWindow('OpticalFlow1');
    cvNamedWindow('Flow Results');

    cvShowImage('OpticalFlow0', imgA);
    cvShowImage('OpticalFlow1', imgB);

    // Call the actual Horn and Schunck algorithm
    //
    cvCalcOpticalFlowHS(imgA, imgB, 0, velx, vely, 0.10, cvTermCriteria(CV_TERMCRIT_ITER or CV_TERMCRIT_EPS,
      imgA^.width, 1E-6));

    // Now make some image of what we are looking at:
    //
    cvZero(imgC);
    step := 4;
    y    := 0;
    While y < imgC^.height do
    begin
      px := pFloat(velx^.imageData + y * velx^.widthStep);
      py := pFloat(vely^.imageData + y * vely^.widthStep);
      x  := 0;
      While x < imgC^.width do
      begin
        if (px[x] > 1) and (py[x] > 1) then
        begin
          cvCircle(imgC, cvPoint(x, y), 2, //
            // cvScalar(100),                 // CVX_GRAY50
            cvScalar(255), // CVX_WHITE
            -1);
          cvLine(imgC, cvPoint(x, y), cvPoint(Trunc(x + px[x] / 2), Trunc(y + py[x] / 2)), CV_RGB(255, 0, 0), 1, 0);
        end;
        x := x + step;
      end;
      y := y + step;
    end;
    // show tracking
    cvShowImage('Flow Results', imgC);

    cvWaitKey(0);

    // destroy windows
    cvDestroyWindow('OpticalFlow0');
    cvDestroyWindow('OpticalFlow1');
    cvDestroyWindow('Flow Results');
    // release memory
    cvReleaseImage(imgA);
    cvReleaseImage(imgB);
    cvReleaseImage(imgC);

  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;

end.
