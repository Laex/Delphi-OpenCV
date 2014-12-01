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

program ExtractContours;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  System.SysUtils,
  ocv.highgui_c,
  ocv.core_c,
  ocv.core.types_c,
  ocv.imgproc_c,
  ocv.imgproc.types_c,
  ocv.tracking_c,
  ocv.lib,
  ocv.mat,
  uResourcePaths;

Var
  img0, img1, img2: pIplImage;
  storage: pCvMemStorage = nil;
  contours: pCvSeq = nil;
  mask, crop, maskc: pIplImage;

begin
  try
    // read in the apple (change path to the file)
    img0 := cvLoadImage(cResourceMedia+'apple.jpg');
    img1 := cvCreateImage(cvGetSize(img0), IPL_DEPTH_8U, 1);
    img2 := cvCreateImage(cvGetSize(img0), IPL_DEPTH_8U, 1);
    cvCvtColor(img0, img2, CV_RGB2GRAY);
    // apply your filter
    cvCanny(img2, img1, 100, 200, 3);
    // find the contours
    storage := cvCreateMemStorage(0);
    contours := AllocMem(SizeOf(TCvSeq));
    cvFindContours(img1, storage, @contours, SizeOf(TCvContour), CV_RETR_EXTERNAL, CV_CHAIN_APPROX_NONE, cvPoint(0, 0));
    // you could also reuse img1 here
    mask := cvCreateImage(cvGetSize(img1), IPL_DEPTH_8U, 1);
    cvZero(mask);
    // CV_FILLED fills the connected components found
    cvDrawContours(mask, contours, CV_RGB(255, 255, 255), CV_RGB(255, 255, 255), 1, CV_FILLED, 8, cvPoint(0, 0));

    (*
      Before drawing all contours you could also decide
      to only draw the contour of the largest connected component
      found. Here's some commented out code how to do that:
    *)

    // let's create a new image now
    crop := cvCreateImage(cvGetSize(img0), IPL_DEPTH_8U, 3);

    // set background to green
    cvSet(crop, CV_RGB(0, 255, 0));

    // and copy the magic apple
    cvCopy(img0, crop, mask);

    // normalize so imwrite(...)/imshow(...) shows the mask correctly!
    maskc := cvClone(mask);
    cvNormalize(maskc, mask, 0.0, 255.0, CV_MINMAX);

    // show the images
    cvNamedWindow('original', CV_WINDOW_AUTOSIZE);
    cvShowImage('original', img0);
    cvNamedWindow('mask', CV_WINDOW_AUTOSIZE);
    cvShowImage('mask', mask);
    cvNamedWindow('canny', CV_WINDOW_AUTOSIZE);
    cvShowImage('canny', img1);
    cvNamedWindow('cropped', CV_WINDOW_AUTOSIZE);
    cvShowImage('cropped', crop);

    cvSaveImage(cResourceMedia+'apple_canny.jpg',img1);
    cvSaveImage(cResourceMedia+'apple_mask.jpg',mask);
    cvSaveImage(cResourceMedia+'apple_cropped.jpg',crop);

    cvWaitKey();
    cvDestroyAllWindows();
    cvReleaseImage(img0);
    cvReleaseImage(img1);
    cvReleaseImage(img2);
    cvReleaseImage(crop);
    cvReleaseImage(mask);
    cvReleaseImage(maskc);
    cvReleaseMemStorage(storage);

  except
    on E: Exception do
      WriteLn(E.ClassName, ': ', E.Message);
  end;

end.
