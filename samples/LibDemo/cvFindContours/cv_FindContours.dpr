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

program cv_FindContours;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  System.SysUtils,
  ocv.highgui_c,
  ocv.core_c,
  ocv.core.types_c,
  ocv.imgproc_c,
  ocv.imgproc.types_c,
  uResourcePaths;

const
  filename = cResourceMedia + 'opencv_logo_with_text_sm.png';

var
  image: pIplImage = nil;
  dst: pIplImage = nil;
  img_gray: pIplImage = Nil;
  contours: pCvSeq = nil;
  storage: pCvMemStorage = nil;

begin
  try
    // получаем картинку
    image := cvLoadImage(filename, CV_LOAD_IMAGE_UNCHANGED);
    if Assigned(image) then
    begin
      cvNamedWindow('Source image', CV_WINDOW_AUTOSIZE);
      cvNamedWindow('Gray image', CV_WINDOW_AUTOSIZE);
      cvNamedWindow('Threshold image', CV_WINDOW_AUTOSIZE);
      cvNamedWindow('Contour image', CV_WINDOW_AUTOSIZE);
      cvShowImage('Source image', image);
      // cоздаем изображение в градациях cерого
      img_gray := cvCreateImage(cvSize(image^.width, image^.height), IPL_DEPTH_8U, 1);
      dst := cvCreateImage(cvSize(image^.width, image^.height), IPL_DEPTH_8U, 1);
      cvCvtColor(image, img_gray, CV_BGR2GRAY);
      cvShowImage('Gray image', img_gray);
      storage := cvCreateMemStorage(0);
      cvThreshold(img_gray, dst, 128, 255, CV_THRESH_BINARY_INV);
      // cvAdaptiveThreshold(img_gray, img_gray, 255, CV_ADAPTIVE_THRESH_GAUSSIAN_C, CV_THRESH_BINARY, 21, 7);
      cvShowImage('Threshold image', dst);
      contours := AllocMem(SizeOf(TCvSeq));
      cvClearMemStorage(storage);
      cvFindContours(dst, storage, @contours, SizeOf(TCvContour), CV_RETR_LIST, CV_CHAIN_APPROX_SIMPLE, cvPoint(0, 0));
      cvDrawContours(image, contours, CV_RGB(100, 200, 0), CV_RGB(200, 100, 0), 2, 2, CV_AA, cvPoint(0, 0));
      cvShowImage('Contour image', image);
      cvWaitKey(0);
      cvDestroyAllWindows;
      cvReleaseImage(image);
      cvReleaseImage(img_gray);
      cvReleaseImage(dst);
    end;
  except
    on E: Exception do
      WriteLn(E.ClassName, ': ', E.Message);
  end;

end.
