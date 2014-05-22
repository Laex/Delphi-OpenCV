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

program cv_CvtColor;

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
  filename = cResourceMedia + 'opencv_logo_with_text.png';
  filename_gray = cResourceMedia + 'opencv_logo_with_text_gray.png';

var
  image: pIplImage = nil;
  gray_image: pIplImage = nil;

begin
  try
    image := cvLoadImage(filename, 1);
    gray_image := cvCreateImage(cvGetSize(image), IPL_DEPTH_8U, 1);
    cvCvtColor(image, gray_image, CV_RGB2GRAY);
    cvSaveImage(filename_gray, gray_image);
    cvNamedWindow(filename, CV_WINDOW_AUTOSIZE);
    cvNamedWindow('Gray image', CV_WINDOW_AUTOSIZE);
    cvShowImage(filename, image);
    cvShowImage('Gray image', gray_image);
    cvWaitKey(0);
    cvReleaseImage(image);
    cvReleaseImage(gray_image);
    cvDestroyAllWindows;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;

end.
