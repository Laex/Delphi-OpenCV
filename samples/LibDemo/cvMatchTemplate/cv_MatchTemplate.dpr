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

program cv_MatchTemplate;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  ocv.highgui_c,
  ocv.core_c,
  ocv.core.types_c,
  ocv.imgproc_c,
  ocv.imgproc.types_c,
  uResourcePaths;

var
  imgSrc, imgTmp, imgMat: PIplImage;
  min: double;
  p1, p2: TCvPoint;

begin
  imgSrc := cvLoadImage(cResourceMedia + 'My_Desk.jpg');
  imgTmp := cvLoadImage(cResourceMedia + 'Stapler2.jpg');
  imgMat := cvCreateImage(CvSize(imgSrc.Width - imgTmp.Width + 1, imgSrc.Height - imgTmp.Height + 1), IPL_DEPTH_32F, 1);
  try
    cvNamedWindow('Src');
    cvNamedWindow('Temp');
    cvNamedWindow('Match');
    cvNamedWindow('Result');

    cvShowImage('Src', imgSrc);
    cvShowImage('Temp', imgTmp);

    cvMatchTemplate(imgSrc, imgTmp, imgMat, CV_TM_CCOEFF_NORMED);
    cvShowImage('Match', imgMat);

    cvMinMaxLoc(imgMat, @min, @min, nil, @p1, nil);
    p2.X := p1.X + imgTmp.Width - 1;
    p2.Y := p1.Y + imgTmp.Height - 1;
    cvRectangle(imgSrc, p1, p2, CV_RGB(255, 0, 0));

    cvShowImage('Result', imgSrc);
    cvWaitKey(0);

  finally
    cvReleaseImage(imgSrc);
    cvReleaseImage(imgTmp);
    cvReleaseImage(imgMat);
    cvDestroyAllWindows;
  end;

end.
