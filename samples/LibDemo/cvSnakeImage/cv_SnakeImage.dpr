//*****************************************************************
  //                       Delphi-OpenCV Demo
  //               Copyright (C) 2013 Project Delphi-OpenCV
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
  //*******************************************************************

program cv_SnakeImage;

{$APPTYPE CONSOLE}
{$POINTERMATH ON}

uses
  System.SysUtils,
  ocv.highgui_c,
  ocv.core_c,
  ocv.core.types_c,
  ocv.imgproc_c,
  ocv.imgproc.types_c,
  ocv.legacy,
  uResourcePaths;

const
  w = 500;
  filename = cResourceMedia + 'cat2.jpg';

var
  imgA, imgB, imgAA: pIplImage;
  storage: pCvMemStorage;
  contour: pCvPointArray;
  Approx_contours: PcvSeq;
  contour_num: integer;
  i1, i2, i, j, length, f1: integer;
  point: pInteger;
  c_alpha: single = 0.45;
  c_beta: single = 0.10;
  c_gamma: single = 0.45;
  contours: pCvPoint;

begin
  try
    cvNamedWindow('image', 1);
    imgA := cvLoadImage(filename, CV_LOAD_IMAGE_UNCHANGED);
    imgAA := cvCreateImage(cvSize(imgA^.width, imgA^.Height), imgA^.depth, imgA^.nChannels);
    // Smooth or Copy
    // cvSmooth(imgA, imgAA, CV_GAUSSIAN, 7, 0, 0);
    cvCopy(imgA, imgAA);
    imgB := cvCreateImage(cvSize(imgA^.width, imgA^.Height), IPL_DEPTH_8U, 1);
    cvCvtColor(imgAA, imgB, CV_BGR2GRAY);
    length := 200;
    contour := AllocMem(sizeof(TCvPoint) * length);
    for i := 0 to length - 1 do
    begin
      contour[i].x := round(200 * cos(2 * PI * i / length) + 100);
      contour[i].y := round(200 * sin(2 * PI * i / length) + 100);
    end;

    while true do
    begin
      cvSnakeImage(imgB, contour, length, @c_alpha, @c_beta, @c_gamma, CV_VALUE, cvSize(21, 21),
        cvTermCriteria(CV_TERMCRIT_ITER, 1, 0.0), 1);
      cvCopy(imgAA, imgA);
      for i := 0 to length - 2 do
        cvLine(imgA, contour[i], contour[i + 1], CV_RGB(255, 0, 0), 2, 8, 0);
      cvLine(imgA, contour[length - 1], contour[0], CV_RGB(255, 0, 0), 2, 8, 0);

      cvShowImage('image', imgA);
      if cvWaitKey(200) = 27 then
        Break;
    end;
    cvReleaseImage(imgA);
    cvReleaseImage(imgB);
    cvDestroyAllWindows;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;

end.
