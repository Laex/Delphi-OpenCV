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
  //*****************************************************************
  // Original file:
  // opencv\samples\c\adaptiveskindetector.cpp
  // ****************************************************************

program cv_CalcHist;

{$APPTYPE CONSOLE}
{$POINTERMATH ON}
{$R *.res}

uses
  WinApi.Windows,
  System.SysUtils,
  ocv.highgui_c,
  ocv.core_c,
  ocv.core.types_c,
  ocv.imgproc_c,
  ocv.imgproc.types_c,
  uResourcePaths;

Function DrawHist(src_img: pIplImage): pIplImage;
Var
  i, j, bin_w, hist_size, ch_width, sch: integer;
  max_value: Single;
  range_0: array [0 .. 1] of Single;
  ranges: pSingle;
  hist_img: pIplImage;
  dst_img: array [0 .. 3] of pIplImage;
  Hist: pCvHistogram;
  CS: TcvSize;
Begin
  range_0[0] := 0;
  range_0[1] := 256;
  ranges := @range_0;
  dst_img[0] := NIL;
  dst_img[1] := NIL;
  dst_img[2] := NIL;
  dst_img[3] := NIL;
  hist_size := 256;
  ch_width := 260;
//  sch := 0;
  CS.Height := src_img.Height;
  CS.Width := src_img.Width;
  sch := src_img.NChannels;
  for i := 0 to sch - 1 do
  begin
    dst_img[i] := cvCreateImage(CS, src_img.Depth, 1);
  end;
  Hist := cvCreateHist(1, @hist_size, CV_HIST_ARRAY, @ranges, 1);
  hist_img := cvCreateImage(cvSize((ch_width * sch), 200), 8, 1);
  if sch = 1 Then
    cvCopy(src_img, dst_img[0], NIL)
  else
    cvSplit(src_img, dst_img[0], dst_img[1], dst_img[2], dst_img[3]);
  cvSet(hist_img, cvScalarAll(255), nil);
  for i := 0 to sch - 1 do
  begin
    cvCalcHist(dst_img[i], Hist, 0, NIL);
    cvGetMinMaxHistValue(Hist, nil, @max_value, nil, nil);
    cvConvertScale(Hist.bins, Hist.bins, (hist_img.Height / max_value), 0);
    bin_w := cvRound(ch_width / hist_size);
    for j := 0 to hist_size - 1 do
      cvRectangle(hist_img, cvPoint(j * bin_w + (i * ch_width), hist_img.Height),
        cvPoint((j + 1) * bin_w + (i * ch_width), hist_img.Height - cvRound(cvGetReal1D(Hist.bins, j))), cvScalarAll(0),
        -1, 8, 0);
  End;
  Result := hist_img;
  for i := 0 to sch - 1 do
    cvReleaseImage(dst_img[i]);
  cvReleaseHist(Hist);
End;

Var
  src, Hist: pIplImage;

begin
  try
    src := cvLoadImage(cResourceMedia+'baboon.jpg', CV_LOAD_IMAGE_ANYCOLOR OR CV_LOAD_IMAGE_ANYDEPTH);
    Hist := DrawHist(src);
    cvShowImage('Image Source', src);
    cvNamedWindow('Histogram', CV_WINDOW_AUTOSIZE);
    cvShowImage('Histogram', Hist);
    cvWaitKey(0);
    cvReleaseImage(src);
    cvReleaseImage(Hist);
    cvDestroyAllWindows;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
