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

program cv_FindContours3;

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
  filename = cResourceMedia + 'Contours.jpg';

var
  image: pIplImage = nil;
  dst: pIplImage = nil;
  img_gray: pIplImage = Nil;
  contours: pCvSeq = nil;
  lp_contour: pCvSeq = nil;
  child_contours: pCvSeq = nil;
  storage: pCvMemStorage = nil;
  color_ext, color_hole: TCvScalar;
  l_pt0, l_pt1: TCvPoint;
  l_reader: TCvSeqReader;
  l_iterator: TCvTreeNodeIterator;
  i: Integer;

begin
  try
    // получаем картинку
    image := cvLoadImage(filename, CV_LOAD_IMAGE_UNCHANGED);
    if Assigned(image) then
    begin
      cvNamedWindow('Source image', CV_WINDOW_AUTOSIZE);
      cvNamedWindow('Contour image', CV_WINDOW_AUTOSIZE);

      cvShowImage('Source image', image);
      // cоздаем изображение в градациях cерого
      img_gray := cvCreateImage(cvSize(image^.width, image^.height), IPL_DEPTH_8U, 1);
      dst := cvCreateImage(cvSize(image^.width, image^.height), IPL_DEPTH_8U, 1);
      cvCvtColor(image, img_gray, CV_BGR2GRAY);
      storage := cvCreateMemStorage(0);
      cvThreshold(img_gray, dst, 128, 255, CV_THRESH_BINARY_INV);
      // cvAdaptiveThreshold(img_gray, img_gray, 255, CV_ADAPTIVE_THRESH_GAUSSIAN_C, CV_THRESH_BINARY, 21, 7);
      contours := nil; // AllocMem(SizeOf(TCvSeq));
      cvClearMemStorage(storage);      
      cvFindContours(dst, storage, @contours, SizeOf(TCvContour), CV_RETR_EXTERNAL, // Только внешние контуры
        // CV_RETR_CCOMP, //Все контуры с разделением внешние и дырки
        CV_CHAIN_APPROX_SIMPLE, cvPoint(0, 0));

      color_ext := CV_RGB(100, 200, 0);
      color_hole := CV_RGB(200, 100, 0);

      // ---------------------------- Внешние контуры ----------------------------
      // lp_contour := contours;
      // cvInitTreeNodeIterator(
      // l_iterator,
      // contours,
      // 1);
      // lp_contour := cvNextTreeNode(@l_iterator);
      // while Assigned(lp_contour) do
      // begin
      // cvStartReadSeq(
      // lp_contour,
      // @l_reader,
      // 0);
      // // Первая точка
      // CV_READ_SEQ_ELEM(
      // @l_pt0,
      // l_reader,
      // SizeOf(l_pt0));
      // // l_pts1.push_back(l_pt0);
      // for i := 0 to lp_contour^.total - 1 do
      // begin
      // // Последующие точки
      // CV_READ_SEQ_ELEM(
      // @l_pt1,
      // l_reader,
      // SizeOf(l_pt0));
      // cvLine(
      // image,
      // l_pt0,
      // l_pt1,
      // CV_RGB(0, 255, 0),
      // 2);
      // l_pt0 := l_pt1;
      // // l_pts1.push_back(l_pt0);
      // end;
      // lp_contour := cvNextTreeNode(@l_iterator);
      // end;

      // ------------------------ Все контуры ----------------------------
      while Assigned(contours) do
      begin
        if CV_IS_SEQ_CLOSED(contours) then
          cvDrawContours(image, contours, color_ext, color_hole, -1, 2,
            // CV_FILLED, // replace CV_FILLED with 1 to see the outlines
            CV_AA, cvPoint(0, 0));
        contours := contours^.h_next;
      end;

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
