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

program Squares;

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
  filename = cResourceMedia + 'pic2.bmp';
  // pic1.bmp...pic6.bmp
  wndname = 'Squares';

function angle(pt1, pt2, pt0: PCvPoint): double;
var
  dx1, dy1, dx2, dy2: double;
begin
  dx1 := pt1^.x - pt0^.x;
  dy1 := pt1^.y - pt0^.y;
  dx2 := pt2^.x - pt0^.x;
  dy2 := pt2^.y - pt0^.y;
  result := (dx1 * dx2 + dy1 * dy2) / sqrt((dx1 * dx1 + dy1 * dy1) * (dx2 * dx2 + dy2 * dy2) + 1E-10);
end;

function findSquares4(img: PIplImage; storage: PCvMemStorage): PCvSeq;
var
  thresh: integer;
  CC: TCvSeq;
  contours: PCvSeq;
  PP: PCvSeq;
  i, c, l, N: integer;
  sz: TCvSize;
  timg: PIplImage;
  gray: PIplImage;
  pyr: PIplImage;
  tgray: PIplImage;
  s, t: double;
  Squares: PCvSeq;
  result_: PCvSeq;
  rr: integer;

  contourstorage: PCvMemStorage;

  yy: pointer;
  a: AnsiString;
begin
  contours := @CC;
  PP := @contours;

  N := 11;
  thresh := 50;
  // cvSaveImage('ee1.bmp', PCvArr(img));

  sz := cvSize((img^.width AND -2), (img^.height AND -2));
  timg := cvCloneImage(img); // make a copy of input image
  gray := cvCreateImage(sz, 8, 1);
  pyr := cvCreateImage(cvSize(sz.width div 2, sz.height div 2), timg^.depth, timg^.nChannels);

  // create empty sequence that will contain points -
  // 4 points per square (the square's vertices)
  Squares := cvCreateSeq(0, sizeof(TCvSeq), sizeof(TCvPoint), storage);

  // select the maximum ROI in the image
  // with the width and height divisible by 2
  // cvSetImageROI(timg, cvRect(0, 0, sz.width, sz.height));
  cvResetImageROI(timg);

  contourstorage := cvCreateMemStorage(0);

  // down-scale and upscale the image to filter out the noise
  cvPyrDown(timg, pyr, 7);
  cvPyrUp(pyr, timg, 7);
  tgray := cvCreateImage(sz, 8, 1);

  // find squares in every color plane of the image
  for c := 0 to timg^.nChannels-1 do
  begin
    // extract the c-th color plane
    cvSetImageCOI(timg, c + 1);
    cvCopy(timg, tgray);
    // cvSaveImage('ee11.bmp', PCvArr(tgray));

    for l := 0 to N - 1 do
    begin
      // hack: use Canny instead of zero threshold level.
      // Canny helps to catch squares with gradient shading
      if (l = 0) then
      begin
        // apply Canny. Take the upper threshold from slider
        // and set the lower to 0 (which forces edges merging)
        cvCanny(tgray, gray, 0, thresh, 5);
        // dilate canny output to remove potential
        // holes between edge segments
        cvDilate(gray, gray);
        // a := inttostr(l) + 'ee1.bmp';
        // cvSaveImage(pCVChar(@a[1]), PCvArr(gray));
      end
      else
      begin
        // apply threshold if l!=0:
        // tgray(x,y) = gray(x,y) < (l+1)*255/N ? 255 : 0
        cvThreshold(tgray, gray, (l + 1) * 255 / N, 255, CV_THRESH_BINARY);
        // a := inttostr(l) + 'ee1.bmp';
        // cvSaveImage(pCVChar(@a[1]), PCvArr(gray));
      end;

      // try
      // find contours and store them all as a list
      cvClearMemStorage(contourstorage);
      rr := cvFindContours(gray, contourstorage, @contours, sizeof(TCvContour), CV_RETR_LIST, CV_CHAIN_APPROX_SIMPLE,
        CvPoint(0, 0));

      // test each contour
      while contours <> nil do
      begin
        // approximate contour with accuracy proportional
        // to the contour perimeter
        result_ := cvApproxPoly(contours, sizeof(TCvContour), storage, CV_POLY_APPROX_DP,
          cvContourPerimeter(contours) * 0.02, 0);
        // square contours should have 4 vertices after approximation
        // relatively large area (to filter out noisy contours)
        // and be convex.
        // Note: absolute value of an area is used because
        // area may be positive or negative - in accordance with the
        // contour orientation
        if (result_^.total = 4) AND (abs(cvContourArea(result_, CV_WHOLE_SEQ, 0)) > 1000) AND
          (cvCheckContourConvexity(result_) > 0) then
        begin
          s := 0;
          //
          for i := 0 to 5 - 1 do
          begin
            // find minimum angle between joint
            // edges (maximum of cosine)
            if (i >= 2) then
            begin
              t := abs(angle(PCvPoint(cvGetSeqElem(result_, i)), PCvPoint(cvGetSeqElem(result_, i - 2)),
                PCvPoint(cvGetSeqElem(result_, i - 1))));
              if s <= t then
                s := t; // s = s > t ? s : t;
            end;
          end;
          //
          // // if cosines of all angles are small
          // // (all angles are ~90 degree) then write quandrange
          // // vertices to resultant sequence
          if (s < 0.3) then
            for i := 0 to 3 do
              cvSeqPush(Squares, cvGetSeqElem(result_, i));
        end;

        // take the next contour
        contours := contours^.h_next;
      end;

    end;
  end;

  // release all the temporary images
  cvReleaseImage(gray);
  cvReleaseImage(pyr);
  cvReleaseImage(tgray);
  cvReleaseImage(timg);
  cvReleaseMemStorage(contourstorage);
  result := Squares;
end;

// the function draws all the squares in the image
procedure drawSquares(img: PIplImage; Squares: PCvSeq);
var
  reader: TCvSeqReader;
  cpy: PIplImage;
  i: integer;
  count: integer;
  pt: array [0 .. 3] of TCvPoint;
  rect: PCvPoint;
begin
  rect := @pt;
  cpy := cvCloneImage(img);

  // initialize reader of the sequence
  cvStartReadSeq(Squares, @reader, 0);

  // read 4 sequence elements at a time (all vertices of a square)
  for i := 0 to Squares^.total - 1 do // ; i += 4
  begin
    // CvPoint pt[4], *rect = pt;
    count := 4;
    // read 4 vertices
    CV_READ_SEQ_ELEM(@pt[0], reader, sizeof(TCvPoint));
    CV_READ_SEQ_ELEM(@pt[1], reader, sizeof(TCvPoint));
    CV_READ_SEQ_ELEM(@pt[2], reader, sizeof(TCvPoint));
    CV_READ_SEQ_ELEM(@pt[3], reader, sizeof(TCvPoint));

    // draw the square as a closed polyline
    cvPolyLine(cpy, @rect, @count, 1, 1, CV_RGB(255, 0, 0), 3, CV_AA, 0);

    // cvSaveImage('ee2.bmp', PCvArr(cpy));
  end;

  // show the resultant image
  cvShowImage(wndname, cpy);
  cvReleaseImage(cpy);
end;

var
  storage: PCvMemStorage;
  img, img0: PIplImage;

begin
  try

    storage := cvCreateMemStorage(0);
    img0 := cvLoadImage(filename);
    img := cvCloneImage(img0);
    cvNamedWindow(wndname, 1);
    // find and draw the squares
    drawSquares(img, findSquares4(img, storage));
    cvWaitKey;
    cvReleaseImage(img);
    cvReleaseImage(img0);
    cvClearMemStorage(storage);

  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;

end.
