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
// ****************************************************************
// Original file:
// opencv\samples\c\contours.c
// ****************************************************************

program cv_FindContours2;

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

procedure help;
begin
  Writeln('This program creates an image to demonstrate the use of the ''c'' contour');
  Writeln('functions: cvFindContours() and cvApproxPoly() along with the storage');
  Writeln('functions cvCreateMemStorage() and cvDrawContours().');
  Writeln('It also shows the use of a trackbar to control contour retrieval.');
  Writeln;
  Writeln('Usage :');
  Writeln('contours');
end;

const
  w = 500;

Var
  levels: Integer = 3;
  contours: pCvSeq = nil;

procedure on_trackbar(pos: Integer); cdecl;
Var
  cnt_img: pIplImage;
  _contours: pCvSeq;
  _levels: Integer;
begin
  cnt_img := cvCreateImage(cvSize(w, w), 8, 3);
  _contours := contours;
  _levels := levels - 3;
  // (void)pos;

  if (_levels <= 0) then // get to the nearest face to make it look more funny
    _contours := _contours^.h_next^.h_next^.h_next;
  cvZero(cnt_img);
  cvDrawContours(cnt_img, _contours, CV_RGB(255, 0, 0), CV_RGB(0, 255, 0), _levels, 3, CV_AA, cvPoint(0, 0));
  cvShowImage('contours', cnt_img);
  cvReleaseImage(cnt_img);
end;

procedure findCComp(img: pIplImage);
var
  x, y, cidx: Integer;
  mask: pIplImage;
begin
  cidx := 1;
  mask := cvCreateImage(cvSize(img^.width + 2, img^.height + 2), 8, 1);
  cvZero(mask);
  cvRectangle(mask, cvPoint(0, 0), cvPoint(mask^.width - 1, mask^.height - 1), cvScalarAll(1), 1, 8, 0);

  for y := 0 to img^.height - 1 do
    for x := 0 to img^.width - 1 do
    begin
      if uchar(CV_IMAGE_ELEM(mask, SizeOf(uchar), y + 1, x + 1)^) <> 0 then
        continue;
      cvFloodFill(img, cvPoint(x, y), cvScalarAll(cidx), cvScalarAll(0), cvScalarAll(0), nil, 4, mask);
      Inc(cidx);
    end;
end;

Var
  i, j: Integer;
  storage: pCvMemStorage;
  img: pIplImage;
  img32f: pIplImage;
  img32s: pIplImage;
  img3: pIplImage;

  dx, dy: Integer;
  white: TCvScalar;
  black: TCvScalar;
  angle: double;
  attrs: array [0 .. 2] of pCVChar;
  rng: TCvRNG;
  tcontours: pCvSeq;
  color: TCvScalar;

begin
  try
    storage := cvCreateMemStorage(0);
    img := cvCreateImage(cvSize(w, w), 8, 1);
    img32f := cvCreateImage(cvSize(w, w), IPL_DEPTH_32F, 1);
    img32s := cvCreateImage(cvSize(w, w), IPL_DEPTH_32S, 1);
    img3 := cvCreateImage(cvSize(w, w), 8, 3);
    help;

    cvZero(img);

    for i := 0 to 5 do
    begin
      dx := (i mod 2) * 250 - 30;
      dy := (i div 2) * 150;
      white := cvRealScalar(255);
      black := cvRealScalar(0);

      if (i = 0) then
      begin
        for j := 0 to 9 do
        begin
          angle := (j + 5) * CV_PI / 21;
          cvLine(img, cvPoint(cvRound(dx + 100 + j * 10 - 80 * cos(angle)), cvRound(dy + 100 - 90 * sin(angle))),
            cvPoint(cvRound(dx + 100 + j * 10 - 30 * cos(angle)), cvRound(dy + 100 - 30 * sin(angle))), white, 3, 8, 0);
        end;
      end;

      cvEllipse(img, cvPoint(dx + 150, dy + 100), cvSize(100, 70), 0, 0, 360, white, -1, 8, 0);
      cvEllipse(img, cvPoint(dx + 115, dy + 70), cvSize(30, 20), 0, 0, 360, black, -1, 8, 0);
      cvEllipse(img, cvPoint(dx + 185, dy + 70), cvSize(30, 20), 0, 0, 360, black, -1, 8, 0);
      cvEllipse(img, cvPoint(dx + 115, dy + 70), cvSize(15, 15), 0, 0, 360, white, -1, 8, 0);
      cvEllipse(img, cvPoint(dx + 185, dy + 70), cvSize(15, 15), 0, 0, 360, white, -1, 8, 0);
      cvEllipse(img, cvPoint(dx + 115, dy + 70), cvSize(5, 5), 0, 0, 360, black, -1, 8, 0);
      cvEllipse(img, cvPoint(dx + 185, dy + 70), cvSize(5, 5), 0, 0, 360, black, -1, 8, 0);
      cvEllipse(img, cvPoint(dx + 150, dy + 100), cvSize(10, 5), 0, 0, 360, black, -1, 8, 0);
      cvEllipse(img, cvPoint(dx + 150, dy + 150), cvSize(40, 10), 0, 0, 360, black, -1, 8, 0);
      cvEllipse(img, cvPoint(dx + 27, dy + 100), cvSize(20, 35), 0, 0, 360, white, -1, 8, 0);
      cvEllipse(img, cvPoint(dx + 273, dy + 100), cvSize(20, 35), 0, 0, 360, white, -1, 8, 0);
    end;

    cvNamedWindow('image', 1);
    cvShowImage('image', img);
    cvConvert(img, img32f);
    findCComp(img32f);
    cvConvert(img32f, img32s);

    // cvFindContours(img32s, storage, @contours, SizeOf(TCvContour), CV_RETR_CCOMP, CV_CHAIN_APPROX_SIMPLE, cvPoint(0, 0));
    cvClearMemStorage(storage);    
    cvFindContours(img, storage, @contours, SizeOf(TCvContour), CV_RETR_TREE, CV_CHAIN_APPROX_SIMPLE, cvPoint(0, 0));

    attrs[0] := 'recursive';
    attrs[1] := '1';
    cvSave(PAnsiChar(cResourceResult+'contours.xml'), contours, nil, nil, cvAttrList(@attrs));
    contours := pCvSeq(cvLoad(PAnsiChar(cResourceResult+'contours.xml'), storage));

    // comment this out if you do not want approximation
    contours := cvApproxPoly(contours, SizeOf(TCvContour), storage, CV_POLY_APPROX_DP, 3, 1);

    cvNamedWindow('contours', 1);
    cvCreateTrackbar('levels+3', 'contours', @levels, 7, on_trackbar);

    rng := CvRNG(-1);
    tcontours := contours;

    cvCvtColor(img, img3, CV_GRAY2BGR);

    while Assigned(tcontours^.h_next) do
      tcontours := tcontours^.h_next;

    while not Assigned(tcontours) do
    begin
      color.val[0] := cvRandInt(rng) mod 256;
      color.val[1] := cvRandInt(rng) mod 256;
      color.val[2] := cvRandInt(rng) mod 256;
      color.val[3] := cvRandInt(rng) mod 256;
      cvDrawContours(img3, tcontours, color, color, 0, -1, 8, cvPoint(0, 0));
      if Assigned(tcontours^.v_next) then
      begin
        color.val[0] := cvRandInt(rng) mod 256;
        color.val[1] := cvRandInt(rng) mod 256;
        color.val[2] := cvRandInt(rng) mod 256;
        color.val[3] := cvRandInt(rng) mod 256;
        cvDrawContours(img3, tcontours^.v_next, color, color, 1, -1, 8, cvPoint(0, 0));
      end;
      tcontours := tcontours^.h_prev;
    end;

    cvShowImage('colored', img3);
    on_trackbar(0);

    cvWaitKey(0);
    cvReleaseMemStorage(storage);
    cvReleaseImage(img);
    cvReleaseImage(img32f);
    cvReleaseImage(img32s);
    cvReleaseImage(img3);
    cvDestroyAllWindows;

  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;

end.
