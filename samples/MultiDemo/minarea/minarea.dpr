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
// Original file:
// opencv\samples\c\example_cmake\minarea.c
// ***************************************************************

program minarea;

{$APPTYPE CONSOLE}
{$POINTERMATH ON}
{$R *.res}

uses
  System.SysUtils,
  ocv.highgui_c,
  ocv.core_c,
  ocv.core.types_c,
  ocv.imgproc_c,
  ocv.imgproc.types_c;

{$DEFINE ARRAY }

var
  img: pIplImage;
{$IFNDEF ARRAY}
  storage: pCvMemStorage;
{$ELSE}
  points: pCvPoint;
  pointMat: TCvMat;
{$ENDIF}
  key: Integer;
  i: Integer;
  count: Integer;
  pt0, pt: TCvPoint;
  box: TCvBox2D;
  box_vtx: TBoxPoints;
  center: TCvPoint2D32f;
  icenter: TCvPoint;
  radius: Single;
  ptseq: pCvSeq;

begin
  try
    WriteLn('This program demonstrates finding the minimum enclosing box or circle of a set');
    WriteLn('of points using functions: minAreaRect() minEnclosingCircle().');
    WriteLn('Random points are generated and then enclosed.');
    WriteLn('Call:');
    WriteLn('minarea');

    img := cvCreateImage(cvSize(500, 500), 8, 3);

{$IFNDEF ARRAY}
    storage := cvCreateMemStorage(0);
{$ENDIF}
    cvNamedWindow('rect & circle', 1);
    While True do
    begin
      count := random(100) + 1;
{$IFNDEF ARRAY}
      ptseq := cvCreateSeq(CV_SEQ_KIND_GENERIC or CV_32SC2, sizeof(TCvContour), sizeof(TCvPoint), storage);
      for i := 0 to count - 1 do
      begin
        pt0.x := random(img^.width div 2) + img^.width div 4;
        pt0.y := random(img^.height div 2) + img^.height div 4;
        cvSeqPush(ptseq, @pt0);
      end;
{$IFNDEF _EiC} // * unfortunately, here EiC crashes */
      box := cvMinAreaRect2(ptseq);
{$ENDIF}
      cvMinEnclosingCircle(ptseq, @center, @radius);
{$ELSE}
      points := Allocmem(count * sizeof(TCvPoint));
      pointMat := cvMat(1, count, CV_32SC2, points);
      for i := 0 to count - 1 do
      begin
        pt0.x := random(img^.width div 2) + img^.width div 4;
        pt0.y := random(img^.height div 2) + img^.height div 4;
        points[i] := pt0;
      end;
{$IFNDEF _EiC}
      box := cvMinAreaRect2(@pointMat, 0);
{$ENDIF}
      cvMinEnclosingCircle(@pointMat, @center, @radius);
{$ENDIF}
      cvBoxPoints(box, box_vtx);
      cvZero(img);
      for i := 0 to count - 1 do
      begin
{$IFNDEF ARRAY}
        pt0 := pCvPoint(CV_GET_SEQ_ELEM(sizeof(TCvPoint), ptseq, i))^;
{$ELSE}
        pt0 := points[i];
{$ENDIF}
        cvCircle(img, pt0, 2, CV_RGB(255, 0, 0), CV_FILLED, CV_AA, 0);
      end;
{$IFNDEF _EiC}
      pt0.x := cvRound(box_vtx[3].x);
      pt0.y := cvRound(box_vtx[3].y);
      for i := 0 to 3 do
      begin
        pt.x := cvRound(box_vtx[i].x);
        pt.y := cvRound(box_vtx[i].y);
        cvLine(img, pt0, pt, CV_RGB(0, 255, 0), 1, CV_AA, 0);
        pt0 := pt;
      end;
{$ENDIF}
      icenter.x := cvRound(center.x);
      icenter.y := cvRound(center.y);
      cvCircle(img, icenter, cvRound(radius), CV_RGB(255, 255, 0), 1, CV_AA, 0);

      cvShowImage('rect & circle', img);

      if cvWaitKey(0) = 27 then
        Break;
{$IFNDEF ARRAY}
      cvClearMemStorage(storage);
{$ELSE}
      freemem(points);
{$ENDIF}
    end;
    cvDestroyWindow('rect & circle');
  except
    on E: Exception do
      WriteLn(E.ClassName, ': ', E.Message);
  end;

end.
