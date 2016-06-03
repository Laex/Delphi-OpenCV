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

program cv_DistanceTransform;

{$APPTYPE CONSOLE}
{$POINTERMATH ON}
{$R *.res}

uses
  System.SysUtils,
  ocv.highgui_c,
  ocv.core_c,
  ocv.core.types_c,
  ocv.imgproc_c,
  ocv.imgproc.types_c,
  ocv.utils,
  uResourcePaths;

const
  filename = cResourceMedia + 'stuff.jpg';
  wndname = 'Distance transform';
  tbarname = 'Threshold';

Var
  build_voronoi: Integer = 0;
  edge_thresh: Integer = 100;
  mask_size: Integer = CV_DIST_MASK_5;
  // The output and temporary images
  dist: pIplImage = nil;
  dist8u1: pIplImage = nil;
  dist8u2: pIplImage = nil;
  dist8u: pIplImage = nil;
  dist32s: pIplImage = nil;

  gray: pIplImage = nil;
  edge: pIplImage = nil;
  labels: pIplImage = nil;

const
  colors: array [0 .. 8, 0 .. 2] of byte = ((0, 0, 0), (255, 0, 0),
    (255, 128, 0), (255, 255, 0), (0, 255, 0), (0, 128, 255), (0, 255, 255),
    (0, 0, 255), (255, 0, 255));

  // threshold trackbar callback
procedure on_trackbar(dummy: Integer); cdecl;
Var
  msize, i, j: Integer;
  ll: pInteger;
  dd: pFloat;
  d: PByte;
  idx: Integer;
  b, g, r: Integer;
begin
  msize := mask_size;

  cvThreshold(gray, edge, edge_thresh, edge_thresh, CV_THRESH_BINARY);

  if (build_voronoi <> 0) then
  begin
    msize := CV_DIST_MASK_5;
    cvDistTransform(edge, dist, CV_DIST_L2, msize, nil, labels);
  end
  else
    cvDistTransform(edge, dist, CV_DIST_L2, msize, nil, nil);

  if (build_voronoi = 0) then
  begin
    // begin "painting" the distance transform result
    cvConvertScale(dist, dist, 5000.0, 0);
    cvPow(dist, dist, 0.5);

    cvConvertScale(dist, dist32s, 1.0, 0.5);
    cvAndS(dist32s, cvScalarAll(255), dist32s, 0);
    cvConvertScale(dist32s, dist8u1, 1, 0);
    cvConvertScale(dist32s, dist32s, -1, 0);
    cvAddS(dist32s, cvScalarAll(255), dist32s, 0);
    cvConvertScale(dist32s, dist8u2, 1, 0);
    cvMerge(dist8u1, dist8u2, dist8u2, 0, dist8u);
    // end "painting" the distance transform result
  end
  else
  begin
    for i := 0 to labels^.height - 1 do
    begin
      ll := pInteger(labels^.imageData + i * labels^.widthStep);
      dd := pFloat(dist^.imageData + i * dist^.widthStep);
      d := PByte(dist8u^.imageData + i * dist8u^.widthStep);
      for j := 0 to labels^.width - 1 do
      begin
        if (ll[j] = 0) or (dd[j] = 0) then
          idx := 0
        else
          idx := ((ll[j] - 1) mod 8) + 1;
        b := cvRound(colors[idx][0]);
        g := cvRound(colors[idx][1]);
        r := cvRound(colors[idx][2]);
        d[j * 3] := b;
        d[j * 3 + 1] := g;
        d[j * 3 + 2] := r;
      end;
    end;
  end;

  cvShowImage(wndname, dist8u);
end;

Var
  c: Integer;

begin
  try

    if ParamCount > 0 then
      gray := cvLoadImage(c_str(ParamStr(1)), 0)
    else
      gray := cvLoadImage(filename, 0);

    if not Assigned(gray) then
      Halt(1);

    WriteLn('Hot keys: '#13#10 + #9'ESC - quit the program'#13#10 +
      #9'3 - use 3x3 mask'#13#10 + #9'5 - use 5x5 mask'#13#10 +
      #9'0 - use precise distance transform'#13#10 +
      #9'v - switch Voronoi diagram mode on/off'#13#10 +
      #9'ENTER - loop through all the modes');

    dist := cvCreateImage(cvGetSize(gray), IPL_DEPTH_32F, 1);
    dist8u1 := cvCloneImage(gray);
    dist8u2 := cvCloneImage(gray);
    dist8u := cvCreateImage(cvGetSize(gray), IPL_DEPTH_8U, 3);
    dist32s := cvCreateImage(cvGetSize(gray), IPL_DEPTH_32S, 1);
    edge := cvCloneImage(gray);
    labels := cvCreateImage(cvGetSize(gray), IPL_DEPTH_32S, 1);

    cvNamedWindow(wndname, 1);

    cvCreateTrackbar(tbarname, wndname, @edge_thresh, 255, on_trackbar);

    while True do
    begin
      // Call to update the view
      on_trackbar(0);

      c := cvWaitKey(0);

      if c = 27 then
        break;

      if Chr(c) = '3' then
        mask_size := CV_DIST_MASK_3
      else if Chr(c) = '5' then
        mask_size := CV_DIST_MASK_5
      else if Chr(c) = '0' then
        mask_size := CV_DIST_MASK_PRECISE
      else if Chr(c) = 'v' then
        build_voronoi := 1
      else if c = 13 then
      begin
        if build_voronoi <> 0 then
        begin
          build_voronoi := 0;
          mask_size := CV_DIST_MASK_3;
        end
        else if (mask_size = CV_DIST_MASK_3) then
          mask_size := CV_DIST_MASK_5
        else if (mask_size = CV_DIST_MASK_5) then
          mask_size := CV_DIST_MASK_PRECISE
        else if (mask_size = CV_DIST_MASK_PRECISE) then
          build_voronoi := 1;
      end;
    end;

    cvReleaseImage(gray);
    cvReleaseImage(edge);
    cvReleaseImage(dist);
    cvReleaseImage(dist8u);
    cvReleaseImage(dist8u1);
    cvReleaseImage(dist8u2);
    cvReleaseImage(dist32s);
    cvReleaseImage(labels);

    cvDestroyWindow(wndname);

  except
    on E: Exception do
      WriteLn(E.ClassName, ': ', E.Message);
  end;

end.
