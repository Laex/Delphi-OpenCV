(* *****************************************************************
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
program Transparency;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  System.SysUtils,
  System.Math,
  ocv.highgui_c,
  ocv.core_c,
  ocv.core.types_c,
  ocv.imgproc_c,
  ocv.imgproc.types_c,
  uResourcePaths;

const
  batmanimg = cResourceMedia + 'batman.png';
  parrotimg = cResourceMedia + 'parrot.png';

procedure overlayImage(const src, overlay: pIplImage; const px, py: Integer);
Var
  c, y, x, fX, fY: Integer;
  opacity: Double;
  overlayPx, srcPx: Byte;
begin
  for y := max(py, 0) to src^.height - 1 do
  begin
    fY := y - py;

    if (fY >= overlay^.height) then
      continue;

    for x := max(px, 0) to src^.width do
    begin
      fX := x - px;

      if (fX >= overlay^.width) then
        continue;

      opacity := (overlay^.imageData[fY * overlay^.widthStep + fX * overlay^.nChannels + 3]) / 255;
      c:=0;
      while (opacity > 0) and (c < src^.nChannels) do
      begin
        overlayPx := overlay^.imageData[fY * overlay^.widthStep + fX * overlay^.nChannels + c];
        srcPx := src^.imageData[y * src^.widthStep + x * src^.nChannels + c];
        src^.imageData[y * src^.widthStep + src^.nChannels * x + c] := Trunc(srcPx * (1 - opacity) + overlayPx * opacity);
        inc(c);
      end;
    end;
  end;
end;

var
  overlay: pIplImage = nil;
  underlay: pIplImage = nil;
  rgba: array [0 .. 2] of pIplImage;
  i: Integer;
  alpha_underlay, alpha_overlay, alpha_underlay_result: pIplImage;

begin
  try
    overlay := cvLoadImage(batmanimg, CV_LOAD_IMAGE_UNCHANGED);
    underlay := cvLoadImage(parrotimg, CV_LOAD_IMAGE_UNCHANGED);

    for i := 0 to 2 do
      rgba[i] := cvCreateImage(cvGetSize(underlay), IPL_DEPTH_8U, 1);
    alpha_underlay := cvCreateImage(cvGetSize(underlay), IPL_DEPTH_8U, 1);
    cvSplit(underlay, rgba[2], rgba[1], rgba[0], alpha_underlay);

    cvNamedWindow('alpha_underlay', CV_WINDOW_AUTOSIZE);
    cvShowImage('alpha_underlay', alpha_underlay);

    for i := 0 to 2 do
    begin
      cvReleaseImage(rgba[i]);
      rgba[i] := cvCreateImage(cvGetSize(overlay), IPL_DEPTH_8U, 1);
    end;
    alpha_overlay := cvCreateImage(cvGetSize(overlay), IPL_DEPTH_8U, 1);
    cvSplit(overlay, rgba[2], rgba[1], rgba[0], alpha_overlay);

    cvNamedWindow('alpha_overlay', CV_WINDOW_AUTOSIZE);
    cvShowImage('alpha_overlay', alpha_overlay);

    overlayImage(underlay, overlay, 100, 100);

    for i := 0 to 2 do
    begin
      cvReleaseImage(rgba[i]);
      rgba[i] := cvCreateImage(cvGetSize(underlay), IPL_DEPTH_8U, 1);
    end;
    alpha_underlay_result := cvCreateImage(cvGetSize(underlay), IPL_DEPTH_8U, 1);
    cvSplit(underlay, rgba[2], rgba[1], rgba[0], alpha_underlay_result);

    cvNamedWindow('alpha_underlay_result', CV_WINDOW_AUTOSIZE);
    cvShowImage('alpha_underlay_result', alpha_underlay_result);

    cvNamedWindow('underlay', CV_WINDOW_AUTOSIZE);
    cvShowImage('underlay', underlay);

    cvWaitKey(0);

    cvReleaseImage(alpha_underlay);
    cvReleaseImage(alpha_overlay);
    cvReleaseImage(alpha_underlay_result);
    cvReleaseImage(overlay);
    cvReleaseImage(underlay);
    for i := 0 to 2 do
      cvReleaseImage(rgba[i]);

    cvDestroyAllWindows;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;

end.
