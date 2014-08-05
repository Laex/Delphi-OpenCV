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
// opencv\samples\c\morphology.c
// ***************************************************************

program cv_CreateStructuringElementEx;

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

var
  src: pIplImage = nil;
  dst: pIplImage = 0;

  element: pIplConvKernel = nil;
  element_shape: Integer = CV_SHAPE_RECT;

  // the address of variable which receives trackbar position update
  max_iters: Integer = 10;
  open_close_pos: Integer = 0;
  erode_dilate_pos: Integer = 0;

  // callback function for open/close trackbar
procedure OpenClose(pos: Integer); cdecl;
var
  n, an: Integer;
begin
  n := open_close_pos - max_iters;
  an := iif(n > 0, n, -n);
  element := cvCreateStructuringElementEx(an * 2 + 1, an * 2 + 1, an, an, element_shape, 0);
  if (n < 0) then
  begin
    cvErode(src, dst, element, 1);
    cvDilate(dst, dst, element, 1);
  end
  else
  begin
    cvDilate(src, dst, element, 1);
    cvErode(dst, dst, element, 1);
  end;
  cvReleaseStructuringElement(element);
  cvShowImage('Open/Close', dst);
end;

// callback function for erode/dilate trackbar
procedure ErodeDilate(pos: Integer); cdecl;
var
  n, an: Integer;
begin
  n := erode_dilate_pos - max_iters;
  an := iif(n > 0, n, -n);

  element := cvCreateStructuringElementEx(an * 2 + 1, an * 2 + 1, an, an, element_shape, 0);
  if (n < 0) then
  begin
    cvErode(src, dst, element, 1);
  end
  else
  begin
    cvDilate(src, dst, element, 1);
  end;
  cvReleaseStructuringElement(&element);
  cvShowImage('Erode/Dilate', dst);
end;

procedure help;
begin
  Writeln('This program demonstrated the use of the morphology operator, especially open, close, erode, dilate operations');
  Writeln('Morphology operators are built on max (close) and min (open) operators as measured by pixels covered by small structuring elements.');
  Writeln('These operators are very efficient.');
  Writeln('This program also allows you to play with elliptical, rectangluar and cross structure elements');
  Writeln('Usage: ');
  Writeln('morphologyc [image_name -- Default baboon.jpg]');
  Writeln('Hot keys: ');
  Writeln(#9'ESC - quit the program');
  Writeln(#9'r - use rectangle structuring element');
  Writeln(#9'e - use elliptic structuring element');
  Writeln(#9'c - use cross-shaped structuring element');
  Writeln(#9'SPACE - loop through all the options');
end;

var
  filename: AnsiString;
  c: Integer;

begin
  try
    help();

    filename := iif(ParamCount = 1, ParamStr(1), cResourceMedia + 'baboon.jpg');
    src := cvLoadImage(pCvChar(@filename[1]), 1);
    if not Assigned(src) then
    begin
      Writeln('Cannot load file image %s\n', filename);
      help();
      Exit;
    end;

    dst := cvCloneImage(src);

    // create windows for output images
    cvNamedWindow('Open/Close', 1);
    cvNamedWindow('Erode/Dilate', 1);

    open_close_pos := max_iters;
    erode_dilate_pos := max_iters;
    cvCreateTrackbar('iterations', 'Open/Close', @open_close_pos, max_iters * 2 + 1, OpenClose);
    cvCreateTrackbar('iterations', 'Erode/Dilate', @erode_dilate_pos, max_iters * 2 + 1, ErodeDilate);

    while True do
    begin

      OpenClose(open_close_pos);
      ErodeDilate(erode_dilate_pos);
      c := cvWaitKey(0);

      if (c = 27) then
        break;
      if (c = Ord('e')) then
        element_shape := CV_SHAPE_ELLIPSE
      else if (c = Ord('r')) then
        element_shape := CV_SHAPE_RECT
      else if (c = Ord('c')) then
        element_shape := CV_SHAPE_CROSS
      else if (c = Ord(' ')) then
        element_shape := (element_shape + 1) mod 3;
    end;

    // release images
    cvReleaseImage(src);
    cvReleaseImage(dst);

    // destroy windows
    cvDestroyWindow('Open/Close');
    cvDestroyWindow('Erode/Dilate');
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;

end.
