// *****************************************************************
// Delphi-OpenCV Demo
// Copyright (C) 2013 Project Delphi-OpenCV
// ****************************************************************
// Contributor:
// laentir Valetov
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
// Source: http://answers.opencv.org/question/20484/get-the-color-percentage/
//
// Get the color percentage of RGB(ex red 50%, green 25% and blue %25) in an image
//
// *******************************************************************
program cv_Sum;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  System.SysUtils,
  highgui_c,
  core_c,
  Core.types_c,
  imgproc_c,
  imgproc.types_c;

const
  // имя картинки
  filename = 'Resource\cat2.jpg';

var
  img     : pIplImage;
  channels: array [0 .. 2] of pIplImage;
  BGRSum  : Array [0 .. 2] of TCvScalar;
  i, total: Integer;

begin
  try

    img := cvLoadImage(
      filename,
      1);

    for i         := 0 to 2 do
      channels[i] := cvCreateImage(
        cvGetSize(img),
        8,
        1);

    cvSplit(
      img,
      channels[0],
      channels[1],
      channels[2],
      0);

    for i       := 0 to 2 do
      BGRSum[i] := cvSum(channels[i]);

    total := img^.width * img^.height * 255;

    WriteLn('Color percentage of RGB(ex red 50%, green 25% and blue %25) in an image is');

    writeln(
      'red:   ',
      BGRSum[2].val[0] / total * 100:2:2);
    writeln(
      'green: ',
      BGRSum[1].val[0] / total * 100:2:2);
    writeln(
      'blue:  ',
      BGRSum[0].val[0] / total * 100:2:2);

    readln;
  except
    on E: Exception do
      writeln(
        E.ClassName,
        ': ',
        E.Message);
  end;

end.
