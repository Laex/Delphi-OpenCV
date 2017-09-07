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
program Transparency1;

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

const
  px = 200;
  py = 100;

var
  overlay: pIplImage = nil;
  underlay: pIplImage = nil;
  rgba: array [0 .. 3] of pIplImage;
  i, j, k: Integer;
  opacity: Double;
  overlayPx, srcPx: Byte;

begin
  try
    overlay := cvLoadImage(batmanimg, CV_LOAD_IMAGE_UNCHANGED);
    underlay := cvLoadImage(parrotimg, CV_LOAD_IMAGE_UNCHANGED);

    cvNamedWindow('overlay_source', CV_WINDOW_AUTOSIZE);
    cvShowImage('overlay_source', overlay);
    cvNamedWindow('underlay_source', CV_WINDOW_AUTOSIZE);
    cvShowImage('underlay_source', underlay);

    for i := 0 to 3 do
      rgba[i] := cvCreateImage(cvGetSize(overlay), IPL_DEPTH_8U, 1);
    cvSplit(overlay, rgba[2], rgba[1], rgba[0], rgba[3]);

    cvNamedWindow('alpha_overlay', CV_WINDOW_AUTOSIZE);
    cvShowImage('alpha_overlay', rgba[3]);

    for i := 0 to overlay^.width - 1 do
      for j := 0 to overlay^.height - 1 do
        for k := 0 to overlay^.nChannels - 1 do
        begin
          opacity := (overlay^.imageData[j * overlay^.widthStep + i * overlay^.nChannels + 3]) / 255;
          overlayPx := overlay^.imageData[j * overlay^.widthStep + i * overlay^.nChannels + k];
          srcPx := underlay^.imageData[(py + j) * underlay^.widthStep + (i + px) * overlay^.nChannels + k];
          underlay^.imageData[(py + j) * underlay^.widthStep + (i + px) * overlay^.nChannels + k] :=
            Trunc(srcPx * (1 - opacity) + overlayPx * opacity);
        end;

    cvNamedWindow('underlay', CV_WINDOW_AUTOSIZE);
    cvShowImage('underlay', underlay);

    cvWaitKey(0);

    cvReleaseImage(overlay);
    cvReleaseImage(underlay);
    for i := 0 to 3 do
      cvReleaseImage(rgba[i]);

    cvDestroyAllWindows;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;

end.
