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
// opencv\samples\c\pyramid_segmentation.c
// ***************************************************************

program cv_PyrSegmentation;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  System.SysUtils,
  ocv.highgui_c,
  ocv.core_c,
  ocv.core.types_c,
  ocv.imgproc_c,
  ocv.imgproc.types_c,
  ocv.legacy,
  uResourcePaths;

procedure help;
begin
  WriteLn('This program demonstrated color pyramid segmentation cvcvPyrSegmentation() which is controlled');
  WriteLn('by two trhesholds which can be manipulated by a trackbar. It can take an image file name or defaults to ''fruits.jpg''');
  WriteLn('Usage :');
  WriteLn('pyaramid_segmentation [image_path_filename -- Defaults to fruits.jpg]');
end;

Var
  image: array [0 .. 1] of pIplImage = (
    nil,
    nil
  );
  image0: pIplImage;
  image1: pIplImage;
  size: TCvSize;

  w0, h0, i, threshold1, threshold2, l: Integer;
  level: Integer = 4;
  sthreshold1, sthreshold2, l_comp: Integer;
  block_size: Integer = 1000;
  parameter: single;
  threshold, rezult, min_rezult: double;
  filter: Integer = CV_GAUSSIAN_5x5;
  cur_comp, min_comp: pCvConnectedComp;
  comp: pCvSeq;
  storage: pCvMemStorage;
  pt1, pt2: TCvPoint;

procedure ON_SEGMENT(a: Integer); cdecl;
begin
  cvPyrSegmentation(image0, image1, storage, comp, level, threshold1 + 1, threshold2 + 1);
  cvShowImage('Segmentation', image1);
end;

Var
  filename: AnsiString;

begin
  try
    help;
    filename := iif(ParamCount = 1, ParamStr(1), cResourceMedia + 'fruits.jpg');
    image[0] := cvLoadImage(pcvChar(@filename[1]), 1);
    if not Assigned(image[0]) then
    begin
      WriteLn(Format('Cannot load fileimage - %s', [filename]));
      Halt;
    end;

    cvNamedWindow('Source', 0);
    cvShowImage('Source', image[0]);

    cvNamedWindow('Segmentation', 0);

    storage := cvCreateMemStorage(block_size);

    image[0]^.width := image[0]^.width and (-(1 shl level));
    image[0]^.height := image[0]^.height and (-(1 shl level));

    image0 := cvCloneImage(image[0]);
    image1 := cvCloneImage(image[0]);
    // segmentation of the color image
    l := 1;
    threshold1 := 255;
    threshold2 := 30;

    ON_SEGMENT(1);

    sthreshold1 := cvCreateTrackbar('Threshold1', 'Segmentation', @threshold1, 255, ON_SEGMENT);
    sthreshold2 := cvCreateTrackbar('Threshold2', 'Segmentation', @threshold2, 255, ON_SEGMENT);

    cvShowImage('Segmentation', image1);
    cvWaitKey(0);

    cvDestroyWindow('Segmentation');
    cvDestroyWindow('Source');

    cvReleaseMemStorage(storage);

    cvReleaseImage(image[0]);
    cvReleaseImage(image0);
    cvReleaseImage(image1);

  except
    on E: Exception do
      WriteLn(E.ClassName, ': ', E.Message);
  end;

end.
