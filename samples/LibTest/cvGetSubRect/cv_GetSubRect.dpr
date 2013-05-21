(* /*****************************************************************
  //                       Delphi-OpenCV Demo
  //               Copyright (C) 2013 Project Delphi-OpenCV
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
  ******************************************************************* *)
// JCL_DEBUG_EXPERT_GENERATEJDBG OFF
// JCL_DEBUG_EXPERT_INSERTJDBG OFF
// JCL_DEBUG_EXPERT_DELETEMAPFILE OFF
program cv_GetSubRect;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  System.SysUtils,
  uLibName in '..\..\..\include\uLibName.pas',
  highgui_c in '..\..\..\include\highgui\highgui_c.pas',
  core_c in '..\..\..\include\Core\core_c.pas',
  Core.types_c in '..\..\..\include\Core\Core.types_c.pas',
  imgproc.types_c in '..\..\..\include\imgproc\imgproc.types_c.pas',
  imgproc_c in '..\..\..\include\imgproc\imgproc_c.pas',
  legacy in '..\..\..\include\legacy\legacy.pas',
  calib3d in '..\..\..\include\calib3d\calib3d.pas',
  imgproc in '..\..\..\include\imgproc\imgproc.pas',
  haar in '..\..\..\include\objdetect\haar.pas',
  objdetect in '..\..\..\include\objdetect\objdetect.pas',
  tracking in '..\..\..\include\video\tracking.pas',
  Core in '..\..\..\include\Core\core.pas',
  Mat in '..\..\..\include\core\Mat.pas',
  core.types in '..\..\..\include\core\core.types.pas',
  cvUtils in '..\..\..\include\cvUtils.pas';

const
  filename = 'Resource\opencv_logo_with_text.png';

var
  image: pIplImage = nil;
  cut_image: pIplImage = nil;
  R:TCvRect;
begin
  try
    image := cvLoadImage(filename, CV_LOAD_IMAGE_UNCHANGED);
    R:=CvRect(50,50,250,250);
    cut_image:=cvCreateImage(CvSize(0,0), image^.depth, image^.nChannels);
    cut_image := cvGetSubRect(image,cut_Image,R);
    cvNamedWindow(filename, CV_WINDOW_AUTOSIZE);
    cvNamedWindow('Cut image', CV_WINDOW_AUTOSIZE);
    cvRectangle(image,CvPoint(50,50),CvPoint(50+250,50+250),CvScalar(150));
    cvShowImage(filename, image);
    cvShowImage('Cut image', cut_image);
    cvWaitKey(0);
    cvReleaseImage(image);
    cvReleaseImage(cut_image);
    cvDestroyAllWindows;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;

end.
