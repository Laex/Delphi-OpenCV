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

program cv_LoadImage;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  ocv.highgui_c,
  ocv.core_c,
  ocv.core.types_c,
  ocv.imgproc_c,
  uResourcePaths;

const
  // declare image filename
  IMAGE_FILE_NAME = cResourceMedia + 'opencv_logo_with_text.png';

var
  // declare an opencv image pointer variable
  image: pIplImage;

begin
  try
    // load image from file
    // REMARK: all opencv strings are PAnsiChar, pay attention to this
    // when using with Delphi 2010/2009
    image := cvLoadImage(IMAGE_FILE_NAME);
    // create display window
    cvNamedWindow('image');
    // display image inside "image" window
    cvShowImage('image', image);
    // wait until user keypress
    cvWaitKey();
    // release image memory
    cvReleaseImage(image);
    // close and release all display windows
    cvDestroyAllWindows;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;

end.
