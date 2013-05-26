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
program Class_ObjectTracking;

{$APPTYPE CONSOLE}
{$POINTERMATH ON}
{$R *.res}

uses
  System.SysUtils,
  cvUtils,
  Core.types_c,
  core_c,
  Mat,
  highgui,
  highgui_c,
  core.types,
  core;

Var
  // initial min and max HSV filter values.
  // these will be changed using trackbars
  H_MIN: Integer = 0;
  H_MAX: Integer = 256;
  S_MIN: Integer = 0;
  S_MAX: Integer = 256;
  V_MIN: Integer = 0;
  V_MAX: Integer = 256;

  // default capture width and height
const
  FRAME_WIDTH = 640;
  FRAME_HEIGHT = 480;
  // max number of objects to be detected in frame
  MAX_NUM_OBJECTS = 50;
  // minimum and maximum object area
  MIN_OBJECT_AREA = 20 * 20;
  MAX_OBJECT_AREA = FRAME_HEIGHT * FRAME_WIDTH / 1.5;
  // names that will appear at the top of each window
  windowName: string = 'Original Image';
  windowName1: string = 'HSV Image';
  windowName2: string = 'Thresholded Image';
  windowName3: string = 'After Morphological Operations';
  trackbarWindowName: string = 'Trackbars';

procedure on_trackbar(pos: Integer; user: Pointer); cdecl;
begin
  // This function gets called whenever a
  // trackbar position is changed
end;

procedure createTrackbars;
begin
  // create window for trackbars
  namedWindow(trackbarWindowName, 0);
  // create memory to store trackbar name on window
  // char TrackbarName[50];
  // sprintf( TrackbarName, 'H_MIN', H_MIN);
  // sprintf( TrackbarName, 'H_MAX', H_MAX);
  // sprintf( TrackbarName, 'S_MIN', S_MIN);
  // sprintf( TrackbarName, 'S_MAX', S_MAX);
  // sprintf( TrackbarName, 'V_MIN', V_MIN);
  // sprintf( TrackbarName, 'V_MAX', V_MAX);
  // create trackbars and insert them into window
  // 3 parameters are: the address of the variable that is changing when the trackbar is moved(eg.H_LOW),
  // the max value the trackbar can move (eg. H_HIGH),
  // and the function that is called whenever the trackbar is moved(eg. on_trackbar)
  // ---->    ---->     ---->
  createTrackbar('H_MIN', trackbarWindowName, @H_MIN, H_MAX, on_trackbar);
  createTrackbar('H_MAX', trackbarWindowName, @H_MAX, H_MAX, on_trackbar);
  createTrackbar('S_MIN', trackbarWindowName, @S_MIN, S_MAX, on_trackbar);
  createTrackbar('S_MAX', trackbarWindowName, @S_MAX, S_MAX, on_trackbar);
  createTrackbar('V_MIN', trackbarWindowName, @V_MIN, V_MAX, on_trackbar);
  createTrackbar('V_MAX', trackbarWindowName, @V_MAX, V_MAX, on_trackbar);
end;

procedure drawObject(x, y: Integer; frame: IMat);
begin
  // use some of the openCV drawing functions to draw crosshairs
  // on your tracked image!
  circle(frame, Point(x, y), 20, Scalar(0, 255, 0), 2);
  line(frame, Point(x, y - 5), Point(x, y - 25), Scalar(0, 255, 0), 2);
  line(frame, Point(x, y + 5), Point(x, y + 25), Scalar(0, 255, 0), 2);
  line(frame, Point(x - 5, y), Point(x - 25, y), Scalar(0, 255, 0), 2);
  line(frame, Point(x + 5, y), Point(x + 25, y), Scalar(0, 255, 0), 2);
  PutText(frame, x.AsString + ', ' + y.AsString, Point(x, y + 30), 1, 1, Scalar(0, 255, 0), 2);
end;

//procedure morphOps(Mat &thresh);
//begin
//
//  // create structuring element that will be used to "dilate" and "erode" image.
//  // the element chosen here is a 3px by 3px rectangle
//
//  Mat erodeElement = getStructuringElement(MORPH_RECT, Size(3, 3));
//  // dilate with larger element so make sure object is nicely visible
//  Mat dilateElement = getStructuringElement(MORPH_RECT, Size(8, 8));
//
//  erode(thresh, thresh, erodeElement);
//  erode(thresh, thresh, erodeElement);
//
//  dilate(thresh, thresh, dilateElement);
//  dilate(thresh, thresh, dilateElement);
//
//end;

begin
  try

  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;

end.
