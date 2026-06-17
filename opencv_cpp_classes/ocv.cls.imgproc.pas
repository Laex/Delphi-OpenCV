(*
  *****************************************************************
  Delphi-OpenCV
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

unit ocv.cls.imgproc;

{$I OpenCV.inc}

interface

Uses
  ocv.core.types_c;

procedure Canny(
  { } image: PIplImage;
  { } edges: PIplImage;
  { } threshold1: Double;
  { } threshold2: Double;
  { } apertureSize: Integer = 3;
  { } L2gradient: cbool = false); stdcall;

implementation

Uses
  ocv.utils,
  ocv.lib;

procedure Canny; external opencv_classes_lib name '_prxCanny@32';

end.
