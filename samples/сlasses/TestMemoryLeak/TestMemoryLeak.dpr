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

program TestMemoryLeak;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  WinApi.Windows,
  System.SysUtils,
  ocv.cls.core,
  ocv.highgui_c,
  ocv.core_c,
  ocv.core.types_c,
  ocv.imgproc_c,
  ocv.imgproc.types_c,
  uResourcePaths;

const
  filename = cResourceMedia + 'panorama_image2.jpg';

Var
  image: pIplImage = nil;
  M: IMat;

begin
  try
    ReportMemoryLeaksOnShutdown := True;
    while True do
    begin
      image := cvLoadImage(filename);
      M := TMat.Create(image);
      M := nil;
      cvReleaseImage(image);
    end;
  except
    on E: Exception do
      WriteLn(E.ClassName, ': ', E.Message);
  end;

end.
