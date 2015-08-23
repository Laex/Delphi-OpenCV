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
program cv_CreateFGDStatModel;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  System.SysUtils,
  ocv.highgui_c,
  ocv.core_c,
  ocv.core.types_c,
  ocv.imgproc_c,
  ocv.legacy;

var
  k: Integer;
  fr: Integer;
  tmp_frame: pIplImage;
  cap: PCvCapture;
  update_bg_model: Boolean;
  bg_model: pCvBGStatModel;
  t, learningRate: double;

begin
  try
    update_bg_model := true;
    cap := cvCreateCameraCapture(CV_CAP_ANY);
    Assert(Assigned(cap));
    cvNamedWindow('Original', 1);
    cvNamedWindow('BG', 1);
    cvNamedWindow('FG', 1);

    bg_model := NIL;
    fr := 1;
    tmp_frame := cvQueryFrame(cap);
    while tmp_frame <> NIL do
    begin
      cvShowImage('Original', tmp_frame);
      if (bg_model = NIL) then
      begin
        // create BG model
        bg_model := cvCreateFGDStatModel(tmp_frame);
      end
      else
      begin
        t := cvGetTickCount();
        if (update_bg_model) then
          learningRate := -1
        else
          learningRate := 0;

        cvUpdateBGStatModel(tmp_frame, bg_model, learningRate);
        t := cvGetTickCount - t;

        Writeln(format('%d. %.1f', [fr, t / (cvGetTickFrequency * 1000.0)]));

        cvShowImage('BG', bg_model^.background);
        cvShowImage('FG', bg_model^.foreground);
      end;
      k := cvWaitKey(5);
      if (k = 27) then
        break;
      if (k = ord(' ')) then
        update_bg_model := Not update_bg_model;
      tmp_frame := cvQueryFrame(cap);
      Inc(fr);
    end;
    cvReleaseBGStatModel(bg_model);
    cvReleaseCapture(cap);
    cvDestroyAllWindows;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;

end.
