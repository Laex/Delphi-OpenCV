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

program cv_CreateGaussianBGModel;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  System.SysUtils,
  uLibName in '..\..\..\include\uLibName.pas',
  highgui_c in '..\..\..\include\highgui\highgui_c.pas',
  imgproc.types_c in '..\..\..\include\imgproc\imgproc.types_c.pas',
  imgproc_c in '..\..\..\include\imgproc\imgproc_c.pas',
  legacy in '..\..\..\include\legacy\legacy.pas',
  calib3d in '..\..\..\include\calib3d\calib3d.pas',
  imgproc in '..\..\..\include\imgproc\imgproc.pas',
  haar in '..\..\..\include\objdetect\haar.pas',
  objdetect in '..\..\..\include\objdetect\objdetect.pas',
  tracking in '..\..\..\include\video\tracking.pas',
  Core.types_c in '..\..\..\include\сore\Core.types_c.pas',
  core_c in '..\..\..\include\сore\core_c.pas',
  Core in '..\..\..\include\сore\core.pas';

Var
  k: Integer;
  fr: Integer;
  tmp_frame: pIplImage;
  cap: PCvCapture;
  update_bg_model: Boolean;
  bg_model: pCvBGStatModel;
  t, learningRate: double;
  params:TCvGaussBGStatModelParams;

begin
  try
    update_bg_model := true;
    cap := cvCreateCameraCapture(CV_CAP_ANY);
    Assert(Assigned(cap));
    cvNamedWindow('Original', 1);
    cvNamedWindow('BG', 1);
    cvNamedWindow('FG', 1);
    cvMoveWindow('BG', 670, 0);
    cvMoveWindow('FG', 335, 0);
    cvMoveWindow('Original', 0, 0);

    params.win_size:=200;
    params.n_gauss:=5;	// cars:5, trees: 2/10
    params.bg_threshold:=0.6;	// cars: 0.7, trees: 0.9, car thief: 0.6
    params.std_threshold:=3.0;	// cars: 3.5, car thief: 3.0
    params.minArea:=5;	// cars: 25, trees: 1
    params.weight_init:=0.05;
    params.variance_init:=50;

    bg_model := NIL;
    fr := 1;
    tmp_frame := cvQueryFrame(cap);
    while tmp_frame <> NIL do
    begin
      cvShowImage('Original', tmp_frame);
      if (bg_model = NIL) then
      begin
        // create BG model
        bg_model := cvCreateGaussianBGModel(tmp_frame,@params);
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
