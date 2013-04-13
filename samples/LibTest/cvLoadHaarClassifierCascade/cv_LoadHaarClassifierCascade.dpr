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
  *******************************************************************
  // Original file:
  // opencv\samples\c\convert_cascade.c
  // *************************************************************** *)

// JCL_DEBUG_EXPERT_GENERATEJDBG OFF
// JCL_DEBUG_EXPERT_INSERTJDBG OFF
// JCL_DEBUG_EXPERT_DELETEMAPFILE OFF
program cv_LoadHaarClassifierCascade;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  System.SysUtils,
  uLibName in '..\..\..\include\uLibName.pas',
  highgui_c in '..\..\..\include\highgui\highgui_c.pas',
  core_c in '..\..\..\include\ñore\core_c.pas',
  Core.types_c in '..\..\..\include\ñore\Core.types_c.pas',
  imgproc.types_c in '..\..\..\include\imgproc\imgproc.types_c.pas',
  imgproc_c in '..\..\..\include\imgproc\imgproc_c.pas',
  legacy in '..\..\..\include\legacy\legacy.pas',
  calib3d in '..\..\..\include\calib3d\calib3d.pas',
  imgproc in '..\..\..\include\imgproc\imgproc.pas',
  haar in '..\..\..\include\objdetect\haar.pas',
  objdetect in '..\..\..\include\objdetect\objdetect.pas',
  tracking in '..\..\..\include\video\tracking.pas',
  Core in '..\..\..\include\ñore\core.pas';

procedure help;
begin
  WriteLn('This sample demonstrates cascade''s convertation to 640x480');
  WriteLn('Usage:');
  WriteLn('convert_cascade <input_cascade_path> <output_cascade_filename>');
  WriteLn;
  WriteLn('Example:');
  WriteLn('convert_cascade FaceDetectXML\haarcascade_eye.xml result\test_cascade.xml');
end;

Var
  cascade: pCvHaarClassifierCascade = nil;
  size: TCvSize;
  input_cascade, output_cascade, comment: AnsiString;

begin
  try
    input_cascade := iif((ParamCount > 0) and FileExists(ParamStr(1)), ParamStr(1), 'FaceDetectXML\haarcascade_eye.xml');
    if not FileExists(input_cascade) then
    begin
      help;
      WriteLn('input_cascade not found');
      Halt;
    end;
    output_cascade := iif(ParamCount > 1, ParamStr(2), 'result\test_cascade.xml');

    size.width := 640;
    size.height := 480;

    cascade := cvLoadHaarClassifierCascade(pCvChar(@input_cascade[1]), size);

    if not Assigned(cascade) then
    begin
      WriteLn('Input cascade could not be found/opened');
      Halt;
    end;

    comment := Format('Automatically converted from %s, window size = %dx%d', [input_cascade, size.width, size.height]);
    cvSave(pCvChar(@output_cascade[1]), cascade, 0, pCvChar(@comment[1]), cvAttrList(0, 0));

  except
    on E: Exception do
      WriteLn(E.ClassName, ': ', E.Message);
  end;

end.
