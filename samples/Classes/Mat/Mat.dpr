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
program Mat;

{$APPTYPE CONSOLE}
{$POINTERMATH ON}
{$R *.res}

uses
  System.SysUtils,
  uLibName in '..\..\..\include\uLibName.pas',
  opencv_classes in '..\..\..\include\opencv_classes\opencv_classes.pas',
  Core.types_c in '..\..\..\include\core\Core.types_c.pas',
  core_c in '..\..\..\include\core\core_c.pas';

Var
  TestMat: IMat;

begin
  try
    TestMat := CreateMat;
    Writeln('Mat._type=', TestMat._type);
    Writeln('Mat.depth=', TestMat.depth);
    Writeln('Mat.channels=', TestMat.channels);
    Writeln('Mat.empty=', TestMat.empty);
    Readln;

    TestMat := CreateMat(2,2,CV_8UC1);
    Writeln('Mat._type=', TestMat._type);
    Writeln('Mat.depth=', TestMat.depth);
    Writeln('Mat.channels=', TestMat.channels);
    Writeln('Mat.empty=', TestMat.empty);
    Readln;

  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;

end.
